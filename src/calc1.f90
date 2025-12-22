subroutine calc
!***********************************************************************
!             CALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls Flow and Processing of mixed Modules
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED: Split RLINE source into RLINEXT and RLINE.
!                  Wood, 03/04/2019
!
!        MODIFIED: Added code to process a RLINE source.
!                  Wood, 07/20/2018
!
!        MODIFIED: Added code to process a buoyant line source; several
!                  routines from the original BLP are incorporated into
!                  this file.
!                  Amec Foster Wheeler, 06/30/2015
!
!        MODIFIED: Added check for runtime error (RUNERR) before
!                  continuing with source loop.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!        MODIFIED: Moved METHDR assignment statement from SUB. PCALC
!                  to beginning of source loop.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 12/28/05
!
!        INPUTS:  Arrays of Source Parameters
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each
!                 Source/Receptor
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line, only: numblgrps, bl_uref
   use rline_data, only: rlprocessed
   implicit none
   character modnam*12
   integer  :: kk
   logical  :: blprocessed

!     Variable Initializations
   modnam = 'CALC'
   path = 'CN'

!    Added for TTRM; AECOM
!    Initialize array for TTRM calculations
   if (runttrm) then
      ttrmout(:,:,:) = 0.0d0
   endif
!    End TTRM insert; Feb. 2021

! --- Set the bouyant line source flag to false to indicate that no
!      lines have been processed for this hour since all lines are
!      processed together
   blprocessed = .false.

! --- Set the RLINE source flag to false to indicate that no
!      RLINES have been processed for this hour since rotation of
!      receptors only occurs for first source.
   rlprocessed = .false.

!     Assing METHDR = .TRUE. to print source&receptor-independent
!     meteorology debug information to METEORDBG debug output file.
   methdr = .true.

!     Begin Source LOOP
   source_loop: do isrc = 1, numsrc
      if (srctyp(isrc)(1:5) .eq. 'POINT') then
!           Calculate Point Source Values                   ---   CALL PCALC
         call pcalc

      else if (srctyp(isrc) .eq. 'VOLUME') then
!           Calculate Volume Source Values                  ---   CALL VCALC
         call vcalc

      else if (srctyp(isrc)(1:4) .eq. 'AREA') then
!           Calculate AREA/AREAPOLY/AREACIRC Source Values  ---   CALL ACALC
         call acalc

      else if (srctyp(isrc) .eq. 'OPENPIT') then
!           Calculate OpenPit Source Values                 ---   CALL OCALC
         call ocalc

! ---    Added option to process LINE source, using AREA source calc routines
      else if (srctyp(isrc) .eq. 'LINE') then
!           Calculate LINE Source Values                    ---   CALL ACALC
         call acalc

! ---    Added option to process RLINE source
      else if ((srctyp(isrc) .eq. 'RLINE') .or.&
      &(srctyp(isrc) .eq. 'RLINEXT')) then
!           Calculate RLINE Source Values                   ---   CALL RLCALC
         call rlcalc
         rlprocessed = .true.

      else if (srctyp(isrc) .eq. 'BUOYLINE' .and.&
      &(.not. blprocessed)) then

! Multiple_BuoyLines_D41_Wood
!           The original BLP used meteorology from the met processor CRSTER.
!           CRSTER did not allow wind speeds to be less than 1 m/s.  Set the
!           reference wind speed used in the calculations below to be a minimum
!           of 1.0 m/s.  Use the local variable BL_UREF.  Keep the reference
!           wind speed height as UREFHT.
         if (uref .ge. 1.0d0) then
            bl_uref = uref
         else
            bl_uref = 1.0d0
!             WRITE Message              ! Ref ht wind speed less than minimum
            write(dummy,'(''on '',I8.8)') kurdat
            call errhdl(path,modnam,'W','471',dummy)
         end if

! ---       Calculate Bouyant Line Source Values            ---   CALL BL_CALC
!           BLPROCESSED lets AERMOD know that all lines associated with
!            the buoyant line sources were processed on the first pass
!            through the sources

! Multiple_BuoyLines_D41_Wood

         do kk = 1,numblgrps
            call bl_calc(kk)
         end do
         blprocessed = .true.

!        Added for Sidewash
      else if (srctyp(isrc) .eq. 'SWPOINT') then
!           Calculate SIDEWASH Source Values                    ---   CALL SWCALC
         call swcalc
      end if

!        Check for runtime error (RUNERR) before continuing loop
      if (runerr) exit

   end do source_loop
!     End Source LOOP

   if (l_backgrnd .and. .not.arm2 .and.&
   &.not.olm .and. .not.grsm .and.&
   &.not.runttrm .and. .not.pvmrm) then
! ---    User-specified background concentrations are included;
!        add to modeled concentrations by source group, unless
!        NO2 options are specified, which are handled separately
      do igrp = 1, numgrp
         call sumback
      end do
   end if

   return
end

subroutine pcalc
!***********************************************************************
!             PCALC Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis, J. Scire
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: Calculates concentration or deposition values
!                 for POINT sources
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        CHANGES:
!                  Added computation of AC & BC for PLAT_CUBIC function,
!                  called from PLAT_GRADPLUMADJ for PLATFORM sources.
!                  Michelle G. Snyder, WOOD, 08/05/2021.
!
!                  Modified to calculate conc*travel time for GRSM NO2 option
!                  CERC, 11/30/20
!
!                  Added arrays to save WDSIN and WDCOS by source for use
!                  with PVMRM option.  Corrects potential problem for
!                  PVMRM applications with multi-level wind inputs.
!                  R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!                  Added call to subroutine HEFF for calculation
!                  of ZSUBP for deposition applications.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
!
!                  Moved METHDR assignment statement from SUB. PCALC
!                  to beginning of source loop in SUB. CALC.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 12/28/05
!
!                  Modified for capped stack option (POINTCAP) and
!                  for multiple urban area option.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
!
!                  Removed code that adjusts DHCRIT based on distance
!                  to well-mixed layer for stable conditions for
!                  consistency with Model Formulation Document.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
!
!                  Modified to include initialization of __VAL arrays
!                  at end of receptor loop.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
!
!                  Modified to include the PVMRM and OLM options for
!                  modeling conversion of NOx to NO2.
!                  Added debug statement based on ENSR code.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
!
!RWB               Modified to call DHPSS to obtain plume centroid height
!RWB               (CENTER) for Schulman-Scire downwash cases.  Modified
!RWB               to compare XFINAL to XMIXED only for unstable cases with
!RWB               HS < ZI.  Added initialization of TGEFF and TGEFF3 as TGS.
!RWB               Additional modifications made to improve consistency with
!RWB               the implementation of Schulman-Scire downwash algorithm
!RWB               in the ISCST3 model.
!RWB               R. Brode, PES - 12/6/99
!
!RWB               Modified to use wind direction at midpoint between
!RWB               stack height and "final" plume height for transport.
!RWB               R. Brode, PES - 1/22/98
!
!RWB               Use effective parameters evaluated at stack height
!RWB               for the indirect plume, as for direct plume.  Also
!RWB               commented out calls to LOCATE and GINTRP with ZIO2.
!RWB               This change is made for the Base Case model.
!RWB               R. Brode, PES - 12/8/94
!
!RWB               Commented out calls to LOCATE and GINTRP with HTEFF in
!RWB               order to use effective parameters evaluated at stack height
!RWB               instead of HTEFF for the direct plume and the stable
!RWB               plume.  This change is made for the Base Case model.
!RWB               R. Brode, PES - 12/7/94
!
!                  Moved calculation of penetration factor from outside
!                  to inside receptor loop, and deleted code related
!                  to indirect and penetrated plumes which is no
!                  longer needed.  (R.F. Lee, 7/13/94)
!
!                  Added true centerline concentration calculations
!                  for EVALFL output.  (R.F. Lee, 7/25/94)
!
!RJP               Changes made to calculations of effective parameters
!RJP               in conjunction with new treatment of inhomogeneity.
!RJP               (Bob Paine, 10/4/94)
!
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
!        References:  "A Dispersion Model for the Convective Boundary
!                      Layer", J. Weil, 8/17/93
!                     "Inhomogeneous Boundary Layer", A. Venkatram,
!                      6/25/93
!
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   integer :: i, kiter, ndxzmid, ndxzpl
   integer :: ndxbh
   double precision :: hsprim, zplm, dhfold, svpm, upm, tgpm,&
   &ptpm, ptp, zmid
   double precision :: valabv, vbelow
   double precision :: ustack, ubldg, xbrec, ybrec
   double precision :: aerplm(numtyp), aerpan(numtyp), fran, fran3
   double precision :: vseq
!   Added for TTRM; AECOM
!      DOUBLE PRECISION :: DEFF, DEFF3, DEFFD
   double precision :: chin, chid
!   End TTRM insert; Feb. 2021
   logical :: l_plume

   logical :: ldbhr

!MGS  Michelle G. Snyder 8/5/2021 D063 Variables needed for platform downwash
   double precision :: dum, platsz0, rz0


!     Variable Initializations
   modnam = 'PCALC'

!     Initialize __VAL arrays (1:NUMTYP)
   hrval(:)   = 0.0d0
   aerval(:)  = 0.0d0
   aerplm(:)  = 0.0d0
   aerpan(:)  = 0.0d0
   prmval(:)  = 0.0d0
   if( allocated(chi) ) chi(:,isrc,:) = 0.0d0
   if(grsm)then
      chi_ttravplm = 0.0d0
      chi_ttravpan = 0.0d0
      chi_ttravaer = 0.0d0
      chi_ttravprm = 0.0d0
      chi_ttravchm(:,isrc) = 0.0d0
      bldfac(:,isrc) = 0.0d0
      PRMVAL_Src1 = 0.0d0
   end if

! --- Initialize FRAN and FRAN3
   fran  = 0.0d0
   fran3 = 0.0d0

   if (awmadwdbg) then
      write(awmadwdbunt,*) '========================================'
      write(awmadwdbunt,*) ' PCALC: YYMMDDHH ',kurdat,&
      &'  Source  ',srcid(isrc)
   end if

!     Set the Source Variables for This Source              ---   CALL SETSRC
   call setsrc

!     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
   call emfact(qs)

   if (qtk .ne. 0.0d0) then

!        Set Mixing Height and Profiles for Urban Option if Needed
      if (urbsrc(isrc) .eq. 'Y') then
!           Find Urban Area Index for This Source
         do i = 1, numurb
            if (iurbgrp(isrc,i) .eq. 1) then
               iurb = i
               exit
            end if
         end do
         if (stable .or. L_MorningTrans(iurb)) then
            urbstab = .true.
            zi = max( ziurb(iurb), zimech )
            gridsv = grdsvu(1:mxglvl,iurb)
            gridsw = grdswu(1:mxglvl,iurb)
            gridtg = grdtgu(1:mxglvl,iurb)
            gridpt = grdptu(1:mxglvl,iurb)
            obulen = urbobulen(iurb)
            ustar  = urbustr(iurb)
         else
            urbstab = .false.
            zi = zirur
            gridsv = grdsvr
            gridsw = grdswr
            gridtg = grdtgr
            gridpt = grdptr
            obulen = rurobulen
            ustar  = rurustr
         end if
      else if (urban .and. urbsrc(isrc) .eq. 'N') then
         urbstab = .false.
         zi = zirur
         gridsv = grdsvr
         gridsw = grdswr
         gridtg = grdtgr
         gridpt = grdptr
         obulen = rurobulen
         ustar  = rurustr
      else
! ---       Rural
         urbstab = .false.
      end if

!        Calculate the initial meteorological variables     ---   CALL METINI
      call metini

!        Calculate Buoyancy and Momentum Fluxes             ---   CALL FLUXES
      call fluxes(vseq)

!        Set Wake and Building Type Switches                ---   CALL WAKFLG
! ---    NOTE:  WAKFLG sets building dimensions based on wind
!        direction at stack top.

! ---    CRT, 1/20/2012: D063 Set dependency for call to WAKFLG
!        If source is subject to platform downwash, set
!        WAKE to false.  Only call WAKFLG if source is not
!        subject to platform downwash.
      if (osplat(isrc)) then
         wake = .false.
      else
         call wakflg
      end if

!        Define temporary values of CENTER and SURFAC based on HS
      center = hs
      if( center .lt. 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

! ---    Check for stack-tip downwash option and adjust if necessary, but
!        first check for POINTCAP option to avoid NOSTD overriding POINTCAP
      if (srctyp(isrc) .eq. 'POINTCAP') then
!           Apply stack-tip downwash for capped stacks with VS = 0.001m/s
         hsp = hsprim ( us, vseq, hs, ds )
      else if (srctyp(isrc) .eq. 'POINTHOR') then
!           Do not apply stack-tip downwash for horizontal releases
         hsp = hs
      else if( nostd )then
!           No stack-tip downwash, no adjustments necessary
         hsp = hs
      else
!           Make adjustments for stack-tip downwash
         hsp = hsprim ( us, vs, hs, ds )
      end if

!        Calculate Distance to Final Rise                   ---   CALL DISTF
      call distf

!        Calculate the plume penetration factor             ---   CALL PENFCT
      call penfct


      if(debug) then
         write(dbgunt,6000) dhfaer, up, tgs
6000     format(/,5x,'INITIAL PLUME RISE ESTIMATE:  DELH = ',&
         &f6.1,' M; Uplume = ',f5.2,' M/S; DTHDZ = ',&
         &f7.4,' DEG K/M')
      end if

      if (stable .or. (unstab.and.(hs.ge.zi))) then
!           Use iterative approach to stable plume rise calculations
         kiter = 0
50       zplm = hsp + 0.5d0 * dhfaer
         dhfold = dhfaer

!----       Locate index below ZPLM

         call locate(gridht, 1, mxglvl, zplm, ndxzpl)

!----       Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
!           vertical potential temperature gradient, if stable.

         call gintrp( gridht(ndxzpl), gridsv(ndxzpl),&
         &gridht(ndxzpl+1), gridsv(ndxzpl+1), zplm, svpm )
         call gintrp( gridht(ndxzpl), gridws(ndxzpl),&
         &gridht(ndxzpl+1), gridws(ndxzpl+1), zplm, upm )
         svpm = max( svpm, svmin, svumin*upm )
         if( l_vectorws )then
            upm = dsqrt( upm*upm + 2.0d0*svpm*svpm )
         endif

         upm  = max( upm, wsmin )


!RWB        Use average of stack top and midpoint wind speeds.
         up = 0.5d0 * (us + upm)

         call gintrp( gridht(ndxzpl), gridtg(ndxzpl),&
         &gridht(ndxzpl+1), gridtg(ndxzpl+1), zplm, tgpm )
         call gintrp( gridht(ndxzpl), gridpt(ndxzpl),&
         &gridht(ndxzpl+1), gridpt(ndxzpl+1), zplm, ptpm )
!RWB        Use average of stack top and midpoint temperature gradients.
         tgp = 0.5d0 * (tgs + tgpm)
         ptp = 0.5d0 * (pts + ptpm)
         bvf = dsqrt( g * tgp / ptp )
         if(bvf .lt. 1.0d-10) bvf = 1.0d-10
         bvprim  = 0.7d0 * bvf

         call distf

         kiter = kiter + 1

!RJP        Add temporary debugging statements

         if(debug) then
            write(dbgunt,6001) kiter,dhfold, dhfaer, zplm, up,tgp
6001        format(/,5x,'OPTH2 ITER. #',i1,': OLD DELH = ',&
            &f6.1,' M; NEW DELH = ',f6.1,' M; MET LEVEL = ',&
            &f6.1,' M; NEW Upl = ',f5.2,' M/S; NEW DTHDZ = ',&
            &f7.4,' K/M')
         end if

!           Check for convergence
         if(dabs((dhfold - dhfaer)/dhfaer) .lt. 0.01d0) go to 60

         if(kiter .ge. 5) then
            dhfaer = 0.5d0 * (dhfaer + dhfold)
            if(debug) write(dbgunt,6002) dhfaer
6002        format(/,5x,'PLUME RISE ITERATION FAILED TO CONVERGE; ',&
            &' PLUME RISE SET AT ',f6.1,' METERS.',/)
            go to 60
         else
            go to 50
         end if

60       continue

!RWB        After completing iteration, reset UP and TGP to stack top
!RWB        values for subsequent distance-dependent plume rise calcs.
         up = us
         tgp = tgs
         ptp = pts
         bvf = dsqrt( g * tgp / ptp )
         if(bvf .lt. 1.0d-10) bvf = 1.0d-10
         bvprim  = 0.7d0 * bvf
      end if

!        Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
      prm_fstrec = .true.

!        Initialize 'ARC' Arrays for EVALFILE Output        ---   CALL EVLINI
      if (eval(isrc)) then
         call evlini
      end if

      zmidmx = 0.5d0 * zi

!RJP     Add temporary debugging statement.

      if( debug) then
         write(dbgunt,6010) kurdat, zmidmx
6010     format(/,72('*'),//,5x,'YR/MN/DY/HR: ',i8,//,&
         &5x,'Height assigned to midpoint of ',&
         &'well-mixed layer for effective parameters = ',&
         &f6.1,' meters.',/)
      end if
!RJP
!RJP     Calculate distance to uniformly mixed plume within the
!RJP     boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
!RJP     distance is approximately (Zi * UAVG)/SWAVG, where UAVG
!RJP     and SWAVG are wind speed and sigma-w averaged over the depth
!RJP     between the ground and Zi (or the plume height, if higher in
!RJP     stable conditions); this height is denoted as 2 * ZMIDMX.
!RJP
!RJP     First, get refined estimate of final rise and distance to final
!RJP     rise if downwash conditions prevail.
!RJP
      xfinal = xmax
      dhcrit = dhfaer
      xmixed = zi * uavg / swavg
      if (unstab .and. hs.lt.zi) then
!           Check for XMIXED smaller than 1.25*XFINAL
         if (xmixed .lt. 1.25d0*xfinal) then
            xfinal = 0.8d0 * xmixed
            call cblprd (xfinal)
            dhcrit = dhp1
         end if
      end if


! ---    Initialize PDF parameters for use in calculating ZSUBP
      if( unstab  .and.  (hs .lt. zi) ) then
         call pdf
      end if
!        Set Dry Deposition Variables for this Source
      if (luservd .and. ldgas .and. npd.eq.0) then
!           Assign user-specified gas dry deposition velocity (GASDEPVD option)
         vdepg = uservd
      else if (ldpart .or. (.not.luservd .and. ldgas .and.&
      &npd.eq.0)) then
!           Calculate Deposition Velocities for this Source    ---   CALL VDP
         call vdp
      end if
      if (lwpart .or. lwgas) then
!PES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
!PES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
!PES        evaluated at a distance of 20 kilometers downwind.
!PES        Apply minimum value of 500m and maximum value of 10,000m.
         call heff (20000.0d0)
         if( stable .or. (unstab .and. hs .ge. zi) )then
            he = hsp + dhcrit
            call sigz(20000.0d0)
            zsubp = max( 500.0d0, zi, he + szcoef*szas )
         else if (unstab) then
            hed1 = hsp + dhcrit
            if (ppf .gt. 0.0d0) then
               call cblpr3
            end if
            call sigz(20000.0d0)

            if (ppf .eq. 0.0d0) then
               zsubp=max( 500.0d0, zi, hed1 + szcoef*(szad1+szad2)/&
               &2.0d0 )
            else if (ppf .eq. 1.0d0) then
               zsubp=max( 500.0d0, zi, he3 + szcoef*sza3)
            else
               zsubp=max( 500.0d0, zi, ppf*(he3+szcoef*sza3) +&
               &(1.0d0-ppf)*(hed1 + szcoef*(szad1+szad2)/2.0d0) )
            end if
         end if
         zsubp = min( 10000.0d0, zsubp )
!           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
         call scavrat
      end if

!RWB     Determine transport wind direction using midpoint between
!RWB     stack height and "final" plume height.  R. Brode, PES, 1/22/98
!----    Define ZMID=midpoint between stack height and "final" plume height
      zmid = min( 4000.0d0, (hs + 0.5d0*dhfaer) )

!----    Locate index below ZMID
      call locate(gridht, 1, mxglvl, zmid, ndxzmid)

!----    Extract WD for grid levels above and below ZMID
      valabv = gridwd(ndxzmid+1)
      vbelow = gridwd(ndxzmid)

!----    Check for 360 crossover and adjust if necessary
      if( (valabv-vbelow) .lt. -180.0d0) then
         valabv = valabv + 360.0d0
      else if( (valabv-vbelow) .gt. 180.0d0) then
         valabv = valabv - 360.0d0
      end if

!----    Assign Wind direction
      if (vbelow .eq. valabv) then
         wdir = vbelow
      else
!----       Interpolate to ZMID
         call gintrp( gridht(ndxzmid), vbelow,&
         &gridht(ndxzmid+1), valabv,&
         &zmid, wdir )
      end if

!        Check for WDIR > 360 or < 0
      if (wdir .gt. 360.0d0) then
         wdir = wdir - 360.0d0
      else if (wdir .le. 0.0d0) then
         wdir = wdir + 360.0d0
      end if
!
!----    Convert direction to radians, compute sine and cosine of direction,
!        and determine nearest 10-degree sector.
!
!---->   wind direction = wind direction in degrees * DTORAD

      wdsin = dsin(wdir * dtorad)
      wdcos = dcos(wdir * dtorad)

! ---    Save WDSIN and WSCOS for later use by PVMRM/GRSM option
      awdsin(isrc) = wdsin
      awdcos(isrc) = wdcos

      afv = wdir - 180.0d0
      if (afv .lt. 0.0d0) then
         afv = afv + 360.0d0
      end if
      aafv(isrc) = afv       ! save flowvector for this source
      ! for use in MAXDCONT processing
      ifvsec = idint (afv*0.10d0 + 0.4999d0)
      if (ifvsec .eq. 0) ifvsec = 36

!
! --- PRIME ---------------------------------------------------------
! ---    Setup computations for numerical plume rise algorithm
! ---    and building wake analysis
      if(wake) then
! ---       Store selected data in new variables for future reference
         ustack=us

! ---       Compute wind speed at top of building           ---   CALL WSADJ
! ---       Locate index below building height
         call locate(gridht, 1, mxglvl, dsbh, ndxbh)

         call gintrp( gridht(ndxbh), gridws(ndxbh),&
         &gridht(ndxbh+1), gridws(ndxbh+1), dsbh, ubldg )

! ---       Refresh /WAKEDAT/ variables                     ---   CALL WAKE_INI
         ldbhr=primedbg
! ---       Note that logical RURAL has no impact on calculations
         rural = .true.
         call wake_ini(kurdat,ldbhr,prmdbunt,rural,dsbh,dsbw,dsbl,&
         &xadj,yadj,ubldg,ustack)
      end if
! ------------------------------------------------------------

! ---- PLATFORM DOWNWASH ADDITIONS (BEGIN) -------------------
!   Compute distance INDENPENDENT AC and BC values for CUBIC (function PLAT_CUBIC) to
!   determine plume rise adjustment (function PLAT_GRADPLUMADJ); AC & BC are global
!   variables in modules.f/MAIN1. Taken from OCD(v5).FOR/PTR at PTR01130.
!   Michelle G. Snyder, Wood 7/29/2021

!CRT   D063
      if (osplat(isrc)) then
         ac = 0.0d0
         bc = 0.0d0
         platsz0 = 0.0d0
         dum = 0.0d0
         if ((platwb(isrc) > 0.0d0) .and. (plathb(isrc) > 0.0d0)) then
            call plat_downwash (2.2d0*plathb(isrc),&
            &plathb(isrc)+0.0d0, platwb(isrc), 0.0d0,&
            &platsz0, dum)
            rz0 = platsz0/dsqrt(2.0d0 / pi)
            ac = 10.0d0/3.0d0 * rz0
            bc = 25.0d0/3.0d0 * rz0 * rz0
         end if

         if (platfmdbg) then
            write(platfmdbunt,'(A,1(A, 2X, I6), 6(A, 2X, F8.2))')&
            &'calc1.f/PCALC: ',&
            &' ISRC = ', isrc,&
            &' PLATWB(ISRC) = ', platwb(isrc),&
            &' PLATHB(ISRC) = ', plathb(isrc),&
            &' PLATSZ0 = ', platsz0,&
            &' RZ0 = ', rz0,&
            &' AC = ', ac,&
            &' BC = ', bc
         end if

      end if
! ---- PLATFORM DOWNWASH ADDITIONS (END) -------------------

!RJP     Add temporary debugging statement.

      if(debug) then
! ---       Include "effective" wind direction here
         write(dbgunt, 6011) dhcrit, xfinal, xmixed, afv
6011     format(5x,'For effective parameter calculations: ',&
         &'"Final" plume rise = ',g14.6, ' m; Distance to final ',&
         &'rise = ',g14.6,' m',/,5x,'Distance to well-mixed ',&
         &'state = ',g14.6,' m;',/,5x,'"Effective" flow vector = ',&
         &f6.2)
!RJP
!RJP        Make call to PSRDEB
!RJP
         call psrdeb

      end if
!
!        Begin Receptor LOOP *******************************************
!CRFL
!CRFL    Add logical variable METHDR, which is set to TRUE at the start
!CRFL    of the receptor loop, and reset to false after the headers and
!CRFL    non-receptor dependent meteorological variables in the
!CRFL    meteorological debug file are printed.  METHDR is also added
!CRFL    to MAIN.INC and METEXT.FOR (Subroutine METDEB).  9/27/94, R.F. Lee.
!CRFL
!CRWB    METHDR assignment statement moved to SUB. CALC.  12/28/05, R.W. Brode.

      receptor_loop: do irec = 1, numrec
!CRT        D063 Platform Downwash Debug
         if (platfmdbg .and. osplat(isrc)) then
            write(platfmdbunt,'(A, A, I4, 3(2X, A, I4), A)')&
            &'-------------',&
            &'JDAY= ', jday,&
            &'IHOUR = ', ihour,&
            &'ISRC= ', isrc,&
            &'IREC= ', irec,&
            &'-------------'
         end if


!           Calculate Down and Crosswind Distances          ---   CALL XYDIST
         if (awmadwdbg) then
            write(awmadwdbunt,*) '  Receptor  ', irec
         end if

         if (evonly) then
            call xydist(ievent)
         else
            call xydist(irec)
         end if

! ---       First check for receptor exceeding minimum distance (1m) or
!           maximum distance (80km for FASTALL or 1.0D20 otherwise).
         if (distr .lt. 0.99d0 .or. distr .gt. maxdist) then
!              Receptor distance exceeds the minimum or maximum distance;
!              assign 0.0 to applicable arrays, and cycle to next receptor
            hrval(:)  = 0.0d0
            aerplm(:) = 0.0d0
            aerpan(:) = 0.0d0
            prmval(:) = 0.0d0

            if (pvmrm .or. olm .or. arm2 .or.&
            &runttrm .or. grsm) then
! ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                 cycling to the next receptor to avoid
!                 persisting value from previous hour.
               chi(irec,isrc,:) = 0.0d0
               if(grsm)then
                  chi_ttravplm = 0.0d0
                  chi_ttravpan = 0.0d0
                  chi_ttravaer = 0.0d0
                  chi_ttravprm = 0.0d0
                  chi_ttravchm(irec,isrc) = 0.0d0
                  bldfac(irec,isrc) = 0.0d0
                  PRMVAL_Src1 = 0.0d0
               end if
               if (pvmrm .or. grsm) then
                  hecntr(irec,isrc)  = 0.0d0
                  ueffs(irec,isrc)   = 0.0d0
                  epsef(irec,isrc)   = 0.0d0
                  ppfact(irec,isrc)  = 0.0d0
                  hecntr3(irec,isrc) = 0.0d0
                  ueff3s(irec,isrc)  = 0.0d0
                  epsef3(irec,isrc)  = 0.0d0
                  fopts(irec,isrc)   = 0.0d0
               end if
            end if
! ---          Cycle to next receptor
            cycle receptor_loop
         end if

! ---       Calculate AERMOD Concentration Without Downwash, AERVAL
! ---       First calculate coherent plume component using downwind distance
         l_plume = .true.
! ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
         xdist = x

! ---       CALL AERCALC to calculate concentrations without downwash;
!           downwash is handled later in PRMCALC, if applicable
         call aercalc( x, l_plume, aerplm )

! ---       CERC 11/30/20 Calculate the conc*travel time for the Gaussian plume
         if(grsm)then
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               uchm=ueff
            else
               uchm=ueffd
            end if
            if (uchm/=0) then
               chi_ttravplm=max(0.0d0,aerplm(1)*x/uchm) !Don't allow negative conc*travel times
            else
               !Error - no travel time if UCHM is zero
               call errhdl(path, modnam, 'E','601','GRSM')
               runerr=.true.
               return
            end if
         end if

         if (l_effsigy) then
! ---          No "pancake" calculation for non-DFAULT FASTALL option (EFFSIGY),
!              assign AERPLM to AERVAL and set FRAN and AERPAN to 0.0
            aerval = aerplm
            aerpan = 0.0d0
            fran   = 0.0d0
! ---          In this case travel time is just the conc*travel time in the Gaussian plume
            if(grsm)then
               chi_ttravpan=0.0d0
               chi_ttravaer=chi_ttravplm
            end if

         else

! ---          Next calculate random "pancake" component using radial distance
            l_plume = .false.
! ---          Assign XDIST for use in dry depletion (FUNCTION F2INT)
            xdist = distr
! ---          Call AERCALC to get random "pancake" component, AERPAN
            call aercalc( distr, l_plume, aerpan )

! ---          CERC 11/30/20 Calculate the conc*travel time for the "pancake" component
            if(grsm)then
               if (stable .or. (unstab.and.(hs.ge.zi))) then
                  uchm=ueff
               else
                  uchm=ueffd
               end if
               if (uchm/=0) then
                  chi_ttravpan=max(0.0d0,aerpan(1)*xdist/uchm) !Don't allow negative conc*travel time
               else
                  !Error - no travel time if UCHM is zero
                  call errhdl(path, modnam, 'E','601','GRSM')
                  runerr=.true.
                  return
               end if
            end if

! ---          Calculate fraction of random kinetic energy to total kinetic energy.
!              Note that these effective parameters are based on the radial dist.
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               call meandr( ueff, sveff, fran )
            else if (unstab) then
               call meandr( ueffd, sveffd, fran )
               if (ppf .gt. 0.0d0) then
!                    For penetrated source calculate weighted average of
!                    direct/indirect plume component and penetrated component
                  call meandr( ueff3, sveff3, fran3 )
                  fran = ppf*fran3 + (1.0d0-ppf)*fran
               end if
            end if

! Added for TTRM; AECOM
            if (runttrm .and. unstab) then
!CRFL
!CRFL    Revised emission rate terms:  for CHID & CHIN, to QTK*(1-PPF),
!CRFL    and for CHI3 to QTK*PPF.  Ref:  P.D.F. Model for Dispersion in
!CRFL    the Convective Boundary Layer, J.C. Weil, 6/27/94.  Changed
!CRFL    7/19/94, R.F. Lee.
!CRFL
               chid = (qtk*emifac(1) * (1.0d0-ppf) / ueffd) * (fsubyd*fsubzd)
               chin = (qtk*emifac(1) * (1.0d0-ppf) / ueffn) * (fsubyn*fsubzn)
            end if

            if (runttrm) then
               ttrmout(irec,isrc,12) = ueff
               ttrmout(irec,isrc,13) = ueffd
               ttrmout(irec,isrc,14) = ueff3
               ttrmout(irec,isrc,15) = fran
               ttrmout(irec,isrc,16) = ppf
! GAMFACT is saved to TTRMOUT(IREC,ISRC,17) below
               ttrmout(irec,isrc,18) = chin
               ttrmout(irec,isrc,19) = chid
            end if
! End TTRM insert; Feb. 2021

! ---          Combine "random" and "coherent" plume contributions
            aerval = fran*aerpan + (1.0d0-fran)*aerplm

! ---          CERC 11/30/20 Calculate the conc*travel time for the combined plume
            if(grsm)then
               chi_ttravaer=fran*chi_ttravpan +&
               &(1.0d0-fran)*chi_ttravplm
            end if

         end if

!           ENSR STATEMENT
         if(debug) then
            do ityp = 1, numtyp
               write(dbgunt,10) aerpan(ityp), aerplm(ityp), fran,&
               &aerval(ityp)
10             format(/,'AERVAL(ITYP) = FRAN*AERPAN(ITYP) + (1.-FRAN)',&
               &'*AERPLM(ITYP)',//,&
               &'PANCAKE/MEANDER COMPONENT, AERPAN(ITYP) = ',&
               &g16.8,/,'COHERENT PLUME COMPONENT,  AERPLM(ITYP) = ',&
               &g16.8,/,'MEANDER FACTOR, FRAN = ',&
               &g16.8,/,'RESULTANT CONC, AERVAL(ITYP) = ',g16.8,//)
            end do
         end if

         if (wake .and. (stable .or. hs.le.zi)) then
! ---          Apply wake effects - note that wake effects are not applied
!              for stack heights > ZI.
! ---          Calculate receptor coordinates relative to upwind face of bldg.:
!              xbrec is downwind dist. of receptor from upwind
!              bldg face; ybrec is crosswind dist. of receptor from
!              center of upwind bldg. face
            xbrec=x-xadj
            ybrec=y-yadj

            xdist = x
! ---          Calculate PRIME Downwash Concentration, PRMVAL
            call prmcalc ( xbrec, ybrec )
! ---          Check for runtime error (RUNERR) before continuing
            if (runerr) return

! ---          CERC 11/30/20 Calculate conc*travel time when buildings effects included
            if(grsm)then
               if (ueff/=0) then
                  chi_ttravprm=max(0.0d0,prmval(1)*xdist/ueff) !Don't allow negative conc*travel time
               else
                  !Error - no travel time if UEFF is zero
                  call errhdl(path, modnam, 'E','601','GRSM')
                  runerr=.true.
                  return
               end if
            end if

! ---          Calculate Gamma weighting factor, GAMFACT
            call gamcalc ( xbrec, ybrec )

! ---          Calculate hourly concentration from PRIME and AERMOD values (1:NUMTYP)
            hrval =  gamfact  * prmval + (1.0d0-gamfact) * aerval

!              CERC 11/30/20
! ---          Calculate conc*travel time from PRIME and AERMOD values
            if(grsm)then
               chi_ttravchm(irec,isrc) = gamfact*chi_ttravprm +&
               &(1.0d0-gamfact)*chi_ttravaer
               if(hrval(1) > 0.0d0)then
                  bldfac(irec,isrc) =&
                  &gamfact*(prmval(1)-PRMVAL_Src1)/hrval(1)
               endif
            end if

            if (primedbg) then
               do ityp = 1, numtyp
                  write(prmdbunt,*) ' '
                  write(prmdbunt,*) 'YR/MN/DY/HR:     ',kurdat,&
                  &' ISRC: ',isrc,&
                  &' IREC: ',irec
                  write(prmdbunt,*)
                  write(prmdbunt,*) ' GAMFACT = ',gamfact
                  write(prmdbunt,*) ' AERVAL  = ',aerval(ityp)
                  write(prmdbunt,*) ' PRMVAL  = ',prmval(ityp)
                  write(prmdbunt,*) ' HRVAL   = ',hrval(ityp)
                  write(prmdbunt,*) ' '
               end do
            end if

         else
! ---          No WAKE effects or HS > ZI, set GAMFACT to 0.0 and use AERVAL only.
            gamfact = 0.0d0
! ---          Calculate hourly concentration from PRIME and AERMOD values (1:NUMTYP)
            hrval  = aerval
            prmval = 0.0d0
! ---          CERC 11/30/20 Calculate conc*travel time (no buildings)
            if(grsm)then
               chi_ttravchm(irec,isrc) = chi_ttravaer
            end if
         end if
! Added for HBPDEBUG; JAN 2023
! Write out results to debug file
         if (hbplume .and. hbpdbg) then
            if(ppfn .lt. 1)then
               write(hbpunt,7717) kurdat,irec,srcid(isrc),ziconv,zimech,&
               &ziconvn,zimechn,ziavg,he3,sz3dbg,hhtop,&
               &hhbot,htopdif,zrt,ppf,ppfn,hrval!,
!     &                           NOHBP_HRVAL
            else
               write(hbpunt,7817) kurdat,irec,srcid(isrc),ziconv,zimech,&
               &ziconvn,zimechn,zrt,ppf,ppfn,hrval
            endif
7717        format(1x,i8.8,',',i6,',',a12,6(',',f8.2),',',f7.3,',',&
            &4(f8.2,','),f7.3,',',f7.3,',',f14.6)
7817        format(1x,i8.8,',',i6,',',a12,4(',',f8.2),6(',-999.0'),',',&
            &f8.2,',',f7.3,',',f7.3,',',f14.6)
         endif
! End HBPDEBUG insert
!     Added for TTRM; AECOM
         if (runttrm) then
            ttrmout(irec,isrc,10) = x
            ttrmout(irec,isrc,11) = distr
            ttrmout(irec,isrc,20) = y
            ttrmout(irec,isrc,17) = gamfact
            ttrmout(irec,isrc,22) = aerval(1)
            ttrmout(irec,isrc,23) = prmval(1)
!              TTRMOUT(IREC,ISRC,24) = UEFFe ! Effective wind speed for downwash (saved in PRM_PCHI)
         end if
!     End TTRM insert; Feb. 2021

         if (pvmrm) then
! ---          Store data by source and receptor for PVMRM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               hecntr(irec,isrc) = he
               ueffs(irec,isrc)  = ueff
               epsef(irec,isrc)  = epseff
            else
               hecntr(irec,isrc) = center
               ueffs(irec,isrc)  = ueffd
               epsef(irec,isrc)  = epseffd
            end if
            if (ppf .gt. 0.0d0) then
               ppfact(irec,isrc)  = ppf
               hecntr3(irec,isrc) = he3
               ueff3s(irec,isrc)  = ueff3
               epsef3(irec,isrc)  = epseff3
            else
               ppfact(irec,isrc)  = 0.0d0
               hecntr3(irec,isrc) = 0.0d0
               ueff3s(irec,isrc)  = 0.0d0
               epsef3(irec,isrc)  = 0.0d0
            end if
            fopts(irec,isrc) = fopt

!              Initialize __VAL arrays (1:NUMTYP)
            hrval   = 0.0d0
            aerval  = 0.0d0
            aerplm  = 0.0d0
            aerpan  = 0.0d0
            prmval  = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

         else if (olm) then
! ---          Store conc by source and receptor for OLM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval   = 0.0d0
            aerval  = 0.0d0
            aerplm  = 0.0d0
            aerpan  = 0.0d0
            prmval  = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop
!     Added for TTRM; AECOM
         else if (runttrm) then
! ---          Store conc by source and receptor for TTRM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval   = 0.0d0
            aerval  = 0.0d0
            aerplm  = 0.0d0
            aerpan  = 0.0d0
            prmval  = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop
!    End TTRM insert; Feb. 2021

         else if (arm2) then
! ---          Store conc by source and receptor for ARM2 options
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval   = 0.0d0
            aerval  = 0.0d0
            aerplm  = 0.0d0
            aerpan  = 0.0d0
            prmval  = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

!           CERC 11/30/20
         else if (grsm) then
! ---          Store conc by source and receptor for GRSM options
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

            if (stable .or. (unstab.and.(hs.ge.zi))) then
               hecntr(irec,isrc) = he
               ueffs(irec,isrc)  = ueff
               epsef(irec,isrc)  = epseff
            else
               hecntr(irec,isrc) = center
               ueffs(irec,isrc)  = ueffd
               epsef(irec,isrc)  = epseffd
            end if
            if (ppf .gt. 0.0d0) then
               ppfact(irec,isrc)  = ppf
               hecntr3(irec,isrc) = he3
               ueff3s(irec,isrc)  = ueff3
               epsef3(irec,isrc)  = epseff3
            else
               ppfact(irec,isrc)  = 0.0d0
               hecntr3(irec,isrc) = 0.0d0
               ueff3s(irec,isrc)  = 0.0d0
               epsef3(irec,isrc)  = 0.0d0
            end if
            fopts(irec,isrc) = fopt

!              Initialize __VAL arrays (1:NUMTYP)
            hrval   = 0.0d0
            aerval  = 0.0d0
            aerplm  = 0.0d0
            aerpan  = 0.0d0
            prmval  = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

         end if

!           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
!           As noted just above, the calls to EV_SUMVAL and SUMVAL
!           are skipped for a PVMRM, OLM, ARM2 or GRSM run; the
!           summing is performed for these options in the
!           respective routines
         if (evonly) then
            call ev_sumval
         else
            do igrp = 1, numgrp
               call sumval
            end do
         end if

         if (eval(isrc)) then
!              Check ARC centerline values for EVALFILE
!              output                              ---   CALL EVALCK
            call evalck
         end if

!           Initialize __VAL arrays (1:NUMTYP)
         hrval   = 0.0d0
         aerval  = 0.0d0
         aerplm  = 0.0d0
         aerpan  = 0.0d0
         prmval  = 0.0d0

      end do receptor_loop
!        End Receptor LOOP

!        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
      if (eval(isrc)) then
         call evalfl
      end if

   end if

   return
end

subroutine aercalc( xarg, l_plume, aerout )
!***********************************************************************
!             AERCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the AERMOD concentration without downwash
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:     November 10, 2000
!
!        CHANGES:
!                  Added gradual plume rise adjustment (PLAT_GRADPLUMADJ)
!                  when PLATFORM is present.
!                  Michelle G. Snyder, WOOD, 8/5/2021
!
!                  Added debug statement based on ENSR code.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
!
!        INPUTS:   XARG         - Real - Distance (m), downwind for coherent
!                                        plume component (X) and radial for
!                                        random component (DISTR)
!                  L_PLUME      - Log  - Specifies coherent plume calculation
!                                        if TRUE, otherwise random component
!
!        OUTPUTS:  AEROUT(NTYP) - Real - AERMOD component of concentration
!                                        without building downwash for either
!                                        coherent plume component or for
!                                        random component, depending on
!                                        L_PLUME.
!
!        CALLED FROM:   PCALC
!
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   integer :: j
   double precision :: aerout(numtyp), aertmp(numtyp), fyout, xarg,&
   &adj, fran, fran3
   logical :: l_plume
! Unused:       INTEGER :: NDXZMID
! Unused:       DOUBLE PRECISION :: ZMID, VALABV, VBELOW

!MGS  Wood 8/5/2021 D063 Added to make plume rise adjustment due to PLATFORM
!MGS  External Functions:
   double precision, external  :: plat_gradplumadj

!     Variable Initializations
   modnam = 'AERCALC'

!     Initialize AEROUT(NUMTYP) and AERTMP(NUMTYP) arrays
   aerout(:) = 0.0d0
   aertmp(:) = 0.0d0

! --- Initialize FRAN and FRAN3
   fran  = 0.0d0
   fran3 = 0.0d0

   if (xarg .lt. 1.0d0) then
!        Receptor is too close to source for calculation
!        or upwind of source for coherent plume component
      aerout(:) = 0.0d0
      return

   end if

!     Determine Deposition Correction Factors
   if (npd .eq. 0 .and. (ldgas .or. lwgas)) then
      call pdepg (xarg)
   else
      dqcorg = 1.0d0
      wqcorg = 1.0d0
   end if
   if (npd .gt. 0 .and. (ldpart .or. lwpart)) then
      call pdep (xarg)
   else if (npd .gt. 0) then
!        Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
      dqcor = 1.0d0
      wqcor = 1.0d0
   end if

!     Set initial effective parameters
   ueff  = us
   sveff = svs
   sweff = sws
   tgeff = tgs
   if ( unstab  .and.  (hs .lt. zi) ) then
      ueffd  = us
      sveffd = svs
      sweffd = sws
      ueffn  = us
      sveffn = svs
      sweffn = sws
      ueff3  = us
      sveff3 = svs
      sweff3 = sws
      tgeff3 = tgs
   end if

!RJP  Add temporary debugging statement here.

!     ENSR ENHANCEMENT OF WRITE STATEMENT TO IDENTIFY COMPONENT CONCENTRATION
   if(debug) then
      write(dbgunt, 6014) srcid(isrc)
6014  format(//,'SRCID: ', a8)
      if(l_plume)then
         write(dbgunt, 6015) ueff, sveff, sweff
6015     format(//,'COHERENT PLUME COMPONENT',/,5x,&
         &'Initial effective parameters for ',&
         &'stable or direct convective ',&
         &'plume:',//,5x,'Ueff = ',f7.2,' m/s; ',&
         &'SVeff = ',f7.2,&
         &' m/s; SWeff = ',f7.2,' m/s.',/)
      else
         write(dbgunt, 6016) ueff, sveff, sweff
6016     format(//,'MEANDER COMPONENT',/,5x,&
         &'Initial effective parameters for ',&
         &'stable or direct convective ',&
         &'plume:',//,5x,'Ueff = ',f7.2,' m/s; ',&
         &'SVeff = ',f7.2,&
         &' m/s; SWeff = ',f7.2,' m/s.',/)
      end if
   end if

!     Define plume centroid height (CENTER) for use in
!     inhomogeneity calculations
   call centroid ( xarg )

!     Calculate the plume rise                     ---   CALL DELTAH
   call deltah ( xarg )

!     If the atmosphere is unstable and the stack
!     top is below the mixing height, calculate
!     the CBL PDF coefficients                     ---   CALL PDF
   if( unstab  .and.  (hs .lt. zi) ) then
      call pdf
   end if

!     Determine Effective Plume Height             ---   CALL HEFF
   call heff ( xarg )
! ----------------------------------------------------------------------
! ---------- BEGIN PLATFORM DOWNWASH PLUME RISE ADJUSTMENT -------------
! ----------------------------------------------------------------------
!        Adjust plume rise if PLATFORM is present. This will decrease the
!        effective height when there is a PLATFORM due to downwash effects
!        of the structure on top of the platform. Calls PLAT_GRADPLUMEADJ,
!        PLAT_DOWNWASH, and PLAT_CUBIC (all contained in calc1.f).
!        Michelle G. Snyder, WOOD, 8/5/2021
!CRT     D063
   if (osplat(isrc) .and. (xarg > 0.0d0)) then
      if (stable) then
         dhp = dhp - plat_gradplumadj( xarg )
      else !modify direct plume downdraft (DHP1) only
         dhp1 = dhp1 - plat_gradplumadj( xarg )
      end if

!           Get new effective heights based on adjusted plume rise from platform
      call heff ( xarg )

   end if

! ----------------------------------------------------------------------
! --------- END OF PLATFORM DOWNWASH PLUME RISE ADJUSTMENT -------------
!----------             RESUME NORMAL PROCESSING           -------------
! ----------------------------------------------------------------------

!     Compute effective parameters using an
!     average through plume layer
   call iblval ( xarg )

!     Call PDF & HEFF again for final CBL plume heights
   if (unstab .and. (hs.lt.zi) ) then
      call pdf
      call heff ( xarg )
   end if

!     Determine Dispersion Parameters              ---   CALL PDIS
   call pdis ( xarg )

!     Calculate the 'y-term' contribution to
!     dispersion, FSUBY
   if (l_plume) then
      if (l_effsigy) then
! ---       Calculate fraction of random kinetic energy to total kinetic energy
!           for FASTALL option to optimize meander using effective sigma-y.
!           Note that these effective parameters are based on the downwind distance,
!           rather than the radial distance used in the standard meander approach
         if (stable .or. (unstab.and.(hs.ge.zi))) then
            call meandr( ueff, sveff, fran )
         else if (unstab) then
            call meandr( ueffd, sveffd, fran )
            if (ppf .gt. 0.0d0) then
!                 For penetrated source calculate weighted average of
!                 direct/indirect plume component and penetrated component
               call meandr( ueff3, sveff3, fran3 )
               fran = ppf*fran3 + (1.0d0-ppf)*fran
            end if
         end if

!           Calculate effective sigma-y for non-DFAULT FASTALL option (L_EFFSIGY)
         syeff = 1.0d0/((fran/(srt2pi*xarg)) + (1.0d0-fran)/sy)
         if (l_effsigy) then
            if (dabs(y) .gt. numsyeff*syeff) then
!                 Plume is more than 4 sigmas off centerline, skip calculation
               fyout = 0.0d0
            else
!                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
               call fyplm(syeff,fyout)
            end if
         else
            if (x.lt.1.0d0 .or. dabs(y) .gt. numsyeff*syeff) then
!                 Receptor is upwind of source or more than 4 sigmas off
!                 ceenterline, skip calculation
               fyout = 0.0d0
            else
!                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
               call fyplm(syeff,fyout)
            end if
         end if

      else
!           Calculate FSUBY for coherent plume        ---   CALL FYPLM
         call fyplm(sy,fyout)

      end if

   else
!        Calculate FSUBY for random component      ---   CALL FYPAN
      call fypan(fyout)

   end if

   fsuby  = fyout
   fsubyd = fsuby
   fsubyn = fsubyd

!     Calculate the 'y-term' contribution to dispersion
!     for the penetrated plume, FSUBY3
   if( unstab  .and.  (hs .lt. zi)  .and. (ppf .gt. 0.0d0) )then
!        Compute meander fraction of horizontal distribution function
!        from Venky's memo of 6/24/98.
      if (l_plume) then
         if (l_effsigy) then
! ---          Calculate fraction of random kinetic energy to total kinetic energy
!              for FASTALL option to optimize meander using effective sigma-y.
!              Note that these effective parameters are based on the downwind distance,
!              rather than the radial distance used in the standard meander approach
            syeff = 1.0d0/((fran/(srt2pi*xarg)) + (1.0d0-fran)/sy3)

            if (dabs(y) .gt. numsyeff*syeff) then
!                 Plume is more than 4 sigmas off centerline, skip calculation
               fyout = 0.0d0
            else
!                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
               call fyplm(syeff,fyout)
            end if

         else
!              Calculate FSUBY for coherent plume        ---   CALL FYPLM
            call fyplm(sy3,fyout)
         end if
      else
!           Calculate FSUBY for random component   ---   CALL FYPAN
         call fypan(fyout)
      end if

      fsuby3 = fyout
   else
      fsuby3 = 0.0d0
   end if

!     Check for zero "y-terms"; if zero then skip calculations
!     and go to next receptor.
   if( fsuby.eq.0.0d0 .and. fsuby3.eq.0.0d0 )then
!        Set AEROUT(NUMTYP) array to 0.0
      aerout(:) = 0.0d0

   else

      if (npd .eq. 0) then
!           Perform calculations for gases
!           Assign plume tilt, HV = 0.0
         hv = 0.0d0

         adj = dqcorg * wqcorg

         if (stable .or. (unstab.and.(hs.ge.zi))) then
!              Calculate height of the "effective reflecting surface"
            call refl_ht (he, xarg, szb, 0.0d0, hsbl)
         elseif ( unstab ) then
            hsbl = 0.0d0
         end if

         if (unstab .and. (hs.lt.zi) .and. (ppf.gt.0.0d0)) then
!              Calculate height of the "effective reflecting surface"
            call refl_ht (he3, xarg, szb3, 0.0d0, hpen)
         else
            hpen = 0.0d0
         end if

!           Determine the CRITical Dividing Streamline---   CALL CRITDS
         call critds (he)

!           Calculate the fraction of plume below
!           HCRIT, PHEE                               ---   CALL PFRACT
         call pfract (he)

!           Calculate FOPT = f(PHEE)                  ---   CALL FTERM
         call fterm

!           Calculate AERMOD Concentration     ---   CALL AER_PCHI
         call aer_pchi( xarg, adj, vdepg, 0, aerout(:) )

      else
!           Perform calculations for particles, loop through particle sizes

!           Begin loop over particle sizes
         do j = 1, npd

!              Calculate Plume Tilt Due to Settling, HV
            hv = (xarg/us) * vgrav(j)

!              Adjust Jth contribution by mass fraction and source
!              depletion
            adj = phi(j) * dqcor(j) * wqcor(j)

            if (stable .or. (unstab.and.(hs.ge.zi))) then
!                 Calculate height of the "effective reflecting surface"
!                 Calculate Settled Plume Height(s), HESETL
               hesetl = max( 0.0d0, he - hv )
               call refl_ht (hesetl, xarg, szb, 0.0d0, hsbl)
            elseif ( unstab ) then
               hesetl = max( 0.0d0, 0.5d0*(hed1+hed2) - hv )
               hsbl = 0.0d0
            end if

            if (unstab .and. (hs.lt.zi) .and. (ppf.gt.0.0d0)) then
!                 Calculate height of the "effective reflecting surface"
!                 Calculate Settled Plume Height(s), HE3SETL
               he3setl = max( 0.0d0, he3 - hv )
               call refl_ht (he3setl, xarg, szb3, 0.0d0, hpen)
               hpen = max( hpen, zi )
            else
               hpen = 0.0d0
            end if

!              Determine the CRITical Dividing Streamline---   CALL CRITDS
            call critds (hesetl)

!              Calculate the fraction of plume below
!              HCRIT, PHEE                               ---   CALL PFRACT
            call pfract (hesetl)

!              Calculate FOPT = f(PHEE)                  ---   CALL FTERM
            call fterm

!              Calculate AERMOD Concentration            ---   CALL AER_PCHI
            call aer_pchi( xarg, adj, vdep(j), j, aertmp(:) )
            aerout(:) = aerout(:) + aertmp(:)

         end do
!           End loop over particle sizes

      end if

   end if

   return
end

subroutine prmcalc (xbrec, ybrec)
!***********************************************************************
!             PRMCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the PRIME downwash component of the
!                 concentration
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:     November 10, 2000
!
!        MODIFIED:
!                  Modified to place receptor on centerline of cavity
!                  plumes by setting Y2 = 0.0 for SCREEN option.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
!
!                  AWMA PRIME2 Subcommittee
!                  - Changed definition of ZLO, ZHI
!                  - Added provisions for U30, SV30, SW30 for use
!                    in wake_u_turb.
!                  - USE module cpp_wakedat to transfer values
!
!        INPUTS:  XBREC - Real    - Downwind distance (m) of receptor
!                                   from upwind edge of building
!                 YBREC - Real    - Lateral distance (m) of receptor from
!                                   center of upwind edge of building
!
!        OUTPUTS: PRMVAL(NTYP) - Real - PRIME downwash component of
!                                       concentration
!
!        CALLED FROM:   PCALC
!
!***********************************************************************
!     Variable Declarations
   use main1

!     The following are for AWMA ALPHA option
   use prime_wakedat, only: hb
   use PRM2_wakedat, only:  Zeff_PRM2, u30, sv30, sw30,&
   &Ueff_save, SWeff_save, SVeff_save

   implicit none

   character modnam*12
   integer :: ipositn, n1, n2, is, j
   double precision :: adj
   double precision :: dhpout, syout, szout, fyout
   double precision :: xbrec, ybrec, fqcav, sycav,&
   &szcav
   double precision :: zhi, zlo

!     The following two lines are for AWMA ALPHA option
   double precision :: BetaJ
   double precision :: z30hi, z30lo

   integer :: ndxbhi, ndxblo, ndxalo
! --- Declare local PRIME arrays for "3-source" data
   double precision :: q2(3),y2(3),sy2(3),z2(3),h2(3),sz2(3),qc2(3),&
   &qtksav,ppfsav

   logical :: l_inwake

!     Variable Initializations
   modnam = 'PRMCALC'
   prmval = 0.0d0
   if(grsm) PRMVAL_Src1 = 0.0d0
   Ueff_save  = 0.0d0
   SWeff_save = 0.0d0
   SVeff_save = 0.0d0

! --- PRIME ---------------------------------------------------------
! --- Calculate where receptor is relative to near-wake cavity
!     and building (IPOSITN=1 for within bldg; 2=within
!     near-wake, 3=within far wake; 4=outside)
! --- Note:  xbrec is downwind dist. of receptor from upwind
!     bldg face; ybrec is crosswind dist. of receptor from
!     center of upwind bldg. face                  ---  CALL POSITION
   call position(xbrec,ybrec,zflag,ipositn)

   if(ipositn.eq.4 .and. x.le.0.0d0) then
! ---    Receptor is upwind of sources and is not within
! ---    a building wake - use AERMOD calculation
! ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
      prmval = aerval
      if(grsm) PRMVAL_Src1 = aerval(1)

   elseif(ipositn.ne.2 .and. distr .lt. 0.99d0) then
! ---    Receptor Too Close to Source for Calculation and is not
! ---    within a building near-wake (cavity) - use AERMOD calculation
! ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
      prmval = aerval
      if(grsm) PRMVAL_Src1 = aerval(1)
! -------------------------------------------------------------

   else if (.not. wake) then
! ---    No wake effects for this source for this hour - use AERMOD calculation
! ---    Set PRMVAL(NUMTYP) array = AERVAL(NUMTYP) array
      prmval = aerval
      if(grsm) PRMVAL_Src1 = aerval(1)

   else
! ---    Calculate PRIME concentration with downwash

! ---    Calculate effective parameters to define ambient turbulence intensities,
!        as averages across layer from ground to top of wake (as calculated at
!        a downwind distance of 15R).
      zhi = 1.2d0*rscale * (15.0d0 +&
      &(dsbh/(1.2d0*rscale))**3)**third
      if (unstab) then
         zhi = min( zhi, zi )
      end if
      zlo = 0.0d0

! --- AWMA version D20350
      if (L_AWMA_UTurb .or. L_AWMA_UTurbHX) then
!---        The values of ZHI and ZLO in the regulatory version of
!           AERMOD are replaced with the values below for AWMADWNW option
         BetaJ = 1.0d0/3.0d0 + us/vs
         Zeff_PRM2 = 0.9d0 * (fm * us/ustar)**0.5d0 / (us * BetaJ)
         Zeff_PRM2 = Zeff_PRM2 + hs
         Zeff_PRM2 =  max(Zeff_PRM2,hb,5.0d0)

         zhi = Zeff_PRM2 + 5.0d0
         zlo = Zeff_PRM2 - 5.0d0
      endif

! ---    Compute average values between ZLO and ZHI
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

! ---    AWMA version D20350
!        If the AWMA enhanced downwash options are applied,
!          save the effective parameters so they can be restored later
!
      if (L_AWMA_UTurbHX) then
         Ueff_save  = Ueff
         SWeff_save = SWeff
         SVeff_save = SVeff
      end if

! ---    AWMA version D20350
      if (L_AWMA_UTurb  .or. L_AWMA_UTurbHX) then
!---        Additional calculations for AWMADW -
!              "effective" parameters (at 30m) for use in wake_u_turb
!           Note: alternate algorithm to extract 30m value directly may
!                 be preferred, but for now using method parallel to above.
!                 Requires averaging the values over a narrow range
!                 centered on 30m. Does this reliably work?
         z30lo = 28.d0
         z30hi = 32.d0
         call locate(gridht, 1, mxglvl, Z30lo, ndxblo)
         call locate(gridht, 1, mxglvl, z30hi, ndxbhi)

!           Define index above based on index below to obtain correct
!           values from subr.anyavg
         ndxalo = ndxblo + 1

         call anyavg (mxglvl, gridht, gridws, z30lo, ndxalo,&
         &z30hi, ndxbhi, u30)
         !---
         call anyavg (mxglvl, gridht, gridsv, z30lo, ndxalo,&
         &z30hi, ndxbhi, sv30)
         !----
         call anyavg (mxglvl, gridht, gridsw, z30lo, ndxalo,&
         &z30hi, ndxbhi, sw30)
         !----
      end if

!RWB     Modify treatment of low wind/low turbulence cases.
!RWB     R. Brode, PES, 8/15/96

      sweff = max( sweff, swmin )

      sveff = max( sveff, svmin, svumin*ueff )

      if( l_vectorws )then
         ueff  = dsqrt( ueff*ueff + 2.0d0*sveff*sveff )
      endif

      ueff  = max( ueff, wsmin )

! ---    AWMA version D20350
      if (L_AWMA_UTurb .or. L_AWMA_UTurbHX )then
         sw30 =  max (sw30, swmin)
         sv30 =  max (sv30, svmin, svumin*u30)
         if (l_vectorws) u30 = dsqrt (u30*u30 +2.d0*sv30*sv30)
         u30 = max (u30, wsmin)
      end if

      if (primedbg) then
         write(prmdbunt,*) 'PRIME Effective Parameters: '
         if (L_AWMA_UTurb .or. L_AWMA_UTurbHX) then
            write(prmdbunt,*) '  AWMAUturb/HX applied to eff params'
         end if
         write(prmdbunt,*)   '  ZLO, ZHI     = ', zlo, zhi
         write(prmdbunt,*)   '  SWEFF, SVEFF = ', sweff, sveff
         write(prmdbunt,*)   '  UEFF,  TGEFF = ', ueff, tgeff
      end if

!        Calculate the plume rise                     ---   CALL PRMDELH
      call prmdelh ( x, l_inwake )

! ---    Check for runtime error (RUNERR) before continuing
      if (runerr) return

      if (.not. l_inwake) then
!           Plume is not affected by wake, set PRMVAL = AERVAL and return
         prmval = aerval
         if(grsm) PRMVAL_Src1 = aerval(1)
         return
      end if

!        Determine Effective Plume Height             ---   CALL PRMHEFF
      call prmheff

      if (unstab .and. he .ge. zi) then
!           Plume is above ZI, set PRMVAL = AERVAL and return
         prmval = aerval
         if(grsm) PRMVAL_Src1 = aerval(1)
         return
      end if

! ---    Calculate sigmas
      dhpout = dhp
      call wake_xsig(x,hs,dhpout,nobid,szout,syout,&
      &szcav,sycav)
      sy = syout
      sz = szout

! ---    PRIME ---------------------------------------------------
! ---    When there is a building wake, consider treatment of mass in
! ---    cavity as additional sources, or as only source
      qtksav = qtk
      ppfsav = ppf
! ---    Place selected plume data into transfer arrays (first element)
      q2(1)  = qtk
      y2(1)  = y
      sy2(1) = sy
      z2(1)  = zflag
      h2(1)  = he
      sz2(1) = sz
      n1 = 1
      n2 = 1
      if(wake) then
! ---       Define cavity source                              ---   CALL CAV_SRC
         call cav_src(x,y,zflag,fqcav,qc2,h2,y2,z2,sz2,sy2,n1,n2)
         if (screen) then
! ---          Force receptor to be on "centerline" for all plumes for SCREEN
            y2 = 0.0d0
         end if
         if(fqcav.gt.0.0d0) then
! ---          Set source strengths
            q2(1)=qtk*(1.0d0-fqcav)
            q2(2)=qtk*fqcav*qc2(2)
            q2(3)=qtk*fqcav*qc2(3)
         end if
      end if

! ---    Initialize PRMVAL(NUMTYP) output array values to zero, because contributions
! ---    due to more than one source are summed here (or do loop may
! ---    not execute if neither source contributes)
      prmval = 0.0d0
! ---    Initialise source 1 concentration for GRSM
      if(grsm)then
         PRMVAL_Src1 = 0.0d0
      endif

! ---    Loop over 3 possible sources (is=1 for primary source,
! ---    is=2 for "outside" cavity source, and is=3 for "inside" cavity source)
!         IF (AWMADWDBG) THEN
!            WRITE(AWMADWDBUNT,*) '-----------------------------'
!            WRITE(AWMADWDBUNT,*) ' In PRMCALC: Src ', SRCID(ISRC),
!     &                           'Recep: ',IREC
!         END IF

      do is = n1, n2
!         IF (AWMADWDBG) THEN
!            WRITE(AWMADWDBUNT,*) ' ---> Processing source "is" ',is
!            WRITE(AWMADWDBUNT,*)'      (1=primary, 2=outside cav,',
!     &                                        ' 3=inside cav)'
!         END IF
! ---       Cycle to next source if emission rate is 0.0
         if (q2(is) .eq. 0.0d0) cycle

! ---       Transfer data for current source
         qtk = q2(is)
         y   = y2(is)
         sy  = sy2(is)
         sz  = sz2(is)
         he  = h2(is)
         zflag = z2(is)

! -------------------------------------------------------------
!           Calculate the 'y-term' contribution to
!           dispersion, FSUBY                              ---   CALL FYPLM
         call fyplm(sy,fyout)
         fsuby  = fyout

         if( fsuby.eq.0.0d0 )then
! ---          Lateral term is 0.0, set PRMVAL array (1:NUMTYP) to 0.0.
            prmval = 0.0d0
            if(grsm) PRMVAL_Src1 = 0.0d0

         else

! ---          Set FOPT = 0.5 for PRIME calculation since wake is "near neutral"
            fopt = 0.5d0

            if (npd .eq. 0) then
!                 Determine Deposition Correction Factors
               if ((ldgas.or.lwgas) .and. is.ne.3 .and.&
               &x.gt.1.0d0) then
!                    Do not apply depletion for "inside cavity source", IS=3
                  call prm_pdepg (x)

!                    Reassign plume height and sigmas, which may have changed
!                    during integration
                  sy  = sy2(is)
                  sz  = sz2(is)
                  he  = h2(is)
               else
                  dqcorg = 1.0d0
                  wqcorg = 1.0d0
               end if

               adj = dqcorg * wqcorg
               call prm_pchi( adj, vdepg, 0, is )                    ! ORD (EMM) change

            else
               if ((ldpart.or.lwpart) .and. is.ne.3 .and.&
               &x.gt.1.0d0) then
!                    Do not apply depletion for "inside cavity source", IS=3
                  call prm_pdep (x)

!                    Reassign plume height and sigmas, which may have changed
!                    during integration
                  sy  = sy2(is)
                  sz  = sz2(is)
                  he  = h2(is)
               else
!                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                  dqcor = 1.0d0
                  wqcor = 1.0d0
               end if

               do j = 1, npd

                  adj = phi(j) * dqcor(j) * wqcor(j)
                  hv  = (x/us) * vgrav(j)
                  he  = max ( 0.0d0, he - hv )

                  call prm_pchi( adj, vdep(j), j, is )               ! ORD (EMM) change

               end do
            end if

!              If this is source 1, store the concentration for GRSM
            if(grsm .and. is==1) PRMVAL_Src1 = prmval(1)

         end if

      end do

! ---    Restore original plume data
      qtk = qtksav
      ppf = ppfsav
      y   = y2(1)
      sy  = sy2(1)
      sz  = sz2(1)
      he  = h2(1)
      zflag = z2(1)

   end if

   return
end


subroutine gamcalc ( xarg, yarg )
!***********************************************************************
!             GAMCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the Gamma weighting factor to combine
!                 AERMOD and PRIME concentrations
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:     July 19, 2001
!
!        INPUTS:   XARG - Real - Downwind distance (m) of receptor
!                                from upwind edge of building
!                  YARG - Real - Lateral distance (m) of receptor from
!                                center of upwind edge of building
!
!        OUTPUTS:  GAMFACT - Real - Gamma weighting factor to combine
!                                   AERMOD and PRIME concentrations
!
!        CALLED FROM:   PCALC
!
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: xarg, yarg
   double precision :: wake_len, wake_wid, wake_hgt
   double precision :: xay, xaz, xamx
   double precision :: sigma_xg,  sigma_yg,  sigma_zg,&
   &exparg_xg, exparg_yg, exparg_zg

! --- Variable Initializations
   modnam = 'GAMCALC'

   if (xarg .le. 0.0d0 .or. .not.wake) then
! ---    Receptor is upwind of building or no WAKE, set GAMFACT = 0.0 to
! ---    use AERVAL only.
      gamfact = 0.0d0

   else

! ---    Calculate the height, half-width and "length" of the wake.
! ---    Length of wake is measured from upwind edge of building.
      wake_hgt = 1.2d0*rscale * (xarg/rscale +&
      &(dsbh/(1.2d0*rscale))**3)**third
      wake_wid = 0.5d0*dsbw + (rscale/3.0d0)*&
      &(xarg/rscale)**third
! ---    Obtain distance to transition from wake to ambient turbulence,
! ---    without cap at 15R.
      call wake_xa2(dsbl,rscale,xaz,xay)
      xamx = max(xaz,xay)
! ---    Set WAKE_LEN as maximum of 15R and transition distance
      wake_len = max(15.0d0 * rscale, xamx)

! ---    Assign wake dimensions to SIGMA_?G terms
      sigma_xg = wake_len
      sigma_yg = wake_wid
      sigma_zg = wake_hgt

! ---    Calculate exponential argument for alongwind dimension
      if (xarg .le. sigma_xg) then
         exparg_xg = 0.0d0
      else
         exparg_xg = -((xarg-sigma_xg)**2 / (2.0d0 * sigma_xg**2))
      end if

! ---    Calculate exponential argument for crosswind dimension
      if (dabs(yarg) .le. sigma_yg) then
         exparg_yg = 0.0d0
      else
         exparg_yg = -((dabs(yarg)-sigma_yg)**2 /&
         &(2.0d0 * sigma_yg**2))
      end if

! ---    Calculate exponential argument for vertical dimension, using ZRT,
! ---    height of receptor above stack base, including terrain and flagpole.
      if (zrt .le. sigma_zg) then
         exparg_zg = 0.0d0
      else
         exparg_zg = -((zrt-sigma_zg)**2 / (2.0d0 * sigma_zg**2))
      end if

!        Calculate gamma weighting factor, GAMFACT
      if (exparg_xg.gt.explim .and. exparg_yg.gt.explim .and.&
      &exparg_zg.gt.explim) then
         gamfact = dexp(exparg_xg)*dexp(exparg_yg)*dexp(exparg_zg)
      else
         gamfact = 0.0d0
      end if

   end if

   return
end


subroutine centroid (xarg)
!***********************************************************************
!             CENTROID Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the plume centroid height, and sets the
!                 SURFAC logical variable
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:     November 10, 2000
!
!        MODIFIED: Modified for Aircraft's Plume Rise for
!                  VOLUME/AREA Sources only for Aircraft Source Group.
!                  Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                  04/01/2023
!
!        INPUTS:  Downwind distance, XARG (m)
!
!        OUTPUTS: Plume centroid height, CENTER
!                 Surface logical variable, SURFAC
!
!        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
!
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: xarg, delx, delz, frac, xtmp

!     Variable Initializations
   modnam = 'CENTROID'

   xtmp = max( xarg, 1.0d0 )

   if( unstab  .and.  (hs .lt. zi) ) then
!----    Calculate plume centerline height without PDF adjustments
      if (srctyp(isrc)(1:5) .eq. 'POINT') then
         call cblprd ( xtmp )
         hteff = min (hsp+dhp1, zi)
!**  Added for Aircraft Plume Rise; UNC-IE
      else if (srctyp(isrc) .eq. 'VOLUME' .and.&
      &aftsrc(isrc) .eq. 'Y') then
         call acblprd ( xtmp )
         hteff = min (hsp+dhp1, zi)
      else if (srctyp(isrc) (1:4) .eq. 'AREA' .and.&
      &aftsrc(isrc) .eq. 'Y') then
         call acblprd ( xtmp )
         hteff = min (hsp+dhp1, zi)
!**  End Aircraft Plume Rise insert; April 2023
      else
         hteff = hsp
! ---       Assign DHCRIT = 0.0  and XFINAL = 0.0 for non-POINT and non-aircraft sources
         dhcrit = 0.0d0
         xfinal = 0.0d0
      end if
      if( xtmp .lt. xfinal) then
         center = hteff
      else if( xtmp .ge. xmixed) then
         center = zmidmx
      else
         delx = xmixed - xfinal
         delz = zmidmx - min( hsp+dhcrit, zi)
         frac = (xtmp-xfinal)/delx
         center = min(hsp+dhcrit,zi) + frac * delz
      end if

!----    Determine if this is a surface layer release
      if( center .lt. 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

   else if( unstab .and. (hs .ge. zi) )then
      surfac = .false.
      center = hsp
! ----   Account for distance-dependent CENTER and SURFAC
      if( center .lt. 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if
   else
!----    Assign centroid height to release height
      center = hsp
! ----   Account for distance-dependent CENTER and SURFAC
      if( center .lt. 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

   end if

   return
end


subroutine refl_ht (hearg, xarg, szbarg, vsigzarg, herefl)
!***********************************************************************
!             REFL_HT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates height of the "effective reflecting surface"
!                 for stable plumes, including penetrated source for
!                 point sources.
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:     November 10, 2000
!
!        INPUTS:  Height of plume (m),                HEARG
!                 Downwind distance (m),              XARG
!                 Bouyancy induced dispersion (m),    SZBARG
!                 Virtual source dispersion term (m), VSIGZARG
!
!        OUTPUTS: Effective height of the reflecting surface (m), HEREFL
!
!        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
!
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   integer :: ndxhe
   double precision :: hearg, herefl, xarg, szbarg, vsigzarg,&
   &svrefl, swrefl, urefl, tgrefl, ptrefl,&
   &ttravl, bvfrq, ztmp, szrefl, szrf, sigf, szhsbl
! Unused:       DOUBLE PRECISION :: SVREFL2

!     Variable Initializations
   modnam = 'REFL_HT'

!---- Compute the height of the "effective reflecting surface"
!---- for stable plumes, HEREFL
!---- First locate index below HEARG
   call locate(gridht, 1, mxglvl, hearg, ndxhe)

   if (ndxhe .ge. 1) then

!---- Sigma_V at HEARG
      call gintrp( gridht(ndxhe), gridsv(ndxhe),&
      &gridht(ndxhe+1), gridsv(ndxhe+1),&
      &hearg, svrefl )

!---- Sigma_W at HEARG
      call gintrp( gridht(ndxhe), gridsw(ndxhe),&
      &gridht(ndxhe+1), gridsw(ndxhe+1),&
      &hearg, swrefl )

!---- Wind speed at HEARG
      call gintrp( gridht(ndxhe), gridws(ndxhe),&
      &gridht(ndxhe+1), gridws(ndxhe+1),&
      &hearg, urefl )

!---- Temperature gradient at HEARG
      call gintrp( gridht(ndxhe), gridtg(ndxhe),&
      &gridht(ndxhe+1), gridtg(ndxhe+1),&
      &hearg, tgrefl )

!---- Potential temperature at HEARG
      call gintrp( gridht(ndxhe), gridpt(ndxhe),&
      &gridht(ndxhe+1), gridpt(ndxhe+1),&
      &hearg, ptrefl )

   else
      svrefl = gridsv(1)
      swrefl = gridsw(1)
      urefl  = gridws(1)
      tgrefl = gridtg(1)
      ptrefl = gridpt(1)
   end if

!---- Apply minimum wind speed and turbulence checks to values
!     at HEARG
!
   swrefl = max( swrefl, swmin )
   svrefl = max( svrefl, svmin, svumin*urefl )
   if( l_vectorws )then
      urefl  = dsqrt( urefl*urefl + 2.0d0*svrefl*svrefl )
   endif
   urefl  = max( urefl, wsmin )

!     Compute surface sigma-z term for stable conditions
   if (stable) then
      szsurf = (rtof2/rtofpi) * ustar * (xarg/urefl) *&
      &(1.0d0 + 0.7d0*xarg/obulen)**(-1.0d0*third)
   else
      szsurf = 0.0d0
   end if

!     Compute ambient sigma-z term at HEARG
   ttravl = xarg / urefl
!---- Apply Sigma-Z formulation from CTDMPLUS

   bvfrq = dsqrt( g * tgrefl / ptrefl )
   if(bvfrq .lt. 1.0d-10) bvfrq = 1.0d-10

!     Set height for sigma-z calculation, ZTMP
   ztmp = max( hs, hearg, 1.0d-4 )
   szrefl = swrefl * ttravl /&
   &dsqrt( 1.0d0 + swrefl*ttravl * ( 1.0d0/(0.72d0*ztmp) +&
   &bvfrq/(0.54d0*swrefl) ) )

   if (hearg .ge. zi) then
      szrf = szrefl
   else
      sigf = min( hearg/zi, 1.0d0 )
      szrf = (1.0d0 - sigf) * szsurf + sigf * szrefl
   end if

!     Calculate sigma-z at plume height, SZHSBL
   szhsbl = dsqrt( szbarg*szbarg + szrf*szrf + vsigzarg*vsigzarg)

!     Compute height of effective reflecting surface, HEREFL
   herefl = max( zi, hearg + 2.15d0*szhsbl )

   return
end

subroutine psrdeb
!***********************************************************************
!             PSRDEB Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Outputs point source information for debugging
!                 purposes
!
!        PROGRAMMER: Bob Paine.  Implemented by Russ Lee.
!
!        DATE:    August 18, 1994
!
!        INPUTS:  Source Parameters for Specific Source, including
!                 those calculated in PCALC
!
!RJP              DHCRIT = "Final" plume rise (m)
!
!        OUTPUTS: Debugging information for a specific source.
!
!        CALLED FROM:   CALC
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

!     Variable Initializations
   modnam = 'PSRDEB'

   write (dbgunt, 6130) kurdat

   write (dbgunt, 6135) isrc, qs, ts, vs, ds, fb, fm, hs, dhcrit
   write (dbgunt, 6140) hs, wdir, us, up, svs, sws, tgs

6130 format(20('===='),//,' YR/MN/DY/HR:  ', i8,&
   &//,8x,'<----------------- SOURCE INFORMATION ---------',&
   &'------> FINAL PLUME',/,&
   &' SOURCE   QS    TS      VS     DS   BUOY FLUX  MOM FLUX  ',&
   &' HS      RISE',/,&
   &'   #    (G/S)   (K)    (M/S)   (M)   (M4/S3)    (M4/S2)  ',&
   &' (M)      (M)',/)
6135 format(i4,f9.1,f7.1,f7.2,f7.2,f10.1,2x,f9.1,f6.1,f9.1,&
   &//,2x,'VARIABLES AT ',t21,'HEIGHT   WDIR   ',&
   &'USCAL  URISE  SIGV   SIGW   DTHDZ ',&
   &/,2x,'STACK HEIGHT:',t21,&
   &'  (M)    (DEG)  (M/S)  (M/S)  (M/S)  (M/S) (DEG/M)',/)
6140 format(19x,f7.1,f7.0,f8.2,2f7.2,f7.2,f8.4,/)

   return
end

subroutine vcalc
!***********************************************************************
!        VCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates concentration or deposition values
!                 for VOLUME sources
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:
!                  Modified to add Aircraft's plume rise for
!                  VOLUME Source only for Aircraft Source Group.
!                  Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                  04/01/2023
!
!                  Modified to calculate conc*travel time for
!                  GRSM NO2 option.
!                  CERC, 11/30/20
!
!                  Added arrays to save WDSIN and WDCOS by source for use
!                  with PVMRM option.  Corrects potential problem for
!                  PVMRM applications with multi-level wind inputs.
!                  R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!                  Added call to subroutine HEFF for calculation
!                  of ZSUBP for deposition applications.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
!
!                  Modified to include initialization of __VAL arrays
!                  at end of receptor loop.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
!
!                  Modified to include the PVMRM and OLM options for
!                  modeling conversion of NOx to NO2.
!                  Added debug statement based on ENSR code.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
!                  To assign values to XDIST before calls to
!                  SUBROUTINE VOLCALC.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
!
!        INPUTS:  Source Parameters for Specific Source
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
!                 Particular Source
!
!        CALLED FROM:   CALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   integer :: i
   double precision :: aerplm(numtyp), aerpan(numtyp), fran
   logical :: l_plume

!**  Added for Aircraft Plume Rise; UNC-IE
   integer :: kiter, ndxzmid, ndxzpl
   integer :: ndxbh
   double precision :: hsprim, zplm, dhfold, svpm, upm, tgpm,&
   &ptpm, ptp, zmid, fran3, swpm
   double precision :: valabv, vbelow
!**  End Aircraft Plume Rise insert; April 2023

!     Variable Initializations
   modnam = 'VCALC'
   wake = .false.

!     Initialize __VAL arrays (1:NUMTYP)
   hrval   = 0.0d0
   aerval  = 0.0d0
   aerplm  = 0.0d0
   aerpan  = 0.0d0
   prmval  = 0.0d0
   fran    = 0.0d0
   fran3   = 0.0d0         ! Added for ARISE; UNC-IE

   if( allocated(chi) ) chi(:,isrc,:) = 0.0d0

   if(grsm)then
      chi_ttravplm = 0.0d0
      chi_ttravpan = 0.0d0
      chi_ttravaer = 0.0d0
      chi_ttravchm(:,isrc) = 0.0d0
   end if

!     Set the Source Variables for This Source              ---   CALL SETSRC
   call setsrc

!     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
   call emfact(qs)

!     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
   if (eval(isrc)) then
      call evlini
   end if

   if (qtk .ne. 0.0d0) then

!        Set Mixing Height and Profiles for Urban Option if Needed
      if (urbsrc(isrc) .eq. 'Y') then
!           Find Urban Area Index for This Source
         do i = 1, numurb
            if (iurbgrp(isrc,i) .eq. 1) then
               iurb = i
               exit
            end if
         end do
         if (stable .or. L_MorningTrans(iurb)) then
            urbstab = .true.
            zi = max( ziurb(iurb), zimech )
            gridsv = grdsvu(1:mxglvl,iurb)
            gridsw = grdswu(1:mxglvl,iurb)
            gridtg = grdtgu(1:mxglvl,iurb)
            gridpt = grdptu(1:mxglvl,iurb)
            obulen = urbobulen(iurb)
            ustar  = urbustr(iurb)
         else
            urbstab = .false.
            zi = zirur
            gridsv = grdsvr
            gridsw = grdswr
            gridtg = grdtgr
            gridpt = grdptr
            obulen = rurobulen
            ustar  = rurustr
         end if
      else if (urban .and. urbsrc(isrc) .eq. 'N') then
         urbstab = .false.
         zi = zirur
         gridsv = grdsvr
         gridsw = grdswr
         gridtg = grdtgr
         gridpt = grdptr
         obulen = rurobulen
         ustar  = rurustr
      else
! ---       Rural
         urbstab = .false.
      end if

!        Initialize meteorological variables                ---   CALL METINI
      call metini

!        Save WDSIN and WSCOS for later use by PVMRM/GRSM option
      awdsin(isrc) = wdsin
      awdcos(isrc) = wdcos
      aafv(isrc)   = afv

!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
!        Find Aircraft Index for This Source

         fm  = 0.0d0                                       ! Momentum Flux
         hsp = hs

!         Calculate Buoyancy Flux                            ---   CALL AFLUXES
         call afluxes

!         Define temporary values of CENTER and SURFAC based on HS
         center = hs
         if( center .lt. 0.1d0*zi )then
            surfac = .true.
         else
            surfac = .false.
         end if

!         Calculate Distance to Final Rise                   ---   CALL ADISTF
         call adistf

!         Calculate the plume penetration factor             ---   CALL PENFCT
         call penfct

         if(arcftdebug) then
!            WRITE(ARCFTDBG,*) '========================================'
!            WRITE(ARCFTDBG,*) ' VCALC: YYMMDDHH ',KURDAT,
!     &                        '  Source  ',srcid(isrc)
            write(arcftdbg,6000) dhfaer, up, tgs
6000        format(/,5x,'INITIAL PLUME RISE ESTIMATE:  DELH = ',&
            &f6.1,' M; Uplume = ',f5.2,' M/S; DTHDZ = ',&
            &f7.4,' DEG K/M')
         end if

         if (stable .or. (unstab.and.(hs.ge.zi))) then
!            Use iterative approach to stable plume rise calculations
            kiter = 0
50          zplm = hsp + 0.5d0 * dhfaer
            dhfold = dhfaer

!----        Locate index below ZPLM

            call locate(gridht, 1, mxglvl, zplm, ndxzpl)

!----        Get Wind speed at ZPLM; replace UP.  Also, replace SVP,SWP,TGP,
!            vertical potential temperature gradient, if stable.

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

!RWB         Use average of stack top and midpoint wind speeds and others.
            up = 0.5d0 * (us + upm)
            svp = 0.5d0 * (svs + svpm)
            swp = 0.5d0 * (sws + swpm)

            call gintrp( gridht(ndxzpl), gridtg(ndxzpl),&
            &gridht(ndxzpl+1), gridtg(ndxzpl+1), zplm, tgpm )
            call gintrp( gridht(ndxzpl), gridpt(ndxzpl),&
            &gridht(ndxzpl+1), gridpt(ndxzpl+1), zplm, ptpm )
!RWB         Use average of stack top and midpoint temperature gradients.
            tgp = 0.5d0 * (tgs + tgpm)
            ptp = 0.5d0 * (pts + ptpm)
            bvf = dsqrt( g * tgp / ptp )
            if(bvf .lt. 1.0d-10) bvf = 1.0d-10
            bvprim  = 0.7d0 * bvf

!           Calculate Distance to Final Rise                   ---   CALL ADISTF
            call adistf

            kiter = kiter + 1

!RJP         Add temporary debugging statements

            if(arcftdebug) then
               write(arcftdbg,6001) kiter,dhfold, dhfaer, zplm, up,tgp
6001           format(/,5x,'OPTH2 ITER. #',i1,': OLD DELH = ',&
               &f6.1,' M; NEW DELH = ',f6.1,' M; MET LEVEL = ',&
               &f6.1,' M; NEW Upl = ',f5.2,' M/S; NEW DTHDZ = ',&
               &f7.4,' K/M')
            end if

!            Check for convergence
            if(dabs((dhfold - dhfaer)/dhfaer) .lt. 0.01d0) go to 60

            if(kiter .ge. 5) then
               dhfaer = 0.5d0 * (dhfaer + dhfold)
               if(arcftdebug) write(arcftdbg,6002) dhfaer
6002           format(/,5x,'PLUME RISE ITERATION FAILED TO CONVERGE; ',&
               &' PLUME RISE SET AT ',f6.1,' METERS.',/)
               go to 60
            else
               go to 50
            end if

60          continue

!RWB         After completing iteration, reset UP, SVP, SWP, and TGP to stack top
!RWB         values for subsequent distance-dependent plume rise calcs.
            up = us
            svp = svs
            swp = sws
            tgp = tgs
            ptp = pts
            bvf = dsqrt( g * tgp / ptp )
            if(bvf .lt. 1.0d-10) bvf = 1.0d-10
            bvprim  = 0.7d0 * bvf
         end if

!         Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
         prm_fstrec = .true.

!         Initialize 'ARC' Arrays for EVALFILE Output        ---   CALL EVLINI
         if (eval(isrc)) then
            call evlini
         end if

         zmidmx = 0.5d0 * zi

!RJP      Add temporary debugging statement.

         if(arcftdebug) then
            write(arcftdbg,6010) kurdat, zmidmx
6010        format(/,72('*'),//,5x,'YR/MN/DY/HR: ',i8,//,&
            &5x,'Height assigned to midpoint of ',&
            &'well-mixed layer for effective parameters = ',&
            &f6.1,' meters.',/)
         end if
!RJP
!RJP      Calculate distance to uniformly mixed plume within the
!RJP      boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
!RJP      distance is approximately (Zi * UAVG)/SWAVG, where UAVG
!RJP      and SWAVG are wind speed and sigma-w averaged over the depth
!RJP      between the ground and Zi (or the plume height, if higher in
!RJP      stable conditions); this height is denoted as 2 * ZMIDMX.
!RJP
!RJP      First, get refined estimate of final rise and distance to final
!RJP      rise if downwash conditions prevail.
!RJP
         xfinal = xmax
         dhcrit = dhfaer
         xmixed = zi * uavg / swavg
         if (unstab .and. hs.lt.zi) then
!            Check for XMIXED smaller than 1.25*XFINAL
            if (xmixed .lt. 1.25d0*xfinal) then
               xfinal = 0.8d0 * xmixed
               call acblprd (xfinal)
               dhcrit = dhp1
            end if
         end if

      else
!**  End Aircraft Plume Rise insert; April 2023

!        Initialize miscellaneous variables
         fb  = 0.0d0
         fm  = 0.0d0
         ppf = 0.0d0
         hsp = hs
         dhp  = 0.0d0
         dhp1 = 0.0d0
         dhp2 = 0.0d0
         dhp3 = 0.0d0
         dhcrit = 0.0d0
         xfinal = 0.0d0
         xmixed = zi * uavg / swavg
         if(xmixed .lt. xfinal) xmixed = xfinal
         zmidmx = 0.5d0 * zi

!        Calculate Effective Radius
         xrad = 2.15d0*syinit
      end if                    ! Added for Aircraft; UNC-IE

! ---    Initialize PDF parameters for use in calculating ZSUBP
      if( unstab  .and.  (hs .lt. zi) ) then
         call pdf
      end if
!        Set Dry Deposition Variables for this Source
      if (luservd .and. ldgas .and. npd.eq.0) then
!           Assign user-specified gas dry deposition velocity (GASDEPVD option)
         vdepg = uservd
      else if (ldpart .or. (.not.luservd .and. ldgas .and.&
      &npd.eq.0)) then
!           Calculate Deposition Velocities for this Source    ---   CALL VDP
         call vdp
      end if
      if (lwpart .or. lwgas) then
!PES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
!PES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
!PES        evaluated at a distance of 20 kilometers downwind.
!PES        Apply minimum value of 500m and maximum value of 10,000m.
         call heff (20000.0d0)
         if( stable .or. (unstab .and. hs .ge. zi) )then
!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
               he = hsp + dhcrit
               call sigz(20000.0d0)
               zsubp = max( 500.0d0, zi, he + szcoef*szas )
            else
!**  End Aircraft Plume Rise insert; April 2023
               call sigz(20000.0d0)
               zsubp = max( 500.0d0, zi, hs + szcoef*szas )
            end if               ! Added for ARISE; UNC-IE
         else if (unstab) then
!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
               hed1 = hsp + dhcrit
               if (ppf .gt. 0.0d0) then
                  call acblpr3
               end if
               call sigz(20000.0d0)

               if (ppf .eq. 0.0d0) then
                  zsubp=max( 500.0d0, zi, hed1 + szcoef*(szad1+szad2)/&
                  &2.0d0 )
               else if (ppf .eq. 1.0d0) then
                  zsubp=max( 500.0d0, zi, he3 + szcoef*sza3)
               else
                  zsubp=max( 500.0d0, zi, ppf*(he3+szcoef*sza3) +&
                  &(1.0d0-ppf)*(hed1 + szcoef*(szad1+szad2)/2.0d0) )
               end if
            else
!**  End Aircraft Plume Rise insert; April 2023
               call sigz(20000.0d0)
               zsubp = max( 500.0d0, zi, hs +&
               &szcoef*(szad1+szad2)/2.0d0 )
            end if               ! Added for ARISE; UNC-IE
         end if
         zsubp = min( 10000.0d0, zsubp )
!           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
         call scavrat
      end if

!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
!RWB     Determine transport wind direction using midpoint between
!RWB     stack height and "final" plume height.  R. Brode, PES, 1/22/98
!----    Define ZMID=midpoint between stack height and "final" plume height
         zmid = min( 4000.0d0, (hs + 0.5d0*dhfaer) )

!----    Locate index below ZMID
         call locate(gridht, 1, mxglvl, zmid, ndxzmid)

!----    Extract WD for grid levels above and below ZMID
         valabv = gridwd(ndxzmid+1)
         vbelow = gridwd(ndxzmid)

!----    Check for 360 crossover and adjust if necessary
         if( (valabv-vbelow) .lt. -180.0d0) then
            valabv = valabv + 360.0d0
         else if( (valabv-vbelow) .gt. 180.0d0) then
            valabv = valabv - 360.0d0
         end if

!----    Assign Wind direction
         if (vbelow .eq. valabv) then
            wdir = vbelow
         else
!----       Interpolate to ZMID
            call gintrp( gridht(ndxzmid), vbelow,&
            &gridht(ndxzmid+1), valabv,&
            &zmid, wdir )
         end if

!        Check for WDIR > 360 or < 0
         if (wdir .gt. 360.0d0) then
            wdir = wdir - 360.0d0
         else if (wdir .le. 0.0d0) then
            wdir = wdir + 360.0d0
         end if
!
!----    Convert direction to radians, compute sine and cosine of direction,
!        and determine nearest 10-degree sector.
!
!---->   wind direction = wind direction in degrees * DTORAD

         wdsin = dsin(wdir * dtorad)
         wdcos = dcos(wdir * dtorad)

! ---    Save WDSIN and WSCOS for later use by PVMRM/GRSM option
         awdsin(isrc) = wdsin
         awdcos(isrc) = wdcos

         afv = wdir - 180.0d0
         if (afv .lt. 0.0d0) then
            afv = afv + 360.0d0
         end if
         aafv(isrc) = afv       ! save flowvector for this source
         ! for use in MAXDCONT processing
         ifvsec = idint (afv*0.10d0 + 0.4999d0)
         if (ifvsec .eq. 0) ifvsec = 36

!RJP     Add temporary debugging statement.

         if(arcftdebug) then
! ---       Include "effective" wind direction here
            write(arcftdbg, 6011) dhcrit, xfinal, xmixed, afv
6011        format(5x,'For effective parameter calculations: ',&
            &'"Final" plume rise = ',g14.6, ' m; Distance to final ',&
            &'rise = ',g14.6,' m',/,5x,'Distance to well-mixed ',&
            &'state = ',g14.6,' m;',/,5x,'"Effective" flow vector = ',&
            &f6.2)

         end if

      end if
!**  End Aircraft Plume Rise insert; April 2023

!        Begin Receptor LOOP
      receptor_loop: do irec = 1, numrec
!           Calculate Down and Crosswind Distances          ---   CALL XYDIST
         if (evonly) then
            call xydist(ievent)
         else
            call xydist(irec)
         end if

! ---       First check for receptor exceeding minimum distance (1m) or
!           maximum distance (80km for FASTALL or 1.0D20 otherwise).
         if( distr.lt.(xrad+0.99d0) .or. (distr.gt.maxdist) )then
!              Receptor too close to source for calculation or
!              receptor is beyond the maximum distance from source.
            hrval(:)  = 0.0d0
            aerplm(:) = 0.0d0
            aerpan(:) = 0.0d0
            if (pvmrm .or. olm .or. arm2&
            &.or. runttrm .or. grsm) then
! ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                 cycling to the next receptor to avoid
!                 persisting value from previous hour.
               chi(irec,isrc,:) = 0.0d0
               if(grsm)then
                  chi_ttravplm = 0.0d0
                  chi_ttravpan = 0.0d0
                  chi_ttravaer = 0.0d0
                  chi_ttravchm(irec,isrc) = 0.0d0
               end if
               if (pvmrm .or. grsm) then
                  hecntr(irec,isrc)  = 0.0d0
                  ueffs(irec,isrc)   = 0.0d0
                  epsef(irec,isrc)   = 0.0d0
                  ppfact(irec,isrc)  = 0.0d0
                  hecntr3(irec,isrc) = 0.0d0
                  ueff3s(irec,isrc)  = 0.0d0
                  epsef3(irec,isrc)  = 0.0d0
                  fopts(irec,isrc)   = 0.0d0
               end if
            end if
! ---          Cycle to next receptor
            cycle receptor_loop
         end if

! ---       Calculate coherent plume component using downwind distance, X
         l_plume = .true.

! ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
         xdist = x

! ---       Start with coherent plume component, but check for X < 1m
         if( x .lt. 1.0d0 )then
!              Receptor is upwind of source; set AERPLM = 0.0 and skip
!              the call to VOLCALC
            aerplm = 0.0d0
!              CERC 11/30/20 Also set conc*travel time to zero
            if(grsm)then
               chi_ttravplm=0.0d0
            end if
         else
! ---          Receptor is downwind of source; call VOLCALC to get AERPLM
            call volcalc( x, l_plume, aerplm )

! ---          CERC 11/30/20 Calculate the conc*travel time for the Gaussian plume
            if(grsm)then
               if (stable .or. (unstab.and.(hs.ge.zi))) then
                  uchm=ueff
               else
                  uchm=ueffd
               end if
               if (uchm/=0) then
                  chi_ttravplm=max(0.0d0,aerplm(1)*x/uchm) !Don't allow negative conc*travel times
               else
                  !Error - no travel time if UEFF is zero
                  call errhdl(path, modnam, 'E','601','GRSM')
                  runerr=.true.
                  return
               end if
            end if
         end if

! ---       Next determine "random/pancake" component of plume
         if (l_effsigy) then
! ---          No "pancake" calculation for non-DFAULT FASTALL option (L_EFFSIGY)
            hrval  = aerplm
            aerpan = 0.0d0
            fran   = 0.0d0
!              CERC 11/30/20 Also set conc*travel time for GRSM to zero
            if(grsm)then
               chi_ttravpan=0.0d0
            end if

         else

! ---          Next calculate random "pancake" component using radial distance
            l_plume = .false.
! ---          Assign XDIST based on radial distance for use in dry depletion (FUNCTION F2INT)
            xdist = distr
            call volcalc( distr, l_plume, aerpan )
! ---          CERC 11/30/20 Calculate the conc*travel time for the Gaussian plume
            if(grsm)then
               if (stable .or. (unstab.and.(hs.ge.zi))) then
                  uchm=ueff
               else
                  uchm=ueffd
               end if
               if (uchm/=0) then
                  chi_ttravpan=max(0.0d0,aerpan(1)*xdist/uchm) !Don't allow negative conc*travel times
               else
                  !Error - no travel time if UEFF is zero
                  call errhdl(path, modnam, 'E','601','GRSM')
                  runerr=.true.
                  return
               end if
            end if

! ---          Calculate fraction of random kinetic energy to total kinetic energy.
!              Note that these effective parameters are based on the radial dist.
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               call meandr( ueff, sveff, fran )
            else if (unstab) then
               call meandr( ueffd, sveffd, fran )
!**   Added for Aircraft Plume Rise; UNC-IE
               if (aftsrc(isrc) .eq. 'Y') then
                  if (ppf .gt. 0.0d0) then
!                    For penetrated source calculate weighted average of
!                    direct/indirect plume component and penetrated component
                     call meandr( ueff3, sveff3, fran3 )
                     fran = ppf*fran3 + (1.0d0-ppf)*fran
                  end if
               end if
!**   End Aircraft Plume Rise insert; April 2023
            end if

            hrval = fran*aerpan + (1.0d0-fran)*aerplm

! ---          CERC 11/30/20 Combine to calculate the conc*travel time for the combined plume
            if(grsm)then
               chi_ttravchm(irec,isrc)=fran*chi_ttravpan +&
               &(1.0d0-fran)*chi_ttravplm
            end if

         end if

!           ENSR STATEMENT
         if(debug) then
            do ityp = 1, numtyp
               write(dbgunt,10) aerpan(ityp), aerplm(ityp), fran,&
               &hrval(ityp)
10             format(/,'HRVAL(ITYP) = FRAN*AERPAN(ITYP) + (1.-FRAN)',&
               &'*AERPLM(ITYP)',//,&
               &'PANCAKE/MEANDER COMPONENT, AERPAN(ITYP) = ',&
               &g16.8,/,'COHERENT PLUME COMPONENT,  AERPLM(ITYP) = ',&
               &g16.8,/,'MEANDER FACTOR, FRAN = ',&
               &g16.8,/,'RESULTANT CONC, HRVAL(ITYP) = ',g16.8,//)
            end do
         end if
!  Added for TTRM; AECOM

         if (runttrm) then
            ttrmout(irec,isrc,12) = ueff
            ttrmout(irec,isrc,13) = ueffd
            ttrmout(irec,isrc,15) = fran
         end if

         if (runttrm) then
            ttrmout(irec,isrc,10) = x
            ttrmout(irec,isrc,11) = distr
            ttrmout(irec,isrc,20) = y
         end if
! End TTRM insert; Feb. 2021


         if (pvmrm) then
! ---          Store data by source and receptor for PVMRM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               hecntr(irec,isrc) = he
               ueffs(irec,isrc)  = ueff
               epsef(irec,isrc)  = epseff
            else
               hecntr(irec,isrc) = center
               ueffs(irec,isrc)  = ueffd
               epsef(irec,isrc)  = epseffd
            end if
            if (ppf .gt. 0.0d0) then
               ppfact(irec,isrc)  = ppf
               hecntr3(irec,isrc) = he3
               ueff3s(irec,isrc)  = ueff3
               epsef3(irec,isrc)  = epseff3
            else
               ppfact(irec,isrc)  = 0.0d0
               hecntr3(irec,isrc) = 0.0d0
               ueff3s(irec,isrc)  = 0.0d0
               epsef3(irec,isrc)  = 0.0d0
            end if
            fopts(irec,isrc) = fopt

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerplm(:) = 0.0d0
            aerpan(:) = 0.0d0
            prmval(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

         else if (olm) then
! ---          Store data by source and receptor for OLM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerplm(:) = 0.0d0
            aerpan(:) = 0.0d0
            prmval(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop
!    Added for TTRM; AECOM
         else if (runttrm) then
! ---          Store data by source and receptor for TTRM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerplm(:) = 0.0d0
            aerpan(:) = 0.0d0
            prmval(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop
!    End TTRM insert; Feb. 2021

         else if (arm2) then
! ---          Store data by source and receptor for ARM2 options
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerplm(:) = 0.0d0
            aerpan(:) = 0.0d0
            prmval(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

!           CERC 11/30/20
         else if (grsm) then
! ---          Store data by source and receptor for GRSM options
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

            if (stable .or. (unstab.and.(hs.ge.zi))) then
               hecntr(irec,isrc) = he
               ueffs(irec,isrc)  = ueff
               epsef(irec,isrc)  = epseff
            else
               hecntr(irec,isrc) = center
               ueffs(irec,isrc)  = ueffd
               epsef(irec,isrc)  = epseffd
            end if
            if (ppf .gt. 0.0d0) then
               ppfact(irec,isrc)  = ppf
               hecntr3(irec,isrc) = he3
               ueff3s(irec,isrc)  = ueff3
               epsef3(irec,isrc)  = epseff3
            else
               ppfact(irec,isrc)  = 0.0d0
               hecntr3(irec,isrc) = 0.0d0
               ueff3s(irec,isrc)  = 0.0d0
               epsef3(irec,isrc)  = 0.0d0
            end if
            fopts(irec,isrc) = fopt

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerplm(:) = 0.0d0
            aerpan(:) = 0.0d0
            prmval(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

         end if

!           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
!           As noted just above, the calls to EV_SUMVAL and SUMVAL
!           are skipped for a PVMRM, OLM, ARM2 or GRSM run; the
!           summing is performed for these options in the
!           respective routines
         if (evonly) then
            call ev_sumval
         else
            do igrp = 1, numgrp
               call sumval
            end do
         end if

         if (eval(isrc)) then
!              Check ARC centerline values for EVALFILE
!              output                              ---   CALL EVALCK
            call evalck
         end if

      end do receptor_loop
!        End Receptor LOOP

!        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
      if (eval(isrc)) then
         call evalfl
      end if

   end if

   return
end


subroutine volcalc( xarg, l_plume, aerout )
!***********************************************************************
!             VOLCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the AERMOD concentration without downwash
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:     November 10, 2000
!
!        CHANGES:  Modified to add Aircraft's plume rise for
!                  VOLUME Source only for Aircraft Source Group.
!                  Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                  04/01/2023
!
!        CHANGES:
!                  Added debug statement based on ENSR code.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
!
!        INPUTS:   XARG         - Real - Distance (m), downwind for coherent
!                                        plume component and radial for
!                                        random component
!                  L_PLUME      - Log  - Specifies coherent plume calculation
!                                        if TRUE, otherwise random component
!
!        OUTPUTS:  AEROUT(NTYP) - Real - AERMOD component of concentration
!                                        without building downwash for either
!                                        coherent plume component or for
!                                        random component, depending on
!                                        L_PLUME.
!
!        CALLED FROM:   VCALC
!
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: aerout(numtyp), aertmp(numtyp), fyout, xarg,&
   &adj, fran,&
   &fran3                 ! Added for ARISE; UNC-IE
   integer :: j
   logical :: l_plume

!     Variable Initializations
   modnam = 'VOLCALC'

! --- Initialize FRAN
   fran  = 0.0d0
! --- Initialize FRAN3                          ! Added for ARISE; UNC-IE
   fran3 = 0.0d0                             ! Added for ARISE; UNC-IE

!     Initialize AEROUT and AERTMP arrays (1:NUMTYP)
   aerout = 0.0d0
   aertmp = 0.0d0

!     Determine Deposition Correction Factors
   if (npd .eq. 0 .and. (ldgas .or. lwgas)) then
      call pdepg (xarg)
   else
      dqcorg = 1.0d0
      wqcorg = 1.0d0
   end if
   if (npd .gt. 0 .and. (ldpart .or. lwpart)) then
      call pdep (xarg)
   else if (npd .gt. 0) then
!        Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
      dqcor = 1.0d0
      wqcor = 1.0d0
   end if

!     Set initial effective parameters
   ueff  = us
   sveff = svs
   sweff = sws
   tgeff = tgs
   if ( unstab  .and.  (hs .lt. zi) ) then
      ueffd  = us
      sveffd = svs
      sweffd = sws
      ueffn  = us
      sveffn = svs
      sweffn = sws
!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
         ueff3  = us
         sveff3 = svs
         sweff3 = sws
         tgeff3 = tgs
      end if
!**  End Aircraft Plume Rise insert; April 2023
   end if

!RJP  Add temporary debugging statement here.

!     ENSR ENHANCEMENT OF WRITE STATEMENT TO IDENTIFY COMPONENT CONCENTRATION
   if(debug) then
      write(dbgunt, 6014) srcid(isrc)
6014  format(//,'SRCID: ', a8)
      if(l_plume)then
         write(dbgunt, 6015) ueff, sveff, sweff
6015     format(//,'COHERENT PLUME COMPONENT',/,5x,&
         &'Initial effective parameters for ',&
         &'stable or direct convective ',&
         &'plume:',//,5x,'Ueff = ',f7.2,' m/s; ',&
         &'SVeff = ',f7.2,&
         &' m/s; SWeff = ',f7.2,' m/s.',/)
      else
         write(dbgunt, 6016) ueff, sveff, sweff
6016     format(//,'MEANDER COMPONENT',/,5x,&
         &'Initial effective parameters for ',&
         &'stable or direct convective ',&
         &'plume:',//,5x,'Ueff = ',f7.2,' m/s; ',&
         &'SVeff = ',f7.2,&
         &' m/s; SWeff = ',f7.2,' m/s.',/)
      end if
   end if

!     Define plume centroid height (CENTER) for use in
!     inhomogeniety calculations
   call centroid (xarg)

!**  Added for Aircraft Plume Rise; UNC-IE
   if (aftsrc(isrc) .eq. 'Y') then
!     Calculate the plume rise                     ---   CALL ADELTAH
      call adeltah ( xarg )
   end if
!**  End Aircraft Plume Rise insert; April 2023

!     If the atmosphere is unstable and the stack
!     top is below the mixing height, calculate
!     the CBL PDF coefficients                     ---   CALL PDF
   if( unstab  .and.  (hs .lt. zi) ) then
      call pdf
   end if

!     Determine Effective Plume Height             ---   CALL HEFF
   call heff ( xarg )

!     Compute effective parameters using an
!     iterative average through plume rise layer
   call iblval ( xarg )

!     Call PDF & HEFF again for final CBL plume heights
   if (unstab .and. (hs.lt.zi) ) then
      call pdf
      call heff ( xarg )
   end if

!     Determine Dispersion Parameters              ---   CALL VDIS
   call vdis ( xarg )

!     Calculate the 'y-term' contribution to
!     dispersion, FSUBY
   if (l_plume) then
!        Calculate FSUBY for coherent plume
      if( l_effsigy )then
! ---       Calculate fraction of random kinetic energy to total kinetic energy
!           for FASTALL option to optimize meander using effective sigma-y (EFFSIGY),
!           and for the Beta option that uses an effective sigma-y value.
!           Note that these effective parameters are based on the downwind distance,
!           rather than the radial distance used in the standard meander approach
         if (stable .or. (unstab.and.(hs.ge.zi))) then
            call meandr( ueff, sveff, fran )
         else if (unstab) then
            call meandr( ueffd, sveffd, fran )
!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
               if (ppf .gt. 0.0d0) then
!                 For penetrated source calculate weighted average of
!                 direct/indirect plume component and penetrated component
                  call meandr( ueff3, sveff3, fran3 )
                  fran = ppf*fran3 + (1.0d0-ppf)*fran
               end if
            end if
!**  End Aircraft Plume Rise insert; April 2023
         end if

!           Calculate effective sigma-y for non-DFAULT FASTALL option (EFFSIGY)
         syeff = 1.0d0/((fran/(srt2pi*xarg)) + (1.0d0-fran)/sy)
         if (dabs(y) .gt. numsyeff*syeff) then
!               Plume is more than 4 sigmas off centerline, skip calculation
            fyout = 0.0d0
         else
!               Calculate FSUBY for coherent plume        ---   CALL FYPLM
            call fyplm(syeff,fyout)
         end if
      else
!           Calculate FSUBY for coherent plume        ---   CALL FYPLM
         call fyplm(sy,fyout)

      end if

   else
!        Calculate FSUBY for random component      ---   CALL FYPAN
      call fypan(fyout)

   end if

   fsuby  = fyout
   fsubyd = fsuby
   fsubyn = fsubyd

!**  Added for Aircraft Plume Rise; UNC-IE
   if (aftsrc(isrc) .eq. 'Y') then
!      Calculate the 'y-term' contribution to dispersion
!      for the penetrated plume, FSUBY3
      if( unstab  .and.  (hs .lt. zi)  .and. (ppf .gt. 0.0d0) )then
!        Compute meander fraction of horizontal distribution function
!        from Venky's memo of 6/24/98.
         if (l_plume) then
            if (l_effsigy) then
! ---          Calculate fraction of random kinetic energy to total kinetic energy
!              for FASTALL option to optimize meander using effective sigma-y.
!              Note that these effective parameters are based on the downwind distance,
!              rather than the radial distance used in the standard meander approach
               syeff = 1.0d0/((fran/(srt2pi*xarg)) + (1.0d0-fran)/sy3)

               if (dabs(y) .gt. numsyeff*syeff) then
!                 Plume is more than 4 sigmas off centerline, skip calculation
                  fyout = 0.0d0
               else
!                 Calculate FSUBY for coherent plume        ---   CALL FYPLM
                  call fyplm(syeff,fyout)
               end if

            else
!              Calculate FSUBY for coherent plume        ---   CALL FYPLM
               call fyplm(sy3,fyout)
            end if
         else
!           Calculate FSUBY for random component   ---   CALL FYPAN
            call fypan(fyout)
         end if

         fsuby3 = fyout
      else
         fsuby3 = 0.0d0
      end if
   else
!**  End Aircraft Plume Rise insert; February 2023
!     Set lateral term = 0.0 for penetrated source
      fsuby3 = 0.0d0
   end if                       ! Added for ARISE; UNC-IE

!     Check for zero "y-terms"; if zero then skip calculations
!     and go to next receptor.
   if( fsuby.eq.0.0d0 .and. fsuby3.eq.0.0d0 )then
      aerout = 0.0d0

   else

      if (npd .eq. 0) then
!           Perform calculations for gases
!           Assign plume tilt, HV = 0.0

         adj = dqcorg * wqcorg

         if (stable .or. (unstab.and.(hs.ge.zi))) then
!              Calculate height of the "effective reflecting surface"
!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
               call refl_ht (he, xarg, szb, vsigz, hsbl)
            else
!**  End Aircraft Plume Rise insert; April 2023
               call refl_ht (he, xarg, 0.0d0, vsigz, hsbl)
            end if                         ! Added for ARISE; UNC-IE
         else if ( unstab ) then
            hsbl = 0.0d0
         end if

!**  Added for Aircraft Plume Rise; UNC-IE
         if (aftsrc(isrc) .eq. 'Y') then
            if (unstab .and. (hs.lt.zi) .and. (ppf.gt.0.0d0)) then
!               Calculate height of the "effective reflecting surface"
               call refl_ht (he3, xarg, szb3, vsigz, hpen)
            else
               hpen = 0.0d0
            end if
         end if
!**  End Aircraft Plume Rise insert; April 2023

!           Determine the CRITical Dividing Streamline---   CALL CRITDS
         call critds (he)

!           Calculate the fraction of plume below
!           HCRIT, PHEE                               ---   CALL PFRACT
         call pfract (he)

!           Calculate FOPT = f(PHEE)                  ---   CALL FTERM
         call fterm

!           Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
         call aer_pchi( xarg, adj, vdepg, 0, aerout )

      else
!           Perform calculations for particles, loop through particle sizes

!           Begin loop over particle sizes
         do j = 1, npd

!              Calculate Plume Tilt Due to Settling, HV
            hv = (xarg/us) * vgrav(j)

!              Adjust Jth contribution by mass fraction and source
!              depletion
            adj = phi(j) * dqcor(j) * wqcor(j)

            if (stable .or. (unstab.and.(hs.ge.zi))) then
!                 Calculate height of the "effective reflecting surface"
               hesetl = max( 0.0d0, he - hv )
!**  Added for Aircraft Plume Rise; UNC-IE
               if (aftsrc(isrc) .eq. 'Y') then
                  call refl_ht (hesetl, xarg, szb, vsigz, hsbl)
               else
!**  End Aircraft Plume Rise insert; April 2023
                  call refl_ht (hesetl, xarg, 0.0d0, vsigz, hsbl)
               end if                         ! Added for ARISE; UNC-IE

            else if ( unstab ) then
               hesetl = max( 0.0d0, 0.5d0*(hed1+hed2) - hv )
               hsbl = 0.0d0
            end if

!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
               if (unstab .and. (hs.lt.zi) .and. (ppf.gt.0.0d0)) then
!                 Calculate height of the "effective reflecting surface"
!                 Calculate Settled Plume Height(s), HE3SETL
                  he3setl = max( 0.0d0, he3 - hv )
                  call refl_ht (he3setl, xarg, szb3, vsigz, hpen)
                  hpen = max( hpen, zi )
               else
                  hpen = 0.0d0
               end if
            end if
!**  End Aircraft Plume Rise insert; April 2023

!              Determine the CRITical Dividing Streamline---   CALL CRITDS
            call critds (hesetl)

!              Calculate the fraction of plume below
!              HCRIT, PHEE                               ---   CALL PFRACT
            call pfract (hesetl)

!              Calculate FOPT = f(PHEE)                  ---   CALL FTERM
            call fterm

!              Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
            call aer_pchi( xarg, adj, vdep(j), j, aertmp )
            aerout = aerout + aertmp

         end do
!           End loop over particle sizes

      end if
   end if

   return
end


subroutine acalc
!***********************************************************************
!            ACALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates concentration or deposition values
!                 for AREA sources utilizing an integrated line source.
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Modified to add Aircraft's plume rise for
!                    AREA Source only for Aircraft Source Group.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   To calculate conc*travel time for GRSM NO2 option.
!                    CERC, 11/30/20
!
!        MODIFIED:
!                    Added arrays to save WDSIN and WDCOS by source for use
!                    with PVMRM option.  Corrects potential problem for
!                    PVMRM applications with multi-level wind inputs.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!                    Added call to subroutine HEFF for calculation
!                    of ZSUBP for deposition applications.
!                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
!
!        MODIFIED:   Modified to initialize HE = HS prior to first
!                    call to sub. ADISY.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/05/05
!
!        MODIFIED:   Modified to include initialization of __VAL arrays
!                    at end of receptor loop.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
!
!        MODIFIED:   To include the PVMRM and OLM options for
!                    modeling conversion of NOx to NO2.
!                    Added debug statement based on ENSR code.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
!
!        MODIFIED:   To include tilted plume for point source
!                    approximation of particle emissions.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/23/04
!
!        MODIFIED:   To allow TOXICS option for AREAPOLY sources.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 05/12/04
!
!        MODIFIED:   To assign value to XDIST for use in dry depletion.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
!
!        MODIFIED:   To avoid potential math errors for AREAPOLY sources
!                    R.W. Brode, PES, Inc. - 02/25/02
!
!        MODIFIED:   To incorporate numerical integration algorithm
!                    for AREA source - 7/7/93
!
!        INPUTS:  Source Parameters for Specific Source
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
!
!        CALLED FROM:   CALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   integer :: i, j
   double precision :: xdep, width, length, xminr, xmaxr, xpoint,&
   &qtksav, adj
   double precision :: aerout(numtyp), fyout
   double precision :: aerout_plm, aerout_pan, fran !Wood 4/14/22

!**  Added for Aircraft Plume Rise; UNC-IE
   integer :: kiter, ndxzmid, ndxzpl
   integer :: ndxbh
   double precision :: hsprim, zplm, dhfold, svpm, upm, tgpm,&
   &ptpm, ptp, zmid, swpm
   double precision :: valabv, vbelow
!**  End Aircraft Plume Rise insert; April 2023

!     Variable Initializations
   modnam = 'ACALC'
   wake = .false.
   l_aplume = .true.                !5/31/22 Wood initalized L_APLUME
   aerout_plm = 0.0d0               !5/31/22 Wood initalized  Plume
   aerout_pan = 0.0d0               !5/31/22 Wood initalized  Meander/Pancake
   fran = 0.0d0                     !5/31/22 Wood initalized  FRAN

!     Initialize __VAL arrays (1:NUMTYP)
   hrval   = 0.0d0
   aerval  = 0.0d0
   aerout  = 0.0d0
   prmval  = 0.0d0
   if( allocated(chi) ) chi(:,isrc,:) = 0.0d0
   if(grsm)then
      chi_ttravchm(:,isrc) = 0.0d0
   end if

!     Set the Source Variables for This Source              ---   CALL SETSRC
   call setsrc

!     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
   call emfact(qs)

!     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
   if (eval(isrc)) then
      call evlini
   end if

! ---  Write info to AREA debug file if needed
   if( areadbg )then
      write(areadbunt,100) kurdat
100   format(/'AREA Debug Sub_ACALC: ',2x,'YYMMDDHH = ',i8.8)
   endif

   if (qtk .ne. 0.0d0) then

! ---    Save original emission rate, which may be needed if RECLOOP is cycled
!        and point source approximation is used (FASTAREA or FASTALL options)
      qtksav = qtk

!        Set Mixing Height and Profiles for Urban Option if Needed
      if (urbsrc(isrc) .eq. 'Y') then
!           Find Urban Area Index for This Source
         do i = 1, numurb
            if (iurbgrp(isrc,i) .eq. 1) then
               iurb = i
               exit
            end if
         end do
         if (stable .or. L_MorningTrans(iurb)) then
            urbstab = .true.
            zi = max( ziurb(iurb), zimech )
            gridsv = grdsvu(1:mxglvl,iurb)
            gridsw = grdswu(1:mxglvl,iurb)
            gridtg = grdtgu(1:mxglvl,iurb)
            gridpt = grdptu(1:mxglvl,iurb)
            obulen = urbobulen(iurb)
            ustar  = urbustr(iurb)
         else
            urbstab = .false.
            zi = zirur
            gridsv = grdsvr
            gridsw = grdswr
            gridtg = grdtgr
            gridpt = grdptr
            obulen = rurobulen
            ustar  = rurustr
         end if
      else if (urban .and. urbsrc(isrc) .eq. 'N') then
         urbstab = .false.
         zi = zirur
         gridsv = grdsvr
         gridsw = grdswr
         gridtg = grdtgr
         gridpt = grdptr
         obulen = rurobulen
         ustar  = rurustr
      else
! ---       Rural
         urbstab = .false.
      end if

!        Initialize meteorological variables                ---   CALL METINI
      call metini

!        Save WDSIN and WSCOS for later use by PVMRM/GRSM option
      awdsin(isrc) = wdsin
      awdcos(isrc) = wdcos
      aafv(isrc)   = afv

!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
!        Find Aircraft Index for This Source

         fm  = 0.0d0                                       ! Momentum Flux
         hsp = hs

!         Calculate Buoyancy Flux                            ---   CALL AFLUXES
         call afluxes

!         Define temporary values of CENTER and SURFAC based on HS
         center = hs
         if( center .lt. 0.1d0*zi )then
            surfac = .true.
         else
            surfac = .false.
         end if

!         Calculate Distance to Final Rise                   ---   CALL ADISTF
         call adistf

!         Calculate the plume penetration factor             ---   CALL PENFCT
         call penfct

         if(arcftdebug) then
!            WRITE(ARCFTDBG,*) '========================================'
!            WRITE(ARCFTDBG,*) 'ACALC: YYMMDDHH ',KURDAT,
!     &                        '  Source  ',srcid(isrc)
            write(arcftdbg,6000) dhfaer, up, tgs
6000        format(/,5x,'INITIAL PLUME RISE ESTIMATE:  DELH = ',&
            &f6.1,' M; Uplume = ',f5.2,' M/S; DTHDZ = ',&
            &f7.4,' DEG K/M')
         end if

         if (stable .or. (unstab.and.(hs.ge.zi))) then
!            Use iterative approach to stable plume rise calculations
            kiter = 0
50          zplm = hsp + 0.5d0 * dhfaer
            dhfold = dhfaer

!----        Locate index below ZPLM

            call locate(gridht, 1, mxglvl, zplm, ndxzpl)

!----        Get Wind speed at ZPLM; replace UP, SVP, SWP.  Also, replace TGP,
!            vertical potential temperature gradient, if stable.

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

!RWB         Use average of stack top and midpoint wind speeds and others.
            up = 0.5d0 * (us + upm)
            svp = 0.5d0 * (svs + svpm)
            swp = 0.5d0 * (sws + swpm)

            call gintrp( gridht(ndxzpl), gridtg(ndxzpl),&
            &gridht(ndxzpl+1), gridtg(ndxzpl+1), zplm, tgpm )
            call gintrp( gridht(ndxzpl), gridpt(ndxzpl),&
            &gridht(ndxzpl+1), gridpt(ndxzpl+1), zplm, ptpm )
!RWB         Use average of stack top and midpoint temperature gradients.
            tgp = 0.5d0 * (tgs + tgpm)
            ptp = 0.5d0 * (pts + ptpm)
            bvf = dsqrt( g * tgp / ptp )
            if(bvf .lt. 1.0d-10) bvf = 1.0d-10
            bvprim  = 0.7d0 * bvf

!            Calculate Distance to Final Rise                   ---   CALL ADISTF
            call adistf

            kiter = kiter + 1

!RJP         Add temporary debugging statements

            if(arcftdebug) then
               write(arcftdbg,6001) kiter,dhfold, dhfaer, zplm, up,tgp
6001           format(/,5x,'OPTH2 ITER. #',i1,': OLD DELH = ',&
               &f6.1,' M; NEW DELH = ',f6.1,' M; MET LEVEL = ',&
               &f6.1,' M; NEW Upl = ',f5.2,' M/S; NEW DTHDZ = ',&
               &f7.4,' K/M')
            end if

!            Check for convergence
            if(dabs((dhfold - dhfaer)/dhfaer) .lt. 0.01d0) go to 60

            if(kiter .ge. 5) then
               dhfaer = 0.5d0 * (dhfaer + dhfold)
               if(arcftdebug) write(arcftdbg,6002) dhfaer
6002           format(/,5x,'PLUME RISE ITERATION FAILED TO CONVERGE; ',&
               &' PLUME RISE SET AT ',f6.1,' METERS.',/)
               go to 60
            else
               go to 50
            end if

60          continue

!RWB         After completing iteration, reset UP, SVP, SWP, and TGP to stack top
!RWB         values for subsequent distance-dependent plume rise calcs.
            up = us
            svp = svs
            swp = sws
            tgp = tgs
            ptp = pts
            bvf = dsqrt( g * tgp / ptp )
            if(bvf .lt. 1.0d-10) bvf = 1.0d-10
            bvprim  = 0.7d0 * bvf
         end if

!         Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
         prm_fstrec = .true.

!         Initialize 'ARC' Arrays for EVALFILE Output        ---   CALL EVLINI
         if (eval(isrc)) then
            call evlini
         end if

         zmidmx = 0.5d0 * zi

!RJP      Add temporary debugging statement.

         if(arcftdebug) then
            write(arcftdbg,6010) kurdat, zmidmx
6010        format(/,72('*'),//,5x,'YR/MN/DY/HR: ',i8,//,&
            &5x,'Height assigned to midpoint of ',&
            &'well-mixed layer for effective parameters = ',&
            &f6.1,' meters.',/)
         end if
!RJP
!RJP      Calculate distance to uniformly mixed plume within the
!RJP      boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
!RJP      distance is approximately (Zi * UAVG)/SWAVG, where UAVG
!RJP      and SWAVG are wind speed and sigma-w averaged over the depth
!RJP      between the ground and Zi (or the plume height, if higher in
!RJP      stable conditions); this height is denoted as 2 * ZMIDMX.
!RJP
!RJP      First, get refined estimate of final rise and distance to final
!RJP      rise if downwash conditions prevail.
!RJP
         xfinal = xmax
         dhcrit = dhfaer
         xmixed = zi * uavg / swavg
         if (unstab .and. hs.lt.zi) then
!            Check for XMIXED smaller than 1.25*XFINAL
            if (xmixed .lt. 1.25d0*xfinal) then
               xfinal = 0.8d0 * xmixed
               call acblprd (xfinal)
               dhcrit = dhp1
            end if
         end if

      else
!**  End Aircraft Plume Rise insert; April 2023
!        Initialize miscellaneous variables
         fb  = 0.0d0
         fm  = 0.0d0
         ppf = 0.0d0
         hsp = hs
         he  = hs
         dhp  = 0.0d0
         dhp1 = 0.0d0
         dhp2 = 0.0d0
         dhp3 = 0.0d0
         dhcrit = 0.0d0
         xfinal = 0.0d0
         xmixed = zi * uavg / swavg
         if(xmixed .lt. xfinal) xmixed = xfinal
         zmidmx = 0.5d0 * zi
      end if                    ! Added for Aircraft; UNC-IE

! ---    Initialize PDF parameters for use in calculating ZSUBP
      if( unstab  .and.  (hs .lt. zi) ) then
         call pdf
      end if
!        Set Dry Deposition Variables for this Source
      if (luservd .and. ldgas .and. npd.eq.0) then
!           Assign user-specified gas dry deposition velocity (GASDEPVD option)
         vdepg = uservd
      else if (ldpart .or. (.not.luservd .and. ldgas .and.&
      &npd.eq.0)) then
!           Calculate Deposition Velocities for this Source    ---   CALL VDP
         call vdp
      end if
      if (lwpart .or. lwgas) then
!PES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
!PES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
!PES        evaluated at a distance of 20 kilometers downwind.
!PES        Apply minimum value of 500m and maximum value of 10,000m.
         call heff (20000.0d0)
         if( stable .or. (unstab .and. hs .ge. zi) )then
!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
               he = hsp + dhcrit
               call sigz(20000.0d0)
               zsubp = max( 500.0d0, zi, he + szcoef*szas )
            else
!**  End Aircraft Plume Rise insert; April 2023
               call sigz(20000.0d0)
               zsubp = max( 500.0d0, zi, hs + szcoef*szas )
            end if               ! Added for Aircraft; UNC-IE
         else if (unstab) then
!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
               hed1 = hsp + dhcrit
               if (ppf .gt. 0.0d0) then
                  call acblpr3
               end if
               call sigz(20000.0d0)

               if (ppf .eq. 0.0d0) then
                  zsubp=max( 500.0d0, zi, hed1 + szcoef*(szad1+szad2)/&
                  &2.0d0 )
               else if (ppf .eq. 1.0d0) then
                  zsubp=max( 500.0d0, zi, he3 + szcoef*sza3)
               else
                  zsubp=max( 500.0d0, zi, ppf*(he3+szcoef*sza3) +&
                  &(1.0d0-ppf)*(hed1 + szcoef*(szad1+szad2)/2.0d0) )
               end if

            else
!**  End Aircraft Plume Rise insert; April 2023
               call sigz(20000.0d0)
               zsubp = max( 500.0d0, zi, hs +&
               &szcoef*(szad1+szad2)/2.0d0 )
            end if               ! Added for Aircraft; UNC-IE
         end if
         zsubp = min( 10000.0d0, zsubp )
!           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
         call scavrat
      end if

!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
!RWB     Determine transport wind direction using midpoint between
!RWB     stack height and "final" plume height.  R. Brode, PES, 1/22/98
!----    Define ZMID=midpoint between stack height and "final" plume height
         zmid = min( 4000.0d0, (hs + 0.5d0*dhfaer) )
!----    Locate index below ZMID
         call locate(gridht, 1, mxglvl, zmid, ndxzmid)

!----    Extract WD for grid levels above and below ZMID
         valabv = gridwd(ndxzmid+1)
         vbelow = gridwd(ndxzmid)

!----    Check for 360 crossover and adjust if necessary
         if( (valabv-vbelow) .lt. -180.0d0) then
            valabv = valabv + 360.0d0
         else if( (valabv-vbelow) .gt. 180.0d0) then
            valabv = valabv - 360.0d0
         end if

!----    Assign Wind direction
         if (vbelow .eq. valabv) then
            wdir = vbelow
         else
!----       Interpolate to ZMID
            call gintrp( gridht(ndxzmid), vbelow,&
            &gridht(ndxzmid+1), valabv,&
            &zmid, wdir )
         end if

!        Check for WDIR > 360 or < 0
         if (wdir .gt. 360.0d0) then
            wdir = wdir - 360.0d0
         else if (wdir .le. 0.0d0) then
            wdir = wdir + 360.0d0
         end if
!
!----    Convert direction to radians, compute sine and cosine of direction,
!        and determine nearest 10-degree sector.
!
!---->   wind direction = wind direction in degrees * DTORAD

         wdsin = dsin(wdir * dtorad)
         wdcos = dcos(wdir * dtorad)

         afv = wdir - 180.0d0
         if (afv .lt. 0.0d0) then
            afv = afv + 360.0d0
         end if
         aafv(isrc) = afv       ! save flowvector for this source
         ! for use in MAXDCONT processing
         ifvsec = idint (afv*0.10d0 + 0.4999d0)
         if (ifvsec .eq. 0) ifvsec = 36

!RJP     Add temporary debugging statement.

         if(arcftdebug) then
! ---       Include "effective" wind direction here
            write(arcftdbg, 6011) dhcrit, xfinal, xmixed, afv
6011        format(5x,'For effective parameter calculations: ',&
            &'"Final" plume rise = ',g14.6, ' m; Distance to final ',&
            &'rise = ',g14.6,' m',/,5x,'Distance to well-mixed ',&
            &'state = ',g14.6,' m;',/,5x,'"Effective" flow vector = ',&
            &f6.2)

         end if

      end if
!**  End Aircraft Plume Rise insert; April 2023

! ---    Save WDSIN and WSCOS for later use by PVMRM/GRSM option
      awdsin(isrc) = wdsin
      awdcos(isrc) = wdcos

!        Begin Receptor LOOP
      receptor_loop: do irec = 1, numrec

!  Added for TTRM; AECOM
         if (runttrm) then
            ttrmout(irec,isrc,11) = distr
            ttrmout(irec,isrc,12) = ueff
            ttrmout(irec,isrc,13) = ueffd
         end if
!  End TTRM insert; Feb. 2021

!           Calculate Down and Crosswind Distances          ---   CALL ARDIST
         if( areadbg )then
            if( evonly )then
               write(areadbunt,101) srcid(isrc), ievent
101            format(/1x,'SRCID= ', a12,'  IEVT=',i7,/)
            else
               write(areadbunt,102) srcid(isrc), irec
102            format(/1x,'SRCID= ', a12,'  IREC=',i7,/)
            endif
         endif

         if (evonly) then
            call ardist(ievent,xdep,width,length,xminr,xmaxr)
         else
            call ardist(irec,xdep,width,length,xminr,xmaxr)
         end if

!           Check to see if receptor is upwind of area source;
!           also check to see if receptor is beyond the maximum
!           distance from source; assigned value of 80km
!           for obsolescent TOXICS option or new FASTAREA or
!           FASTALL options; otherwise "unlimited" (1.0D20)
         if (xmaxr .lt. 1.0d0 .or. distr .gt. maxdist) then
!     Added for TTRM; AECOM
!           For TTRM, reset radial distance if the receptor is upwind or
!           the maximum distance from the source
            if (runttrm) then
               ttrmout(irec,isrc,11) = 0.0d0
            end if
!    End TTRM insert; Feb. 2021
!              Receptor is upwind of source or receptor is beyond the
!              maximum distance from source; set CHI = 0.0 for current
!              REC and SRC and set NO2 option variables to 0.0 before
!              cycling to next receptor
            if (pvmrm .or. olm .or. arm2&
            &.or. runttrm .or. grsm) then
! ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                 cycling to the next receptor to avoid
!                 persisting value from previous hour.
               chi(irec,isrc,:) = 0.0d0
               if(grsm)then
                  chi_ttravchm(irec,isrc) = 0.0d0
               end if
               if (pvmrm .or. grsm) then
                  hecntr(irec,isrc)  = 0.0d0
                  ueffs(irec,isrc)   = 0.0d0
                  epsef(irec,isrc)   = 0.0d0
                  ppfact(irec,isrc)  = 0.0d0
                  hecntr3(irec,isrc) = 0.0d0
                  ueff3s(irec,isrc)  = 0.0d0
                  epsef3(irec,isrc)  = 0.0d0
                  fopts(irec,isrc)   = 0.0d0
               end if
            end if
! ---          Also set HRVAL/AERVAL/AEROUR = 0
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

! ---          Reset QTK since it may have been changed if
!              point source approximation has been used
            qtk = qtksav
! ---          Cycle to next receptor
            cycle receptor_loop
         end if

!           Initialize HE for initial call to ADISY
         he = hs
!           Set initial effective parameters
         ueff  = us
         sveff = svs
         sweff = sws
         tgeff = tgs
         if ( unstab  .and.  (hs .lt. zi) ) then
            ueffd  = us
            sveffd = svs
            sweffd = sws
            ueffn  = us
            sveffn = svs
            sweffn = sws
!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
               ueff3  = us
               sveff3 = svs
               sweff3 = sws
               tgeff3 = tgs
            end if
!**  End Aircraft Plume Rise insert; April 2023
         end if

! ---         Removed this check if calculating meander since the lateral plume width is extended
! ---             and this check would artificially reduce the width of the plume Wood 6/10/22
! ---       Check to see if receptor is beyond edge of plume laterally.
         if (.not. l_areamndr) then
            if ( (dabs(y)-0.5d0*width) .gt. 0.0d0) then
! ---              Receptor is outside "projected" width of effective area source
!                  Calculate maximum sigma-y based on XMAXR and compare minimum
!                  lateral distance of receptor from source to 5*sigma-y;
!                  Note that Y represents the lateral distance to the receptor
!                  from the center of the source.

! ---              Calculate SY at XMAXR to determine if receptor is beyond
!                  the plume laterally.
               call adisy(xmaxr)
               if ((dabs(y)-0.5d0*width) .ge. 5.0d0*sy) then
!         Added for TTRM; AECOM
!               For TTRM, reset radial distance if the receptor is more than
!               5&SY from the edge of the projected area source
                  if (runttrm) then
                     ttrmout(irec,isrc,11) = 0.0d0
                  end if
!         End TTRM insert; Feb. 2021
!                     Receptor is more than 5*SY from the edge of the
!                     projected area source; set CHI = 0.0 for current
!                     REC and SRC and set NO2 option variables to 0.0
!                     before cycling to next receptor.
                  if (pvmrm .or. olm .or. arm2&
                  &.or. runttrm .or. grsm) then
! ---                    Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                        cycling to the next receptor to avoid
!                        persisting value from previous hour.
                     chi(irec,isrc,:) = 0.0d0
                     if(grsm)then
                        chi_ttravchm(irec,isrc) = 0.0d0
                     end if
                     if (pvmrm .or. grsm) then
                        hecntr(irec,isrc)  = 0.0d0
                        ueffs(irec,isrc)   = 0.0d0
                        epsef(irec,isrc)   = 0.0d0
                        ppfact(irec,isrc)  = 0.0d0
                        hecntr3(irec,isrc) = 0.0d0
                        ueff3s(irec,isrc)  = 0.0d0
                        epsef3(irec,isrc)  = 0.0d0
                        fopts(irec,isrc)   = 0.0d0
                     end if
                  end if
!                     Also reinitialize HRVAL, AERVAL and AEROUT
                  hrval(:)  = 0.0d0
                  aerval(:) = 0.0d0
                  aerout(:) = 0.0d0

! ---                 Reset QTK since it may have been changed if
!                     point source approximation has been used
                  qtk = qtksav

! ---                 Cycle to next receptor
                  cycle receptor_loop
               end if
            end if
         end if

         if(debug) then
            write(dbgunt, 6015) ueff, sveff, sweff
!             ENSR ENHANCEMENT OF WRITE STATEMENT
6015        format(//,'AERMOD AREA SOURCE COMPONENT',/,5x,&
            &'Initial effective parameters for ',&
            &'stable or direct convective ',&
            &'plume:',//,5x,'Ueff = ',f7.2,' m/s; ',&
            &'SVeff = ',f7.2,&
            &' m/s; SWeff = ',f7.2,' m/s.',/)
         end if

!           Determine the CRITical Dividing Streamline      ---   CALL CRITDS
         call critds (he)

!           Set distance factor for point source approx. for FASTAREA option
!           based on "equivalent" PG stability class (KST)
         if (urbstab) then
            vp_fact = virtpnt_urb(kst)
         else
            vp_fact = virtpnt_rur(kst)
         end if

!           Calculate distance for switch to point source approx. for FASTAREA
         xpoint = 1.5d0*length + vp_fact*width

! ---       Assign XDIST for use in dry depletion (FUNCTION F2INT)
         xdist = x

         if ( (.not.fastarea .and. .not.fastall) .or.&
         &((fastarea .or. fastall) .and. x .lt. xpoint)) then
            if (ardplete) then

               if (npd .eq. 0 .and. (ldgas .or. lwgas)) then
                  call pdepg (xdep)
               else
                  dqcorg = 1.0d0
                  wqcorg = 1.0d0
               end if
               if (npd .gt. 0 .and. (ldpart .or. lwpart)) then
                  call pdep (xdep)
               else if (npd .gt. 0) then
!                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                  dqcor = 1.0d0
                  wqcor = 1.0d0
               end if

            end if

            do ityp = 1, numtyp
! ---       Area meander option added Wood 6/3/22
! ---       Calculate coherent plume component using downwind distance, X
               l_aplume = .true.
               aerout_plm = 0.0d0

!                 Calculate Area Source Integral - Plume         ---   CALL AREAIN
               call areain
               aerout_plm = hrval(ityp)

!                 Check if user selected to calculate meander
!                 Calculate Area Source Integral - meander/pancake plume         ---   CALL AREAIN
               if (l_areamndr) then
                  aerout_pan = 0.0d0
                  l_aplume = .false.
                  call areain
                  aerout_pan = hrval(ityp)

                  if (stable .or. (unstab.and.(hs.ge.zi))) then
                     call meandr( ueff, sveff, fran )
                  else if (unstab) then
                     call meandr( ueffd, sveffd, fran )
                  end if
!                Concentration calculated based on FRAN wieghting between plume and meander/pancake plume components
                  hrval(ityp) = fran*aerout_pan + (1.0d0-fran)*aerout_plm
               else
!                    Concentration equivalent to the plume component only
                  hrval(ityp) = aerout_plm
               end if
            end do
         else
!              Use point source approximation
!              Save emissions per unit area and calculate total emissions
            qtksav = qtk
            if (srctyp(isrc) .eq. 'AREA' .or.&
            &srctyp(isrc) .eq. 'LINE' .or.&
            &srctyp(isrc) .eq. 'AREAPOLY') then
!                 Note that XINIT and YINIT are equivalent values for AREAPOLY
               qtk = qtk * xinit * yinit
            else if (srctyp(isrc) .eq. 'AREACIRC') then
               qtk = qtk * pi * radius(isrc) * radius(isrc)
            end if
            syinit = 0.0d0

!              Determine Deposition Correction Factors
            if (npd .eq. 0 .and. (ldgas .or. lwgas)) then
               call pdepg (x)
            else
               dqcorg = 1.0d0
               wqcorg = 1.0d0
            end if
            if (npd .gt. 0 .and. (ldpart .or. lwpart)) then
               call pdep (x)
            else if (npd .gt. 0) then
!                 Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
               dqcor = 1.0d0
               wqcor = 1.0d0
            end if

!              Define plume centroid height (CENTER) for use in
!              inhomogeniety calculations
            call centroid (x)

!**  Added for Aircraft Plume Rise; UNC-IE
            if (aftsrc(isrc) .eq. 'Y') then
!     Calculate the plume rise                              ---   CALL ADELTAH
               call adeltah ( x )
            end if
!**  End Aircraft Plume Rise insert; April 2023

!              If the atmosphere is unstable and the stack
!              top is below the mixing height, calculate
!              the CBL PDF coefficients                     ---   CALL PDF
            if( unstab  .and.  (hs .lt. zi) ) then
               call pdf
            end if

!              Determine Effective Plume Height             ---   CALL HEFF
            call heff ( x )

!              Compute effective parameters using an
!              iterative average through plume rise layer
            call iblval (x)

!              Call PDF & HEFF again for final CBL plume heights
            if (unstab .and. (hs.lt.zi) ) then
               call pdf
               call heff (x)
            end if

!              Determine Dispersion Parameters              ---   CALL VDIS
            call vdis (x)

!              Calculate the 'y-term' contribution to
!              dispersion, FSUBY, for coherent plume        ---   CALL FYPLM
            call fyplm(sy,fyout)
            fsuby  = fyout
            fsubyd = fsuby
            fsubyn = fsubyd

!              Set lateral term = 0.0 for penetrated source
            fsuby3 = 0.0d0

!              Check for zero "y-terms"; if zero then skip calculations
!              and go to next receptor.
            if( fsuby.eq.0.0d0 .and. fsuby3.eq.0.0d0 )then
!      Added for TTRM; AECOM
!              For TTRM, if the "y-terms" are zero then set the radial
!              distance to zero.
               if (runttrm) then
                  ttrmout(irec,isrc,11) = 0.0d0
               end if
!      End TTRM insert; Feb. 2021
!                 FSUBY terms are both 0.0; assign 0.0 to arrays
               if (pvmrm .or. olm .or. arm2&
               &.or. runttrm .or. grsm) then
! ---                Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                    cycling to the next receptor to avoid
!                    persisting value from previous hour.
                  chi(irec,isrc,:) = 0.0d0
                  if(grsm)then
                     chi_ttravchm(irec,isrc) = 0.0d0
                  end if
                  if (pvmrm .or. grsm) then
                     hecntr(irec,isrc)  = 0.0d0
                     ueffs(irec,isrc)   = 0.0d0
                     epsef(irec,isrc)   = 0.0d0
                     ppfact(irec,isrc)  = 0.0d0
                     hecntr3(irec,isrc) = 0.0d0
                     ueff3s(irec,isrc)  = 0.0d0
                     epsef3(irec,isrc)  = 0.0d0
                     fopts(irec,isrc)   = 0.0d0
                  end if

               end if
!                 Also reinitialize HRVAL, AERVAL and AEROUT
               hrval(:)  = 0.0d0
               aerval(:) = 0.0d0
               aerout(:) = 0.0d0

! ---             Reset QTK since it may have been changed if
!                 point source approximation has been used
               qtk = qtksav

! ---             Cycle to next receptor
               cycle receptor_loop

            else

               if (npd .eq. 0) then
!                    Perform calculations for gases
!                    Assign plume tilt, HV = 0.0

                  adj = dqcorg * wqcorg

                  if (stable .or. (unstab.and.(hs.ge.zi))) then
!                       Calculate height of the "effective reflecting surface"
!**  Added for Aircraft Plume Rise; UNC-IE
                     if (aftsrc(isrc) .eq. 'Y') then
                        call refl_ht (he, x, szb, vsigz, hsbl)
                     else
!**  End Aircraft Plume Rise insert; April 2023
                        call refl_ht (he, x, 0.0d0, vsigz, hsbl)
                     end if                         ! Added for ARISE; UNC-IE
                  else if ( unstab ) then
                     hsbl = 0.0d0
                  end if

!**  Added for Aircraft Plume Rise; UNC-IE
                  if (aftsrc(isrc) .eq. 'Y') then
                     if (unstab.and.(hs.lt.zi).and.(ppf.gt.0.0d0))then
!               Calculate height of the "effective reflecting surface"
                        call refl_ht (he3, x, szb3, vsigz, hpen)
                     else
                        hpen = 0.0d0
                     end if
                  end if
!**  End Aircraft Plume Rise insert; April 2023

!                    Determine the CRITical Dividing Streamline---   CALL CRITDS
                  call critds (he)

!                    Calculate the fraction of plume below
!                    HCRIT, PHEE                               ---   CALL PFRACT
                  call pfract (he)

!                    Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                  call fterm

!                    Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                  call aer_pchi( x, adj, vdepg, 0, aerout )
                  hrval = aerout

               else
!                    Perform calculations for particles, loop through particle sizes

!                    Begin loop over particle sizes
                  do j = 1, npd

!                       Calculate Plume Tilt Due to Settling, HV
                     hv = (x/us) * vgrav(j)

!                       Adjust Jth contribution by mass fraction and source
!                       depletion
                     adj = phi(j) * dqcor(j) * wqcor(j)

                     if (stable .or. (unstab.and.(hs.ge.zi))) then
!                          Calculate height of the "effective reflecting surface"
!                          Calculate Settled Plume Height(s), HESETL
                        hesetl = max( 0.0d0, he - hv )
!**  Added for Aircraft Plume Rise; UNC-IE
                        if (aftsrc(isrc) .eq. 'Y') then
                           call refl_ht (hesetl, x, szb, vsigz, hsbl)
                        else
!**  End Aircraft Plume Rise insert; April 2023
                           call refl_ht (hesetl, x, 0.0d0, vsigz, hsbl)
                        end if                         ! Added for ARISE; UNC-IE
                     else if ( unstab ) then
!                          Calculate Settled Plume Height(s), HESETL
                        hesetl = max( 0.0d0, 0.5d0*(hed1+hed2) - hv )
                        hsbl = 0.0d0
                     end if

!**  Added for Aircraft Plume Rise; UNC-IE
                     if (aftsrc(isrc) .eq. 'Y') then
                        if(unstab.and.(hs.lt.zi).and.(ppf.gt.0.0d0))then
!                 Calculate height of the "effective reflecting surface"
!                 Calculate Settled Plume Height(s), HE3SETL
                           he3setl = max( 0.0d0, he3 - hv )
                           call refl_ht (he3setl, x, szb3, vsigz, hpen)
                           hpen = max( hpen, zi )
                        else
                           hpen = 0.0d0
                        end if
                     end if
!**  End Aircraft Plume Rise insert; April 2023

!                       Determine the CRITical Dividing Streamline---   CALL CRITDS
                     call critds (hesetl)

!                       Calculate the fraction of plume below
!                       HCRIT, PHEE                               ---   CALL PFRACT
                     call pfract (hesetl)

!                       Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                     call fterm

!                       Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                     call aer_pchi( x, adj, vdep(j), j, aerout )
                     hrval = hrval + aerout

                  end do
!                    End loop over particle sizes

               end if
            end if
! ---          Reset QTK since it may have been changed if
!              point source approximation has been used
            qtk = qtksav
         end if

         if (pvmrm) then
! ---          Store data by source and receptor for PVMRM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               hecntr(irec,isrc) = he
               ueffs(irec,isrc)  = ueff
               epsef(irec,isrc)  = epseff
            else
               hecntr(irec,isrc) = center
               ueffs(irec,isrc)  = ueffd
               epsef(irec,isrc)  = epseffd
            end if
            if (ppf .gt. 0.0d0) then
               ppfact(irec,isrc)  = ppf
               hecntr3(irec,isrc) = he3
               ueff3s(irec,isrc)  = ueff3
               epsef3(irec,isrc)  = epseff3
            else
               ppfact(irec,isrc)  = 0.0d0
               hecntr3(irec,isrc) = 0.0d0
               ueff3s(irec,isrc)  = 0.0d0
               epsef3(irec,isrc)  = 0.0d0
            end if
            fopts(irec,isrc) = fopt

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

         else if (olm) then
! ---          Store data by source and receptor for OLM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop
!     Added for TTRM; AECOM
         else if (runttrm) then
! ---          Store data by source and receptor for OLM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop
!     End TTRM insert; Feb. 2021

         else if (arm2) then
! ---          Store data by source and receptor for ARM2 options
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

!           CERC 11/30/20
         else if (grsm) then

!              Calculate the effective wind speed
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               uchm=ueff
            else
               uchm=ueffd
            end if
!              Calculate the conc x travel time for the chemistry scheme
            if(uchm/=0)then
               chi_ttravchm(irec,isrc)=&
               &max(0.0d0,hrval(1)*x/uchm) !Don't allow negative conc*travel times
            else
               !Error - no travel time if UCHM is zero
               call errhdl(path, modnam, 'E','601','GRSM')
               runerr=.true.
               return
            end if

! ---          Store data by source and receptor for GRSM options
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

            if (stable .or. (unstab.and.(hs.ge.zi))) then
               hecntr(irec,isrc) = he
               ueffs(irec,isrc)  = ueff
               epsef(irec,isrc)  = epseff
            else
               hecntr(irec,isrc) = center
               ueffs(irec,isrc)  = ueffd
               epsef(irec,isrc)  = epseffd
            end if
            if (ppf .gt. 0.0d0) then
               ppfact(irec,isrc)  = ppf
               hecntr3(irec,isrc) = he3
               ueff3s(irec,isrc)  = ueff3
               epsef3(irec,isrc)  = epseff3
            else
               ppfact(irec,isrc)  = 0.0d0
               hecntr3(irec,isrc) = 0.0d0
               ueff3s(irec,isrc)  = 0.0d0
               epsef3(irec,isrc)  = 0.0d0
            end if
            fopts(irec,isrc) = fopt

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

         end if

!           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
!           As noted just above, the calls to EV_SUMVAL and SUMVAL
!           are skipped for a PVMRM, OLM, ARM2 or GRSM run; the
!           summing is performed for these options in the
!           respective routines
         if (evonly) then
            call ev_sumval
         else
            do igrp = 1, numgrp
               call sumval
            end do
         end if

!           Initialize __VAL arrays (1:NUMTYP)
         hrval(:)  = 0.0d0
         aerval(:) = 0.0d0
         aerout(:) = 0.0d0

      end do receptor_loop
!        End Receptor LOOP

!        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
      if (eval(isrc)) then
         call evalfl
      end if

   end if

   return
end

subroutine ardist(indx,xdep,width,length,xminrec,xmaxrec)
!***********************************************************************
!                 ARDIST Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sets Receptor Variables and Calculates Downwind (X)
!                 and Crosswind (Y) Distances, Crosswind Width (WIDTH),
!                 Distance used for AREADPLT Option (XDEP), Maximum
!                 Downwind Distance by Vertex (XMAXREC), and
!                 Radial Distance from Source to Receptor (DISTR)
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Moved calculation of center of effective area
!                    source for OPENPIT sources to subroutine PITEFF.
!                    Previous versions would have skipped calculation
!                    of center coordinates if first receptor was located
!                    inside the actual OPENPIT source.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, XX/YY/2013
!
!        MODIFIED:   Recalculate the center of effective area source
!                    for OPENPIT sources when INDX = 1.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        INPUTS:  Receptor Index, INDX
!                 Source Location
!                 Arrays of Receptor Locations
!                 SIN and COS of Wind Direction FROM Which Wind
!                 is Blowing, WDSIN and WDCOS
!
!        OUTPUTS: Values of X, Y, and DISTR (m) [in MAIN1]
!                 XDEP (m)
!                 WIDTH (m)
!                 LENGTH (m)
!                 XMAXREC (m)
!
!        CALLED FROM:   ACALC
!                       OCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   integer :: i, indx
   double precision :: xsrc, ysrc, xminrec, xmaxrec, yminrec,&
   &ymaxrec, xdep, width, length

!     Variable Initializations
   modnam = 'ARDIST'

!     Set Receptor Coordinates, Terrain Elevation and Flagpole Heights
   xr = axr(indx)
   yr = ayr(indx)
   zelev = azelev(indx)
   zhill = azhill(indx)
   zflag = azflag(indx)

!     Initialize min and max distances from source to receptor
   xminrec =  9.0d10
   xmaxrec = -9.0d10
   yminrec =  9.0d10
   ymaxrec = -9.0d10

!     Initialize SPA(:,:) array
   spa(:,:) = 0.0d0

!     Calculate Downwind (X) and Crosswind (Y) Distances for Each Vertex
   do i = 1, nvert+1
      xsrc = xvert(i)
      ysrc = yvert(i)
      spa(i,1) = -((xr-xsrc)*wdsin + (yr-ysrc)*wdcos)
      spa(i,2) =   (xr-xsrc)*wdcos - (yr-ysrc)*wdsin
      xminrec = min(xminrec, spa(i,1))
      xmaxrec = max(xmaxrec, spa(i,1))
      yminrec = min(yminrec, spa(i,2))
      ymaxrec = max(ymaxrec, spa(i,2))
   end do

!     Calculate crosswind width, WIDTH, and alongwind length, LENGTH
   width  = ymaxrec - yminrec
   length = xmaxrec - xminrec

   if (areadbg) then
      write(areadbunt,*)
      write(areadbunt,*) 'AREA Debug Sub_ARDIST: '
      write(areadbunt,*)
      if (evonly) then
         write(areadbunt,*) ' AREA DEBUG for SRC: ',isrc,&
         &' and EVENT: ',indx
         write(areadbunt,*)
      else
         write(areadbunt,*) ' AREA DEBUG for SRC: ',isrc,&
         &'   and REC: ',indx
         write(areadbunt,*)
      end if
      write(areadbunt,'(2x,"XMINREC, XMAXREC: ",2(F15.6))') xminrec,&
      &xmaxrec
      write(areadbunt,'(2x,"YMINREC, YMAXREC: ",2(F15.6))') yminrec,&
      &ymaxrec
      write(areadbunt,*)

      do i = 1, nvert+1
         write(areadbunt,'(a,i4,2(2x,F15.6))') '  IVERT, SPA1, SPA2: ',&
         &i, spa(i,1), spa(i,2)
      enddo
      write(areadbunt,*)
      write(areadbunt,*) '   WIDTH = ',width
      write(areadbunt,*) '  LENGTH = ',length
   endif

!     Determine downwind distance to use for AREADPLT option, XDEP
   if (xminrec .ge. 0.0d0) then
      xdep = xminrec + length/3.0d0
   else
      xdep = xmaxrec/3.0d0
   end if

   xdep = max( 1.0d0, xdep )

!     Calculate Downwind (X) and Crosswind (Y) Distances from Center of Source;
!     X and Y are included in module MAIN1
   x = -((xr-xcntr)*wdsin + (yr-ycntr)*wdcos)
   y =   (xr-xcntr)*wdcos - (yr-ycntr)*wdsin

!     Calculate Radial Distance from Center of Source;
!     DISTR is included in module MAIN1
   distr = dsqrt(x*x + y*y)

!     Calculate height of receptor above stack base, ZRT
   if (l_flatsrc(isrc)) then
      zrt = zflag
   else
      zrt = zelev - zs + zflag
   end if

   return
end

subroutine ocalc
!***********************************************************************
!                 OCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates concentration or deposition values
!                 for OPENPIT sources
!
!        PROGRAMMER: Jayant Hardikar, Roger Brode
!        ADAPTED FROM:  SUBROUTINE ACALC
!
!        DATE:    July 19, 1994
!
!        MODIFIED:   To calculate conc*travel time for GRSM NO2 option.
!                    CERC, 11/30/20
!
!        MODIFIED:
!                    Corrected calculation of adjusted emission for the
!                    point source approximation under FASTAREA option
!                    to use length and width of effective area source
!                    rather than length and width of the openpit source.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!                    Added arrays to save WDSIN and WDCOS by source for use
!                    with PVMRM option.  Corrects potential problem for
!                    PVMRM applications with multi-level wind inputs.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!                    Added call to subroutine HEFF for calculation
!                    of ZSUBP for deposition applications.
!                    R. W. Brode, U.S. EPA, OAQPS, AQMG, 01/24/2007
!
!        MODIFIED:   Modified to initialize HE = HS prior to first
!                    call to sub. ADISY.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/05/05
!
!        MODIFIED:   Modified to include initialization of __VAL arrays
!                    at end of receptor loop.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/04
!
!        MODIFIED:   To include tilted plume for point source
!                    approximation of particle emissions.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/23/04
!
!        MODIFIED:   To move call to METINI up since AFV is needed for
!                    SUBROUTINE LWIND.
!                    R. W. Brode, PES Inc., - 1/22/98
!
!        MODIFIED:   To skip calculations if QPTOT = 0.0, avoiding
!                    zero divide error in SUB. AMFRAC.
!                    R. W. Brode, PES Inc., - 4/14/95
!
!        INPUTS:  Source Parameters for Specific Source
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
!
!        CALLED FROM:   CALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   integer :: i, ii, j, icat, inout, ndxr
   double precision :: qptot, xvm(5), yvm(5), xdep, width, length,&
   &xminr, xmaxr, qtksav, xpoint, adj, fyout
   double precision :: aerout(numtyp)

!     Variable Initializations
   modnam = 'OCALC'
   wake = .false.
   l_aplume = .true.            !6/14/22 Wood set L_APLUME to TRUE only coherent plume calculated

!     Initialize __VAL arrays (1:NUMTYP)
   hrval   = 0.0d0
   aerval  = 0.0d0
   aerout  = 0.0d0
   prmval  = 0.0d0
   if( allocated(chi) ) chi(:,isrc,:) = 0.0d0
   if(grsm)then
      chi_ttravchm(:,isrc) = 0.0d0
   end if

!     Obtain reference wind speed at 10-meters
!---- First locate index below 10.
   call locate(gridht, 1, mxglvl, 10.0d0, ndxr)
   call gintrp( gridht(ndxr), gridws(ndxr),&
   &gridht(ndxr+1), gridws(ndxr+1),&
   &10.0d0, uref10 )

!     Set Mixing Height and Profiles for Urban Option if Needed
   if (urbsrc(isrc) .eq. 'Y') then
!        Find Urban Area Index for This Source
      do i = 1, numurb
         if (iurbgrp(isrc,i) .eq. 1) then
            iurb = i
            exit
         end if
      end do
      if (stable .or. L_MorningTrans(iurb)) then
         urbstab = .true.
         zi = max( ziurb(iurb), zimech )
         gridsv = grdsvu(1:mxglvl,iurb)
         gridsw = grdswu(1:mxglvl,iurb)
         gridtg = grdtgu(1:mxglvl,iurb)
         gridpt = grdptu(1:mxglvl,iurb)
         obulen = urbobulen(iurb)
         ustar  = urbustr(iurb)
      else
         urbstab = .false.
         zi = zirur
         gridsv = grdsvr
         gridsw = grdswr
         gridtg = grdtgr
         gridpt = grdptr
         obulen = rurobulen
         ustar  = rurustr
      end if
   else if (urban .and. urbsrc(isrc) .eq. 'N') then
      urbstab = .false.
      zi = zirur
      gridsv = grdsvr
      gridsw = grdswr
      gridtg = grdtgr
      gridpt = grdptr
      obulen = rurobulen
      ustar  = rurustr
   else
      urbstab = .false.
   end if

!     Set the Source Variables for This Source              ---   CALL SETSRC
   call setsrc

!     Initialize meteorological variables                   ---   CALL METINI
   call metini

!     Save WDSIN and WSCOS for later use by PVMRM/GRSM option
   awdsin(isrc) = wdsin
   awdcos(isrc) = wdcos
   aafv(isrc)   = afv

!*    Initialize the Total Adjusted Emission Rate from
!*    All Particles
   qptot = 0.0d0

   if (npd .eq. 0) then
!*       Assign input emission to QPTOT variable for gas emissions
      qptot = qs

   else

!*       Loop over Particle Size Categories
      do icat = 1,npd
!*          Calculate the Escape Fraction for Each Category    ---   CALL ESCAPE
         call escape(icat)

!*          Adjust the Emission Rate for Each Category         ---   CALL ADJEMI
         call adjemi(icat,qptot)

!*       End Loop Over Particle Size Categories
      end do

   end if

!*    Skip Calculations if QPTOT = 0.0
   if (qptot .eq. 0.0d0)  return

   if (npd .gt. 0) then
!*       Adjust the Mass Fractions for All the Particle
!*       Size Categories                                    ---   CALL AMFRAC
      call amfrac(qptot)
   end if

!*    Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
   call lwind

!*    Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
   call pdepth

!*    Calculate the Fractional Size of the
!*    Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
   call ptfrac

!*    If DEBUG Requested, Write Out Pit Info; include in AREA debug file if
!     specified, otherwise in the MODEL debug file
   if (areadbg) then
      write (areadbunt,*)
      write (areadbunt,*)&
      &'DETAIL INFORMATION ON THE OPENPIT SOURCE :', srcid(isrc)
      write (areadbunt,*)
!*    WRITE DEBUG INFORMATION
   elseif (debug) then
      write (dbgunt,*)
      write (dbgunt,*) 'DETAIL INFORMATION ON THE OPENPIT SOURCE :',&
      &srcid(isrc)
      write (dbgunt,*)
   end if

!*    Determine the Coordinates of the Effective Pit Area
!*    in Wind Direction Coordinate System                   ---   CALL PITEFF
   call piteff

!*    Calculate the adusted Emission Rate per unit area (QEFF) for the
!*    Effective Pit Area (PITFRA)                           ---   CALL PITEMI
   call pitemi(qptot)

!*    If DEBUG Requested, Write Out Pit Info; include in AREA debug file if
!     specified, otherwise in the MODEL debug file
   if (areadbg) then
      if (npd .gt. 0) then
         write (areadbunt,*) 'OPENPIT PARTICLE CHARACTERISTICS:'
         write (areadbunt,*) '--------------------------------'
         write (areadbunt,*)
         write (areadbunt,8000) (efrac(ii),ii = 1, npd)
8000     format (1x,'ESCAPE FRACTIONS        = ',10(f8.3,2x))
         write (areadbunt,8200) (qpart(ii),ii = 1, npd)
8200     format (1x,'ADJUSTED EMISSION RATES = ',10(f8.3,2x))
         write (areadbunt,8400) (phi(ii),ii = 1, npd)
8400     format (1x,'ADJUSTED MASS FRACTIONS = ',10(f8.3,2x))
      end if
      write (areadbunt,*) ' EMISSION RATE OF EFFECTIVE PIT = ',qeff
      write (areadbunt,*)
   elseif (debug) then
      if (npd .gt. 0) then
         write (dbgunt,*) 'OPENPIT PARTICLE CHARACTERISTICS:'
         write (dbgunt,*) '--------------------------------'
         write (dbgunt,*)
         write (dbgunt,8000) (efrac(ii),ii = 1, npd)
         write (dbgunt,8200) (qpart(ii),ii = 1, npd)
         write (dbgunt,8400) (phi(ii),ii = 1, npd)
      end if
      write (dbgunt,*) ' EMISSION RATE OF EFFECTIVE PIT = ',qeff
      write (dbgunt,*)
   end if

!     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
   call emfact(qeff)

!     Initialize 'ARC' Arrays for EVALFILE Output           ---   CALL EVLINI
   if (eval(isrc)) then
      call evlini
   end if

   if ((qeff.ne.0.0d0) .and. (stable .or. (hs.le.zi))) then
!        Initialize miscellaneous variables
      fb  = 0.0d0
      fm  = 0.0d0
      ppf = 0.0d0
      hsp = hs
      dhp  = 0.0d0
      dhp1 = 0.0d0
      dhp2 = 0.0d0
      dhp3 = 0.0d0
      dhcrit = 0.0d0
      xfinal = 0.0d0
      xmixed = zi * uavg / swavg
      if(xmixed .lt. xfinal) xmixed = xfinal
      zmidmx = 0.5d0 * zi

! ---    Save original emission rate, which may be needed if RECLOOP is cycled
!        and point source approximation is used for FASTAREA or FASTALL options
      qtksav = qtk

! ---    Initialize PDF parameters for use in calculating ZSUBP
      if( unstab  .and.  (hs .lt. zi) ) then
         call pdf
      end if
!        Set Dry Deposition Variables for this Source
      if (luservd .and. ldgas .and. npd.eq.0) then
!           Assign user-specified gas dry deposition velocity (GASDEPVD option)
         vdepg = uservd
      else if (ldpart .or. (.not.luservd .and. ldgas .and.&
      &npd.eq.0)) then
!           Calculate Deposition Velocities for this Source    ---   CALL VDP
         call vdp
      end if
      if (lwpart .or. lwgas) then
!PES        Set value of ZSUBP = MAX( ZI, TOP OF PLUME ), where
!PES        TOP OF PLUME is defined as plume height (HE) plus 2.15*SZ,
!PES        evaluated at a distance of 20 kilometers downwind.
!PES        Apply minimum value of 500m and maximum value of 10,000m.
         call heff (20000.0d0)
         if( stable .or. (unstab .and. hs .ge. zi) )then
            call sigz(20000.0d0)
            zsubp = max( 500.0d0, zi, hs + szcoef*szas )
         else if (unstab) then
            call sigz(20000.0d0)
            zsubp = max( 500.0d0, zi, hs +&
            &szcoef*(szad1+szad2)/2.0d0 )
         end if
         zsubp = min( 10000.0d0, zsubp )
!           Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
         call scavrat
      end if

!        Begin Receptor LOOP
      receptor_loop: do irec = 1, numrec
!           Check for receptor located inside boundary of open pit source
         do i = 1, nvert+1
            xvm(i) = axvert(i,isrc)
            yvm(i) = ayvert(i,isrc)
         end do
         xr = axr(irec)
         yr = ayr(irec)
         call pnpoly(xr,yr,xvm,yvm,4,inout)

         if (inout .gt. 0) then
!              Receptor is within boundary - skip to next receptor

            if (pvmrm .or. olm .or. arm2 .or. grsm) then
! ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                 cycling to the next receptor to avoid
!                 persisting value from previous hour.
               chi(irec,isrc,:)   = 0.0d0
               if(grsm)then
                  chi_ttravchm(irec,isrc) = 0.0d0
               end if
               if (pvmrm .or. grsm) then
                  hecntr(irec,isrc)  = 0.0d0
                  ueffs(irec,isrc)   = 0.0d0
                  epsef(irec,isrc)   = 0.0d0
                  ppfact(irec,isrc)  = 0.0d0
                  hecntr3(irec,isrc) = 0.0d0
                  ueff3s(irec,isrc)  = 0.0d0
                  epsef3(irec,isrc)  = 0.0d0
                  fopts(irec,isrc)   = 0.0d0
               end if
            end if
!              Also reinitialize HRVAL, AERVAL and AEROUT
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

! ---          Reset QTK since it may have been changed if
!              point source approximation has been used
            qtk = qtksav

            cycle receptor_loop
         end if

!           Calculate Down and Crosswind Distances          ---   CALL ARDIST
         if (evonly) then
            call ardist(ievent,xdep,width,length,xminr,xmaxr)
         else
            call ardist(irec,xdep,width,length,xminr,xmaxr)
         end if

!           Check to see if receptor is upwind of area source;
!           also check to see if receptor is beyond the maximum
!           distance from source; assigned value of 80km for
!           the obsolescent TOXICS option or new FASTALL option;
!           otherwise "unlimited" (1.0D20)
         if (xmaxr .lt. 1.0d0 .or. distr .gt. maxdist) then
            if (pvmrm .or. olm .or. arm2&
            &.or. runttrm .or. grsm) then
! ---             Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                 cycling to the next receptor to avoid
!                 persisting value from previous hour.
               chi(irec,isrc,:) = 0.0d0
               if(grsm)then
                  chi_ttravchm(irec,isrc) = 0.0d0
               end if
               if (pvmrm .or. grsm) then
                  hecntr(irec,isrc)  = 0.0d0
                  ueffs(irec,isrc)   = 0.0d0
                  epsef(irec,isrc)   = 0.0d0
                  ppfact(irec,isrc)  = 0.0d0
                  hecntr3(irec,isrc) = 0.0d0
                  ueff3s(irec,isrc)  = 0.0d0
                  epsef3(irec,isrc)  = 0.0d0
                  fopts(irec,isrc)   = 0.0d0
               end if
            end if
!              Also reinitialize HRVAL, AERVAL and AEROUT
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

! ---          Reset QTK since it may have been changed if
!              point source approximation has been used
            qtk = qtksav

            cycle receptor_loop
         end if

!           Initialize HE for initial call to ADISY
         he = hs
!           Set initial effective parameters
         ueff  = us
         sveff = svs
         sweff = sws
         tgeff = tgs
         if ( unstab  .and.  (hs .lt. zi) ) then
            ueffd  = us
            sveffd = svs
            sweffd = sws
            ueffn  = us
            sveffn = svs
            sweffn = sws
         end if

! ---       Check to see if receptor is beyond edge of plume laterally.
         if ( (dabs(y)-0.5d0*width) .gt. 0.0d0) then
! ---          Receptor is outside "projected" width of effective area source
!              Calculate maximum sigma-y based on XMAXR and compare minimum
!              lateral distance of receptor from source to 4*sigma-y
            call adisy(xmaxr)

            if ((dabs(y)-0.5d0*width) .ge. 5.0d0*sy) then
! ---             Receptor is beyond the edge of plume; set CHI = 0.0
!                 and cycle to next receptor
               if (pvmrm .or. olm .or. arm2&
               &.or. runttrm .or. grsm) then
! ---                Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                    cycling to the next receptor to avoid
!                    persisting value from previous hour.
                  chi(irec,isrc,:) = 0.0d0
                  if(grsm)then
                     chi_ttravchm(irec,isrc) = 0.0d0
                  end if
                  if (pvmrm .or. grsm) then
                     hecntr(irec,isrc)  = 0.0d0
                     ueffs(irec,isrc)   = 0.0d0
                     epsef(irec,isrc)   = 0.0d0
                     ppfact(irec,isrc)  = 0.0d0
                     hecntr3(irec,isrc) = 0.0d0
                     ueff3s(irec,isrc)  = 0.0d0
                     epsef3(irec,isrc)  = 0.0d0
                     fopts(irec,isrc)   = 0.0d0
                  end if
               end if
!                 Also reinitialize HRVAL, AERVAL and AEROUT
               hrval(:)  = 0.0d0
               aerval(:) = 0.0d0
               aerout(:) = 0.0d0

! ---             Reset QTK since it may have been changed if
!                 point source approximation has been used
               qtk = qtksav

! ---             Cycle to next receptor
               cycle receptor_loop
            end if

         end if

         if(debug) then
            write(dbgunt, 6015) ueff, sveff, sweff
6015        format(//,'AERMOD OPENPIT SOURCE COMPONENT',/,5x,/&
            &'Initial effective parameters for ',&
            &'stable or direct convective ',&
            &'plume:',//,5x,'Ueff = ',f7.2,' m/s; ',&
            &'SVeff = ',f7.2,&
            &' m/s; SWeff = ',f7.2,' m/s.',/)
         end if

!           Determine the CRITical Dividing Streamline---   CALL CRITDS
         call critds (he)

!           Set distance factor for point source approx. for FASTAREA option
!           based on "equivalent" PG stability class (KST)
         if (urbstab) then
            vp_fact = virtpnt_urb(kst)
         else
            vp_fact = virtpnt_rur(kst)
         end if

!           Calculate distance for switch to point source approx. for FASTAREA
         xpoint = 1.5d0*length + vp_fact*width
         if ( (.not.fastarea .and. .not.fastall) .or.&
         &((fastarea .or. fastall) .and. x .lt. xpoint)) then
            if (ardplete) then

               if (npd .eq. 0 .and. (ldgas .or. lwgas)) then
                  call pdepg (xdep)
               else
                  dqcorg = 1.0d0
                  wqcorg = 1.0d0
               end if
               if (npd .gt. 0 .and. (ldpart .or. lwpart)) then
                  call pdep (xdep)
               else if (npd .gt. 0) then
!                    Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
                  dqcor = 1.0d0
                  wqcor = 1.0d0
               end if

            end if

            do ityp = 1, numtyp
               l_aplume = .true.
!                 Calculate Area Source Integral         ---   CALL AREAIN
               call areain
            end do
         else
! ---          Use point source approximation
!              Save emissions per unit area (QTK) and calculate total emissions
!              based on effective area (= XEFF*YEFF)
            qtksav = qtk
            qtk = qtk * xeff * yeff
            syinit = 0.0d0

!              Determine Deposition Correction Factors
            if (npd .eq. 0 .and. (ldgas .or. lwgas)) then
               call pdepg (x)
            else
               dqcorg = 1.0d0
               wqcorg = 1.0d0
            end if
            if (npd .gt. 0 .and. (ldpart .or. lwpart)) then
               call pdep (x)
            else if (npd .gt. 0) then
!                 Set DQCOR(NPD) and WQCOR(NPD) arrays to 1.0
               dqcor = 1.0d0
               wqcor = 1.0d0
            end if

!              Define plume centroid height (CENTER) for use in
!              inhomogeniety calculations
            call centroid (x)

!              If the atmosphere is unstable and the stack
!              top is below the mixing height, calculate
!              the CBL PDF coefficients                     ---   CALL PDF
            if( unstab  .and.  (hs .lt. zi) ) then
               call pdf
            end if

!              Determine Effective Plume Height             ---   CALL HEFF
            call heff ( x )

!              Compute effective parameters using an
!              iterative average through plume rise layer
            call iblval (x)

!              Call PDF & HEFF again for final CBL plume heights
            if (unstab .and. (hs.lt.zi) ) then
               call pdf
               call heff (x)
            end if

!              Determine Dispersion Parameters              ---   CALL VDIS
            call vdis (x)

!              Calculate the 'y-term' contribution to
!              dispersion, FSUBY, for coherent plume        ---   CALL FYPLM
            call fyplm(sy,fyout)
            fsuby  = fyout
            fsubyd = fsuby
            fsubyn = fsubyd

!              Set lateral term = 0.0 for penetrated source
            fsuby3 = 0.0d0

!              Check for zero "y-terms"; if zero then skip calculations
!              and go to next receptor.
            if( fsuby.eq.0.0d0 .and. fsuby3.eq.0.0d0 )then
!                 FSUBY terms are both 0.0; assign 0.0 to arrays
               if (pvmrm .or. olm .or. arm2 .or. grsm)then
! ---                Assign 0.0 to CHI(IREC,ISRC,ITYP) before
!                    cycling to the next receptor to avoid
!                    persisting value from previous hour.
                  chi(irec,isrc,:) = 0.0d0
                  if(grsm)then
                     chi_ttravchm(irec,isrc) = 0.0d0
                  end if
                  if (pvmrm .or. grsm) then
                     hecntr(irec,isrc)  = 0.0d0
                     ueffs(irec,isrc)   = 0.0d0
                     epsef(irec,isrc)   = 0.0d0
                     ppfact(irec,isrc)  = 0.0d0
                     hecntr3(irec,isrc) = 0.0d0
                     ueff3s(irec,isrc)  = 0.0d0
                     epsef3(irec,isrc)  = 0.0d0
                     fopts(irec,isrc)   = 0.0d0
                  end if
               end if
!                 Also reinitialize HRVAL, AERVAL and AEROUT
               hrval(:)  = 0.0d0
               aerval(:) = 0.0d0
               aerout(:) = 0.0d0

! ---             Reset QTK since it may have been changed if
!                 point source approximation has been used
               qtk = qtksav

               cycle receptor_loop
            else

               if (npd .eq. 0) then
!                    Perform calculations for gases
!                    Assign plume tilt, HV = 0.0

                  adj = dqcorg * wqcorg

                  if (stable .or. (unstab.and.(hs.ge.zi))) then
!                       Calculate height of the "effective reflecting surface"
                     call refl_ht (he, x, 0.0d0, vsigz, hsbl)
                  else if ( unstab ) then
                     hsbl = 0.0d0
                  end if

!                    Determine the CRITical Dividing Streamline---   CALL CRITDS
                  call critds (he)

!                    Calculate the fraction of plume below
!                    HCRIT, PHEE                               ---   CALL PFRACT
                  call pfract (he)

!                    Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                  call fterm

!                    Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                  call aer_pchi( x, adj, vdepg, 0, aerout )
                  hrval = aerout

               else
!                    Perform calculations for particles, loop through particle sizes

!                    Begin loop over particle sizes
                  do j = 1, npd

!                       Calculate Plume Tilt Due to Settling, HV
                     hv = (x/us) * vgrav(j)

!                       Adjust Jth contribution by mass fraction and source
!                       depletion
                     adj = phi(j) * dqcor(j) * wqcor(j)

                     if (stable .or. (unstab.and.(hs.ge.zi))) then
!                          Calculate height of the "effective reflecting surface"
!                          Calculate Settled Plume Height(s), HESETL
                        hesetl = max( 0.0d0, he - hv )
                        call refl_ht (hesetl, x, 0.0d0, vsigz, hsbl)
                     else if ( unstab ) then
!                          Calculate Settled Plume Height(s), HESETL
                        hesetl = max( 0.0d0, 0.5d0*(hed1+hed2) - hv )
                        hsbl = 0.0d0
                     end if

!                       Determine the CRITical Dividing Streamline---   CALL CRITDS
                     call critds (hesetl)

!                       Calculate the fraction of plume below
!                       HCRIT, PHEE                               ---   CALL PFRACT
                     call pfract (hesetl)

!                       Calculate FOPT = f(PHEE)                  ---   CALL FTERM
                     call fterm

!                       Calculate Conc. for Virtual Point Source  ---   CALL AER_PCHI
                     call aer_pchi( x, adj, vdep(j), j, aerout )
                     hrval = hrval + aerout

                  end do
!                    End loop over particle sizes

               end if
            end if
! ---          Reset QTK since it may have been changed if
!              point source approximation has been used
            qtk = qtksav
         end if

         if (pvmrm) then
! ---          Store data by source and receptor for PVMRM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               hecntr(irec,isrc) = he
               ueffs(irec,isrc)  = ueff
               epsef(irec,isrc)  = epseff
            else
               hecntr(irec,isrc) = center
               ueffs(irec,isrc)  = ueffd
               epsef(irec,isrc)  = epseffd
            end if
            if (ppf .gt. 0.0d0) then
               ppfact(irec,isrc)  = ppf
               hecntr3(irec,isrc) = he3
               ueff3s(irec,isrc)  = ueff3
               epsef3(irec,isrc)  = epseff3
            else
               ppfact(irec,isrc)  = 0.0d0
               hecntr3(irec,isrc) = 0.0d0
               ueff3s(irec,isrc)  = 0.0d0
               epsef3(irec,isrc)  = 0.0d0
            end if
            fopts(irec,isrc) = fopt

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

         else if (olm) then
! ---          Store data by source and receptor for OLM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop
!    Added for TTRM; AECOM
         else if (runttrm) then
! ---          Store data by source and receptor for TTRM option
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop
!    End TTRM insert, Feb. 2021

         else if (arm2) then
! ---          Store data by source and receptor for ARM2 options
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

!           CERC 11/30/20
         else if (grsm) then

!              Calculate the effective wind speed
            if (stable .or. (unstab.and.(hs.ge.zi))) then
               uchm=ueff
            else
               uchm=ueffd
            end if
!              Calculate the conc x travel time for the chemistry scheme
            if(uchm/=0)then
               chi_ttravchm(irec,isrc)=&
               &max(0.0d0,hrval(1)*x/uchm) !Don't allow negative conc*travel times
            else
               !Error - no travel time if UEFF is zero
               call errhdl(path, modnam, 'E','601','GRSM')
               runerr=.true.
               return
            end if

! ---          Store data by source and receptor for GRSM options
            do ityp = 1, numtyp
               chi(irec,isrc,ityp) = hrval(ityp)
            end do

            if (stable .or. (unstab.and.(hs.ge.zi))) then
               hecntr(irec,isrc) = he
               ueffs(irec,isrc)  = ueff
               epsef(irec,isrc)  = epseff
            else
               hecntr(irec,isrc) = center
               ueffs(irec,isrc)  = ueffd
               epsef(irec,isrc)  = epseffd
            end if
            if (ppf .gt. 0.0d0) then
               ppfact(irec,isrc)  = ppf
               hecntr3(irec,isrc) = he3
               ueff3s(irec,isrc)  = ueff3
               epsef3(irec,isrc)  = epseff3
            else
               ppfact(irec,isrc)  = 0.0d0
               hecntr3(irec,isrc) = 0.0d0
               ueff3s(irec,isrc)  = 0.0d0
               epsef3(irec,isrc)  = 0.0d0
            end if
            fopts(irec,isrc) = fopt

!              Initialize __VAL arrays (1:NUMTYP)
            hrval(:)  = 0.0d0
            aerval(:) = 0.0d0
            aerout(:) = 0.0d0

!              Cycle to next receptor & skip call to SUMVAL (will be done later)
            cycle receptor_loop

         end if

!           Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
!           As noted just above, the calls to EV_SUMVAL and SUMVAL
!           are skipped for a PVMRM, OLM, ARM2 or GRSM run; the
!           summing is performed for these options in the
!           respective routines
         if (evonly) then
            call ev_sumval
         else
            do igrp = 1, numgrp
               call sumval
            end do
         end if

!           Initialize __VAL arrays (1:NUMTYP)
         hrval(:)  = 0.0d0
         aerval(:) = 0.0d0
         aerout(:) = 0.0d0

      end do receptor_loop
!        End Receptor LOOP

!        Output 'ARC' Values for EVALFILE                   ---   CALL EVALFL
      if (eval(isrc)) then
         call evalfl
      end if

   end if

   return
end


subroutine setsrc
!***********************************************************************
!             SETSRC Module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: Sets the Source Parameters for a Particular Source
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:
!                    Added Aircraft's Engine Parameters for
!                    Volume and Area Sources only for Aircraft Source Group
!                    Gavendra Pandey, UNC-IE Chapel Hill, USA,
!                    04/01/2023
!
!        MODIFIED:   To initialize AREA source dimensions and initial
!                    sigmas to correct initialization problems with PVMRM.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!                    To incorporate factor to adjust initial diameter of
!                    plume for capped stacks for use in the PRIME
!                    algorithm, for BETA-test draft option.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
!
!        MODIFIED:   To incorporate inputs for numerical integration
!                    algorithm for AREA source - 7/7/93
!
!        INPUTS:  Source Parameters Arrays
!                 Source Index
!
!        OUTPUTS: Source Parameters for a Particular Source
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   integer :: j

!     Variable Initializations
   modnam = 'SETSRC'

! --- Initialize AREA source dimensions and initial sigmas
!     to avoid initialization problems with PVMRM
   xinit  = 0.0d0
   yinit  = 0.0d0
   syinit = 0.0d0
   szinit = 0.0d0
   angle  = 0.0d0
   palpha = 0.0d0
   pdeff  = 0.0d0
   pitlen = 0.0d0
   pitwid = 0.0d0

!     Assign The Values From Array Elements To Variables
   if (srctyp(isrc)(1:5) .eq. 'POINT') then
      xs = axs(isrc)
      ys = ays(isrc)
      zs = azs(isrc)
      qs = aqs(isrc)
      hs = ahs(isrc)

      ds = ads(isrc)
      vs = avs(isrc)
      ts = ats(isrc)

      if (srctyp(isrc) .eq. 'POINTCAP') then
!           Assign factor to adjust initial diameter of plume for
!           capped stacks for use in PRIME algorithm.
         dsfact = adsfact(isrc)
      else
         dsfact = 1.0d0
      end if

   else if (srctyp(isrc) .eq. 'VOLUME') then
      xs = axs(isrc)
      ys = ays(isrc)
      zs = azs(isrc)
      qs = aqs(isrc)
      hs = ahs(isrc)

      syinit = asyini(isrc)
      szinit = aszini(isrc)
!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
!        Find Aircraft Index for This Source
!        Aircraft Engine Parameters
         mfuel    = amfuel(isrc)
         thrust   = athrust(isrc)
         vaa      = avaa(isrc)
         afr      = aafr(isrc)
         bypr     = abypr(isrc)
         rpwr     = arpwr(isrc)
         srcangle = asrcangle(isrc)
      else
         mfuel    = 0.0d0
         thrust   = 0.0d0
         vaa      = 0.0d0
         afr      = 0.0d0
         bypr     = 0.0d0
         rpwr     = 0.0d0
         srcangle = 0.0d0
      end if
!**  End Aircraft Plume Rise insert; April 2023

   else if (srctyp(isrc) .eq. 'AREA' .or.&
   &srctyp(isrc) .eq. 'LINE') then
      xs = axs(isrc)
      ys = ays(isrc)
      zs = azs(isrc)
      qs = aqs(isrc)
      hs = ahs(isrc)

      xinit = axinit(isrc)
      yinit = ayinit(isrc)
      angle = aangle(isrc)

      szinit = aszini(isrc)
      nvert  = 4

!        Store Vertices in Temporary Arrays
      do ivert = 1, nvert+1
         xvert(ivert) = axvert(ivert,isrc)
         yvert(ivert) = ayvert(ivert,isrc)
      end do

      xcntr = axcntr(isrc)
      ycntr = aycntr(isrc)
!**  Added for Aircraft Plume Rise; UNC-IE
      if (srctyp(isrc) .eq. 'AREA' .and.&
      &aftsrc(isrc) .eq. 'Y') then
!        Find Aircraft Index for This Source
!        Aircraft Engine Parameters
         mfuel    = amfuel(isrc)
         thrust   = athrust(isrc)
         vaa      = avaa(isrc)
         afr      = aafr(isrc)
         bypr     = abypr(isrc)
         rpwr     = arpwr(isrc)
         srcangle = asrcangle(isrc)
      else
         mfuel    = 0.0d0
         thrust   = 0.0d0
         vaa      = 0.0d0
         afr      = 0.0d0
         bypr     = 0.0d0
         rpwr     = 0.0d0
         srcangle = 0.0d0
      end if
!**  End Aircraft Plume Rise insert; April 2023

   else if (srctyp(isrc) .eq. 'AREAPOLY') then
      xs = axs(isrc)
      ys = ays(isrc)
      zs = azs(isrc)
      qs = aqs(isrc)
      hs = ahs(isrc)

      szinit = aszini(isrc)
      nvert  = nverts(isrc)

!        Store Vertices in Temporary Arrays
      do ivert = 1, nvert+1
         xvert(ivert) = axvert(ivert,isrc)
         yvert(ivert) = ayvert(ivert,isrc)
      end do

!        Assign equivalent values of XINIT and YINIT for calculating area
      xinit = axinit(isrc)
      yinit = ayinit(isrc)

!        Assign centroid of polygon
      xcntr = axcntr(isrc)
      ycntr = aycntr(isrc)

!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
!        Find Aircraft Index for This Source
!        Aircraft Engine Parameters
         mfuel    = amfuel(isrc)
         thrust   = athrust(isrc)
         vaa      = avaa(isrc)
         afr      = aafr(isrc)
         bypr     = abypr(isrc)
         rpwr     = arpwr(isrc)
         srcangle = asrcangle(isrc)
      else
         mfuel    = 0.0d0
         thrust   = 0.0d0
         vaa      = 0.0d0
         afr      = 0.0d0
         bypr     = 0.0d0
         rpwr     = 0.0d0
         srcangle = 0.0d0
      end if
!**  End Aircraft Plume Rise insert; April 2023

   else if (srctyp(isrc) .eq. 'AREACIRC') then
      xs = axs(isrc)
      ys = ays(isrc)
      zs = azs(isrc)
      qs = aqs(isrc)
      hs = ahs(isrc)

      szinit = aszini(isrc)
      nvert  = nverts(isrc)

!        Store Vertices in Temporary Arrays
      do ivert = 1, nvert+1
         xvert(ivert) = axvert(ivert,isrc)
         yvert(ivert) = ayvert(ivert,isrc)
      end do

!        Assign equivalent values of XINIT and YINIT for calculating area
      xinit = axinit(isrc)
      yinit = ayinit(isrc)

      xcntr = axcntr(isrc)
      ycntr = aycntr(isrc)
!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
!         Find Aircraft Index for This Source
!         Aircraft Engine Parameters
         mfuel    = amfuel(isrc)
         thrust   = athrust(isrc)
         vaa      = avaa(isrc)
         afr      = aafr(isrc)
         bypr     = abypr(isrc)
         rpwr     = arpwr(isrc)
         srcangle = asrcangle(isrc)
      else
         mfuel    = 0.0d0
         thrust   = 0.0d0
         vaa      = 0.0d0
         afr      = 0.0d0
         bypr     = 0.0d0
         rpwr     = 0.0d0
         srcangle = 0.0d0
      end if
!**  End Aircraft Plume Rise insert; April 2023

   else if (srctyp(isrc) .eq. 'OPENPIT') then
      xs = axs(isrc)
      ys = ays(isrc)
      zs = azs(isrc)
      qs = aqs(isrc)
!        Set Emission Height of Effective Area, HS = 0.0
      hs = 0.0d0
!        Set Height of Emissions Above Base of Pit, EMIHGT
      emihgt = ahs(isrc)
      nvert  = 4

      xinit = axinit(isrc)
      yinit = ayinit(isrc)
      angle = aangle(isrc)
      palpha = aalpha(isrc)
      pdeff  = apdeff(isrc)
      szinit = aszini(isrc)
      pitlen = max(xinit,yinit)
      pitwid = min(xinit,yinit)

!        Store Vertices in Temporary Arrays
      do ivert = 1, nvert+1
         xvert(ivert) = axvert(ivert,isrc)
         yvert(ivert) = ayvert(ivert,isrc)
      end do

      xcntr = axcntr(isrc)
      ycntr = aycntr(isrc)

   else if (srctyp(isrc) .eq. 'BUOYLINE') then
      qs = aqs(isrc)

!     Added for sidewash
   else if (srctyp(isrc) .eq. 'SWPOINT') then
      xs = axs(isrc)
      ys = ays(isrc)
      zs = azs(isrc)
      qs = aqs(isrc)
      hs = ahs(isrc)
      bw = abw(isrc)
      bl = abl(isrc)
      bh = abh(isrc)
      ba = aba(isrc)

   end if

   npd = inpd(isrc)
   if (npd .gt. 0) then
      do j = 1, npd
         pdiam(j) = apdiam(j,isrc)
         phi(j)   = aphi(j,isrc)
         pdens(j) = apdens(j,isrc)
         vgrav(j) = avgrav(j,isrc)
         tstop(j) = atstop(j,isrc)
      end do
   end if

! --- Initialize SURFAC for this source
   if( hs .lt. 0.1d0 * zi )then
      surfac = .true.
   else
      surfac = .false.
   end if

   return
end

subroutine fluxes(vseq)
!***********************************************************************
!             FLUXES Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the source momentum and buoyancy fluxes
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        MODIFIED:
!                  To incorporate capped stack (POINTCAP) and
!                  horizontal release (POINTHOR) options.
!                  R. W. Brode, MACTEC (f/k/a PES), Inc., 09/30/05
!                  R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
!
!        INPUTS:  Ambient temperature at source height, TA
!                 Source gas exit temperature, TS
!                 Source gas exit velocity, VS
!                 Source diameter, DS
!
!        OUTPUTS: Momentum flux, FM, and buoyancy flux, FB
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   double precision  tseq, dseq, vseq

!     Variable Initializations
   modnam = 'FLUXES'

!     Note:  TA is now ambient temperature AT STACK HEIGHT and
!            was computed in METINI

!     Check for Negative Stack Temperature, Used to
!     Indicate Constant TS-TA
   if (ts .lt. 0.0d0) then
      ts = ta + dabs(ts)
   end if

   if (ts .lt. ta)  ts = ta
   fb = (0.25d0 / ts) * (vs * ds * ds) * g * (ts - ta)
   fm = (0.25d0 / ts) * (vs * ds * ds) * vs * ta

!     Set effective exit velocity, diameter and momentum flux for
!     capped stack (POINTCAP) or horizontal release (POINTHOR).
   if (srctyp(isrc) .eq. 'POINTCAP') then
      vseq = 0.001d0
      dseq = ds * dsqrt( vs/vseq )
      tseq = ts
      fm = (0.25d0 / tseq) * (vseq * dseq * dseq) * vseq * ta
   else if (srctyp(isrc) .eq. 'POINTHOR') then
      vseq = 0.001d0
      dseq = ds * dsqrt( vs/vseq )
      tseq = ts
      fm = (0.25d0 / tseq) * (vseq * dseq * dseq) * vseq * ta
   else
      vseq = vs
      dseq = ds
      tseq = ts
   end if

!     To avoid divide by zero or underflow, set FB and FM to a minimum value
   if( fb .lt. 1.0d-10 ) fb = 1.0d-10
   if( fm .lt. 1.0d-10 ) fm = 1.0d-10

   return
end

subroutine heff ( xarg )
!***********************************************************************
!             HEFF Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Effective Plume Height (m)
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        REVISIONS:  Corrected to include plume rise for penetrated
!                    source, DHP3, for purposes of calculating plume
!                    height at 20km for use in wet deposition/depletion
!                    calculations.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, MM/DD/2013
!
!                    Corrected formulations for HEN1 & HEN2 per
!                    Model Formulation Document and conversation
!                    with Russ Lee and Jeff Weil.
!                    Roger Brode, PES, Inc. - 12/7/94
!
!        INPUTS:  Arrays of Source Parameters
!                 Logical Wake Flags
!                 Meteorological Variables for One Hour
!                 Wind Speed Adjusted to Stack Height
!                 Downwind Distance
!                 Terrain Elevation of Receptor
!
!        OUTPUTS: Effective Plume Height (HE)
!
!        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
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
   character modnam*12
   double precision :: xarg

!     Variable Initializations
   modnam = 'HEFF'

   dhp3plat = 0.0d0     !D142 initialized DHP3PLAT 2/2/23 WSP

!     Compute the effective plume height
   if( stable  .or.  (unstab .and. (hs .ge. zi) ) )then
!        The atmosphere is stable or the release is above the CBL
!        mixing ht.
      he = hsp + dhp
!        Don't Allow Effective Plume Height to be < 0.0
      he = max( 0.0d0, he)

      hed1 = 0.0d0
      hed2 = 0.0d0
      hen1 = 0.0d0
      hen2 = 0.0d0
      he3  = 0.0d0

!CRT     D063 Platform Downwash Debug
      if (platfmdbg .and. osplat(isrc)) then
         write(platfmdbunt,'(A, 1(2X, A, F12.3), 3(2X, A, F7.3))')&
         &'calc1.f/HEFF(STABLE): ',&
         &'XARG = ', xarg,&
         &'HSP = ', hsp,&
         &'DHP = ', dhp,&
         &'HE = ', he
      end if

   elseif( unstab )then
!        The atmosphere is unstable and the release is below the
!        mixing ht.

! ---    Set HE = 0.0 for unstable conditions
      he = 0.0d0

!        Compute the effective direct plume height, for both the
!        Plume 1 (HED1) and Plume 2 (HED2)
      hed1 = hsp + dhp1 + (asub1 * wstar * xarg / ueffd)
      hed2 = hsp + dhp1 + (asub2 * wstar * xarg / ueffd)

!        Compute the effective indirect plume height, for both the
!        updraft (HEN1) and downdraft (HEN2)

      hen1 = hsp + dhp1 - dhp2 + (asub1 * wstar * xarg / ueffn)
      hen2 = hsp + dhp1 - dhp2 + (asub2 * wstar * xarg / ueffn)

! ---    Include calculation of penetrated plume rise and height
      if( ppf .gt. 0.0d0 )then

!           Compute the plume height for the penetrated source
!           (See Eq. 8 in the reference for Source 3)
         if (ppf .eq. 1.0d0) then
            dhp3 = hedhh * (zi-hsp)
         else
            dhp3 = 0.75d0 * (zi-hsp) * hedhh + 0.5d0 * (zi-hsp)
         end if

!MGS        D063 Platform Downwash
         if (osplat(isrc))then
            he3 = hsp + dhp3 - dhp3plat
         else
            he3 = hsp + dhp3
         end if

      else
         dhp3 = 0.0d0
         he3  = 0.0d0

      end if

!CRT     D063 Platform Downwash Debug
      if (platfmdbg .and. osplat(isrc)) then
         write(platfmdbunt,'(A, 1(2X, A, F12.3), 12(2X, A, F7.3))')&
         &'calc1.f/HEFF(UNSTABLE): ',&
         &'XARG = ', xarg,&
         &'HSP = ', hsp,&
         &'DHP1 = ', dhp1,&
         &'WSTAR = ', wstar,&
         &'UEFFD = ', ueffd,&
         &'ASUB1 = ', asub1,&
         &'HED1 = ', hed1,&
         &'ASUB2 = ', asub2,&
         &'HED2 = ', hed2,&
         &'DHP2 = ', dhp2,&
         &'HEN1 = ', hen1,&
         &'HEN2 = ', hen2,&
         &'HE3 = ', he3
      end if

   end if

   return
end

subroutine prmheff
!***********************************************************************
!             PRMHEFF Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Effective Plume Height (m)
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        REVISIONS:  Corrected formulations for HEN1 & HEN2 per
!                    Model Formulation Document and conversation
!                    with Russ Lee and Jeff Weil.
!                    Roger Brode, PES, Inc. - 12/7/94
!
!        INPUTS:  Arrays of Source Parameters
!                 Logical Wake Flags
!                 Meteorological Variables for One Hour
!                 Wind Speed Adjusted to Stack Height
!                 Downwind Distance
!                 Terrain Elevation of Receptor
!
!        OUTPUTS: Effective Plume Height (HE)
!
!        CALLED FROM:   PCALC, VCALC, ACALC, PLUMEF
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
   character modnam*12

!     Variable Initializations
   modnam = 'PRMHEFF'

!     Compute the effective plume height
   if( stable )then
!        The atmosphere is stable or the release is above the CBL
!        mixing ht.

      he = hs + dhp
!        Don't Allow Effective Plume Height to be < 0.0
      he = max( 0.0d0, he)

   else if( unstab )then
!        The atmosphere is unstable and the release is below the
!        mixing ht.

      he = hs + dhp
!        Don't Allow Effective Plume Height to be < 0.0
      he = max( 0.0d0, he)
   end if

   return
end

subroutine pdis ( xarg )
!***********************************************************************
!             PDIS module of the AMS/EPA Regulatory Model - AERMOD
! ----------------------------------------------------------------------
! ---    ISC-PRIME     Version 1.0    Level 970812              Modified
! ---        D. Strimaitis
! ---        Earth Tech, Inc.
!            Prepared for EPRI under contract WO3527-01
! ----------------------------------------------------------------------
!
!        PURPOSE: Calculates Dispersion Parameters for POINT Sources
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!        DATE:    Spetember 30, 1993
!
!        REVISIONS:
!                  Added call to PLAT_DOWNWASH to get additional initial
!                  SIGMAY and SIGMAZ due to PLATFORM.
!                  Michelle G. Snyder, WOOD, 8/5/2021
!
!                 SZSURF calculation reinstated 7/13/94, R.F. Lee
!
!        INPUTS:  Arrays of Source Parameters
!                 Logical Wake Flags
!                 Wake Plume Height, HEMWAK
!                 Meteorological Variables for One Hour
!                 Distance, XARG
!
!        OUTPUTS: Lateral and Vertical Dispersion Coefficients, SY and SZ
!
!        CALLED FROM:   PCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: xarg

!     Variable Initializations
   modnam = 'PDIS'

!     Calculate Sigma-z from formuale                 --- CALL SIGZ
   call sigz ( xarg )
!     Calculate Sigma-y from formulae                 --- CALL SIGY
   call sigy ( xarg )

!     Set all virtual source terms to 0 for non-downwashing sources
   vsigy = 0.0d0
   vsyn  = 0.0d0
   vsigz = 0.0d0
   vszd1 = 0.0d0
   vszd2 = 0.0d0
   vszn1 = 0.0d0
   vszn2 = 0.0d0
   vsz3  = 0.0d0
   vsy3  = 0.0d0

!     Calculate the buoyancy-induced dispersion parameters
   if( nobid )then
!        Set BID Terms to 0.0
      if( stable  .or.  (unstab .and. (hs .ge. zi) ) )then
         syb = 0.0d0
         szb = 0.0d0

      else if( unstab )then
         syb  = 0.0d0
         szbd = 0.0d0
         szbn = 0.0d0
         syb3 = 0.0d0
         szb3 = 0.0d0

      end if

   else
!        Specify BID Terms                                 --- CALL BID
      call bid

   end if

! ----------------------------------------------------------------------
! ---------- BEGIN PLATFORM DOWNWASH INTITAL SIGMAY and SIGMAZ ---------
! ----------------------------------------------------------------------
!MGS        Compute additional initial sigmay and sigmaz due to presence of
!MGS        platform. Calls PLAT_DOWNWASH(contained in calc1.f) for
!MGS        effective height of each plume being modeled. The sigma's are
!MGS        added in quadrature in sigmas.f/RMSSIG.
!MGS        Michelle G. Snyder, WOOD, 8/5/2021

!CRT  D063
!     Initialize platform downwash sigmas
   platsz   = 0.0d0  ! stable or unstable injected
   platsy   = 0.0d0  ! stable or unstable injected
   platszd1 = 0.0d0  ! unstable, direct, updraft
   platsyd1 = 0.0d0  ! unstable, direct, updraft
   platszd2 = 0.0d0  ! unstable, direct, downdraft
   platsyd2 = 0.0d0  ! unstable, direct, downdraft
   platszn1 = 0.0d0  ! unstable, indirect, updraft
   platsyn1 = 0.0d0  ! unstable, indirect, updraft
   platszn2 = 0.0d0  ! unstable, indirect, downdraft
   platsyn2 = 0.0d0  ! unstable, indirect, downdraft
   platszp  = 0.0d0  ! unstable, penetrated
   platsyp  = 0.0d0  ! unstable, penetrated

   if (osplat(isrc)) then


!           Get initial value for downwash sigma-z
!           Consider all conditions in HEFF
      if (stable) then

!              The atmosphere is stable or the release is above the CBL
         call plat_downwash (xarg, plathb(isrc)+0.0d0,&
         &platwb(isrc), he, platsz, platsy)

      elseif (unstab .and. (hs .ge. zi)) then

!              Do not apply to injected plume
         continue


      elseif( unstab )then
!              Direct Plume updraft(HED1) and downdraft(HED2)
         call plat_downwash (xarg, plathb(isrc)+0.0d0,&
         &platwb(isrc), hed1, platszd1, platsyd1)

         call plat_downwash (xarg, plathb(isrc)+0.0d0,&
         &platwb(isrc), hed2, platszd2, platsyd2)

!              Indirect Plume updraft(HEN1) and downdraft(HEN2)
         call plat_downwash (xarg, plathb(isrc)+0.0d0,&
         &platwb(isrc), hen1, platszn1, platsyn1)

         call plat_downwash (xarg, plathb(isrc)+0.0d0,&
         &platwb(isrc), hen2, platszn2, platsyn2)

!              Penetrated Plume
         call plat_downwash (xarg, plathb(isrc)+0.0d0,&
         &platwb(isrc), he3, platszp, platsyp)

      end if

   end if
! ----------------------------------------------------------------------
! ---------- END PLATFORM DOWNWASH INTITAL SIGMAY and SIGMAZ -----------
!----------             RESUME NORMAL PROCESSING           -------------
! ----------------------------------------------------------------------

!---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
   call rmssig

   return
end

subroutine vdis (xarg)
!***********************************************************************
!             VDIS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Dispersion Parameters for VOLUME Sources
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        MODIFIED:Modified to add plume rise for
!                 VOLUME Source only for Aircraft Source Group.
!                 Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                 04/01/2023
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Arrays of Source Parameters
!                 Meteorological Variables for One Hour
!                 Downwind Distance
!
!        OUTPUTS: Lateral and Vertical Dispersion Coefficients
!
!        CALLED FROM:   VCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: xarg

!     Variable Initializations
   modnam = 'VDIS'

!     Calculate Sigma-y from formulae                 --- CALL SIGY
   call sigy ( xarg )
!     Calculate Sigma-z from formulae                 --- CALL SIGZ
   call sigz ( xarg )

!     Set virtual source terms based on initial sigmas input by user
   vsigy = syinit
   vsyn  = syinit
   vsigz = szinit
   vszd1 = szinit
   vszd2 = szinit
   vszn1 = szinit
   vszn2 = szinit
   vsz3  = 0.0d0
   vsy3  = 0.0d0
!**  Added for Aircraft Plume Rise; UNC-IE
   if (aftsrc(isrc) .eq. 'Y') then
!         CALL ADELTAH (XARG)
!        Specify BID Terms                                 --- CALL BID
      call bid
   else
!**  End Aircraft Plume Rise insert; April 2023
!     Set BID terms to zero
      syb  = 0.0d0
      szb  = 0.0d0
      szbd = 0.0d0
      szbn = 0.0d0
      syb3 = 0.0d0
      szb3 = 0.0d0
   end if                 ! Added for Aircraft; UNC-IE

!---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
   call rmssig

   return
end

subroutine adisy(xarg)
!***********************************************************************
!                 ADISY Module of the AERMOD Model
!
!        PURPOSE: Calculates Lateral Dispersion Parameters for AREA Sources
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    July 21, 1994
!
!        MODIFIED:   Modified to add plume rise for
!                    AREA Source only for Aircraft Source Group.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   To calculate sigma-y and sigma-z separately
!                    for AREA source - R.Brode, PES, 12/9/98
!
!        INPUTS:  Arrays of Source Parameters
!                 Meteorological Variables for One Hour
!                 Downwind Distance
!
!        OUTPUTS: Lateral and Vertical Dispersion Coefficients
!
!        CALLED FROM:   VCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: xarg

!     Variable Initializations
   modnam = 'ADISY'

!     Calculate Sigma-y from formulae                 --- CALL SIGY
   call sigy ( xarg )

!     Set virtual source terms for AREA sources to zero
   vsigy = 0.0d0
   vsy3  = 0.0d0

!**  Added for Aircraft Plume Rise; UNC-IE
   if (aftsrc(isrc) .eq. 'Y') then
!         CALL ADELTAH (XARG)
!        Specify BID Terms                                 --- CALL BID
      call bid
   else
!**  End Aircraft Plume Rise insert; April 2023
!     Set BID terms to zero
      syb  = 0.0d0
      syb3 = 0.0d0
   end if                 ! Added for Aircraft; UNC-IE

   sy = syamb

   return
end

subroutine adisz(xarg)
!***********************************************************************
!                 ADISZ Module of the AERMOD Model
!
!        PURPOSE: Calculates Vertical Dispersion Parameters for AREA
!                 and LINE Sources
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    July 21, 1994
!
!        MODIFIED:   Modified to add plume rise for
!                    AREA Source only for Aircraft Source Group.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   To calculate sigma-y and sigma-z separately
!                    for AREA source - R.Brode, PES, 12/9/98
!
!        INPUTS:  Arrays of Source Parameters
!                 Meteorological Variables for One Hour
!                 Downwind Distance
!
!        OUTPUTS: Lateral and Vertical Dispersion Coefficients
!
!        CALLED FROM:   VCALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: xarg

!     Variable Initializations
   modnam = 'ADISZ'

!     Calculate Sigma-z from formulae                 --- CALL SIGZ
   call sigz ( xarg )

!     Set virtual source terms based on initial sigmas input by user
   vsigz = szinit
   vszd1 = szinit
   vszd2 = szinit
   vszn1 = szinit
   vszn2 = szinit
   vsz3  = 0.0d0

!**  Added for Aircraft Plume Rise; UNC-IE
   if (aftsrc(isrc) .eq. 'Y') then
!         CALL ADELTAH (XARG)
!        Specify BID Terms                                 --- CALL BID
      call bid
   else
!**  End Aircraft Plume Rise insert; April 2023
!     Set BID terms to zero
      szb  = 0.0d0
      szbd = 0.0d0
      szbn = 0.0d0
      szb3 = 0.0d0
   end if                ! Added for Aircraft; UNC-IE

!---- Calculate the root-mean-square sigma_Y and sigma_Z   --- CALL RMSSIG
   call rmssig

   return
end

subroutine aer_pchi( xarg, adj, vdinp, jin, aerout )
!***********************************************************************
!        AER_PCHI Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Hourly Concentration for POINT Sources
!                 Using Gaussian Plume Equation
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    November 10, 2000
!
!        MODIFIED:   To include lateral term (FSUBY) in weighting of
!                    direct and penetrated contributions for wet dep.
!                    Added debug statement for CONC based on ENSR.
!                    - R.Brode, MACTEC, 7/27/2004
!
!        MODIFIED:   To correct WETFLUX values for conversion from
!                    seconds to hours and to include SQRT(2PI) in
!                    denominator of integrated vertical term.
!                    - R.Brode, MACTEC, 3/9/2004
!
!        INPUTS:  Distance, XARG (downwind for plume; radial for pancake)
!                 Crosswind Distance
!                 Plume Height
!                 Stack Top Wind Speed
!                 Lateral Dispersion Parameter
!                 Vertical Dispersion Parameter
!                 Stability Class
!                 Mixing Height
!                 Receptor Height Above Ground
!                 Emission Rate and Units Scaling Factor
!                 Source Parameter Arrays
!
!        OUTPUTS: AEROUT, AERMOD Concentration for Particular
!                 Source/Receptor Combination
!
!        CALLED FROM:   AERCALC, VOLCALC, ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   integer :: jin
   double precision :: aerout(numtyp), xarg, adj, vdinp, dryflux,&
   &wetflux
   character modnam*12

!     Variable Initializations
   modnam = 'AER_PCHI'
   dryflux = 0.0d0
   wetflux = 0.0d0

!---- Calculate the contribution due to horizontal plume, CWRAP
   if (fopt .eq. 0.0d0) then
      cwrap = 0.0d0
   else
      call cplume (zrt, cwrap)
   end if

!---- Calculate the contribution due to terrain-following plume, CLIFT
   if (zrt .eq. zflag) then
!----    Effective receptor heights are equal, therefore CLIFT = CWRAP
      clift = cwrap
   else if (fopt .eq. 1.0d0) then
      clift = 0.0d0
   else
      call cplume (zflag, clift)
   end if

!---- Calculate the exponential decay term, D               ---   CALL DECAY
   Call decay (xarg)

!---- Calculate the hourly concentration and deposition values
   ityp = 0
   if (conc) then
      ityp = 1
      aerout(ityp) = adj * emifac(ityp) *&
      &(fopt * cwrap + (1.0d0 - fopt) * clift) * d

!   ENHANCEMENT TO DEBUG OUTPUT BASED ON ENSR
      if (debug) then
         write(dbgunt,10) ityp, adj, fopt, cwrap, clift, d,&
         &aerout(ityp)
10       format(/,'ITYP = ',i2,' - CONC:',&
         &/,'AEROUT(ITYP) = ADJ * EMIFAC(ITYP) * (FOPT * ',&
         &'CWRAP + (1.0 -FOPT) * CLIFT) * D',&
         &/,' ADJ   = ',g16.8,&
         &/,' FOPT  = ',g16.8,&
         &/,' CWRAP = ',g16.8,&
         &/,' CLIFT = ',g16.8,&
         &/,' D     = ',g16.8,&
         &/,' AEROUT(ITYP) = ',g16.8,/)
      end if

   end if

   if (depos .or. ddep) then
!        Calculate DRYFLUX, vertical term for wet deposition
!----    Calculate the contribution due to horizontal plume, CWRAP
      if (fopt .eq. 0.0d0) then
         cwrap = 0.0d0
      else
         call cplume (zrt-zflag+zrdep, cwrap)
      end if

!----    Calculate the contribution due to terrain-following plume, CLIFT
      if (zrt .eq. zflag) then
!----       Effective receptor heights are equal, therefore CLIFT = CWRAP
         clift = cwrap
      else if (fopt .eq. 1.0d0) then
         clift = 0.0d0
      else
         call cplume (zrdep, clift)
      end if

      dryflux = (fopt * cwrap + (1.0d0 - fopt) * clift) * d&
      &
   end if

   if (depos .or. wdep) then
!        Calculate WETFLUX, vertical term for wet deposition.
!        Note that the SRT2PI for the integrated vertical term
!        has been removed since it should be divided by SRT2PI.
!        Additional factor of 3600. has been added to denominator
!        to account for conversion from seconds to hours when
!        divided by wind speed below.
      if (prate .gt. 0.0d0) then
         if (npd .eq. 0) then
            wetflux = (adj*fracsat*prate*1.0d6*rgas*ta)/&
            &(zsubp*henry(isrc)*1.0d9*denom*3600.0d0)
         else
            wetflux = 1.0d-3*adj*washout(jin)*prate/&
            &(zsubp*3600.0d0)
         end if
      else
         wetflux = 0.0d0
      end if
   end if

   if (depos) then
      ityp = ityp + 1
      if( stable  .or.  (unstab .and. (hs .ge. zi) ) ) then
         aerout(ityp) = adj * vdinp * emifac(ityp) * dryflux +&
         &qtk * wetflux * emifac(ityp) * fsuby/ueff
      else if (unstab) then
         aerout(ityp) = adj * vdinp * emifac(ityp) * dryflux +&
         &qtk * wetflux * emifac(ityp) *&
         &(ppf*fsuby3/ueff3+(1.0d0-ppf)*fsuby/ueffd)
      end if

      if (debug) then
         write(dbgunt,11) ityp, adj, vdinp, dryflux, wetflux,&
         &aerout(ityp)
11       format(/,'ITYP = ',i2,' - DEPOS:',&
         &/,' ADJ     = ',g16.8,&
         &/,' VDINP   = ',g16.8,&
         &/,' DRYFLUX = ',g16.8,&
         &/,' WETFLUX = ',g16.8,&
         &/,' AEROUT(ITYP) = ',g16.8,/)
      end if

   end if

   if (ddep) then
      ityp = ityp + 1
      aerout(ityp) = adj * vdinp * emifac(ityp) * dryflux

      if (debug) then
         write(dbgunt,12) ityp, adj, vdinp, dryflux,&
         &aerout(ityp)
12       format(/,'ITYP = ',i2,' - DDEP:',&
         &/,' ADJ     = ',g16.8,&
         &/,' VDINP   = ',g16.8,&
         &/,' DRYFLUX = ',g16.8,&
         &/,' AEROUT(ITYP) = ',g16.8,/)
      end if

   end if

   if (wdep) then
      ityp = ityp + 1
      if( stable  .or.  (unstab .and. (hs .ge. zi) ) ) then
         aerout(ityp) = qtk * wetflux * emifac(ityp) * fsuby/ueff
      else if (unstab) then
         aerout(ityp) = qtk * wetflux * emifac(ityp) *&
         &(ppf*fsuby3/ueff3+(1.0d0-ppf)*fsuby/ueffd)
      end if

      if (debug) then
         write(dbgunt,13) ityp, adj, zsubp, prate, wetflux,&
         &aerout(ityp)
13       format(/,'ITYP = ',i2,' - WDEP:',&
         &/,' ADJ     = ',g16.8,&
         &/,' ZSUBP   = ',g16.8,&
         &/,' PRATE   = ',g16.8,&
         &/,' WETFLUX = ',g16.8,&
         &/,' AEROUT(ITYP) = ',g16.8,/)
      end if

   end if


!CRFL Call to METDEB was moved here from METEXT on 9/26/94, R.F. Lee.
!CRFL Print meteorological debug output.                   ---   CALL METDEB
   if (meteordbg) call metdeb

   if ( debug ) then
!        Print Out Debugging Information                    ---   CALL DEBOUT
      call debout
   end if

   return
end

subroutine prm_pchi(adj, vdinp, jin, insrc)                       ! ORD(EMM) change
!***********************************************************************
!        PRM_PCHI Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Hourly Concentration for POINT Sources
!                 with PRIME Downwash Algorithm
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:     November 10, 2000
!
!        MODIFIED:   To correct WETFLUX values for conversion from
!                    seconds to hours and to include SQRT(2PI) in
!                    denominator of integrated vertical term.
!                    - R.Brode, MACTEC, 3/9/2004
!
!                 Modifications for the ORD_DWNW & AWMADWNW ALPHA options
!                    - August 2018
!
!                 Modified for TTRM2 option to save UEFFe for travel time
!                    - November 2021
!
!        INPUTS:  Downwind Distance
!                 Crosswind Distance
!                 Plume Height
!                 Stack Top Wind Speed
!                 Lateral Dispersion Parameter
!                 Vertical Dispersion Parameter
!                 Stability Class
!                 Mixing Height
!                 Receptor Height Above Ground
!                 Emission Rate and Units Scaling Factor
!                 Source Parameter Arrays
!
!        OUTPUTS: PRMVAL, PRIME Concentration for Particular
!                 Source/Receptor Combination, summed across
!                 three PRIME "sources", i.e., primary source,
!                 inside cavity source and outside cavity source
!
!        CALLED FROM:   PRMCALC
!***********************************************************************

!     Variable Declarations
   use main1
   use prime_wakedat, only : hb
   implicit none
   character modnam*12

   integer          :: jin, insrc, ndxbldg          !  EMM ADD
   integer          :: NDXBHIe, NDXBLOe             !  EMM ADD
   integer          :: ht_ndx
   double precision :: zpht, uhb                    !  EMM ADD
   double precision :: UEFFe                        !  EMM ADD
   double precision :: adj, vdinp, dryflux, wetflux
   double precision :: ht2use,U_above,U_below
!     ZPHT_MIN is the lowest height to use in the calculations; this is
!       to avoid problems with very short stacks and stack-tip downwash
   double precision, parameter :: zpht_min=0.5d0
! Unused:       INTEGER          :: LOWWFLG
! Unused:       DOUBLE PRECISION :: UZPHT, UZLO

!     Variable Initializations
   modnam = 'PRM_PCHI'
   dryflux = 0.0d0
   wetflux = 0.0d0

!---- Calculate the exponential decay term, D               ---   CALL DECAY
   Call decay (x)

   if (L_ORD_Ueff) then
! ---    ORD formulation
      zpht = dhp + hs
      call locate(gridht, 1, mxglvl, zflag, NDXBLOe)
      call locate(gridht, 1, mxglvl, zpht, NDXBHIe)

!        Find the wind speed at building height for source 2 calc
      call locate(gridht, 1, mxglvl, hb, ndxbldg)
      uhb = gridws(ndxbldg)  ! Wind speed at building height

      call anyavg ( mxglvl, gridht, gridws, zflag ,NDXBLOe+1,&
      &zpht, NDXBHIe, UEFFe )
      if ((insrc.eq.2) .or. (insrc .eq. 3)) then
         UEFFe = us
      end if

   else if (L_AWMA_Ueff) then
! ---    AWMA PRIME2 Subcommittee (CPP) formulation
      zpht = dhp + hs       ! GET PLUME HEIGHT

!        Set the height to use in calculations to
!        MAX(plume ht, receptor ht)
      ht2use = max(zpht,zpht_min)

!        Identify the grid height immediately below HT2USE
      call locate(gridht, 1, mxglvl, ht2use, ht_ndx)
      U_below = gridws(ht_ndx)
      U_above = gridws(ht_ndx+1)

      call gintrp(gridht(ht_ndx),U_below, gridht(ht_ndx+1),U_above,&
      &ht2use,UEFFe)

!        Check if source is inside or outside cavity and if so, use
!          appropriate wind speed for calculations
      if ((insrc .eq. 3)) then     !! "inside" cavity source
         UEFFe = us                 ! Wind speed at stack height

      else if((insrc.eq.2)) then    !! "outside" cavity source
!           Get grid height immediately BELOW the building ht
         call locate(gridht, 1, mxglvl, hb, ndxbldg)
!           Get the wind speed at
         uhb = gridws(ndxbldg)
         UEFFe = uhb                ! Wind speed at 'building ht'
      end if

   else
! --- Set UEFFe to the regulatory AERMOD value
      UEFFe = us
   end if

! Added for TTRM; AECOM, Nov. 2021
   if (runttrm) then
      ttrmout(irec,isrc,24) = UEFFe
   endif
! End TTRM insert

!---- Calculate the hourly concentration value
   ityp = 0
   if (conc) then
      ityp = 1
!----    Calculate the contribution due to horizontal plume, CWRAP
      if (fopt .eq. 0.0d0) then
         cwrap = 0.0d0
      else
         call prm_plume (zrt, UEFFe, cwrap)
      end if

!----    Calculate the contribution due to terrain-following plume, CLIFT
      if (zrt .eq. zflag) then
!----       Effective receptor heights are equal, therefore CLIFT = CWRAP
         clift = cwrap
      else if (fopt .eq. 1.0d0) then
         clift = 0.0d0
      else
         call prm_plume (zflag, UEFFe, clift)
      end if

      prmval(ityp) = prmval(ityp) + adj * emifac(ityp) *&
      &(fopt * cwrap + (1.0d0 - fopt) * clift) * d
   end if

   if (depos .or. ddep) then
!        Calculate DRYFLUX, vertical term for wet deposition
!----    Calculate the contribution due to horizontal plume, CWRAP
      if (fopt .eq. 0.0d0) then
         cwrap = 0.0d0
      else
         call prm_plume (zrt-zflag+zrdep, UEFFe, cwrap)
      end if

!----    Calculate the contribution due to terrain-following plume, CLIFT
      if (zrt .eq. zflag) then
!----       Effective receptor heights are equal, therefore CLIFT = CWRAP
         clift = cwrap
      else if (fopt .eq. 1.0d0) then
         clift = 0.0d0
      else
         call prm_plume (zrdep, UEFFe, clift)
      end if

      dryflux = (fopt * cwrap + (1.0d0 - fopt) * clift) * d
   end if

   if (depos .or. wdep) then
!        Calculate WETFLUX, vertical term for wet deposition
!        Note that the SRT2PI for the integrated vertical term
!        has been removed since it should be divided by SRT2PI.
!        Additional factor of 3600. has been added to denominator
!        to account for conversion from seconds to hours when
!        divided by wind speed below.
      if (prate .gt. 0.0d0) then
         if (npd .eq. 0) then
            wetflux = (adj*fracsat*prate*1.0d6*rgas*ta)/&
            &(zsubp*henry(isrc)*1.0d9*denom*3600.0d0)
         else
            wetflux = 1.0d-3*adj*washout(jin)*prate/&
            &(zsubp*3600.0d0)
         end if
      else
         wetflux = 0.0d0
      end if
   end if

   if (depos) then
      ityp = ityp + 1
      prmval(ityp) = prmval(ityp) + adj*vdinp*emifac(ityp)*dryflux +&
      &qtk * wetflux * emifac(ityp) * fsuby/ueff
   end if

   if (ddep) then
      ityp = ityp + 1
      prmval(ityp) = prmval(ityp) + adj*vdinp*emifac(ityp)*dryflux
   end if

   if (wdep) then
      ityp = ityp + 1
      prmval(ityp) = prmval(ityp)+qtk*wetflux*emifac(ityp)*fsuby/ueff
   end if

   if ( debug ) then
!        Print Out Debugging Information                    ---   CALL DEBOUT
      call debout
   end if

   return
end

subroutine aer_achi( xarg, adj, vdinp, jin, fyarg, pout )
!***********************************************************************
!        AER_ACHI Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Hourly Concentration for AREA Sources
!                 Using Gaussian Plume Equation
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    November 10, 2000
!
!        MODIFIED:   To correct WETFLUX values for conversion from
!                    seconds to hours and to include SQRT(2PI) in
!                    denominator of integrated vertical term.
!                    - R.Brode, MACTEC, 3/9/2004
!
!        INPUTS:  Distance, XARG (downwind for plume; radial for pancake)
!                 Crosswind Distance
!                 Plume Height
!                 Stack Top Wind Speed
!                 Lateral Dispersion Parameter
!                 Vertical Dispersion Parameter
!                 Stability Class
!                 Mixing Height
!                 Receptor Height Above Ground
!                 Emission Rate and Units Scaling Factor
!                 Source Parameter Arrays
!
!        OUTPUTS: AEROUT, AERMOD Concentration for Particular
!                 Source/Receptor Combination
!
!        CALLED FROM:   AERCALC, VOLCALC, ACALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   integer :: jin
   double precision :: pout, xarg, adj, vdinp, fyarg, dryflux,&
   &wetflux
   logical :: sconc, sdepos, sddep, swdep
   character modnam*12

!     Variable Initializations
   modnam = 'AER_ACHI'
   dryflux = 0.0d0
   wetflux = 0.0d0

!     Determine appropriate output type for this ITYP, assign output type
!     logicals to local variables, and set others to .FALSE.
   if (outtyp(ityp) .eq. 'CONC') then
      sconc  = .true.
      sdepos = .false.
      sddep  = .false.
      swdep  = .false.
   else if (outtyp(ityp) .eq. 'DEPOS') then
      sconc  = .false.
      sdepos = .true.
      sddep  = .false.
      swdep  = .false.
   else if (outtyp(ityp) .eq. 'DDEP') then
      sconc  = .false.
      sdepos = .false.
      sddep  = .true.
      swdep  = .false.
   else if (outtyp(ityp) .eq. 'WDEP') then
      sconc  = .false.
      sdepos = .false.
      sddep  = .false.
      swdep  = .true.
   else   ! this condition should never happen, set all F
      sconc  = .false.
      sdepos = .false.
      sddep  = .false.
      swdep  = .false.
      return
   end if

   pout = 0.0d0

!---- Calculate the exponential decay term, D               ---   CALL DECAY
   Call decay (xarg)

   if (sconc) then
!----    Get Concentration or Deposition due to horizontal plume, CWRAP
      if (fopt .eq. 0.0d0) then
         cwrap = 0.0d0
      else
         call acplume (zrt, fyarg, cwrap)
      end if

!----    Calculate the contribution due to terrain-following plume, CLIFT
      if (zrt .eq. zflag) then
!----       Effective receptor heights are equal, therefore CLIFT = CWRAP
         clift = cwrap
      else if (fopt .eq. 1.0d0) then
         clift = 0.0d0
      else
!           Get Concentration or Deposition due to LIFT algorithm
         call acplume (zflag, fyarg, clift)
      end if

!----    Calculate the hourly concentration value
!        Now compute the function
      pout = adj * (fopt * cwrap + (1.0d0 - fopt) * clift) * d

   end if
   if (sdepos .or. sddep) then
!----    Get Concentration or Deposition due to horizontal plume, CWRAP
      if (fopt .eq. 0.0d0) then
         cwrap = 0.0d0
      else
         call acplume (zrt-zflag+zrdep, fyarg, cwrap)
      end if

!----    Calculate the contribution due to terrain-following plume, CLIFT
      if (zrt .eq. zflag) then
!----       Effective receptor heights are equal, therefore CLIFT = CWRAP
         clift = cwrap
      else if (fopt .eq. 1.0d0) then
         clift = 0.0d0
      else
!           Get Concentration or Deposition due to LIFT algorithm
         call acplume (zrdep, fyarg, clift)
      end if

!----    Calculate the hourly concentration value
!        Now compute the function
      dryflux = adj * (fopt * cwrap + (1.0d0 - fopt) * clift) * d

   end if
   if (sdepos .or. swdep) then
!        Calculate WETFLUX, vertical term for wet deposition
!        Note that the SRT2PI for the integrated vertical term
!        has been removed since it should be divided by SRT2PI.
!        Additional factor of 3600. has been added to denominator
!        to account for conversion from seconds to hours when
!        divided by wind speed below.
      if (prate .gt. 0.0d0) then
         if (npd .eq. 0) then
            wetflux = (adj*fracsat*prate*1.0d6*rgas*ta)/&
            &(zsubp*henry(isrc)*1.0d9*denom*3600.0d0)
         else
            wetflux = 1.0d-3*adj*washout(jin)*prate/&
            &(zsubp*3600.0d0)
         end if
      else
         wetflux = 0.0d0
      end if
   end if
   if (sdepos) then
      if( stable  .or.  (unstab .and. (hs .ge. zi) ) ) then
         pout = vdinp*dryflux + wetflux*fyarg/ueff
      else if (unstab) then
         pout = vdinp*dryflux + wetflux*fyarg/ueffd
      end if
   end if
   if (sddep) then
      pout = vdinp*dryflux
   end if
   if (swdep) then
      if( stable  .or.  (unstab .and. (hs .ge. zi) ) ) then
         pout = wetflux*fyarg/ueff
      else if (unstab) then
         pout = wetflux*fyarg/ueffd
      end if
   end if


!CRFL Call to METDEB was moved here from METEXT on 9/26/94, R.F. Lee.
!CRFL Print meteorological debug output.                   ---   CALL METDEB
   if (meteordbg) call metdeb

   if ( debug ) then
!        Print Out Debugging Information                    ---   CALL DEBOUT
      call debout
   end if

   return
end

subroutine debout
!***********************************************************************
!             DEBOUT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Outputs Debugging Information: Sigmas, Plume Heights,
!                 etc., for Each Calculation
!
!        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!        DATE:    October 8, 1993
!
!        REVISIONS:  Revised emission rate terms:  for CHID & CHIN,
!                    to QTK*(1-PPF), and for CHI3 to QTK*PPF.
!                    Ref:  P.D.F. Model for Dispersion in the
!                    Convective Boundary Layer, J.C. Weil, 6/27/94.
!                    Changed 7/19/94, R.F. Lee.
!
!                    Revised by Bob Paine to improve readability
!                    of debugging output.  Changed 8/18/94, R.F. Lee
!                   & R. Paine.
!
!
!        INPUTS:  Downwind Distance
!                 Crosswind Distance
!                 Plume Height
!                 Stack Top Wind Speed
!                 Lateral Dispersion Parameter
!                 Vertical Dispersion Parameter
!                 Stability Class
!                 Mixing Height
!                 Receptor Height Above Ground
!                 Emission Rate and Units Scaling Factor
!                 Source Parameter Arrays
!
!        OUTPUTS: Debug Outputs
!
!        CALLED FROM:   PCHI, PDEP, AREAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: chid, chin, chi3

!     Variable Initializations
   modnam = 'DEBOUT'

!     Calculate contributions from each "plume"
   if (stable .or. (unstab .and. hs.ge.zi)) then
      chid = hrval(1)
   else if (unstab) then
!CRFL
!CRFL    Revised emission rate terms:  for CHID & CHIN, to QTK*(1-PPF),
!CRFL    and for CHI3 to QTK*PPF.  Ref:  P.D.F. Model for Dispersion in
!CRFL    the Convective Boundary Layer, J.C. Weil, 6/27/94.  Changed
!CRFL    7/19/94, R.F. Lee.
!CRFL
      chid = (qtk*emifac(1) * (1.0d0-ppf) / ueffd) * (fsubyd*fsubzd)
      chin = (qtk*emifac(1) * (1.0d0-ppf) / ueffn) * (fsubyn*fsubzn)
      if (ppf.gt.0.0d0 .and. ueff3.gt.0.0d0) then
         chi3 = (qtk * emifac(1) * ppf / ueff3) * (fsuby3*fsubz3)
      else
         chi3 = 0.0d0
      end if
   end if

!     Write a blank line to separate the groupings
   write ( dbgunt , 101 )

!     Write the debug output for the receptor data
   write ( dbgunt , 320 ) irec, xr, yr, zelev, zhill, zflag,&
   &x,  y, zelev-zs, hcrit, phee, fopt,&
   &d, cwrap*emifac(1), clift*emifac(1),&
   &aerval(1),prmval(1)
!
!     Write header for plume sigma information
!
   write ( dbgunt, 330 )
!
!     Write the data that was used in the plume computations,
!      which is stability-dependent.
!
   if( stable  .or.  (unstab .and. (hs .ge. zi) ) )then
      write ( dbgunt, 400 ) ppf,qtk,he,syamb,vsigy,syb,sy,&
      &fsuby,szamb,vsigz,szb,szsurf,sz,fsubz,chid

   elseif ( unstab ) then
      if((1.0d0 - ppf) * qtk .gt. 0.0d0) then
         write ( dbgunt, 410 ) ppf,(1.0d0-ppf)*qtk,hed1,syamb,&
         &vsigy,syb,sy,szad1,vszd1,szbd,szsurf,szd1
         write ( dbgunt, 420 ) (1.0d0-ppf)*qtk,hed2,syamb,vsigy,&
         &syb,sy,fsubyd,szad2,vszd2,szbd,szsurf,szd2,fsubzd,chid
!CRFL
!CRFL  SZSURF has been added to the indirect plume sigma z calculations--
!CRFL  add it also to the debug output for the indirect plume.
!CRFL  Changed 9/12/94.  R.F. Lee.  (Format statements 430 and 440 were
!CRFL  changed also.)
         write ( dbgunt, 430 ) ppf,(1.0d0-ppf)*qtk,hen1,syamb,&
         &vsigy,syb,sy,szan1,vszn1,szbn,szsurf,szn1
         write ( dbgunt, 440 ) (1.0d0-ppf)*qtk,hen2,syamb,&
         &vsigy,syb,sy,fsubyn,szan2,vszn2,szbn,szsurf,szn2,fsubzn,&
         &chin
      end if
      if ((ppf*qtk) .gt. 0.0d0) then
         write ( dbgunt, 450 ) ppf, ppf*qtk, he3,sya3,vsy3,&
         &syb3,sy3,fsuby3,sza3,vsz3,szb3,sz3,fsubz3,chi3
      end if
   end if
!
!     FORMAT STATEMENTS
!
101 format ( 1x )
320 format('  REC   REC-X    REC-Y    REC-Z    HILLHT  FLAGPL    ',&
   &'DEL-X   DEL-Y  DEL-Z   HCRIT   PHEE  FOPT  DECAY   CWRAP ',&
   &'     CLIFT      AERVAL    PRMVAL',/,&
   &'    #    (M)      (M)      (M)      (M)     (M)       (M)   ',&
   &'  (M)    (M)     (M)                       (UG/M3)    ',&
   &'(UG/M3)    (UG/M3)',//,&
   &i5,f9.1,f10.1,f8.1,f9.1,f8.1,f10.1,f8.1,f7.1,f7.1,f7.3,f6.3,&
   &f7.3,4e11.4,/)
330 format('   PLUME   PART.  SOURCE  PLUME  <----- SIGMA-Y TERMS --',&
   &'--->   GAUSS.     <--------- SIGMA-Z TERMS -------->   GAUSS.',&
   &/,&
   &' COMPONENT PEN.     Q     HEIGHT  AMB.  DOWNW.  BUOY.  TO',&
   &'TAL   HORIZ.      AMB.  DOWNW.  BUOY.  SURF.  TOTAL   VERT.  ',&
   &'       CHI ',/,&
   &'   TYPE    FRAC.   (G/S)   (M)    (M)    (M)     (M)    ',&
   &'(M)    TERM        (M)    (M)     (M)    (M)    (M)    TERM ',&
   &'       (UG/M3)',/)
400 format(' GAUSSIAN ',f6.3,f9.2,f7.1,4f7.1,e11.4,2x,5f7.1,e11.4,&
   &e12.4)
410 format(' DIRECT #1',f6.3,f9.2,f7.1,4f7.1,13x,5f7.1)
420 format(' DIRECT #2',6x,f9.2,f7.1,4f7.1,e11.4,2x,5f7.1,e11.4,&
   &e12.4)
430 format(' INDIRECT1',f6.3,f9.2,f7.1,4f7.1,13x,4f7.1,&
   &f7.1)
440 format(' INDIRECT2',6x,f9.2,f7.1,4f7.1,e11.4,2x,5f7.1,&
   &e11.4,e12.4)
450 format(' PENETRATE',f6.3,f9.2,f7.1,4f7.1,e11.4,2x,3f7.1,7x,&
   &f7.1,e11.4,e12.4,/)
!
   return
end


subroutine penfct
!***********************************************************************
!             PENFCT Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: Calculate the plume penetration factor
!
!   PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
!
!   DATE:    September 30, 1993
!
!   MODIFIED: Modified to add aircraft speed for Aircraft Source Group
!             Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!             04/01/2023
!
!   REVISED: To use VPTGZI = 0.01 for Base Case model. R.Brode, PES - 12/7/94
!
!   INPUTS:  Stability, STABLE/UNSTAB
!            Buoyancy flux, FB
!            Wind speed at release height, UP (computed in METINI)
!            Potential temperature at ZI
!            Potential temperature gradient above ZI, VPTGZI (from
!            AERMET)
!
!   OUTPUTS: Plume penetration factor, PPF
!
!   CALLED FROM:   PCALC
!
!   Assumptions:
!
!   References:   "Plume Penetration of the CBL and Source 3: Source
!                 Strength and Plume Rise", J. Weil, 9/1/93
!                 "A Dispersion Model for the Convective Boundary Layer",
!                 J. Weil, 8/17/93
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   double precision    bvzi2

   modnam = 'PENFCT'

   if( stable ) then
      ppf = 0.0d0

   else if (unstab .and. (hs .ge. zi) )then
      ppf = 1.0d0

   else
!        Compute the square of the Brunt-Vaisala frequency at ZI, BVZI2

      bvzi2 = (g / ptatzi) * vptgzi

!        Compute the value of PsubS, Eq. 26b in the 2nd reference
!**  Added for Aircraft Plume Rise; UNC-IE
      if (aftsrc(isrc) .eq. 'Y') then
         psubs = fb / ((up+vaa)*bvzi2*(zi-hsp)*(zi-hsp)*(zi-hsp))
      else
!**  End Aircraft Plume Rise insert; April 2023
         psubs = fb / ( up * bvzi2 * (zi-hsp)*(zi-hsp)*(zi-hsp) )
      end if                        ! Added for ARISE; UNC-IE

!        Compute the ratio of delta(Hsub_e)/delta(Hsub_h), HEDHH
!        (Eq. 25 in the 2nd ref.
!        NOTE: 17.576 = (2.6)**3 and 0.296296 is (2/3)**3
      hedhh = (17.576d0 * psubs + 0.296296d0) ** third

!        Check the value of HEDHH and compute the plume penetration, P
      if( hedhh .lt. (2.0d0*third) )then
         ppf = 0.0d0

      else if( hedhh .gt. 2.0d0 )then
         ppf = 1.0d0

      else
         ppf = 1.5d0 - (1.0d0 / hedhh)

      end if

   end if

   return
end

subroutine cplume (zarg, cout)
!***********************************************************************
!             CPLUME Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: Calculate the contribution to the concentration due to
!            plume component, either horizontal or terrain-following,
!            depending on the input receptor height, ZARG
!
!   PROGRAMMER: Roger Brode, PES, Inc.
!
!   DATE:    September 30, 1993
!
!   REVISIONS:
!               Make stable plume reflections dependent on the
!               developmental option switch, OPTG1 & OPTG2,
!               R. Brode, PES, 1/6/95
!
!               Remove stable plume reflections off of ZI for
!               Base Case model.  R. Brode, PES - 12/7/94
!
!               Revised emission rates for each plume to QTK*(1.-PPF)
!               for the direct and indirect plumes, and to QTK*PPF
!               for the penetrated plume.  Ref:  P.D.F. Model for
!               Dispersion in the Convective Boundary Layer,
!               J.C. Weil, 6/27/94. Changes made 7/19/94, R.F. Lee.
!
!               Added true centerline concentration calculations.
!               Changes made 7/25/94, R.F. Lee.
!
!   INPUTS:  Stability, STABLE/UNSTAB
!            Fraction of plume vertical flux remaining in the CBL, FOPT
!            Mixing height, ZI
!            Plume heights, HE/HED1/HED2/HEN1/HEN2
!            sigma_Z's: SZ, SZD1, SZD2, SZN1, SZN2, SZ3
!            Receptor height, ZARG
!
!   OUTPUTS: Contribution due to WRAP, CWRAP
!
!   CALLED FROM:   PCHI
!
!   Assumptions:  For receptor height (ZR) above the mixing height (ZI)
!                 for unstable conditions, the direct and indirect plume
!                 impacts are set to zero.
!
!   References:   "A Dispersion Model for the Convective Boundary
!                 Layer", J. Weil, 8/17/93
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: zarg, cout

   modnam = 'CPLUME'

!     Assign receptor height for vertical term calculations
   zr = zarg

   if( stable  .or.  (unstab .and. (hs .ge. zi) ) ) then
!        Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
!        With stable plume reflections and effective Zi
      if (zr .le. hsbl) then
         call vrtsbl (sz, max( 0.0d0, he-hv ), hsbl)
      else
         call vrtsbn (sz, max( 0.0d0, he-hv ))
      end if

!        Calculate the concentration for a stable atmosphere
      cout = (qtk / ueff) * ( fsuby * fsubz )

   elseif( unstab )then
      if (ppf .lt. 1.0d0) then
!           Calculate the vertical term for the direct plume, FSUBZD
         if (zr .le. zi) then
!              Calculation for Receptor below Zi      ---   CALL VRTCBL
            call vrtcbl ( hed1-hv, hed2-hv, szd1, szd2, 1.0d0)
            fsubzd = fsubz
         else
!              Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
            fsubzd = 0.0d0
         end if

!           Calculate the vertical term for the indirect plume, FSUBZN
         if (zr .le. zi) then
!              Calculation for Receptor below Zi      ---   CALL VRTCBL
            call vrtcbl ( hen1-hv, hen2-hv, szn1, szn2, -1.0d0 )
            fsubzn = fsubz
         else
!              Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
            fsubzn = 0.0d0
         end if
      else
         fsubzd = 0.0d0
         fsubzn = 0.0d0

      end if

!        Note that UEFF and UEFF3 can never be zero, since they get
!        set to a minimum value earlier on.

      if( ppf .gt. 0.0d0 )then
!           Calculate the vertical term for the penetrated
!           plume, FSUBZ3                                ---   CALL VRTSBL
         if (zr .le. hpen) then
            call vrtsbl (sz3, max(0.0d0,he3-hv), hpen)
         else
            call vrtsbn (sz3, max(0.0d0,he3-hv))
         end if
         fsubz3 = fsubz
! Added for HIGHLY BUOYANT PLUME (HBP), JAN 2023
! Apply HBP only for identified sources
         if ((hbplume) .and. (hbpsrc(isrc).eq.'Y')) then
            if (ppf .lt. 1.0d0) then
!  ************************************* modified code JAN 2023  --kja
               cout = (qtk * (1.0d0-ppf) / ueffd) * ( fsubyd*fsubzd ) +&
               &(qtk * (1.0d0-ppf) / ueffn) * ( fsubyn*fsubzn ) +&
               &(qtk * ppf*ppfn / ueff3) * ( fsuby3*fsubz3 )
            else
               cout = (qtk * ppf*ppfn / ueff3) * ( fsuby3*fsubz3 )
!  ************************************* modified code end  --kja
            endif
         else ! For DEFAULT plume bouyancy
            if (ppf .lt. 1.0d0) then
               cout = (qtk * (1.0d0-ppf) / ueffd) * ( fsubyd*fsubzd ) +&
               &(qtk * (1.0d0-ppf) / ueffn) * ( fsubyn*fsubzn ) +&
               &(qtk * ppf / ueff3) * ( fsuby3*fsubz3 )

            else
               cout = (qtk * ppf / ueff3) * ( fsuby3*fsubz3 )
            endif
         endif
! End HBP insert
      else
         fsubz3 = 0.0d0
         hpen   = 0.0d0
         cout = (qtk / ueffd) * ( fsubyd * fsubzd ) +&
         &(qtk / ueffn) * ( fsubyn * fsubzn )

      end if

   end if

   return
end

subroutine prm_plume (zarg, ueffph, cout)                   ! ORD (EMM) change
!***********************************************************************
!             PRM_PLUME Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: Calculate the contribution to the concentration due to
!            PRIME downwash component
!
!   PROGRAMMER: Roger Brode, PES, Inc.
!
!   DATE:    July 5, 2001
!
!   INPUTS:  Receptor height, ZARG
!
!   OUTPUTS: Contribution due to PRIME, COUT
!
!   CALLED FROM:   PRM_PCHI
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: zarg, cout
   double precision :: ueffph

   modnam = 'PRM_PLUME'

!     Assign receptor height for vertical term calculations
   zr = zarg

   if (stable) then
      call vrtsbn (sz, he)
   else if (unstab .and. he.le.zi) then
      call vrtsbl (sz, he, zi)
   else
      fsubz = 0.0d0
   end if

! --- Calculate the WRAP term for a stable atmosphere
!      UEFFPH is defined properly in PRM_PCHI (the calling program)
!      for regulatory AERMOD, AWMA_DOWNWASH, and ORD_DOWNWASH
!      COUT = (QTK / US) * ( FSUBY * FSUBZ )                    ! Original AERMOD
   cout = (qtk / ueffph) * ( fsuby * fsubz )
!      IF (AWMADWDBG) THEN
!         WRITE(AWMADWDBUNT,*) '     Contribution due to PRIME:'
!         WRITE(AWMADWDBUNT,*) '     SY=',SY,' SZ=',SZ,
!     &                        ' UEFFPH=',UEFFPH
!         WRITE(AWMADWDBUNT,*) '     FSUBY=',FSUBY,  ', FSUBZ=',FSUBZ
!         WRITE(AWMADWDBUNT,*) '     QTK / UEFFPH)*( FSUBY*FSUBZ )=',COUT
!      END IF

   return
end

subroutine acplume (zarg, fyarg, cout)
!***********************************************************************
!             ACPLUME Module of the AMS/EPA Regulatory Model - AERMOD
!
!   PURPOSE: Calculate the contribution to the concentration due to
!            plume component, either horizontal or terrain-following,
!            for AREA sources
!
!   PROGRAMMER: Roger Brode, PES, Inc.
!
!   DATE:    September 30, 1993
!
!   REVISIONS:
!               Make stable plume reflections dependent on the
!               developmental option switch, OPTG1 & OPTG2,
!               R. Brode, PES, 1/6/95
!
!               Remove stable plume reflections off of ZI for
!               Base Case model.  R. Brode, PES - 12/7/94
!
!               Revised emission rates for each plume to QTK*(1.-PPF)
!               for the direct and indirect plumes, and to QTK*PPF
!               for the penetrated plume.  Ref:  P.D.F. Model for
!               Dispersion in the Convective Boundary Layer,
!               J.C. Weil, 6/27/94. Changes made 7/19/94, R.F. Lee.
!
!               Added true centerline concentration calculations.
!               Changes made 7/25/94, R.F. Lee.
!
!   INPUTS:  Stability, STABLE/UNSTAB
!            Fraction of plume vertical flux remaining in the CBL, FOPT
!            Mixing height, ZI
!            Plume heights, HE/HED1/HED2/HEN1/HEN2
!            sigma_Z's: SZ, SZD1, SZD2, SZN1, SZN2, SZ3
!
!   OUTPUTS: Contribution due to WRAP, CWRAP
!
!   CALLED FROM:   ACHI
!
!   Assumptions:  For receptor height (ZR) above the mixing height (ZI)
!                 for unstable conditions, the direct and indirect plume
!                 impacts are set to zero.
!
!   References:   "A Dispersion Model for the Convective Boundary
!                 Layer", J. Weil, 8/17/93
!
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: zarg, fyarg, cout

   modnam = 'ACPLUME'

!     Assign receptor height for vertical term calculations
   zr = zarg

!     Assign lateral term
   fsuby = fyarg

   if( stable  .or.  (unstab .and. (hs .ge. zi) ) ) then
!        Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
!        With stable plume reflections
      if (zr .le. hsbl) then
         call vrtsbl (sz, max( 0.0d0, he-hv ), hsbl)
      else
         call vrtsbn (sz, max( 0.0d0, he-hv ))
      end if

!        Calculate the WRAP term for a stable atmosphere
      cout = (1.0d0 / ueff) * ( fsuby * fsubz )

   elseif( unstab )then
!        Calculate the vertical term for the direct plume, FSUBZD
      if (zr .le. zi) then
!           Calculation for Receptor below Zi         ---   CALL VRTCBL
         call vrtcbl ( hed1-hv, hed2-hv, szd1, szd2, 1.0d0 )
         fsubzd = fsubz
      else
!           Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
         fsubzd = 0.0d0
      end if

!        Calculate the vertical term for the indirect plume, FSUBZN
      if (zr .le. zi) then
!           Calculation for Receptor below Zi         ---   CALL VRTCBL
         call vrtcbl ( hen1-hv, hen2-hv, szn1, szn2, -1.0d0 )
         fsubzn = fsubz
      else
!           Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
         fsubzn = 0.0d0
      end if

!        Note that UEFF and UEFF3 can never be zero, since they get
!        set to a minimum value earlier on.

      fsubz3 = 0.0d0
      hpen   = 0.0d0
      cout = (1.0d0 / ueffd) * ( fsuby * fsubzd ) +&
      &(1.0d0 / ueffn) * ( fsuby * fsubzn )

   end if

   return
end

subroutine ltopg(lstab)
!-----------------------------------------------------------------------
!                LTOPG Module of AERMOD Model
!
!        PURPOSE:  Converts Monin-Obukhov length to PG stability class
!                  for use with FASTAREA option and buoyant line
!                  processing, based on Golder (1972)
!
!        MODIFIED: The original LTOPG routine in AERMOD was replaced
!                  with the LSTAB function from the CTDMPLUS model, which
!                  more closely matches the PG-class curves in Figure 4
!                  of the Golder (1972) paper.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 02/29/2012
!
! LTOPG is based on the LSTAB function in the CTDMPLUS model:
!
! FUNCTION: LSTAB
!
! PURPOSE: THIS FUNCTION CALCULATES A P-G STABILITY CLASS GIVEN THE
!               MONIN-OBUKHOV LENGTH (L) AND THE SURFACE ROUGHNESS
!               LENGTH (Z0).
!
! ASSUMPTIONS: THE DIVIDING LINES BETWEEN CATEGORIES ARE ASSUMED TO BE
!               LINEAR.
!
! LIMITATIONS: THIS FUNCTION IS ONLY VALID FOR 0.01 <= Z0 <= 0.5(M).
!              HOWEVER, RESULTS ARE EXTENDED TO OTHER VALUES OF Z0 BY
!              USING Z0 = 0.01 IF Z0 < 0.01 M, AND BY USING Z0 = 0.5
!              IF Z0 > 0.5 M.
!
! ARGUMENTS
!  PASSED:
!       EL      REAL    MONIN-OBUKHOV LENGTH (M)
!       ZR0     REAL    SURFACE ROUGHNESS LENGTH (M)
!  RETURNED FUNCTION VALUE:
!       LSTAB   INT     P-G STABILITY CATEGORY 1=A, 2=B, ETC.
!
! CALLING ROUTINES: SEQMOD
!
! EXTERNAL ROUTINES: NONE
!
! INTERNAL FUNCTIONS:
!       XL - EQUATION OF DIVIDING LINE BETWEEN P-G STABILITY CLASSES
!
! INTRINSIC FUNCTIONS: ALOG
!
! REFERENCES:
!       GOLDER, D. (1972): RELATIONS AMONG STABILITY PARAMETERS IN THE
!                       SURFACE LAYER, BOUNDARY-LAYER METEOROLOGY, 3:56.
!
!-----------------------------------------------------------------------
!
!     Variable Declarations
   use main1
   implicit none
! Unused:      CHARACTER MODNAM*12

   double precision :: el, xel, xl, z0, zr0, yy, xm, b

   integer :: lstab

   xl(yy,xm,b)=xm/(dlog(yy)-b)
!
!
   el  = obulen
   zr0 = sfcz0
!
   z0 = zr0
   if(z0 .gt. 0.5d0) z0 = 0.5d0
   if(z0 .lt. 0.01d0) z0 = 0.01d0
   if(el .lt. 0.0d0) then
      xel = -el
      if(xel .le. xl(z0,-70.0d0,4.35d0)) then
!             STABILITY A
         lstab=1
      else if(xel .le. xl(z0,-85.2d0,0.502d0)) then
!             STABILITY B
         lstab=2
      else if(xel .le. xl(z0,-245.0d0,0.050d0)) then
!             STABILITY C
         lstab=3
      else
!             STABILITY D
         lstab=4
      endif
   else
      if(el .ge. xl(z0,-327.0d0,0.627d0)) then
!             STABILITY D
         lstab=4
      else if(el .ge. xl(z0,-70.0d0,0.295d0)) then
!             STABILITY E
         lstab=5
      else
!             STABILITY F
         lstab=6
      endif
   endif
!
   return
end

!----------------------------------------------------------------------
subroutine vdp
!----------------------------------------------------------------------
!
! --- AERMOD     R.W. Brode, PES, Inc.
!
! --- PURPOSE:  Compute particle and gas dry deposition velocities
!               based on ANL report,
!               Wesely, et. al. (2001)
!
! --- MODIFIED: To add lower limit to dq to avoid zero-divide.
!               R. W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
!
! --- MODIFIED: To add SCHMIDT number as global array.
!               R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
!
! --- INPUTS (and other variables):
!
!      DEFINITIONS OF DRY DEPOSITION VARIABLES AND CONSTANTS
!      C0-C6 = coefficients used in computing saturation vapor pressure
!      de = water vapor deficit computed from Ta and ambient RH (kPa)
!      D_suba = diffusivity in air of gas of interest (m**2/s) (User Input)
!      D_sub_b = diffusivity in air of particle (m**2/s)
!      dq = specific humidity deficit (g/kg)
!      Dv = diffusivity of water vapor in air (0.219e-04m**2/s)
!      el = Monin-Obukhov stability length scale (m)
!      EsTa = saturation vapor pressure at the ambient temperature (kPa)
!             (calculated outside source loop and passed through MODULE MAIN1)
!      F =  factor used in specifying LAIr
!      fo = measure of reactivity
!      f1 = factor for the variation of Rs with solar irradiance
!      f2 = factor for the variation of Rs with available soil moisture (global)
!      f3 = factor for the variation of Rs with water vapor deficit
!      f4 = factor for the variation of Rs with temperature
!      QSW = solar irradiance (W/m**2) (Calculated from AERMET outputs)
!      Gr = reference solar irradiance (30 W/m**2 for forests, otherwise
!           100 W/m**2)
!      Gust_Adj = unstable gusty wind adjustment for Rd
!      HENRY = Henry's Law coefficient for gas of interest (Pa*m**3/mol)
!              (User Input)
!      ISEA5 = Wesely season category (1-5) (Based on User Input)
!      iseas = assign Wesely season category by month for the locale of the
!              meteorological data
!      LANUSE = land use category (1-9) (User Input)
!      P = ambient pressure (kPa)  (Provided by AERMET)
!      Po = reference pressure (101.3 kPa)
!      Prate = precipitation total for the current hour (mm) (Provided by AERMET)
!      prec1 = precipitation one hour back
!      prec2 = precipitation two hours back
!      q = ambient specific humidity (g/kg)
!      qsat = specific humidity at saturation (g/kg)
!      rLAI = relative leaf area index factor
!      Ra = aerodynamic resistance (s/m)
!      Rac = gas-phase aerodymanic resistance within the canopy (s/m)
!      Raci = in-canopy aerodynamic resistance appropriate for Ustar=0.3 (s/m)
!      Rb = quasiliminar resistance for bulk surface (s/m)
!      Rc = bulk surface resistance (s/m)
!      Rcl = bulk cuticle resistance to uptake associated with lipid
!            solubility (s/m)
!      Rcli = resistance to uptake by lipids in cuticles for individual
!             leaves (s/m) (User Input)
!      Rcox = cuticle resistance for ozone, wetted (s/m)
!      Rcs = bulk surface resistance for sulfur dioxide, wetted (s/m)
!      Rcut = cuticle resistance (s/m)
!      Restab = table of resistances that vary with land use and season
!               categories only.
!      Rg = ground resistance (s/m)
!      Rgo = ground resistance for ozone, wetted (s/m)
!      Rgs = ground resistance for sulfur dioxide, wetted (s/m)
!      Ri = surface resistance component (from table) (s/m)
!      RH = relative humidity (%)  (Provided by AERMET)
!      rLAI = relative leaf area index
!      Rm = mesophyll resistance (s/m)
!      Rp = resistance component for particles (s/m)
!      Rs = bulk canopy stomatal resistance (s/m)
!pes   Rx = term used to adjust components of cuticular and ground resistances
!           in the event of a hard freeze
!      S = scaling factor used to estimate cuticle resistance by land use category
!      Stab = table of S by land use category
!      Ta = ambient temperature (deg K) (Provided by AERMET)
!      Tcel = ambient temperature in celsius
!      To = reference temperature (273.16 K)
!      ustar  = friction velocity at the meteorological site (Provided by AERMET)
!      uref = wind speed at anemometer height from the meteorological site
!             (Provided by AERMET)
!      Vdepg = gaseous deposition velocity (m/s)
!      Vdep(i) = particle deposition velocity for i-th particle size category (m/s)
!      vd1 = submicron particle deposition velocity (cm/s)
!      vd2 = coarse particle deposition velocity (cm/s)
!      VONKAR = von Karman constant (0.4)
!      vp = vapor pressure (kPa)
!      Wnew = available rootzone water for current hour (mm) (calculated
!             outside source loop and passed through MODULE MAIN1)
!      Wold = available rootzone water for previous hour (mm) (global)
!      Xnu = kinematic viscosity of air (0.1505 x 10-4 m**2/s, before
!            correction for ambient temp. and pressure)
!      Zrdep = reference height (m)

!
! --- OUTPUT:  Deposition velocity for gases, Vdepg (m/s), or
!              Deposition velocity for particles, Vdep(i) (m/s) by
!                 particle size for Method 1 or for single category
!                 for Method 2
!
! --- VDP_TOX called by:  VDP
! --- VDP_TOX calls:      none
!----------------------------------------------------------------------
!
   use main1
   implicit none

! Unused:      DOUBLE PRECISION, PARAMETER :: a1=1.257D0, a2=0.4D0, a3=0.55D0,
!     &                               xmfp=6.5D-6
   integer lanuse, iland_ndx
!     JAT 7/22/21 D065 Tcel not used
!      DOUBLE PRECISION Tcel, Ri, Rcs, Rco, Rx
   double precision Ri, Rcs, Rco, Rx
   double precision Raci, Rgs, Rgo, f, rLAI, Gr
   double precision de, Rp, f1, alfa, f3, f4, ppp, Rcox, Rac
   double precision D_suba, Rb, Rcl, vd1, vd2, Sfact, D_sub_b
   double precision Stab(9), Restab(9,6,5), Dv
   double precision Po, To, Pressure, a, q, dq, qsat, vp
   integer j, k, isea5

   integer :: i, n
   double precision :: ra, t1, st, xinert,&
   &rd(npdmax), rg, rs, rc, gust_adj

!CRT  Initialize variables 12/27/2017
   Sfact = 0.0d0
   rLAI =  0.0d0
   Ri =    0.0d0
   Rgs =   0.0d0
   Rgo =   0.0d0
   Rcs =   0.0d0
   Rco =   0.0d0
   Raci =  0.0d0
   isea5 = 0.0d0
   Gr =    0.0d0
   f =     0.0d0

!
!     Initialize stability factor array by land use category
   data Stab/1.0d-5,6.0d0,5.0d0,7.0d0,3.0d0,4.0d0,1.0d-5,&
   &1.0d-5,3.0d0/

!     Initialize resistance table by land use category and season
!     Split DATA statement into 2 parts to avoid continuation line limit
   data (((Restab(i,j,k),i=1,9),j=1,6),k=1,3)/&
   &1.d07,   60.d0, 120.d0, 100.d0, 200.d0, 150.d0,1.d07,1.d07,80.d0,&
   &1.d07, 2000.d0,2000.d0,2000.d0,2000.d0,2000.d0,1.d07,1.d07,2.5d3,&
   &1.d07, 1000.d0,1000.d0,1000.d0,2000.d0,2000.d0,1.d07,1.d07, 1.d3,&
   &100.d0, 200.d0, 100.d0,2000.d0, 100.d0,1500.d0,0.d0, 0.d0, 300.d0,&
   &400.d0, 150.d0, 350.d0, 300.d0, 500.d0, 450.d0,0.d0,1000.d0, 0.d0,&
   &300.d0, 150.d0, 200.d0, 200.d0, 300.d0, 300.d0,2.d3,400.d0, 1.d3,&
   &1.d07,   1.d07,  1.d07, 350.d0,  1.d07, 700.d0,1.d07,1.d07, 1.d07,&
   &1.d07, 6500.d0,6500.d0,3000.d0,2000.d0,2000.d0,1.d07,1.d07, 6.5d3,&
   &1.d07,  400.d0, 300.d0, 500.d0, 600.d0,1000.d0,1.d07,1.d07,300.d0,&
   &100.d0, 150.d0, 100.d0,1700.d0, 100.d0,1200.d0, 0.d0, 0.d0,200.d0,&
   &400.d0, 200.d0, 350.d0, 300.d0, 500.d0, 450.d0, 0.d0,1.d3,  0.d0,&
   &300.d0, 150.d0, 200.d0, 200.d0, 300.d0, 300.d0,2.0d3,400.d0,8.d2,&
   &1.d07,   1.d07,  1.d07, 500.d0,  1.d07,1000.d0,1.d07, 1.d07,1.d07,&
   &1.d07,   1.d07,9000.d0,6000.d0,2000.d0,2000.d0,1.d07, 1.d07, 9.d3,&
   &1.d07,   1.d07, 400.d0, 600.d0, 800.d0,1600.d0,1.d07, 1.d07,8.d2,&
   &100.d0,   0.d0, 100.d0,1500.d0, 100.d0,1000.d0, 0.d0,  0.d0,1.d2,&
   &400.d0, 150.d0, 350.d0, 300.d0, 500.d0, 450.d0, 0.d0,  0.d0,1.d3,&
   &300.d0, 150.d0, 200.d0, 200.d0, 300.d0, 300.d0,2.d3, 400.d0,1.d3/
   data (((Restab(i,j,k),i=1,9),j=1,6),k=4,5)/&
   &1.d07,   1.d07,  1.d07, 800.d0,  1.d07,1600.d0,1.d07, 1.d07,1.d07,&
   &1.d07,   1.d07,  1.d07, 400.d0,  1.d07, 800.d0,1.d07, 1.d07, 9.d3,&
   &1.d07, 2000.d0,1000.d0, 600.d0,2000.d0,1200.d0,1.d07, 1.d07,8.d2,&
   &100.d0,   0.d0,  10.d0,1500.d0, 100.d0,1000.d0, 0.d0, 0.d0, 50.d0,&
   &100.d0, 100.d0, 100.d0, 100.d0, 200.d0, 200.d0, 0.d0, 1.d3,100.d0,&
   &600.d0,3500.d0,3500.d0,3500.d0, 500.d0, 500.d0,2.d3,400.d0,3.5d3,&
   &1.d07,  100.d0, 120.d0, 100.d0, 200.d0, 150.d0,1.d07, 1.d07,80.d0,&
   &1.d07, 2000.d0,2000.d0,1500.d0,2000.d0,2000.d0,1.d07, 1.d07, 2.d3,&
   &1.d07, 1000.d0, 250.d0, 350.d0, 500.d0, 700.d0,1.d07,1.d07,300.d0,&
   &100.d0,  50.d0,  80.d0,1500.d0, 100.d0,1000.d0, 0.d0, 0.d0,200.d0,&
   &500.d0, 150.d0, 350.d0, 300.d0, 500.d0, 450.d0, 0.d0,1.d3,  0.d0,&
   &300.d0, 150.d0, 200.d0, 200.d0, 300.d0, 300.d0,2.d3, 400.d0, 1.d3/

   Dv = 0.219d-04
   Po = 101.3d0
   To = 273.16d0
!PES  Define alfa based on Eqn. 10
   alfa = 0.1d0


! ... Convert surface pressure and temperature to proper units.
   Pressure = sfcp/10.0d0
! --- Assume 100 kPa if missing
   if (Pressure .lt. 10.0d0) pressure = 100.0d0
!     JAT 7/22/21 D065 Tcel not used
!      Tcel = Ta-To

!PESc ... check to catch errors in temperature input
!PESc     This code is not used since currently no dew point data (Td)
!PESc     provided from AERMET.
!PES      if (Tcel.lt.Td .and. Td.lt.50.) then
!PES       Tcel = Tdry
!PES       Ta = Tcel+273.2
!PES      end if

   if (npd .eq. 0 .and. ldgas .and. .not.luservd) then
! ...    Assign parameters for gas deposition

! ...    Assign land use category for this direction based on user input.
!        Use method consistent with specification of IFVSEC
      iland_ndx = idint (afv*0.10d0 + 0.4999d0)
      if (iland_ndx .eq. 0) iland_ndx = 36
      lanuse = iland_gd(iland_ndx)

! ...    Assign Wesely "seasonal" category (1-5) based on calendar month
      isea5 = iseas_gd(imonth)

! ...    Assign surface roughness, stability factor and resistance terms
      Sfact   = Stab(lanuse)
      Ri      = Restab(lanuse,1,isea5)
      Rcs     = Restab(lanuse,2,isea5)
      Rco     = Restab(lanuse,3,isea5)
      Raci    = Restab(lanuse,4,isea5)
      Rgs     = Restab(lanuse,5,isea5)
      Rgo     = Restab(lanuse,6,isea5)
! ...    Compute rLAI and reference solar irradiance as a function of
!        land use category and season.
      if (isea5.eq.1 .or. isea5.eq.3 .or. isea5.eq.4) then
         f = 1.0d0
      else if (isea5 .eq. 2) then
!           Assign user-supplied value for season 2, default it 0.50
         f = fseas2
      else if (isea5 .eq. 5) then
!           Assign user-supplied value for season 5, default it 0.25
         f = fseas5
      end if
      if (lanuse.eq.4 .or. lanuse.eq.6) then
         rLAI = f
         Gr = 30.0d0
      else
         rLAI = dsqrt(f)
         Gr = 100.0d0
      end if

   else if (npd .eq. 0 .and. ldgas .and. luservd) then
      return

   end if

! ... Use Zrdep of SFCZ0 plus 1.0 meter for deposition option
   Zrdep = sfcz0 + 1.0d0

! ... Check to avoid corruption by bad humidity input data
   if (rh.gt.100.0d0) rh = 100.0d0
   if (rh.lt.5.0d0) rh = 5.0d0

! ... Compute vapor pressure deficit
   de = ((100.0d0-rh)/100.0d0)*EsTa
   if (de.lt.0.0d0) de = 0.0d0

! ... Compute specific humidity at saturation (g/kg)
   qsat = 1.0d03*0.622d0*EsTa/(Pressure-0.378d0*EsTa)

! ... Compute ambient specific humidity (g/kg)
   vp = (rh/100.0d0)*EsTa
   q = 1.0d03*0.622d0*vp/(Pressure-0.378d0*vp)

! ... Compute specific humidity deficit (g/kg)
   dq = qsat-q
! ... For negative or zero humidity deficit, set dq=0.001 to avoid zero-divide
   if (dq.le.0.0d0) dq = 0.001d0

! ... Compute atmospheric resistance Ra

   if (obulen.ge.0.0d0) then
      Ra = (1.0d0/(vonkar*ustar))*(dlog(Zrdep/sfcz0) +&
      &5.0d0*Zrdep/obulen)

   else
!        Ra = (1.0D0/(VONKAR*ustar))*(log(Zrdep/SFCZ0)-
!    1   2.*log(0.5D0*(1.+sqrt(1.0D0-16.0D0*(Zrdep/obulen)))))
! ...   The following is the expanded form of the unstable Ra equation (2c)
      Ra = (1.0d0/(vonkar*ustar))*dlog(((dsqrt(1.0d0-&
      &16.0d0*Zrdep/obulen)-1.0d0)*&
      &(dsqrt(1.0d0-16.0d0*sfcz0/obulen)+1.0d0))/&
      &((dsqrt(1.0d0-16.0d0*Zrdep/obulen)+1.0d0)*(dsqrt(1.0d0-&
      &16.0d0*sfcz0/obulen)-1.0d0)))
   end if

! ... Compute kinematic viscosity of air (m**2/s), with temp and presssure corrections
   Xnu = 0.1505d0*1.0d-4*((Ta/To)**1.772d0)*(Pressure/Po)*&
   &(1.0d0+0.0132d0*(Pressure-Po))

!***
   if (npd .eq. 0 .and. ldgas) then

! ...    Compute gas deposition velocity, vdepg

! ...    compute parameters necessary for bulk stomatal resistance
      f1 = ((qsw/Gr)+0.01d0)/((qsw/Gr)+1.0d0)
      if (f1 .le. 0.01d0) f1 = 0.01d0
      if (f1 .gt. 1.0d0)  f1 = 1.0d0
      if (Ri .eq. 1.0d07) f1 = 0.01d0
!PES     Calculation of Wnew and f2 moved outside the source loop

      f3 = 1.0d0/(1.0d0+alfa*de)
      if (f3.le.0.01d0) f3 = 0.01d0

      f4 = 1.0d0-0.0016d0*(298.2d0-Ta)**2
      if (f4.le.0.01d0) f4 = 0.01d0


! ...    Modify certain resistances if the surface is wetted (high humidity/
!        weak mixing or rain during current or previous two hours)
      ppp = Prate+prec1+prec2

!PES     Adjust Rcs and Rco based on cloud cover from AERMET
! ...    Determine factor "a" as a function of cloud cover
      a = 0.30d0
      if (ncloud.le.2) a = 0.45d0
      if (ncloud.ge.8) a = 0.15d0
!        JAT 12/11/17
!        correct ISEAS to be 4 "winter with snow" to be consistent with science document
!         if (ISEA5 .EQ. 5 .and. IPCODE .gt.18 .and. Ta .LT. To) then
      if (isea5 .eq. 4 .and. ipcode .gt.18 .and. Ta .lt. To) then
! ...       Skip adjustments for wetted surface if surface is snow covered
         Rcox = Rco
         continue
      else if ((ustar.lt.(a/dq).and.((ihour.lt.8).or.(ihour.gt.19)))&
      &.or. ppp.gt.0.0d0) then
         Rcs  = 50.0d0
         Rcox = 0.75d0*Rco
         Rgs  = 50.0d0
         if ((ustar.lt.(a/dq)).and.&
         &((ihour.lt.8).or.(ihour.gt.19)))then
! ...          Limit Ra for gases if surface is wetted by dew
            if (Ra.lt.1000.0d0) Ra = 1000.0d0
         end if
      else
         Rcox = Rco
      end if

! ...    Calculate Rx term, used to adjust cuticular and ground terms for
!        hard freeze
      Rx = 1.0d03*dexp(-(Ta-269.2d0))

! ...    drive some resistances to high values if there is a hard freeze
      Rcs  = Rcs + Rx
      Rgs  = Rgs + Rx
      Rgo  = Rgo + Rx
      Rcox = Rcox + Rx

! ...    then compute in-canopy aerodynamic resistance, Racx
      Rac = 0.3d0*Raci/ustar

! ...    Assign diffusivity for this source. Note that conversion of
!        diffusivity to m2/s is made in VDP1
      D_suba = pdiff(isrc)

! ...    Compute quasiliminar resistance for bulk surface, Rb

      Rb = 2.2d0*((Xnu/D_suba)**(2.0d0*third))/(vonkar*ustar)

! ...    Compute bulk surface resistance, Rc

! ...    First compute the bulk canopy stomatal resistance, Rs

      Rs = Ri*(Dv/D_suba)/(f1*f2*f3*f4)
      if (Rs.gt.1.0d07) Rs = 1.0d07

! ...    Next compute bulk canopy leaf mesophyll resistance, Rm
! ...    The fo factor applies to ozone (fo=1.0) and nitrogen oxide (fo=0.1).
!        The fo factor should also be set to 1.0 for titanium tetrachloride
!        and divalent mercury, otherwise fo is 0.0

      Rm = 1.0d0/((0.034d0/henry(isrc))+100.0d0*fo)
      if (Rm.gt.1.0d07) Rm = 1.0d07

! ...    Then compute cuticular resistance, Rcut
!        Note that Rcli is converted from s/cm to s/m in SUB. VDP1

      Rcl = Rcli(isrc)/(rLAI*Sfact)
! ...    Adjust Rcl for hard freeze
      Rcl = Rcl + Rx
      if (Rcl.lt.100.0d0) Rcl = 100.0d0

      Rcut = 1.0d0/((1.0d-3/(henry(isrc)*Rcs))+&
      &((fo+fo*fo/henry(isrc))/Rcox)+(1.0d0/Rcl))

! ...    Next compute ground resistance Rg

      Rg=1.0d0/((1.0d-3/(henry(isrc)*Rgs))+((fo+(0.1d0*fo*fo))/Rgo))
      if (Rg.gt.1.0d07) Rg = 1.0d07

! ...    Finally, combine to compute Rc

      Rc = 1.0d0/((rLAI/(Rs+Rm))+(rLAI/Rcut)+(1.0d0/(Rac+Rg)))

! ...    Add the parallel resistances and take the inverse to compute the
!        deposition velocity for gases, Vdepg.

      Vdepg = 1.0d0/(Ra+Rb+Rc)

!***
   else if (npd .gt. 0) then
! ...    Compute particle deposition velocity, vdep

      if (.not. l_method2(isrc)) then
! ...       Calculate using existing ISCST3 method with modified Rb (Eq. 21)

! ---       Calculate t1 using Xnu, adjusted for ambient temp and pressure
         t1 = ustar*ustar/Xnu
!
! ---       LOOP OVER SIZE INTERVALS
         do i=1,npd
!
            st=tstop(i)*t1
!
! ---          Compute inertial impaction term
            xinert=10.0d0**(-3.0d0/st)
!
! ---          Calculate Schmidt number based on ambient temp and pressure
            D_sub_b = 8.09d-14 * (ta*scf(i)/pdiam(i))
            Schmidt(i) = Xnu/D_sub_b

! ---          Calculate unstable gusty wind adjustment for rd,
!              set factor to 1.0 for DFAULT option
            if (wstar .le. 0.0d0) then
               gust_adj = 1.0d0
            else
               gust_adj = 1.0d0 + 0.24d0*wstar*wstar/(ustar*ustar)
            end if
!
! ---          Compute the deposition layer resistance (s/m)
            rd(i) = 1.0d0 / (gust_adj * ustar *&
            &(Schmidt(i)**(-2.0d0*third) + xinert))
!
! ---          Deposition velocity for this particle size category
            vdep(i) =1.0d0/(ra+rd(i)+ra*rd(i)*vgrav(i))+vgrav(i)+&
            &vdphor

         end do
! ***
         if(debug .or. deposdbg)then
            write(dbgunt,*)
            write(dbgunt,*)'RA (s/m)    = ',ra
            write(dbgunt,*)'RD (s/m)    = ',(rd(n),n=1,npd)
            write(dbgunt,*)'VDEP (m/s)  = ',(vdep(n),n=1,npd)
         end if

      else if (l_method2(isrc)) then
! ...       Calculate deposition velocities for fine (vd1) and
!           coarse (vd2) particles for METHOD 2
         if (obulen .ge. 0.0d0) then
            Rp = 500.0d0/ustar
         else
            Rp = 500.0d0/(ustar*(1.0d0-(300.0d0/obulen)))
         end if

!
! ---       Calculate Schmidt number based on ambient temp and pressure
         D_sub_b = 8.09d-14 * (ta*scf(1)/pdiam(1))
         Schmidt(1) = Xnu/D_sub_b

         vd1 = 1.0d0/(Ra+Rp)
! ...       Assign value of 0.002 m/s for Vgrav for coarse mode of METHOD 2
         vd2 = 0.002d0+(1.0d0/(Ra+Rp+0.002d0*Ra*Rp))

! ...       Combine fine and coarse terms to get total deposition velocity.
!           Note that subscript 1 for vdep is used since NPD = 1 for METHOD 2.
         vdep(1) = finemass(isrc)*vd1 + (1.0d0-finemass(isrc))*vd2
      end if

   end if

! ... Write resistances and deposition velocities to separate files:
!     GDEP.DAT for gas deposition and PDEP.DAT for particle deposition.

   if (debug .or. deposdbg) then
      if (ldgas .and. npd .eq. 0) then
         write(gdepdbg,1001) kurdat, isrc, ra, rb, rc, vdepg
1001     format(1x,i8,1x,i6,4(2x,e12.6))
      end if
      if (ldpart .and. .not. l_method2(isrc)) then
         do i = 1, npd
            write(pdepdbg,1002) kurdat, isrc, i, ra, rd(i), vgrav(i),&
            &vdep(i)
1002        format(1x,i8,1x,i6,2x,i3,'  METHOD_1 ',4(2x,e12.6))
         end do
      else if (ldpart .and. l_method2(isrc)) then
         write(pdepdbg,1003) kurdat, isrc, ra, rp, vgrav(1), vdep(1)
1003     format(1x,i8,1x,i6,4x,'-  METHOD_2 ',4(2x,e12.6))
      end if
   end if

   return
end


!----------------------------------------------------------------------
subroutine scavrat
!----------------------------------------------------------------------
!
! --- AERMOD     R.W. Brode, PES
!
! --- PURPOSE:  Compute the wet SCAVenging RATio for particles, as a
!               function of particle size, and for gases, based on
!               new algorithms developed by Chris Walcek
!
! --- MODIFIED: To add calculation of collision efficiency as a
!               function of particle size and raindrop size for
!               particulate emissions, based on Slinn (1984) and
!               Seinfeld and Pandis (1998).
!               R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
!
! --- INPUTS:
!     Global variables:
!            IPCODE - integer    - Precip. code (00-45)
!             PRATE - real       - Precip. rate (mm/hr)
!                TA - real       - Ambient Temperature (deg K)
!               NPD - integer    - Number of particle size categories
!
! --- OUTPUT:
!     Global variables:
!            WASHOUT- real array - Washout coefficient for particles
!            PSCVRT - real array - Scavenging ratio for particles (1/s)
!            GSCVRT - real       - Scavenging ratio for gases (1/s)
!            ECOLL  - real array - Collision efficiency for particles
!
!     Local variables:
!
! --- SCAVRAT called by:  PCALC, VCALC, ACALC
! --- SCAVRAT calls:      none
!----------------------------------------------------------------------
!
! --- Include common blocks
   use main1
   implicit none

! --- Assign RHOW, density of water (g/m^3), as a parameter = 1.0E6
   double precision, parameter :: rhow = 1.0d6
   integer :: i, n
   double precision :: vfall, rdrop, fsubg, fsubl,&
   &tabs, tres, reynold, stoke, sstar, kappa,&
   &term1, term2, term3

   if(debug .or. deposdbg)then
      write(dbgunt,*)
      write(dbgunt,*)'SUBR. SCAVRAT -- Inputs'
      write(dbgunt,*)'IPCODE               = ',ipcode
      write(dbgunt,*)'PRATE (mm/hr)        = ',prate
      write(dbgunt,*)'TA (deg K)           = ',ta
      write(dbgunt,*)'NPD                  = ',npd
      write(dbgunt,*)
   end if

! --- If no precipitation, no wet removal
   if(prate .eq. 0.0d0) then
      if (npd .gt. 0) then
! ---       Set pscvrt(npd), washout(npd), and ecoll(npd) arrays to 0
         pscvrt = 0.0d0
         washout= 0.0d0
         ecoll  = 0.0d0
      else
         gscvrt = 0.0d0
      end if

   else if (npd .gt. 0) then
!PES --- Apply deposition option based on Wesely, et. al. (2001), with
!PES     with modifications based on Chris Walcek, for particles.
!PES     ZSUBP is calculated in PCALC as the top of the plume or the PBL
!PES     height (ZI), whichever is greater.  The top of the plume is defined
!PES     as plume centerline height plus 2.15 sigma-z, evaluated at a downwind
!PES     distance of 20 kilometers.  Since STABLE hours are modeled as
!PES     unlimited mixing, ZSUBP is simply the top of the plume for those
!PES     hours.

! ---    Calculate the precipitaion fall speed, VFALL (m/s) based on
!        precipitation rate in mm/hr.
      vfall = 3.75d0 * prate**0.111d0

! ---    Calculate rainfall droplet radius, RDROP (cm), based on precipitation
!        rate in mm/hr.
      rdrop = (prate**0.232d0) / 18.11d0

      do i = 1, npd
!           JAT 6/25/19 added from 18081
!           add code to calculate terms for scavenging ratio
!           on the basis of whether Method 1 or Method 2 is used.
!           This change is added because the inputs needed for
!           scavenging ratio calculations are not input by Method 2
!           and some of the assumptions used for the calculations are
!           incorrect for Method 2.  This includes that Vg is calculated
!           for the fine particle for wet deposition but not dry deposition
!           and the coarse mode is ignored for wet deposition for method 2
!           using the current calculations.  Particle density is also
!           needed but not input by Method 2.
!           When using method 2 for dry dep and also calculating wet dep
!           the modification is to calculate washout from Wesley et al. 2001
!           and then solve for Ecoll to calculate the scavenging ratio
         if (.not. l_method2(isrc)) then
! ---       Calculate collision efficiency, ECOLL, as function of particle
!           size and raindrop size based on Slinn (1984) and Seinfeld and
!           Pandis (1998).

! ---       Calculate Reynolds number for raindrop
            reynold = rdrop*0.01d0*vfall/xnu
!           Calculate diffusion term, TERM1
            term1 = (4.0d0/(reynold*schmidt(i)))*(1.0d0+0.4d0*&
            &dsqrt(reynold)*schmidt(i)**third+&
            &0.16d0*dsqrt(reynold*schmidt(i)))

! ---       Calculate ratio of particle diameter and raindrop diameter,
!           KAPPA, with adjustments for units
            kappa = (pdiam(i)*1.0d-6)/(rdrop*0.02d0)
!           Calculate interception term, TERM2
!           The constant term 1.81E-2 is ratio of viscosity or air to water
            term2 = 4.0d0*kappa*(1.81d-2+kappa*&
            &(1.0d0+2.0d0*dsqrt(reynold)))

! ---       Calculate Stokes number for raindrop
            stoke = tstop(i)*(vfall-vgrav(i))/(rdrop*0.01d0)
!           Calculate critical Stokes number
            sstar = (1.2d0+dlog(1.0d0+reynold)/12.0d0)/&
            &(1.0d0+dlog(1.0d0+reynold))
            sstar = min( sstar, stoke )
!           Calculate inertial impaction term, TERM3
            term3 = ((stoke-sstar)/&
            &(stoke-sstar+2.0d0*third))**(1.5d0)
!           Scale TERM3, inertial impaction term,by ratio of water
!           density (1 g/cm**3) to particle density
            term3 = term3 * dsqrt(1.0d0/pdens(i))

            ecoll(i) = min( 1.0d0, term1 + term2 + term3 )

! ---       Calculate washout coefficient from Equation 29; factor of 0.01
!           converts drop radius from cm to m.
            washout(i) = 1.5d0 * (zsubp * ecoll(i)) /&
            &(2.0d0*rdrop*0.01d0)
         else  !NEW METHOD 2 code
!             washout is calculated by 10^5*D (see Wesley et al, 2001)
!             modify to account for fine and coarse fractions
            washout(i)=1.0d5*pdiam(i)*finemass(isrc)+&
            &1.0d5*6.0d0*(1-finemass(isrc))
            ecoll(i)=((2.0d0*rdrop*0.01d0)*washout(i))/(1.5d0*zsubp)
            ecoll(i)=min(1.0d0,ecoll(i))
         endif
         if (prate .gt. 0.0d0) then
! ---          Calculate scavenging rate (1/s); factor of 3.6E4 converts drop
!              radius from cm to mm, and converts hours to seconds.
            pscvrt(i) = 1.5d0 * ecoll(i) * prate /&
            &(2.0d0*rdrop * 3.6d4)
         else
            pscvrt(i) = 0.0d0
         end if
      end do
   else
!PES --- Apply deposition option based on Wesely, et. al. (2001),
!PES     with modifications based on Chris Walcek, for gases.
!PES     ZSUBP is calculated in PCALC as the top of the plume or the PBL
!PES     height (ZI), whichever is greater.  The top of the plume is defined
!PES     as plume centerline height plus 2.15 sigma-z, evaluated at a downwind
!PES     distance of 20 kilometers.

! ---    Calculate the precipitaion fall speed, VFALL (m/s) based on
!        precipitation rate in mm/hr.
      vfall = 3.75d0 * prate**0.111d0

! ---    Calculate rainfall droplet radius, RDROP (cm), based on precipitation
!        rate in mm/hr.
      rdrop = (prate**0.232d0) / 18.11d0

! ---    Calculate liquid content of falling rain, LIQCONT (g/m^3), based on
!        precipitation rate in mm/hr.
      liqcont = (prate**0.889d0) / 13.28d0

! ---    Calculate gas-side diffusion enhancement factor, FSUBG (unitless),
!        based on droplet radius (cm).  Linear approximation based on
!        Figure 13-20 of "Microphysics of Clouds and Precipitation" by
!        Hans Pruppacher and James Klett.
      fsubg = 80.0d0*rdrop + 1.0d0

! ---    Set the liquid-side diffusion enhancement factor, FSUBL (unitless),
!        based on droplet radius.
      if (rdrop .lt. 0.01d0) then
         fsubl = 1.0d0
      else if (rdrop .le. 0.05d0) then
         fsubl = 2.6d0
      else
         fsubl = 20.0d0
      end if

! ---    Calculate the absorption time scale, TABS (s); first calculate
!        term in the denominator.
      denom = (1.0d0 + (liqcont*rgas*ta)/(henry(isrc)*rhow))
      tabs=(rdrop**2*rgas*ta/(henry(isrc)*3.0d0*pdiff(isrc)*&
      &1.0d4*fsubg)+(4.0d0*rdrop*rgas*ta)/(henry(isrc)*3.0d0*&
      &50000.0d0*0.01d0)+(rdrop**2*0.17d0)/(3.0d0*pdiffw(isrc)*&
      &1.0d4*fsubl))/denom

! ---    Calculate the residence time of drops in the plume, TRES (s);
!        ZSUBP is defined in PCALC.
      tres = zsubp/vfall

! ---    Calculate the fraction of saturation, FRACSAT, based on time scales.
      fracsat = min( 1.0d0, tres/tabs )

! ---    Calculate equivalent scavenging rate (1/s)
      gscvrt = (fracsat*rgas*ta*prate)/&
      &(3600.0d0*zsubp*henry(isrc)*1.0d3*denom)

   end if

   if(debug .or. deposdbg)then
      write(dbgunt,*)'SUBR. SCAVRAT -- Results'
      if (npd .eq. 0) then
         write(dbgunt,*)'GSCVRT (1/s)= ',gscvrt
      else if (npd .gt. 0) then
         write(dbgunt,*)'PSCVRT (1/s)= ',(pscvrt(n),n=1,npd)
         write(dbgunt,*)'COLL. EFF.  = ',(ecoll(n),n=1,npd)
         write(dbgunt,*)'WASHOUT COEF= ',(washout(n),n=1,npd)
      end if
      write(dbgunt,*)
   end if

   return
end

subroutine pdep (xarg)
!***********************************************************************
!               PDEP Module of AERMOD Model
!
!        PURPOSE: Calculates Deposition Adjustment Factors from DEPLETE
!
!        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
!
!        DATE:       September 20, 2003
!
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   integer :: i
   double precision :: xarg

!     Variable Initializations
   modnam = 'PDEP'

!     Loop over particle sizes
   do i = 1, npd
      dqcor(i) = 1.0d0
      wqcor(i) = 1.0d0
      if (ddplete) then
!           Determine factor for dry depletion
         vsetl = vgrav(i)
         call deplete(vdep(i),xarg,romberg,dqcor(i))
      end if
      if (wdplete .and. pscvrt(i).gt.0.0d0) then
!           Determine source depletion factor from wet removal
!           Simple Terrain Model
         wqcor(i) = dexp(-pscvrt(i)*xarg/us)
      end if
   end do

   return
end


subroutine pdepg (xarg)
!***********************************************************************
!               PDEPG Module of AERMOD Model
!
!        PURPOSE: Calculates Deposition Adjustment Factors from DEPLETE
!                 for Gases
!
!        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
!
!        DATE:       September 29, 2003
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   double precision :: xarg

!     Variable Initializations
   modnam = 'PDEPG'

!     Initialize source depletion factors to unity.
   dqcorg  = 1.0d0
   wqcorg  = 1.0d0
   if (ddplete) then
!        Determine factor for dry depletion
      call deplete( vdepg, xarg, romberg, dqcorg)
   end if
   if (wdplete .and. gscvrt.gt.0.0d0) then
!        Determine source depletion factor
!        from wet removal (GASES)
!        Simple Terrain Model
      wqcorg = dexp(-gscvrt*xarg/us)
   end if

   return
end

!-----------------------------------------------------------------------
subroutine deplete(vdi, xri, lromb, qcor)
!-----------------------------------------------------------------------
!
! --- DEPLETE Module of AERMOD
!              R.W. Brode, MACTEC/PES
!
! PURPOSE:     Subroutine DEPLETE provides the value of the integral of
!              the vertical distribution function over the travel of the
!              plume from the source to the receptor.  Integration is
!              performed by 2-point gaussian quadrature or Romberg
!              integration method, depending on logical argument, lromb.
!
! ARGUMENTS:
!    PASSED:   vdi      deposition velocity (m/s)              [r]
!              vsi      gravitational settling velocity (m/s)  [r]
!              xri      distance from source to receptor (m)   [r]
!              hmixi    mixing height (m)                      [r]
!              lromb    logical for use of Romberg integration [l]
!
!  RETURNED:   qcor     ratio of depleted emission rate to original  [r]
!
! CALLING ROUTINES:   PDEP, PDEPG
!
! EXTERNAL ROUTINES:  F2INT, QATR2, QG2D2
!-----------------------------------------------------------------------

!     Set up call to QATR2(xl,xu,eps,ndim2,fct,y,ier,num,aux2)
!     Declare parameter to fix the size of the aux2 array
   implicit none

   logical lromb
   double precision :: vdi, xri, qcor, eps, value
   double precision, external :: f2int
   integer num, ier
   integer, parameter :: ndim2=12
   double precision aux2(ndim2)

!     Evaluate integral, Use Romberg if LROMB=.T., otherwise use
!     two-point Gaussian Quadrature:
   if (lromb) then
!        Use ROMBERG Integration
      eps = 0.050d0
      call qatr2(1.0d0,xri,eps,ndim2,f2int,value,ier,num,aux2)
   else
!        Use 2-point Gaussian Quadrature
      call qg2d2(1.0d0,xri,f2int,value)
   end if

   if (vdi*value .gt. 50.0d0) then
!        Potential underflow, limit product to 50.0
      value = 50.0d0/vdi
   else if (vdi*value .lt. -50.0d0) then
!        Potential overflow, limit product to 50.0
      value = -50.0d0/vdi
   end if

   qcor=dexp(-vdi*value)

   return
end

!-----------------------------------------------------------------------
function f2int(xi)
!-----------------------------------------------------------------------
!
! --- F2INT Module of AERMOD
!              R.W. Brode, MACTEC/PES
!
! PURPOSE:     Function is the integrand of integral over the travel
!              distance to obtain the fraction of material removed from
!              the plume. Module MAIN1 is used to pass data that are
!              constant during the integration, so QATR (the integrator)
!              only needs to pass values of distance.
!
!              Modified to add plume rise for
!              Volume/Area Source only for Aircraft Source Group
!              Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA, 04/01/2023
!
! ARGUMENTS:
!    PASSED:  xi        distance from source                         [r]
!
!  RETURNED:  f2int     value of integrand                           [r]
!
! CALLING ROUTINES:   QATR2, QG2D2
!
! EXTERNAL ROUTINES:
!
!-----------------------------------------------------------------------
!
   use main1
   implicit none

   double precision xi, vwrap, vlift, f2int, zetmp, zhtmp

!     Initialize VWRAP and VLIFT
   vwrap = 0.0d0
   vlift = 0.0d0

!     Set initial effective parameters
   ueff  = us
   sveff = svs
   sweff = sws
   tgeff = tgs
   if ( unstab  .and.  (hs .lt. zi) ) then
      ueffd  = us
      sveffd = svs
      sweffd = sws
      ueffn  = us
      sveffn = svs
      sweffn = sws
      ueff3  = us
      sveff3 = svs
      sweff3 = sws
      tgeff3 = tgs
   end if

!     Set temporary receptor elevation and height scale
   zetmp = zelev
   zelev = zs + (zetmp-zs)*xi/xdist

   zhtmp = zhill
   zhill = hs + (zhtmp-hs)*xi/xdist

!     Define plume centroid height (CENTER) for use in
!     inhomogeneity calculations
   call centroid ( xi )

!     Define plume centroid height (CENTER) for use in
!     inhomogeneity calculations
   call centroid ( xi )

   if (srctyp(isrc)(1:5) .eq. 'POINT') then
!        Calculate the plume rise                     ---   CALL DELTAH
      call deltah ( xi )
   end if
!**  Added for Aircraft Plume Rise; UNC-IE
   if (srctyp(isrc) .eq. 'VOLUME' .and.&
   &aftsrc(isrc) .eq. 'Y') then
!        Calculate the plume rise                     ---   CALL ADELTAH
      call adeltah (xi)
   end if

   if (srctyp(isrc) (1:4) .eq. 'AREA' .and.&
   &aftsrc(isrc) .eq. 'Y') then
!        Calculate the plume rise                     ---   CALL ADELTAH
      call adeltah (xi)
   end if
!**  End Aircraft Plume Rise insert; April 2023

!     If the atmosphere is unstable and the stack
!     top is below the mixing height, calculate
!     the CBL PDF coefficients                     ---   CALL PDF
   if( unstab  .and.  (hs .lt. zi) ) then
      call pdf
   end if

!     Determine Effective Plume Height             ---   CALL HEFF
   call heff ( xi )

!     Compute effective parameters using an
!     average through plume layer
   call iblval ( xi )

!     Call PDF & HEFF again for final CBL plume heights
   if (unstab .and. (hs.lt.zi) ) then
      call pdf
      call heff ( xi )
   end if

!     Determine Dispersion Parameters              ---   CALL PDIS
   call pdis ( xi )

!     Calculate Plume Tilt Due to Settling, HV
   hv = (xi/us) * vsetl

   if (stable .or. (unstab.and.(hs.ge.zi))) then
!        Calculate height of the "effective reflecting surface"
!        Calculate Settled Plume Height(s), HESETL
      hesetl = max( 0.0d0, he - hv )
      call refl_ht (hesetl, xi, szb, 0.0d0, hsbl)
   else if ( unstab ) then
      hesetl = max( 0.0d0, 0.5d0*(hed1+hed2) - hv )
      hsbl = 0.0d0
   end if

   if (unstab .and. (hs.lt.zi) .and. (ppf.gt.0.0d0)) then
!        Calculate height of the "effective reflecting surface"
!        Calculate Settled Plume Height(s), HE3SETL
      he3setl = max( 0.0d0, he3 - hv )
      call refl_ht (he3setl, xi, szb3, 0.0d0, hpen)
      hpen = max( hpen, zi )
   else
      hpen = 0.0d0
   end if

!     Determine the CRITical Dividing Streamline---   CALL CRITDS
   call critds (hesetl)

!     Calculate the fraction of plume below
!     HCRIT, PHEE                               ---   CALL PFRACT
   call pfract (hesetl)

!     Calculate FOPT = f(PHEE)                  ---   CALL FTERM
   call fterm

   if (fopt .eq. 0.0d0) then
      vwrap = 0.0d0
   else
!        Assign receptor height for vertical term calculations
      zr = zrt + zrdep

      if( stable  .or.  (unstab .and. (hs .ge. zi) ) ) then
!           Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
!           With stable plume reflections and effective Zi
         if (zr .le. hsbl) then
            call vrtsbl (sz, max( 0.0d0, he-hv ), hsbl)
         else
            call vrtsbn (sz, max( 0.0d0, he-hv ))
         end if

!           Calculate value of integral, VWRAP
         vwrap = fsubz/ueff

      else if( unstab )then
         if (ppf .lt. 1.0d0) then
!              Calculate the vertical term for the direct plume, FSUBZD
            if (zr .le. zi) then
!                 Calculation for Receptor below Zi      ---   CALL VRTCBL
               call vrtcbl ( hed1-hv, hed2-hv, szd1, szd2, 1.0d0 )
               fsubzd = fsubz
            else
!                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
               fsubzd = 0.0d0
            end if

!              Calculate the vertical term for the indirect plume, FSUBZN
            if (zr .le. zi) then
!                 Calculation for Receptor below Zi      ---   CALL VRTCBL
               call vrtcbl ( hen1-hv, hen2-hv, szn1, szn2, -1.0d0 )
               fsubzn = fsubz
            else
!                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
               fsubzn = 0.0d0
            end if
         else
            fsubzd = 0.0d0
            fsubzn = 0.0d0

         end if

!           Note that UEFF and UEFF3 can never be zero, since they get
!           set to a minimum value earlier on.

         if( ppf .gt. 0.0d0 )then
!              Calculate the vertical term for the penetrated
!              plume, FSUBZ3                                ---   CALL VRTSBL
            if (zr .le. hpen) then
               call vrtsbl (sz3, max(0.0d0,he3-hv), hpen)
            else
               call vrtsbn (sz3, max(0.0d0,he3-hv))
            end if
            fsubz3 = fsubz

!              Calculate value of integral, VWRAP
            if (ppf .lt. 1.0d0) then
               vwrap = (1.0d0-ppf)*fsubzd/ueffd +&
               &(1.0d0-ppf)*fsubzn/ueffn +&
               &ppf*fsubz3/ueff3
            else
               vwrap = ppf*fsubz3/ueff3
            end if

         else
            fsubz3 = 0.0d0
            hpen   = 0.0d0

!              Calculate value of integral, VWRAP
            vwrap = fsubzd/ueffd +&
            &fsubzn/ueffn

         end if

      end if
   end if

!---- Calculate the contribution due to terrain-following plume, VLIFT
   if (zrt .eq. 0.0d0) then
!----    Effective receptor heights are equal, therefore VLIFT = VWRAP
      vlift = vwrap
   else if (fopt .eq. 1.0d0) then
      vlift = 0.0d0
   else
!        Assign receptor height for vertical term calculations
      zr = zrdep

      if( stable  .or.  (unstab .and. (hs .ge. zi) ) ) then
!           Calculate the vertical term, FSUBZ              ---   CALL VRTSBL
!           With stable plume reflections and effective Zi
         if (zr .le. hsbl) then
            call vrtsbl (sz, max( 0.0d0, he-hv ), hsbl)
         else
            call vrtsbn (sz, max( 0.0d0, he-hv ))
         end if

!           Calculate value of integral, VLIFT
         vlift = fsubz/ueff

      else if( unstab )then
         if (ppf .lt. 1.0d0) then
!              Calculate the vertical term for the direct plume, FSUBZD
            if (zr .le. zi) then
!                 Calculation for Receptor below Zi      ---   CALL VRTCBL
               call vrtcbl ( hed1-hv, hed2-hv, szd1, szd2, 1.0d0 )
               fsubzd = fsubz
            else
!                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
               fsubzd = 0.0d0
            end if

!              Calculate the vertical term for the indirect plume, FSUBZN
            if (zr .le. zi) then
!                 Calculation for Receptor below Zi      ---   CALL VRTCBL
               call vrtcbl ( hen1-hv, hen2-hv, szn1, szn2, -1.0d0 )
               fsubzn = fsubz
            else
!                 Set FSUBZ = 0.0 for "receptor height" (ZR) > ZI
               fsubzn = 0.0d0
            end if
         else
            fsubzd = 0.0d0
            fsubzn = 0.0d0

         end if

!           Note that UEFF and UEFF3 can never be zero, since they get
!           set to a minimum value earlier on.

         if( ppf .gt. 0.0d0 )then
!              Calculate the vertical term for the penetrated
!              plume, FSUBZ3                                ---   CALL VRTSBL
            if (zr .le. hpen) then
               call vrtsbl (sz3, max(0.0d0,he3-hv), hpen)
            else
               call vrtsbn (sz3, max(0.0d0,he3-hv))
            end if
            fsubz3 = fsubz

!              Calculate value of integral, VLIFT
            if (ppf .lt. 1.0d0) then
               vlift = (1.0d0-ppf)*fsubzd/ueffd +&
               &(1.0d0-ppf)*fsubzn/ueffn +&
               &ppf*fsubz3/ueff3
            else
               vlift = ppf*fsubz3/ueff3
            end if

         else
            fsubz3 = 0.0d0
            hpen   = 0.0d0

!              Calculate value of integral, VLIFT
            vlift = fsubzd/ueffd +&
            &fsubzn/ueffn

         end if

      end if
   end if

!     Blend horizontal and terrain-responding components of integral
   f2int = fopt * vwrap + (1.0d0 - fopt) * vlift

!     Reassign receptor elevation and height scales
   zelev = zetmp
   zhill = zhtmp

   return
end

subroutine prm_pdep (xarg)
!***********************************************************************
!               PRM_PDEP Module of AERMOD Model
!
!        PURPOSE: Calculates Deposition Adjustment Factors from
!                 PRM_DEPLETE for PRIME component
!
!        PROGRAMMER: R. W. Brode, PES, Inc.
!
!        DATE:       September 29, 1994
!
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   integer :: i
   double precision :: xarg

!     Variable Initializations
   modnam = 'PRM_PDEP'

!     Loop over particle sizes
   do i = 1, npd
      dqcor(i) = 1.0d0
      wqcor(i) = 1.0d0
      if (ddplete) then
!           Determine factor for dry depletion
         vsetl = vgrav(i)
         call prm_deplete(vdep(i),xarg,romberg,dqcor(i))
      end if
      if (wdplete .and. pscvrt(i).gt.0.0d0) then
!           Determine source depletion factor from wet removal
!           Simple Terrain Model
         wqcor(i) = dexp(-pscvrt(i)*xarg/us)
      end if
   end do

   return
end


subroutine prm_pdepg (xarg)
!***********************************************************************
!               PRM_PDEPG Module of AERMOD Model
!
!        PURPOSE: Calculates Deposition Adjustment Factors from
!                 PRM_DEPLETE for PRIME component for gases
!
!        PROGRAMMER: R. W. Brode, MACTEC/PES, Inc.
!
!        DATE:       September 29, 2003
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   double precision :: xarg

!     Variable Initializations
   modnam = 'PRM_PDEPG'

!     Initialize source depletion factors to unity.
   dqcorg  = 1.0d0
   wqcorg  = 1.0d0
   if (ddplete) then
!        Determine factor for dry depletion
      call prm_deplete( vdepg, xarg, romberg, dqcorg)
   end if
   if (wdplete .and. gscvrt.gt.0.0d0) then
!        Determine source depletion factor
!        from wet removal (GASES)
!        Simple Terrain Model
      wqcorg = dexp(-gscvrt*xarg/us)
   end if

   return
end

!-----------------------------------------------------------------------
subroutine PRM_deplete(vdi, xri, lromb, qcor)
!-----------------------------------------------------------------------
!
! --- PRM_DEPLETE Module of AERMOD
!
!
! PURPOSE:     Subroutine PRM_DEPLETE provides the value of the integral of
!              the vertical distribution function over the travel of the
!              plume from the source to the receptor for the PRIME downwash
!              component.  Integration is performed by 2-point gaussian
!              quadrature or Romberg integration method, depending on
!              logical argument, lromb.
!
! ARGUMENTS:
!    PASSED:   vdi      deposition velocity (m/s)              [r]
!              vsi      gravitational settling velocity (m/s)  [r]
!              xri      distance from source to receptor (m)   [r]
!              hmixi    mixing height (m)                      [r]
!              lromb    logical for use of Romberg integration [l]
!
!  RETURNED:   qcor     ratio of depleted emission rate to original  [r]
!
! CALLING ROUTINES:   PRM_PDEP, PRM_PDEPG
!
! EXTERNAL ROUTINES:  PRM_F2INT, QATR2, QG2D2
!-----------------------------------------------------------------------

!     Set up call to QATR2(xl,xu,eps,ndim2,fct,y,ier,num,aux2)
!     Declare parameter to fix the size of the aux2 array
   implicit none

   logical lromb
   double precision :: vdi, xri, qcor, eps, value
   double precision, external :: prm_f2int
   integer num, ier
   integer, parameter :: ndim2=12
   double precision aux2(ndim2)

!     Evaluate integral, Use Romberg if LROMB=.T., otherwise use
!     two-point Gaussian Quadrature:
   if (lromb) then
!        Use ROMBERG Integration
      eps = 0.05d0
      call qatr2(1.0d0,xri,eps,ndim2,prm_f2int,value,ier,num,aux2)
   else
!        Use 2-point Gaussian Quadrature
      call qg2d2(1.0d0,xri,prm_f2int,value)
   end if

   if (vdi*value .gt. 50.0d00) then
!        Potential underflow, limit product to 50.0
      value = 50.0d0/vdi
   else if (vdi*value .lt. -50.0d0) then
!        Potential overflow, limit product to 50.0
      value = -50.0d0/vdi
   end if

   qcor=dexp(-vdi*value)

   return
end

!-----------------------------------------------------------------------
function PRM_f2int(xi)
!-----------------------------------------------------------------------
!
! --- PRM_F2INT Module of AERMOD
!              R.W. Brode, MACTEC/PES
!
! PURPOSE:     Function is the integrand of integral over the travel
!              distance to obtain the fraction of material removed from
!              the plume for the PRIME downwash component. Module MAIN1
!              is used to pass data that are constant during the
!              integration, so QATR (the integrator) only needs to
!              pass values of distance.
!
! ARGUMENTS:
!    PASSED:  xi         distance from source                        [r]
!
!  RETURNED:  PRM_f2int  value of integrand                          [r]
!
! CALLING ROUTINES:   QATR2, QG2D2
!
! EXTERNAL ROUTINES:
!
!-----------------------------------------------------------------------
   use main1
   implicit none

   double precision :: xi, vwrap, vlift, prm_f2int, zetmp, zhtmp
   double precision :: dhpout, syout, szout, sycav, szcav
   logical l_inwake

!     Set temporary receptor elevation and height scale
   zetmp = zelev
   zelev = zs + (zetmp-zs)*xi/x

   zhtmp = zhill
   zhill = hs + (zhtmp-hs)*xi/x

!     Calculate the plume rise                     ---   CALL PRMDELH
   call prmdelh ( xi, l_inwake )

!     Determine Effective Plume Height             ---   CALL PRMHEFF
   call prmheff

! --- Calculate sigmas
   dhpout = dhp
   call wake_xsig(xi,hs,dhpout,nobid,szout,syout,&
   &szcav,sycav)
   sy = syout
   sz = szout

!     Calculate Plume Tilt Due to Settling, HV
   hv = (xi/us) * vsetl
   he = max( 0.0d0, he - hv )

!     Calculate FOPT = f(PHEE)                  ---   CALL FTERM
   fopt = 0.5d0

!     Assign receptor height for vertical term calculations
   zr = zrt + zrdep

   if (stable) then
      call vrtsbn (sz, he)
   else if (unstab .and. he.le.zi) then
      call vrtsbl (sz, he, zi)
   else
      fsubz = 0.0d0
   end if

   vwrap = fsubz/ueff

!---- Calculate the contribution due to terrain-following plume, VLIFT
   if (zrt .eq. 0.0d0) then
!----    Effective receptor heights are equal, therefore VLIFT = VWRAP
      vlift = vwrap
   else if (fopt .eq. 1.0d0) then
      vlift = 0.0d0
   else
!        Assign receptor height for vertical term calculations
      zr = zrdep
      if (stable) then
         call vrtsbn (sz, he)
      else if (unstab .and. he.le.zi) then
         call vrtsbl (sz, he, zi)
      else
         fsubz = 0.0d0
      end if
      vlift = fsubz/ueff
   end if

!     Blend horizontal and terrain-responding components of integral
   prm_f2int = fopt * vwrap + (1.0d0 - fopt) * vlift

!     Reassign receptor elevation and height scales
   zelev = zetmp
   zhill = zhtmp

   return
end

!-----------------------------------------------------------------------
subroutine qatr2(xl,xu,eps,ndim,fct,y,ier,i,aux)
!-----------------------------------------------------------------------
!
! --- AERMOD    QATR2
!
! PURPOSE:      Integration routine adapted from the IBM SSP program
!               DQATR.  Modified for single precision.  This is a COPY
!               of QATR for use in double integrations.
!
! MODIFIED:     To use new convergence criteria, including a lower
!               threshold in the value of the integral (1.0E-10), and
!               to check for "delta-x" < 1.0 meters (delta-x = hh).
!               R. W. Brode, PES, Inc. - 9/30/94
!
! ARGUMENTS:
!    PASSED:    xl,xu   lower and upper limits of integration        [r]
!               eps     fractional error used to define convergence  [r]
!               ndim    dimension of array aux (parameter)           [p]
!               fct     external function (integrand)
!               aux     working array, passed to allow variable dim. [r]
!  RETURNED:    y       value of integral                            [r]
!               ier     status flag at terminatio                    [i]
!               i       number of subdivision steps                  [i]
!
! CALLING ROUTINES:     DEPLETE
!
! EXTERNAL ROUTINES:    none
!-----------------------------------------------------------------------

!  NOTES: status flags denote the following --
!               ier=0   value of integral converged to within eps
!               ier=1   value of integral is diverging
!               ier=2   value of integral did not converge to within
!                       eps before ndim limit was reached

!  NDIM Note:  The aux(ndim) array keeps track of the average value of
!              the integrand for each of the steps in subdividing the
!              interval.  For example, when i=4 in the "do 7 i=2,ndim"
!              loop, aux(4) contains the mean value as obtained from
!              the trapezoidal rule, while aux(1 through 3) contain
!              a set of current Romberg extrapolations.  At each new
!              value of i, the interval is subdivided again, and the
!              integrand is evaluated at jj=2**(i-2) new points.
!              Therefore, at i=5, there will be jj=8 new points added
!              to the 9 points already used in the interval.  When i=17
!              there will be jj=32,768 new points added to the 32,769
!              already used.  This is the maximum number of new points
!              that are allowed as jj is an INTEGER*2 variable, with
!              a maximum value of 2**15.  Therefore, i should not exceed
!              17, and probably should be no larger than 16.  This means
!              that NDIM should be set at 16.  Larger values of NDIM
!              could be accepted if the INTEGER*2 variables were changed
!              to INTEGER*4, but for most applications, 30000 to 60000
!              points ought to be sufficient for evaluating an integral.

   implicit none

   integer :: ndim, ier
   double precision :: y, eps, xu, xl, aux(ndim), half, fct, h, hh,&
!     JAT 7/22/21 D065, DELT1 NOT USED
!     &                    DELT2, P, DELT1, HD, X, SM, Q
   &delt2, p, hd, x, sm, q
   external fct
   integer i,ii,ji,j,jj
   half=0.5d0

!     Preparations for Romberg loop
   aux(1)=half*(fct(xl)+fct(xu))
   h=xu-xl

   if(h .eq. 0.0d0 .or. aux(1) .eq. 0.0d0) then
      ier=0
      y = 0.0d0
      return
   end if

   hh=h
   delt2=0.0d0
   p=1.0d0
   jj=1

   do i=2,ndim
      y=aux(1)
!     JAT 7/22/21 D065, DELT1 NOT USED
!        delt1=delt2
      hd=hh
      hh=half*hh
      p=half*p
      x=xl+hh
      sm=0.0d0

      do j=1,jj
         sm=sm+fct(x)
         x=x+hd
      end do

!  A new approximation to the integral is computed by means
!  of the trapezoidal rule
      aux(i)=half*aux(i-1)+p*sm

!  Start of Rombergs extrapolation method

      q=1.0d0
      ji=i-1
      do j=1,ji
         ii=i-j
         q=q+q
         q=q+q
         aux(ii)=aux(ii+1)+(aux(ii+1)-aux(ii))/(q-1.0d0)
      end do

!  End of Romberg step

      delt2=dabs(y-aux(1))

      if (i .ge. 3) then
!  Modification for cases in which function = 0 over interval
!rwb        add lower threshold convergence test
         if (aux(1) .lt. 1.0d-10) then
            ier=0
            y=h*aux(1)
            return
         elseif (delt2 .le. eps*dabs(aux(1)) ) then
            ier=0
            y=h*aux(1)
            return
!rwb        add lower limit on "delta-x" of 1.0m
         elseif (hh .lt. 1.0d0) then
            ier=0
            y=h*aux(1)
            return
!           elseif (delt2 .GE. delt1)then
!              ier=1
!              y=h*y
!              return
         endif
      endif
      jj=jj+jj
   end do

   ier=2
   y=h*aux(1)

   return
end



subroutine qg2d2(xl,xu,fct,y)
!     ..................................................................
!
!        SUBROUTINE QG2D2
!
!        PURPOSE
!           TO COMPUTE INTEGRAL(FCT(X), SUMMED OVER X FROM XL TO XU)
!
!        USAGE
!           CALL QG2 (XL,XU,FCT,Y)
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
!
   implicit none

   double precision :: a, b, y, xl, xu, fct
   external fct

   a = 0.5d0*(xu+xl)
   b = xu-xl
   y = 0.2886751d0*b
   y = 0.5d0*b*(fct(a+y)+fct(a-y))

   return
end

subroutine olm_calc
!***********************************************************************
!             OLM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Hourly Results for OLM Option
!
!        PROGRAMMER: Roger W. Brode, PES, Inc.
!
!        DATE:    May 6, 2002
!
!        MODIFIED: Incorporated equilibrium ratio for OLM option,
!                  with default of 0.90, as with PVMRM option.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 02/28/2011
!
!        MODIFIED: Corrected initialization problem with OLMGROUP
!                  keyword option.  NO2VAL and NO_VAL arrays need
!                  to be reinitialized for each receptor.
!                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12, blnk8*8
   integer :: iastat
! --- Declare allocatable arrays that are needed if OLMGROUPs
!     are used
   double precision, allocatable :: olmval(:,:),&
   &no2val(:), no_val(:),&
   &OLMGrp_PercentNO2(:)
!     JAT 7/22/21 D065, BCKGRD AND DUMVAL NOT USED
   double precision :: no2, no, olmtemp, PercentNO2
!      DOUBLE PRECISION :: NO2, NO, OLMTEMP, PercentNO2, BCKGRD,
!     &                                                  DUMVAL

!     Variable Initializations
   modnam = 'OLM_CALC'
!     JAT 7/22/21 D065, BCKGRD AND DUMVAL NOT USED
!      BCKGRD = 0.0D0
!      DUMVAL = 0.0D0
   hrval(:) = 0.0d0
   iastat = 0
   blnk8  = '        '


! --- Allocate OLMGROUP arrays if needed for OLMGROUPs
   if( numolm .ge. 1 )then
      if( .not.allocated(olmval) )then
         allocate( olmval(numolm,numtyp),&
         &stat=iastat )
      endif
      if( .not.allocated(no2val) )then
         allocate( no2val(numolm),&
         &stat=iastat )
      endif
      if( .not.allocated(no_val) )then
         allocate( no_val(numolm),&
         &stat=iastat )
      endif
      if( .not.allocated(OLMGrp_PercentNO2) )then
         allocate( OLMGrp_PercentNO2(numolm),&
         &stat=iastat )

      endif
      if (iastat .ne. 0) then
         call errhdl(path,modnam,'E','409','OLMGROUP')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation ',&
         &'Arrays for OLM_CALC!'
         return
      end if
   end if

!     Initialize scalar variables
   iolm = 0
   no2  = 0.0d0
   no   = 0.0d0
   olmtemp = 0.0d0

!     Initialize NO2VAL(NUMOLM) and NO_VAL(NUMOLM) arrays, if allocated
   if (allocated(no2val)) no2val(:) = 0.0d0
   if (allocated(no_val)) no_val(:) = 0.0d0
!     Initialize OLMVAL(NUMOLM,NUMTYP) array, if allocated
   if (allocated(olmval)) olmval(:,:) = 0.0d0
   if (allocated(OLMGrp_PercentNO2)) OLMGrp_PercentNO2(:) = 0.0d0

! --- Begin Receptor LOOP
   receptor_loop: do irec = 1, numrec
!        Reinitialize the scalar variables
      no2 = 0.0d0
      no  = 0.0d0
      olmtemp = 0.0d0
      PercentNO2 = 0.0d0
!        Reinitialize the NO2VAL, NO_VAL, and OLMVAL arrays
      if (allocated(no2val)) no2val(:) = 0.0d0
      if (allocated(no_val)) no_val(:) = 0.0d0
      if (allocated(olmval)) olmval(:,:) = 0.0d0
      if (allocated(OLMGrp_PercentNO2)) OLMGrp_PercentNO2(:) = 0.0d0

! ---    First determine whether OLMGROUP(s) are being used;
!        if OLMGROUPs then first calculate the OLMGROUP-specific
!        PercentNO2 value(s) for all OLMGROUP(s)
      if (numolm .ge. 1) then
! ---       Loop through all OLMGROUPs to determine OLMGrp_PercentNO2 for each OLMGRP
         olmgrp_loop: do iolm = 1, numolm
!              Loop through SOURCEs to determine NO2VAL, NO_VAL, and OLMVAL for each
!              OLMGRP
            source_loop: do isrc = 1, numsrc
               if (l_olmgrp(isrc)) then
                  if (igrp_olm(isrc,iolm) .eq. 1) then

! ---                Source is included in this OLMGRoup;
!                    Calculate NO2 and NO CONV Values for OLMGROUP
                     no2val(iolm) = no2val(iolm) +&
                     &ano2_ratio(isrc)* chi(irec,isrc,1)
                     no_val(iolm) = no_val(iolm) +&
                     &(1.0d0-ano2_ratio(isrc))*chi(irec,isrc,1)*&
                     &(30.0d0/46.0d0)
                     do ityp = 1, numtyp
!                       Calculate Hourly Values (Full Conversion) for OLMGROUP
                        olmval(iolm,ityp) = olmval(iolm,ityp) +&
                        &chi(irec,isrc,ityp)
                     end do
                  end if
               end if
            end do source_loop
!              Determine if combined plume is O3 limited, and assign
!              PercentNO2 to OLMGroup for CONC (ITYP=1); Apply full
!              conversion if O3MISS
            if (.not. o3miss .and. (o3conc/48.0d0) .lt.&
            &((no_val(iolm))/30.0d0)) then
! ---             .NOT. O3MISS and NO2 conversion is O3-limited;
!                 compute O3-limited NO2 concentration (OLMTEMP)
!                 and then compute PercentNO2 conversion for this
!                 OLMGroup
               olmtemp  = no2val(iolm)+(o3conc*(46.0d0/48.0d0))
               if (olmval(iolm,1) .gt. 0.0d0) then
                  OLMGrp_PercentNO2(iolm) = olmtemp/olmval(iolm,1)
               else
                  OLMGrp_PercentNO2(iolm) = NO2Equil
               end if
            else
! ---             O3MISS or not O3-limited; apply full conversion
!                 (subject later to equilibrium ratio)
               OLMGrp_PercentNO2(iolm) = 1.0d0
            end if

            OLMGrp_PercentNO2(iolm) = min( OLMGrp_PercentNO2(iolm),&
            &1.0d0 )
!              Limit to equilibrium concentration of NO2 (default set at 90 percent)
            if (OLMGrp_PercentNO2(iolm) .gt. NO2Equil) then
               OLMGrp_PercentNO2(iolm) = NO2Equil
            end if
         end do olmgrp_loop
      end if

      if( .not.evonly )then
! ---      Begin processing for NON-EVENT application

! ---      Begin Source Group LOOP to sum values
         group_loop: do igrp = 1, numgrp

! ---      Loop through all SOURCEs again to apply OLMGrp_PercentNO2
!          as appropriate and sum values for this source group
            source_loop2: do isrc = 1, numsrc

               if( igroup(isrc,igrp) .ne. 1 )then
! ---          This source is not included in the current SRCGROUP
                  cycle source_loop2
               endif

               if( l_olmgrp(isrc) )then
! ---          This source is included in the current SRCGROUP and is also
!              included in an OLMGROUP; apply appropriate OLMGrp_PercentNO2
                  olmgrp_loop2: do iolm = 1, numolm
                     if( igrp_olm(isrc,iolm) .eq. 1 )then
!                    Apply equivalent PercentNO2 to all ITYPs (CONC, DDEP, WDEP or DEPOS)
                        do ityp = 1, numtyp
                           hrval(ityp) = OLMGrp_PercentNO2(iolm) *&
                           &chi(irec,isrc,ityp)
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between OLM and TTRM
                           if (runttrm2) then
                              if (ttrm2dbg .and. (cmeth .lt. 3)) then
                                 write(ttrm2tmp(2),pstfrm,err=85) axr(irec),&
                                 &ayr(irec), hrval(1), azelev(irec),&
                                 &azhill(irec), azflag(irec), chrave(1),&
                                 &grpid(igrp), kurdat, srcid(isrc)
                                 go to 885
85                               write(dummy,'("PSTFL",I4.4)') ttrm2tmp(2)
                                 call errhdl(path,modnam,'E','520',dummy)
                                 runerr = .true.
885                              continue
                              endif
                              if (ttrmcompare(igrp,isrc,irec,ityp) .lt. hrval(ityp)) then
                                 hrval(ityp) = ttrmcompare(igrp,isrc,irec,ityp)
                              endif
                              if (ttrm2dbg .and. (cmeth .lt. 3)) then
                                 write(ttrm2tmp(3),pstfrm,err=95) axr(irec),&
                                 &ayr(irec), hrval(1), azelev(irec),&
                                 &azhill(irec), azflag(irec), chrave(1),&
                                 &grpid(igrp), kurdat, srcid(isrc)
                                 go to 895
95                               write(dummy,'("PSTFL",I4.4)') ttrm2tmp(3)
                                 call errhdl(path,modnam,'E','520',dummy)
                                 runerr = .true.
895                              continue
                              endif
                           endif
!! End of TTRM2 insert; Nov. 2021
                        end do
! ---                Exit OLMGRP_LOOP2 since a source can only be in one OLMGRP
                        exit olmgrp_loop2
                     endif
                  enddo olmgrp_loop2

! ---          Call SUMVAL to add current source's contribution to AVEVAL
                  call sumval

! ---          Write data to OLM debugging file
                  if (olmdebug) then
                     if (igroup(isrc,igrp) .eq. 1) then
                        write(olmdbg,9988,err=999) kurdat, irec,&
                        &grpid(igrp),  isrc, srcid(isrc), iolm,&
                        &olmid(iolm), o3conc,&
                        &olmval(iolm,1), ano2_ratio(isrc),&
                        &no2val(iolm), no_val(iolm),&
                        &chi(irec,isrc,1),&
                        &OLMGrp_PercentNO2(iolm), hrval(1),&
                        &aveval(irec,igrp,1,1)
                     end if
9988                 format(1x,i8.8,i6,2x,a8,i6,2x,a12,2x,i4,2x,a8,&
                     &9(1x,e12.5:))
                  end if

               else
! ---          Source is in this SRCGROUP, but NOT in an OLMGROUP;
!              apply OLM to individual source
                  no2 = ano2_ratio(isrc) * chi(irec,isrc,1)
                  no  = (1.0d0-ano2_ratio(isrc))*chi(irec,isrc,1)*&
                  &(30.0d0/46.0d0)

! ---          Determine if CONC is O3-limited; if not, then no conversion needed
!              First check for missing O3 data (O3MISS) and apply full conversion
!              (subject to equilibrium ratio)
                  if (.not. o3miss .and. (o3conc/48.0d0) .lt.&
                  &(no/30.0d0)) then
! ---             .NOT. O3MISS and NO2 conversion is O3-limited;
!                 compute O3-limited NO2 concentration (OLMTEMP)
!                 and then compute PercentNO2 conversion for this
!                 OLMGroup
                     hrval(1) = no2 + (o3conc*(46.0d0/48.0d0))
                  else
! ---             HRVAL is NOT O3-limited
                     hrval(1) = chi(irec,isrc,1)
                  end if

                  if (chi(irec,isrc,1) .gt. 0.0d0) then
!                 Calculate an equivalent Percent NO2 for CONC
                     PercentNO2 = hrval(1)/chi(irec,isrc,1)
                  else
                     PercentNO2 = NO2Equil
                  end if

! ---          Ensure the PercentNO2 is not > 1.0
                  PercentNO2 = min( PercentNO2, 1.0d0 )

!              Limit PercentNO2 to equilibrium concentration of NO2 (default=90 percent)
                  if (PercentNO2 .gt. NO2Equil) PercentNO2 = NO2Equil

!              Apply equivalent PercentNO2 to all ITYPs (CONC, DDEP, WDEP or DEPOS)
                  do ityp = 1, numtyp
                     hrval(ityp) = PercentNO2 * chi(irec,isrc,ityp)
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between OLM and TTRM
                     if (runttrm2) then
                        if (ttrm2dbg .and. (cmeth .lt. 3)) then
                           write(ttrm2tmp(2),pstfrm,err=86) axr(irec),&
                           &ayr(irec), hrval(1),  azelev(irec),&
                           &azhill(irec), azflag(irec), chrave(1),&
                           &grpid(igrp), kurdat, srcid(isrc)
                           go to 886
86                         write(dummy,'("PSTFL",I4.4)') ttrm2tmp(2)
                           call errhdl(path,modnam,'E','520',dummy)
                           runerr = .true.
886                        continue
                        endif
                        if (ttrmcompare(igrp,isrc,irec,ityp) .lt. hrval(ityp)) then
                           hrval(ityp) = ttrmcompare(igrp,isrc,irec,ityp)
                        endif
                        if (ttrm2dbg .and. (cmeth .lt. 3)) then
                           write(ttrm2tmp(3),pstfrm,err=96) axr(irec),&
                           &ayr(irec), hrval(1), azelev(irec),&
                           &azhill(irec), azflag(irec), chrave(1),&
                           &grpid(igrp), kurdat, srcid(isrc)
                           go to 896
96                         write(dummy,'("PSTFL",I4.4)') ttrm2tmp(3)
                           call errhdl(path,modnam,'E','520',dummy)
                           runerr = .true.
896                        continue
                        endif
                     endif
!! End of TTRM2 insert; Nov. 2021
                  end do

! ---          Call SUMVAL to add current source's contribution to AVEVAL
                  call sumval

! ---          Write data to OLM debugging file; since this source is
!              NOT in an OLMGROUP, assign IOLM = 0 and use no_val, no2val,
!              olmval, and PercentNO2 scalar variables; also use BLNK8 field
!              instead of OLMID
                  iolm = 0
                  if (olmdebug) then
                     if (igroup(isrc,igrp) .eq. 1) then
                        write(olmdbg,9988,err=9981) kurdat, irec,&
                        &grpid(igrp), isrc, srcid(isrc), iolm,&
                        &blnk8, o3conc, chi(irec,isrc,1),&
                        &ano2_ratio(isrc), no2, no, chi(irec,isrc,1),&
                        &PercentNO2, hrval(1), aveval(irec,igrp,1,1)
                     end if
                  end if

               end if    ! source not in olmgrp

               go to 1234
9981           call errhdl(path,modnam,'E','520','OLMDEBUG')
1234           continue

               if (eval(isrc)) then
!              Check ARC centerline values for EVALFILE
!              output                              ---   CALL EVALCK
                  call evalck
               end if

            end do source_loop2

            if (grp_back(igrp)) then
! ---         Call SUMBACK_NO2 to update BACKGROUND contributions
               call sumback_no2

               if (olmdebug) then
! ---            Include BACKGROUND contribution in OLM debug file
                  write(olmdbg,99872,err=999) kurdat, irec,&
                  &grpid(igrp), 'BACKGROUND',&
                  &bgconc, aveval(irec,igrp,1,1)
99872             format(1x,i8.8,i6,2x,a8,8x,a10,109x,&
                  &2(1x,e12.5))
               end if

            end if

         end do group_loop
! ---     End of .NOT.EVONLY processing

      else if (evonly) then
! ---      Check for inclusion of BACKGROUND in this EVENT and add to DEBUG file
!          as a separate "source" contribution
         isrc = 0
         iolm = 0

! ---      Loop through all SOURCEs again to apply OLMGrp_PercentNO2
!          as appropriate and sum values for this group
         ev_source_loop: do isrc = 1, numsrc

            ityp = 1

            if( igroup(isrc,idxev(ievent)) .ne. 1 )then
! ---          This source is not included in the current SRCGROUP
               cycle ev_source_loop
            endif

            if( l_olmgrp(isrc) )then
! ---         This source is included in an OLMGROUP; apply appropriate OLMGrp_PercentNO2
               ev_olmgrp_loop: do iolm = 1, numolm
                  if( igrp_olm(isrc,iolm) .eq. 1 )then
!                   Apply equivalent PercentNO2 to all ITYPs (CONC, DDEP, WDEP or DEPOS)
                     do ityp = 1, numtyp
                        hrval(ityp) = OLMGrp_PercentNO2(iolm) *&
                        &chi(1,isrc,ityp)
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between OLM and TTRM
                        if (runttrm2) then
                           if (ttrmcompare(igrp,isrc,1,ityp) .lt. hrval(ityp)) then
                              hrval(ityp) = ttrmcompare(igrp,isrc,1,ityp) !! <----- for EVENT, lock receptor
                           endif
                        endif
!! End of TTRM2 insert; Nov. 2021
                     end do
                     exit ev_olmgrp_loop
                  endif
               enddo ev_olmgrp_loop

! ---         Call EV_SUMVAL to add current source's contribution to
!             EV_AVEVAL, GRPAVE, etc.
               call ev_sumval

! ---         Write data to OLM debugging file
               if (olmdebug) then
                  if (igroup(isrc,idxev(ievent)) .eq. 1) then
                     write(olmdbg,9987,err=998) kurdat, ievent,&
                     &evname(ievent), evaper(ievent),&
                     &grpid(idxev(ievent)), isrc, srcid(isrc),&
                     &iolm, olmid(iolm), o3conc,&
                     &olmval(iolm,1), ano2_ratio(isrc),&
                     &no2val(iolm), no_val(iolm),&
                     &chi(1,isrc,1),&
                     &OLMGrp_PercentNO2(iolm), hrval(1),&
                     &grpave(idxev(ievent))
                  end if
9987              format(1x,i8.8,i6,2x,a10,2x,i3,2x,a8,i6,2x,a12,2x,&
                  &i4,2x,a8,1x,9(1x,e12.5))
               end if

            else
! ---          Source is NOT in and OLMGROUP; apply OLM to individual source
               no2 = ano2_ratio(isrc) * chi(1,isrc,1)
               no  = (1.0d0-ano2_ratio(isrc))*chi(1,isrc,1)*&
               &(30.0d0/46.0d0)

! ---          Determine if CONC is O3-limited; if not, then no conversion needed
!              First check for missing O3 data (O3MISS) and apply full conversion
!              (subject to equilibrium ratio)
               if (.not. o3miss .and. (o3conc/48.0d0) .lt.&
               &(no/30.0d0)) then
! ---             .NOT. O3MISS and NO2 conversion is O3-limited;
!                 compute O3-limited NO2 concentration (OLMTEMP)
!                 and then compute PercentNO2 conversion for this
!                 OLMGroup
                  hrval(1) = no2 + (o3conc*(46.0d0/48.0d0))
               else
! ---             HRVAL is NOT O3-limited
                  hrval(1) = chi(1,isrc,1)
               end if

               if (chi(1,isrc,1) .gt. 0.0d0) then
!                 Calculate an equivalent Percent NO2 for CONC
                  PercentNO2 = hrval(1)/chi(1,isrc,1)
               else
                  PercentNO2 = NO2Equil
               end if

! ---          Ensure the PercentNO2 is not > 1.0
               PercentNO2 = min( PercentNO2, 1.0d0 )

!              Limit PercentNO2 to equilibrium concentration of NO2 (default=90 percent)
               if (PercentNO2 .gt. NO2Equil) PercentNO2 = NO2Equil

!              Apply equivalent PercentNO2 to all ITYPs (CONC, DDEP, WDEP or DEPOS)
               do ityp = 1, numtyp
                  hrval(ityp) = PercentNO2 * chi(1,isrc,ityp)
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between OLM and TTRM
                  if (runttrm2) then
                     if (ttrmcompare(igrp,isrc,1,ityp) .lt. hrval(ityp)) then
                        hrval(ityp) = ttrmcompare(igrp,isrc,1,ityp)  !! <---- for EVENT processing, locking receptor
                     endif
                  endif
!! End of TTRM2 insert; Nov. 2021
               end do

! ---          Call EV_SUMVAL to add current source's contribution to
!              EV_AVEVAL, GRPAVE, etc.
               call ev_sumval

! ---          Write data to OLM debugging file; since this source is
!              NOT in an OLMGROUP, assign IOLM = 0 and use no_val, no2val,
!              olmval, and PercentNO2 scalar variables; also use BLNK8 field
!              instead of OLMID
               iolm = 0
               if (olmdebug) then
                  if (igroup(isrc,idxev(ievent)) .eq. 1) then
                     write(olmdbg,9987,err=998) kurdat, ievent,&
                     &evname(ievent), evaper(ievent),&
                     &grpid(idxev(ievent)), isrc,&
                     &srcid(isrc), iolm, blnk8, o3conc,&
                     &chi(1,isrc,1), ano2_ratio(isrc), no2,&
                     &no, chi(1,isrc,1),&
                     &PercentNO2, hrval(1),&
                     &grpave(idxev(ievent))
                  end if
               end if

            end if    ! source not in olmgrp IF-THEN block

            go to 2345
998         call errhdl(path,modnam,'E','520','OLMDEBUG')
2345        continue

            if (eval(isrc)) then
!              Check ARC centerline values for EVALFILE
!              output                              ---   CALL EVALCK
               call evalck
            end if

         end do ev_source_loop
! ---      End of EVENT Source Loop

         if ( grp_back(idxev(ievent)) ) then
! ---         Call EV_SUMBACK to update BACKGROUND contributions
            call ev_sumback
            if (olmdebug) then
! ---            Include BACKGROUND contribution in OLM debug file
               write(olmdbg,99873,err=999) kurdat, ievent,&
               &evname(ievent), evaper(ievent),&
               &grpid(idxev(ievent)), 'BACKGROUND',&
               &ev_bgconc(ihour), grpave(idxev(ievent))
99873          format(1x,i8.8,i6,2x,a10,2x,i3,2x,a8,8x,a10,110x,&
               &2(1x,e12.5))
            end if
         end if
      end if

!       ReInitialize __VAL arrays (1:NUMTYP)
      hrval  = 0.0d0

   end do receptor_loop
!     End Receptor LOOP

   go to 3456
999 call errhdl(path,modnam,'E','520','OLMDEBUG')
3456 continue

   return
end


subroutine ttrm_calc
!***********************************************************************
!             TTRM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Hourly Results for Ozone Reaction Rate Option
!
!        PROGRAMMER: Carlos Szembek.
!
!        DATE:    October 16, 2020
!
!
!        INPUTS: O3 value (hourly or other specified value), ambient temp.
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
!     JAT 7/22/21 D065, BLNK8 NOT USED
!      CHARACTER MODNAM*12, BLNK8*8
   character modnam*12
!     JAT 7/22/21 D065, IASTAT NOT USED
!      INTEGER :: IASTAT

!      DOUBLE PRECISION :: NO2, NOX, BCKGRD, CHIN, CHID,
!     &                    O3PPB, KO3, DUMVAL
!     JAT 7/22/21 D065, BCKGRD AND DUMVAL NOT USED
!      DOUBLE PRECISION :: NO2, NOX, BCKGRD, O3PPB, KO3, DUMVAL
   double precision :: no2, nox, o3ppb, ko3

!     Variable Initializations
   modnam = 'TTRM_CALC'
!     JAT 7/22/21 D065, BCKGRD AND DUMVAL NOT USED
!      BCKGRD = 0.0D0
!      DUMVAL = 0.0D0
   hrval(:) = 0.0d0
!     JAT 7/22/21 D065, IASTAT AND BLNK8 NOT USED
!      IASTAT = 0
!      BLNK8  = '        '

!     Initialize scalar variables
   ittrm = 0
   no2 = 0.0d0
   nox = 0.0d0
   ttrmtime = 0.0d0
   ttrmtime_prm = 0.0d0
   gamf = 0.0d0

!     Initialize TTRMVAL(NUMTYP) array
   ttrminst(:) = 0.0d0
   ttrmfrac(:) = 0.0d0
   ttrmfrac_prm(:) = 0.0d0
   ttrmfrac_aer(:) = 0.0d0
   ttrmno2(:)  = 0.0d0
   ttrmsub(:)  = 0.0d0

! --- Begin Receptor LOOP
   receptor_loop: do irec = 1, numrec
!        Reinitialize the scalar variables
      no2 = 0.0d0
      nox = 0.0d0
      ttrmtime = 0.0d0
      ttrmtime_prm = 0.0d0

      if( .not.evonly )then
! ---      Begin processing for NON-EVENT application

! ---      Begin Source Group LOOP to sum values
         group_loop: do igrp = 1, numgrp

! ---      Loop through all SOURCEs
!          as appropriate and sum values for this source group
            source_loopttrm: do isrc = 1, numsrc

               if( igroup(isrc,igrp) .ne. 1 )then
! ---          This source is not included in the current SRCGROUP
                  cycle source_loopttrm
               endif

! Calculate hourly k1 value (units: ppb^-1 sec^-1)
               k1 = (15.33 / ta)*dexp(-1450.0 / ta)
! Convert hourly ozone value back to ppb
               o3ppb = o3conc / o3_ppb
! Calculate the coefficient of time
               ko3 = -1.0*k1*o3ppb

! Save hourly data to TTRM debug file
               if (ttrmdbg) then
                  ttrmout(irec,isrc,1) = kurdat
                  ttrmout(irec,isrc,2) = axr(irec)
                  ttrmout(irec,isrc,3) = ayr(irec)
                  ttrmout(irec,isrc,4) = o3ppb ! O3 (in PPB)
                  ttrmout(irec,isrc,5) = ta ! ambient temperature
                  ttrmout(irec,isrc,6) = k1
               endif

! ---          Source is in this SRCGROUP
!              Calculate in-stack converted NO2 and available NOx
               no2 = ano2_ratio(isrc) * chi(irec,isrc,1)
               nox = (1.0d0-ano2_ratio(isrc))*chi(irec,isrc,1)  ! Total NO

!     Perform TTRM for POINT, VOLUME or AREA type sources
               if ((srctyp(isrc)(1:5) .eq. 'POINT') .or.&
               &(srctyp(isrc) .eq. 'VOLUME') .or.&
               &(srctyp(isrc)(1:4) .eq. 'AREA')) then
!                Calculate the final TTRM time based on the source type,
!                stability and plume dispersion type
                  call ttrm_step
! ---            Determine if CONC is O3-limited; if not, then no conversion needed
!                First check for missing O3 data (O3MISS) and apply full conversion
!                (subject to equilibrium ratio)
                  gamf = ttrmout(irec,isrc,17) !Saved GAMFACT
                  if (.not. o3miss) then !
                     do ityp = 1, numtyp  ! Calculate TTRM for all ITYPs (CONC, DDEP, WDEP or DEPOS)
!                       TTRMINST(ITYP) = NO2
                        if(ttrmtime .gt. 0.0) then
                           ttrmfrac(ityp) = (1.0 - dexp(ko3*ttrmtime))
                        else
                           ttrmfrac(ityp) = 0.0
                        endif
!                Check on whether the TTRM fraction is equal to or less than the NO2Equil (default set to 90%)
                        if (ttrmfrac(ityp) .ge. NO2Equil) then
                           ttrmfrac(ityp) = NO2Equil
                           hrval(ityp)   = NO2Equil*chi(irec,isrc,ityp)
                           no2           = 0.0
                           nox           = chi(irec,isrc,ityp) ! Total NO
                           ttrmsub(ityp)  = hrval(ityp)
                        else
                           ttrmno2(ityp)  = ttrmfrac(ityp)*nox
                           hrval(ityp)   = ttrmno2(ityp) + no2
                           ttrmsub(ityp)  = hrval(ityp)
                        end if
! Calculate for downwash cases
                        if (wake .and. (stable .or. hs.le.zi)) then
                           ttrmfrac_aer(ityp) = ttrmfrac(ityp)
!   Calculate Prime contribution (using travel time for downwash)
                           if (ttrmtime_prm .gt. 0.0) then
                              ttrmfrac_prm(ityp) = (1.0 - dexp(ko3*&
                              &ttrmtime_prm))
                              if (ttrmfrac_prm(ityp) .ge. NO2Equil) then
                                 ttrmfrac_prm(ityp) = NO2Equil
                              endif
                           else
                              ttrmfrac_prm(ityp) = 0.0
                           endif
                           ttrmfrac(ityp) = gamf*ttrmfrac_prm(ityp) +&
                           &(1.0d0 - gamf)*ttrmfrac_aer(ityp)
                           ttrmno2(ityp)  = ttrmfrac(ityp)*nox
                           hrval(ityp)   = ttrmno2(ityp) + no2
                           ttrmsub(ityp)  = hrval(ityp)
                        endif
!! Added for TTRM2;
                        if (runttrm2) then
                           ttrmcompare(igrp,isrc,irec,ityp) = hrval(ityp)
                        endif
!! End of TTRM2 insert; Nov. 2021
                     end do
                  else
! ---               For missing O3, scale by default NO2Equil (0.9)
                     do ityp = 1, numtyp
                        hrval(ityp)  = NO2Equil*chi(irec,isrc,ityp)
                        ttrmsub(ityp) = hrval(ityp)
!! Added for TTRM2
                        if (runttrm2) then
                           ttrmcompare(igrp,isrc,irec,ityp) = hrval(ityp)
                        endif
                     enddo
!! End of TTRM2 insert; Nov. 2021
                  end if
               else
!              For LINE, BUOYLINE, OPENPIT and RLINE sources
!              scale by default NO2Equil (0.9) and issue warning
                  do ityp = 1, numtyp
                     hrval(ityp)  = NO2Equil*chi(irec,isrc,ityp)
                     ttrmsub(ityp)  = hrval(ityp)
!! Added for TTRM2
                     if (runttrm2) then
                        ttrmcompare(igrp,isrc,irec,ityp) = hrval(ityp)
                     endif
!! End of TTRM2 insert; Nov. 2021
                     if (.not. l_ttrmsrctyp(isrc)) then
!                     Write Warning Message: TTRM not configured for source type
                        call errhdl(path,modnam,'W','710',srcid(isrc))
                        l_ttrmsrctyp(isrc) = .true.
                     end if
                  enddo
               end if
! Save values for TTRM2 evaluation file, if requested
               if (ttrm2dbg .and. (cmeth .lt. 3)) then
                  write(ttrm2tmp(1),pstfrm,err=77) axr(irec), ayr(irec),&
                  &ttrmcompare(igrp,isrc,irec,1),&
                  &azelev(irec), azhill(irec),&
                  &azflag(irec), chrave(1),&
                  &grpid(igrp), kurdat, srcid(isrc)
                  go to 877
77                write(dummy,'("PSTFL",I3.3)') ttrm2tmp(1)
                  call errhdl(path,modnam,'E','520',dummy)
                  runerr = .true.
877               continue
               endif

! Once all the sources in a group are complete, if requested, write output to debug file
               if (igroup(isrc,igrp) .eq. 1) then
                  if (ttrmdbg .and. (cmeth .lt. 3)) then
                     if (srctyp(isrc)(1:5) .eq. 'POINT') then
                        write(ttrmunt,77882,err=7781) kurdat, axr(irec),&
                        &ayr(irec), grpid(igrp), srcid(isrc), o3conc,&
                        &o3ppb, ta, k1, ko3,   ttrmout(irec,isrc,7),&
                        &ttrmout(irec,isrc,8),  ttrmout(irec,isrc,9),&
                        &ttrmout(irec,isrc,24), ttrmout(irec,isrc,25),&
                        &ttrmsrc(irec,isrc),&
                        &ttrmout(irec,isrc,10), ttrmout(irec,isrc,11),&
                        &ttrmout(irec,isrc,20), ttrmout(irec,isrc,12),&
                        &ttrmout(irec,isrc,13), ttrmout(irec,isrc,14),&
                        &ttrmout(irec,isrc,15), ttrmout(irec,isrc,16),&
                        &ttrmout(irec,isrc,17), ttrmout(irec,isrc,18),&
                        &ttrmout(irec,isrc,19), chi(irec,isrc,1), no2, nox,&
                        &ttrmfrac(1), ttrmno2(1), ttrmsub(1)

77882                   format(1x,i8.8,',',f9.2,',',f10.2,',',a8,',',a10,',',&
                        &f7.2,',',f7.3,',',f5.1,',',e8.2,',',e8.2,',',&
                        &f10.4,',',f9.4,',',f9.3,',',f9.4,',',f9.3,',',&
                        &a10,',',f8.2,',',&
                        &f8.2,',',f8.2,',',f5.2,',',f5.2,',',f5.2,',',&
                        &f5.3,',',f5.3,',',f5.3,',',f8.3,',',f8.3,',',&
                        &f8.3,',',f8.3,',',f8.3,',',f8.3,',',f8.3,',',&
                        &f8.3)
                        go to 5678
7781                    call errhdl(path,modnam,'W','720','TTRMDEBUG')
5678                    continue
                     else if (srctyp(isrc) .eq. 'VOLUME') then
! ---          Write data to TTRM debugging file
                        write(ttrmunt,77782,err=7771) kurdat, axr(irec),&
                        &ayr(irec), grpid(igrp), srcid(isrc), o3conc,&
                        &o3ppb, ta, k1, ko3,   ttrmout(irec,isrc,7),&
                        &ttrmout(irec,isrc,8),  ttrmout(irec,isrc,9),&
                        &ttrmsrc(irec,isrc),&
                        &ttrmout(irec,isrc,10), ttrmout(irec,isrc,11),&
                        &ttrmout(irec,isrc,20), ttrmout(irec,isrc,12),&
                        &ttrmout(irec,isrc,13),&
                        &ttrmout(irec,isrc,15),&
                        &chi(irec,isrc,1), no2, nox,&
                        &ttrmfrac(1), ttrmno2(1), ttrmsub(1)

77782                   format(1x,i8.8,',',f9.2,',',f10.2,',',a8,',',a10,',',&
                        &f7.2,',',f7.3,',',f5.1,',',e8.2,',',e8.2,',',&
                        &f10.4,',',f9.4,',',f9.3,',',a10,',',f8.2,',',&
                        &f8.2,',',f8.2,',',f5.2,',',f5.2,',','n/a',',',&
                        &f5.3,',','n/a',',','n/a',',','n/a',',','n/a',&
                        &',',f8.3,',',f8.3,',',f8.3,',',f8.3,',',f8.3,&
                        &',',f8.3)
                        go to 5778
7771                    call errhdl(path,modnam,'W','720','TTRMDEBUG')
5778                    continue
                     else if (srctyp(isrc)(1:4) .eq. 'AREA') then
! ---          Write data to TTRM debugging file for AREA, AREAPOLY or AREACIRC sources
                        write(ttrmunt,77772,err=7772) kurdat, axr(irec),&
                        &ayr(irec), grpid(igrp), srcid(isrc), o3conc,&
                        &o3ppb, ta, k1, ko3,   ttrmout(irec,isrc,7),&
                        &ttrmout(irec,isrc,8),  ttrmout(irec,isrc,9),&
                        &ttrmsrc(irec,isrc),&
                        &ttrmout(irec,isrc,11), ttrmout(irec,isrc,12),&
                        &ttrmout(irec,isrc,13),&
                        &chi(irec,isrc,1), no2, nox,&
                        &ttrmfrac(1), ttrmno2(1), ttrmsub(1)

77772                   format(1x,i8.8,',',f9.2,',',f10.2,',',a8,',',a10,',',&
                        &f7.2,',',f7.3,',',f5.1,',',e8.2,',',e8.2,',',&
                        &f10.4,',',f9.4,',',f9.3,',',a10,',n/a,',f8.2,&
                        &',n/a,',f5.2,',',f5.2,',','n/a',',','n/a',',',&
                        &'n/a',',','n/a',',','n/a',',','n/a',',',f8.3,',',&
                        &f8.3,',',f8.3,',',f8.3,',',f8.3,',',f8.3)
                        go to 7778
7772                    call errhdl(path,modnam,'W','720','TTRMDEBUG')
7778                    continue
                     else
! ---          For all other (non-supported) type of sources
                        write(ttrmunt,77773,err=7773) kurdat, axr(irec),&
                        &ayr(irec), grpid(igrp), srcid(isrc)

77773                   format(1x,i8.8,',',f9.2,',',f10.2,',',a8,',',a10,&
                        &18(',n/a'))
                        go to 7779
7773                    call errhdl(path,modnam,'W','720','TTRMDEBUG')
7779                    continue
                     end if
                  end if
               end if

! ---          Call SUMVAL to add current source's contribution to AVEVAL
               if (.not. runttrm2) then
                  call sumval
               endif

               if (eval(isrc)) then
!              Check ARC centerline values for EVALFILE
!              output                              ---   CALL EVALCK
                  call evalck
               end if

            end do source_loopttrm

            if (grp_back(igrp)) then
! ---         Call SUMBACK_NO2 to update BACKGROUND contributions
               if (.not. runttrm2) then
                  call sumback_no2
               endif
            end if

         end do group_loop
      else if (evonly) then
! ---      Check for inclusion of BACKGROUND in this EVENT
!C ---      Begin Source Group LOOP to sum values
!           EV_GROUP_LOOP: DO IGRP = 1, NUMGRP

! ---      Loop through all SOURCEs
!          as appropriate and sum values for this source group
         ev_source_loopttrm: do isrc = 1, numsrc
            ityp = 1

            if( igroup(isrc,igrp) .ne. 1 )then
! ---          This source is not included in the current SRCGROUP
               cycle ev_source_loopttrm
            endif

! Calculate hourly k1 value (units: ppb^-1 sec^-1)
            k1 = (15.33 / ta)*dexp(-1450.0 / ta)
! Convert hourly ozone value back to ppb
            o3ppb = o3conc / o3_ppb
! Calculate the coefficient of time
            ko3 = -1.0*k1*o3ppb

! Save hourly data to TTRM debug file
            if (ttrmdbg) then
               ttrmout(irec,isrc,1) = kurdat
               ttrmout(irec,isrc,2) = axr(1)
               ttrmout(irec,isrc,3) = ayr(1)
               ttrmout(irec,isrc,4) = o3ppb ! O3 (in PPB)
               ttrmout(irec,isrc,5) = ta ! ambient temperature
               ttrmout(irec,isrc,6) = k1
            endif

! ---          Source is in this SRCGROUP
!              Calculate in-stack converted NO2 and available NOx
            no2 = ano2_ratio(isrc) * chi(1,isrc,1)
            nox = (1.0d0-ano2_ratio(isrc))*chi(1,isrc,1) ! Total NO
!
!     Perform TTRM for POINT, VOLUME or AREA type sources
            if ((srctyp(isrc)(1:5) .eq. 'POINT') .or.&
            &(srctyp(isrc) .eq. 'VOLUME') .or.&
            &(srctyp(isrc)(1:4) .eq. 'AREA')) then
!                Calculate the final TTRM time based on the source type,
!                stability and plume dispersion type
               call ttrm_step
! ---            Determine if CONC is O3-limited; if not, then no conversion needed
!                First check for missing O3 data (O3MISS) and apply full conversion
!                (subject to equilibrium ratio)
               if (.not. o3miss) then !
                  do ityp = 1, numtyp  ! Calculate TTRM for all ITYPs (CONC, DDEP, WDEP or DEPOS)
!                       TTRMINST(ITYP) = NO2
                     if(ttrmtime .gt. 0.0) then
                        ttrmfrac(ityp) = (1.0 - dexp(ko3*ttrmtime))
                     else
                        ttrmfrac(ityp) = 0.0
                     endif
!                Check on whether the TTRM fraction is equal to or less than the NO2Equil (default set to 90%)
                     if (ttrmfrac(ityp) .ge. NO2Equil) then
                        ttrmfrac(ityp) = NO2Equil
                        hrval(ityp)   = NO2Equil*chi(1,isrc,1)
                        no2           = 0.0
                        nox           = chi(1,isrc,1) ! Total NO
                        ttrmsub(ityp)  = hrval(ityp)
                     else
                        ttrmno2(ityp)  = ttrmfrac(ityp)*nox
                        hrval(ityp)   = ttrmno2(ityp) + no2
                        ttrmsub(ityp)  = hrval(ityp)
                     end if
! Calculate for downwash cases
                     if (wake .and. (stable .or. hs.le.zi)) then
                        ttrmfrac_aer(ityp) = ttrmfrac(ityp)
!   Calculate Prime contribution (using travel time for downwash)
                        if (ttrmtime_prm .gt. 0.0) then
                           ttrmfrac_prm(ityp) = (1.0 - dexp(ko3*&
                           &ttrmtime_prm))
                           if (ttrmfrac_prm(ityp) .ge. NO2Equil) then
                              ttrmfrac_prm(ityp) = NO2Equil
                           endif
                        else
                           ttrmfrac_prm(ityp) = 0.0
                        endif
                        ttrmfrac(ityp) = gamf*ttrmfrac_prm(ityp) +&
                        &(1.0d0 - gamf)*ttrmfrac_aer(ityp)
                        ttrmno2(ityp)  = ttrmfrac(ityp)*nox
                        hrval(ityp)   = ttrmno2(ityp) + no2
                        ttrmsub(ityp)  = hrval(ityp)
                     endif
!! Added for TTRM2
                     if (runttrm2) then
                        ttrmcompare(igrp,isrc,1,ityp) = hrval(ityp)
                     endif
!! End of TTRM2 insert; Nov. 2021
                  end do
               else
! ---               For missing O3, scale by default NO2Equil (0.9)
                  do ityp = 1, numtyp
                     hrval(ityp)  = NO2Equil*chi(1,isrc,ityp)
                     ttrmsub(ityp) = hrval(ityp)
!! Added for TTRM2
                     if (runttrm2) then
                        ttrmcompare(igrp,isrc,1,ityp) = hrval(ityp)
                     endif
                  enddo
!! End of TTRM2 insert; Nov. 2021
               end if
            else
!              For LINE, BUOYLINE, OPENPIT and RLINE sources
!              scale by default NO2Equil (0.9) and issue warning
               do ityp = 1, numtyp
                  hrval(ityp)  = NO2Equil*chi(1,isrc,ityp)
                  ttrmsub(ityp) = hrval(ityp)
!! Added for TTRM2
                  if (runttrm2) then
                     ttrmcompare(igrp,isrc,1,ityp) = hrval(ityp)
                  endif
               enddo
!! End of TTRM2 insert; Nov. 2021
               if (.not. l_ttrmsrctyp(isrc)) then
!                   Write Warning Message: TTRM not configured for source type
                  call errhdl(path,modnam,'W','710',srcid(isrc))
                  l_ttrmsrctyp(isrc) = .true.
               end if
            end if

! ---          Call EV_SUMVAL to add current source's contribution to
!              EV_AVEVAL, GRPAVE, etc.
            if (.not. runttrm2) then
               call ev_sumval
            endif
!               CALL EV_SUMVAL

! If requested, write output to debug file
            if (igroup(isrc,igrp) .eq. 1) then
               if (ttrmdbg .and. (cmeth .lt. 3)) then
                  if (srctyp(isrc)(1:5) .eq. 'POINT') then
                     write(ttrmunt,97882,err=9781) kurdat,&
                     &ievent, evname(ievent), evaper(ievent),&
                     &grpid(igrp), srcid(isrc), o3conc,&
                     &o3ppb, ta, k1, ko3,   ttrmout(1,isrc,7),&
                     &ttrmout(1,isrc,8),  ttrmout(1,isrc,9),&
                     &ttrmout(irec,isrc,24), ttrmout(irec,isrc,25),&
                     &ttrmsrc(irec,isrc),&
                     &ttrmout(1,isrc,10), ttrmout(1,isrc,11),&
                     &ttrmout(1,isrc,20), ttrmout(1,isrc,12),&
                     &ttrmout(1,isrc,13), ttrmout(1,isrc,14),&
                     &ttrmout(1,isrc,15), ttrmout(1,isrc,16),&
                     &ttrmout(1,isrc,17), ttrmout(1,isrc,18),&
                     &ttrmout(1,isrc,19), chi(1,isrc,1), no2, nox,&
                     &ttrmfrac(1), ttrmno2(1), ttrmsub(1)

97882                format(1x,i8.8,',',i6,',',a10,',',&
                     &i3,',',a8,',',a10,',',&
                     &f7.2,',',f7.3,',',f5.1,',',e8.2,',',e8.2,',',&
                     &f10.4,',',f9.4,',',f9.3,',',f9.4,',',f9.3,',',&
                     &a10,',',f8.2,',',&
                     &f8.2,',',f8.2,',',f5.2,',',f5.2,',',f5.2,',',&
                     &f5.3,',',f5.3,',',f5.3,',',f8.3,',',f8.3,',',&
                     &f8.3,',',f8.3,',',f8.3,',',f8.3,',',f8.3,',',&
                     &f8.3)
                     go to 56789
9781                 call errhdl(path,modnam,'W','720','TTRMDEBUG')
56789                continue
                  else if (srctyp(isrc) .eq. 'VOLUME') then
! ---          Write data to TTRM debugging file
                     write(ttrmunt,97782,err=9771) kurdat,&
                     &ievent, evname(ievent), evaper(ievent),&
                     &grpid(igrp), srcid(isrc), o3conc,&
                     &o3ppb, ta, k1, ko3,ttrmout(1,isrc,7),&
                     &ttrmout(1,isrc,8),  ttrmout(1,isrc,9),&
                     &ttrmsrc(irec,isrc),&
                     &ttrmout(1,isrc,10), ttrmout(1,isrc,11),&
                     &ttrmout(1,isrc,20), ttrmout(1,isrc,12),&
                     &ttrmout(1,isrc,13),&
                     &ttrmout(1,isrc,15),&
                     &chi(1,isrc,1), no2, nox,&
                     &ttrmfrac(1), ttrmno2(1), ttrmsub(1)

97782                format(1x,i8.8,',',i6,',',a10,',',&
                     &i3,',',a8,',',a10,',',&
                     &f7.2,',',f7.3,',',f5.1,',',e8.2,',',e8.2,',',&
                     &f10.4,',',f9.4,',',f9.3,',',a10,',',f8.2,',',&
                     &f8.2,',',f8.2,',',f5.2,',',f5.2,',','n/a',',',&
                     &f5.3,',','n/a',',','n/a',',','n/a',',','n/a',&
                     &',',f8.3,',',f8.3,',',f8.3,',',f8.3,',',f8.3,&
                     &',',f8.3)
                     go to 57789
9771                 call errhdl(path,modnam,'W','720','TTRMDEBUG')
57789                continue
                  else if (srctyp(isrc)(1:4) .eq. 'AREA') then
! ---          Write data to TTRM debugging file for AREA, AREAPOLY or AREACIRC sources
                     write(ttrmunt,97772,err=9772) kurdat,&
                     &ievent, evname(ievent), evaper(ievent),&
                     &grpid(igrp), srcid(isrc), o3conc,&
                     &o3ppb, ta, k1, ko3,   ttrmout(1,isrc,7),&
                     &ttrmout(1,isrc,8),  ttrmout(1,isrc,9),&
                     &ttrmsrc(irec,isrc),&
                     &ttrmout(1,isrc,11), ttrmout(1,isrc,12),&
                     &ttrmout(1,isrc,13),&
                     &chi(1,isrc,1), no2, nox,&
                     &ttrmfrac(1), ttrmno2(1), ttrmsub(1)

97772                format(1x,i8.8,',',i6,',',a10,',',&
                     &i3,',',a8,',',a10,',',&
                     &f7.2,',',f7.3,',',f5.1,',',e8.2,',',e8.2,',',&
                     &f10.4,',',f9.4,',',f9.3,',',a10,',n/a,',f8.2,&
                     &',n/a,',f5.2,',',f5.2,',','n/a',',','n/a',',',&
                     &'n/a',',','n/a',',','n/a',',','n/a',',',f8.3,',',&
                     &f8.3,',',f8.3,',',f8.3,',',f8.3,',',f8.3)
                     go to 77789
9772                 call errhdl(path,modnam,'W','720','TTRMDEBUG')
77789                continue

                  else
! ---          for all other (non-supported) source types
                     write(ttrmunt,97773,err=9773) kurdat,&
                     &ievent, evname(ievent), evaper(ievent),&
                     &grpid(igrp), srcid(isrc)

97773                format(1x,i8.8,',',i6,',',a10,',',&
                     &i3,',',a8,',',a10,18(',n/a'))
                     go to 77790
9773                 call errhdl(path,modnam,'W','720','TTRMDEBUG')
77790                continue

                  end if
               end if
            end if

            if (eval(isrc)) then
!              Check ARC centerline values for EVALFILE
!              output                              ---   CALL EVALCK
               call evalck
            end if

         end do ev_source_loopttrm
! ---      End of EVENT Source Loop

         if ( grp_back(idxev(ievent)) ) then
! ---         Call EV_SUMBACK to update BACKGROUND contributions
            if (.not. runttrm2) then
               call ev_sumback
            endif
         end if
      end if !  End of Event_or_Non-Event IF BLOCK

!       ReInitialize __VAL arrays (1:NUMTYP)
      hrval  = 0.0d0

   end do receptor_loop
!     End Receptor LOOP

   return
end

subroutine ttrm_step
!***********************************************************************
!             TTRM_STEP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Steps through Hourly Calcs for the Travel Time Reaction
!                 Method and saves intermediary variables for an optional
!                 debug file.
!
!        PROGRAMMER: Carlos Szembek.
!
!        DATE:    February 17, 2021
!
!
!        INPUTS: O3 value (hourly or other specified value), ambient temp.
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   TTRM_CALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
!      CHARACTER MODNAM*12, BLNK8*8
   character modnam*12
!      INTEGER :: IASTAT

!      DOUBLE PRECISION :: NO2, NOX, BCKGRD, CHIN, CHID,
!     &                    O3PPB, KO3, DUMVAL

!      DOUBLE PRECISION :: TTRMNO2(NUMTYPE)

!     Variable Initializations
   modnam = 'TTRM_STEP'

!     Perform TTRM calculations based on SRCTYPE, dominant plume dispersion type and stability
   if (srctyp(isrc)(1:5) .eq. 'POINT') then
!       Calculate travel time if POINT source is subject to downwash
      if (wake .and. (stable .or. hs.le.zi)) then
         if(ttrmout(irec,isrc,24) .gt. 0.0) then
            ttrmtime_prm = dabs(ttrmout(irec,isrc,10) /&
            &ttrmout(irec,isrc,24)) ! X / UEFFe ! Use downwind distance and wind speed for downwash (UEFFe)
         else
            ttrmtime_prm = 0.0
         endif
         ttrmout(irec,isrc,25) = ttrmtime_prm
      endif
!       Calculate travel time for portion of plume not subject to downwash
!     Calculate the travel time conservatively based on the radial distance (DISTR) and effective wind speed (ueff)
!!!         The effective wind speed based on the meander fraction; stability and plume type:
!!!         If the meander fraction (FRAN) <= 0.5 then
!!!           If STABLE use [ueff]
!!!           Else if the partial penetration fration (PPF) > 0.5 use [ueff3]
!!!           Else if the concentration from the direct plume (CHID) > indirect (CHIN) use [ueffd]
!!!           Otherwise for the indirect plume use [ueff]
!!!         Else for FRAN > 0.5
!!!           If STABLE use [ueff]
!!!           Else use [ueffd]
      if (ttrmout(irec,isrc,15) .ge. 0.5) then !MEANDER FRACTION >= 0.5
         if (stable) then
            if(ttrmout(irec,isrc,12) .gt. 0.0) then
               ttrmtime = dabs(ttrmout(irec,isrc,11) /&
               &ttrmout(irec,isrc,12)) ! DISTR / UEFF ! Use radial distance: TTRMOUT(IREC,ISRC,11)
            else
               ttrmtime = 0.
            endif
            if (ttrmdbg) then
               ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
               ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
               ttrmout(irec,isrc,9) = ttrmtime
               ttrmsrc(irec,isrc) = 'STAB-M-GAU'
            end if
         else
            if (ttrmout(irec,isrc,16) .ge. 0.5) then ! For partial penetrated dominant meander plume
               if(ttrmout(irec,isrc,14) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,11) /&
                  &ttrmout(irec,isrc,14)) ! DISTR / UEFF3
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,14) ! UEFF3
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'UNST-M-PEN'
               end if
            else if (ttrmout(irec,isrc,19) .ge.&
            &ttrmout(irec,isrc,18)) then! CHID >= CHIN; For direct dominant meander plume
               if(ttrmout(irec,isrc,13) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,11) /&
                  &ttrmout(irec,isrc,13)) ! DISTR / UEFFD
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,13) ! UEFFD
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'UNST-M-DIR'
               end if
            else ! for indirect dominant meander plume
               if(ttrmout(irec,isrc,12) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,11) /&
                  &ttrmout(irec,isrc,12)) ! DISTR / UEFF
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'UNST-M-IND'
               end if
            end if
         end if
      else ! For the COHERENT Plume
         if (ttrmout(irec,isrc,10) .ge. 0.) then ! If the downwind distance is >= 0 use downwind distance
            if (stable) then
               if(ttrmout(irec,isrc,12) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,10) /&
                  &ttrmout(irec,isrc,12)) ! X / UEFF ! Use downwind distance: TTRMOUT(IREC,ISRC,10)
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,10) ! X
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'STAB-C-GAU'
               end if
            else
               if (ttrmout(irec,isrc,16) .ge. 0.5) then ! For partial penetrated dominant COHERENT plume
                  if(ttrmout(irec,isrc,14) .gt. 0.0) then
                     ttrmtime = dabs(ttrmout(irec,isrc,10)/&
                     &ttrmout(irec,isrc,14)) ! X / UEFF3
                  else
                     ttrmtime = 0.
                  endif
                  if (ttrmdbg) then
                     ttrmout(irec,isrc,7) = ttrmout(irec,isrc,10) ! X
                     ttrmout(irec,isrc,8) = ttrmout(irec,isrc,14) ! UEFF3
                     ttrmout(irec,isrc,9) = ttrmtime
                     ttrmsrc(irec,isrc) = 'UNST-C-PEN'
                  end if
               else if (ttrmout(irec,isrc,19) .ge.&
               &ttrmout(irec,isrc,18)) then! CHID >= CHIN; For direct dominant COHERENT plume
                  if(ttrmout(irec,isrc,13) .gt. 0.0) then
                     ttrmtime = dabs(ttrmout(irec,isrc,10)/&
                     &ttrmout(irec,isrc,13)) ! X / UEFFD
                  else
                     ttrmtime = 0.
                  endif
                  if (ttrmdbg) then
                     ttrmout(irec,isrc,7) = ttrmout(irec,isrc,10) ! X
                     ttrmout(irec,isrc,8) = ttrmout(irec,isrc,13) ! UEFFD
                     ttrmout(irec,isrc,9) = ttrmtime
                     ttrmsrc(irec,isrc) = 'UNST-C-DIR'
                  end if
               else ! for indirect dominant COHERENT plume
                  if(ttrmout(irec,isrc,12) .gt. 0.0) then
                     ttrmtime = dabs(ttrmout(irec,isrc,10)/&
                     &ttrmout(irec,isrc,12)) ! X / UEFF
                  else
                     ttrmtime = 0.
                  endif
                  if (ttrmdbg) then
                     ttrmout(irec,isrc,7) = ttrmout(irec,isrc,10) ! X
                     ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                     ttrmout(irec,isrc,9) = ttrmtime
                     ttrmsrc(irec,isrc) = 'UNST-C-IND'
                  end if
               end if
            end if
         else ! If the downwind distance is < 0, use the RADIAL distance
            if (stable) then
               if(ttrmout(irec,isrc,12) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,11) /&
                  &ttrmout(irec,isrc,12)) ! DISTR / UEFF ! Use downwind distance: TTRMOUT(IREC,ISRC,10)
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'STAB-m-GAU'
               end if
            else
               if (ttrmout(irec,isrc,16) .ge. 0.5) then ! For partial penetrated dominant COHERENT plume
                  if(ttrmout(irec,isrc,14) .gt. 0.0) then
                     ttrmtime = dabs(ttrmout(irec,isrc,11)/&
                     &ttrmout(irec,isrc,14)) ! DISTR / UEFF3
                  else
                     ttrmtime = 0.
                  endif
                  if (ttrmdbg) then
                     ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                     ttrmout(irec,isrc,8) = ttrmout(irec,isrc,14) ! UEFF3
                     ttrmout(irec,isrc,9) = ttrmtime
                     ttrmsrc(irec,isrc) = 'UNST-m-PEN'
                  end if
               else if (ttrmout(irec,isrc,19) .ge.&
               &ttrmout(irec,isrc,18)) then! CHID >= CHIN; For direct dominant COHERENT plume
                  if(ttrmout(irec,isrc,13) .gt. 0.0) then
                     ttrmtime = dabs(ttrmout(irec,isrc,11)/&
                     &ttrmout(irec,isrc,13)) ! DISTR / UEFFD
                  else
                     ttrmtime = 0.
                  endif
                  if (ttrmdbg) then
                     ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                     ttrmout(irec,isrc,8) = ttrmout(irec,isrc,13) ! UEFFD
                     ttrmout(irec,isrc,9) = ttrmtime
                     ttrmsrc(irec,isrc) = 'UNST-m-DIR'
                  end if
               else ! for indirect dominant COHERENT plume
                  if(ttrmout(irec,isrc,12) .gt. 0.0) then
                     ttrmtime = dabs(ttrmout(irec,isrc,11)/&
                     &ttrmout(irec,isrc,12)) ! DISTR / UEFF
                  else
                     ttrmtime = 0.
                  endif
                  if (ttrmdbg) then
                     ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                     ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                     ttrmout(irec,isrc,9) = ttrmtime
                     ttrmsrc(irec,isrc) = 'UNST-m-IND'
                  end if
               end if
            end if
         endif
      endif
   else if (srctyp(isrc) .eq. 'VOLUME') then
      if (ttrmout(irec,isrc,15) .ge. 0.5) then ! Meander plume; Use RADIAL distance (DISTR)
         if (stable) then ! Use  eff. wspd (UEFF)
            if(ttrmout(irec,isrc,12) .gt. 0.0) then
               ttrmtime = dabs(ttrmout(irec,isrc,11) /&
               &ttrmout(irec,isrc,12)) ! DISTR / UEFF
            else
               ttrmtime = 0.
            endif
            if (ttrmdbg) then
               ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
               ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFFD
               ttrmout(irec,isrc,9) = ttrmtime
               ttrmsrc(irec,isrc) = 'STAB-M-UEF'
            end if
         else if (ttrmout(irec,isrc,13) .gt.&
         &ttrmout(irec,isrc,12)) then ! If UEFFD > UEFF: Direct meander plume
            if(ttrmout(irec,isrc,13) .gt. 0.0) then
               ttrmtime = dabs(ttrmout(irec,isrc,11) /&
               &ttrmout(irec,isrc,13)) ! DISTR / UEFFD
            else
               ttrmtime = 0.
            endif
            if (ttrmdbg) then
               ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
               ttrmout(irec,isrc,8) = ttrmout(irec,isrc,13) ! UEFFD
               ttrmout(irec,isrc,9) = ttrmtime
               ttrmsrc(irec,isrc) = 'UNST-M-DIR'
            end if
         else ! If UEFFD < UEFF: INDIRECT Meander plume
            if(ttrmout(irec,isrc,12) .gt. 0.0) then
               ttrmtime = dabs(ttrmout(irec,isrc,11) /&
               &ttrmout(irec,isrc,12)) ! DISTR / UEFF
            else
               ttrmtime = 0.
            endif
            if (ttrmdbg) then
               ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
               ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
               ttrmout(irec,isrc,9) = ttrmtime
               ttrmsrc(irec,isrc) = 'UNST-M-IND'
            end if
         end if
      else ! for COHERENT plume
         if (ttrmout(irec,isrc,10) .ge. 0.0) then ! For downwind distances >= 0.0 use the DOWNWIND distance
            if (stable) then ! Use  eff. wspd (UEFF)
               if(ttrmout(irec,isrc,12) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,10)/&
                  &ttrmout(irec,isrc,12)) ! X / UEFF
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,10) ! X
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'STAB-C-UEF'
               end if
            else if (ttrmout(irec,isrc,13) .gt.&
            &ttrmout(irec,isrc,12)) then ! IF UEFFD > UEFF: DIRECT COHERENT
               if(ttrmout(irec,isrc,13) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,10)/&
                  &ttrmout(irec,isrc,13)) ! X / UEFFD
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,10) ! X
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,13) ! UEFFD
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'UNST-C-DIR'
               end if
            else ! UEFFD < UEFF: INDIRECT COHERENT
               if(ttrmout(irec,isrc,12) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,10)/&
                  &ttrmout(irec,isrc,12)) ! X / UEFF
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,10) ! X
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'UNST-C-IND'
               end if
            end if
         else ! for DOWNWIND distance < 0, use RADIAL distance
            if (stable) then ! Use  eff. wspd (UEFFD)
               if(ttrmout(irec,isrc,12) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,11)/&
                  &ttrmout(irec,isrc,12)) ! DISTR / UEFF
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'STAB-m-UEF'
               end if
            else if (ttrmout(irec,isrc,13) .gt.&
            &ttrmout(irec,isrc,12)) then ! IF UEFFD > UEFF: DIRECT COHERENT
               if(ttrmout(irec,isrc,13) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,11)/&
                  &ttrmout(irec,isrc,13)) ! DISTR / UEFFD
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! X
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,13) ! UEFFD
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'UNST-m-DIR'
               end if
            else ! UEFFD < UEFF: INDIRECT COHERENT
               if(ttrmout(irec,isrc,12) .gt. 0.0) then
                  ttrmtime = dabs(ttrmout(irec,isrc,11)/&
                  &ttrmout(irec,isrc,12)) ! DISTR / UEFF
               else
                  ttrmtime = 0.
               endif
               if (ttrmdbg) then
                  ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
                  ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
                  ttrmout(irec,isrc,9) = ttrmtime
                  ttrmsrc(irec,isrc) = 'UNST-m-IND'
               end if
            end if
         end if
      end if
   else if (srctyp(isrc)(1:4) .eq. 'AREA') then
!     Perform TTRM calcs for AREA, AREAPOLY or AREACIRC sources
      if (stable) then ! Use  eff. wspd (UEFF)
         if(ttrmout(irec,isrc,12) .gt. 0.0) then
            ttrmtime = dabs(ttrmout(irec,isrc,11)/&
            &ttrmout(irec,isrc,12)) ! DISTR / UEFF
         else
            ttrmtime = 0.
         endif
         if (ttrmdbg) then
            ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
            ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
            ttrmout(irec,isrc,9) = ttrmtime
            ttrmsrc(irec,isrc) = 'AREA-STAB'
         end if
      else if (ttrmout(irec,isrc,13) .gt.&
      &ttrmout(irec,isrc,12)) then ! IF UEFFD > UEFF: DIRECT COHERENT
         if(ttrmout(irec,isrc,13) .gt. 0.0) then
            ttrmtime = dabs(ttrmout(irec,isrc,11)/&
            &ttrmout(irec,isrc,13)) ! DISTR / UEFFD
         else
            ttrmtime = 0.
         endif
         if (ttrmdbg) then
            ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
            ttrmout(irec,isrc,8) = ttrmout(irec,isrc,13) ! UEFFD
            ttrmout(irec,isrc,9) = ttrmtime
            ttrmsrc(irec,isrc) = 'AREA-U-DIR'
         end if
      else ! UEFFD < UEFF: INDIRECT COHERENT
         if(ttrmout(irec,isrc,12) .gt. 0.0) then
            ttrmtime = dabs(ttrmout(irec,isrc,11)/&
            &ttrmout(irec,isrc,12)) ! DISTR / UEFF
         else
            ttrmtime = 0.
         endif
         if (ttrmdbg) then
            ttrmout(irec,isrc,7) = ttrmout(irec,isrc,11) ! DISTR
            ttrmout(irec,isrc,8) = ttrmout(irec,isrc,12) ! UEFF
            ttrmout(irec,isrc,9) = ttrmtime
            ttrmsrc(irec,isrc) = 'AREA-U-IND'
         end if
      end if
   end if

   return
end


subroutine arm2_calc
!***********************************************************************
!             ARM2_CALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Hourly Results for ARM2 Option
!
!        PROGRAMMER: Roger W. Brode, U.S. EPA/AQMG
!
!        DATE:    November 19, 2013
!
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: ARM2_Coef(7)
   double precision :: noxconc(numrec,numtyp)

!     Variable Initializations
   data ARM2_Coef / -1.1723d-17, 4.2795d-14, 5.8345d-11, 3.4555d-08,&
   &5.6062d-06, 2.7383d-03, 1.2441d+00 /

   modnam = 'ARM2_CALC'
   noxconc(:,:) = 0.0d0
   hrval(:)     = 0.0d0
   RatioARM2    = 0.0d0

   if (.not.evonly) then
! ---    Non-EVENT processing mode

! ---    Calculate total NOXCONC for each receptor from all sources
!        based on the CHI(IREC,ISRC,ITYP) array
      noxconc(:,:) = sum(chi,dim=2)

! ---    Begin Receptor LOOP
      receptor_loop: do irec = 1, numrec

! ---       Initialize ARM2 Ratio for this receptor
         RatioARM2 = 0.0d0

!           Calculate ARM2 ratio, RatioARM2, based on NOXCONC for this receptor for ITYP=1
         RatioARM2 = (ARM2_Coef(1)*noxconc(irec,1)**6) +&
         &(ARM2_Coef(2)*noxconc(irec,1)**5) -&
         &(ARM2_Coef(3)*noxconc(irec,1)**4) +&
         &(ARM2_Coef(4)*noxconc(irec,1)**3) -&
         &(ARM2_Coef(5)*noxconc(irec,1)**2) -&
         &ARM2_Coef(6)*noxconc(irec,1) + ARM2_Coef(7)

!           Adjust RatioARM2 if needed based on ARM2_Max and ARM2_min
         if( RatioARM2 .gt. ARM2_Max )then
            RatioARM2 = ARM2_Max
         elseif( RatioARM2 .lt. ARM2_min )then
            RatioARM2 = ARM2_min
         endif

! ---       Begin Source Group Loop
         group_loop: do igrp = 1, numgrp

! ---        Begin Source Loop
            source_loop: do isrc = 1, numsrc

               do ityp = 1, numtyp

! ---             Assign CHI value adjusted by RatioARM2 to HRVAL
                  hrval(ityp) = chi(irec,isrc,ityp) * RatioARM2
!! Added for TTRM2
                  if (runttrm2) then
                     if (ttrm2dbg .and. (cmeth .lt. 3)) then
                        write(ttrm2tmp(2),pstfrm,err=87) axr(irec), ayr(irec),&
                        &hrval(1),&
                        &azelev(irec), azhill(irec), azflag(irec),&
                        &chrave(1), grpid(igrp), kurdat, srcid(isrc)
                        go to 887
87                      write(dummy,'("PSTFL",I4.4)') ttrm2tmp(2)
                        call errhdl(path,modnam,'E','520',dummy)
                        runerr = .true.
887                     continue
                     endif
                     if (ttrmcompare(igrp,isrc,irec,ityp) .lt. hrval(ityp)) then
                        hrval(ityp) = ttrmcompare(igrp,isrc,irec,ityp)
                     endif
                     if (ttrm2dbg .and. (cmeth .lt. 3)) then
                        write(ttrm2tmp(3),pstfrm,err=97) axr(irec), ayr(irec),&
                        &hrval(1),&
                        &azelev(irec), azhill(irec), azflag(irec),&
                        &chrave(1), grpid(igrp), kurdat, srcid(isrc)
                        go to 897
97                      write(dummy,'("PSTFL",I4.4)') ttrm2tmp(3)
                        call errhdl(path,modnam,'E','520',dummy)
                        runerr = .true.
897                     continue
                     endif
                  endif
!! End of TTRM2 insert; Nov. 2021
               end do

!              Call SUMVAL to update AVEVAL, ANNVAL, and SHVALS Arrays          ---   CALL SUMVAL
               call sumval

! ---          Write data to ARM2 debugging file if needed
               if (arm2debug) then
                  if (igroup(isrc,igrp) .eq. 1) then
                     write(arm2dbg,9987,err=998) kurdat, irec,&
                     &grpid(igrp), isrc, srcid(isrc),&
                     &noxconc(irec,1), chi(irec,isrc,1),&
                     &RatioARM2, hrval(1),&
                     &aveval(irec,igrp,1,1)
9987                 format(1x,i8.8,i6,2x,a8,i6,2x,a12,2x,&
                     &5(1x,e12.5))
                  end if
               end if

               go to 1234
998            call errhdl(path,modnam,'E','520','ARM2DEBUG')
               runerr = .true.
1234           continue

            end do source_loop

            if (grp_back(igrp)) then

               call sumback_no2

! ---          Write data to ARM2 debugging file
               if (arm2debug) then
                  write(arm2dbg,99871,err=9981) kurdat, irec,&
                  &grpid(igrp), 'BACKGROUND',&
                  &bgconc, aveval(irec,igrp,1,1)
99871             format(1x,i8.8,i6,2x,a8,8x,a10,43x,2(1x,e12.5))
               end if

               go to 2345
! ---          Issue error message for writing to ARMDEBUG file
9981           call errhdl(path,modnam,'E','520','ARMDEBUG')
               runerr = .true.
2345           continue

            end if

         end do group_loop

      end do receptor_loop

   else if (evonly) then
! ---    EVENT Processing; initialize arrays
      ev_aveval(:) = 0.0d0
! ---    Initialize ARM2 Ratio to 0.0D0
      RatioARM2    = 0.0d0

! ---    Calculate total NOXCONC for this EVENT from all sources
!        based on the CHI(IREC,ISRC,ITYP) array
      noxconc = sum(chi,dim=2)

!        Calculate ARM2 ratio, RatioARM2, based on NOXCONC
      RatioARM2 = (ARM2_Coef(1)*noxconc(1,1)**6) +&
      &(ARM2_Coef(2)*noxconc(1,1)**5) -&
      &(ARM2_Coef(3)*noxconc(1,1)**4) +&
      &(ARM2_Coef(4)*noxconc(1,1)**3) -&
      &(ARM2_Coef(5)*noxconc(1,1)**2) -&
      &ARM2_Coef(6)*noxconc(1,1) + ARM2_Coef(7)

!        Adjust RatioARM2 if needed based on ARM2_Max and ARM2_min
      if (RatioARM2 .gt. ARM2_Max) then
         RatioARM2 = ARM2_Max
      else if (RatioARM2 .lt. ARM2_min) then
         RatioARM2 = ARM2_min
      end if

! ---    Begin Source Loop
      ev_source_loop: do isrc = 1, numsrc

         ityp = 1

! ---       Apply RatioARM2 to hourly value from CHI array and
!           assign to HRVAL array; then call EV_SUMVAL
         hrval(ityp) = chi(1,isrc,ityp) * RatioARM2
!! Added for TTRM2
         if (runttrm2) then
            if (ttrmcompare(igrp,isrc,1,ityp) .lt. hrval(ityp)) then
               hrval(ityp) = ttrmcompare(igrp,isrc,1,ityp)
            endif
         endif
!! End of TTRM2 insert; Nov. 2021
!           Sum HRVAL to HRVALS and EV_AVEVAL    ---   CALL EV_SUMVAL
         if (igroup(isrc,idxev(ievent)) .eq. 1) then
            call ev_sumval

! ---          Write data to ARM2 debugging file
            if (arm2debug) then
               if (igroup(isrc,idxev(ievent)) .eq. 1) then
                  write(arm2dbg,99872,err=999) kurdat, ievent,&
                  &evname(ievent), evaper(ievent),&
                  &grpid(idxev(ievent)), isrc, srcid(isrc),&
                  &noxconc(1,1), chi(1,isrc,1), RatioARM2,&
                  &hrval(1), grpave(idxev(ievent))
               end if
            end if
99872       format(1x,i8.8,i6,2x,a10,i5,2x,a8,1x,i5,2x,a12,2x,&
            &5(1x,e12.5))

         end if

         go to 3456
999      call errhdl(path,modnam,'E','520','ARM2DEBUG')
         runerr = .true.
3456     continue

      end do ev_source_loop

!        Sum BACKGRND to HRVALS, EV_AVEVAL, GRPVAL and GRPAV Arrays ---   CALL EV_SUMBACK
      if (idxev(ievent) .eq. igrp .and.&
      &grp_back(idxev(ievent))) then
         call ev_sumback

! ---       Write data to ARM2 debugging file
         if (arm2debug) then
            write(arm2dbg,99881,err=9991) kurdat, ievent,&
            &evname(ievent), evaper(ievent),&
            &grpid(idxev(ievent)), 'BACKGROUND',&
            &ev_bgconc(ihour), grpave(idxev(ievent))
99881       format(1x,i8.8,i6,2x,a10,1x,i4,2x,a8,8x,a10,43x,&
            &2(1x,e12.5))
         end if

         go to 4567
! ---       Issue error message for writing to ARM2DEBUG file
9991     call errhdl(path,modnam,'E','520','ARM2DEBUG')
         runerr = .true.
4567     continue

      end if

   end if

   return
end

subroutine pvmrm_calc(srcs2use)
!***********************************************************************
!             PVMRM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Hourly Results for PVMRM Option
!
!        PROGRAMMER: Roger W. Brode, MACTEC, Inc. (f/k/a PES, Inc.)
!
!        DATE:    May 12, 2004
!
!        MODIFIED:   Include calls to SETSRC for all source types
!                    to correct initialization problems with PVMRM.
!                    Also include call to subroutine EMFACT to apply
!                    emission factors, if appropriate, in order to
!                    use the EMISFACT-adjusted emission rates in
!                    the calculation of the moles of NOx.
!                    Also assigned file unit 9 to variable PVMDBG
!                    for the PVMRM debug file, and adjusted the
!                    PVMRM debug output to report total PercentNO2,
!                    including in-stack contribution.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!                    Retrieve WDSIN and WDCOS for dominant source from
!                    new arrays that save values by source. Corrects
!                    potential problem for PVMRM applications with
!                    multi-level wind inputs.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!                    Removed code related to EVENT processing and
!                    EVALCART options that are not supported for
!                    PSDCREDIT option.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
!
!        INPUTS:  SRCS2USE - 'ALLSRCS' = all three PSDGROUPs
!                            'NAAQSRC' = existing baseline and
!                                        increment consuming PSDGROUPs
!                            'ALLBASE' = existing and retired baseline
!                                        PSDGROUPs
!
!        OUTPUTS:
!
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character srcs2use*7
   character modnam*12

   double precision :: maxchi(numrec,numtyp), maxconc
   double precision :: bvert,bvert3, bhoriz,bhoriz3, voldom,volsum,&
!     JAT 7/22/21 YDOM NOT USED
!     &                    XDOM, YDOM, DISTDOM, UDOM, FDOM
   &xdom, distdom, udom, fdom
   double precision :: domO3moles, sumO3moles, domNOxmoles,&
   &sumNOxmoles, domConverted, sumConverted,&
   &PercentNO2
   double precision :: ave_no2ratio, ave_no2ratio3
   double precision :: zcorr, pcorr, tcorr, ptz, tap
   double precision :: xdep, width, length, xminr, xmaxr
   double precision :: qptot
   integer :: domidx(numrec,numtyp), idom, ndxblz

!     Variable Initializations
   modnam = 'PVMRM_CALC'
   width  = 0.0d0
   length = 0.0d0
   bhoriz = 0.0d0
   bhoriz3= 0.0d0
   bvert  = 0.0d0
   bvert3 = 0.0d0

! --- Initialize AVE_NO2RATIO and other variables:
   ave_no2ratio = 0.0d0
   ave_no2ratio3= 0.0d0
   domO3moles   = 0.0d0
   sumO3moles   = 0.0d0
   domNOxmoles  = 0.0d0
   sumNOxmoles  = 0.0d0
   domConverted = 0.0d0
   sumConverted = 0.0d0
   PercentNO2   = 0.0d0

! --- Initialize MAXCHI and DOMIDX?
   maxchi = 0.0d0
   domidx = 0

!     Determine maximum values and corresponding index by receptor
   if (trim(srcs2use) .eq. 'ALLSRCS') then
!        Standard (Non-PSDCREDIT) run; use the global array operators
!        MAXVAL and MAXLOC to determine max chi val and receptor index
      maxchi = maxval(chi,dim=2)
      domidx = maxloc(chi,dim=2)
   else
!        PSDCREDIT run; use the routine AERMAX to limit which sources to
!        use in determining the dominant and major contributing sources
      call aermax(srcs2use,maxchi,domidx)
   end if

!     Begin Receptor LOOP
   receptor_loop: do irec = 1, numrec

!        Get max concentration and index for dominant source for
!        this receptor
      maxconc = maxchi(irec,1)
      idom    = domidx(irec,1)
      if (maxconc .eq. 0.0d0) then
!           No impacts, cycle to next receptor
         hrval  = 0.0d0
         aerval = 0.0d0
         cycle receptor_loop
      end if

!        Assign effective wind speed and terrain weighting factor
!        for dominant source, IDOM
      udom = ueffs(irec,idom)
      fdom = fopts(irec,idom)

!        Assign wind direction based on dominant source
      wdsin = awdsin(idom)
      wdcos = awdcos(idom)
      afv   = aafv(idom)

!        Determine extent of major contributing sources;
!        first initialize variables
      hmnt  =   1.0d10
      hmxt  =  -1.0d10
      hmnh  =   1.0d10
      hmxh  =  -1.0d10
      hmnt3 =   1.0d10
      hmxt3 =  -1.0d10
      hmnh3 =   1.0d10
      hmxh3 =  -1.0d10
      cwmin =   1.0d20
      cwmax =  -1.0d20
      cwmin3=   1.0d20
      cwmax3=  -1.0d20
      dwmin =   1.0d20
      dwmax =  -1.0d20
      dwmin3=   1.0d20
      dwmax3=  -1.0d20
      xminr =   1.0d20
      xmaxr =  -1.0d20
      width  =  0.0d0
      length =  0.0d0
      bhoriz =  0.0d0
      bhoriz3=  0.0d0
      bvert  =  0.0d0
      bvert3 =  0.0d0

      numcont = 0
! ---    Loop through sources to obtain major contributing sources
!        Consideration given as to whether this is an emission credit run
!         'ALLSRCS' = full PVMRM run
!         'NAAQSRC' = increment-consuming and non-retired baseline
!         'ALLBASE' = non-retired baseline and retired baseline
!
!        MAJOR_CONT subroutine also defines the lateral (crosswind)
!        and longitudinal (alongwind) extent of major contributing
!        sources, through CWMIN/CWMAX and DWMIN/DWMAX
      source_loop1: do isrc = 1, numsrc

         if (.not. psdcredit) then
!              Full PVMRM run
!              Included FDOM fraction in call to MAJOR_CONT (via IDOM srcid)
            call major_cont(maxconc,idom)

         else if (psdcredit .and. trim(srcs2use) .eq. 'NAAQSRC') then
!              Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
            if (psdsrctyp(isrc) .eq. 'IC' .or.&
            &psdsrctyp(isrc) .eq. 'NB') then
               call major_cont(maxconc,idom)
            else
               cycle source_loop1
            end if

         else if (psdcredit .and. trim(srcs2use) .eq. 'ALLBASE') then
!              Emission credit run: Nonretired Baseline and Retired Baseline
            if (psdsrctyp(isrc) .eq. 'NB' .or.&
            &psdsrctyp(isrc) .eq. 'RB') then
               call major_cont(maxconc,idom)
            else
               cycle source_loop1
            end if
         end if

      end do source_loop1

!        Set vertical dimensions of "box" for major cont. sources.
!        Use terrain weighting factor for dominant source (FDOM) to
!        combine heights for horizontal and terrain responding plumes.
      bvert = fdom*(hmxh - hmnh) + (1.0d0-fdom)*(hmxt - hmnt)
      if (unstab .and. ppfact(irec,idom) .gt. 0.0d0) then
         bvert3 = fdom*(hmxh3 - hmnh3) + (1.0d0-fdom)*(hmxt3 - hmnt3)
      else
         bvert3 = 0.0d0
      end if

!        Set horizontal dimensions of "box" for major cont. sources.
      if (cwmax .gt. cwmin) then
         bhoriz = cwmax - cwmin
      else
         bhoriz = 0.0d0
      end if

!        Set horizontal dimensions of "box" for major cont. sources.
!        for penetrated plumes
      if (cwmax3 .gt. cwmin3) then
         bhoriz3 = cwmax3 - cwmin3
      else
         bhoriz3 = 0.0d0
      end if

! ---    Assign temporary global source index based on dominant source
      isrc = idom

! ---    Calculate Distance from Dominant Source (and other parameters)
      if (srctyp(idom)(1:4) .eq. 'AREA' .or.&
      &srctyp(idom) .eq. 'LINE') then
! ---       Calculations for AREA source types
         call setsrc
         call emfact(qs)

         if (evonly) then
            call ardist(ievent,xdep,width,length,xminr,xmaxr)
         else
            call ardist(irec,xdep,width,length,xminr,xmaxr)
         end if
!           Assign downwind distance (XDOM) and crosswind distance (YDOM) based on
!           distance from center of AREA/LINE source
         xdom = x
!     JAT 7/22/21 YDOM NOT USED
!            YDOM = Y

! ---       Use maximum of downwind distance from center of source to
!           receptor (X) and 1m for PVMRM option
         distdom = max( x, 1.0d0 )

! ---       Apply maximum value of 4.3*SY (based on distance from center of
!           the source) for WIDTH of AREA/LINE source for the PVMRM option;
!           otherwise use full projected WIDTH of source
         if (pvmrm) then
            call adisy(distdom)
            if (4.3d0*sy .lt. width) then
               width = 4.3d0*sy
            endif
         end if

!           Calculate volume of dominant plume, passing WIDTH for BHORIZ
         call plume_vol(distdom,irec,idom,0.0d0,0.0d0,width,&
         &0.0d0,voldom)

      else if (srctyp(idom) .eq. 'OPENPIT') then
! ---       Calculations for OPENPIT sources
         call setsrc
!*          Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
         call lwind

!*          Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
         call pdepth

!*          Calculate the Fractional Size of the
!*          Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
         call ptfrac

!*          Determine the Coordinates of the Effective Pit Area
!*          in Wind Direction Coordinate System                   ---   CALL PITEFF
         call piteff

!*          Calculate the adusted Emission Rate per unit area (QEFF) for the
!*          Effective Pit Area (PITFRA)                           ---   CALL PITEMI
!*          First assign source emission rate to QPTOT to get adjusted
!*          emission rate per unit area of effective source
         qptot = qs
         call pitemi(qptot)
         call emfact(qeff)

         if (evonly) then
            call ardist(ievent,xdep,width,length,xminr,xmaxr)
         else
            call ardist(irec,xdep,width,length,xminr,xmaxr)
         end if
!           Assign downwind distance (XDOM) and crosswind distance (YDOM) based on
!           distance from center of area source
         xdom = x
!     JAT 7/22/21 YDOM NOT USED
!            YDOM = Y

! ---       Use maximum of downwind distance from center of source to
!           receptor (X) and 1m for PVMRM option
         distdom = max( x, 1.0d0 )

! ---       Apply maximum value of 4.3*SY (based on distance from center of
!           the source) for WIDTH of OPENPIT source for the new PVMRM option;
!           otherwise use full projected WIDTH of source
         if (pvmrm) then
            call adisy(distdom)
            if (4.3d0*sy .lt. width) then
               width = 4.3d0*sy
            endif
         end if

!           Calculate volume of dominant plume, passing WIDTH for BHORIZ
         call plume_vol(distdom,irec,idom,0.0d0,0.0d0,width,&
         &0.0d0,voldom)

      else
! ---       Calculations for POINT or VOLUME sources
         call setsrc
         call emfact(qs)

         if (evonly) then
            call xydist(ievent)
         else
            call xydist(irec)
         end if

         if( srctyp(idom) .eq. 'VOLUME' )then
! ---          Assign XDOM, based on X, and WIDTH for VOLUME source
            xdom  = max( x, 1.0d0 )
!              Assign WIDTH for VOLUME source based on initial sigma-y
            width = 4.3d0*asyini(idom)
         else
! ---          Assign XDOM, based on X, and WIDTH for POINT source
            xdom  = max( x, 1.0d0 )
            width = 0.0d0
         end if
! ---       Assign YDOM based on Y
!     JAT 7/22/21 YDOM NOT USED
!           YDOM = Y

! ---       Use maximum of downwind distance to receptor (X) and 1m for PVMRM option
         distdom = max( xdom, 1.0d0 )

!           Calculate volume of dominant plume
         call plume_vol(distdom,irec,idom,0.0d0,0.0d0,width,&
         &0.0d0,voldom)
      end if

      if (unstab .and. distdom .ge. xmixed) then
! ---       Set BVERT = 0.0 if X is >/= distance to uniform vertical mixing
         bvert = 0.0d0
      else
         bvert = fdom*(hmxh - hmnh) + (1.0d0-fdom)*(hmxt - hmnt)
      end if

      if (bvert .gt. 0.0d0 .or. bvert3 .gt. 0.0d0 .or.&
      &bhoriz .gt. 0.0d0 .or. bhoriz3 .gt. 0.0d0) then
!           Calculate volume of dominant plus major contributing sources
         call plume_vol(distdom,irec,idom,bvert,bvert3,&
         &bhoriz,bhoriz3,volsum)
      else
         volsum = voldom
      end if

!        Re-Assign wind direction based on dominant source, since values
!        are overwritten in SUB. PLUME_VOL (call to METINI).
      wdsin = awdsin(idom)
      wdcos = awdcos(idom)
      afv   = aafv(idom)          ! Add AAFV array

!        Correct plume volume to conditions of standard temperature
!        and pressure (Hanrahan, 1999)
!        0.028966 is the average kg/mole of air

!        First obtain a height to use in the correction
      zcorr = azs(idom) + hecntr(irec,idom)

!        Calculate ambient temperature at plume height from pot. temp. profile
      call locate(gridht, 1, mxglvl, zcorr, ndxblz)
      if (ndxblz .ge. 1) then
!----       Potential temperature
         call gintrp( gridht(ndxblz), gridpt(ndxblz),&
         &gridht(ndxblz+1), gridpt(ndxblz+1),&
         &zcorr, ptz )
      else
!           Use GRID value for lowest level
         ptz = gridpt(1)
      end if
      tap = ptz - govrcp * zcorr

      pcorr = dexp(-g * 0.028966d0 * zcorr / ( rgas * tap))
      tcorr = 273.15d0 / tap

      voldom = voldom * pcorr * tcorr
      volsum = volsum * pcorr * tcorr

! ---    Check for O3MISS and adjust calculations
      if (.not. o3miss) then
!           get moles of ozone in dominant plume
!           O3CONC expressed in ug/m3
         domO3moles= VOLdom * o3conc/(1.0d6 * 48.0d0)

!           get moles of ozone in the combined plume
         sumO3moles= VOLsum * o3conc/(1.0d6 * 48.0d0)
      else
! ---       Assign domO3moles and sumO3moles = 0.0 for full conversion
!           if O3MISS
         domO3moles= 0.0d0
         sumO3moles= 0.0d0
      end if

!        get moles of NOx in the dominant plume
!        Use molecular weight of NO2 (46) since emission calcs are based on NO2
      if (srctyp(idom) .eq. 'AREA' .or.&
      &srctyp(idom) .eq. 'AREAPOLY' .or.&
      &srctyp(idom) .eq. 'LINE') then
         domNOxmoles = qtk*xinit*yinit *&
         &(1.0d0-ano2_ratio(idom)) * distdom/(udom*46.0d0)
      else if (srctyp(idom) .eq. 'AREACIRC') then
         domNOxmoles = qtk*pi*radius(idom)*radius(idom) *&
         &(1.0d0-ano2_ratio(idom))*distdom/(udom*46.0d0)
      else if (srctyp(idom) .eq. 'OPENPIT') then
         domNOxmoles = qeff*xinit*yinit *&
         &(1.0d0-ano2_ratio(idom)) * distdom/(udom*46.0d0)
      else
! ---       Calculate domNOxmoles for POINT and VOLUME sources
         domNOxmoles = qtk * (1.0d0-ano2_ratio(idom)) *&
         &distdom/(udom * 46.0d0)
      end if

!        get moles of NOx in the merged plume
!        Sum emissions for sources within projected width of major
!        contributing sources
      qsum   = 0.0d0
      qsum3  = 0.0d0
      sum_no2rat  = 0.0d0
      sum3_no2rat = 0.0d0

      source_loop2: do isrc = 1, numsrc
! ---       Loop through sources to determine the moles of NOx emitted by all
!           sources located within the box defined laterally and vertically
!           by the major contributing sources

         if (.not. psdcredit) then
!              Full PVMRM run
            call moles_nox(idom)

         else if (psdcredit .and. trim(srcs2use) .eq. 'NAAQSRC') then
!              Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
            if (psdsrctyp(isrc) .eq. 'IC' .or.&
            &psdsrctyp(isrc) .eq. 'NB') then
               call moles_nox(idom)
            else
               cycle source_loop2
            end if

         else if (psdcredit .and. trim(srcs2use) .eq. 'ALLBASE') then
!              Emission credit run: Nonretired Baseline and Retired Basline
            if (psdsrctyp(isrc) .eq. 'NB' .or.&
            &psdsrctyp(isrc) .eq. 'RB') then
               call moles_nox(idom)
            else
               cycle source_loop2
            end if
         end if

      end do source_loop2

!----    Check for QSUM = 0.0 error (i.e., dominant source not inc. in sum)
!        This condition should never be encountered.
      if (qsum .eq. 0.0d0 .and. qsum3 .eq. 0.0d0) then
! ---      Issue warning messages for cases with QSUM = 0.0
         if (evonly) then
            if (ievent .le. 999) then
               write(dummy,'(I3.3,1X,I8.8)') ievent, kurdat
            else if (ievent .le. 9999) then
               write(dummy,'(I4.4,I8.8)') ievent, kurdat
            else
               write(dummy,'(''000'',1X,I8.8)') kurdat
            end if
            call errhdl(path,modnam,'W','412',evname(ievent))
         else
            if (irec .le. 999) then
               write(dummy,'(I3.3,1X,I8.8)') irec, kurdat
            else if (irec .le. 9999) then
               write(dummy,'(I4.4,I8.8)') irec, kurdat
            else
               write(dummy,'(''000'',1X,I8.8)') kurdat
            end if
            call errhdl(path,modnam,'W','411',dummy)
         end if
!          Assign 0.0 to AVE_NO2RATIO
         ave_no2ratio = 0.0d0
! ---      Assign 0.0 to HRVAL and AERVAL, CYCLE RECEPTOR_LOOP unless
!          this is the last receptor, otherwise RETURN
         hrval(:)  = 0.0d0
         aerval(:) = 0.0d0
         if (irec .lt. numrec) then
            cycle receptor_loop
         else
            return
         end if
      end if

!        Calculate NOx moles for combined plume, and average in-stack ratio
!        Use molecular weight of NO2 (46) since emission calcs are based on NO2;
!        Use XDOM (downwind distance), which better reflects transport time from
!        source to receptor, instead of DISTDOM (radial distance)
      if( qsum .gt. 0.0d0 )then
         ave_no2ratio = sum_no2rat/qsum
      else
         ave_no2ratio = 0.0d0
      endif

      if( qsum3 .gt. 0.0d0 )then
         ave_no2ratio3 = sum3_no2rat/qsum3
      else
         ave_no2ratio3 = 0.0d0
      endif

      sumNOxmoles = qsum * (1.0d0-ave_no2ratio) *&
      &distdom/(udom * 46.0d0)&
      &+ qsum3* (1.0d0-ave_no2ratio3) *&
      &distdom/(udom * 46.0d0)

! ---    Calculate NOx conversion ratios for dominant plume and combined plume;
!        excluding in-stack NO2/NOx ratio(s)
!        Ensure that conversion ratios do not exceed 1.0
      if( domNOxmoles .gt. 0.0d0 )then
         domConverted = min( domO3moles/domNOxmoles, 1.0d0 )
      else
         domConverted = 0.0d0
      endif
      if( sumNOxmoles .gt. 0.0d0 )then
         sumConverted = min( sumO3moles/sumNOxmoles, 1.0d0 )
      else
         sumConverted = 0.0d0
      endif

! ---    Find which is more important -- the dominant plume or the combined plume;
!        also include appropriate in-stack NO2/NOx contribution.
!        Select the minimum conversion since the conversion for combined plumes
!        should not be any larger than conversion for the single dominant plume.
! ---    Set PercentNO2 = 1.0 if O3MISS, subject to NO2Equil
      if (o3miss) then
         PercentNO2 = 1.0d0
      else
! ---       Calculate PercentNO2 based on the lessor of domConverted or
!           sumConverted (accounting for in-stack NO2 ratios)
         if ( (domConverted + ano2_ratio(idom)) .le.&
         &(sumConverted + ave_no2ratio) ) then
            PercentNO2 = domConverted + ano2_ratio(idom)
         else
            PercentNO2 = sumConverted + ave_no2ratio
         end if
      end if

! ---    Limit conversion to equilibrium concentration of NO2 (default set at 90 percent)
      if (PercentNO2 .gt. NO2Equil) PercentNO2 = NO2Equil

!        Update HRVAL, AVEVAL and ANNVAL Arrays                         ! jop, 9/30/06
      if (.not. psdcredit) then
! ---      Non-PSDCREDIT run; loop through srcgrps and sources for this receptor and
!          this hour or event; also output results to PVMRM debug files if needed

         group_loop: do igrp = 1, numgrp

            source_loop3: do isrc = 1, numsrc
! ---          Loop through sources again, and apply PercentNO2 to the hourly
!              values before summing the group/event values
               do ityp = 1, numtyp
                  hrval(ityp) = chi(irec,isrc,ityp) * PercentNO2
!! Added for TTRM2
!! Perform comparison of calculated hourly NO2 values between PVMRM and TTRM
                  if (runttrm2) then
                     if(ttrm2dbg .and. (cmeth .lt. 3)) then
                        write(ttrm2tmp(2),pstfrm,err=47) axr(irec), ayr(irec),&
                        &hrval(1),&
                        &azelev(irec), azhill(irec), azflag(irec),&
                        &chrave(1), grpid(igrp), kurdat, srcid(isrc)
                        go to 847
47                      write(dummy,'("PSTFL",I4.4)') ttrm2tmp(2)
                        call errhdl(path,modnam,'E','520',dummy)
                        runerr = .true.
847                     continue
                     endif
                     if (ttrmcompare(igrp,isrc,irec,ityp) .lt. hrval(ityp)) then
                        hrval(ityp) = ttrmcompare(igrp,isrc,irec,ityp)
                     endif
                     if (ttrm2dbg .and. (cmeth .lt. 3)) then
                        write(ttrm2tmp(3),pstfrm,err=37) axr(irec), ayr(irec),&
                        &hrval(1),&
                        &azelev(irec), azhill(irec), azflag(irec),&
                        &chrave(1), grpid(igrp), kurdat, srcid(isrc)
                        go to 837
37                      write(dummy,'("PSTFL",I4.4)') ttrm2tmp(3)
                        call errhdl(path,modnam,'E','520',dummy)
                        runerr = .true.
837                     continue
                     endif
                  endif
!! End of TTRM2 insert; Nov. 2021
               end do

               if (evonly) then
!                 Sum HRVAL to EV_AVEVAL, GRPVAL and GRPAVE Arrays  ---   CALL EV_SUMVAL

                  if (idxev(ievent) .eq. igrp .and.&
                  &igroup(isrc,igrp) .eq. 1) then
                     call ev_sumval

! ---                Write data to PVMRM debug output file
!                    Check whether dominant plume or combined plume is controlling
                     if (pvmrmdbg) then
                        if ( (domConverted + ano2_ratio(idom)) .le.&
                        &(sumConverted + ave_no2ratio) ) then
                           write(pvmdbg,9987) 'DOM: ',srcid(idom), kurdat,&
                           &ievent, evname(ievent), evaper(ievent),&
                           &grpid(idxev(ievent)),isrc, srcid(isrc),&
                           &numcont, distdom, maxconc, o3conc,&
                           &domo3moles, domnoxmoles, bhoriz, bvert,&
                           &bvert3, voldom, chi(1,isrc,1), percentno2,&
                           &hrval(1), grpave(idxev(ievent))
                        else
                           write(pvmdbg,9987) 'SUM: ',srcid(idom), kurdat,&
                           &ievent, evname(ievent), evaper(ievent),&
                           &grpid(idxev(ievent)),isrc, srcid(isrc),&
                           &numcont, distdom, maxconc, o3conc,&
                           &sumo3moles, sumnoxmoles, bhoriz, bvert,&
                           &bvert3, volsum, chi(1,isrc,1), percentno2,&
                           &hrval(1), grpave(idxev(ievent))
                        endif
                     endif
9987                 format(1x,a5,1x,a12,2x,i8.8,1x,i5,2x,a12,1x,&
                     &i5,2x,a8,1x,i5,2x,a12,1x,i4,13(2x,e12.5))

                  end if

               else if (.not. evonly) then
! ---             Sum HRVAL to AVEVAL, ANNVAL and SHVALS Arrays     ---   CALL SUMVAL

                  if (igroup(isrc,igrp) .eq. 1) then
                     call sumval

! ---               Write data to PVMRM debug output file
!                   Check whether dominant plume or combined plume is controlling
                     if (pvmrmdbg) then
                        if ( (domConverted + ano2_ratio(idom)) .le.&
                        &(sumConverted + ave_no2ratio) ) then
                           write(pvmdbg,99871) 'DOM: ',srcid(idom), kurdat,&
                           &irec, grpid(igrp), isrc, srcid(isrc),&
                           &numcont, distdom, maxconc, o3conc,&
                           &domo3moles, domnoxmoles, bhoriz, bvert,&
                           &bvert3,&
                           &voldom, chi(irec,isrc,1), percentno2,&
                           &hrval(1), aveval(irec,igrp,1,1)
                        else
                           write(pvmdbg,99871) 'SUM: ',srcid(idom), kurdat,&
                           &irec, grpid(igrp), isrc, srcid(isrc),&
                           &numcont, distdom, maxconc, o3conc,&
                           &sumo3moles, sumnoxmoles, bhoriz, bvert,&
                           &bvert3,&
                           &volsum, chi(irec,isrc,1), percentno2,&
                           &hrval(1), aveval(irec,igrp,1,1)
                        end if
99871                   format(1x,a5,1x,a12,2x,i8.8,1x,i5,2x,a8,1x,i5,&
                        &2x,a12,2x,i5,2x,13(2x,e12.5))

                     end if
                  end if
               end if
               if (eval(isrc)) then
!                 Check ARC centerline values for EVALFILE output   ---   CALL EVALCK
                  call evalck
               end if

            end do source_loop3

            if (evonly) then
! --          Include BACKGROUND contributions in PVMRM debug files
!             in needed

               if (idxev(ievent) .eq. igrp .and.&
               &grp_back(idxev(ievent)) )then
! ---            This SRCGROUP includes BACKGROUND; Call EV_SUMBACK which
!                sums values for the current EVENT
                  call ev_sumback
                  if (pvmrmdbg) then
                     if ( (domConverted + ano2_ratio(idom)) .le.&
                     &(sumConverted + ave_no2ratio) ) then
                        write(pvmdbg,99872) 'DOM: ',srcid(idom), kurdat,&
                        &ievent, evname(ievent), evaper(ievent),&
                        &grpid(idxev(ievent)), 'BACKGROUND',&
                        &ev_bgconc(ihour), grpave(idxev(ievent))

                     else
                        write(pvmdbg,99872) 'SUM: ', srcid(idom), kurdat,&
                        &ievent, evname(ievent), evaper(ievent),&
                        &grpid(idxev(ievent)), 'BACKGROUND',&
                        &ev_bgconc(ihour), grpave(idxev(ievent))
                     end if
                  end if
99872             format(1x,a5,1x,a12,2x,i8.8,i6,4x,a10,i6,2x,a8,&
                  &8x,a10,161x,2(2x,e12.5))
               end if

            elseif (.not. evonly) then

               if (grp_back(igrp)) then
! ---             This SRCGROUP includes BACKGROUND; Call SUMBACK_NO2 which
!                 sums values for the current receptor only, whereas sub SUMBACK
!                 sums values across the full receptor network
                  call sumback_no2
                  if (pvmrmdbg) then
                     if ( (domConverted + ano2_ratio(idom)) .le.&
                     &(sumConverted + ave_no2ratio) ) then
                        write(pvmdbg,99873) 'DOM: ',srcid(idom), kurdat,&
                        &irec, grpid(igrp), 'BACKGROUND',&
                        &bgconc, aveval(irec,igrp,1,1)
                     else
                        write(pvmdbg,99873) 'SUM: ',srcid(idom), kurdat,&
                        &irec, grpid(igrp), 'BACKGROUND',&
                        &bgconc, aveval(irec,igrp,1,1)
                     end if
                  end if
99873             format(1x,a5,1x,a12,2x,i8.8,i6,2x,a8,8x,a10,165x,&
                  &2(2x,e12.5))
               end if

            end if

         end do group_loop

      else if (psdcredit .and. trim(srcs2use) .eq. 'NAAQSRC') then
!           IC = increment consuming source
!           NB = nonretired baseline source
!           Use ABVAL to store combined results rather than HRVAL
         do isrc = 1, numsrc
            do ityp = 1, numtyp
               if( psdsrctyp(isrc) .eq. 'IC' .or.&
               &psdsrctyp(isrc) .eq. 'NB' )then
                  abval(irec,ityp) = abval(irec,ityp) +&
                  &chi(irec,isrc,ityp) * PercentNO2
               end if
            end do
         end do

!           Sum ABVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVALPSD
         call sumvalpsd(srcs2use)

      else if (psdcredit .and. trim(srcs2use) .eq. 'ALLBASE') then
!           NB = nonretired baseline source
!           RB =    retired baseline source
!           Use BCVAL to store combined results rather than HRVAL
         do isrc = 1, numsrc
            do ityp = 1, numtyp
               if( psdsrctyp(isrc) .eq. 'NB' .or.&
               &psdsrctyp(isrc) .eq. 'RB' )then
                  bcval(irec,ityp) = bcval(irec,ityp) +&
                  &chi(irec,isrc,ityp) * PercentNO2
               end if
            end do
         end do

!           Sum ABVAL-BCVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVALPSD
         call sumvalpsd(srcs2use)

      end if

!        Initialize __VAL arrays (1:NUMTYP)
      hrval   = 0.0d0
      aerval  = 0.0d0

   end do receptor_loop
!     End Receptor LOOP

   return
end

subroutine major_cont(maxconc,idom)
!***********************************************************************
!             MAJOR_CONT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Identify major contributing souces for PVMRM option,
!                 based on source impacts at the receptor of at least
!                 50% of the dominant source impact.  Also calculates
!                 the lateral and vertical range of the plumes from the
!                 major contributing sources.
!
!        PROGRAMMER: James Paumier, MACTEC FPI
!
!        DATE:    September 30, 2006
!
!        MODIFIED:   Include calls to SETSRC for all sources types
!                    to correct initialization problems with PVMRM.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFICATIONS:
!           Moved from SUBROUTINE PVMRM_CALC to be able to apply
!           emission credit calculations more effectively
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   PVMRM_CALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12

! --- Define threshold for major contributing sources,
!     based on sources which contribute at least 0.5 times
!     the contribution from the dominant source, for each
!     receptor
   double precision, parameter :: majcont_thresh = 0.5d0

   double precision :: maxconc, width, length, xminr, xmaxr
   double precision :: xdep
! Unused: DOUBLE PRECISION :: XAREA

   integer          :: idom

!     Variable Initializations
   modnam = 'MAJOR_CONT'

   if (chi(irec,isrc,1) .ge. majcont_thresh*maxconc) then
! ---    This is a major contributing source
      if (srctyp(isrc)(1:4) .eq. 'AREA' .or.&
      &srctyp(isrc) .eq. 'LINE') then
         call setsrc
!           Calls to ARDIST modified to include XMINR, minimum downwind distance
!           from the source to the receptor
         if (evonly) then
            call ardist(ievent,xdep,width,length,xminr,xmaxr)
         else
            call ardist(irec,xdep,width,length,xminr,xmaxr)
         end if

! ---       Check for receptor > MAXDIST from source
         if( distr .gt. maxdist )then
            return
         else
            numcont = numcont + 1
!              Store max and min values of crosswind and downwind distances
            cwmax = max( cwmax, y )
            cwmin = min( cwmin, y )
            dwmax = max( dwmax, x )
            dwmin = min( dwmin, x )
!              Apply lower limit of 1.0 on DWMIN and DWMAX for PVMRM option
            if( dwmax .lt. 1.0d0 ) dwmax = 1.0d0
            if( dwmin .lt. 1.0d0 ) dwmin = 1.0d0
         endif
      else if (srctyp(isrc) .eq. 'OPENPIT') then
         call setsrc
!*          Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
         call lwind

!*          Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
         call pdepth

!*          Calculate the Fractional Size of the
!*          Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
         call ptfrac

!*          Determine the Coordinates of the Effective Pit Area
!*          in Wind Direction Coordinate System                   ---   CALL PITEFF
         call piteff

         if (evonly) then
            call ardist(ievent,xdep,width,length,xminr,xmaxr)
         else
            call ardist(irec,xdep,width,length,xminr,xmaxr)
         end if

         if( distr .gt. maxdist )then
            return
         else
            numcont = numcont + 1
!              Store max and min values of crosswind and downwind distances
! ---          Use distance from center of source for PVMRM option
            cwmax = max( cwmax, y )
            cwmin = min( cwmin, y )
            dwmax = max( dwmax, x )
            dwmin = min( dwmin, x )
!              Apply lower limit of 1.0 on DWMIN and DWMAX for PVMRM option
            if( dwmax .lt. 1.0d0 ) dwmax = 1.0d0
            if( dwmin .lt. 1.0d0 ) dwmin = 1.0d0
         endif
      else
! ---       POINT or VOLUME Source
         call setsrc
         if (evonly) then
            call xydist(ievent)
         else
            call xydist(irec)
         end if

         if( distr .gt. maxdist )then
            return

         else
            numcont = numcont + 1
!              Store max and min values of crosswind and downwind distances
            cwmax = max( cwmax, y )
            cwmin = min( cwmin, y )
            dwmax = max( dwmax, x )
            dwmin = min( dwmin, x )
!              Apply lower limit of 1.0 on DWMIN and DWMAX for PVMRM option
            if( dwmax .lt. 1.0d0 ) dwmax = 1.0d0
            if( dwmin .lt. 1.0d0 ) dwmin = 1.0d0
            if (ppfact(irec,isrc) .gt. 0.0d0) then
!                 Include lateral extent of penetrated plume
               cwmax3 = max( cwmax3, y )
               cwmin3 = min( cwmin3, y )
               dwmax3 = max( dwmax3, x )
               dwmin3 = min( dwmin3, x )
!                 Apply lower limit of 1.0 on DWMIN3 and DWMAX3 for PVMRM option
               if( dwmax3 .lt. 1.0d0 ) dwmax3 = 1.0d0
               if( dwmin3 .lt. 1.0d0 ) dwmin3 = 1.0d0
            endif
         endif

      end if

!        Assign receptor height above stack base for dominant source
      if (l_flatsrc(idom)) then
         zrt = zflag
      else
         zrt = zelev - azs(idom) + zflag
      end if

!        Check min/max plume height ranges for terrain-responding (HMNT/HMXT)
!        and horizontal (HMNH/HMXH) plumes
      if (hecntr(irec,isrc) .lt. hmnt) then
         hmnt = hecntr(irec,isrc)
      endif
      if (hecntr(irec,isrc) .gt. hmxt) then
         hmxt = hecntr(irec,isrc)
      endif

      if ( (hecntr(irec,isrc)-zrt) .lt. hmnh) then
         hmnh = hecntr(irec,isrc) - zrt
      endif
      if ( (hecntr(irec,isrc)-zrt) .gt. hmxh) then
         hmxh = hecntr(irec,isrc) - zrt
      endif

      if (ppfact(irec,isrc) .gt. 0.0d0) then
         if (hecntr3(irec,isrc).lt.hmnt3) then
            hmnt3 = hecntr3(irec,isrc)
         endif
         if (hecntr3(irec,isrc).gt.hmxt3) then
            hmxt3 = hecntr3(irec,isrc)
         endif

         if ( (hecntr3(irec,isrc)-zrt) .lt. hmnh3) then
            hmnh3 = hecntr3(irec,isrc) - zrt
         endif
         if ( (hecntr3(irec,isrc)-zrt) .gt. hmxh3) then
            hmxh3 = hecntr3(irec,isrc) - zrt
         endif

      end if
   end if

   return
end

subroutine moles_nox(idom)
!***********************************************************************
!             MOLES_NOX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates the moles of NOx in the combined plume
!
!        PROGRAMMER: James Paumier, MACTEC FPI
!
!        DATE:    September 30, 2006
!
!        MODIFIED:   Include calls to SETSRC for all sources types
!                    to correct initialization problems with PVMRM.
!                    Also include call subroutine EMFACT to apply
!                    emission factors, if appropriate, in order to
!                    use the EMISFACT-adjusted emission rates in
!                    the calculations of the moles of NOx.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFICATIONS:
!           Moved from SUBROUTINE PVMRM_CALC to be able to apply
!           emission credit calculations more effectively
!
!        INPUTS:
!
!
!        OUTPUTS:
!            QSUM
!            SUM_NO2RAT
!
!
!        CALLED FROM:   PVMRM_CALC
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12

   double precision, parameter :: eps = 1.0d-5
   double precision :: width, length, xminr, xmaxr
   double precision :: cwdist, dwdist
   double precision :: qarea, xdep
   double precision :: qptot, fdom
   integer          :: idom

!     Variable Initializations
   modnam = 'MOLES_NOX'

   if (srctyp(isrc)(1:4) .eq. 'AREA' .or.&
   &srctyp(isrc) .eq. 'LINE') then
! ---    Determine NOX moles for AREA, AREACIRC, AREAPOLY,
!        and LINE sources
      call setsrc
      call emfact(qs)
      if (evonly) then
         call ardist(ievent,xdep,width,length,xminr,xmaxr)
      else
         call ardist(irec,xdep,width,length,xminr,xmaxr)
      end if
!        Assign Y and X to CWDIST and DWDIST based on distance from
!        center of area source to receptor.  If center of area source
!        is within "box" of major contributing sources, then include
!        it's emissions in calculation of NOx moles.
      cwdist = y
      dwdist = max( x, 1.0d0 )
   else if (srctyp(isrc) .eq. 'OPENPIT') then
! ---    Determine NOX moles for OPENPIT sources
      call setsrc
!*       Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
      call lwind

!*       Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH
      call pdepth

!*       Calculate the Fractional Size of the
!*       Effective Pit Area (PITFRA)                           ---   CALL PTFRAC
      call ptfrac

!*       Determine the Coordinates of the Effective Pit Area
!*       in Wind Direction Coordinate System                   ---   CALL PITEFF
      call piteff

!*       Calculate the adusted Emission Rate per unit area (QEFF) for the
!*       Effective Pit Area (PITFRA)                           ---   CALL PITEMI
!*       First assign source emission rate to QPTOT to get adjusted
!*       emission rate per unit area of effective source
      qptot = qs
      call pitemi(qptot)
      call emfact(qeff)

      if (evonly) then
         call ardist(ievent,xdep,width,length,xminr,xmaxr)
      else
         call ardist(irec,xdep,width,length,xminr,xmaxr)
      end if

!        Assign Y and X to CWDIST and DWDIST based on distance from
!        center of OPENPIT source to receptor.  If center of effective
!        source is within "box" of major contributing sources, then
!        include it's emissions in calculation of NOx moles.
      cwdist = y
      dwdist = max( x, 1.0d0 )
   else
! ---    Determine NOX moles for POINT and VOLUME sources
      call setsrc
      call emfact(qs)
      if (evonly) then
         call xydist(ievent)
      else
         call xydist(irec)
      end if
!        Assign Y and X to CWDIST and DWDIST based on distance from
!        center of POINT or VOLUME source to receptor.  If center of
!        source is within "box" of major contributing sources, then
!        include it's emissions in calculation of NOx moles.
      cwdist = y
      dwdist = max( x, 1.0d0 )
   end if

! --- Get FDOM based on dominant source
   fdom = fopts(irec,idom)

!     Check for crosswind distance between MIN and MAX of projected width
!     and for downwind distance between MIN and MAX of downwind distances
!     of major contributing sources
   if( (cwdist .ge. cwmin-eps .and. cwdist .le. cwmax+eps) .and.&
   &(dwdist .ge. dwmin-eps .and. dwdist .le. dwmax+eps) )then
!           Source is located within the horizontal "box" of major contributing sources;
!           Also check min/max plume height ranges for terrain-responding (HMNT/HMXT)
!           and horizontal (HMNH/HMXH) plumes
      if( hecntr(irec,isrc) .ge. hmnt-eps .and.&
      &hecntr(irec,isrc) .le. hmxt+eps )then
         if (srctyp(isrc).eq.'AREA' .or.&
         &srctyp(isrc).eq.'AREAPOLY' .or.&
         &srctyp(isrc).eq.'LINE') then
! ---           Add contribution from AREA, AREAPOLY, and LINE sources
! ---           Note that XINIT and YINIT are "effective" dimensions for
!               AREAPOLY sources that are used to define the area of the
!               source.
            qarea = qtk*xinit*yinit
            qsum  = qsum + qarea*(1.0d0-fdom)
            sum_no2rat = sum_no2rat + ano2_ratio(isrc)*qarea*&
            &(1.0d0-fdom)
         else if (srctyp(isrc) .eq. 'AREACIRC') then
! ---           Add contribution from AREACIRC sources
            qarea = qtk*pi*radius(isrc)*radius(isrc)
            qsum  = qsum + qarea*(1.0d0-fdom)
            sum_no2rat = sum_no2rat + ano2_ratio(isrc)*qarea*&
            &(1.0d0-fdom)
         else if (srctyp(isrc).eq.'OPENPIT') then
! ---           Add contribution from OPENPIT sources
            qarea = qeff*xinit*yinit
            qsum  = qsum + qarea*(1.0d0-fdom)
            sum_no2rat = sum_no2rat + ano2_ratio(isrc)*qarea*&
            &(1.0d0-fdom)
         else
! ---           Add contribution from POINT and VOLUME sources
            qsum = qsum + qtk*(1.0d0-ppfact(irec,isrc))*(1.0d0-fdom)
            sum_no2rat = sum_no2rat + ano2_ratio(isrc)*qtk*&
            &(1.0d0-ppfact(irec,isrc))*&
            &(1.0d0-fdom)
         end if
      endif

      if( (hecntr(irec,isrc)-zrt) .ge. hmnh-eps .and.&
      &(hecntr(irec,isrc)-zrt) .le. hmxh+eps )then
         if (srctyp(isrc).eq.'AREA' .or.&
         &srctyp(isrc).eq.'AREAPOLY' .or.&
         &srctyp(isrc).eq.'LINE') then
! ---           Add contribution from AREA, AREAPOLY, and LINE sources
! ---           Note that XINIT and YINIT are "effective" dimensions for
!               AREAPOLY sources that are used to define the area of the
!               source.
            qarea = qtk*xinit*yinit
            qsum  = qsum + qarea*fdom
            sum_no2rat = sum_no2rat + ano2_ratio(isrc)*qarea*fdom
         else if (srctyp(isrc) .eq. 'AREACIRC') then
! ---           Add contribution from AREACIRC sources
            qarea = qtk*pi*radius(isrc)*radius(isrc)
            qsum  = qsum + qarea*fdom
            sum_no2rat = sum_no2rat + ano2_ratio(isrc)*qarea*fdom
         else if (srctyp(isrc).eq.'OPENPIT') then
! ---           Add contribution from OPENPIT sources
            qarea = qeff*xinit*yinit
            qsum  = qsum + qarea*fdom
            sum_no2rat = sum_no2rat + ano2_ratio(isrc)*qarea*fdom
         else
! ---           Add contribution from POINT and VOLUME sources
            qsum = qsum + qtk*(1.0d0-ppfact(irec,isrc))*fdom
            sum_no2rat = sum_no2rat + ano2_ratio(isrc)*qtk*&
            &(1.0d0-ppfact(irec,isrc))*fdom
         end if
      endif
   endif

   if( (cwdist .ge. cwmin3-eps .and. cwdist .le. cwmax3+eps) .and.&
   &(dwdist .ge. dwmin3-eps .and. dwdist .le. dwmax3+eps) )then
      if( hecntr3(irec,isrc) .ge. hmnt3-eps .and.&
      &hecntr3(irec,isrc) .le. hmxt3+eps )then
         if( srctyp(isrc)(1:5) .eq. 'POINT' )then
! ---            Add contribution from POINT and VOLUME sources
            qsum3 = qsum3 + qtk*ppfact(irec,isrc)*(1.0d0-fdom)
            sum3_no2rat = sum3_no2rat + ano2_ratio(isrc)*qtk*&
            &ppfact(irec,isrc)*(1.0d0-fdom)
         end if
      endif

      if( (hecntr3(irec,isrc)-zrt) .ge. hmnh3-eps .and.&
      &(hecntr3(irec,isrc)-zrt) .le. hmxh3+eps )then
         if( srctyp(isrc)(1:5) .eq. 'POINT' )then
! ---            Add contribution from POINT and VOLUME sources
            qsum3 = qsum3 + qtk*ppfact(irec,isrc)*fdom
            sum3_no2rat = sum3_no2rat + ano2_ratio(isrc)*qtk*&
            &ppfact(irec,isrc)*fdom
         end if
      endif

   end if

   return
end

subroutine plume_vol(xarg,irdx,isdx,bvarg,bvarg3,&
&bharg,bharg3,volout)
!***********************************************************************
!             PLUME_VOL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates plume volume for PVMRM option
!
!        PROGRAMMER: Roger W. Brode, PES, Inc.
!
!        DATE:    May 6, 2002
!
!        MODIFICATIONS:
!
!               Modified to add Aircraft's plume rise for
!               Volume/Area Source only for Aircraft Source Group
!               Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA,
!               04/01/2023
!
!               Modified XTABLE array of distances to use a more
!               logical progression of distance intervals and to
!               reduce the number of distances used.
!               R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!               Added call to PENFCT to calculate plume penetration
!               factor.
!               R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!               Remove calls to VDP and SCAVRAT.
!               R. Brode, MACTEC (f/k/a PES), Inc. - 08/02/05
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   PVMRM_CALC
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   integer, parameter :: ntab = 59
!---  Declare parameters for the number of sigma-z's to define the volume of
!     of the plume for stable (NSUMZS) and unstable (NSUMZU) conditions,
!     and for the minimum values of sigma-r for stable (SRMINS) and unstable
!     (SRMINU), to distinguish between total dispersion coefficents (used for
!     stable conditions) and relative dispersion (used for unstable conditions).

! --- Declare NSUBZS (number of sigma's to define the plume volume) and SRMINS (minimum value of sigma-r)
!     for STABLE conditions based on use of "total" dispersion
   double precision, parameter :: nsubzs = 1.282d0, srmins = 15.0d0         ! NSUBZS = approx 80% of plume

! --- Declare NSUBZU (number of sigma's to define the plume volume) and
!     SRMINU (minimum value of sigma-r) for UNSTABLE conditions under the new PVMRM option:
   double precision, parameter :: nsubzu = 2.58d0,  srminu =  9.0d0         ! NSUBZU = approx 99% of plume

   double precision ::  nsubz, srmin, srmin3
!     JAT 7/22/21 DO65 FRAN3 NOT USED
!      DOUBLE PRECISION ::  FRAN, FRAN3
   double precision ::  fran
   double precision ::  xarg, bvarg, bvarg3, bharg, bharg3,&
   &vert, vert3, volout, sum, sum3
   double precision ::  xtable(ntab), sr(ntab), sr3(ntab), sigr,&
!     JAT 7/22/21 DO65 SZTMP NOT USED
!     &                     SIGR3, SZTMP
   &sigr3
   double precision ::  xtab, xinp, deltax, bvtmp, sigrztmp
   integer ::  i, isdx, irdx
   integer ::  kiter, ndxzpl
   double precision :: hsprim, zplm, dhfold, svpm, upm, tgpm, ptpm,&
   &ptp !, SVPM2
   double precision :: vseq
   double precision :: swpm                           ! Added for ARISE; UNC-IE

! --- Variable Initializations:
!     Distance table for plume volume calculation, XTABLE
   data xtable/&
   &10.d0,20.d0,30.d0,40.d0,50.d0,60.d0,70.d0,80.d0,90.d0,100.d0,&
   &120.d0,140.d0,160.d0,180.d0,200.d0,&
   &250.d0,300.d0,350.d0,400.d0,450.d0,500.d0,&
   &600.d0,700.d0,800.d0,900.d0,1000.d0,&
   &1200.d0,1400.d0,1600.d0,1800.d0,2000.d0,&
   &2500.d0,3000.d0,3500.d0,4000.d0,4500.d0,5000.d0,&
   &6000.d0,7000.d0,8000.d0,9000.d0,10000.d0,&
   &12000.d0,14000.d0,16000.d0,18000.d0,20000.d0,&
   &25000.d0,30000.d0,35000.d0,40000.d0,45000.d0,50000.d0,&
   &60000.d0,70000.d0,80000.d0,90000.d0,100000.d0,&
   &500000.d0/

   data sr/ntab*0.0d0/, sr3/ntab*0.0d0/

   modnam = 'PLUME_VOL'

!     Assign source index to global variable
   isrc   = isdx
   irec   = irdx
   sum    = 0.0d0
   sum3   = 0.0d0
! --- Initialize SR and SR3 arrays
   sr(:)  = 0.0d0
   sr3(:) = 0.0d0
   sigr   = 0.0d0
   sigr3  = 0.0d0
!     JAT 7/22/21 DO65 SZTMP NOT USED
!      SZTMP  = 0.0D0

   vert   = 0.0d0
   vert3  = 0.0d0

   fran   = 0.0d0
!     JAT 7/22/21 DO65 FRAN3 NOT USED
!      FRAN3  = 0.0D0

! --- PVMRM option uses "total" dispersion coefficients for
!     STABLE conditions, including injected plumes (UNSTAB .and. HS < ZI),
!     and uses "relative" dispersion coefficients for UNSTABLE conditions.
!     Assign SRMIN and NSUBZ based on stability:
!     Use HS based on ISDX
   if (stable .or. (unstab .and. (ahs(isdx) .ge. zi))) then
! ---    Assign SRMIN and NSUBZ for STABLE or UNSTAB and HS .ge. ZI
      srmin = srmins
      nsubz = nsubzs
   else
! ---    Assign SRMIN and NSUBZ for UNSTAB and HS .lt. ZI
      srmin = srminu
      nsubz = nsubzu
   end if

   if( pvmrmdbg .and. debug .and. kurdat .gt. 0 )then
! ---    PVMRM and MODEL DEBUG options have been selected; include the
!        RELDISP Calcs in the MODEL DEBUG file
      write(rdispunt,*)
      write(rdispunt,*) 'RELDISP Calcs: '
      if( evonly )then
         write(rdispunt,*) ' DATE: ' ,kurdat,'  IEVT: ',ievent
      else
         write(rdispunt,*) ' DATE: ' ,kurdat,'  IREC: ',irec
      endif
      write(rdispunt,*)&
      &'  DOM SRCID   IDX      DIST       SR(IDX)      SR3(IDX)',&
      &'      SIGR        SRMIN'
   endif

!     Set Mixing Height and Profiles for Urban Option if Needed
   if (urbsrc(isrc) .eq. 'Y') then
!        Find Urban Area Index for This Source
      do i = 1, numurb
         if (iurbgrp(isrc,i) .eq. 1) then
            iurb = i
            exit
         end if
      end do
      if (stable .or. L_MorningTrans(iurb)) then
         urbstab = .true.
         zi = max( ziurb(iurb), zimech )
         gridsv = grdsvu(1:mxglvl,iurb)
         gridsw = grdswu(1:mxglvl,iurb)
         gridtg = grdtgu(1:mxglvl,iurb)
         gridpt = grdptu(1:mxglvl,iurb)
         obulen = urbobulen(iurb)
         ustar  = urbustr(iurb)
      else
         urbstab = .false.
         zi = zirur
         gridsv = grdsvr
         gridsw = grdswr
         gridtg = grdtgr
         gridpt = grdptr
         obulen = rurobulen
         ustar  = rurustr
      end if
   else if (urban .and. urbsrc(isrc) .eq. 'N') then
      urbstab = .false.
      zi = zirur
      gridsv = grdsvr
      gridsw = grdswr
      gridtg = grdtgr
      gridpt = grdptr
      obulen = rurobulen
      ustar  = rurustr
   else
! ---    Rural
      urbstab = .false.
   end if

!     Set the Source Variables for This Source           ---   CALL SETSRC
   call setsrc

!     Calculate the initial meteorological variables     ---   CALL METINI
   call metini

   if ((srctyp(isrc) .eq. 'VOLUME' .and. aftsrc(isrc) .eq. 'N').or.&  ! Added for Aircraft; UNC-IE
   &srctyp(isrc) .eq. 'LINE' .or.&
   &(srctyp(isrc)(1:4) .eq. 'AREA'.and.aftsrc(isrc).eq.'N') .or.&  ! Added for Aircraft; UNC-IE
   &srctyp(isrc) .eq. 'OPENPIT') then
      fb  = 0.0d0
      fm  = 0.0d0
      ppf = 0.0d0
      hsp = hs
      dhp  = 0.0d0
      dhp1 = 0.0d0
      dhp2 = 0.0d0
      dhp3 = 0.0d0
      dhcrit = 0.0d0
      xfinal = 0.0d0
      xmixed = zi * uavg / swavg
      if(xmixed .lt. xfinal) xmixed = xfinal
      zmidmx = 0.5d0 * zi
!        Define temporary values of CENTER and SURFAC based on HS
!        to account for non-POINT sources
      center = hecntr(irec,isrc)
      if( center .lt. 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

!**  Added for Aircraft Plume Rise; UNC-IE
   else if (srctyp(isrc) .eq. 'VOLUME' .or.&
   &srctyp(isrc) .eq. 'AREA'  .and.&
   &aftsrc(isrc) .eq. 'Y') then
!        Calculate Buoyancy Flux                            ---   CALL AFLUXES
      call afluxes
      fm  = 0.0d0

!        Define temporary values of CENTER and SURFAC based on HS
      center = hecntr(irec,isrc)
      if( center .lt. 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

      hsp = hs
!        Calculate Distance to Final Rise                   ---   CALL ADISTF
      call adistf

!        Calculate the plume penetration factor             ---   CALL PENFCT
      call penfct

      if (stable .or. (unstab.and.(hs.ge.zi))) then
!           Use iterative approach to stable plume rise calculations
         kiter = 0
150      zplm = hsp + 0.5d0 * dhfaer
         dhfold = dhfaer

!----       Locate index below ZPLM

         call locate(gridht, 1, mxglvl, zplm, ndxzpl)

!----       Get Wind speed at ZPLM; replace UP, SVP, SWP.  Also, replace TGP,
!           vertical potential temperature gradient, if stable.

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


!RWB        Use average of stack top and midpoint wind speeds and others.
         up = 0.5d0 * (us + upm)
         svp = 0.5d0 * (svs + svpm)
         swp = 0.5d0 * (sws + swpm)

         call gintrp( gridht(ndxzpl), gridtg(ndxzpl),&
         &gridht(ndxzpl+1), gridtg(ndxzpl+1), zplm, tgpm )
         call gintrp( gridht(ndxzpl), gridpt(ndxzpl),&
         &gridht(ndxzpl+1), gridpt(ndxzpl+1), zplm, ptpm )
!RWB        Use average of stack top and midpoint temperature gradients.
         tgp = 0.5d0 * (tgs + tgpm)
         ptp = 0.5d0 * (pts + ptpm)
         bvf = dsqrt( g * tgp / ptp )
         if(bvf .lt. 1.0d-10) bvf = 1.0d-10
         bvprim  = 0.7d0 * bvf

         call adistf

         kiter = kiter + 1

!           Check for convergence
         if(dabs((dhfold - dhfaer)/dhfaer) .lt. 0.01d0) go to 160

         if(kiter .ge. 5) then
            dhfaer = 0.5d0 * (dhfaer + dhfold)
            go to 160
         else
            go to 150
         end if

160      continue

!RWB        After completing iteration, reset UP, SVP, SWP and TGP to stack top
!RWB        values for subsequent distance-dependent plume rise calcs.
         up = us
         svp = svs
         swp = sws
         tgp = tgs
         ptp = pts
         bvf = dsqrt( g * tgp / ptp )
         if(bvf .lt. 1.0d-10) bvf = 1.0d-10
         bvprim  = 0.7d0 * bvf
      end if

!        Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
      prm_fstrec = .true.

      zmidmx = 0.5d0 * zi

!RJP
!RJP     Calculate distance to uniformly mixed plume within the
!RJP     boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
!RJP     distance is approximately (Zi * UAVG)/SWAVG, where UAVG
!RJP     and SWAVG are wind speed and sigma-w averaged over the depth
!RJP     between the ground and Zi (or the plume height, if higher in
!RJP     stable conditions); this height is denoted as 2 * ZMIDMX.
!RJP
!RJP     First, get refined estimate of final rise and distance to final
!RJP     rise if downwash conditions prevail.
!RJP
      xfinal = xmax
      dhcrit = dhfaer
      xmixed = zi * uavg / swavg
      if (unstab .and. hs.lt.zi) then
!           Check for XMIXED smaller than 1.25*XFINAL
         if (xmixed .lt. 1.25d0*xfinal) then
            xfinal = 0.8d0 * xmixed
            call acblprd (xfinal)
            dhcrit = dhp1
         end if
      end if
!**  End Aircraft Plume Rise insert; April 2023

   else if (srctyp(isrc)(1:5) .eq. 'POINT') then
!        Calculate Buoyancy and Momentum Fluxes             ---   CALL FLUXES
      call fluxes(vseq)

!        Set Wake and Building Type Switches                ---   CALL WAKFLG
! ---    NOTE:  WAKFLG sets building dimensions based on wind
!        direction at stack top.
! ---    WAKE is set to false for purposes of calculating plume volume,
!        since minimum volume is intended
      wake = .false.

!        Define temporary values of CENTER and SURFAC based on HS
      center = hecntr(irec,isrc)
      if( center .lt. 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

! ---    Apply HSP for POINTCAP and POINTHOR first to avoid NOSTD option
!        overriding POINTCAP
      if (srctyp(isrc) .eq. 'POINTCAP') then
!           Apply stack-tip downwash for capped stacks with VS = 0.001m/s
         hsp = hsprim ( us, vseq, hs, ds )
      else if (srctyp(isrc) .eq. 'POINTHOR') then
!           Do not apply stack-tip downwash for horizontal releases
         hsp = hs
      else if( nostd )then
!           No stack-tip downwash, no adjustments necessary
         hsp = hs
      else
!           Make adjustments for stack-tip downwash
         hsp = hsprim ( us, vs, hs, ds )
      end if

!        Calculate Distance to Final Rise                   ---   CALL DISTF
      call distf

!        Calculate the plume penetration factor             ---   CALL PENFCT
      call penfct

      if (stable .or. (unstab.and.(hs.ge.zi))) then
!           Use iterative approach to stable plume rise calculations
         kiter = 0
50       zplm = hsp + 0.5d0 * dhfaer
         dhfold = dhfaer

!----       Locate index below ZPLM

         call locate(gridht, 1, mxglvl, zplm, ndxzpl)

!----       Get Wind speed at ZPLM; replace UP.  Also, replace TGP,
!           vertical potential temperature gradient, if stable.

         call gintrp( gridht(ndxzpl), gridsv(ndxzpl),&
         &gridht(ndxzpl+1), gridsv(ndxzpl+1), zplm, svpm )
         call gintrp( gridht(ndxzpl), gridws(ndxzpl),&
         &gridht(ndxzpl+1), gridws(ndxzpl+1), zplm, upm )

         svpm = max( svpm, svmin, svumin*upm )
         if( l_vectorws )then
            upm = dsqrt( upm*upm + 2.0d0*svpm*svpm )
         endif
         upm  = max( upm, wsmin )


!RWB        Use average of stack top and midpoint wind speeds.
         up = 0.5d0 * (us + upm)

         call gintrp( gridht(ndxzpl), gridtg(ndxzpl),&
         &gridht(ndxzpl+1), gridtg(ndxzpl+1), zplm, tgpm )
         call gintrp( gridht(ndxzpl), gridpt(ndxzpl),&
         &gridht(ndxzpl+1), gridpt(ndxzpl+1), zplm, ptpm )
!RWB        Use average of stack top and midpoint temperature gradients.
         tgp = 0.5d0 * (tgs + tgpm)
         ptp = 0.5d0 * (pts + ptpm)
         bvf = dsqrt( g * tgp / ptp )
         if(bvf .lt. 1.0d-10) bvf = 1.0d-10
         bvprim  = 0.7d0 * bvf

         call distf

         kiter = kiter + 1

!           Check for convergence
         if(dabs((dhfold - dhfaer)/dhfaer) .lt. 0.01d0) go to 60

         if(kiter .ge. 5) then
            dhfaer = 0.5d0 * (dhfaer + dhfold)
            go to 60
         else
            go to 50
         end if

60       continue

!RWB        After completing iteration, reset UP and TGP to stack top
!RWB        values for subsequent distance-dependent plume rise calcs.
         up = us
         tgp = tgs
         ptp = pts
         bvf = dsqrt( g * tgp / ptp )
         if(bvf .lt. 1.0d-10) bvf = 1.0d-10
         bvprim  = 0.7d0 * bvf
      end if

!        Initialize PRM_FSTREC Logical Switch for First Receptor of Loop;
      prm_fstrec = .true.

      zmidmx = 0.5d0 * zi

!RJP
!RJP     Calculate distance to uniformly mixed plume within the
!RJP     boundary layer (XMIXED) after Turner's Workbook (1970), page 7:
!RJP     distance is approximately (Zi * UAVG)/SWAVG, where UAVG
!RJP     and SWAVG are wind speed and sigma-w averaged over the depth
!RJP     between the ground and Zi (or the plume height, if higher in
!RJP     stable conditions); this height is denoted as 2 * ZMIDMX.
!RJP
!RJP     First, get refined estimate of final rise and distance to final
!RJP     rise if downwash conditions prevail.
!RJP
      xfinal = xmax
      dhcrit = dhfaer
      xmixed = zi * uavg / swavg
      if (unstab .and. hs.lt.zi) then
!           Check for XMIXED smaller than 1.25*XFINAL
         if (xmixed .lt. 1.25d0*xfinal) then
            xfinal = 0.8d0 * xmixed
            call cblprd (xfinal)
            dhcrit = dhp1
         end if
      end if

   end if

!     First build table of relative dispersion coefficients for
!     dominant source (source index = ISDX)
   do i = 1, ntab

      xtab = xtable(i)

! ---    Assign XINP depending on whether XARG (src-rec dist) is
!        greater or less than current table value (XTAB)
      if (xarg .gt. xtab) then
         xinp = xtab
         if (i .gt. 1) then
            deltax = xtable(i) - xtable(i-1)
         else
            deltax = xtable(1)
         end if
      else
         xinp = xarg
         if (i .gt. 1) then
            deltax = xarg - xtable(i-1)
         else
            deltax = xarg
         end if
      end if

!        Receptor distance is > current table value, XTAB; calculate values at XTAB
!        Define plume centroid height (CENTER) for use in
!        inhomogeneity calculations
      call centroid ( xinp )

      if (srctyp(isrc)(1:5) .eq. 'POINT') then
!           Calculate the plume rise                  ---   CALL DELTAH
         call deltah ( xinp )
!**  Added for Aircraft Plume Rise; UNC-IE
      else if (srctyp(isrc) .eq. 'VOLUME' .and.&
      &aftsrc(isrc) .eq. 'Y') then
!           Calculate the plume rise                  ---   CALL ADELTAH
         call adeltah ( xinp )
      else if (srctyp(isrc) (1:4) .eq. 'AREA' .and.&
      &aftsrc(isrc) .eq. 'Y') then
!           Calculate the plume rise                  ---   CALL ADELTAH
         call adeltah ( xinp )
!**  End Aircraft Plume Rise insert; April 2023
      else
! ---       Assign 0.0 to DHP plume rise variables for non-POINT sources
         dhp  = 0.0d0
         dhp1 = 0.0d0
         dhp2 = 0.0d0
         dhp3 = 0.0d0
      end if

!        If the atmosphere is unstable and the stack
!        top is below the mixing height, calculate
!        the CBL PDF coefficients                     ---   CALL PDF
      if( unstab  .and.  (hs .lt. zi) ) then
         call pdf
      end if

!        Determine Effective Plume Height             ---   CALL HEFF
      call heff ( xinp )

!        Compute effective parameters using an
!        average through plume rise layer
      call iblval ( xinp )

!        Call PDF & HEFF again for final CBL plume heights
      if (unstab .and. (hs.lt.zi) ) then
         call pdf
         call heff ( xinp )
      end if

      if (srctyp(isrc)(1:5) .eq. 'POINT' .and. .not.nobid) then
!           Call BID to get buoyancy-induced dispersion terms
         call bid
!**  Added for Aircraft Plume Rise; UNC-IE
      else if (srctyp(isrc) .eq. 'VOLUME' .and. .not.nobid .and.&
      &aftsrc(isrc) .eq. 'Y') then
!           Call BID to get buoyancy-induced dispersion terms
         call bid
      else if (srctyp(isrc) (1:4) .eq. 'AREA'.and..not.nobid .and.&
      &aftsrc(isrc) .eq. 'Y') then
!           Call BID to get buoyancy-induced dispersion terms
         call bid
!**  End Aircraft Plume Rise insert; April 2023
      else
!           Set BID Terms to 0.0
         if( stable  .or.  (unstab .and. (hs .ge. zi) ) )then
            syb = 0.0d0
            szb = 0.0d0

         else if( unstab )then
            syb  = 0.0d0
            szbd = 0.0d0
            szbn = 0.0d0
            syb3 = 0.0d0
            szb3 = 0.0d0

         end if
      end if

! ---    Calculate "total" dispersion coefficients for STABLE conditions
!        and UNSTAB conditions with HS .GE. ZI and use "relative"
!        dispersion coefficients for UNSTAB conditions with HS .LT. ZI
!
      if (stable .or. (unstab  .and.  (hs .ge. zi)) )then
         if (srctyp(isrc)(1:5) .eq. 'POINT') then
! ---         Call PDIS to get SY and SZ for POINT sources
            call pdis( xinp )
! ---         Assign geometric mean of SY and SZ for SR
            sr(i) = dsqrt( sz*sy )

! ---         Set SR3 = 0.0 for "stable" plume
            sr3(i) = 0.0d0
         else if (srctyp(isrc) .eq. 'VOLUME') then
! ---         Call VDIS to get SY and SZ for VOLUME sources
            call vdis( xinp )
! ---         Assign geometric mean of SY and SZ for SR
            sr(i) = dsqrt( sy*sz )

! ---         Assign SR3 = 0.0 for non-POINT sources
            sr3(i) = 0.0d0
         else if (srctyp(isrc)(1:4) .eq. 'AREA' .or.&
         &srctyp(isrc)      .eq. 'OPENPIT') then
! ---         Call ADISY and ADISZ to get SY and SZ for AREA and OPENPIT sources
            call adisy( xinp )
            call adisz( xinp )
! ---         Assign geometric mean of SY and SZ for SR
            sr(i) = dsqrt( sy*sz )

! ---         Assign SR3 = 0.0 for non-POINT sources?
            sr3(i) = 0.0d0
         end if

!          First calculate the "effective" sigma-y value that
!          replicates the centerline concentration with the effect
!          of horizontal meander, but based on a "coherent" plume
         call meandr( ueff, sveff, fran )

! ---      Calculate effective sigma-y associated with non-DFAULT FASTALL option (EFFSIGY)
         syeff = 1.0d0/((fran/(srt2pi*xinp)) + (1.0d0-fran)/sy)

! ---      Use the value of NSUBZ based on original Hanrahan value of 1.282 (NSUBZS),
!          adjusted by the ratio of SYEFF/SY to account for meander, or a value of 2.15,
!          whichever is lower
         nsubz = min( 2.15d0,  nsubzs*(syeff/sy) )

! ---      Adjust SRMIN based on SYEFF/SY ratio for stable conditions
         srmin = srmins * (1.282d0/nsubz)

         if (i .gt. 1) then
            sigr = max( srmin, 0.5d0 * (sr(i) + sr(i-1)) )
         else
            sigr = max( srmin, 0.5d0 * sr(i) )
         end if

      else

! ---      Use relative dispersion coefficients for UNSTABLE conditions with HS < ZI;
!          Determine Relative Dispersion Parameters     ---   CALL RELDISP
         call reldisp ( xinp, sr(i), sr3(i) )

         if (i .gt. 1) then
            sigr = max( srmin, 0.5d0 * (sr(i) + sr(i-1)) )
         else
            sigr = max( srmin, 0.5d0 * sr(i) )
         end if

! ---      Use PPF-weighted average of SRMINS and SRMINU for SRMIN3
!          penetrated plumes
         if( ppfact(irec,isrc) .gt. 0.0d0 )then
            ppf = ppfact(irec,isrc)
            srmin3 = ppf*srmins + (1.0d0-ppf)*srminu
         endif

         if (ppfact(irec,isrc) .gt. 0.0d0) then
            if (i .gt. 1) then
               sigr3 = max( srmin3, (0.5d0 * (sr3(i) + sr3(i-1))) )
            else
               sigr3 = max( srmin3, (0.5d0 * sr3(i)) )
            end if
         else
            sigr3 = 0.0d0
         end if

      end if

      if( pvmrmdbg .and. debug )then
! ---       PVMRM and MODEL DEBUG options have been selected; include the
!           RELDISP Calcs in the MODEL DEBUG file
         write(rdispunt,111) srcid(isrc), i, xinp, sr(i), sr3(i),&
         &sigr, srmin
111      format(2x,a12,i4,5(2x,e11.5))
      endif

!        Calculate vertical dimension (VERT) taking into account ZI limit
      vert = bvarg + 2.0d0*nsubz*sigr
      if (unstab .and. vert .gt. zi) then
         vert  = zi
         bvtmp = max( 0.0d0, zi-2.0d0*nsubz*sigr )
         if (2.0d0*nsubz*sigr .gt. zi) then
            sigrztmp = zi/(2.0d0*nsubz)
         else
            sigrztmp = sigr
         end if
      else
         bvtmp = bvarg
         sigrztmp = sigr
      end if

!        Plume volume calculation based on rectangle with rounded corners
!        First component is for major contributing plume only
      sum = sum + (pi*(nsubz*sigr)*(nsubz*sigrztmp) +&
      &vert*bharg +&
      &2.0d0*nsubz*sigr*bvtmp) * deltax

      if (unstab .and. ppfact(irec,isrc) .gt. 0.0d0) then
! ---       Compute SUM3 for penetrated source plume if needed

! ---       Use a PPF-weighted average of NSUBZS and NSUBZU for penetrated plumes
         nsubz = ppf*nsubzs + (1.0d0-ppf)*nsubzu

         vert3 = bvarg3 + 2.0d0*nsubz*sigr3

         sum3  = sum3 + (pi*(nsubz*sigr3)*(nsubz*sigr3) +&
         &vert3*bharg3 +&
         &2.0d0*nsubz*sigr3*bvarg3) * deltax

      else

         sum3 = 0.0d0
      end if

! ---    Check for XARG .LE. XTAB, then volume calc is done; exit DO loop
      if (xarg .le. xtab) exit

   end do    ! End of loop on distance table

!     Combine volume for "direct" plume volume and penetrated plume volume
   volout = sum*(1.0d0-ppfact(irec,isrc)) + sum3*ppfact(irec,isrc)

   return
end

subroutine reldisp(xarg,srout,srout3)
!***********************************************************************
!             RELDISP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates relative dispersion coefficients for use
!                 in calculating plume volume for the PVMRM option
!
!        PROGRAMMER: Roger W. Brode, PES, Inc.
!
!        DATE:    May 14, 2002
!
!        MODIFICATIONS:
!
!               Modified treatment of virtual source term, VSIGR,
!               to account for cases when SYINIT or SZINIT may be
!               zero, such as AREA sources.
!               R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   PLUME_VOL
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision, parameter :: a1 = 0.57d0, a2 = 0.62d0*a1,&
   &ar1 = 0.46d0
   double precision :: xarg, srout, srout3, sramb, sramb3, tlr, vsigr

!     Variable Initializations
   modnam = 'RELDISP'

! --- Use square root of the product of lateral and vertical initial sigmas
!     to define the effective virtual source initial sigma, VSIGR;
!     however, need to check for one or the other being zero, e.g., for
!     area source types, szinit may be non-zero but syinit is zero.
   if (szinit .ge. 1.0d-8) then
      if (syinit .ge. 1.0d-8) then
         vsigr = dsqrt(syinit*szinit)
      else
         vsigr = szinit
      end if
   else if (szinit .lt. 1.0d-8) then
      if (syinit .ge. 1.0d-8) then
         vsigr = syinit
      else
         vsigr = 0.0d0
      end if
   else
      vsigr = 0.0d0
   end if

   if( stable .or. (unstab .and. (hs .ge. zi)) )then
!        The atmosphere is stable or the release is above the CBL mixing ht.
      tlr = ar1 * zi/sweff
      sramb = (a1 * dsqrt(epseff) * (xarg/ueff)**1.5d0)/&
      &(1.0d0 + a2 * xarg/(ueff*tlr))
      srout = (sramb**3 + szb**3 + vsigr**3)**third

! ---    Assign SROUT3 = 0.0 for stable or injected plume
      srout3 = 0.0d0

   elseif( unstab )then
!        The atmosphere is unstable and the release is below the CBL mixing ht.
      tlr = ar1 * zi/sweffd
      sramb = (a1 * dsqrt(epseffd) * (xarg/ueffd)**1.5d0)/&
      &(1.0d0 + a2 * xarg/(ueffd*tlr))
      srout = (sramb**3 + szbd**3 + vsigr**3)**third

!        Calculate relative dispersion for a penetrated plume, SROUT3
      if( ppfact(irec,isrc) .gt. 0.0d0 )then
         tlr = ar1 * zi/sweff3
         sramb3 = (a1 * dsqrt(epseff3) * (xarg/ueff3)**1.5d0)/&
         &(1.0d0 + a2 * xarg/(ueff3*tlr))
         srout3 = (sramb3**3 + szb3**3 + vsigr**3)**third

      else
         srout3 = 0.0d0
      end if

   end if

   return
end

subroutine aermax(srcs2use,maxarg,domarg)
!***********************************************************************
!             AERMAX Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To populate the MAXCHI and DOMIDX arrays
!
!        PROGRAMMER: J Paumier, MACTEC
!
!        DATE:    September 30, 2006
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   PVMRM_CALC
!***********************************************************************

   use main1
   implicit none

   double precision :: maxarg(numrec,numtyp)
   integer :: domarg(numrec,numtyp)
   integer :: i, j, k
   character srcs2use*7
   character modnam*12

   modnam = 'AERMAX'

!     Initialize MAXARG(NUMREC,NUMTYP) and DOMARG(NUMREC,NUMYTP) arrays
   maxarg = 0.0d0
   domarg = 0

   if (trim(srcs2use) .eq. 'NAAQSRC') then
!        Emission credit run: Increment Consumption+Nonretired Baseline=NAAQS
      do i = 1, numtyp
         do j = 1, numrec
            do k = 1, numsrc
               if (psdsrctyp(k) .eq. 'IC' .or.&
               &psdsrctyp(k) .eq. 'NB') then
                  if(chi(j,k,i) .gt. maxarg(j,i) )then
                     maxarg(j,i) = chi(j,k,i)
                     domarg(j,i) = k
                  end if
               end if
            end do
         end do
      end do

   else if (trim(srcs2use) .eq. 'ALLBASE') then
!        Emission credit run: Nonretired Baseline and Retired Baseline
      do i = 1, numtyp
         do j = 1, numrec
            do k = 1, numsrc
               if (psdsrctyp(k) .eq. 'NB' .or.&
               &psdsrctyp(k) .eq. 'RB') then
                  if(chi(j,k,i) .gt. maxarg(j,i) )then
                     maxarg(j,i) = chi(j,k,i)
                     domarg(j,i) = k
                  end if
               end if
            end do
         end do
      end do

   end if

   return
end

!CRT   D063
!***** BEGIN OCD PLATFORM ADDITIONS ************************************

subroutine plat_downwash(xd,hb,w,he,sigz0,sigy0)
!***********************************************************************
!             PLAT_DOWNWASH Subroutine
!
!        PURPOSE: COMPUTE PLATFORM DOWNWASH EFFECTS BASED ON PETERSON, 1986
!
!        PROGRAMMER: Clint Tillerson AMEC 2012, copied from OCD.FOR/DOWNWASH
!
!        DATE:    2012
!
!        MODIFICATIONS:
!                 Changed name of subroutine to have PLAT_ to denote it is
!                 used for PLATFORM sources.  Reformatted to AERMOD style.
!                 Added debug statements for PLATFORM debug file.
!                 Michelle G. Snyder, WOOD 8/5/2021.
!
!        INPUTS:  XD - DOWNWIND DISTANCE (meters)
!                 HB - BUILDING HEIGHT ABOVE WATER SURFACE (meters)
!                 W  - BUILDING WIDTH (meters)
!                 HE - EFFECTIVE STACK HEIGHT (meters)
!
!        OUTPUTS: SIGZ0 - INITIAL SIGMA-Z due to PLATFORM (meters)
!                 SIGY0 - INITIAL SIGMA-Y due to PLATFORM (meters)
!
!        CALLED FROM:   AERCALC
!***********************************************************************

   use main1, only: platfmdbg, platfmdbunt
   implicit none

!     Variable Declarations
   double precision, intent(in)  :: xd, hb, w, he
   double precision, intent(out) :: sigz0, sigy0

!     Local Variable Declarations
   double precision  :: a, ly, lz, chk, x, syop, szop
   double precision, parameter ::&
   &ay = 1.9d0, by = 48.2d0, cy = -1.4d0,&
   &az = 3.0d0, bz = 40.2d0, cz = -1.4d0

   sigz0 = 0.0d0
   sigy0 = 0.0d0

   ly = w/2.0d0
   lz = hb
   chk = xd/hb

   if( chk .lt. 2.2d0) then
      x = 2.2d0*hb
      syop = 0.071d0*x*dsqrt(ay+(by*(x/ly)**cy)-1.0d0)
!   3.915 = SQRT PORTION OF SZOP EQ. SOLVED USING X/LZ = 2.2
      szop = (.11d0*x**0.81d0)*3.915d0
   elseif( chk .gt. 12.6d0) then
      x = 12.6d0*hb
      syop = 0.071d0*x*dsqrt(ay+(by*(x/ly)**cy)-1.0d0)
!   3.915 = SQRT PORTION OF SZOP EQ. SOLVED USING X/LZ = 12.6
      szop = (.11d0*x**.81d0)*1.777d0
   else
      syop = 0.071d0*xd*dsqrt(ay+(by*(xd/ly)**cy)-1.0d0)
      szop = 0.11d0*((xd)**0.81d0)*dsqrt(az+(bz*(xd/lz)**cz)-1.0d0)
   endif

   a =he/hb
   if( a .gt. 3.0d0 ) then
      sigy0 = 0.0d0
      sigz0 = 0.0d0
   elseif( (1.2d0 .lt. a) .and. (a .le. 3.0d0) ) then
      sigy0 = 0.0d0
      sigz0 = 0.5d0*(3.0d0-a)*szop
   elseif( (1.0d0 .lt. a) .and. (a.le. 1.2d0) ) then
      sigy0 = 0.5d0*(6.0d0-5.0d0*a)*syop
      sigz0 = 0.5d0*(3.0d0-a)*szop
   else
      sigy0 = syop
      sigz0 = szop
   endif

   if (platfmdbg) then
      write(platfmdbunt,'(A, 4(A, 2X, F10.2),5(A, 2X, F8.5))')&
      &'calc1.f/PLAT_DOWNWASH: ',&
      &' XD = ', xd,&
      &' HB = ', hb,&
      &' W = ', w,&
      &' HE = ', he,&
      &' SZOP = ', szop,&
      &' SYOP = ', syop,&
      &' A = ', a,&
      &' SIGZ0 = ', sigz0,&
      &' SIGY0 = ', sigy0
   end if

   return
end

double precision function plat_gradplumadj( xarg )
!***********************************************************************
!        PLAT_GRADPLUMADJ Function
!
!        PURPOSE: Set-up distance dependent CC for CUBIC function. Output of
!                 CUBIC is DELH used to adjust plumerise based on platform influence.
!
!        PROGRAMMER: Michelle G. Snyder
!
!        DATE:    July 29, 2021
!
!        MODIFICATIONS:
!                 Changed name of subroutine to have PLAT_ to denote it is
!                 used for PLATFORM sources.  Reformatted to AERMOD style.
!                 Added debug statements for PLATFORM debug file.
!                 Michelle G. Snyder, WOOD 8/5/2021.
!
!        INPUTS:  XARG - Downwind distance in meters
!
!        OUTPUTS: Adjustment for gradual plume rise due to platform downwash
!
!        CALLED FROM:   AERCALC
!***********************************************************************
   use main1, only: fb, up, ac, bc, dhp, dhp1, stable, unstab, hs,&
   &zi, xmax, platfmdbg, platfmdbunt
   implicit none

!     Variable declarations - input
   double precision, intent(in)    :: xarg      ! Downwind distance (input)

!     Local variables
   double precision    :: delh  ! Plume rise adjustment from PLAT_CUBIC
   double precision    :: cc    ! Distance-dependent 3rd coefficient of CUBIC
   double precision    :: scoeff  ! check before CUBIC
   double precision, parameter  :: tol =   1.0d-4 !Set tolerance for CUBIC eqn solution

!     External Functions:
   double precision, external  :: plat_cubic

! ----------------------------------------------------------------------
!                    OCD Gradual Plume Rise Adjustment
!
!        IN STABLE CASES:
!           OCD calculates a distance dependent neutral rise CC (OCD PRI01260), then computes a
!           stable rise CC (OCD PRI01620); the lesser is used. This CC is kept and used in cubic
!
!        IN UNSTABLE CASES:
!           Compute cubic coefficient for gradual rise - from OCD model
!           C = (160)*(Fb**1/3) * (x**2/3) / (u)
!            where: Fb = bouyancy flux
!                   x = distance from source (km)
!                   u = stack top windspeed (m/s)
!
!           OCD assumes x is in km and C is in meters
!           C**3 = 4096000 * Fb * x**2 / u**3
!
!           Convert to x in meters for use in AERMOD
!           C**3 = 4.096 * Fb * x**2 / u**3
! ----------------------------------------------------------------------

   delh = 0.0d0 !intitalize

   if (stable  .or.  (unstab  .and.  (hs .ge. zi))) then
      cc = -1.0d0 * dhp * dhp * dhp !OCD PRI01260 & PRI01620; AERMOD calculates distance-dependent DHP in prise.f/SBLRIS, Neutral limit is applied in SBLRIS
      scoeff=ac+bc+cc !AC & BC are distance independent (MAIN1) and computed in calc1.f/PCALC
      if((scoeff.ne.0.0d0) .and. (xarg < xmax)) then
         delh = plat_cubic(ac,bc,cc,dhp,tol)
      end if
   else !modify direct plume downdraft (DHP1) only
      cc = -4.096d0*fb*xarg*xarg/(up*up*up) !OCD REC00970, but XARG in meters see note above.
      scoeff=ac+bc+cc !AC & BC are distance independent (MAIN1) and computed in calc1.f/PCALC
      if(scoeff.ne.0.0d0) then
         delh = plat_cubic(ac,bc,cc,dhp1,tol)
      end if
   end if

   if (platfmdbg) then
      write(platfmdbunt,'(A, 7(A, 2X, F10.2))')&
      &'calc1.f/PLAT_GRADPLUMADJ: ',&
      &' XARG = ', xarg,&
      &' XMAX = ', xmax,&
      &' FB = ', fb,&
      &' UP = ', up,&
      &' CC = ', cc,&
      &' SCOEFF = ', scoeff,&
      &' DELH = ', delh
   end if

   plat_gradplumadj = delh

   return
end

double precision function plat_cubic(a,b,c,zinit,tol)
!***********************************************************************
!             CUBIC Module of the AMS/EPA Regulatory Model
!             (Adapted from AERMOD v.98314)
!
!        PURPOSE: Solves Cubic Equation Using Newton's Method
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!                 Adapted by Clint Tillerson for use with
!                 OCD platform downwash, 1/23/2012
!
!        MODIFICATIONS:
!                 Changed from subroutine to function. Changed name of
!                 function to have PLAT_ to denote it is used for PLATFORM
!                 sources. Michelle G. Snyder, WOOD 8/5/2021
!
!        INPUTS:  Coefficients (A, B and C) of Cubic Equation
!                 Initial Guess for Variable
!                 Tolerance Level for Iteration
!
!        OUTPUTS: Solution to Cubic Equation;
!                    Z**3 + A*Z**2 + B*Z + C = 0
!
!        CALLED FROM:   PLAT_GRADPLUMADJ
!***********************************************************************

   implicit none

!     Variable Declarations

   double precision, intent(in)   :: a, b, c, zinit, tol
   double precision   :: z(25), fz, fp
   integer            :: n                      ! Loop counter

!     Assign Initial Guess to Z(1)
   z(1) = zinit

!     Begin Iterative LOOP (24 iterations)
   do 20 n = 1, 24
!        Calculate Cubic Function and First Derivative With Current Guess
      fz = z(n)*z(n)*z(n) + a*z(n)*z(n) + b*z(n) + c
      fp = 3.0d0*z(n)*z(n) + 2.0d0*a*z(n) + b

!        Calculate New Guess
      z(n+1) = z(n) - fz/fp

!        Check successive iterations for specified tolerance level
      if (abs(z(n+1) - z(n)) .le. tol) then
         plat_cubic = z(n+1)
!           Convergence, thus Exit Loop
         go to 999
      end if

20 continue
!     End Iterative LOOP

!     If No Convergence In Loop, Then Use Average of Last Two Estimates,
   plat_cubic = 0.5d0 * (z(24) + z(25))

999 return
end

!***** END OCD PLATFORM ADDITIONS **************************************

!WSP --- Begin: Moved to end of calc1.f - D174 WSP 7/25/2023
subroutine swcalc
!***********************************************************************
!        PURPOSE: Calculates concentration or deposition values
!                 for SIDEWASH Point sources
!
!        PROGRAMMER: Carlos Szembek (AECOM).
!
!        DATE:    December 23, 2021
!
!        INPUTS:  Source Parameters for Specific Source
!                 Arrays of Receptor Locations
!                 Meteorological Variables for One Hour
!
!        OUTPUTS: 1-hr CONC Values for Each Receptor for
!                 Particular Source ** ONLY WITHIN WAKE ZONE **
!
!        CALLED FROM:   CALC
!
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   integer :: ndxzhs, ndxzbh
   double precision :: stkwd, stkws, swepsilon,& ! Now global: THETA_P & THETA_SW
   &chisw,&                   ! Now global: SX1, SY1, SZW, CONC_P and CONC_SW
   &wtsf,&                    ! Added wind tunnel scaling factor
   &tmptheta,&                ! Temporary variable for storing SWTHETA
   &conc_full
! Added for Sidewash; temporary variables to save translated receptors
   double precision :: swxt(nrec), swyt(nrec)
! End sidewash insert
!     Variable Initializations
   modnam = 'SWCALC'

!     Set the Source Variables for This Source              ---   CALL SETSRC
   call setsrc
! Initialize wind tunnel scaling factor and scaled building dimensions and stack height
   wtsf = 150.0d0
   bhs = bh/wtsf
   bws = bw/wtsf
   bls = bl/wtsf
   hss = hs/wtsf
!     Apply Variable Emission Rate Factors                  ---   CALL EMFACT
!     CALL EMFACT(QS)                                    <----   Not yet implemented

!---- Locate index below (actual) stack height  ! updated April 2022
   call locate(gridht, 1, mxglvl, hs, ndxzhs)
!----    Get Wind direction at stack height ! updated April 2022
   call gintrp( gridht(ndxzhs), gridwd(ndxzhs),&
   &gridht(ndxzhs+1), gridwd(ndxzhs+1), hs, stkwd )
!---- Locate index below (actual) building height  ! updated April 19, 2022
   call locate(gridht, 1, mxglvl, bh, ndxzbh)
!----    Get Wind speed at building height ! updated April 19, 2022
   call gintrp( gridht(ndxzbh), gridws(ndxzbh),&
   &gridht(ndxzbh+1), gridws(ndxzbh+1), bh, stkws )

   u_amb = stkws
!     Check for WDIR > 360 or < 0
   if (stkwd .gt. 360.0d0) then
      stkwd = stkwd - 360.0d0
   else if (stkwd .le. 0.0d0) then
      stkwd = stkwd + 360.0d0
   end if

! Calculate wind angle on building face
   swtheta = ba - (90.-(360. - stkwd))

!     Check for if SWTHETA  > 360 or < 0
   if (swtheta .ge. 359.9d0) then
      swtheta = swtheta - 360.0d0
   else if (swtheta .lt. 0.0d0) then
      swtheta = swtheta + 360.0d0
   end if

   if (swdbg) then
!        *** Write out building dimensions, stack height ***
!        *** ambient wind speed and emission rate        ***
      write(swdbgunt,*)
      write(swdbgunt,*)('OUTPUT FOR GAUSSIAN DISPERSION EQUATION')
      write(swdbgunt,*)('***************************************')
      write(swdbgunt,*)('Definitions:')
      write(swdbgunt,*)('SWCALC:     Final Sidewash Calc.')
      write(swdbgunt,*)('CHISW:      Sidewash Chi Conc.')
      write(swdbgunt,*)('***************************************')
!
!        *** Write out header for input data ***
      write(swdbgunt,887)
887   format(//,'Building ','Building ','Building ','Stack ',&
      &'    Ambient        Stack-top  Emission')

      write(swdbgunt,897)
897   format('Height m ','Length m ','Width m ',' Height m ',&
      &' Wind Speed m/s Wind Dir.  g/s')

      write(swdbgunt,852)
852   format(72('-'))
!        *** Write out values of building dimensions          ***
!        *** stack height,ambient wind speed and emisson rate ***
      write(swdbgunt,810)bh,bl,bw,hs,u_amb,stkwd,qs
810   format(1x,f4.1,6x,f4.1,2x,f6.1,5x,f6.3,3x,f5.2,10x,f5.1,6x,&
      &f8.5)
!
      write(swdbgunt,*)
      write(swdbgunt,850)
850   format(72('*'))
      write(swdbgunt,*)
   end if

! --- Begin Receptor LOOP
   receptor_loop: do irec = 1, numrec

!        Added for Sidewash; Adjust and save copy of receptor grid in local coordinates
!        rotated such that the building angle (BA) is 0 degrees
!        TRANSLATION
!        Shift receptor based on X,Y of each sidewash source (AXS, AYS)
!        Note: the SWXR & SWYR arrays for non-sidewash sources are set but
!        not used by AERMOD.
      swxt(irec) = axr(irec) - axs(isrc)
      swyt(irec) = ayr(irec) - ays(isrc)

!        ROTATION
!        Rotate the translated X,Y such that flow across the building length is rotated
!        aligned with the X-axis based on the input building angle (BA).
!        Wind direction for sidewash is simarly rotated in sub.SWCALC in calc1.f
      swxr(irec) = swxt(irec) * dcos((270.0d0 - stkwd)*dtorad) +&
      &swyt(irec) * dsin((270.0d0 - stkwd)*dtorad)
      swyr(irec) = swyt(irec) * dcos((270.0d0 - stkwd)*dtorad) -&
      &swxt(irec) * dsin((270.0d0 - stkwd)*dtorad)


      sx1 = swxr(irec)/bh         ! changed from AXR(IREC)

      if(sx1 .eq. 0.00d0) then
         sx1 = 1.0d-08
      endif

      sy1 = swyr(irec)/wtsf       ! changed from AYR(IREC)
      swz = azelev(irec)/wtsf     ! changed from AZELEV(IREC)

      if (swdbg) then

!           *** Write out header of distance and modifiers of ***
         write(swdbgunt,*)
         write(swdbgunt,855) irec
855      format('Receptor No: ',i5)
         write(swdbgunt,812)
812      format('Orig. X (m) Orig. Y (m) Adj. X1 (m) Adj. Y1 (m)',&
         &'     Z1 (m)      W/H         X1/H    Bld. Angle',&
         &'  Theta')
         write(swdbgunt,851)
851      format(102('-'))
         write(swdbgunt,816) axr(irec),ayr(irec),sx1,sy1,swz,bw/bh,&
         &sx1/bh,ba,swtheta
816      format(7(f10.4,2x),f5.1,7x,f5.1)
         write(swdbgunt,*)
      end if

!        Initialize SWCONC and CHISW
      swconc = 0.0d0
      chisw  = 0.0d0

!        Wind angle cases:
!        April, 2022; Added symmetric conditions between 300 - 360 degrees
      if ((swtheta .eq. 0.0d0) .or.&
      &(swtheta .eq. 360.0d0)) then

!            CALL PERPEN(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
!     &                  U_AMB, QS, SWCONC)
         call perpen

      elseif ((swtheta .ge. 15.0d0) .and.&
      &(swtheta .le. 60.0d0)) then

!            CALL OBLIQUE(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
!     &                   U_AMB, QS, SWCONC)
         call oblique

      elseif ((swtheta .gt. 0.0d0) .and.&
      &(swtheta .lt. 15.0d0)) then

!           Initialize variables
         tmptheta = swtheta ! Store calculated SWTHETA in temp variable
         conc_p   = 0.0d0
         conc_sw  = 0.0d0
         theta_p  = 0.0d0
         theta_sw = 15.0d0
         swepsilon = 1.0d0-((tmptheta-theta_p)/(theta_sw-theta_p))
         swtheta = theta_p

!            CALL PERPEN(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
!     &                         U_AMB, QS, SWCONC)
         call perpen

         conc_p = swconc
         swconc = 0.0d0
         swtheta = theta_sw

!            CALL OBLIQUE(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
!     &                   U_AMB, QS, SWCONC)
         call oblique

         conc_sw = swconc
         swtheta = tmptheta ! Restore calculated SWTHETA value
         swconc = conc_p*swepsilon + conc_sw*(1-swepsilon)

      elseif ((swtheta .ge. 300.0d0) .and.&
      &(swtheta .le. 345.0d0)) then

         sy1 = -1.0d0*sy1
         tmptheta = swtheta ! Store calculated SWTHETA in temp variable
         swtheta = 360.0d0 - swtheta

!            CALL OBLIQUE(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
!     &                   U_AMB, QS, SWCONC)
         call oblique

         swtheta = tmptheta ! Restore calculated SWTHETA value

      elseif ((swtheta .gt. 345.0d0) .and.&
      &(swtheta .lt. 360.0d0)) then

!           Initialize variables
         sy1 = -1.0d0*sy1
         tmptheta = swtheta ! Store calculated SWTHETA in temp variable
         conc_p   = 0.0d0
         conc_sw  = 0.0d0
         theta_p  = 360.0d0
         theta_sw = 345.0d0
         swepsilon = 1.0d0-((tmptheta-theta_p)/(theta_sw-theta_p))
         swtheta = theta_p

!            CALL PERPEN(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
!     &                  U_AMB, QS, SWCONC)
         call perpen

         conc_p = swconc
         swconc = 0.0d0
         swtheta = 360.0d0 - theta_sw ! Should equal 15

!            CALL OBLIQUE(BHS, BLS, BWS, HSS, SX1, SY1, SWZ, SWTHETA,
!     &                   U_AMB, QS, SWCONC)
         call oblique

         conc_sw = swconc
         swtheta = tmptheta ! Restore calculated SWTHETA value
         swconc = conc_p*swepsilon + conc_sw*(1.0d0-swepsilon)

!        Added April 2022
      else ! For SWTHETA greater than 60 degrees
         swconc = 0.0d0
      endif


!        Calculate CHI for sidewash (April 2022)
      chisw = swconc*u_amb*bhs**2.0/qs             ! Normailized (unitless) Adjusted for wind tunnel scaling
      hrval = 1.0d06*chisw*qs/(u_amb*bh**2.0)      ! Adjusted for wind tunnel scaling; scale from g/m3 to ug/3

      if (swdbg) then

         write(swdbgunt,*)
         write(swdbgunt,833)
833      format(32x,'HRVAL ug/m3',2x,'SWCONC g/m3',2x,'CHI')

!           *** Write out total concentration ***
         write(swdbgunt,834) hrval, swconc, chisw
834      format('SIDEWASH CONCENTRATIONS = ', 3(f12.4,1x))

         write(swdbgunt,*)
         write(swdbgunt,*)
         write(swdbgunt,*)

      end if

!        CRT 4/20/2022 D113 Sidewash
!        If concentration is negative, set to zero and write warning message
      if (swconc .lt. 0.0d0) then
         write(dummy,'(I12)') kurdat
         call errhdl(path, modnam, 'W','650',dummy)
         hrval = 0.0d0
      end if

!        Call AERMOD averaging
      do igrp = 1, numgrp
         call sumval
      end do

!        Initialize __VAL arrays (1:NUMTYP)
      hrval   = 0.0d0

   end do receptor_loop

   return
end

subroutine perpen!(BH,BL,BW,HS,SX1,SY1,SWZ,SWTHETA,U_AMB,QS,SWCONC)
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: beta_b_u_c1, beta_b_u_c2, beta_b_sig_y1_c1,&
   &beta_b_sig_z1, beta_b_mu_z1_c1, u, sig_y1,&
   &sig_z1, sig_y2, sig_z2, rho, term1, term2,&
   &term2a, term2b, term2c, term2d, term3,&
   &term3b, term3c, term3d, term4, term5a, term5b,&
   &term5c, term51, term52, term5 !, SX1, SY1, SWZ  <-- global April 2022

!     Variable Initializations
   modnam = 'PERPEN'

   if (sx1 .lt. 0.0) then
      swconc = 0.0
      goto 899
   endif

   beta_b_u_c1      = (-0.0555d0*(bws/bhs))
   beta_b_u_c2      = 0.975993d0
   beta_b_sig_y1_c1 =  0.0141d0 + 0.0006d0*(bws/bhs)
   beta_b_sig_z1    = 1.5d0* (0.0172d0 +(0.0003d0*(bws/bhs)))
   beta_b_mu_z1_c1  = -0.0217d0 + (0.0016d0*(bws/bhs))

   u      = beta_b_u_c1 + beta_b_u_c2*u_amb
   lambda = 1.0d0
   sig_y1 = beta_b_sig_y1_c1*sx1
   sig_z1 = beta_b_sig_z1*sx1
   sig_y2 = 1.0d0
   sig_z2 = 1.0d0
   mu_y1  = 0.0d0
   mu_z1  = hss + beta_b_mu_z1_c1*sx1
   mu_y2  = 0.0d0
   mu_z2  = 0.0d0
   rho    = 0.0d0

   if (swdbg) then

!        *** CHECK SECOND SET OF VALUES ***
      write(swdbgunt,*) ('PERPENDICULAR:')

!        *** WRITE OUT LAMBDA RHO AND U_AMB ***
      write(swdbgunt,818)lambda,rho,u
818   format('LAMBDA = ',f7.5,3x,'RHO = ',f7.5,3x,'U =',f5.3)

!        ************ WRITE OUT SIG AND MU VALUES *************
!        *** Write out header for sigma values ***
      write(swdbgunt,*)('SIG Y1     SIG Z1    SIG Y2    SIG Z2')

!        *** Write our values of sigma values ***
      write(swdbgunt,820)sig_y1,sig_z1,sig_y2,sig_z2
820   format(4(f8.6,2x))

!        *** Write out header for MU values
      write(swdbgunt,*)('MU Y1      MU Z1     MU Y2      MU Z2')

!        *** Write out values for MU values
      write(swdbgunt,822)mu_y1,mu_z1,mu_y2,mu_z2
822   format(4(f8.6,2x))

   end if

   term1 =(lambda*qs)/((2*pi*u*sig_y1*sig_z1)*(sqrt(1.0d0-(rho**2))))
!
!     *** Second Term, parts A,B,C,D and full TERM2  ***
   term2a = (-1.0d0)/(2.0d0*(1.0d0-(rho**2)))
   term2b = ((sy1-mu_y1)**2)/(sig_y1**2)
   term2c = ((swz-mu_z1)**2)/(sig_z1**2)
   term2d = (2.0d0*rho*(sy1-mu_y1)*(swz-mu_z1))/(sig_y1*sig_z1)
   term2 = dexp(term2a*(term2b + term2c - term2d))

   if (swdbg) then

      write(swdbgunt,*)

!        *** Write out TERM1 and parts A,B,C,D and full of TERM2 ***
      write(swdbgunt,824) term1
824   format( 'TERM1 = ',f14.6)

      write(swdbgunt,*)
      write(swdbgunt,*)'TERM2A    TERM2B     TERM2C     TERM2D',&
      &'     TM2'
      write(swdbgunt,826)term2a,term2b,term2c,term2d,term2
826   format(2(f9.6,1x),f9.3,2x,2(f9.4,1x))

   end if
!
!     *** Third Term ***
   term2a = (-1.0d0/(2.0d0*(1-rho**2)))
   term3b = ((sy1-mu_y1)**2)/(sig_y1**2)
   term3c = ((swz+mu_z1)**2)/(sig_z1**2)
   term3d = (2.0d0*rho*(sy1-mu_y1)*(swz+mu_z1))/(sig_y1*sig_z1)
   term3 = dexp(term2a*(term3b + term3c - term3d))

   if (swdbg) then

      write(swdbgunt,*)

!        *** Write out TERM2A and parts B,C,D and full TERM3 ***
      write(swdbgunt,*)'TERM2A    TERM3B     TERM3C     TERM3D',&
      &'     TM3'
      write(swdbgunt,828)term2a,term3b,term3c,term3d,term3
828   format(2(f9.6,1x),f9.3,2x,2(f9.4,1x))

   end if
!
!     *** Fourth Term ***
   term4 = ((1.0d0 - lambda)*qs)/((2*pi*u*sig_y2*sig_z2))

   if (swdbg) then

      write(swdbgunt,*)

!        *** Write out TERM4 ***
      write(swdbgunt,830)term4
830   format( 'TERM4 = ',f14.6)

   end if
!
!     *** Fifth Term ***
   term5a = ((sy1 - mu_y2)**2)/(sig_y2**2)
   term5b = ((swz - mu_z2)**2)/(sig_z2**2)
   term5c = ((swz + mu_z2)**2)/(sig_z2**2)
   term51 = dexp((-0.5d0*(term5a + term5b)))
   term52 = dexp((-0.5d0*(term5a + term5c)))
   term5 = term51 + term52

   if (swdbg) then

      write(swdbgunt,*)

!        *** Write out parts A,B,C,1,2 and full TERM5 ***
      write(swdbgunt,831)
831   format('TERM5A',7x,'TM5B',8x,'TM5C',5x,'T51',7x,'T52',7x,'T5')

      write(swdbgunt,832) term5a,term5b,term5c,term51,term52,term5
832   format(f9.6,1x,2(f10.4,2x),3(f9.6,1x))

   end if

!     ************* Calculate the total concentration ***********
!
   swconc = (term1*(term2+term3)) + (term4*(term5))
899 continue
   return
end

subroutine oblique!(BH,BL,BW,HS,SX1,SY1,SWZ,SWTHETA,U_AMB,QS,SWCONC)
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   character modnam*12
   double precision :: beta_b_u_c1, beta_b_u_c2, beta_b_lambda,&
   &beta_b_sig_y1_c1, beta_b_sig_y1_c2,&
   &beta_b_sig_z1, beta_b_sig_y2,&
   &beta_b_sig_z2_c1, beta_b_sig_z2_c2,&
   &beta_b_mu_y1_c1, beta_b_mu_y1_c2,&
   &beta_b_mu_z1_c1, beta_b_mu_z1_c2,&
   &beta_b_mu_y2, beta_b_mu_z2,&
   &beta_b_rho_c1, beta_b_rho_c2,&
   &u, sig_y1,&
   &sig_z1, sig_y2, sig_z2, rho, term1, term2,&
   &term2a, term2b, term2c, term2d, term3,&
   &term3b, term3c, term3d, term4, term5a, term5b,&
   &term5c, term51, term52, term5!, SX1, SY1, SWZ  <-- global, April 2022

!     Variable Initializations
   modnam = 'OBLIQUE'

   if (sx1 .lt. 0.0d0) then
      swconc = 0.0d0
      goto 898
   endif

   beta_b_u_c1 = (-0.0555d0*(bws/bhs))
   beta_b_u_c2 = 0.975993d0
   beta_b_lambda = -0.416614d0 + (0.003644d0*swtheta) +&
   &(-0.012852d0*(bws/bhs))
   beta_b_sig_y1_c1 = 0.026233d0
   beta_b_sig_y1_c2 = -0.00117d0
   beta_b_sig_z1 = 1.83d-02 + (-3.83d-05*swtheta) +&
   &(-5.81d-04*(bws/bhs))
   beta_b_sig_y2 = 2.67d-02 + (-8.50d-05*swtheta) +&
   &(4.93d-04*(bws/bhs))
   beta_b_sig_z2_c1 = 0.0385d0 + (-0.000171d0*swtheta) +&
   &(-0.0014d0*(bws/bhs))
   beta_b_sig_z2_c2 = -0.00191d0 + (0.0000108d0*swtheta) +&
   &(0.000118d0*(bws/bhs))
   beta_b_mu_y1_c1 = 0.015081d0 + (0.000243d0*swtheta) +&
   &(-0.0031d0*(bws/bhs))
   beta_b_mu_y1_c2 = -0.00175d0 + (-0.0000178d0*swtheta) +&
   &(0.000331d0*(bws/bhs))
   beta_b_mu_z1_c1 = -0.03578d0 + (-0.00029d0*swtheta) +&
   &(0.00224d0*(bws/bhs))
   beta_b_mu_z1_c2 = 0.00186d0 + (0.00000833d0*swtheta) +&
   &(-0.000116d0*(bws/bhs))
   beta_b_mu_y2 = 2.21d-02 + (-1.82d-04*swtheta) +&
   &(9.71d-04*(bws/bhs))
   beta_b_mu_z2 = -0.357069d0 + (0.002894d0*swtheta) +&
   &(0.002252d0*(bws/bhs))
   beta_b_rho_c1 = 0.012006d0
   beta_b_rho_c2 = -0.00039d0
!
!     *** Start defining terms in Gaussian Dispersion Equation ***
!     *** according to Beta_a(x)
   u = beta_b_u_c1 + beta_b_u_c2*u_amb
   lambda = dexp(beta_b_lambda*sx1)
   sig_y1 = beta_b_sig_y1_c1*sx1 + beta_b_sig_y1_c2*(sx1**2)
   sig_z1 = beta_b_sig_z1*sx1
   sig_y2 = beta_b_sig_y2*sx1
   sig_z2 = beta_b_sig_z2_c1*sx1 + beta_b_sig_z2_c2*(sx1**2)
   mu_y1 = beta_b_mu_y1_c1*sx1 + beta_b_mu_y1_c2*(sx1**2)
   mu_z1 = hss + beta_b_mu_z1_c1*sx1 + beta_b_mu_z1_c2*(sx1**2)
   mu_y2 = beta_b_mu_y2*sx1
   mu_z2 = bhs*dexp(beta_b_mu_z2*sx1)
   rho = beta_b_rho_c1*sx1 + beta_b_rho_c2*(sx1**2)

   if (swdbg) then

!        *** CHECK SECOND SET OF VALUES ***
      write(swdbgunt,*) ('OBLIQUE:')

!        *** WRITE OUT LAMBDA RHO AND U_AMB ***
      write(swdbgunt,818)lambda,rho,u
818   format('LAMBDA = ',f7.5,3x,'RHO = ',f7.5,3x,'U =',f5.3)

!        ************ WRITE OUT SIG AND MU VALUES *************
!        *** Write out header for sigma values ***
      write(swdbgunt,*) 'SIG Y1     SIG Z1    SIG Y2    SIG Z2'

!        *** Write our values of sigma values ***
      write(swdbgunt,820)sig_y1,sig_z1,sig_y2,sig_z2
820   format(4(f8.6,2x))

!        *** Write out header for MU values
      write(swdbgunt,*) 'MU Y1      MU Z1     MU Y2      MU Z2'

!        *** Write out values for MU values
      write(swdbgunt,822)mu_y1,mu_z1,mu_y2,mu_z2
822   format(4(f8.6,2x))

   end if

   term1 =(lambda*qs)/((2*pi*u*sig_y1*sig_z1)*(sqrt(1-(rho**2))))
!
!     *** Second Term, parts A,B,C,D and full TERM2  ***
   term2a = (-1.0d0)/(2.0d0*(1-(rho**2)))
   term2b = ((sy1-mu_y1)**2)/(sig_y1**2)
   term2c = ((swz-mu_z1)**2)/(sig_z1**2)
   term2d = (2.0d0*rho*(sy1-mu_y1)*(swz-mu_z1))/(sig_y1*sig_z1)
   term2 = dexp(term2a*(term2b + term2c - term2d))

   if (swdbg) then

      write(swdbgunt,*)

!        *** Write out TERM1 and parts A,B,C,D and full of TERM2 ***
      write(swdbgunt,824)term1
824   format( 'TERM1 = ',f14.6)

      write(swdbgunt,*)
      write(swdbgunt,*) 'TERM2A    TERM2B     TERM2C     TERM2D',&
      &'     TM2'
      write(swdbgunt,826)term2a,term2b,term2c,term2d,term2
826   format(2(f9.6,1x),f9.3,2x,2(f9.4,1x))

   end if

!     *** Third Term ***
   term2a = (-1.0d0/(2.0d0*(1-rho**2)))
   term3b = ((sy1-mu_y1)**2)/(sig_y1**2)
   term3c = ((swz+mu_z1)**2)/(sig_z1**2)
   term3d = (2.0d0*rho*(sy1-mu_y1)*(swz+mu_z1))/(sig_y1*sig_z1)
   term3 = dexp(term2a*(term3b + term3c - term3d))

   if (swdbg) then

      write(swdbgunt,*)

!        *** Write out TERM2A and parts B,C,D and full TERM3 ***
      write(swdbgunt,*) 'TERM2A    TERM3B     TERM3C     TERM3D',&
      &'     TM3'
      write(swdbgunt,828)term2a,term3b,term3c,term3d,term3
828   format(2(f9.6,1x),f9.3,2x,2(f9.4,1x))

   end if
!
!     *** Fourth Term ***
   term4 = ((1.0 - lambda)*qs)/((2*pi*u*sig_y2*sig_z2))

   if (swdbg) then

      write(swdbgunt,*)

!        *** Write out TERM4 ***
      write(swdbgunt,830)term4
830   format( 'TERM4 = ',f14.6)

   end if

!     *** Fifth Term ***
   term5a = ((sy1 - mu_y2)**2)/(sig_y2**2)
   term5b = ((swz - mu_z2)**2)/(sig_z2**2)
   term5c = ((swz + mu_z2)**2)/(sig_z2**2)
   term51 = dexp((-0.5d0*(term5a + term5b)))
   term52 = dexp((-0.5d0*(term5a + term5c)))
   term5 = term51 + term52

   if (swdbg) then

      write(swdbgunt,*)

!        *** Write out parts A,B,C,1,2 and full TERM5 ***
      write(swdbgunt,831)
831   format('TERM5A',7x,'TM5B',8x,'TM5C',5x,'T51',7x,'T52',7x,'T5')

      write(swdbgunt,832) term5a,term5b,term5c,term51,term52,term5
832   format(f9.6,1x,2(f10.4,2x),3(f9.6,1x))

   end if

!     ************* Calculate the total concentration ***********
!
   swconc = (term1*(term2+term3)) + (term4*(term5))
898 continue
   return
end
!WSP --- End: Moved to end of calc1.f - D174 WSP 7/25/2023
