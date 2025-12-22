subroutine rlcalc
!***********************************************************************
!                 RLCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Introduce RLINE source. Based on RLINEv1.2 released
!                     November 2013.
!
!        PROGRAMMER:  M. Snyder, R. Cleary, J. Paumier, R. Wagoner, Wood
!
!        DATE:        July 20, 2018
!
!        MODIFIED:    Terrain treatment included by adding the ZFLAG, ZHILL
!                     ZELEV heights
!                     Wood, 7/5/2022
!
!        MODIFIED:    Update SFCZ0 when Urban stable environment. Resets
!                     after the receptor loop to the saved SFCZ0 value
!                     Wood, 1/06/2022
!
!        MODIFIED:    Added DEBUG output to RLINE.DBG file.
!                     Wood, 12/07/2021 Michelle G. Snyder & Laura Kent
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:    Added Urban option for RLINE sources.
!                     Wood, 03/04/2019
!                     Corrected processing of EMISFACT for RLINE sources.
!                     D42, Wood, 06/19/2020
!
!        INPUTS:      None
!
!        OUTPUTS:
!
!        CALLED FROM: MAIN
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
!CRT/ORD 5/17/2022 - Add DISTR from Main1 module to initialize for RLINE source
! Wood 10/10/22 removed FOPT FROM MAIN1; NOT USED HERE
   use main1, only: azflag, hrval, numtyp,&
   &numgrp, igrp, isrc, irec, numrec, l_hrlysig,&
   &ityp, arm2, chi, evonly, aqs, ahs, aszini, qflag,&
   &urbsrc, numurb, stable, L_MorningTrans,&
   &zi, ziurb, zirur, zimech, urban, srctyp,&
   &ustar, urbustr, rurustr, urbstab,&
   &iurb, obulen, urbobulen, rurobulen, iurbgrp,&
   &wstar, urbwstr, emifac, qtk,&
   &rlinedbg, rlinedbunt, kurdat,&
   &sfcz0, urbz0, gridsv, grdsvr, grdsvu, mxglvl,&
   &fastall, distr,&
   &azelev, zelev, azhill, zhill, zflag, zs, azs !Wood 7/5/2022
!     &                 FOPT !Wood 7/5/2022
!     rline_emisfact_d42_Wood
!     Added import of QTK from MAIN1 above, and new local QEMIS below
   use rline_data
   implicit none

   integer  :: i
   double precision  :: error
   double precision  :: concd
   double precision  :: concentration
   double precision  :: qemis
   double precision  :: SFCZ0_sav, zi_sav
   double precision  :: zi_orig !D178

!     ERROR         = error in numerical integration
!     CONCD         = dummy concentration at receptor
!     CONCENTRATION = concentration
!     QEMIS         = emission rate

   double precision  :: xtemp, ytemp, ztemp
!     XTEMP          = temporary x-coordinate of receptor for line orientation
!     YTEMP          = temporary y-coordinate of receptor for line orientation
!     ZTEMP          = temporary z-coordinate of receptor for line orientation

!     Variable Initializations:
!MGS      CONCD = 0.0D0 !D178_RLINE_RecpOrder_WSP: Moved to REC loop
!MGS     CONCENTRATION = 0.0D0  !D178_RLINE_RecpOrder_WSP: moved to REC loop
!CRT/ORD 5/17/2022 - Reset DISTR if used from previous sources
   distr = 0.0d0

!     Initialize __VAL arrays
!MGS      HRVAL = 0.0D0 !D178_RLINE_RecpOrder_WSP
   hrval(:) = 0.0d0

!     rline_emisfact_d42_Wood
!     Local QEMIS will hold the hourly source specific emission to be
!     used in the calculation of concentration below, replacing the use
!     of RLSOURCE(ISRC)%QEMIS in those expressions.
   qemis = rlsource(isrc)%qemis
   if(qflag(isrc) == 'HOURLY') then
!        Set hourly variable source height and sigmaz-initial (optional)
      if(l_hrlysig(isrc)) then
         rlsource(isrc)%zsb = ahs(isrc)
         rlsource(isrc)%zse = ahs(isrc)
         rlsource(isrc)%init_sigmaz = aszini(isrc)
      end if
!        Set hourly variable emission rate (required)
      if(srctyp(isrc) == 'RLINEXT') then
         qemis = aqs(isrc)
      else ! RLINE source with Lnemis inputs
         qemis = aqs(isrc)*rlsource(isrc)%width
      end if
!     rline_emisfact_d42_Wood
!     The following block is added to process the EMISFACT keyword with
!     RLINE sources, set QTK equal to appropriate emission factor, and
!     multiply by emission rate.
   else if ((qflag(isrc) == 'MONTH') .or.&
   &(qflag(isrc) == 'HROFDY') .or.&
   &(qflag(isrc) == 'WSPEED') .or.&
   &(qflag(isrc) == 'SEASON') .or.&
   &(qflag(isrc) == 'SEASHR') .or.&
   &(qflag(isrc) == 'HRDOW') .or.&
   &(qflag(isrc) == 'HRDOW7') .or.&
   &(qflag(isrc) == 'SHRDOW') .or.&
   &(qflag(isrc) == 'SHRDOW7') .or.&
   &(qflag(isrc) == 'MHRDOW') .or.&
   &(qflag(isrc) == 'MHRDOW7')) then
      call emfact(1.0d0)
      qemis = rlsource(isrc)%qemis*qtk
   end if
!     Perform first hour calculations
   if(rlfirsthr) then
!        Create exponential tables                                           --- CALL CREATE_EXP_TABLE
      call create_exp_table
!        Perform MOVES to RLINE unit conversion                              --- CALL RLEMCONV
      call rlemconv
      rlfirsthr = .false.
   end if

!        Save the original SFCZ0 & ZI
   SFCZ0_sav = sfcz0
   zi_orig    = zi ! D096; D178 switch ZI_SAV to ZI_ORIG
!        Set Mixing Height and Adjust L & USTAR for Urban Option if Needed
   if (urbsrc(isrc) == 'Y') then
!           Find Urban Area Index for This Source
      do i = 1, numurb
         if (iurbgrp(isrc,i) == 1) then
            iurb = i
            exit
         end if
      end do
      if (stable .or. L_MorningTrans(iurb)) then
         urbstab = .true.
         zi = max( ziurb(iurb), zimech )
         gridsv = grdsvu(1:mxglvl,iurb)
         obulen = urbobulen(iurb)
         ustar  = urbustr(iurb)
         rlwstar = urbwstr(iurb)
         sfcz0 = urbz0(iurb)
      else
         urbstab = .false.
         zi = zirur
         gridsv = grdsvr
         obulen = rurobulen
         ustar  = rurustr
         rlwstar = wstar
      end if
   else if (urban .and. urbsrc(isrc) == 'N') then
      urbstab = .false.
      zi = zirur
      gridsv = grdsvr
      obulen = rurobulen
      ustar  = rurustr
      rlwstar = wstar
   else
! ---       Rural
      urbstab = .false.
      rlwstar = wstar
   end if
   zi_sav = zi !D178

   if(.not. rlprocessed) then
!        Translate and rotate the line source to align with wind direction   --- CALL TRANSLATE_ROTATE
      call translate_rotate
   end if

!        Get translated an rotated source information
   sigmaz0 = rlsource(isrc)%init_sigmaz
   xsbegin = xsb_rot(isrc)
   ysbegin = ysb_rot(isrc)
   xsend   = xse_rot(isrc)
   ysend   = yse_rot(isrc)
   zsbegin = rlsource(isrc)%zsb
   zsend   = rlsource(isrc)%zse

!     Orient the end points so the begining has a lower Y value
   if (ysend < ysbegin) then
      xtemp   = xsend
      ytemp   = ysend
      ztemp   = zsend
      xsend   = xsbegin
      ysend   = ysbegin
      zsend   = zsbegin
      xsbegin = xtemp
      ysbegin = ytemp
      zsbegin = ztemp
   end if
   theta_line = datan2(ysend - ysbegin, xsend - xsbegin)

!        Calculate a vertical displacement of the source to
!        account for the effect of a roadside barrier                        --- CALL BARRIER_DISPLACEMENT
   if((rlsource(isrc)%htwall  > 0.0d0) .or.&
   &(rlsource(isrc)%htwall2 > 0.0d0)) then
      call barrier_displacement
   else
      nbarr = 0
      shift_flag  = .false.
      xshift      = 0.0d0
!CRT/ORD  5/17/2022 variables added to initialization
      yshift      = 0.0d0
      hbu         = 0.0d0
      hbd         = 0.0d0
   end if

!     Calculate initial sigma-y from the road width
   sigmay0 = 0.0d0
   sigmay0 = dabs(0.5d0 * (rlsource(isrc)%width) *&
   &dcos(theta_line))

!     Calculate met parameters                                               --- CALL COMPUTE_MET
   call compute_met

!     Set up interpolation coefficients for FAST option
   if(fastall) then
      call interp_coeffs
   end if

! Set elevation of source, global variable in main1 - Wood 7/5/2022
   zs = azs(isrc)

!     Begin loop over receptors
   receptor_loop: do irec = 1, numrec
!RLM D178 Reset ZI
!RLM Not necessarily needed here. Safe guard for possible other instances where ZI is
!RLM changed within the receptor loop. ZI changed within PLUME_CONC; this has been corrected with D178.
      zi = zi_sav
!MGS     D178_RLINE_RecpOrder_WSP Moved these initalizations from top of RLCALC
      concd = 0.0d0 !D178_RLINE_RecpOrder_WSP
      concentration = 0.0d0  !D178_RLINE_RecpOrder_WSP

! ----Write date and source values to RLINE.DBG
      if (rlinedbg) then
         write(rlinedbunt,'(A, (A, I8),2(" , ", A, I8))')&
         &'rline.f/RLCALC: ',&
         &'KURDAT = ', kurdat,&
         &'ISRC = ', isrc,&
         &'IREC = ', irec
      end if

!        Rotate X, Y receptors.  Z receptor not rotated.
      xr_rot = xrcp_rot(irec)
      yr_rot = yrcp_rot(irec)
      zrecep = azflag(irec)

! begin - Add terrain treatment - Wood 7/5/2022
      zflag = azflag(irec)
      zelev = azelev(irec)
      zhill = azhill(irec)
! end - Add terrain treatment - Wood 7/5/2022
!        Calculate the contribution to concentration at a
!        receptor due to a line source using Romberg integration             --- CALL NUMERICAL_LINE_SOURCE

      call numerical_line_source(concd, error)

!        Convert Qemis from MOVES units (g/hr/link) to RLINE units (g/m/s)
!        EMIFAC(1) is for concentration
!        rline_emisfact_d42_Wood, using local QEMIS
      concentration = concd * emifac(1) * qemis * rlemisconv(isrc)
      hrval(:) = concentration

!        For the initial integration of R-LINE v1.2 into AERMOD,
!        only including ARM2 chemistry options.  OLM, PVMRM and GRSM not included.
      if (arm2) then
!           Store conc by source and receptor for ARM2 options
         do ityp = 1, numtyp
            chi(irec,isrc,ityp) = hrval(ityp)
         end do

!           Initialize __VAL arrays (1:NUMTYP)
         hrval   = 0.0d0

      end if

!        Sum HRVAL to AVEVAL and ANNVAL Arrays                               --- CALL SUMVAL
      if (evonly) then
         call ev_sumval
      else
         do igrp = 1, numgrp
            call sumval
         end do
      end if

!        Initialize __VAL arrays (1:NUMTYP)
      hrval(:) = 0.0d0

!        Reset concentration value
!MGS      CONCENTRATION = 0.0D0 !D178_RLINE_RecpOrder_WSP: Moved to top of REC loop

   end do receptor_loop

!     Reset SFCZ0 to the saved SFCZ0
!     Reset ZI to ZI_SAV; D178 switch ZI_SAV to ZI_ORIG
   sfcz0  = SFCZ0_sav
   zi     = zi_orig ! D096; D178 switch ZI_SAV to ZI_ORIG

end subroutine rlcalc

subroutine barrier_displacement
!***********************************************************************
!       BARRIER_DISPLACEMENT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate a vertical displacement of the source to
!                     account for the effect of a roadside barrier.
!
!        PROGRAMMER:  M. Snyder, D. Heist
!
!        DATE:        November, 2013
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      RLINE source, receptor and barrier parameters
!
!        OUTPUTS:
!
!        CALLED FROM: RLCALC
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
!CRT 3/24/2021, D058 2 Barriers - add variables for error handling
   use main1, only: isrc, srcid, path, sfcz0
   use rline_data, only: rlsource, zsbegin, zsend,&
   &theta_line, sm_num, nbarr, bdw_flag,&
   &dwu, dw_peru, hbu, dwd, dw_perd, hbd,&
   &xshift, shift_flag, alpha_u, alpha_d, yshift

   implicit none

!     Local Variables:
   double precision      :: hb(2), dw(2), dw_per(2)
!     JAT D065 8/9/21 HBMAX SET BUT NOT USED
!      DOUBLE PRECISION      :: RECIRC_LENGTH, HBMAX, Z0
   double precision      :: recirc_length, z0
   integer               :: i
!     HB            = height of barrier; for up to 2 barriers
!     DW            = along wind distance between source and barrier; for up to 2 barriers
!     DW_PER        = perpendicular distance between source and barrier; for up to 2 barriers
!     RECIRC_LENGTH = length of recirculation zone; either 0, 4H, or 6.5H
!     HBMAX         = maximum HB; needed for 2 barrier case to pick the highest barrier height
!     Z0            = local surface roughness for calculation of UH
!     I             = index to indicate if barrier 1 or barrier 2

!     Initialize Variables:
   i        = 2     ! assume barrier 2 is present, correct if necessary
   hbd      = 0.0d0 ! height of downwind barrier
   dw_perd  = 0.0d0 ! perpendicular distance between source and downwind barrier
   dwd      = 0.0d0 ! along wind distance between source and downwind barrier

   hbu      = 0.0d0 ! height of upwind barrier
   dw_peru  = 0.0d0 ! perpendicular distance between source and upwind barrier
   dwu      = 0.0d0 ! along wind distance between source and upwind barrier

   nbarr    = 0     ! number of barriers present

   shift_flag    = .false.
   xshift        = 0.0d0
!CRT/ORD  5/17/2022 variables added to initialization
   yshift        = 0.0d0
   recirc_length = 0.0d0

   alpha_u = 1.0d0
   alpha_d = 1.0d0
   z0      = sfcz0  ! initialize local zrough for UH calculations
!     JAT D065 8/9/21 HBMAX SET BUT NOT USED
!      HBMAX   = 0.0D0  ! needed for 2 barrier case

!     Check for existence of barriers in user input
   if((rlsource(isrc)%htwall  > 0.0d0) .or.&
   &(rlsource(isrc)%htwall2 > 0.0d0)) then

!     Calculate barriers distances and source height
      zsbegin     = 0.5d0 * (zsbegin + zsend)
      zsend       = zsbegin

      hb(1)       = rlsource(isrc)%htwall
      dw_per(1)   = dabs(rlsource(isrc)%dclwall -&
      &rlsource(isrc)%dcl)
      dw(1)       = dw_per(1) / (dabs(dsin(theta_line)) + sm_num)     ! distance between source and barrier along wind direction

      hb(2)       = rlsource(isrc)%htwall2
      dw_per(2)   = dabs(rlsource(isrc)%dclwall2 -&
      &rlsource(isrc)%dcl)
      dw(2)       = dw_per(2) / (dabs(dsin(theta_line)) + sm_num)

!     Determine number of barriers (0, 1, or 2)
      if ((hb(1) > 0.0d0) .and. (hb(2) > 0.0d0)) then           ! TWO BARRIERS
         nbarr = 2
      else if ((hb(1) == 0.0d0) .and. (hb(2) == 0.0d0)) then    ! NO BARRIERS
         nbarr = 0
      else if ((hb(1) > 0.0d0) .or. (hb(2) == 0.0d0)) then      ! ONE BARRIER
         nbarr = 1
      else
         nbarr = -1
      end if

!     Assigning barriers 1 or 2 to upwind or downwind; setting barrier displacement variables for each case
      select case(nbarr)
       case(0) ! NO BARRIERS
         dw_peru   = 0.0d0
         dwu       = 0.0d0
         dw_perd   = 0.0d0
         dwd       = 0.0d0

       case(1) ! ONE BARRIER
         if (hb(1) > 0.0d0) i = 1
         if(bdw_flag(isrc,i) == 1) then                            ! located on downwind side
            hbd     = hb(i)
            dwd     = dw(i)
            dw_perd = dw_per(i)
            hbu     = 0.0d0
            dwu     = 0.0d0
            dw_peru = 0.0d0
            z0      = max(hbd / 9.0d0, sfcz0)                         ! adjusting surface reference for presence of barrier
            alpha_d = max(1.0d0, exp(0.14d0 * log(z0 / sfcz0)))       ! Venkatram & Schulte (2018) alpha; Note: exp(n*log(x)) = x**n
         else                                                        ! located on upwind side
            hbd     = 0.0d0
            dwd     = 0.0d0
            dw_perd = 0.0d0
            hbu     = hb(i)
            dwu     = dw(i)
            dw_peru = dw_per(i)
            z0      = max(hbu / 9.0d0, sfcz0)                         ! adjusting surface reference for presence of barrier
            alpha_u = max(1.0d0, exp(0.14d0 * log(z0 / sfcz0)))       ! Venkatram & Schulte (2018) alpha; Note: exp(n*log(x)) = x**n
            recirc_length = 6.5d0 * hbu                               ! recirculation zone is 6.5h when one barrier present
         end if

       case(2) ! TWO BARRIERS
         if(bdw_flag(isrc, 1) == bdw_flag(isrc, 2)) then           ! both are on same side of source
            print *, "WARNING: Both barriers associated with source ",&
            &srcid(isrc)," are on the same side of the source."
            print *, "Barrier closer to source will be used."
!CRT 3/24/2021, D058 2 Barriers - write warning to .out/.err file
            call errhdl(path,'RLINE','W','620',srcid(isrc))
            nbarr     = 1
            i         = 2                                             ! assume barrier 2 is close; change in next line if not
            if(dabs(dw_per(1)) < dabs(dw_per(2))) i = 1               ! dw_per(i) is closer to source

            if (bdw_flag(isrc, i) == 1) then                        ! barriers are downwind of source but only choosing closest barrier
               hbd     = hb(i)
               dwd     = dw(i)
               dw_perd = dw_per(i)
               hbu     = 0.0d0
               dwu     = 0.0d0
               dw_peru = 0.0d0
               z0      = max(hbd / 9.0d0, sfcz0)                       ! adjusting surface reference for presence of barrier
               alpha_d = max(1.0d0, exp(0.14d0 * log(z0 / sfcz0)))     ! Venkatram & Schulte (2018) alpha
            else                                                      ! barriers are upwind of the source but only choosing closest barrier
               hbd     = 0.0d0
               dwd     = 0.0d0
               dw_perd = 0.0d0
               hbu     = hb(i)
               dwu     = dw(i)
               dw_peru = dw_per(i)
               z0      = max(hbu / 9.0d0, sfcz0)                       ! adjusting surface reference for presence of barrier
               alpha_u = max(1.0d0, exp(0.14d0 * log(z0 / sfcz0)))     ! Venkatram & Schulte (2018) alpha
               recirc_length = 6.5d0 * hbu                             ! assuming one upwind barrier so recirculation zone is 6.5H
            end if

         else                                                        ! barriers are on opposite sides of source
            if (bdw_flag(isrc,1) == 1) then                         ! barrier 1 is downwind, barrier 2 is upwind
               hbd     = hb(1)
               dw_perd = dw_per(1)
               dwd     = dw(1)
               hbu     = hb(2)
               dw_peru = dw_per(2)
               dwu     = dw(2)
               recirc_length = 4.0d0 * hbu                             ! recirculation zone is 4H when two barriers present
            else                                                      ! barrier 2 is downwind, barrier 1 is upwind
               hbd     = hb(2)
               dw_perd = dw_per(2)
               dwd     = dw(2)
               hbu     = hb(1)
               dw_peru = dw_per(1)
               dwu     = dw(1)
               recirc_length = 4.0d0 * hbu                             ! recirculation zone is 4H when two barriers present
            end if
            z0      = max(hbu / 9.0d0, sfcz0)                         ! adjusting surface reference for presence of barrier
            alpha_u = max(1.0d0, exp(0.14d0 * log(z0 / sfcz0)))       ! Venkatram & Schulte (2018) alpha
            z0      = max(hbd / 9.0d0, sfcz0)                         ! adjusting surface reference for presence of barrier
            alpha_d = max(1.0d0, exp(0.14d0 * log(z0 / sfcz0)))       ! Venkatram & Schulte (2018) alpha
         end if

       case default
         print *, "WARNING: Barriers input problem with source ", isrc

      end select

!     Set Shift_flag for cases when there is an upwind barrier present
      if((hbu > 0.0d0) .and. (dw_peru < recirc_length)) then
         shift_flag = .true.
         xshift = dw_peru * dsin(theta_line)
         yshift = -1.0d0 * dw_peru * dcos(theta_line)
         dwd    = dwd + xshift                                    ! adjusting distance to downwind barrier due to shifting source to upwind barrier
      else
         shift_flag  = .false.
         xshift      = 0.0d0
!CRT/ORD  5/17/2022 variables added to initialization
         yshift      = 0.0d0
      end if

   end if

!      Possible future debugging print statement
!      PRINT *, "Xshift = ", Xshift, "Shift_flag = ", Shift_flag

end subroutine barrier_displacement

subroutine compute_met
!***********************************************************************
!        COMPUTE_MET Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate SIGMAV using USTAR and WSTAR.
!
!        PROGRAMMER:  A. Venkatram, M. Snyder
!
!        DATE:        November, 2013
!
!        MODIFIED:    Debug file added for the grid wind speed.
!                     Create Wind speed array for use in calc2.f/CRITDS
!                     Removed unused variables
!                     Wood, 10/11/22
!
!        MODIFIED:    Added DEBUG output to RLINE.DBG file for computed
!                     meteorlogical variables used in computation.
!                     Wood, 12/07/2021 Michelle G. Snyder & Laura Kent
!                     Added SVMIN (which can be user defined) in the place of 0.2
!                     Wood, 12/14/2021 Laura Kent
!
!        MODIFIED:    Updated sigma-v to use GRIDSV caluclated within AERMOD
!                     which also uses SVMIN value from the user.
!                     Wood, 01/3/2021
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      Meteorological variables
!
!        OUTPUTS:     SIGMAV
!
!        CALLED FROM: RLCALC
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
! Wood 10/10/22 removed ISRC, JDAY, GRIDSV, GRIDWS FROM MAIN1; NOT USED HERE
   use main1, only: ustar, obulen, urefht, uref,&
   &rlinedbunt, rlinedbg, sfcz0, svmin,&
   &gridsv, rtof2, wsmin, gridht, mxglvl,&
   &rl_gridws,&     !Added for RL_GRIDWS calculation Wood 7/5/2022
   &zi,&   !Added for RL_GRIDWS calculation Wood 7/14
   &rlinedbunt_ws,iyear,imonth,iday,ihour  !Wood GRID_WS DBUG FILE 10/11

   use rline_data, only: sigmav, fac_dispht, dispht, rlwstar,&
   &wspd_adj, uh, hbd, hbu, i_alpha,&
   &alpha_d, alpha_u,&
   &z0_a, dh_a, ust_a, lmo_a, umin, rlwstar,&
   &rlprocessed     !Wood 10/12/22
   implicit none

!     External Functions:
   double precision, external  :: most_wind

!     Local Variables:
!     DOUBLE PRECISION  :: SIGMAV_CALC, UREFCALC  ! D096
   double precision  :: urefcalc               ! D096
   double precision  :: wstar_loc
   double precision  :: alphas(3)
   double precision  :: zimech_ws !Added for RL_GRIDWS calculation Wood 7/14
   integer  :: NDXBL_Ref
   integer  :: np !added to loop over grid levels for RL_GRIDWS - Wood 7/5/2022
   integer  :: ws_i !added to loop over grid levels for RLINE grid ws dbg - Wood 10/10/22

!     UREFCALC    = theoretical value of wind speed at z = UREFHT
!     ALPHAS      = enhancement of ustar (UST) due to barrier presence

!     Variable Initialization:
   uh        = 0.0d0   ! wind speed at barrier height (H)

!     Check for WSTAR from metext.f
!      WSTAR_LOC = MAX(RLWSTAR, 0.0D0)                       ! D096

!     Calculate SIGMAV from WSTAR and USTAR variables
!     Calculate standard deviation of wind direction
!     SIGMAV_CALC = DSQRT((0.6D0 * WSTAR_LOC)**2 +          ! D096
!    &              (1.9D0 * USTAR)**2)                     ! D096
!     SIGMAV      = MAX(SIGMAV_CALC, SVMIN)                 ! D096

!     Obtain Sigma-V value from GRIDSV at the reference height by using GINTRP
   call locate(gridht, 1, mxglvl, urefht, NDXBL_Ref)     ! D096

   call gintrp( gridht(NDXBL_Ref), gridsv(NDXBL_Ref),&    ! D096
   &gridht(NDXBL_Ref+1), gridsv(NDXBL_Ref+1),&  ! D096
   &urefht, sigmav)                            ! D096
   sigmav      = max(sigmav, svmin)                      ! D096

! ----Write met variables to RLINE.DBG
   if (rlinedbg) then
      write(rlinedbunt,'(A, (A, F5.3),(",", A, F5.3))')&
      &'rline.f/COMPUTE_MET: ',&
      &'USTAR = ', ustar,&
      &'SIGMAV = ', sigmav
   end if

!     Calculate z0 and dh for three cases - no barrier and two possible barrier heights
   z0_a(1)  = sfcz0
   z0_a(2)  = max(hbd / 9.0d0, sfcz0)                                  ! adjusting surface reference for presence of barrier
   z0_a(3)  = max(hbu / 9.0d0, sfcz0)                                  ! adjusting surface reference for presence of barrier
   dh_a(:)  = fac_dispht * z0_a(:)
   dispht = dh_a(1)
   alphas(1) = 1.0d0
   alphas(2) = alpha_d
   alphas(3) = alpha_u
   ust_a(:)    = ustar * alphas(:)
   lmo_a(:)    = obulen * alphas(:)**3
!                                                                            --- CALL MOST_WIND
   call create_wind_table
!     From Bentham & Britter (AE, 2003), assuming z0 = H/10, H = roughness element heights
   i_alpha  = 1
   umin     = max(4.47*ust_a(i_alpha), rtof2 * sigmav)
   umin     = max(umin, wsmin)
   wspd_adj = 1.0d0                      ! D096
   urefcalc = most_wind(urefht,i_alpha)
   wspd_adj = uref / urefcalc

   if (hbd > 0.0d0) then
!                                                                            --- CALL MOST_WIND
      i_alpha = 2
!          UH      = MOST_WIND(HBD,I_ALPHA) * WSPD_ADJ ! D096
      uh      = most_wind(hbd,i_alpha) ! D096
   end if

! begin ---     Create Wind speed array for use in calc2.f/CRITDS - Wood 7/5/2022
!         Modified to only use I_ALPHA=1 (no barrier), since the barrier open will still require FLAT MODELOPT.
!         Note, the array still has 2nd dimension with length 3, but they are set equal. - Wood 8/16/2022

   rl_gridws(:,:) = 0.0d0
!      ZIMECH_WS = MOST_WIND(ZI,1)  * WSPD_ADJ !Compute wind speed at top of boundary layer (ZI) Wood 8/16/2022 D096
   zimech_ws = most_wind(zi,1)  ! Compute wind speed at top of boundary layer (ZI) Wood 8/16/2022 D096

   do np = 1, mxglvl !Number of Grid levels
      do i_alpha = 1,1 ! For each Barrier case (none, downwind, upwind) - set to 1, since barriers will not consider terrain
         rl_gridws(np,:) =&
!     &                    MOST_WIND(GRIDHT(NP),I_ALPHA)  * WSPD_ADJ D096
         &most_wind(gridht(np),i_alpha) !  D096

      end do
   end do
! end ---     Create Wind speed array for use in calc2.f/CRITDS - Wood 7/5/2022
! --- Write components and values to RLINE_GRIDWS.DBG
!     Added gridded wind speed debug file call Wood 10/11/22
!     To keep from being repeated for each source use the RLPROCESSED logical
   if (rlinedbg) then
      if(.not. rlprocessed) then
         do ws_i = 1,mxglvl
            write(rlinedbunt_ws,'(1X,4(2X,I2),F8.2, 1X, F10.7)')&
            &iyear,imonth,iday,ihour,gridht(ws_i),(rl_gridws(ws_i,1))
         end do
      end if
   end if

end subroutine compute_met

subroutine create_exp_table
!***********************************************************************
!        CREATE_EXP_TABLE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Create a lookup table based on arguments of the
!                     built-in exponential function to improve
!                     computation time.
!
!        PROGRAMMER:  A. Venkatram
!
!        DATE:        November, 2013
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM: RLCALC
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use rline_data, only: xexp, aexp, bexp, delexp
   implicit none

!     Local Variables:
   integer  :: ind
   double precision, dimension(1000)  :: ext
!     IND         = local source index
!     EXT         = exponent

!     Create look-up table
   delexp  = 20.0d0 / 999.0d0
   xexp(1) = -20.0d0
   ext(1)  = dexp(xexp(1))

   do ind = 2, 1000
      xexp(ind) = xexp(ind - 1) + delexp
      ext(ind)  = dexp(xexp(ind))
   end do

   do ind = 1, 999
      bexp(ind) = (ext(ind + 1) - ext(ind)) /&
      &(xexp(ind + 1) - xexp(ind))
      aexp(ind) = ext(ind) - bexp(ind) * xexp(ind)
   end do

   bexp(1000) = bexp(999)
   aexp(1000) = aexp(999)

end subroutine create_exp_table

subroutine create_wind_table
!***********************************************************************
!        CREATE_WIND_TABLE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Create a lookup table for wind speed based
!                     on Monin-Obukhov similarity theory.
!
!        PROGRAMMER:  D. K. Heist
!
!        DATE:        January 2022
!
!
!        INPUTS:
!
!        OUTPUTS:     Lookup table for velocity profile
!
!        CALLED FROM: COMPUTE_MET
!
!        References:
!***********************************************************************
! Wood 10/10/22 removed ISRC FROM MAIN1; NOT USED HERE
!      USE MAIN1, ONLY: STABLE, VONKAR, ZI, ISRC, PI
   use main1, only: stable, vonkar, zi, pi
! Wood 10/10/22 removed SIGMAV FROM RLINE_DATA; NOT USED HERE
!      USE RLINE_DATA, ONLY: SIGMAV, LOGZMIN, LOGZMAX,
   use rline_data, only: logzmin, logzmax,&
   &zwind, awind, bwind, delz,&
   &z0_a, dh_a, ust_a, lmo_a, np

   implicit none

   double precision  :: x1(np,3), psi1(np,3), utbl(np,3)
   double precision  :: x2(3), psi2(3)
   double precision  :: expdelz
   integer           :: iz, ia

!     Z0_A        = surface roughness length
!     DH_A        = displacement height
!     UST_A       = surface friction velocity
!     LMO_A       = Monin-Obukhov length
!     X1          = computation within PSI1
!     X2          = computation within PSI2
!     PSI1        = stability function
!     PSI2        = stability function
!     ZWIND       = Heights at which to calculate wind speed
!     EXPDELZ     = EXP(DELZ), Exponential of the increment in ZWIND
!     AWIND       = Intercept used to estimate wind speed at a given height
!     BWIND       = Slope used to estimate wind speed at a given height
!     UTBL        = Wind speed table

!     Create heights for wind table - ZWIND
   do ia = 1, 3
      logzmax     = max(dlog(500.0d0),dlog(zi))
      logzmin(ia) = dlog(z0_a(ia) + dh_a(ia))
      delz(ia)    = (logzmax - logzmin(ia))/dble(np - 1)
      expdelz     = dexp(delz(ia))

!     Create heights for wind table - ZWIND
!     These heights are log-spaced for better resolution near the ground.
      zwind(1,ia) = z0_a(ia) + dh_a(ia)
      do iz = 2, np
         zwind(iz,ia) = zwind(iz - 1,ia) * expdelz
      end do
   end do

!     Create wind speed table - UTBL
   if (stable) then
!MGS        LMO_A(:)     = ABS(LMO_A(:)) !D178_RLINE_RecpOrder_WSP
      lmo_a(:)     = dabs(lmo_a(:))
      do ia = 1, 3
         psi1(:,ia) = -17.0d0 * (1.0d0 - dexp(-0.29d0 *&
         &(zwind(:,ia) - dh_a(ia)) / lmo_a(ia)))
         psi2(ia)   = -17.0d0 * (1.0d0 - dexp(-0.29d0 *&
         &z0_a(ia) / lmo_a(ia)))
      end do

   else
!MGS        LMO_A(:)   = -1.0D0 * ABS(LMO_A(:)) !D178_RLINE_RecpOrder_WSP
      lmo_a(:)   = -1.0d0 * dabs(lmo_a(:))
      do ia = 1, 3
!MGS          X1(:,IA) = SQRT(SQRT(1.0D0 - 16.0D0 * !D178_RLINE_RecpOrder_WSP
         x1(:,ia) = dsqrt(dsqrt(1.0d0 - 16.0d0 *&
         &(zwind(:,ia) - dh_a(ia)) /&
         &lmo_a(ia)))
!MGS          X2(IA)   = SQRT(SQRT(1.0D0 - 16.0D0 * !D178_RLINE_RecpOrder_WSP
         x2(ia)   = dsqrt(dsqrt(1.0d0 - 16.0d0 *&
         &z0_a(ia) / lmo_a(ia)))
         psi1(:,ia) = 2.0d0 * dlog((1.0d0 + x1(:,ia)) / 2.0d0) +&
         &dlog((1.0 + x1(:,ia) * x1(:,ia)) / 2.0d0) -&
         &2.0d0 * datan(x1(:,ia)) + pi / 2.0d0
         psi2(ia)   = 2.0d0 * dlog((1.0d0 + x2(ia)) / 2.0d0) +&
         &dlog((1.0 + x2(ia) * x2(ia)) / 2.0d0) -&
         &2.0d0 * datan(x2(ia)) + pi / 2.0d0
      end do

   end if

   do ia = 1,3
      utbl(:,ia)    = ust_a(ia) *&
      &(dlog((zwind(:,ia) - dh_a(ia)) / z0_a(ia)) -&
      &psi1(:,ia) + psi2(ia)) / vonkar
   end do

!     Ensure there are no negative values for wind speed due to slight rounding
   where(utbl < 0.0d0) utbl = 0.0d0

!     Calculate slope and intercept for each height for use in MOST_WIND function
   do ia = 1,3
      do iz = 1,np-1
         bwind(iz,ia) = (utbl(iz+1,ia) - utbl(iz,ia)) /&
         &(zwind(iz+1,ia) - zwind(iz,ia))
      end do
      bwind(np,ia) = bwind(np-1,ia)
      awind(:,ia)    = utbl(:,ia) - bwind(:,ia) * zwind(:,ia)
      awind(np,ia) = awind(np-1,ia)
   end do

end subroutine create_wind_table


double precision function depressed_displacement(theta_line,ind)
!***********************************************************************
!        DEPRESSED_DISPLACEMENT Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Computes transformation for a depressed roadway,
!                     shifting the roadway upwind, and compresses or expands
!                     the roadway based on the fractional width and distance
!                     from the centerline.
!
!        PROGRAMMER:  M. Snyder, D. Heist
!
!        DATE:        November, 2013
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      THETA_LINE, IND
!
!        OUTPUTS:
!
!        CALLED FROM: TRANSLATE_ROTATE
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use rline_data, only: rlsource, thetaw
   implicit none

!     Local Variables:
   integer  :: ind
   double precision  :: theta_line
   double precision  :: depth, wtop, wbottom, dcl
   double precision  :: effd, reld, fracw, effw, theta_rel, dcrit, f
!     IND         = local source index
!     THETA_LINE  = angle between wind direction and source
!     DEPTH       = depth of depression
!     WTOP        = width of top of depression
!     WBOTTOM     = width of bottom of depression
!     DCL         = offset distance from center line
!     EFFD        = effective depth
!     RELD        = relative roadway depth
!     FRACW       = fractional width
!     EFFW        = effective width
!     THETA_REL   = relative angle between roadway and wind direction
!     DCRIT       = critical depth
!     F           = effective wind fraction

   depth     = rlsource(ind)%depth
   wbottom   = rlsource(ind)%wbottom
   wtop      = rlsource(ind)%wtop
   dcl       = rlsource(ind)%dcl

   theta_rel = theta_line - thetaw

   effd      = (wbottom * dabs(depth) + ((wtop - wbottom) /&
   &2d0 * dabs(depth))) / wtop
   reld      = effd / wbottom

   if (reld >= 0.0483d0) then
      fracw = 1.0d0
   else
      fracw = -0.0776d0 + dsqrt(1.506d0 - 7.143d0 * reld)
   end if

   effw    = fracw**(1.0d0 - (dcos(dabs(theta_rel)) *&
   &dcos(dabs(theta_rel)))) * wbottom
   dcrit   = 0.2064d0 * wtop * wbottom / (0.5d0 * (wtop + wbottom))
   f       = min(1.0d0, wbottom / wtop *&
   &(1.0d0 + dabs(depth) / dcrit))

   depressed_displacement = ((wtop * f - effw) / 2.0d0)*&
   &((dsin(theta_rel))**2) *&
   &dsign(1.0d0, dsin(theta_rel)) -&
   &(effw / wbottom * dcl) *&
   &dsign(1.0d0, dsin(theta_line))

end function depressed_displacement

subroutine effective_wind(xd,heff,hshift)
!***********************************************************************
!        EFFECTIVE_WIND Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate the effective wind speed at mean
!                     plume height.
!
!        PROGRAMMER:  A. Venkatram, M. Snyder, D. Heist
!
!        DATE:        November, 2013
!
!        MODIFIED:    Added RLINE debug file call to write out mean plume height
!                     WSP 10/10/22 formerly Wood
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      XD, HEFF, HSHIFT
!
!        OUTPUTS:
!
!        CALLED FROM: MEANDER
!                     POINT_CONC
!
!        CALLING
!        ROUTINES:    EXPX
!                     MOST_WIND
!                     SIGMAZ
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use main1, only: rtof2, rt2bypi, rlinedbg, rlinedbunt
! JAT 06/22/21 DO65 REMOVE DISPHT AS VARIABLE FROM RLINE_DATA; NOT USED HERE
! Wood 10/10/22 removed Z0_A, UST_A, LMO_A, DH_A FROM RLINE_DATA; NOT USED HERE
!      USE RLINE_DATA, ONLY: UEFF, SIGMAV, WSPD_ADJ, DISPHT,
   use rline_data, only: ueff, sigmav, wspd_adj,&
!     &    I_ALPHA, Z0_A, DH_A, UST_A, LMO_A,
   &i_alpha,&
   &pu1, pu2, pu3, pu4, fastrline

   implicit none

!     External Functions:
   double precision, external  :: sigmaz, most_wind, expx

!     Local Variables:
   integer  :: iter
   double precision  :: erf
   double precision  :: sz, sz_new, err, zbar
   double precision, intent(in)  :: xd, heff, hshift
!     ITER        = iteration
!     ERF         = error function
!     SZ          = effective SIGMAZ
!     SZ_NEW      = intermediate vertical dispersion
!     ERR         = error in each successive calculation
!     ZBAR        = mean plume height
!     XD          = perpendicular distance between source and receptor
!     HEFF        = effective source height
!     HSHIFT      = vertical shift in source height for barriers

   if(fastrline) then
      if(xd <= 10) then
         ueff   = pu1 + pu2 * log(abs(xd))
      else
         ueff   = pu3 + pu4 * log(abs(xd))
      end if
      return
   end if

!     Initialize variables:
   zbar = 0.0d0
   err  = 10.0d0
   iter = 1

!                                                                            --- CALL MOST_WIND
!      UEFF = MOST_WIND(HEFF,I_ALPHA) * WSPD_ADJ ! D096
   ueff = most_wind(heff,i_alpha) ! * WSPD_ADJ D096
!      UEFF = DSQRT(2.0D0 * SIGMAV**2  + UEFF**2)  ! D096
   sz   = sigmaz(xd)
!                                                                            --- CALL EXPX
!MGS      DO WHILE ((ERR > 1.0E-02) .and. (ITER < 20)) !D178_RLINE_RecpOrder_WSP
   do while ((err > 1.0d-02) .and. (iter < 20))
      zbar   = rt2bypi * sz * expx(-0.5d0 * (heff / sz)**2) +&
      &heff * erf(heff / (rtof2 * sz)) + hshift                   ! Venkatram et al. (2013)
!                                                                            --- CALL MOST_WIND
!         UEFF   = MOST_WIND(MAX(ZBAR, HEFF), I_ALPHA) *     ! D096
!     &            WSPD_ADJ                                  ! D096
      ueff   = most_wind(max(zbar, heff), i_alpha) ! *  D096
!     &            WSPD_ADJ                           !    D096
!         UEFF   = DSQRT(2.0D0 * SIGMAV**2  + UEFF**2)  ! D096
      sz_new = sigmaz(xd)
      err    = dabs((sz_new - sz) / sz)
      sz     = sz_new
      iter   = iter + 1
   end do
! ----Write components and values to RLINE.DBG
!     Added debug file call to write out mean plume height Wood 10/10/22
   if (rlinedbg) then
      write(rlinedbunt,&
      &'(A, A, F9.3)')&
      &'rline.f/EFFECTIVE_WIND: ',&
      &'ZBAR = ', zbar
   end if
end subroutine effective_wind

double precision function expx(xp)
!***********************************************************************
!        EXPX Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Computes the exponential function using a table.
!
!        PROGRAMMER:  A. Venkatram
!
!        DATE:        November, 2013
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      XP
!
!        OUTPUTS:
!
!        CALLED FROM: EFFECTIVE_WIND
!                     MEANDER
!                     POINT_CONC
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use rline_data, only: delexp, aexp, bexp, xexp
   implicit none

!     Local Variables:
   integer  :: p
   double precision  :: xpd
   double precision, intent(in) :: xp
!     P           = exponential table index
!     XPD         = closest precalculated exponent value
!     XP          = input exponent value


   xpd  = xp
   xpd  = max(xexp(1), xpd)
   p    = floor((xpd + 20.0d0) / delexp) + 1.0d0
   expx = aexp(p) + bexp(p) * xpd

end function expx

subroutine  interp_coeffs
!***********************************************************************
!        INTERP_COEFFS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate interpolation coefficients for FASTALL
!                     option. Coefficients are calcuated for UEFF,
!                     SIGMAY, and SIGMAZ. Two sets of coefficients
!                     for each variable are computed: one for x <= 10 m,
!                     and another for x > 10 m. This interpolation
!                     was adapted from the CALINE series of models.
!
!        PROGRAMMER:  D. Heist, EPA
!
!        DATE:        Dec 3, 2021
!
!        MODIFIED:
!
!        INPUTS:
!
!        OUTPUTS:     PY1, PY2, PY3, PY4, PZ1, PZ2, PZ3, PZ4
!                     PU1, PU2, PU3, PU4
!
!        CALLED FROM: RLCALC
!
!        CALLING
!        ROUTINES:
!
!***********************************************************************

!     Variable Declarations:
   use rline_data, only: ueff, zsbegin, zsend,&
   &psy1, psy2, psy3, psy4,&
   &psz1, psz2, psz3, psz4,&
   &pu1, pu2, pu3, pu4, alpha,&
   &fastrline, i_alpha
   implicit none

!     External Functions:
   double precision, external :: sigmay, sigmaz

!     Local Variables:
   double precision :: u1, u2, u3
   double precision :: sy1, sy2, sy3, lnsy1, lnsy2, lnsy3
   double precision :: sz1, sz2, sz3, lnsz1, lnsz2, lnsz3
   double precision :: x1, x2, x3, lnx1, lnx2, lnx3
   double precision :: hshift, zs

!     X1, X2, X3  = Distances at which functions are evaluated
!     HSHIFT      = Used in barrier algorithm, set to zero for FAST option
!     Set FASTRLINE to FALSE to calculate interp coeffs using the real functions,
!     then set to TRUE to use the interpolation functions in the receptor loop.

   fastrline = .false.
   alpha   = 1.0d0
   i_alpha = 1          ! D096
   hshift = 0.0d0
   x1     = 1.0d0
   x2     = 10.0d0
   x3     = 500.0d0
   lnx1   = 0.0d0
   lnx2   = log(x2)
   lnx3   = log(x3)
   zs     = 0.5d0 * (zsbegin + zsend)

!     Location 1 calculations
   call effective_wind(x1, zs, hshift)
   u1      = ueff
   sz1     = sigmaz(x1)
   lnsz1   = log(sz1)
   sy1     = sigmay(x1)
   lnsy1   = log(sy1)

!     Location 2 calculations
   call effective_wind(x2, zs, hshift)
   u2      = ueff
   sz2     = sigmaz(x2)
   lnsz2   = log(sz2)
   sy2     = sigmay(x2)
   lnsy2   = log(sy2)

!     Location 3 calculations
   call effective_wind(x3, zs, hshift)
   u3      = ueff
   sz3     = sigmaz(x3)
   lnsz3   = log(sz3)
   sy3     = sigmay(x3)
   lnsy3   = log(sy3)

!     Calculate interpolation coeffs for Ueff
   pu2     = (u2 - u1) / (lnx2 - lnx1)
   pu1     = u2 - pu2 * lnx2
   pu4     = (u3 - u2) / (lnx3 - lnx2)
   pu3     = u2 - pu4 * lnx2

!     Calculate interpolation coeffs for SIGMAY
   psy2    = (lnsy2 - lnsy1) / (lnx2 - lnx1)
   psy1    = exp(0.5d0 * (lnsy1 + lnsy2 - psy2 * (lnx1 + lnx2)))
   psy4    = (lnsy3 - lnsy2) / (lnx3 - lnx2)
   psy3    = exp(0.5d0 * (lnsy2 + lnsy3 - psy4 * (lnx2 + lnx3)))

!     Calculate interpolation coeffs for SIGMAY
   psz2    = (lnsz2 - lnsz1) / (lnx2 - lnx1)
   psz1    = exp(0.5d0 * (lnsz1 + lnsz2 - psz2 * (lnx1 + lnx2)))
   psz4    = (lnsz3 - lnsz2) / (lnx3 - lnx2)
   psz3    = exp(0.5d0 * (lnsz2 + lnsz3 - psz4 * (lnx2 + lnx3)))

   fastrline = .true.
end subroutine interp_coeffs

double precision function meander(x, y, z, hshift)
!***********************************************************************
!        MEANDER Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate the contribution of a point source
!                     at (X,Y,Z) to a receptor at (Xr_rot,Yr_rot,Zrcp),
!                     assuming that the material spreads out radially
!                     in all directions.
!
!        PROGRAMMER:  A. Venkatram, M. Snyder, D. Heist
!
!        DATE:        November, 2013
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      X, Y, Z, HSHIFT
!
!        OUTPUTS:
!
!        CALLED FROM: POINT_CONC
!
!        CALLING
!        ROUTINES:    EXPX
!                     EFFECTIVE_WIND
!                     SIGMAZ
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use main1,      only: pi, rt2bypi
   use rline_data, only: xr_rot, yr_rot, xd_min, zrecep, ueff,&
   &xd_min, hbd, hbu
   implicit none

!     External functions:
   double precision, external :: sigmaz, expx

!     Local Variables:
   double precision  :: r, vert, horz, sz, heff
   double precision, intent(in)  :: x, y, z, hshift
!     R           = radial distance to receptor
!     VERT        = vertical component of concentration
!     HORZ        = horizontal component of concentration
!     SZ          = effective SIGMAZ
!     HEFF        = effective height
!     X           = x-coordinate of source location
!     Y           = y-coordinate of source location
!     Z           = z-coordinate of source location
!     HSHIFT      = vertical shift in source height for barriers

   r       = dsqrt((xr_rot - x)**2 + (yr_rot - y)**2)                     ! radial distance to the receptor
   r       = max(r, xd_min)
   heff    = max(z, 0.75d0 * max(hbu, hbd))                               ! if no barrier, heff = Z; if barrier, heff = 0.75*H using largest H

!     Calculate effective wind speed                                         --- CALL EFFECTIVE_WIND
   call effective_wind(r,heff,hshift)

!     Account for source height                                              --- CALL EXPX
   sz      = sigmaz(r)
   vert    = rt2bypi * (expx(-0.5d0 * ((heff - zrecep)&
   &/ sz)**2) + expx(-0.5d0 *&
   &((heff + zrecep) / sz)**2))/(2.0d0*sz*ueff)

   horz    = 1.0d0 / (2.0d0 * pi * r)
   meander = vert * horz

end function meander

double precision function most_wind(z, ia)
!***********************************************************************
!        MOST_WIND Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Computes the wind speed using a look-up table
!                     created in CREATE_WIND_TABLE.
!
!        PROGRAMMER:  D. K. Heist
!
!        DATE:        December 2020
!
!
!        INPUTS:      Z, IA
!
!        OUTPUTS:
!
!        CALLED FROM: COMPUTE_MET
!                     EFFECTIVE_WIND
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations
! Wood 10/10/22 removed LOGZMAX FROM RLINE_DATA; NOT USED HERE
!      USE RLINE_DATA, ONLY: LOGZMAX, LOGZMIN, DELZ, UMIN,
   use rline_data, only: logzmin, delz, umin,&
   &zwind, awind, bwind, np,&
   &wspd_adj    ! D096
   implicit none

   integer  :: p, ia
   double precision  :: zd, umost
   double precision, intent(in) :: z
!     Z          = input height value
!     P          = velocity table index for closest height
!     ZD         = closest precalculated height

   zd        = max(zwind(1,ia), z)
   p         = int((dlog(zd) - logzmin(ia))/delz(ia)) + 1
   p         = min(p, np)
   umost     = awind(p,ia) + bwind(p,ia) * zd   ! D096
!      UMOST     = UMOST * WSPD_ADJ   ! D096
!      MOST_WIND = MAX(UMOST,UMIN)    ! D096

   umost     = max(umost,umin)     ! D096
   most_wind = max(umost * wspd_adj,umin)    ! D096

end function most_wind

subroutine numerical_line_source(conc_num,err)
!***********************************************************************
!     NUMERICAL_LINE_SOURCE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate the contribution to concentration
!                     at a receptor due to a line source using Romberg
!                     integration wind speed from similarity theory.
!
!        PROGRAMMER:  A. Venkatram, M. Snyder, D. Heist
!
!        DATE:        November, 2013
!
!        MODIFIED:    Added DEBUG output to RLINE.DBG file for each
!                     integration step, computes the total number of points
!                     and average dispersion values for VERT, HORZ, and FRAN
!                     used to compute the total concentration for each step
!                     of the integration.
!                     Wood, 12/07/2021 Michelle G. Snyder & Laura Kent
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:
!
!        OUTPUTS:     CONC_NUM, ERR
!
!        CALLED FROM: RLCALC
!
!        CALLING
!        ROUTINES:    POINT_CONC
!                     POLYINTERP
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use rline_data, only: xsbegin, ysbegin, zsbegin, xsend, ysend,&
   &zsend, xr_rot, yr_rot, sm_num, xper,&
   &error_limit, xd_min, theta_line,&
   &fran_sum, sigmav_sum, ueff_sum,&
   &vert_sum, horz_sum, conc_p_sum, conc_m_sum,&
   &point_conc_sum

   use main1, only:  rlinedbg, rlinedbunt
   implicit none

!     External Functions:
   double precision, external  :: point_conc

!     Local Variables:
   integer  :: no_points, j, is, minj, it_lim, tot_no_points
   integer  :: allocatestatus, allocerror
   integer  :: st, fi
!     NO_POINTS       = number of points added each step
!     J               = index
!     IS              = index for integration of point sources
!     MINJ            = minimum number of iterations
!     IT_LIM          = maximum number of iterations
!     ALLOCATESTATUS  = flag for intermediate array allocation error
!     ALLOCERROR      = status flag for error of allocation
!     ST              = starting indice
!     FI              = finishing indice

   double precision  :: xdif, ydif, zdif
   double precision  :: disp
   double precision  :: hdum(3), concdum(3)
   double precision  :: conc_int
   double precision  :: x, y, z, delt, phi
   double precision  :: t, tmax, cost, sint, sinp, cosp, xrec
   double precision  :: dr
!     XDIF           = x integral limit
!     YDIF           = y integral limit
!     ZDIF           = z integral limit
!     DISP           = dummy variable used to store integral
!     HDUM           = step size
!     CONCDUM        = successive concentration approximations
!     CONC_INT       = numerical integrals
!     X              = x-coordinate of source location
!     Y              = y-coordinate of source location
!     Z              = z-coordinate of source location
!     DELT           = distance between points used to estimate line source
!     PHI            = angle of line elevation
!     T              = half of DELT
!     TMAX           = 3-dimensional
!     COST           = cosine of theta_line
!     SINT           = sin of theta_line
!     SINP           = sin of phi
!     COSP           = cosine of phi
!     XREC           = x-coordinate of point on source directly upwind of receptor
!     DR             = distance beetween source and receptor in wind direction

   double precision, intent(out)  :: conc_num
   double precision, intent(out)  :: err
!     CONC_NUM       = numerical routine output concentration
!     ERR            = integration is set to an arbitrarily large value before it is reduced

   double precision, allocatable  :: h(:), conc(:)
!     H              =  step size
!     CONC           =  successive concentration approximations

!     2K is the order of Romberg integration.  Nmax needs to be greater than K for Romberg integration to be used,
!     otherwise trapezoidal integration is used
   integer, parameter  :: k = 3

!     Computation Parameters
   double precision, parameter :: xinterp = 0.0d0, a = 1.0d0

!     Variable Initializations
   conc_num = 0.0d0
   err = 2.0d0*error_limit !D178_RLINE_RecpOrder_WSP

   xdif  = xsend - xsbegin
   ydif  = ysend - ysbegin
   zdif  = zsend - zsbegin

   tmax  = dsqrt(xdif * xdif + ydif * ydif + zdif * zdif)
   phi   = dasin(zdif / (tmax + sm_num))
   sinp  = dsin(phi)
   cosp  = dcos(phi)
   cost  = dcos(theta_line)
   sint  = dsin(theta_line)

!     Find x-coordinate of point on source line directly upwind of receptor
   xrec = (xsend - (ysend - yr_rot) * (xsend - xsbegin) /&
   &(ysend - ysbegin))
   dr   = dabs(xr_rot - xrec)

!     Prevent user from placing receptor on source
   dr   = max(xd_min, dr)
   xper = dsign(a, xr_rot - xrec) * dabs(-1.0d0 * ydif *&
   &(xr_rot - xsbegin) + xdif * (yr_rot - ysbegin)) /&
   &dsqrt(xdif**2 + ydif**2)

!     Convergence Criteria: Minimum Iterations
   if ((yr_rot > ysbegin - tmax / 2.0d0 * dabs(cost)) .and.&
   &(yr_rot < ysend + tmax / 2.0d0 * dabs(cost))) then
      minj = ceiling(dlog(2.0d0 * tmax /&
      &(max(xd_min, dr * dabs(sint))) -&
      &2.0d0) / dlog(2.0d0)) + 2.0d0
   else
!        Set MINJ = 0 so if receptor is upwind, the conc will converge quickly
      minj = 0
   end if

!     If receptor is upwind of the line
   if ((xr_rot < xsbegin) .and. (xr_rot < xsend)) minj = 0

   it_lim = max(10, 2 * minj)

   allocate(h(it_lim), stat = allocatestatus)
   allocate(conc(it_lim), stat = allocatestatus)

!     Compute concentration at receptor
!     Initialize concentrations
   conc(:) = 0.0d0
   h(:)    = 0.0d0

!     Initialize variables for DEBUG file
   tot_no_points = 0
   fran_sum = 0.0d0
   sigmav_sum = 0.0d0
   ueff_sum = 0.0d0
   vert_sum = 0.0d0
   horz_sum = 0.0d0
   conc_p_sum = 0.0d0
   conc_m_sum = 0.0d0
   point_conc_sum = 0.0d0

!                                                                            --- CALL POINT_CONC
   disp    = (point_conc(xsbegin, ysbegin, zsbegin) +&
   &point_conc(xsend, ysend, zsend)) * 0.5d0

! ----Write components and values to RLINE.DBG; note: the "+2" is for the first iteration where both end points are used.
!     This is only output for iteration 1, J & NO_POINTS not set till loop below.
   if (rlinedbg) then
      no_points = 2
      tot_no_points = tot_no_points + no_points
      j = 1
      write(rlinedbunt,&
      &'(A,I4,2(" , ",A,I4)," , ",(A, F5.3),2(" , ", A, F7.3))')&
      &'rline.f/NUMERICAL_LINE_SOURCE: LAST ITERATION = ', j,&
      &'NO_POINTS_ADDED = ', no_points,&
      &'TOT_NO_POINTS = ', tot_no_points,&
      &'FRAN_AVG = ', fran_sum/tot_no_points,&
      &'SIGMAV_AVG = ', sigmav_sum/tot_no_points,&
      &'UEFF_AVG = ', ueff_sum/tot_no_points

      write(rlinedbunt,&
      &'((A,E9.3),5(" , ", A, E9.3))')&
      &'                           VERT_AVG = ', vert_sum/tot_no_points,&
      &'HORZ_AVG = ', horz_sum/tot_no_points,&
      &'CONC_P_AVG = ', conc_p_sum/tot_no_points,&
      &'CONC_M_AVG = ', conc_m_sum/tot_no_points,&
      &'POINT_CONC_AVG = ', point_conc_sum/tot_no_points,&
      &'CONC_NUM = ', disp
   end if

!     Calculate first approximation of the integration.  Set relative size of
!     integration interval
   conc(1) = disp * tmax
   h(1)    = 1.0d0
!     Trapezoidal integration
   do j = 2, it_lim
      no_points = 2**(j - 2)
      delt      = tmax / no_points
      t         = delt / 2.0d0
      disp      = 0.0d0
      do is = 1, no_points
         x      = t * cost * cosp + xsbegin
         y      = t * sint * cosp + ysbegin
         z      = t * sinp + zsbegin
         disp   = disp + point_conc(x, y, z)
         t      = t + delt
      end do
      conc(j) = (disp * delt + conc(j - 1)) / 2.0d0
!        See page 134 in "Numerical Receipes" for an explanation
      h(j)    = 0.25d0 * h(j - 1)


! Keep track of total number of points int he integration (for DEBUG file & average calcs in DEBUG file)
      tot_no_points = tot_no_points + no_points

!        Romberg integration is invoked if (J >= K)
      if (j >= k) then
         st       = j - k + 1
         fi       = st + k - 1
         hdum     = h(st:fi)
         concdum  = conc(st:fi)
!           Extrapolate to H=0.0 to compute integral                         --- CALL POLYINTERP
         call polyinterp(conc_int, err, hdum, concdum, xinterp, k)
         conc_num = dabs(conc_int)

!           Check convergence criteria
         if ((dabs(err) < error_limit) .and. (j > minj)) then
            deallocate(h, conc, stat = allocerror)
! ----  Write components and values to RLINE.DBG
!       This is for iterations when J >= K (where K=3 above).
            if (rlinedbg) then
               write(rlinedbunt,&
               &'(A,I4,2(" , ",A,I4)," , ",(A, F5.3),2(" , ", A, F7.3))')&
               &'rline.f/NUMERICAL_LINE_SOURCE: LAST ITERATION = ', j,&
               &'NO_POINTS_ADDED = ', no_points,&
               &'TOT_NO_POINTS = ', tot_no_points,&
               &'FRAN_AVG = ', fran_sum/tot_no_points,&
               &'SIGMAV_AVG = ', sigmav_sum/tot_no_points,&
               &'UEFF_AVG = ', ueff_sum/tot_no_points

               write(rlinedbunt,'((A,E9.3),5(" , ", A, E9.3))')&
               &'                           VERT_AVG = ', vert_sum/tot_no_points,&
               &'HORZ_AVG = ', horz_sum/tot_no_points,&
               &'CONC_P_AVG = ', conc_p_sum/tot_no_points,&
               &'CONC_M_AVG = ', conc_m_sum/tot_no_points,&
               &'POINT_CONC_AVG = ', point_conc_sum/tot_no_points,&
               &'CONC_NUM = ', conc_num
            end if
            return
         end if

      end if

      conc_num = dabs(conc(j))

! ----Write components and values to RLINE.DBG
!     This is only output for iteration 2 when J = 2.
      if (rlinedbg) then
         write(rlinedbunt,&
         &'(A,I4,2(" , ",A,I4)," , ",(A, F5.3),2(" , ", A, F7.3))')&
         &'rline.f/NUMERICAL_LINE_SOURCE: LAST ITERATION = ', j,&
         &'NO_POINTS_ADDED = ', no_points,&
         &'TOT_NO_POINTS = ', tot_no_points,&
         &'FRAN_AVG = ', fran_sum/tot_no_points,&
         &'SIGMAV_AVG = ', sigmav_sum/tot_no_points,&
         &'UEFF_AVG = ', ueff_sum/tot_no_points

         write(rlinedbunt,&
         &'((A,E9.3),5(" , ", A, E9.3))')&
         &'                           VERT_AVG = ', vert_sum/tot_no_points,&
         &'HORZ_AVG = ', horz_sum/tot_no_points,&
         &'CONC_P_AVG = ', conc_p_sum/tot_no_points,&
         &'CONC_M_AVG = ', conc_m_sum/tot_no_points,&
         &'POINT_CONC_AVG = ', point_conc_sum/tot_no_points,&
         &'CONC_NUM = ', conc_num
      end if


   end do



end subroutine numerical_line_source

subroutine plume_conc (xd, yd, z, dwdt, heff, hshift,&
&hbdeff, hbmax, conc_plume)
!***********************************************************************
!     PLUME_CONC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     This code computes the direct plume contribution
!                     of a point source using Gaussian dispersion using
!                     receptor height. Two receptor heights are used in
!                     computing the terrain responding and impacting
!                     plume states. The terrain weighting and combination
!                     with meander contibutions occur in POINT_CONC.
!                     See AERMOD v22112 MFD Section 5.1.
!
!        PROGRAMMER:  Originally in POINT_CONC - Last Modified 7/5/2022
!                     Original Code by A. Venkatram, M. Snyder, D. Heist
!
!        DATE:        July 2022 WOOD
!                     Moved code out of POINT_CONC to allow for terrain
!
!        INPUTS:      XD, YD, Z, DWDT, HEFF, HSHIFT, HBDEFF, HBMAX
!
!        OUTPUTS:     CONC_PLUME
!
!        CALLED FROM: POINT_CONC
!
!        CALLING
!        ROUTINES:    EFFECTIVE_WIND, VRTSBL
!
!***********************************************************************

!     Variable Declarations:
   use main1,      only: rt2bypi, srt2pi,rlinedbg, rlinedbunt,&
   &fsubz, zi, zr                      ! D096
! JAT 06/22/21 D065, REMOVE XPER AS A VARIABLE USED FROM RLINE_DATA, NOT USED
   use rline_data, only: xr_rot, yr_rot, zrecep, ueff,&
   &xsbegin, xsend, xr_rot, yr_rot,&
   &zrecep, hbd, dwd, dw_perd, hbu,&
!     &                      SIGMAZ0, SIGMAV, SZB, UEFF, XPER,
   &sigmaz0, sigmav, szb, ueff,&
   &nbarr, uh, xshift, shift_flag, xd_min,&
   &i_alpha, alpha, alpha_u, alpha_d,&
   &fran_sum, sigmav_sum, ueff_sum,&
   &vert_sum, horz_sum, conc_p_sum, conc_m_sum,&
   &point_conc_sum, yshift
   implicit none

!     External Functions:
   double precision, external  :: expx, sigmay, sigmaz

!     Local Variables:
   double precision :: xd  !removed INTENT(IN) ... cause it is set below, compiler did not like it
   double precision, intent(in)  :: yd, z,  dwdt
   double precision, intent(in)  :: hbdeff, hbmax !set in POINT_CONC, used here
   double precision, intent(inout)  :: heff !will be input as Z .. might be changed here, used by terrain functions in POINT_CONC
   double precision, intent(inout)  :: hshift !will be input as 0 .. might be changed here, used by terrain functions in POINT_CONC
   double precision, intent(out)  :: conc_plume
   double precision  :: sy, sz, sigmaz0_orig
   double precision  :: cq, fq, ueff_bu
   double precision  :: f_ueff, f_uh, hmax, a
   double precision  :: vert, horz
   double precision  :: zieff !D178, used to not overwrite ZI
   integer           :: ia_max

!     Declare flags:
   logical ::  Direct_flag     ! TRUE = direct plume; FALSE = meander only plume
   logical ::  Gaussian_flag   ! TRUE = gaussian mode; FALSE = mixed-wake mode

!     ----------------------------------------------------------------------------------
!     Explaining All Case Combinations:
!     1 --  Flat (no barrier), receptor upwind of source; meander only
!     2 --  Flat (no barrier), receptor downwind of source; direct, no shift, gaussian
!     3 --  1 downwind barrier, receptor upwind of source; meander only
!     4 --  1 downwind barrier, receptor between source and barrier; direct, no shift, gaussian
!     5 --  1 downwind barrier, receptor downwind of barrier; direct, no shift, mixed-wake
!     6 --  1 upwind barrier, receptor upwind of barrier; meander only
!     7 --  1 upwind barrier, receptor downwind of source, source within recirc zone; direct, shift, gaussian
!     8 --  1 upwind barrier, receptor upwind of source; meander only
!     9 --  1 upwind barrier, receptor downwind of source, source outside of recirc zone; direct, no shift, gaussian
!     10 -- 2 barriers, receptor upwind of upwind_barrier; meander only
!     11 -- 2 barriers, receptor between source and downwind barrier, source within recirc zone; direct, shift, gaussian
!     12 -- 2 barriers, receptor upwind of source; meander only
!     13 -- 2 barriers, receptor between source and downwind barrier, source outside of recirc zone; direct, no shift, gaussian
!     14 -- 2 barriers, receptor downwind of downwind_barrier, source within recirc zone; direct, shift, mixed-wake
!     15 -- 2 barriers, receptor downwind downwind_barrier, source outside of recirc zone; direct, no shift, mixed-wake
!
!     NOTE: Recirculation zone only occurs with an upwind barrier, and only if the source is
!           located between upwind barrier and 6.5H downwind (for 1 barrier case) or 4H (for
!           2 barrier case). So, there will only be a shift if all of this is true.
!     ----------------------------------------------------------------------------------

!     Initialize local variables
   direct_flag   = .false.
   gaussian_flag = .false.
   szb     = 0.0d0
   sy      = 0.0d0
   sz      = 0.0d0
   vert    = 0.0d0
   horz    = 0.0d0
   alpha   = 1.0d0
   i_alpha = 1
   ia_max  = 2
   sigmaz0_orig = sigmaz0  ! store intial sigmaz value from input file
   f_ueff  = 1.0d0  ! factor for lowering ueff and then slowly increasing back to original ueff
   zieff   = 0.0d0  !D178

!     Set flags for direct/meander and gaussian/mixed-wake
   if (xd < 0.0001d0) then
!       NO DIRECT PLUME
      direct_flag = .false.
   else
!       DIRECT PLUME
      xd            = max(xd, xd_min)                                      ! shift in x has already occurred if needed
      direct_flag   = .true.
      gaussian_flag = .true.

!       If there is a downwind barrier and receptor is downwind of it then mixed-wake mode
      if((hbd > 0.0d0) .and. (xd > dwdt)) then
         gaussian_flag = .false.
      end if

   end if

!     Setting heff, szB, i_alpha, and alpha (and F_UEFF for upwind barriers)
   if(direct_flag) then
      if(shift_flag) then
!         Corresponding to case numbers 7, 11, 14 (shift cases)
         heff    = max(z, 0.75d0 * hbu)                                     ! source height is adjusted upwards due to the upwind barrier
         szb     = 0.0d0
         i_alpha = 3                                                        ! set index for ALPHA_U
         alpha   = alpha_u                                                  ! set alpha
!                                                                            --- CALL EXPX
         f_ueff  = 1.0d0 - 1.0d0 * expx(-1.0d0 * xd / (8.0d0 * hbu))        ! factor for reducing ueff in upwind barrier shift cases
      else
!         Corresponding to case numbers 2, 4, 5, 9, 13, 15 (no shift cases)
         heff    = z
         szb     = 0.0d0
         i_alpha = 1                                                        ! set index for ALPHA = 1.0D0
         alpha   = 1.0d0                                                    ! set alpha
      end if
   else
!       Corresponding to case numbers 1, 3, 6, 8, 10, 12 (meander only cases)
!       Note that heff is set in MEANDER subroutine
      if(nbarr > 0) then
         szb     = 0.25d0 * hbmax
         if(hbu > hbd) ia_max = 3                                        ! If HBU > HBD, use ALPHA_U
         i_alpha = ia_max                                                   ! set index for MAX(ALPHA_D, ALPHA_U)
         alpha   = max(alpha_d, alpha_u)                                    ! set alpha
      else
         i_alpha = 1
         alpha   = 1.0d0
      end if
   end if

!     Calculate vertical plume
   if((direct_flag) .and.&
   &(gaussian_flag)) then
!       Calculate with gaussian mode equations
!       Corresponding to case numbers 2, 4, 7, 9, 11, 13 (gaussian cases)
      hshift = 0.0d0
!                                                                            --- CALL EFFECTIVE_WIND
      call effective_wind(xd, z, hshift)
      sz     = sigmaz(xd)
      sy     = sigmay(xd)
      ueff_bu = f_ueff * ueff                                              ! reduce ueff immediately downwind of the barrier and then slowly increase back to ueff

!       Calculate vertical plume for gaussian mode                           --- CALL EXPX
!        VERT   = RT2BYPI * (EXPX(-0.5D0 * ((HEFF - ZRECEP) / SZ)**2)   ! D096
!     &           + EXPX(-0.5D0 * ((HEFF + ZRECEP) / SZ)**2)) /         ! D096
!     &           (2.0D0 * SZ * UEFF_BU)                                ! D096
      zr = zrecep                      ! D096
!RLM D178 Creating local ZIEFF to not change ZI global
!RLM        ZI = MAX(ZI, HEFF + 2.15 * SZ)   ! D096
!RLM        CALL VRTSBL(SZ, HEFF, ZI)        ! D096
      zieff = max(zi, heff + 2.15 * sz)   !D178 ZI changes to ZIEFF
      call vrtsbl(sz, heff, zieff)        !D178 ZI changes to ZIEFF
      vert = fsubz / ueff_bu           ! D096
!      Calculate horizontal plume                                            --- CALL EXPX
      horz   = 1.0d0 / (srt2pi * sy) * expx(-0.5d0 * (yd / sy)**2)

   else if((direct_flag) .and. (.not. gaussian_flag)) then
!       Corresponding to case numbers 5, 14, 15 (mixed-wake cases)
!       Calculate with mixed-wake mode equations in 2 steps

!       Step 1: calculate plume spread between source and downwind barrier   --- CALL EFFECTIVE_WIND
      hshift  = hbd
      call effective_wind(dwdt, heff, hshift)
      szb     = sigmaz(dwdt) + 0.1d0 * hbd                                 ! assigns vertical spread of plume at dwDt to szB
      sigmaz0 = 0.0d0                                                      ! set to zero to not double count intial sigmaz

!       Step 2: calculate plume spread beyond the downwind barrier           --- CALL EFFECTIVE_WIND
      i_alpha = 2                                                          ! set index for ALPHA_D
      alpha   = alpha_d                                                    ! set alpha
      call effective_wind(xd - dwdt, heff, hshift)
      sz      = sigmaz(xd - dwdt)
      sy      = sigmay(xd - dwdt)
      sigmaz0 = sigmaz0_orig                                               ! reset sigmaz0 for next time thru subroutine

!       Calculate vertical plume for mixed-wake mode                         --- CALL EXPX
      cq      = (1.0d0 / (srt2pi * sz * ueff)) * 2.0d0 *&
      &expx(-0.5d0 * (heff / sz)**2)                             ! Venkatram et al. (2021)

      hmax    = max(hbd,12.0d0)                                            ! tallest barrier height affected by downwind barrier effect
      a       = (1.0d0 - (0.75d0 * hbd)/hmax)
!                                                                            --- CALL EXPX
      f_uh    = 1.0d0 - a * expx(-(xd - dwdt) / (9.0d0 * hbd))             ! UH factor; 9H is the scaling factor controlling rise back to original ueff

      fq      = 1.0d0 / (1.0d0 + f_uh * 0.5d0 * uh * cq * hbdeff)          ! Venkatram et al. (2021); assuming unit emissions

!                                                                            --- CALL EXPX
      if(zrecep > hbdeff) then
!         Note: reflection term is about Z = HBDEFF, so the reflective source is at z = HBDEFF-HEFF
         vert  = fq * (1.0d0 / (srt2pi * sz * ueff)) *&
         &(expx(-0.5d0 * ((zrecep - hbdeff - heff) / sz)**2) +&
         &expx(-0.5d0 * ((zrecep - hbdeff + heff) / sz)**2))         ! Venkatram et al. (2021)
      else
         vert  = fq * cq                                                    ! Venkatram et al. (2021)
      end if
!      Calculate horizontal plume                                            --- CALL EXPX
      horz   = 1.0d0 / (srt2pi * sy) * expx(-0.5d0 * (yd / sy)**2)

   else
!       Corresponding to case numbers 1, 3, 6, 8, 10, 12 (meander only cases)
      hshift = 0.0d0
!        CALL EFFECTIVE_WIND(XD, HEFF, HSHIFT)                               !D160 DKH 3/31/23
      vert = 0.0d0                                                         ! no direct plume, so CONC_P should be zero
      horz = 0.0d0

   end if

!     Calculate total direct plume
   conc_plume = vert * horz


!RLM --- FROM POINT_CONC D178
! ----Write components and values to RLINE.DBG
   if (rlinedbg) then
      write(rlinedbunt,&
      &'(A,(A, E9.3),2(" , ", A, E9.3))')&
      &'rline.f/PLUME_CONC: ',&
      &'VERT = ', vert,&
      &'HORZ = ', horz,&
      &'CONC_PLUME = ', conc_plume
   end if
!RLM ---

end subroutine plume_conc


double precision function point_conc(x, y, z)
!***********************************************************************
!        POINT_CONC Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate the direct plume contribution
!                     of a point source using Gaussian dispersion and
!                     combine the direct plume and meander contributions
!                     to determine the total concentration from a point
!                     to the receptor.
!
!        PROGRAMMER:  A. Venkatram, M. Snyder, D. Heist
!
!        DATE:        November, 2013
!
!        MODIFIED:    Added DEBUG output to RLINE.DBG file for each point
!                     used in the integration.
!                     WSP, 10/11/22 Formally Wood
!                     Calculation of Vertical and Horizontal plumes to
!                     PLUME_CONC. Calcluatle the terrain adjustments to
!                     CONC_P and CONC_M
!                     Wood, 7/5/22
!
!        MODIFIED:    Added DEBUG output to RLINE.DBG file for each point
!                     used in the integration.
!                     Wood, 12/07/2021 Michelle G. Snyder & Laura Kent
!                     Replaced FRAN calculation with MEANDR subroutine call
!                     Wood, 12/14/2021 Laura Kent
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      X, Y, Z
!
!        OUTPUTS:
!
!        CALLED FROM: NUMERICAL_LINE_SOURCE
!
!        CALLING
!        ROUTINES:   EFFECTIVE_WIND
!                    EXPX
!                    MEANDER
!                    SIGMAY
!                    SIGMAZ
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
! Wood 10/10/22 Removed CLIFT, CWRAP, IHOUR, L_FLATSRC, FLAT, RT2BYPI, SRT2PI,
!                       ZHILL FROM MAIN1, NOT USED
!      USE MAIN1,      ONLY: RT2BYPI, SRT2PI,RLINEDBG, RLINEDBUNT,
   use main1,      only: rlinedbg, rlinedbunt,&
!     &                      ZELEV, ZFLAG, ZHILL, ZRT, ZS, STABLE, !WOOD 7/5/2022
   &zelev, zflag, zrt, zs, stable,& !WOOD 7/5/2022
!     &                       CLIFT, CWRAP, HCRIT, FOPT, IHOUR, ! WOOD 6-28-2022
   &hcrit, fopt,& ! WOOD 6-28-2022
   &l_flatsrc,isrc,&  !D173 WSP - 7/24/2023
!     &                       L_FLATSRC, FLAT, HSBL, PHEE, SZ, ZI, UNSTAB ! WOOD 6-28-2022
   &hsbl, phee, sz, zi, unstab ! WOOD 6-28-2022
! JAT 06/22/21 D065, REMOVE XPER AS A VARIABLE USED FROM RLINE_DATA, NOT USED
! Wood 10/10/22 Removed ALPHA, ALPHA_U, ALPHA_D, SHIFT_FLAG, SIGMAZ0, SZB, UH
!                       XD_MIN,  FROM RLINE_DATA, NOT USED
   use rline_data, only: xr_rot, yr_rot, zrecep, ueff,&
   &xsbegin, xsend, xr_rot, yr_rot,&
   &zrecep, hbd, dwd, dw_perd, hbu,&
!     &                      SIGMAZ0, SIGMAV, SZB, UEFF, XPER,
   &sigmav, ueff,&
!     &                      NBARR, UH, XSHIFT, SHIFT_FLAG, XD_MIN,
   &nbarr,  xshift,&
!     &                      I_ALPHA, ALPHA, ALPHA_U, ALPHA_D,
   &i_alpha,&
   &fran_sum, sigmav_sum, ueff_sum,&
   &vert_sum, horz_sum, conc_p_sum, conc_m_sum,&
   &point_conc_sum, yshift
   implicit none

!     External Functions:
   double precision, external  :: meander, expx
   double precision, external  :: sigmay, sigmaz

!     Local Variables:
! Wood 10/10/22 Removed CQ, FQ,F_UEFF, F_UH, HMAX, A, IA_MAX, UEFF_BU NOT USED
   double precision  :: conc_m, conc_p, vert, horz
!     DOUBLE PRECISION  :: SY, SZ, SIGMAZ0_ORIG  ! Wood moved to PLUME_CONC for terrain calculations 7/5/2022
   double precision  :: fran
   double precision  :: xd, yd, xmax, dwdt
   double precision  :: heff, hshift, hbmax, hbdeff
!      DOUBLE PRECISION  :: CQ, FQ, UEFF_BU
!      DOUBLE PRECISION  :: F_UEFF, F_UH, HMAX, A
!      INTEGER           :: IA_MAX
   double precision, intent(in)  :: x, y, z
   double precision  :: clift_m, clift_p, cwrap_p, cwrap_m !Wood 6-8-2022 for terrain calculations

!     CONC_M      = meander concentration
!     CONC_P      = direct plume concentration
!     VERT        = vertical component of concentration
!     HORZ        = horizontal component of concentration
!     SY            = horizontal dispersion coefficient
!     SZ            = vertical dispersion coefficient
!     SIGMAZ0_ORIG  = reset sigmaz0 to the value read from the source input
!     FRAN          = fraction between meander and direct plume
!     XD            = distance between source and receptor in direction parallel to wind
!     YD            = distance between source and receptor in direction perpendicular to wind
!     XMAX          = distance between downwind barrier (at the most upwind point) and the receptor
!     DWDT          = distance between the source and downwind barrier
!     HEFF          = effective source height
!     HSHIFT        = vertical shift in source height for barriers
!     HBMAX         = taller barrier height, either HBU or HBD
!     HBDEFF        = effective downwind barrier height in the mixed-wake algorithm
!     CQ            = variable used to compute concentration for mixed-wake algorithm; Venkatram et al. (2021)
!     FQ            = scaling factor for mixed-wake algorithm; Venkatram et al. (2021)
!     UEFF_BU       = factor for reducing ueff immediately downwind of the upwind barrier
!     F_UEFF        = factor for reducing ueff in upwind barrier shift cases
!     F_UH          = factor for UH
!     HMAX          = tallest barrier height for scaling of UH
!     A             = variable used in UH
!     IA_MAX        = index for alpha for the barrier with greater height (i.e., upwind or downwind barrier)
!     X             = x-coordinate of source location
!     Y             = y-coordinate of source location
!     Z             = z-coordinate of source location

!     Declare flags:
!      logical ::  Direct_flag     ! TRUE = direct plume; FALSE = meander only plume !moved to PLUME_CONC - Wood 7/5/2022
!      logical ::  Gaussian_flag   ! TRUE = gaussian mode; FALSE = mixed-wake mode !moved to PLUME_CONC - Wood 7/5/2022

!     ----------------------------------------------------------------------------------
!     Explaining All Case Combinations:
!     1 --  Flat (no barrier), receptor upwind of source; meander only
!     2 --  Flat (no barrier), receptor downwind of source; direct, no shift, gaussian
!     3 --  1 downwind barrier, receptor upwind of source; meander only
!     4 --  1 downwind barrier, receptor between source and barrier; direct, no shift, gaussian
!     5 --  1 downwind barrier, receptor downwind of barrier; direct, no shift, mixed-wake
!     6 --  1 upwind barrier, receptor upwind of barrier; meander only
!     7 --  1 upwind barrier, receptor downwind of source, source within recirc zone; direct, shift, gaussian
!     8 --  1 upwind barrier, receptor upwind of source; meander only
!     9 --  1 upwind barrier, receptor downwind of source, source outside of recirc zone; direct, no shift, gaussian
!     10 -- 2 barriers, receptor upwind of upwind_barrier; meander only
!     11 -- 2 barriers, receptor between source and downwind barrier, source within recirc zone; direct, shift, gaussian
!     12 -- 2 barriers, receptor upwind of source; meander only
!     13 -- 2 barriers, receptor between source and downwind barrier, source outside of recirc zone; direct, no shift, gaussian
!     14 -- 2 barriers, receptor downwind of downwind_barrier, source within recirc zone; direct, shift, mixed-wake
!     15 -- 2 barriers, receptor downwind downwind_barrier, source outside of recirc zone; direct, no shift, mixed-wake
!
!     NOTE: Recirculation zone only occurs with an upwind barrier, and only if the source is
!           located between upwind barrier and 6.5H downwind (for 1 barrier case) or 4H (for
!           2 barrier case). So, there will only be a shift if all of this is true.
!     ----------------------------------------------------------------------------------

!     Initialize local variables
!      DIRECT_FLAG   = .FALSE. !moved to PLUME_CONC - Wood 7/5/2022
!      GAUSSIAN_FLAG = .FALSE. !moved to PLUME_CONC - Wood 7/5/2022
   conc_p  = 0.0d0
   conc_m  = 0.0d0
   fran    = 0.0d0
!      SZB     = 0.0D0 !moved to PLUME_CONC - Wood 7/5/2022
!      SY      = 0.0D0 !moved to PLUME_CONC - Wood 7/5/2022
!      SZ      = 0.0D0 !moved to PLUME_CONC - Wood 7/5/2022
   xmax    = 0.0d0
!      VERT    = 0.0D0 !moved to PLUME_CONC - Wood 7/5/2022
!      HORZ    = 0.0D0 !moved to PLUME_CONC - Wood 7/5/2022
   dwdt    = 0.0d0
!      ALPHA   = 1.0D0 !moved to PLUME_CONC - Wood 7/5/2022
!      I_ALPHA = 1 !moved to PLUME_CONC - Wood 7/5/2022
!      IA_MAX  = 2 !moved to PLUME_CONC - Wood 7/5/2022                                               ! assume HBD > HBU (will correct this below if it's not)
   heff    = z !initialized here, could be modified in PLUME_CONC - Wood 7/5/2022
   hbdeff  = 0.0d0 !set here, used in PLUME_CONC - Wood 7/5/2022
   hshift  = 0.0d0 !initialized here, could be modified in PLUME_CONC - Wood 7/5/2022
!      SIGMAZ0_ORIG = SIGMAZ0  ! store intial sigmaz value from input file !moved to PLUME_CONC - Wood 7/5/2022
!      F_UEFF  = 1.0D0     ! factor for lowering ueff and then slowly increasing back to original ueff !moved to PLUME_CONC - Wood 7/5/2022

! Initialize WRAP and LIFT components - Wood 7/5/2022
   cwrap_p = 0.0d0
   cwrap_m = 0.0d0
   clift_p = 0.0d0
   clift_m = 0.0d0

!     Compute distance from source to barrier, adjusting for upwind barrier when appropriate
   xd     = xr_rot - (x - xshift)                             ! shift in x is applied here
   yd     = yr_rot - (y - yshift)                             ! shift in y is applied here

!     Setting up options for barrier combinations
   if(nbarr > 0) then
      hbmax   = max(hbu, hbd)
      hbdeff  = hbd * 1.25d0                                   ! effective barrier height in the mixed-wake algorithm

!       For nearly parallel winds, prevent dwDt from exceeding the distance to the end of the line source
      xmax    = max(dabs(xr_rot - (xsbegin - xshift)),&
      &dabs(xr_rot - (xsend - xshift)))
      if(xmax < dw_perd) dwdt = dwd
      if(xmax >= dw_perd) dwdt = min(xmax, dwd)
   end if

! begin - Moved calculation of Vertical and Horizontal plumes to PLUME_CONC  - Wood 7/5/2022

!     Calculate direct plume concentration ----------------------------
!
!     Set flags for direct/meander and gaussian/mixed-wake
!     IF (XD .LT. 0.0001D0) THEN
!       NO DIRECT PLUME
!       DIRECT_FLAG = .FALSE.
!     ELSE
!       DIRECT PLUME
!       XD            = MAX(XD, XD_MIN)                                      ! shift in x has already occurred if needed
!       DIRECT_FLAG   = .TRUE.
!       GAUSSIAN_FLAG = .TRUE.
!
!       If there is a downwind barrier and receptor is downwind of it then mixed-wake mode
!       IF((HBD .GT. 0.0D0) .and. (XD .GT. DWDT)) THEN
!         GAUSSIAN_FLAG = .FALSE.
!       END IF
!
!     END IF
!
!     Setting heff, szB, i_alpha, and alpha (and F_UEFF for upwind barriers)
!     IF(DIRECT_FLAG) THEN
!       IF(SHIFT_FLAG) THEN
!         Corresponding to case numbers 7, 11, 14 (shift cases)
!         HEFF    = MAX(Z, 0.75D0 * HBU)                                     ! source height is adjusted upwards due to the upwind barrier
!         SZB     = 0.0D0
!         I_ALPHA = 3                                                        ! set index for ALPHA_U
!         ALPHA   = ALPHA_U                                                  ! set alpha
!                                                                            --- CALL EXPX
!         F_UEFF  = 1.0D0 - 1.0D0 * EXPX(-1.0D0 * XD / (8.0D0 * HBU))        ! factor for reducing ueff in upwind barrier shift cases
!       ELSE
!         Corresponding to case numbers 2, 4, 5, 9, 13, 15 (no shift cases)
!         HEFF    = Z
!         SZB     = 0.0D0
!         I_ALPHA = 1                                                        ! set index for ALPHA = 1.0D0
!         ALPHA   = 1.0D0                                                    ! set alpha
!       END IF
!     ELSE
!       Corresponding to case numbers 1, 3, 6, 8, 10, 12 (meander only cases)
!       Note that heff is set in MEANDER subroutine
!       IF(NBARR .GT. 0) THEN
!         SZB     = 0.25D0 * HBMAX
!         IF(HBU .GT. HBD) IA_MAX = 3                                        ! If HBU > HBD, use ALPHA_U
!         I_ALPHA = IA_MAX                                                   ! set index for MAX(ALPHA_D, ALPHA_U)
!         ALPHA   = MAX(ALPHA_D, ALPHA_U)                                    ! set alpha
!       ELSE
!         I_ALPHA = 1
!         ALPHA   = 1.0D0
!       END IF
!     END IF
!
!
!C     Calculate vertical plume
!      IF((DIRECT_FLAG) .and.
!     &  (GAUSSIAN_FLAG)) THEN
!C       Calculate with gaussian mode equations
!C       Corresponding to case numbers 2, 4, 7, 9, 11, 13 (gaussian cases)
!        HSHIFT = 0.0D0
!C                                                                            --- CALL EFFECTIVE_WIND
!        CALL EFFECTIVE_WIND(XD, Z, HSHIFT)
!         SZ     = SIGMAZ(XD)
!         SY     = SIGMAY(XD)
!        UEFF_BU = F_UEFF * UEFF                                              ! reduce ueff immediately downwind of the barrier and then slowly increase back to ueff
!
!C       Calculate vertical plume for gaussian mode                           --- CALL EXPX
!        VERT   = RT2BYPI * (EXPX(-0.5D0 * ((HEFF - ZRECEP) / SZ)**2)
!     &           + EXPX(-0.5D0 * ((HEFF + ZRECEP) / SZ)**2)) /
!     &           (2.0D0 * SZ * UEFF_BU)
!C      Calculate horizontal plume                                            --- CALL EXPX
!        HORZ   = 1.0D0 / (SRT2PI * SY) * EXPX(-0.5D0 * (YD / SY)**2)
!
!      ELSE IF((DIRECT_FLAG) .and. (.NOT. GAUSSIAN_FLAG)) THEN
!C       Corresponding to case numbers 5, 14, 15 (mixed-wake cases)
!C       Calculate with mixed-wake mode equations in 2 steps
!
!C       Step 1: calculate plume spread between source and downwind barrier   --- CALL EFFECTIVE_WIND
!        HSHIFT  = HBD
!        CALL EFFECTIVE_WIND(DWDT, HEFF, HSHIFT)
!        SZB     = SIGMAZ(DWDT) + 0.1D0 * HBD                                 ! assigns vertical spread of plume at dwDt to szB
!        SIGMAZ0 = 0.0D0                                                      ! set to zero to not double count intial sigmaz
!
!C       Step 2: calculate plume spread beyond the downwind barrier           --- CALL EFFECTIVE_WIND
!        I_ALPHA = 2                                                          ! set index for ALPHA_D
!        ALPHA   = ALPHA_D                                                    ! set alpha
!        CALL EFFECTIVE_WIND(XD - DWDT, HEFF, HSHIFT)
!        SZ      = SIGMAZ(XD - DWDT)
!        SY      = SIGMAY(XD - DWDT)
!        SIGMAZ0 = SIGMAZ0_ORIG                                               ! reset sigmaz0 for next time thru subroutine
!
!C       Calculate vertical plume for mixed-wake mode                         --- CALL EXPX
!        CQ      = (1.0D0 / (SRT2PI * SZ * UEFF)) * 2.0D0 *
!     &             EXPX(-0.5D0 * (HEFF / SZ)**2)                             ! Venkatram et al. (2021)
!
!        HMAX    = MAX(HBD,12.0D0)                                            ! tallest barrier height affected by downwind barrier effect
!        A       = (1.0D0 - (0.75D0 * HBD)/HMAX)
!C                                                                            --- CALL EXPX
!        F_UH    = 1.0D0 - A * EXPX(-(XD - DWDT) / (9.0D0 * HBD))             ! UH factor; 9H is the scaling factor controlling rise back to original ueff
!
!        FQ      = 1.0D0 / (1.0D0 + F_UH * 0.5D0 * UH * CQ * HBDEFF)          ! Venkatram et al. (2021); assuming unit emissions
!
!C                                                                            --- CALL EXPX
!        IF(ZRECEP .GT. HBDEFF) THEN
!C         Note: reflection term is about Z = HBDEFF, so the reflective source is at z = HBDEFF-HEFF
!          VERT  = FQ * (1.0D0 / (SRT2PI * SZ * UEFF)) *
!     &           (EXPX(-0.5D0 * ((ZRECEP - HBDEFF - HEFF) / SZ)**2) +
!     &            EXPX(-0.5D0 * ((ZRECEP - HBDEFF + HEFF) / SZ)**2))         ! Venkatram et al. (2021)
!        ELSE
!          VERT  = FQ * CQ                                                    ! Venkatram et al. (2021)
!        END IF
!C      Calculate horizontal plume                                            --- CALL EXPX
!        HORZ   = 1.0D0 / (SRT2PI * SY) * EXPX(-0.5D0 * (YD / SY)**2)
!
!      ELSE
!C       Corresponding to case numbers 1, 3, 6, 8, 10, 12 (meander only cases)
!        HSHIFT = 0.0D0
!        CALL EFFECTIVE_WIND(XD, HEFF, HSHIFT)
!        VERT = 0.0D0                                                         ! no direct plume, so CONC_P should be zero
!        HORZ = 0.0D0
!
!      END IF

!C     Calculate total direct plume
!      CONC_P = VERT * HORZ
!
!C     Calculate meander concentration
!C     Get fraction of random kinetic energy (from AERMOD routines)           --- CALL MEANDR
!      CALL MEANDR(UEFF,SIGMAV,FRAN)
!
!
!      CONC_M = MEANDER(X, Y, Z, HSHIFT)
! end - Moved calculation of Vertical and Horizontal plumes to PLUME_CONC  - Wood 7/5/2022

! begin - Terrain adjustments to CONC_P and CONC_M - Wood 7/5/2022

!     Calculate height of receptor above stack base, ZRT
!WSP         ZRT = ZELEV - ZS + ZFLAG
!WSP --- Begin: D173 WSP add 7/24/2023
   if (l_flatsrc(isrc)) then
      zrt = zflag
   else
      zrt = zelev - zs + zflag
   end if
!WSP --- End: D173 WSP add 7/24/2023

! Calculate FOPT for I_ALPHA= 1 (i.e. no barrier) and HEFF = Z (set above) - Wood 7/5/2022
! note: I_ALPHA & HSHIFT are not set YET!
   i_alpha = 1

!  Calculate height of the "effective reflecting surface" !MGS - 7/18/2022
   if (stable .or. (unstab.and.(zs>=zi))) then
      sz = sigmaz(xd)
      call refl_ht (heff, xd, 0.0d0, sz, hsbl)
   elseif ( unstab ) then
      hsbl = 0.0d0
   end if

!     Determine the CRITical Dividing Streamline---   CALL CRITDS
   call critds (heff)

!     Calculate the fraction of plume below   HCRIT, PHEE                               ---   CALL PFRACT
   call pfract (heff)

!     Calculate FOPT = f(PHEE)                  ---   CALL FTERM
   call fterm


!---- Calculate the contribution due to horizontal plume, CWRAP
   if (fopt == 0.0d0) then
      cwrap_p = 0.0d0
      cwrap_m = 0.0d0
   else
      zrecep = zrt
      call plume_conc(xd, yd, z, dwdt, heff, hshift, hbdeff, hbmax,&
      &cwrap_p)
      cwrap_m = meander(x, y, z, hshift)

   end if


!---- Calculate the contribution due to terrain-following plume, CLIFT
   if (zrt == zflag) then
!----    Effective receptor heights are equal, therefore CLIFT = CWRAP
      clift_p = cwrap_p
      clift_m = cwrap_m
   else if (fopt == 1.0d0) then
      clift_p = 0.0d0
      clift_m = 0.0d0
   else
      zrecep = zflag
      call plume_conc(xd, yd, z, dwdt, heff, hshift, hbdeff, hbmax,&
      &clift_p)
      clift_m = meander(x, y, z, hshift)
   end if

! Re-Calculate FOPT if I_ALPHA changes from 1 in PLUME_CONC (i.e. there is a barrier) - Wood 7/5/2022
   if((i_alpha /= 1) .or. (heff /= z)) then

!     Calculate height of the "effective reflecting surface" !Wood - 7/18/2022
      if (stable .or. (unstab.and.(zs>=zi))) then
         sz = sigmaz(xd)
         call refl_ht (heff, xd, sz, 0.0d0, hsbl)
      elseif ( unstab ) then
         hsbl = 0.0d0
      end if

!        Determine the CRITical Dividing Streamline  ---   CALL CRITDS
      call critds (heff)

!        Calculate the fraction of plume below   HCRIT, PHEE   ---   CALL PFRACT
      call pfract (heff)

!        Calculate FOPT = f(PHEE)                  ---   CALL FTERM
      call fterm
   end if

!     Terrain Weighting of CLIFT (Terrain-following) and CWRAP( horizontal plume) for MEANDER and Direct PLUME
   conc_p =  fopt * cwrap_p + ((1.0d0 - fopt) * clift_p)
   conc_m =  fopt * cwrap_m + ((1.0d0 - fopt) * clift_m)

!     Get fraction of random kinetic energy (from AERMOD routines)           --- CALL MEANDR
!      CALL MEANDR(UEFF,SIGMAV,FRAN)    ! D096, move MEANDR call after MEANDER call

!      CONC_M = MEANDER(X, Y, Z, HSHIFT)

!     Calculate meander concentration
!     Get fraction of random kinetic energy (from AERMOD routines)           --- CALL MEANDR
   call meandr(ueff,sigmav,fran)    ! D096, move MEANDR call after MEANDER call

! end - Terrain adjustments to CONC_P and CONC_M - Wood 7/5/2022

!     Combine direct plume and meander contributions with the terrain included
   point_conc = conc_p * (1.0d0 - fran) + conc_m * fran

! ----Write components and values to RLINE.DBG
   if (rlinedbg) then
      write(rlinedbunt,&
!RLM     & '(A,(A, F5.3),2(" , ", A, F7.3),5(" , ", A, E9.3))') !D178
      &'(A,(A, F5.3),2(" , ", A, F7.3),3(" , ", A, E9.3))')&
      &'rline.f/POINT_CONC: ',&
      &'FRAN = ', fran,&
      &'SIGMAV = ', sigmav,&
      &'UEFF = ', ueff,&
!RLM     &           'VERT = ', VERT, !D178
!RLM     &           'HORZ = ', HORZ, !D178
      &'CONC_P = ', conc_p,&
      &'CONC_M = ', conc_m,&
      &'POINT_CONC = ', point_conc
!     Added HCRIT, FOPT and PHEE Wood 10/10/22
      write(rlinedbunt,&
      &'((A, E9.3), 2(" , ", A, E9.3))')&
      &'                           HCRIT = ', hcrit,&
      &'FOPT = ', fopt,&
      &'PHEE = ', phee
   end if

! Store/Update cumulative variables for integration, averages output in numerical line source for last iteration
   fran_sum = fran_sum + fran
   sigmav_sum = sigmav_sum + sigmav
   ueff_sum = ueff_sum + ueff
   vert_sum = vert_sum + vert
   horz_sum = horz_sum + horz
   conc_p_sum = conc_p_sum + conc_p
   conc_m_sum = conc_m_sum + conc_m
   point_conc_sum = point_conc_sum + point_conc

end function point_conc

subroutine polyinterp(y,err,xa,ya,x,n)
!***********************************************************************
!        POLYINTERP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Given vectors xa and ya, y is interpolated value
!                     for x; err is the error in interpolation.
!                     Uses polynomial interpolation from "Numerical
!                     Recipes in Fortran" pages 103-104.
!
!        PROGRAMMER:  A. Venkatram
!
!        DATE:        November, 2013
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      XA, YA, X, N
!
!        OUTPUTS:     Y, ERR
!
!        CALLED FROM: NUMERICAL_LINE_SOURCE
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!
!                    "Numerical Recipes in Fortran", Press et al., (1992)
!***********************************************************************

!     Variable Declarations:
   implicit none

!     D108 - CRT, 11/16/2021 Moved declaration of subroutine arguments
!     from below and reordered to put declaration of N before all others
!     N is used to set length of arrays
   integer, intent(in)  :: n
!     N           = length of XA and YA arrays
   double precision, intent(in)   :: x
   double precision, intent(in)   :: xa(n), ya(n)
   double precision, intent(out)  :: err
   double precision, intent(out)  :: y
!     XA(N)       = table of XA values used in interpolation
!     YA(N)       = table of YA values used in interpolation
!     ERR         = error in interpolation
!     Y           = interpolated value at X

!     Local Variables:
   integer  :: ns
   integer  :: i, m
!     NS          = position of x in array
!     I           = counting index
!     M           = counting index

   double precision  :: dif, dift
   double precision  :: ho, hp, w, den
   double precision  :: deltay
   double precision, dimension(n)  :: c, d
!     DIF         = difference used in calculations
!     DIFT        = difference used in calculations
!     HO          = polyinterpolation point
!     HP          = polyinterpolation point
!     W           = weighting between polynomial interpolation points
!     DEN         = difference between consecutive polynomial interpolation points
!     DELTAY      = polynomial interpolation error


!     Computation Parameters:
!MGS      DOUBLE PRECISION  :: EPS = 1.0E-10  !D178_RLINE_RecpOrder_WSP
   double precision  :: eps = 1.0d-10

!     Variable Initializations:
   deltay = 0.0d0


   ns  = 1
   dif = dabs(x - xa(1))
   do i = 1, n
      dift = dabs(x - xa(i))
      if (dift < dif) then
         ns  = i
         dif = dift
      end if
      c(i) = ya(i)
      d(i) = ya(i)
   end do
   y  = ya(ns)
   ns = ns - 1
   do m = 1, n - 1
      do i = 1, n - m
         ho   = xa(i) - x
         hp   = xa(i + m) - x
         w    = c(i + 1) - d(i)
         den  = ho - hp
         d(i) = w * hp / den
         c(i) = w * ho / den
      end do
      if (2 * ns < (n - m)) then
         deltay = c(ns + 1)
      else
         deltay = d(ns)
         ns = ns - 1
      end if
      y = y + deltay
   end do

   err = dabs(deltay / (y + eps))

end subroutine polyinterp

subroutine rlemconv
!***********************************************************************
!        RLEMCONV Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Fills RLMOVESCONV array with all entries equal
!                     to 1 if FALSE; or a conversion from MOVES
!                     (g/hr/link) to RLINE native units of (g/m/s),
!                     based on length, if TRUE.
!
!        PROGRAMMER:  M. Snyder, Wood
!
!        DATE:        May 24, 2018
!
!        MODIFIED:
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM: RLCALC
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use main1, only: isrc, numsrc, srctyp
   use rline_data, only: rlemisconv, rlsource, rlmovesconv
   implicit none

!     Local Variables:
   integer  :: index
   double precision  :: length
!     INDEX       = index
!     LENGTH      = length of RLINE source

   rlemisconv(:) = 1.0d0

!     Perform conversion of MOVES units (g/hr/link) to RLINE units (g/m/s)
!     only for RLINE sources.
   if(rlmovesconv) then
      do index = isrc, numsrc
         if (srctyp(index) == 'RLINE') then
            length = dsqrt((rlsource(index)%xsb -&
            &rlsource(index)%xse)**2 +&
            &(rlsource(index)%ysb -&
            &rlsource(index)%yse)**2)
            rlemisconv(index) = 1.0d0 / length / 3600d0
         end if
      end do
   end if

end subroutine rlemconv



double precision function sigmay(xd)


!***********************************************************************
!        SIGMAY Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate horizontal plume spread.
!
!        PROGRAMMER:  A. Venkatram, M. Snyder, D. Heist
!
!        DATE:        November, 2013
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      XD
!
!        OUTPUTS:
!
!        CALLED FROM: POINT_CONC
!
!        CALLING
!        ROUTINES:
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use main1, only: obulen, ustar, stable
   use rline_data, only: sigmav, sigmay0, sigz_y,&
   &psy1, psy2, psy3, psy4, fastrline
   implicit none

!     Local Variables:
   double precision  :: sz

   double precision, intent(in)  :: xd
!     SZ          = vertical dispersion
!     XD          = distance between source and receptor in direction parallel to wind

   if(fastrline) then
      if(xd <= 10) then
         sigmay   = psy1 * xd**psy2
      else
         sigmay   = psy3 * xd**psy4
      end if
      return
   end if

!     Set sigmaz to SIGZ from SIGMAZ function before minimum taken & before sz0 included
   sz = sigz_y

!     Calculate SIGMAY based on stability
!      IF (STABLE) THEN
!         SIGMAY = 1.6D0 * SIGMAV / USTAR * SZ *
!     &           (1.0D0 + 2.5D0 * SZ / DABS(OBULEN))
!      ELSE
!         SIGMAY = 1.6D0 * SIGMAV / USTAR * SZ /
!     &            DSQRT(1.0D0 + 1.0D0 * SZ / DABS(OBULEN))
!      END IF
!     D096 Updated the SigmaY coefficients based on optimization WSP 4/5/23
   if (stable) then
      sigmay = 1.4d0 * sigmav / ustar * sz *&
      &(1.0d0 + 1.5d0 * sz / dabs(obulen))
   else
      sigmay = 1.40 * sigmav / ustar * sz /&
      &dsqrt(1.0d0 + 2.5d0 * sz / dabs(obulen))
   end if

   sigmay = dsqrt(sigmay**2 + sigmay0**2)

end function sigmay

double precision function sigmaz(xd)
!***********************************************************************
!        SIGMAZ Function of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Calculate vertical plume spread,
!                     including source configuration effects.
!
!        PROGRAMMER:  A. Venkatram, M. Snyder, D. Heist
!
!        DATE:        November, 2013
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        MODIFIED:    Code integrated into AERMOD for RLINE source.
!                     Wood, 07/20/2018
!
!        INPUTS:      XD
!
!        OUTPUTS:
!
!        CALLED FROM: EFFECTIVE_WIND
!                     MEANDER
!                     POINT_CONC
!                     SIGMAY
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use main1, only: zi, isrc, stable,&
   &rt2bypi, twothirds
   use rline_data, only: rlsource, ueff, sigmaz0, szb,&
   &nbarr, sigz_y, psz1, psz2, psz3, psz4,&
   &fastrline, ust_a, lmo_a, i_alpha
   implicit none

!     Local Variables:
   double precision  :: sigmaz_max, xbar, xdabs, uratio
   double precision  :: sigz
   double precision  :: sigmazd
   double precision  :: sigmazb
   double precision, intent(in)  :: xd
!     SIGMAZ_MAX  = maximum vertical dispersion
!     XBAR        = absolute value of XD/L
!     XDABS       = absolute value of XD
!     URATIO      = USTAR / UEFF
!     SIGZ        = vertical dispersion initial calculation
!     SIGMAZD     = vertical dispersion due to depression
!     SIGMAZB     = vertical dispersion due to barrier
!     XD          = distance between source and receptor in direction parallel to wind

!      SIGMAZ_MAX = RT2BYPI * ZI       ! D096
   if(fastrline) then
      if(xd <= 10) then
         sigz   = psz1 * xd**psz2
      else
         sigz   = psz3 * xd**psz4
      end if
!        SIGMAZ = MIN(SIGZ, SIGMAZ_MAX)   ! D096
      sigmaz = sigz                   ! D096
      return
   end if

   xbar       = dabs(xd / lmo_a(i_alpha))
   xdabs      = dabs(xd)
   sigmazd    = 0.0d0
   sigmazb    = 0.0d0
   uratio     = ust_a(i_alpha) / ueff

!     Calculate vertical dispersion curve based on stability
   if (stable) then
!         SIGZ = 0.57D0 * (URATIO * XDABS) /     ! D096
!     &          (1.0D0 + 3.0D0 * URATIO *       ! D096
!     &          (EXP(TWOTHIRDS * LOG(XBAR))))   ! D096

!        D096 updated coefficies a, bs, and bu based on optimization 4/5/23 WSP
      sigz = 0.70d0 * (uratio * xdabs) /&      ! D096
      &(1.0d0 + 1.5d0 * uratio *&        ! D096
!MGS     &          (EXP(TWOTHIRDS * LOG(XBAR)))) !D178_RLINE_RecpOrder_WSP
      &(dexp(twothirds * dlog(xbar))))
   else
!         SIGZ = 0.57D0 * (URATIO * XDABS) *          ! D096
!     &          (1.0D0 + 1.5D0 * (URATIO  * XBAR))   ! D096

!        D096 updated coefficies a, bs, and bu based on optimization 4/5/23 WSP
      sigz = 0.70d0 * (uratio * xdabs) *&    ! D096
      &(1.0d0 + 1.0d0 * (uratio  * xbar))
   end if

!     Adjust for depressed roadway, if used
   if (rlsource(isrc)%depth < 0.0d0) then
      sigmazd = -1.0d0 * rlsource(isrc)%depth / 2.15d0
   end if

!     Adjust for barrier, if barrier is present
   if(nbarr > 0) then
      sigmazb = szb
   end if

   sigz_y = sigz
   sigz   = dsqrt(sigz * sigz + sigmaz0 * sigmaz0 +&
   &sigmazd * sigmazd + sigmazb * sigmazb)

!      SIGMAZ = MIN(SIGZ, SIGMAZ_MAX)   ! D096
   sigmaz = sigz   ! D096

end function sigmaz

subroutine  translate_rotate
!***********************************************************************
!        TRANSLATE_ROTATE Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:     Translate and rotate the coordinates so that x-axis
!                     lies along the wind. In addition, this subroutine
!                     allows the user to specify sources based on a
!                     centerline and the distance from the
!                     centerline (DCL).. ie an offset.
!
!        PROGRAMMER:  M. Snyder, Wood
!
!        DATE:        May 24, 2018
!
!        MODIFIED:    Incorporated RLINE2 barrier algorithms.
!                     Dianna Francisco & David Heist, US EPA ORD, 02/12/2021
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM: RLCALC
!
!        CALLING
!        ROUTINES:    DEPRESSED_DISPLACEMENT
!
!        References: "RLINE: A Line Source Dispersion Model for Near-Surface
!                    Releases", Snyder et. al (2013)
!***********************************************************************

!     Variable Declarations:
   use main1, only: axr, ayr, numrec, numsrc, isrc, srctyp, wdref, pi
   use rline_data, only: x0, y0, xsb_rot, ysb_rot, xse_rot, yse_rot,&
   &thetaw, xrcp_rot, yrcp_rot, rlsource,&
   &bdw_flag, theta_line
   implicit none

!     External Functions:
   double precision, external :: depressed_displacement

!     Local Variables:
   integer  :: index, i
   double precision :: xr_tran, yr_tran, angle
   double precision :: xsb_tran, ysb_tran, xse_tran, yse_tran
   double precision :: dcl_loc, dclwall_loc(2)
   double precision :: xbb, ybb, xbb_rot
!     INDEX       = index
!     I           = index
!     XR_TRAN     = x-coordinate for translated receptor
!     YR_TRAN     = y-coordinate for translated receptor
!     ANGLE       = 270.0 - wind direction
!     XSB_TRAN    = beginning x-coordinate for translated source
!     YSB_TRAN    = beginning y-coordinate for translated source
!     XSE_TRAN    = end x-coordinate for translated source
!     YSE_TRAN    = end y-coordinate for translated source
!     DCL_LOC        = offset distance from center line
!     DCLWALL_LOC(2) = barrier distance from center line of road; DCLWALL(1) and DCLWALL(2)
!     XBB            = beginning coordinates of the barrier; after rotation
!     YBB            = beginning coordinates of the barrier; after rotation
!     XBB_ROT        = rotated beginning coordinates of the barrier

!     Initialize flag for 'barrier is downwind of source' (1 if true, 0 if false)
   bdw_flag(:,:) = 0

   angle = 270.0d0 - wdref
   if (angle > 180.0d0) then
      angle = angle - 360.0d0
   end if
   thetaw = angle*pi / 180.0d0

!     Translate line source origin
   x0 = rlsource(isrc)%xsb
   y0 = rlsource(isrc)%ysb

!     Translate source and receptor coordinates and then rotate them along wind direction
   do index = isrc, numsrc
      if (srctyp(index) == 'RLINE' .or.&
      &srctyp(index) == 'RLINEXT') then
!           Initialize variables used
         dcl_loc        = rlsource(index)%dcl
         dclwall_loc(1) = rlsource(index)%dclwall
         dclwall_loc(2) = rlsource(index)%dclwall2

!           Move initial user coordinate system so the origin is at the beginning of first source
         xsb_tran = rlsource(index)%xsb - x0
         ysb_tran = rlsource(index)%ysb - y0
         xse_tran = rlsource(index)%xse - x0
         yse_tran = rlsource(index)%yse - y0
         theta_line = datan2(yse_tran - ysb_tran, xse_tran -&
         &xsb_tran)

!           Account for due east and north source lines
         if (dsin(theta_line) == 0.0d0) then
            dcl_loc        = -1.0d0 * dcl_loc              ! needed for lines running West-East; + is North
            dclwall_loc(1) = -1.0d0 * dclwall_loc(1)
            dclwall_loc(2) = -1.0d0 * dclwall_loc(2)
         end if

!           Determine location of the line that is not within a depression,
!           but is specified in source file with the centerline and distance
!           from the centerline
         if (dcl_loc /= 0.0d0 .and.&
         &rlsource(index)%depth == 0.0d0) then
            xse_tran = xse_tran + dcl_loc * dsin(theta_line) *&
            &dsign(1.0d0, dsin(theta_line))
            yse_tran = yse_tran - dcl_loc * dcos(theta_line) *&
            &dsign(1.0d0, dsin(theta_line))
            xsb_tran = xsb_tran + dcl_loc * dsin(theta_line) *&
            &dsign(1.0d0, dsin(theta_line))
            ysb_tran = ysb_tran - dcl_loc * dcos(theta_line) *&
            &dsign(1.0d0, dsin(theta_line))
         end if

!           Adjustments for near source configurations (depression)
         if (rlsource(index)%depth < 0.0d0) then
            xse_tran = xse_tran -&
            &depressed_displacement(theta_line, index) *&
            &dsin(theta_line)
            yse_tran = yse_tran +&
            &depressed_displacement(theta_line, index) *&
            &dcos(theta_line)
            xsb_tran = xsb_tran -&
            &depressed_displacement(theta_line, index) *&
            &dsin(theta_line)
            ysb_tran = ysb_tran +&
            &depressed_displacement(theta_line, index) *&
            &dcos(theta_line)
         end if

         xsb_rot(index) = xsb_tran * dcos(thetaw) +&
         &ysb_tran * dsin(thetaw)
         ysb_rot(index) = -1.0d0 * xsb_tran * dsin(thetaw) +&
         &ysb_tran * dcos(thetaw)
         xse_rot(index) = xse_tran * dcos(thetaw) +&
         &yse_tran * dsin(thetaw)
         yse_rot(index) = -1.0d0 * xse_tran * dsin(thetaw) +&
         &yse_tran * dcos(thetaw)

!           For barrier algorithm: Calculate the beginning coordinates of the barrier.
!           After rotation, determine if the barrier is downwind of the roadway. If so, set a flag.
         if(theta_line == thetaw) theta_line = theta_line + 0.001d0
         do i = 1, 2
            xbb     = xsb_tran + (dclwall_loc(i) - dcl_loc) *&
            &dsin(theta_line) * dsign(1.0d0, dsin(theta_line))
            ybb     = ysb_tran - (dclwall_loc(i) - dcl_loc) *&
            &dcos(theta_line) * dsign(1.0d0, dsin(theta_line))
            xbb_rot = xbb * dcos(thetaw) + ybb * dsin(thetaw)
            if(xbb_rot > xsb_rot(index)) bdw_flag(index, i) = 1
         end do

      end if
   end do

   do index = 1, numrec
      xr_tran = axr(index) - x0
      yr_tran = ayr(index) - y0
      xrcp_rot(index) = xr_tran * dcos(thetaw) +&
      &yr_tran * dsin(thetaw)
      yrcp_rot(index) = -1.0d0 * xr_tran * dsin(thetaw) +&
      &yr_tran * dcos(thetaw)
   end do

end subroutine translate_rotate
