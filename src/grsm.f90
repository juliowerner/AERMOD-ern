subroutine grsm_calc
!***********************************************************************
!        GRSM_CALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Hourly Results for GRSM Option
!
!        PROGRAMMER: CERC
!
!        DATE:    November 2020
!
!        MODIFIED:  CERC adjustments to calculation of instantaneous
!        plume spread (PLUMESIZES), building effects on
!        plume spread, multiple plume effects on plume spread, and
!        setting 0.1 m minimum distance for downstream receptors and
!        dispersion time for going between pancake and gaussian plumes.
!        See CERC technical memo dated January 17, 2023.
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!
!        CALLED FROM:   HRLOOP, EV_LOOP, MAXDCALC
!***********************************************************************
   use main1
   use grsmmod
   implicit none

! --- Local variables
   integer::numgrp_use, igrp_use, nStat
   character :: modnam*12, NightStr*3
   double precision, allocatable::plumebkgno2(:), plumebkgno(:) !Plume background concentrations
   double precision, allocatable:: noxpersource(:), plumebkgo3(:) !NOX concentration for each source
   double precision, allocatable::sigysigz_i_x(:), sigysigz_e_x(:) !Instantaneous and ensemble plume size at X
   double precision::sigysigz_i_0  !Instantaneous plume size at origin
   double precision::minbkgno2, minbkgno, minbkgo3 !Minimum background concentrations
   double precision::concmaxnox, instconcnox !Maximum and total NOX concentrations
   double precision::concsourceno2, concsourceno !Concentrations for a single source
   double precision::ensconcno2, ensconcno, instconcno2, instconcno !Total ensemble and instantaneous concentrations
   double precision::NO2Frac !NO2 fraction
   double precision::bgconc_in, bgconc_out, bgconc_ppb
   logical::L_DoFullConv
   double precision::fDum, MaxNOx, TotalNOx, fMultPlmFac
   double precision::fBldFacMaxMinusMin,fMultPlmFacMaxMinusMin

! --- Constants
   double precision, parameter::fDownstrmStart = 0.1d0
   double precision, parameter::fBldFacMin = 0.0d0
   double precision, parameter::fBldFacMax = 0.5d0
   double precision, parameter::fMultPlmFacMin = 0.25d0
   double precision, parameter::fMultPlmFacMax = 0.75d0

! --- Initialise
   modnam        = 'GRSM_CALC'
   L_DoFullConv  = .false.
   hrval         = 0.0d0
   sigysigz_i_0  = 0.0d0
   concsourceno2 = 0.0
   concsourceno  = 0.0
   concmaxnox    = 0.0
   minbkgno2     = 0.0
   minbkgno      = 0.0
   minbkgo3      = 0.0
   fBldFacMaxMinusMin = fBldFacMax - fBldFacMin
   fMultPlmFacMaxMinusMin = fMultPlmFacMax - fMultPlmFacMin

   if(allocated(plumebkgno2)) deallocate(plumebkgno2,stat=nStat)
   allocate(plumebkgno2(numsrc),stat=nStat)
   if(nStat/=0)then
      call errhdl(path, modnam, 'E','615','GRSM')
   end if
   if(allocated(plumebkgno)) deallocate(plumebkgno,stat=nStat)
   allocate(plumebkgno(numsrc),stat=nStat)
   if(nStat/=0)then
      call errhdl(path, modnam, 'E','615','GRSM')
   end if
   if(allocated(plumebkgo3)) deallocate(plumebkgo3,stat=nStat)
   allocate(plumebkgo3(numsrc),stat=nStat)
   if(nStat/=0)then
      call errhdl(path, modnam, 'E','615','GRSM')
   end if
   if(allocated(noxpersource)) deallocate(noxpersource,stat=nStat)
   allocate(noxpersource(numsrc),stat=nStat)
   if(nStat/=0)then
      call errhdl(path, modnam, 'E','615','GRSM')
   end if

   plumebkgno2   = 0.0
   plumebkgno    = 0.0
   plumebkgo3    = 0.0
   noxpersource  = 0.0

   !Calculate NOx reaction rates
   call rxnrates

   !Determine if its a night-time hour
   if(qsw>0.0d0)then
      L_NightHour = .false.
      NightStr    = 'NO'
   else
      L_NightHour = .true.
      NightStr    = 'YES'
   end if

   if(evonly)then
      bgconc_in = ev_bgconc(ihour)
   else
      bgconc_in = bgconc
   end if

! --- Initialise concs by converting from ug m-3 to ppb.  Don't want to alter input bgd concs
   no2conc_bg = bgconc_in*no2_ppb     !This is NO2 bgd
   if(noxmiss.or.L_CalcNOxFromNO2)then
      !No NOx bgd
   else
      noxconc_bg = noxbgconc*no2_ppb  !This is NOx bgd (NOx as NO2).
   end if

! --- Calculate the upwind concentrations using the given background
!     values. Assumes some type of equilibrium
   if(.not.o3miss)then
      !If O3 bgd is missing assume full conversion later
      o3conc_bg  = o3conc/o3_ppb      !This is O3 bgd
      call getbgdequil
   else
      o3conc_bg  = -999.
      L_DoFullConv=.true.
   end if

   if(evonly)then
      !Event only - 1 grp
      numgrp_use=1
   else
      !Normal calc - use actual number of groups
      numgrp_use=numgrp
   end if

! --- Begin Source Group LOOP to sum values
   do igrp = 1, numgrp_use

      if(evonly)then
         !Event only
         igrp_use=idxev(ievent)
      else
         !Normal calculation use actual group index
         igrp_use=igrp
      end if


      do irec = 1, numrec
         if(.not.L_DoFullConv)then
            !Not doing full conversion, doing proper chemistry

            instconcno2 = 0.0
            instconcno  = 0.0
            instconcnox = 0.0
            concmaxnox  = 0.0

            !Allocate variables for dilution and entrainment
            if(allocated(sigysigz_i_x)) deallocate(sigysigz_i_x,&
            &stat=nStat)
            if(allocated(sigysigz_e_x)) deallocate(sigysigz_e_x,&
            &stat=nStat)
            allocate(sigysigz_i_x(numsrc),stat=nStat)
            if(nStat/=0)then
               call errhdl(path, modnam, 'E','615','GRSM')
            end if
            allocate(sigysigz_e_x(numsrc),stat=nStat)
            if(nStat/=0)then
               call errhdl(path, modnam, 'E','615','GRSM')
            end if
            sigysigz_i_x(:) = 0.0d0
            sigysigz_e_x(:) = 0.0d0

            !Calculate total NOx and highest single contribution to total NOx
            MaxNOx = 0.0d0
            TotalNOx = 0.0d0
            do isrc = 1, numsrc
               if (igroup(isrc,igrp_use) == 0) cycle
               TotalNOx = TotalNOx + chi(irec,isrc,1)
               if(chi(irec,isrc,1) > MaxNOx) MaxNOx = chi(irec,isrc,1)
            enddo
            if(TotalNOx > 0.0d0)then
               fMultPlmFac = 1.d0 - MaxNOx/TotalNOx
            else
               fMultPlmFac = 0.0d0
            endif

            do isrc = 1, numsrc
               noxpersource(isrc) = 0.0d0
               !Is src included in group?
               if (igroup(isrc,igrp_use) == 0) cycle
               if ( chi(irec,isrc,1) == 0.0d0) cycle
! ---           Get concentrations
               concsourceno2 =&
               &ano2_ratio(isrc)*chi(irec,isrc,1)*no2_ppb         !This is the NO2 conc
               concsourceno  =&
               &(1.0d0-ano2_ratio(isrc))*chi(irec,isrc,1)*no2_ppb  !This is the NO conc

               !Find downwind distance to current receptor
               call setsrc
               wdsin = awdsin(isrc)
               wdcos = awdcos(isrc)
               if (evonly) then
                  call xydist(ievent)
               else
                  call xydist(irec)
               end if

               !Implement dilution and entrainment for downstream receptors
               if(x > fDownstrmStart)then

                  !Find the sizes of the instantaneous plume at origin (source)
                  call plumesizes(0.0d0,sigysigz_i_0,fDum)

                  !Find the sizes of the plume at X
                  call plumesizes(x,sigysigz_i_x(isrc),sigysigz_e_x(isrc))

                  !Ensure I_0 <= I_X <= E_X
                  if(sigysigz_i_x(isrc) > sigysigz_e_x(isrc))&
                  &sigysigz_i_x(isrc) = sigysigz_e_x(isrc)
                  if(sigysigz_i_0 > sigysigz_i_x(isrc))&
                  &sigysigz_i_0 = sigysigz_i_x(isrc)

!***************************************************************************************
!*************** BEGIN BUILDING EFFECTS ADJUSTMENT *************************************
!*************** (Comment out this block to remove adjustment) *************************
!***************************************************************************************
                  !Tend calculated instantaneous plume spread back towards ensemble
                  !plume spread in regions that are significnatly buildings-affected
                  if(bldfac(irec,isrc) <= fBldFacMin)then
                     !Do nothing
                  elseif(bldfac(irec,isrc) >= fBldFacMax)then
                     sigysigz_i_x(isrc) = sigysigz_e_x(isrc)
                  else
                     sigysigz_i_x(isrc) = sigysigz_i_x(isrc) +&
                     &(sigysigz_e_x(isrc) - sigysigz_i_x(isrc))*&
                     &(bldfac(irec,isrc)-fBldFacMin)/fBldFacMaxMinusMin
                  endif
!***************************************************************************************
!*************** END BUILDING EFFECTS ADJUSTMENT ***************************************
!***************************************************************************************

                  !Also tend instantaneous plume spread back towards ensemble
                  !plume spread in regions with multiple significant plumes
                  if(fMultPlmFac <= fMultPlmFacMin)then
                     !Do nothing
                  elseif(fMultPlmFac >= fMultPlmFacMax)then
                     sigysigz_i_x(isrc) = sigysigz_e_x(isrc)
                  else
                     sigysigz_i_x(isrc) = sigysigz_i_x(isrc) +&
                     &(sigysigz_e_x(isrc) - sigysigz_i_x(isrc))*&
                     &(fMultPlmFac-fMultPlmFacMin)/fMultPlmFacMaxMinusMin
                  endif

                  !Get the instantaneous plume concentration
                  concsourceno2= concsourceno2*&
                  &sigysigz_e_x(isrc)/(sigysigz_i_x(isrc))
                  concsourceno = concsourceno*&
                  &sigysigz_e_x(isrc)/(sigysigz_i_x(isrc))

                  !Calculate the entrained concentration
                  if(grp_back(igrp_use).and..not.l_nighthour)then
                     plumebkgno2(isrc)= (1.0d0-&
                     &sigysigz_i_0/sigysigz_i_x(isrc))*no2conc_bg
                     plumebkgno(isrc) = (1.0d0-&
                     &sigysigz_i_0/sigysigz_i_x(isrc))*noconc_bg
                  else
                     plumebkgno2(isrc)= 0.0d0
                     plumebkgno(isrc) = 0.0d0
                  end if

                  plumebkgo3(isrc)=(1.0d0-sigysigz_i_0/&
                  &sigysigz_i_x(isrc))*o3conc_bg
               else
                  !Upwind fully entrains background
                  if(grp_back(igrp_use).and..not.l_nighthour)then
                     plumebkgno2(isrc)= no2conc_bg
                     plumebkgno(isrc) = noconc_bg
                  else
                     plumebkgno2(isrc)= 0.0d0
                     plumebkgno(isrc) = 0.0d0
                  end if
                  plumebkgo3(isrc)= o3conc_bg
               end if

               instconcno2 = instconcno2 + concsourceno2
               instconcno  = instconcno  + concsourceno
               noxpersource(isrc) = concsourceno2 + concsourceno
               instconcnox = instconcnox + noxpersource(isrc)

               if (noxpersource(isrc) > concmaxnox) then
                  concmaxnox = noxpersource(isrc)
               end if

            end do

            if(grp_back(igrp_use).and..not.l_nighthour)then
               minbkgno2 = no2conc_bg
               minbkgno  = noconc_bg
            else
               minbkgno2 = 0.0d0
               minbkgno  = 0.0d0
            end if
            minbkgo3  = o3conc_bg

            !Calculate the minimum background based on O3
            do isrc = 1, numsrc
               if (noxpersource(isrc)<=0.5*concmaxnox) cycle
               if (minbkgo3 > plumebkgo3(isrc)) then
                  minbkgno2 = plumebkgno2(isrc)
                  minbkgno  = plumebkgno(isrc)
                  minbkgo3  = plumebkgo3(isrc)
               end if
            end do

            !Put this set of background values into equilibrium
            if(grp_back(igrp_use).and..not.l_nighthour)then
               call QuadraticEquil(minbkgno2,minbkgno,minbkgo3)
            endif

            instconcno2 = instconcno2 + minbkgno2
            instconcno  = instconcno  + minbkgno

! ---       Calculate weighted time
            if(sum(chi(irec,:,1),mask=igroup(:,igrp_use)==1)/=0.0d0)then
               ttravchm(irec)=&
               &sum(chi_ttravchm(irec,:),mask=igroup(:,igrp_use)==1)/&
               &sum(chi(irec,:,1),mask=igroup(:,igrp_use)==1)
            else
               ttravchm(irec)=0.0d0
            end if
! ---       Set time to be a maximum of one hour
            ttravchm(irec)=min(ttravchm(irec),3600.0d0)

            conctemp(nNO)  = instconcno
            conctemp(nNO2) = instconcno2
            conctemp(nO3)  = minbkgo3

! ---       Do GRS chemistry without dilution and entrainment
            call DoGRSMChem

            instconcno  = conctemp(nNO)  - minbkgno
            instconcno2 = conctemp(nNO2) - minbkgno2

            ensconcno   = 0.0
            ensconcno2  = 0.0

            do isrc = 1, numsrc
               if (instconcnox==0.or.noxpersource(isrc)==0) then
                  concsourceno2 = 0.0
                  concsourceno  = 0.0
               else
                  concsourceno2=instconcno2*noxpersource(isrc)/instconcnox
                  concsourceno =instconcno *noxpersource(isrc)/instconcnox

                  call setsrc
                  wdsin = awdsin(isrc)
                  wdcos = awdcos(isrc)
                  if (evonly) then
                     call xydist(ievent)
                  else
                     call xydist(irec)
                  end if
                  if(x > fDownstrmStart)then
                     concsourceno2=concsourceno2*&
                     &sigysigz_i_x(isrc)/(sigysigz_e_x(isrc))
                     concsourceno =concsourceno*&
                     &sigysigz_i_x(isrc)/(sigysigz_e_x(isrc))
                  end if
               end if

               ensconcno2 = ensconcno2 + concsourceno2
               ensconcno  = ensconcno  + concsourceno
            end do

            if(grp_back(igrp_use).and..not.l_nighthour)then
               ensconcno2 = ensconcno2 + no2conc_bg
               ensconcno  = ensconcno  + noconc_bg
            end if

! ---       Calculate secondary NO2 fraction after chemistry
            if((ensconcno2+ensconcno)/=0.0d0)then
               NO2Frac = ensconcno2 / (ensconcno2 + ensconcno)
            else
               NO2Frac = 0.0d0
            end if
! ---       Calculate post chemistry concentrations from secondary NO2 fraction, partitioning background and plume
            if(l_nighthour)then
               !Night-time
               bgconc_out = no2conc_bg/no2_ppb  !No NO at night-time
               bgconc_ppb = no2conc_bg
            else
               !Day-time
               !Background NO2 (other background concs not o/p)
               bgconc_out = NO2Frac * noxconc_bg/no2_ppb
               bgconc_ppb = NO2Frac * noxconc_bg
            end if
         else  !Missing O3 or NOx doing full conversion
            NO2Frac=1.0d0
            !Warning has already been issued in O3EXT
            bgconc_out = bgconc_in
            bgconc_ppb = bgconc_in*no2_ppb
         end if


         source_loop: do isrc = 1, numsrc

            if( igroup(isrc,igrp_use) /= 1 )then
! ---         This source is not included in the current SRCGROUP
               cycle source_loop
            end if

            !Plume
            do ityp = 1,numtyp
               hrval(ityp) = NO2Frac * chi(irec,isrc,ityp)
            end do

! ---       Call SUMVAL to add current source's contribution to AVEVAL
            if( .not.evonly )then
! ---         Processing for NON-EVENT application
               call sumval

! ---         Write debug file
               if (grsmdebug) then
                  if (igroup(isrc,igrp) == 1) then
                     write(grsmdbg,9989,err=999) kurdat, irec,&
                     &grpid(igrp),  isrc, srcid(isrc), nightstr,&
                     &o3conc_bg,noxconc_bg,no2conc_bg,ttravchm(irec),&
                     &chi_ttravchm(irec,isrc), ano2_ratio(isrc),&
                     &chi(irec,isrc,1), NO2Frac, bgconc_ppb, hrval(1),&
                     &aveval(irec,igrp,1,1),sigysigz_i_x(isrc),&
                     &sigysigz_e_x(isrc), bldfac(irec,isrc),fMultPlmFac
                  end if
               end if
9989           format(1x,7x,i8.8,2x,i6,2x,a8,2x,i6,2x,a12,2x,a3,8x,2x,7x,&
               &e12.5,2x,8x,e12.5,2x,8x,e12.5,2x,3x,e12.5,2x,20x,&
               &e12.5,2x,4x,e12.5,2x,13x,e12.5,2x,2x,e12.5,2x,7x,&
               &e12.5,2x,5x,e12.5,2x,6x,e12.5,2x,7x,e12.5,2x,10x,&
               &e12.5,2x,4x,e12.5,2x,6x,e12.5)
            else
! ---         Processing for EVENT application
               call ev_sumval

! ---         Write debug file
               if (grsmdebug) then
                  if (igroup(isrc,idxev(ievent)) == 1) then
                     write(grsmdbg,9990,err=999) kurdat, ievent,&
                     &evname(ievent), evaper(ievent),&
                     &grpid(idxev(ievent)), isrc, srcid(isrc),&
                     &nightstr, o3conc_bg,noxconc_bg, no2conc_bg,&
                     &ttravchm(irec), chi_ttravchm(irec,isrc),&
                     &ano2_ratio(isrc),chi(irec,isrc,1), NO2Frac,&
                     &bgconc_ppb, hrval(1), grpave(idxev(ievent)),&
                     &sigysigz_i_x(isrc),sigysigz_e_x(isrc),&
                     &bldfac(irec,isrc), fMultPlmFac
                  end if
               end if
9990           format(1x,7x,i8.8,2x,i6,2x,a10,2x,1x,i3,2x,a8,2x,i6,2x,&
               &a12,2x,a3,8x,2x,7x,e12.5,2x,8x,e12.5,2x,8x,e12.5,&
               &2x,3x,e12.5,2x,20x,e12.5,2x,4x,e12.5,2x,13x,e12.5,&
               &2x,2x,e12.5,2x,7x,e12.5,2x,5x,e12.5,2x,9x,e12.5,2x,&
               &7x,e12.5,2x,10x,e12.5,2x,4x,e12.5,2x,6x,e12.5)
            end if

            if (eval(isrc)) then
! ---         Check ARC centerline values for EVALFILE output - this is same for EVENT/NON-EVENT
               call evalck
            end if

! ---       ReInitialize __VAL arrays (1:NUMTYP)
            hrval  = 0.0d0
         end do source_loop

         if (grp_back(igrp_use)) then
            if( .not.evonly )then
! ---         Call SUMBACK_NO2 to update BACKGROUND contributions
               bgconc = bgconc_out
               call sumback_no2
            else
! ---         Call EV_SUMBACK to update BACKGROUND contributions
               ev_bgconc(ihour) = bgconc_out
               call ev_sumback
            endif
         endif
      end do  !Loop over points
   end do  !Loop over groups

   go to 3456
999 call errhdl(path,modnam,'E','520','GRSMDEBUG')
3456 continue

   return
end subroutine grsm_calc

subroutine rxnrates
!***********************************************************************
!        RxnRates Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculate the reaction rates for GRS chemistry
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
!        INPUTS:
!
!        OUTPUTS:
!
!        NOTES:
!
!        R1, R2 reaction rates for
!
!                 NO + O3 -> NO2         (1)
!
!                 NO2 + hv -> NO + O3      (2)
!
!        CALLED FROM: GRSM_CALC
!***********************************************************************

!     Variable Declarations
   use main1
   use grsmmod
   implicit none
   character :: modnam*12

   double precision, parameter:: scrnht=1.2d0
   double precision, parameter:: const1=4.405d-2
   double precision, parameter:: exp1=1.370d3
   double precision, parameter:: const2a=8.0d-4
   double precision, parameter:: const2b=7.4d-6
   double precision, parameter:: exp2=10.0d0
   double precision:: chemtk, chempt

   integer::nscrnht

! --- Variable Initializations
   modnam = 'RXNRATES'

! --- Determine temperature (K) at screen height
   if(trefht==scrnht)then
      !Reference height is screen height
      chemtk=ta
   else
      !Interpolate from potential temperature
      call locate(gridht, 1, mxglvl, scrnht, nscrnht)
      call gintrp(gridht(nscrnht),gridpt(nscrnht),gridht(nscrnht+1),&
      &gridpt(nscrnht+1),scrnht,chempt)
      !Convert to temperature
      chemtk=chempt-govrcp*(scrnht+zbase)
   end if

! --- Calculate reaction rates
   r1=const1*exp(-exp1/chemtk)
   r1 = max(r1,1.0d-6) !1e-6 corresponds to temperature of around -150 degC
   if (qsw==0) then
      r2=0.0
   else
      r2=const2a*exp(-exp2/qsw)+const2b*qsw
   end if

   return
end subroutine rxnrates


subroutine getbgdequil
!***********************************************************************
!        GETBDGEQUIL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculate the equilibrium background for GRS chemistry
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM: GRSM_CALC
!***********************************************************************
!     Variable Declarations
   use main1
   use grsmmod
   implicit none
   character :: modnam*12

! --- Variable Initializations
   modnam = 'GETBGDEQUIL'
! --- Check for day or night
   if (.not.l_nighthour) then
      if(L_CalcNOxFromNO2.or.noxmiss)then
! ---     Calculate NOx bgd from NO2
! ---     Solve equilibrium eqns for NOx
         if(o3conc_bg/=0.0d0)then
            noxconc_bg=no2conc_bg/(o3conc_bg)*(o3conc_bg+r2/r1)
            noconc_bg=noxconc_bg-no2conc_bg
         end if
      else
!Calculate NO2 background from NOX values
! ---     If the concentration of NOx < NO2, then complain
         if (noxconc_bg<no2conc_bg) then
            call errhdl(path, modnam, 'W','613','GRSM')
            noxconc_bg = no2conc_bg
         end if
         noconc_bg = noxconc_bg - no2conc_bg
         call QuadraticEquil(no2conc_bg,noconc_bg,o3conc_bg)
      end if
   else ! Nighttime
      if(L_CalcNOxFromNO2.or.noxmiss)then
         !NOx is missing but don't need it
         noxconc_bg=-999.0d0
         noconc_bg=0.0d0
      else
         ! No adjustments made to background at night
         noconc_bg = noxconc_bg - no2conc_bg
      end if
   end if

   return
end subroutine getbgdequil

subroutine QuadraticEquil(no2,no,o3)
!***********************************************************************
!        QuadraticEquil Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Puts NO2, NO and O3 concentrations into photo-stationary
!                 equilibrium, conserving mass of NOx and NO2 + O3
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
!        INPUTS:  NO2 - Input NO2 concentration in volumetric units
!                 NO - Input NO concentraiton in volumetric units
!                 O3 - Input O3 concentration involumetric units
!
!        OUTPUTS: NO2 - Equilibrium NO2 concentration in the same volumetric units
!                 NO - Equilibrium NO concentraiton in the same volumetric units
!                 O3 - Equilibrium O3 concentration in the same volumetric units
!
!        CALLED FROM: GRSM_CALC, GETBGDEQUIL
!***********************************************************************
   use grsmmod
   implicit none
! --- Arguments
   double precision, intent(inout)::no2,no,o3
! --- Local variables
   double precision :: b, c, nox, NO2plusO3

! --- We have 3 equations in 3 unknowns:
!     1. Photo-stationary equilibrium
!      [NO2] = (R1/R2)*[NO]*[O3]
!     2. Conservation of NOx
!      [NO2] + [NO] = [NO2]initial + [NO]initial = [NOX]
!     3. Conservation of [NO2] + [O3]
!      [NO2] + [O3] = [NO2]initial + [O3]initial
!     These 3 lead to quadratic in [NO2]

! --- Set conserved quantities:
   nox = no2 + no
   NO2plusO3 = no2 + o3

! --- Quadratic in final [NO2]:  [NO2]*[NO2] + B*[NO2] + C = 0
!     where:
   b = -(nox + NO2plusO3 + (r2/r1))
   c = nox*NO2plusO3

! --- Take negative root, as positive root will be bigger than NO2plusO3 and NOX
   no2 = 0.5*(-b - sqrt(b*b - 4.*c))

! --- Use equations 2 and 3 above to calculate [NO] and [O3]
   no = nox - no2
   o3 = NO2plusO3 - no2

   return
end subroutine QuadraticEquil

function FindInitDt()
!***********************************************************************
!        FindInitDt Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Find initial GRSM chemistry time step
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM: DoGRSMChem
!***********************************************************************
   use grsmmod
   implicit none
! --- Local variables
   integer::ConcLoop
   double precision:: dCdt(nPolsGRSM), TimeScale, TimeLoop
   double precision:: FindInitDt
   character::modnam*12

! --- Initialisations
   modnam='FindInitDt'

! --- Calculate the first order dreivatives
   call DConcsDt(conctemp,dCdt)

! --- Loop through each non zero concentration and estimate the
!     'time scale' for each pollutant. We want to find the minimum non-
!     zero time period so that all pollutants change by no more than
!     CFrac*Initial Concentration.

   TimeScale = 1. ! initial value
   do ConcLoop = 1, nPolsGRSM
      if ( ConcTemp(ConcLoop )>0.) then
         if(abs(dCdt(ConcLoop))>CFrac*ConcTemp(ConcLoop))then
            TimeLoop = CFrac*abs(ConcTemp(ConcLoop)/dCdt(ConcLoop))
            if ( TimeLoop<TimeScale ) TimeScale = TimeLoop
         end if
      end if
   end do

   FindInitDt = TimeScale

   return
end function FindInitDt

subroutine DConcsDt(conc, dCdt)
!***********************************************************************
!        DConcsDt Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculate first-order time derivatives of each chemistry
!                 pollutant from the reaction equations
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
!        INPUTS:  CONC - input concentrations
!
!        OUTPUTS: dCdt - first-order time derivatives
!
!        CALLED FROM: DoGRSMChem, FindInitDt, CashKarpRK
!***********************************************************************
   use grsmmod
   implicit none
! --- Arguments
   double precision, intent(out):: dCdt(nPolsGRSM)
   double precision, intent(inout):: conc(nPolsGRSM)

! --- Local variables
   character::modnam*12
!RCO 3/4/2021 Removing unused variable
!      INTEGER::I

! --- Initialisations
   modnam='DConcsDt'

! --- Calculate VALUE OF dC/dt

! --- NO2
   dCdt(nNO2)=r1*conc(nNO)*conc(nO3) - r2*conc(nNO2)
! --- NO
   dCdt(nNO)=r2*conc(nNO2) - r1*conc(nNO)*conc(nO3)
! --- O3
   dCdt(nO3)=r2*conc(nNO2) - r1*conc(nNO)*conc(nO3)

   return
end subroutine DConcsDt

subroutine DoGRSMChem
!***********************************************************************
!        DoGRSMChem Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculate the final concentrations after applying
!                 GRSM chemistry scheme
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
! MKP    Modified:  4/23/2024 D193 fix from CERC. Prevents denormal or
!                   very small concentration values from being passed to
!                   chemistry solver resulting in NaNs for certain ground
!                   level releases from area, volume, and openpit source types
!                   See modules.f/GRSMMOD MinimumConc=1.0D-21
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM: GRSM_CALC
!***********************************************************************
   use main1
   use grsmmod
   implicit none

! --- Local variables
   character::modnam*12
   integer, parameter::maxstp=1000000
   integer:: i,nstp
   double precision:: dt, dtnext, tLocal,&
   &dCdt(nPolsGRSM),Cscal(nPolsGRSM)
   double precision, parameter::dtmin=1.0d-6
! --- functions
   double precision::FindInitDt
!
! --- Initialisations
   modnam='DoGRSMChem'
   tLocal=0.d0

! --- Check for concentrations less than the minimum value
   do  i=1,nPolsGRSM
      if (conctemp(i)<MinimumConc) conctemp(i) = 0.d0
   end do

! --- Estimate initial time step
   dt = FindInitDt()

! --- Increment in timesteps until end time reached or we hit max No. of steps
   do nstp=1,maxstp

!       Calculate the derivatives
      call DConcsDt(conctemp,dCdt)

      !Check for concentrations less than the minimum value and negative gradients
      do i=1,nPolsGRSM
         if (conctemp(i)<MinimumConc .and. dCdt(i)<0.d0)then
            conctemp(i) = 0.d0
            dCdt(i) = 0.d0
         endif
      enddo

      do i=1,nPolsGRSM
         Cscal(i)=conctemp(i)+abs(dt*dCdt(i))
      end do

!       Ensure we don't go past the travel time
      if(tLocal+dt>ttravchm(irec)) dt=ttravchm(irec)-tLocal

!       Perform Runge-Kutta integration using adaptive timestep
      call AdaptiveRK(conctemp,dCdt,tLocal,dt,Cscal,dtnext)

!       Check for negative concentrations
      do  i=1,nPolsGRSM
         if (conctemp(i)<0.) conctemp(i) = 0.
      end do

!       If we've reached the travel time, return
      if(tLocal>=ttravchm(irec)) return
!       Otherwise set the timestep for the next iteration
      if(dtnext<dtmin) dtnext=dtmin
      dt=dtnext
   end do
!     Reached maximum No. of time steps - return with latest concentrations
   return
end subroutine DoGRSMChem

subroutine AdaptiveRK(CLocal,dCdt,tLocal,dttry,Cscal,dtnext)
!***********************************************************************
!        AdaptiveRK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Performs Runge-Kutta integration using input timestep fisrt.
!                 If the error is too high, reduces the timestep and
!                 tries again until error is below threshold. Also tries
!                 to set a sensible timestep for the next time this
!                 routine is called based on the magnitude of the error
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
!        INPUTS:  CLocal - Input concentrations
!                 dCdt - Time derivatives of input concentrations
!                 tLocal - Total time so far
!                 dttry - First timestep to try
!                 Cscal - CLocal(i)+abs(dttry*dCdt(i))
!
!        OUTPUTS: tLocal - Total time so far (updated)
!                 CLocal - Output concentrations (updated)
!                 dtnext - Timestep to use next time around
!
!        CALLED FROM: DoGRSMChem
!***********************************************************************
   use main1
   use grsmmod
   implicit none
!
! --- Arguments
   double precision, intent(inout)::CLocal(nPolsGRSM), tLocal
   double precision, intent(in)::dttry, dCdt(nPolsGRSM),&
   &Cscal(nPolsGRSM)
   double precision, intent(out)::dtnext

! --- Local variables
   character::modnam*12
   integer::i
   double precision::dt, CTemp(nPolsGRSM), GrowthFac,&
   &CErr(nPolsGRSM), ErrMax, ttemp, tNew
   !P_Shrink - Determines how much we shrink dt by for the next
   !iteration if the error is too high
   double precision, parameter::P_Shrink=-0.25d0
   !P_Grow - Determines how much we increase dt by for the next
   !call to this routine
   double precision, parameter::P_Grow=-0.2d0
   !MaxGrow - Maximum growth factor for dt
   double precision, parameter::MaxGrow=5.d0
   !SAFETY - reduces dt slightly for the next iteration
   double precision, parameter::safety=0.9d0

! --- Initialisations
   modnam = 'AdaptiveRK'
   dt=dttry
1  call CashKarpRK(CLocal,dCdt,dt,CTemp,CErr)
   errmax=0.

   do i=1,nPolsGRSM
      if (Cscal(i)>MinimumConc)then
         errmax=max(errmax,abs(CErr(i)/Cscal(i)))
      end if
   end do

   errmax=errmax/CFrac
   if(errmax>1.)then
      !Error too large - shrink the timestep and try again
      ttemp=safety*dt*(errmax**P_Shrink)
      dt=max(ttemp,0.1*dt)
      tNew=tLocal+dt
      goto 1
   else
      !Error below threshold - calculate initial timestep for
      !next call to this routine and return
      GrowthFac = safety*(errmax**P_Grow)
      if(GrowthFac>MaxGrow)then
         dtnext=MaxGrow*dt
      else
         dtnext=GrowthFac*dt
      end if
      tLocal=tLocal+dt
      do i=1,nPolsGRSM
         CLocal(i)=CTemp(i)
      end do
      return
   end if
end subroutine AdaptiveRK

subroutine CashKarpRK(c,dCdt,dt,COut,CErr)
!***********************************************************************
!        CashKarpRK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Performs Runge-Kutta integration (Cash-Karp method) once
!                 using input timestep. Also calculates error.
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
!        INPUTS:  C - Input concentrations
!                 dCdt - Time derivatives of input concentrations
!                 dt - Timestep to use
!
!        OUTPUTS: COut - Output concentrations
!                 CErr - Error per pollutant
!
!        CALLED FROM: AdaptiveRK
!***********************************************************************
   use grsmmod
   implicit none

! --- Arguments
   double precision, intent(in)::dt,dCdt(nPolsGRSM),&
   &c(nPolsGRSM)
   double precision, intent(out)::CErr(nPolsGRSM),&
   &COut(nPolsGRSM)

! --- Local variables
   character::modnam*12
   integer::i
   double precision:: ak2(nPolsGRSM),ak3(nPolsGRSM),&
   &ak4(nPolsGRSM),ak5(nPolsGRSM),&
   &ak6(nPolsGRSM),CTemp(nPolsGRSM)
!RCO 3/4/2021 removing unused variables
!      DOUBLE PRECISION::A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,
   double precision::b21,b31,b32,b41,b42,b43,b51,&
   &b52,b53,b54,b61,b62,b63,b64,b65,c1,c3,c4,c6,&
   &dc1,dc3,dc4,dc5,dc6
!      PARAMETER (A2=.2D0,A3=.3D0,A4=.6D0,A5=1.D0,A6=.875D0,B21=.2D0,
   parameter (b21=.2d0,&
   &b31=3.d0/40.d0,b32=9.d0/40.d0,b41=.3d0,b42=-.9d0,b43=1.2d0,&
   &b51=-11.d0/54.d0,b52=2.5d0,b53=-70.d0/27.d0,b54=35.d0/27.d0,&
   &b61=1631.d0/55296.d0,b62=175.d0/512.d0,b63=575.d0/13824.d0,&
   &b64=44275.d0/110592.d0,b65=253.d0/4096.d0,c1=37.d0/378.d0,&
   &c3=250.d0/621.d0,c4=125.d0/594.d0,c6=512.d0/1771.d0,&
   &dc1=c1-2825.d0/27648.d0,dc3=c3-18575.d0/48384.d0,&
   &dc4=c4-13525.d0/55296.d0,dc5=-277.d0/14336.d0,dc6=c6-.25d0)

! --- Initialisations
   modnam = 'CashKarpRK'

   do i=1,nPolsGRSM
      CTemp(i)=c(i)+b21*dt*dCdt(i)
      if(CTemp(i)<0.) CTemp(i) = 0. !Prevent negative concs
   end do
   call DConcsDt(CTemp,ak2)
   do i=1,nPolsGRSM
      CTemp(i)=c(i)+dt*(b31*dCdt(i)+b32*ak2(i))
      if(CTemp(i)<0.) CTemp(i) = 0. !Prevent negative concs
   end do
   call DConcsDt(CTemp,ak3)
   do i=1,nPolsGRSM
      CTemp(i)=c(i)+dt*(b41*dCdt(i)+b42*ak2(i)+b43*ak3(i))
      if(CTemp(i)<0.) CTemp(i) = 0. !Prevent negative concs
   end do
   call DConcsDt(CTemp,ak4)
   do i=1,nPolsGRSM
      CTemp(i)=c(i)+dt*(b51*dCdt(i)+b52*ak2(i)+b53*ak3(i)+b54*ak4(i))
      if(CTemp(i)<0.) CTemp(i) = 0. !Prevent negative concs
   end do
   call DConcsDt(CTemp,ak5)
   do i=1,nPolsGRSM
      CTemp(i)=c(i)+dt*(b61*dCdt(i)+b62*ak2(i)+b63*ak3(i)+b64*ak4(i)+&
      &b65*ak5(i))
      if(CTemp(i)<0.) CTemp(i) = 0. !Prevent negative concs
   end do
   call DConcsDt(CTemp,ak6)
   do i=1,nPolsGRSM
      COut(i)=c(i)+dt*(c1*dCdt(i)+c3*ak3(i)+c4*ak4(i)+c6*ak6(i))
   end do
   do i=1,nPolsGRSM
      CErr(i)=dt*(dc1*dCdt(i)+dc3*ak3(i)+dc4*ak4(i)+dc5*ak5(i)+dc6*&
      &ak6(i))
   end do
   return
end subroutine CashKarpRK


subroutine plumesizes(xarg,sigysigz_i,sigysigz_e)
!***********************************************************************
!        PLUMESIZES Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Gets the size of instantaneous & ensemble plumes at X
!
!        PROGRAMMER: CERC
!
!        DATE: November 2020
!
!        MODIFIED: To add the plume rise for VOLUME/AREA Aircraft Sources.
!                  Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                  04/01/2023
!
!        INPUTS:   XARG - downwind location
!
!        OUTPUTS: SIGYSIGZ_I,SIGYSIGZ_E - instantaneous and ensemble plume size
!
!        CALLED FROM: GRSM_CALC
!***********************************************************************
   use main1
   implicit none

   !Arguments
   double precision, intent(in)::xarg
   double precision, intent(inout)::sigysigz_i, sigysigz_e

   !Local variables
   character::modnam*12
   character(8)::sSrcTypeName
   double precision::sigy, sigz, sigy_i, sigz_i, sigy_e, sigz_e
   double precision::SIGY_NoTurb, SIGZ_NoTurb, time
!RCO 3/4/2021 removing unused variables
!      DOUBLE PRECISION::EPS, T, DUM1, DUM2, FRAN
   double precision::eps, fran
!      DOUBLE PRECISION::VSEQ, HSPRIM, ZPLM, DHFOLD, SVPM, SVPM2, UPM
   double precision::vseq, hsprim, zplm, dhfold, svpm, upm, swpm   ! SWPM - Added for ARISE; UNC-IE
   double precision::tgpm, ptpm, ptp, sigr2
   double precision::SIGY_Turb2, sigy_d2, SIGZ_Turb2, sigz_d2
   integer::i, kiter, ndxzpl
   double precision, parameter::a0 = 0.2d0

   modnam='PLUMESIZES'

   !Initialise
   SIGY_NoTurb=0.0d0
   sigy=0.0d0
   SIGY_Turb2=0.0d0
   sigy_d2=0.0d0
   SIGZ_NoTurb=0.0d0
   sigz=0.0d0
   SIGZ_Turb2=0.0d0
   sigz_d2=0.0d0
   sigr2=0.0d0
   sigy_i=0.0d0
   sigz_i=0.0d0
   sigy_e=0.0d0
   sigz_e=0.0d0


!     Set Mixing Height and Profiles for Urban Option if Needed
   if (urbsrc(isrc) == 'Y') then
!        Find Urban Area Index for This Source
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
   else if (urban .and. urbsrc(isrc) == 'N') then
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

!     Set the Source Variables for This Source           ---   CALL SETSRC
   call setsrc

!     Calculate the initial meteorological variables     ---   CALL METINI
   call metini

   if ((srctyp(isrc) == 'VOLUME' .and. aftsrc(isrc) == 'N').or.&  ! Added for Aircraft; UNC-IE
   &srctyp(isrc) == 'LINE' .or.&
   &(srctyp(isrc)(1:4) == 'AREA' .and. aftsrc(isrc) == 'N').or.&! Added for Aircraft; UNC-IE
   &srctyp(isrc) == 'OPENPIT') then
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
      if(xmixed < xfinal) xmixed = xfinal
      zmidmx = 0.5d0 * zi
!        Define temporary values of CENTER and SURFAC based on HS
!        to account for non-POINT sources
      center = hecntr(irec,isrc)
      if( center < 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

!**  Added for Aircraft Plume Rise; UNC-IE
   else if ((srctyp(isrc) == 'VOLUME' .or.&
   &srctyp(isrc) (1:4) == 'AREA') .and.&
   &(aftsrc(isrc) == 'Y')) then
!        Calculate Buoyancy Fluxes                          ---   CALL AFLUXES
      call afluxes

!        Define temporary values of CENTER and SURFAC based on HS
      center = hecntr(irec,isrc)
      if( center < 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

!        Calculate Distance to Final Rise                   ---   CALL ADISTF
      call adistf

!        Calculate the plume penetration factor             ---   CALL PENFCT
      call penfct

      if (stable .or. (unstab.and.(hs>=zi))) then
!           Use iterative approach to stable plume rise calculations
         kiter = 0
150      zplm = hsp + 0.5d0 * dhfaer
         dhfold = dhfaer

!----       Locate index below ZPLM

         call locate(gridht, 1, mxglvl, zplm, ndxzpl)

!----       Get Wind speed at ZPLM; replace UP.  Also, replace SVP, SWP, TGP,
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
!RWB        Use average of stack top and midpoint wind speeds and other terms.
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
         if(bvf < 1.0d-10) bvf = 1.0d-10
         bvprim  = 0.7d0 * bvf

         call adistf

         kiter = kiter + 1

!           Check for convergence
         if(dabs((dhfold - dhfaer)/dhfaer) < 0.01d0) go to 160

         if(kiter >= 5) then
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
         if(bvf < 1.0d-10) bvf = 1.0d-10
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
      if (unstab .and. hs<zi) then
!           Check for XMIXED smaller than 1.25*XFINAL
         if (xmixed < 1.25d0*xfinal) then
            xfinal = 0.8d0 * xmixed
            call acblprd (xfinal)
            dhcrit = dhp1
         end if
      end if
!**  End Aircraft Plume Rise insert; April 2023

   else if (srctyp(isrc)(1:5) == 'POINT') then
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
      if( center < 0.1d0*zi )then
         surfac = .true.
      else
         surfac = .false.
      end if

! ---    Apply HSP for POINTCAP and POINTHOR first to avoid NOSTD option
!        overriding POINTCAP
      if (srctyp(isrc) == 'POINTCAP') then
!           Apply stack-tip downwash for capped stacks with VS = 0.001m/s
         hsp = hsprim ( us, vseq, hs, ds )
      else if (srctyp(isrc) == 'POINTHOR') then
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

      if (stable .or. (unstab.and.(hs>=zi))) then
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
         if(bvf < 1.0d-10) bvf = 1.0d-10
         bvprim  = 0.7d0 * bvf

         call distf

         kiter = kiter + 1

!           Check for convergence
         if(dabs((dhfold - dhfaer)/dhfaer) < 0.01d0) go to 60

         if(kiter >= 5) then
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
         if(bvf < 1.0d-10) bvf = 1.0d-10
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
      if (unstab .and. hs<zi) then
!           Check for XMIXED smaller than 1.25*XFINAL
         if (xmixed < 1.25d0*xfinal) then
            xfinal = 0.8d0 * xmixed
            call cblprd (xfinal)
            dhcrit = dhp1
         end if
      end if

   end if

!     Define plume centroid height (CENTER)
   call centroid ( xarg )

   if (srctyp(isrc)(1:5) == 'POINT') then
!       Calculate the plume rise                  ---   CALL DELTAH
      call deltah ( xarg )
!**  Added for Aircraft Plume Rise; UNC-IE
   else if ((srctyp(isrc) == 'VOLUME' .or.&
   &srctyp(isrc)(1:4) == 'AREA') .and.&
   &(aftsrc(isrc) == 'Y')) then
!       Calculate the plume rise                  ---   CALL ADELTAH
      call adeltah ( xarg )
!**  End Aircraft Plume Rise insert; April 2023
   else
! ---   Assign 0.0 to DHP plume rise variables for non-POINT sources
      dhp  = 0.0d0
      dhp1 = 0.0d0
      dhp2 = 0.0d0
      dhp3 = 0.0d0
   end if

!     If the atmosphere is unstable and the stack
!     top is below the mixing height, calculate
!     the CBL PDF coefficients                     ---   CALL PDF
   if( unstab  .and.  (hs < zi) ) then
      call pdf
   end if

!     Determine Effective Plume Height             ---   CALL HEFF
   call heff ( xarg )

!     Compute effective parameters using an
!     average through plume rise layer
   call iblval ( xarg )

!     Call PDF & HEFF again for final CBL plume heights
   if (unstab .and. (hs<zi) ) then
      call pdf
      call heff ( xarg )
   end if


   !Calculate SIGMA_Y and SIGMA_Z
   !Find source type
   sSrcTypeName=''
   sSrcTypeName=SrcTyp(isrc)

   !Get 'buoyancy-induced' spread component at current location, and add on 'source' spread component for SIGY.
   !Calling e.g. PDIS with the argument set to zero will set SY to the 'buoyancy-induced' component at the
   !current location because PDIS will only recalculate the 'turbulence' spread component (which is zero at
   !the source) but still include the 'buoyancy-induced' component in SY, which will still be set based on
   !the current location.
   select case(sSrcTypeName)
    case('POINT   ','POINTHOR','POINTCAP')
      call pdis(0.0d0)
      SIGY_NoTurb = sqrt(sy**2.0d0 + (ds/2.0d0)**2.0d0)
    case('LINE    ','BUOYLINE','AREA    ','AREAPOLY','AREACIRC',&
    &'OPENPIT ')
      call adisy(0.0d0)
      call adisz(0.0d0)
      SIGY_NoTurb = sqrt(sy**2.0d0 + (yinit/2.0d0)**2.0d0)
    case('VOLUME  ')
      call vdis(0.0d0)
      SIGY_NoTurb = sy
    case default
      !Unknown source type
      call errhdl(path,modnam,'E','614','')
   end select
   if( stable  .or.  (unstab .and. (hs >= zi) ) )then
      SIGZ_NoTurb = sz
   else
      SIGZ_NoTurb = 0.5d0*(szd1+szd2)
   end if

   !Get 'buoyancy-induced' plus 'turbulence' spread components at current location, and add
   !on 'source' spread component for SIGY.
   !Note that SIGY still doesn't include the meandering component but we don't want it to.
   select case(sSrcTypeName)
    case('POINT   ','POINTHOR','POINTCAP')
      call pdis(xarg)
      sigy = sqrt(sy**2.0d0 + (ds/2.0d0)**2.0d0)
    case('LINE    ','BUOYLINE','AREA    ','AREAPOLY','AREACIRC',&
    &'OPENPIT ')
      call adisy(xarg)
      call adisz(xarg)
      sigy = sqrt(sy**2.0d0 + (yinit/2.0d0)**2.0d0)
    case('VOLUME  ')
      call vdis(xarg)
      sigy = sy
    case default
      !Unknown source type
      call errhdl(path,modnam,'E','614','')
   end select
   if( stable  .or.  (unstab .and. (hs >= zi) ) )then
      sigz = sz
   else
      sigz = 0.5d0*(szd1+szd2)
   end if

   sigy = max(sigy,0.01d0)
   SIGY_NoTurb = max(SIGY_NoTurb,0.01d0)
   sigz = max(sigz,0.01d0)
   SIGZ_NoTurb = max(SIGZ_NoTurb,0.01d0)

   !Calculate the epsilon term
! MKP IF(ZI/OBULEN<=0.0D0)THEN
! MKP    !Convective turbulent dissipation
! MKP    EPS=(2.5D0/CENTER+2.0D0/ZI)*USTAR**3+0.4D0*(WSTAR**3)/ZI
! MKP ELSE
! MKP    !Stable turbulent dissipation
! MKP    EPS=(2.5D0/CENTER+4.0D0/ZI)*USTAR**3
! MKP END IF
!
! MKP Bug fix proposed by CERC 3/26/2024 where CENTROID returns zero for CENTER
!     for ground sources with no plume rise and stable hours, this causes divide
!     by zero NaN values in subsequent calcs that use SIGR2 below
   !Calculate the epsilon term
   if(zi/obulen<=0.0d0)then
      !Convective turbulent dissipation
      eps=(2.5d0/max(center,sfcz0,0.0001d0)+2.0d0/zi)*ustar**3+&
      &0.4d0*(wstar**3)/zi
   else
      !Stable turbulent dissipation
      eps=(2.5d0/max(center,sfcz0,0.0001d0)+4.0d0/zi)*ustar**3
   end if


   !Calculate dispersion time for Gaussian part of plume
   if (stable .or. (unstab.and.(hs>=zi))) then
      uchm=ueff
   else
      uchm=ueffd
   end if
   if (uchm/=0) then
      time = max(0.0d0,xarg/uchm) !Don't allow negative travel times
   else
      !Error - no travel time if UCHM is zero
      call errhdl(path, modnam, 'E','601','GRSM')
   end if

!      !Calculate the instantaneous plume spread at X
!RCO 2/25/2021 CERC determined to change the formulation for the instantaneous
!    plume spread. The original equation is equivalent to eq 3 in the
!    2017 JAWMA paper documenting the method and evaluation. This change
!    will need to be noted somewhere in documentation (white paper, maybe).
!      SIGY_I=(1.0D0/SIGY_NM+
!     &          (1.0D0/(SQRT((2.0D0/3.0D0)*EPS*TIME**3)+SIGYINIT)))**-1
!      SIGZ_I=(1.0D0/SIGZ+
!     &          (1.0D0/(SQRT((2.0D0/3.0D0)*EPS*TIME**3)+SIGYINIT)))**-1
!
! MKP  Previous SIGY, SIGZ, and SIGR2 coding from v22112, as per 2017
!      JAWMA paper
!      SIGY_I=MIN(SIGY_NM,SQRT((2.0D0/3.0D0)*EPS*TIME**3+SIGYINIT**2))
!      SIGZ_I=MIN(SIGZ,SQRT((2.0D0/3.0D0)*EPS*TIME**3+SIGYINIT**2))
!      SIGR2 = EPS*TIME**3.0D0/3.0D0
!
! MKP  See 2013-01-17 technical paper from CERC on changes to
! instantaneous plume spread calculations.
   sigr2 = eps*time**3.0d0/3.0d0

   SIGY_Turb2 = sigy**2.0d0 - SIGY_NoTurb**2.0d0
   if(SIGY_Turb2 <= 0.01d0)then
      sigy_i = sigy
   else
      sigy_d2 = SIGY_Turb2*sigr2/(SIGY_Turb2 +&
      &a0*sqrt(SIGY_Turb2)*sqrt(sigr2) + sigr2)
      sigy_i = sqrt(sigy_d2 + SIGY_NoTurb**2.0d0)
   endif

   SIGZ_Turb2 = sigz**2.0d0 - SIGZ_NoTurb**2.0d0
   if(SIGZ_Turb2 <= 0.01d0)then
      sigz_i = sigz
   else
      sigz_d2 = SIGZ_Turb2*sigr2/(SIGZ_Turb2 +&
      &a0*sqrt(SIGZ_Turb2)*sqrt(sigr2) + sigr2)
      sigz_i = sqrt(sigz_d2 + SIGZ_NoTurb**2.0d0)
   endif

   !Get fraction of random kinetic energy
   if (stable .or. (unstab.and.(hs>=zi))) then
      call meandr(ueff, sveff, fran)
   else
      call meandr(ueffd, sveffd, fran)
   end if
   !Get the meandered plume spread (ensemble)
   if(xarg/=0.0d0)then
      sigy_e=1.0d0/((fran/(srt2pi*xarg))+(1.0d0-fran)/sigy)
   else
      sigy_e=0.0
   end if
   sigz_e=sigz

   !Calculate return values
   sigysigz_i=sigy_i*sigz_i
   sigysigz_e=sigy_e*sigz_e

   return
end subroutine plumesizes
