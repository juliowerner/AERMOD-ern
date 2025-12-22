program aermod
!=======================================================================
!            MAIN Module of the AMS/EPA Regulatory Model - AERMOD
!                         Version Dated v14142
!
!                              May 21, 2024
!
!               *** SEE AERMOD MODEL CHANGE BULLETIN MCB #18 ***
!
!       ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS (SCRAM) WEBSITE
!
!                      https://www.epa.gov/scram
!
!=======================================================================
!
!       This revised version of AERMOD (dated 24142) includes
!       updates relative to the previous version (dated 23132);
!       see AERMOD Model Change Bullentin (MCB) #18 and the AERMOD User's
!       Guide.
!
! https://www.epa.gov/scram/air-quality-dispersion-modeling-preferred-and-recommended-models#aermod
!
!-----  MODIFIED BY:    U.S. EPA, OAQPS/AQAD
!                       Air Quality Modeling Group
!
!                       May 21, 2024
!
!-----  MODIFIED FROM:  AERMOD (Version Dated 23132)
!
!=======================================================================
!
!     Variable Declarations
   use main1
   use buoyant_line

   implicit none
   character :: modnam*12

! --- Declare character strings for use in the header records of
!     debug files for NO2 options
   character (len=18) :: NO2DebugString
   character (len=50) :: NO2DebugStrNonEvt, NO2DebugStrEvent

   integer :: idstat, iastat, ibstat
!     JAT D065 7/22/21 HIT_THRESH IS SET BUT NOT USED IN THE MAIN PROGRAM
!      LOGICAL :: L_OPENED, HIT_THRESH
   logical :: l_opened
! UnuseD: INTEGER :: I
!     JAT 12/14/17 variables added for getting
!     filenames from command line argument
   integer :: linp, lout, lpre, lerr, levt,i1
   character(len=300) :: modpre, errfil, evtfil
   integer   :: iargc, status
   character :: c*300 !JAT changed 30 to 300
! Unused: integer :: len, lstat

!     Variable Initializations
   modnam = 'MAIN'
   NO2DebugString    = ''
   NO2DebugStrNonEvt = ''
   NO2DebugStrEvent  = ''

   ntothrs = 0
   nyears  = 0
   ibstat  = 0

   fatal  = .false.
   runerr = .false.
   l_opened = .false.
!     JAT 7/22/21 HIT_THRESH SET BUT NOT USED IN MAIN PROGRAM
!      HIT_THRESH = .FALSE.
   L_NoHeader(:) = .false.

!     CRT 8/6/2021 D100 Populate Error/Warning message arrays
   call errwrnmsg()

!     JAT 12/14/17, added for the command line argument code
!     Set input and output file names based on optional
!     command line arguments and use the base name of
!     the input file as a prefix for other scratch files.
   iargc = command_argument_count()
   if(      iargc == 0 ) then
      inpfil = 'aermod.inp'
      modpre=''
      linp = 10
      outfil = 'aermod.out'
      lout = 10
      lpre=0
   else if( iargc == 1 ) then
      call get_command_argument (1, c, linp, status)
      if (status /= 0) then
         write (*,*) 'get_command_argument failed: status = ',&
         &status, ' iargc = ', 1, ' arg = ', 1
         stop
      end if

!        JAT: parse from right to left and look for first
!        '.', which would be the extension.
      outfil=c
      modpre=c
      lpre=linp
      do i1=linp,1,-1
         if (c(i1:i1) == '.' .and. i1 /= 1) then
            outfil=outfil(1:i1-1)
            modpre=modpre(1:i1-1)
            lpre=i1-1
            exit
         endif
      enddo
      write(outfil,'(2(a))')trim(adjustl(outfil)),'.out'
      ! not good if file extension not .inp
!         if ( (linp .le. 4)
!     &        .or. ( c(linp-3:linp-3) .ne. '.' )
!     &        .or. ( c(linp-2:linp-2) .ne. 'i'
!     &               .and. c(linp-2:linp-2) .ne. 'I' )
!     &        .or. ( c(linp-1:linp-1) .ne. 'n'
!     &               .and. c(linp-1:linp-1) .ne. 'N' )
!     &        .or. ( c(linp-0:linp-0) .ne. 'p'
!    &               .and. c(linp-0:linp-0) .ne. 'P' ) ) then
      call usage
!             write (*,*) ''
!             write (*,*) 'The first argument must be the' //
!     &                   ' input file name ending in .INP'
!             write (*,*) '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^' //
!     &                   '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
!             stop
!         end if
      inpfil = c(1:linp)

!         ! output file name's extension to match case of input file name's extension
!         outfil = c(1:linp-3)
!         if( c(linp-2:linp-2) .eq. 'i' ) outfil(linp-2:linp-2)='o'
!         if( c(linp-2:linp-2) .eq. 'I' ) outfil(linp-2:linp-2)='O'
!         if( c(linp-1:linp-1) .eq. 'n' ) outfil(linp-1:linp-1)='u'
!         if( c(linp-1:linp-1) .eq. 'N' ) outfil(linp-1:linp-1)='U'
!         if( c(linp-0:linp-0) .eq. 'p' ) outfil(linp-0:linp-0)='t'
!         if( c(linp-0:linp-0) .eq. 'P' ) outfil(linp-0:linp-0)='T'
!         lout = linp
   else if( iargc == 2 ) then
      call get_command_argument (1, c, linp, status)
      if (status /= 0) then
         write (*,*) 'get_command_argument failed: status = ',&
         &status, ' iargc = ', 2, ' arg = ', 1
         stop
      end if
      inpfil = c(1:linp)
      modpre=inpfil
      lpre=linp
      do i1=linp,1,-1
         if (c(i1:i1) == '.' .and. i1 /= 1) then
            modpre=modpre(1:i1-1)
            lpre=i1-1
            exit
         endif
      enddo
      call get_command_argument (2, c, lout, status)
      if (status /= 0) then
         write (*,*) 'get_command_argument failed: status = ',&
         &status, ' iargc = ', 2, ' arg = ', 2
         stop
      end if
      outfil = c(1:lout)
   else
      call usage
      stop
   end if

!      write(*,*) inpfil(1:linp)//":"
!      write(*,*) inpfil//":"
!      write(*,*) linp,":"
!      write(*,*) outfil(1:lout)//":"
!      write(*,*) outfil//":"
!      write(*,*) lout,":"
!      lpre = 0
!     Open the Temporary File for Error Messages Generated from the Program
!     JAT 12/14/17 added call to uninam to get filename for file formerly
!     called ERMMSG.TMP.  Removing hard-coded filename allows multiple
!     AERMOD runs in same directory
   call uninam( modpre(1:lpre), 'ERRMSG.TMP', errfil, lerr )
   open(unit=ierunt,file=errfil(1:lerr),status='REPLACE')

!     Initialize TITLE1 and TITLE2 in case of fatal errors during PRESET phase
   title1 = ' '
   title2 = ' '
!     Get Run Date and Time using Fortran 90 functions      ---   CALL DATIME
   call datime (rundat, runtim)

!     Open Input and Output Files                           ---   CALL FILOPN
   call filopn

!     Preprocess Setup Information to Determine Data Storage Needs
   call preset

! --- Check for FATAL error occurring during PRESET processing, and
!     bypass further processing
   if (fatal) goto 12345

   if (nyears == 0) then
! ---    Number of years for MAXDCONT arrays has not be specified,
!        set to default value of 5 years
      nyears = 5
   end if

   if (.not. evonly) then
!       see comments above for temporary error file
      call uninam( modpre(1:lpre), 'EVENT.TMP', evtfil, levt )
      open(unit=itevut,file=evtfil(1:levt),status='REPLACE')
!        Initialize the Event Counter
      ievent = 0
   end if

!     Allocate SETUP Array Storage
   call allsetup

   if (alloc_err) then
!        Error occurred during allocation of Setup arrays.
!        Issue error messages and ABORT model run.
      write(iounit,*) ' '
      write(iounit,*) '  ERROR OCCURRED DURING ALLOCATION OF SETUP ',&
      &'ARRAYS! ABORTING MODEL EXECUTION!'
      write(iounit,10901) nsrc,ngrp,nrec,nsec,nqf,nbf,npdmax,nvmax,&
      &nurb,nolm,npsd,nblgrp,nnet,ixm,iym,nave,&   ! D41_Wood
      &ntyp,nval,nhiann,nmax
10901 format(/'   ARRAY PARAMETER SETTINGS: ',/&
      &'         NSRC   = ', i8,/&
      &'         NGRP   = ', i8,/&
      &'         NREC   = ', i8,/&
      &'         NSEC   = ', i8,/&
      &'         NQF    = ', i8,/&
      &'         NBF    = ', i8,/&
      &'         NPDMAX = ', i8,/&
      &'         NVMAX  = ', i8,/&
      &'         NURB   = ', i8,/&
      &'         NOLM   = ', i8,/&
      &'         NPSD   = ', i8,/&
      &'         NBLGRP = ', i8,/&                             ! D41_Wood
      &'         NNET   = ', i8,/&
      &'         IXM    = ', i8,/&
      &'         IYM    = ', i8,/&
      &'         NAVE   = ', i8,/&
      &'         NTYP   = ', i8,/&
      &'         NVAL   = ', i8,/&
      &'         NHIANN = ', i8,/&
      &'         NMAX   = ', i8)

      write(iounit,*)
      write(iounit,9057) store
9057  format(/'   Estimated Storage Requirements of Model = ',&
      &f9.1,' MB of RAM.'/)

!        Write error message to terminal
      write(*,*) ' '
      write(*,*)'  ERROR OCCURRED DURING ALLOCATION OF SETUP ',&
      &'ARRAYS! ABORTING MODEL EXECUTION!'
      write(*,9057) store

      go to 9999
   end if

!     Variable Initializations                              ---   CALL VARINI
   call varini

!     Process The Model Setup Information                   ---   CALL SETUP
   if (evonly) then
      call ev_setup
   else
      call setup
   end if

! --- Open files for NO2 Debug Options, including PVMRM, OLM,
!     ARM2 and TTRM and write file header information. First
!     define char string explaining order of results for both
!     Non-EVENT and EVENT processing.
   NO2DebugStrNonEvt =&
   &'Hrly non-EVT results sorted by hour/rec/srcgrp/src'
   NO2DebugStrEvent  =&
   &'Hrly & Ave EVT results sorted by day/event/grp/src'

   if (pvmrm) then
      NO2DebugString = 'PVMRM Debug File: '
   else if (olm) then
      NO2DebugString = 'OLM Debug File:   '
   else if (arm2) then
      NO2DebugString = 'ARM2 Debug File:  '
   else if (grsm) then
      NO2DebugString = 'GRSM Debug File: '
   else if (runttrm) then
      NO2DebugString = 'TTRM Debug File:   '
   end if

! --- Text included in second header record of NO2 option debug files:
! Non-Event runs:
! OLM Debug File:   "Hrly non-EVT results sorted by hour/rec/srcgrp/src"
! ARM2 Debug File:  "Hrly non-EVT results sorted by hour/rec/srcgrp/src"
! PVMRM Debug File: "Hrly non-EVT results sorted by hour/rec/srcgrp/src"
! GRSM Debug File: "Hrly non-EVT results sorted by hour/rec/srcgrp/src"
! Event runs:
! OLM Debug File:   "Hrly & Ave EVT results sorted by day/event/grp/src"
! ARM2 Debug File:  "Hrly & Ave EVT results sorted by day/event/grp/src"
! PVMRM Debug File: "Hrly & Ave EVT results sorted by day/event/grp/src"
! GRSM Debug File: "Hrly & Ave EVT results sorted by day/event/grp/src"

! --- Open file with PVMRM debugging output, but first check
!     for potential file unit conflict
   if (pvmrmdbg) then
      l_opened = .false.
      inquire (file=dbpvfil,opened=l_opened)
      if( .not.l_opened )then
         inquire (unit=pvmdbg,opened=l_opened)
         if (.not.l_opened) then
            open(unit=pvmdbg,file=dbpvfil,status='REPLACE')
!             Write the standard header information with AERMOD/AERMET
!             versions, 1st title, and rundat/runtim to the debug output file
            write(pvmdbg,9028) versn, title1(1:68), rundat
9028        format('*** AERMOD - VERSION ',a6,' ***',3x,'*** ',a68,&
            &' ***',8x,a8)
            if( .not.evonly )then
! ---          Write header record with column labels for Non-EVENT results
               write(pvmdbg,9029) c_metver, NO2DebugString,&
               &NO2DebugStrNonEvt, runtim
9029           format('*** AERMET - VERSION ',a6,' ***',3x,'*** ',&
               &a18, a50,' ***',8x,a8/)
!              Write the model options (MODOPS) to the debug output file
               write ( pvmdbg, 200 )&
               &MODOPS_String(1:len_trim(MODOPS_String))
200            format ( ' OPTIONS: ', a /)

               write(pvmdbg,9001)
9001           format(1x,'TYPE    DOM_SRCID     DATE    IREC    GRPID',&
               &'   ISRC    SRCID       NUMCONT     DISTDOM     ',&
               &'MAXCONC_NOx     O3CONC        O3MOLES      ',&
               &'NOxMOLES       BHORIZ        BVERT         ',&
               &'BVERT3       ',&
               &'PLUMEVOL      OrigConc  X  PercentNO2 =    ',&
               &'HRVAL         AVEVAL')
            else
! ---          Write header record with column labels for EVENT results
               write(pvmdbg,9029) c_metver, NO2DebugString,&
               &NO2DebugStrEvent, runtim
!              Write the model options (MODOPS) to the debug output file
               write ( pvmdbg, 200 )&
               &MODOPS_String(1:len_trim(MODOPS_String))
               write(pvmdbg,90011)
90011          format(1x,'TYPE    DOM_SRCID     DATE    IEVE     ',&
               &'EVENTID    IAVE    GRPID   ISRC    SRCID     ',&
               &'NUMCONT   DISTDOM     MAXCONC_NOx     O3CONC   ',&
               &'     O3MOLES      NOxMOLES       BHORIZ        ',&
               &'BVERT         BVERT3       PLUMEVOL      ',&
               &'OrigConc  X  PercentNO2 =    ',&
               &'HRVAL         AVEVAL')
            endif

         else
! ---          Unit is already opened, issue error message
            call errhdl(path,modnam,'E','501','PVMRMDBG')
         end if
      else
! ---       Unit is already opened, issue error message
         call errhdl(path,modnam,'E','501','PVMRMDBG')
      end if
   end if

! --- Add separate debug file for relative dispersion coefficients for PVMRM option;
!     the RELDISP debug file will automatically be named RELDISP.DBG
   if (pvmrmdbg) then
      l_opened = .false.
      inquire (file=rdispfil,opened=l_opened)
      if( .not.l_opened )then
         inquire (unit=rdispunt,opened=l_opened)
         if (.not.l_opened) then
            open(unit=rdispunt,file=rdispfil,status='REPLACE')
!             Write the standard header information with AERMOD/AERMET
!             versions, 1st title, and rundat/runtim to the debug output file
            write(rdispunt,9028) versn, title1(1:68), rundat
            if( .not.evonly )then
! ---          Write header record with column labels for Non-EVENT results

               write(rdispunt,9029) c_metver, NO2DebugString,&
               &NO2DebugStrNonEvt, runtim
!              Write the model options (MODOPS) to the debug output file
               write ( rdispunt, 200 )&
               &MODOPS_String(1:len_trim(MODOPS_String))

! ---          PVMRM and MODEL DEBUG options have been selected; include the
!              RELDISP Calcs in the RELDISP DEBUG file

            else
! ---          Write header record with column labels for EVENT results
               write(rdispunt,9029) c_metver, NO2DebugString,&
               &NO2DebugStrEvent, runtim
!              Write the model options (MODOPS) to the debug output file
               write ( rdispunt, 200 )&
               &MODOPS_String(1:len_trim(MODOPS_String))

! ---          PVMRM and MODEL DEBUG options have been selected; include the
!              RELDISP Calcs in the RELDISP DEBUG file

            end if
         else
! ---          Unit is already opened, issue error message
            call errhdl(path,modnam,'E','501','PVMRMDBG')
         end if
      else
! ---       Unit is already opened, issue error message
         call errhdl(path,modnam,'E','501','PVMRMDBG')
      end if
   end if


! --- Open file with OLM debugging output, but first check
!     for potential file unit conflict
   if (olmdebug) then
      l_opened = .false.
      inquire (file=dbolmfil,opened=l_opened)
      if (.not.l_opened) then
         inquire (unit=olmdbg,opened=l_opened)
         if (.not.l_opened) then
            open(unit=olmdbg,file=dbolmfil,status='REPLACE')
!             Write the standard header information with AERMOD/AERMET
!             versions, 1st title, and rundat/runtim to the debug output file
            write(olmdbg,9028) versn, title1(1:68), rundat
            if( .not.evonly )then
               write(olmdbg,9029) c_metver, NO2DebugString,&
               &NO2DebugStrNonEvt, runtim
!               Write the model options (MODOPS) to the debug output file
               write ( olmdbg, 200 )&
               &MODOPS_String(1:len_trim(MODOPS_String))
! ---           Write header record with column labels for Non-EVENT results
               write(olmdbg,9002)
9002           format(3x,'DATE    IREC    GRPID   ISRC    SRCID',7x,&
               &'IOLM    OLMID     O3CONC',7x,'OLMVAL',3x,&
               &'X   NO2Ratio  =  NO2VAL',7x,'NO_VAL',6x,'OrigConc  ',&
               &'X  PercentNO2 =  HRVAL',8x,'AVEVAL')

            else
               write(olmdbg,9029) c_metver, NO2DebugString,&
               &NO2DebugStrEvent, runtim
! ---            Write header record with column labels for EVENT results
!                Write the model options (MODOPS) to the debug output file
               write ( olmdbg, 200 )&
               &MODOPS_String(1:len_trim(MODOPS_String))
               write(olmdbg,90021)
90021          format(3x,'DATE    IEVE   EVENTID   IAVE    GRPID',&
               &3x,'ISRC',4x,'SRCID       IOLM  OLMID       O3CONC',7x,&
               &'OLMVAL   X   NO2Ratio  =  NO2VAL       NO_VAL',6x,&
               &'OrigConc  X  PercentNO2 =  HRVAL       EV_AVEVAL')

            end if
         else
! ---          Unit is already opened, issue error message
            call errhdl(path,modnam,'E','501','OLMDEBUG')
         end if
      else
! ---       Unit is already opened, issue error message
         call errhdl(path,modnam,'E','501','OLMDEBUG')
      end if
   end if

! --- Open file with ARM2 debugging output, but first check
!     for potential file unit conflict
   if (arm2debug) then
      l_opened = .false.
      inquire (file=dbarm2fil,opened=l_opened)
      if (.not.l_opened) then
         open(unit=arm2dbg,file=dbarm2fil,status='REPLACE')
!            Write the standard header information with AERMOD/AERMET
!            versions, 1st title, and rundat/runtim to the debug output file
         write(arm2dbg,9028) versn, title1(1:68), rundat
         if( .not.evonly )then
            write(arm2dbg,9029) c_metver, NO2DebugString,&
            &NO2DebugStrNonEvt, runtim
!              Write the model options (MODOPS) to the debug output file
            write ( arm2dbg, 200 )&
            &MODOPS_String(1:len_trim(MODOPS_String))
! ---          Write header record with column labels for Non-EVENT results
            write(arm2dbg,9004)
9004        format(3x,'DATE',4x,'IREC    GRPID   ISRC    SRCID',11x,&
            &'NOXCONC',5x,'OrigConc  X  ARM2Ratio =',3x,&
            &'HRVAL',8x,'AVEVAL')
         else
            write(arm2dbg,9029) c_metver, NO2DebugString,&
            &NO2DebugStrEvent, runtim
!              Write the model options (MODOPS) to the debug output file
            write ( arm2dbg, 200 )&
            &MODOPS_String(1:len_trim(MODOPS_String))
! ---          Write header record with column labels for EVENT results
            write(arm2dbg,90041)
90041       format(3x,'DATE',4x,'IEVE   EVENTID   IAVE    GRPID',3x,&
            &'ISRC',4x,'SRCID',11x,'NOXCONC',6x,'OrigConc X  ',&
            &'ARM2Ratio =',4x,'HRVAL',6x,'EV_AVEVAL')
         end if
      else
! ---       Unit is already opened, issue error message
         call errhdl(path,modnam,'E','501','ARM2DEBUG')
      end if
   end if

!     CERC 11/30/20
! --- Open file with GRSM debugging output, but first check
!     for potential file unit conflict
   if (grsmdebug) then
      l_opened = .false.
      inquire (file=dbgrsmfil,opened=l_opened)
      if (.not.l_opened) then
         inquire (unit=grsmdbg,opened=l_opened)
         if (.not.l_opened) then
            open(unit=grsmdbg,file=dbgrsmfil,status='REPLACE')
!             Write the standard header information with AERMOD/AERMET
!             versions, 1st title, and rundat/runtim to the debug output file
            write(grsmdbg,9028) versn, title1(1:68), rundat
            if( .not.evonly )then
               write(grsmdbg,9029) c_metver, NO2DebugString,&
               &NO2DebugStrNonEvt, runtim
!               Write the model options (MODOPS) to the debug output file
               write ( grsmdbg, 200 )&
               &MODOPS_String(1:len_trim(MODOPS_String))
! ---           Write header record with column labels for Non-EVENT results
               write(grsmdbg,90023)
90023          format(1x,'DATE (YYMMDDHH)',2x,'  IREC',2x,'GRPID   ',&
               &2x,'  ISRC',2x,'SRCID       ',2x,'NIGHT-TIME?',2x,&
               &'O3 BGD BEFORE (PPB)',2x,'NOX BGD BEFORE (PPB)',2x,&
               &'NO2 BGD BEFORE (PPB)',2x,'TRAVEL TIME (S)',2x,&
               &'CONC x TRAVEL TIME (MICROG.S/M3)',2x,&
               &'PRIMARY NO2 FRAC',2x,'ORIG NOX CONC (MICROG/M3)',2x,&
               &'NO2 FRAC AFTER',2x,'NO2 BGD AFTER (PPB)',2x,&
               &'HRVAL (MICROG/M3)',2x,'AVEVAL (MICROG/M3)',2x,&
               &'INST. PL. AREA (M2)',2x,'ENSEMBLE PL. AREA (M2)',2x,&
               &'BUILDINGS FACTOR',2x,'MULT PLUMES FACTOR')
            else
               write(grsmdbg,9029) c_metver, NO2DebugString,&
               &NO2DebugStrEvent, runtim
! ---           Write header record with column labels for EVENT results
!               Write the model options (MODOPS) to the debug output file
               write ( grsmdbg, 200 )&
               &MODOPS_String(1:len_trim(MODOPS_String))
               write(grsmdbg,90025)
90025          format(1x,'DATE (YYMMDDHH)',2x,'  IEVE',2x,'EVENTID   ',&
               &2x,'IAVE',2x,'GRPID   ',2x,&
               &'  ISRC',2x,'SRCID       ',2x,'NIGHT-TIME?',2x,&
               &'O3 BGD BEFORE (PPB)',2x,'NOX BGD BEFORE (PPB)',2x,&
               &'NO2 BGD BEFORE (PPB)',2x,'TRAVEL TIME (S)',2x,&
               &'CONC x TRAVEL TIME (MICROG.S/M3)',2x,&
               &'PRIMARY NO2 FRAC',2x,'ORIG NOX CONC (MICROG/M3)',2x,&
               &'NO2 FRAC AFTER',2x,'NO2 BGD AFTER (PPB)',2x,&
               &'HRVAL (MICROG/M3)',2x,'EV_AVEVAL (MICROG/M3)',2x,&
               &'INST. PL. AREA (M2)',2x,'ENSEMBLE PL. AREA (M2)',2x,&
               &'BUILDINGS FACTOR',2x,'MULT PLUMES FACTOR')
            end if
         else
! ---          Unit is already opened, issue error message
            call errhdl(path,modnam,'E','501','GRSMDEBUG')
         end if
      else
! ---       Unit is already opened, issue error message
         call errhdl(path,modnam,'E','501','GRSMDEBUG')
      end if
   end if

! --- Write the model options and debug data template to the
!     debug file if MODEL is specified on the DEBUGOPT keyword
   if( debug )then
!        Write the standard header information with AERMOD/AERMET
!        versions, 1st title, and rundat/runtim to the debug output file
      write(dbgunt,9028) versn, title1(1:68), rundat
      if( evonly )then
! ---       Write header information for EVENT results
         write(dbgunt,90291) c_metver,&
         &'MODEL Debug File: EVENT Processing   ', runtim
      else
! ---       Write header information for Non-EVENT results
         write(dbgunt,90291) c_metver,&
         &'MODEL Debug File: NonEVENT Processing', runtim
      endif
90291 format('*** AERMET - VERSION ',a6,' ***',3x,'*** ',&
      &a37,31x,' ***',8x,a8/)
! ---    Include string of MODEL Options in DEBUG file header
      write (dbgunt, 200)  MODOPS_String(1:len_trim(MODOPS_String))
   end if

! --- Write the model options and debug data template to the
!     debug file if METEOR is specified on the DEBUGOPT keyword
   if( meteordbg )then
      write(dbmunt,9028) versn, title1(1:68), rundat
      if( evonly )then
! ---       Write header information for EVENT results
         write(dbmunt,90292) c_metver,&
         &'METEOR Debug File: EVENT Processing   ', runtim
      else
! ---       Write header information for Non-EVENT results
         write(dbmunt,90292) c_metver,&
         &'METEOR Debug File: NonEVENT Processing', runtim
      endif
90292 format('*** AERMET - VERSION ',a6,' ***',3x,'*** ',&
      &a38,30x,' ***',8x,a8/)
! ---    Include string of MODEL Options in DEBUG file header
      write (dbmunt, 200)  MODOPS_String(1:len_trim(MODOPS_String))
   end if

! --- Write the model options and debug data template to the
!     debug file if PRIME is specified on the DEBUGOPT keyword
!     and model run includes sources with building downwash (NSEC > 0)
   if (primedbg .and. nsec > 0) then
! ---    Write main header records for PRIME debug file
      write(prmdbunt,9028) versn, title1(1:68), rundat
      if( evonly )then
! ---       Write header information for EVENT results
         write(prmdbunt,90293) c_metver,&
         &'PRIME Debug File: EVENT Processing   ', runtim
      else
! ---       Write header information for NonEVENT results
         write(prmdbunt,90293) c_metver,&
         &'PRIME Debug File: NonEVENT Processing', runtim
      endif
90293 format('*** AERMET - VERSION ',a6,' ***',3x,'*** ',&
      &a37,31x,' ***',8x,a8/)
!        Write the model options (MODOPS) to the debug output file
      write ( prmdbunt,200 ) MODOPS_String(1:len_trim(MODOPS_String))
   end if

! --- Write the model options and debug data template to the
!     debug file if AREA/LINE is specified on the DEBUGOPT keyword
!     and model run includes AREA, LINE, or OPENPIT sources
   if( areadbg .and. (narea>0 .or. ncirc>0 .or.&
   &nline>0 .or. npit>0) )then
! ---    Write main header records for METEOR debug file, then reset METDBGHDR = .F.
      write(areadbunt,9028) versn, title1(1:68), rundat
      if( evonly )then
! ---       Write header information for EVENT results
         write(areadbunt,90294) c_metver,&
         &'AREA Debug File: EVENT Processing   ', runtim
      else
! ---       Write header information for NonEVENT results
         write(areadbunt,90294) c_metver,&
         &'AREA Debug File: NonEVENT Processing', runtim
      endif
90294 format('*** AERMET - VERSION ',a6,' ***',3x,'*** ',&
      &a36,32x,' ***',8x,a8/)
!        Write the model options (MODOPS) to the debug output file
      write( areadbunt,200 ) MODOPS_String(1:len_trim(MODOPS_String))
   end if

!     Added for TTRM, AECOM - Feb. 2021; Modified Nov. 2021
   if (ttrmdbg) then
      if( .not.evonly )then
         write ( ttrmunt, 6010 )
6010     format (' KURDAT,XREC,YREC,SRCGRP,SRCID,O3CONC (μg/m3),',&
         &'O3CONC (ppb),Amb. Temp. (K),k1, time coefficent,Distance (m),',&
         &'Effective WSPD (m/s),Transit Time (s),PRIME Eff. WSPD (m/s),',&
         &'PRIME Transit Time (s),Plume Type,Downwind Distance,',&
         &'Radial Distance,Crosswind Distance,UEFF,UEFFD,UEFF3,FRAN,PPF,',&
         &'GAMFACT,CHI-Indirect,CHI-DIRECT,TOTAL NO,Instack-NO2,',&
         &'Available NO (as NO2),TTRM FRACTIONAL,TTRM Converted NO2,',&
         &'Source Sub-total NO2')
      else if(evonly)then
         write ( ttrmunt, 6011 )
6011     format (' KURDAT,XREC,YREC,ievent,EVNAME,EVAPER,',&
         &'SRCGRP,SRCID,O3CONC (μg/m3),',&
         &'O3CONC (ppb),Amb. Temp. (K),k1, time coefficent,Distance (m),',&
         &'Effective WSPD (m/s),Transit Time (s),PRIME Eff. WSPD (m/s),',&
         &'PRIME Transit Time (s),Plume Type,Downwind Distance,',&
         &'Radial Distance,Crosswind Distance,UEFF,UEFFD,UEFF3,FRAN,PPF,',&
         &'GAMFACT,CHI-Indirect,CHI-DIRECT,TOTAL NO,Instack-NO2,',&
         &'Available NO (as NO2),TTRM FRACTIONAL,TTRM Converted NO2,',&
         &'Source Sub-total NO2')
      end if
   end if
!     End of TTRM insert

! Added for HBPDEBUG; Jan. 2023
! added all the header information for the HBP debug file:
   if (hbplume .and. hbpdbg) then
!        Write the title(s) to the debug output file
      write ( hbpunt, 7601 )
      write ( hbpunt, 7100 ) title1(1:68), title2(1:68)
      write ( hbpunt,"(' ')")
      write ( hbpunt, 7701 )
7100  format ( ' Title: ',a68,/,'        ',a68,/)
7601  format (' * * * * * * * AERMOD HBP DEBUG FILE * * * * * * * *')
7701  format (' KURDAT,IREC,SCRID,Current_ZIC,Current_ZIM,',&
      &'NextHr_ZIC,NextHr_ZIM,Avg_ZI,Centerline_HE3,',&
      &'SigmaZ_SZ3,HTOP,HBOT,HTOPDIF,ZRT,PPF,PPFN,HBP_HRVAL')
   endif
! End HBRDEBUG insert
!RCO D095 Added for urban debug 8/3/2021
   if (urbdbug) then
!        Write the title(s) to the debug output file
      write ( urbunt, 611 )
      write ( urbunt, 110 ) title1(1:68), title2(1:68)
      write ( urbunt,"(' ')")
110   format ( ' Title: ',a68,/,'        ',a68,/)
611   format (' * * * * * * * URBAN    DEBUG FILE * * * * * * * ',//)
      write( urbunt, 2222 )
2222  format (1x,'URBAN METEOROLOGY OUTPUT',//,&
      &1x,'IURB',2x, 'YR',2x,'MO',2x,'DY',2x,'HR',&
      &4x,'URBOBLEN',3x,'URBOBLSAV',3x,'RUROBULEN',4x,'URBUSTAR',&
      &2x,'URBUSTRSAV',5x,'RURUSTR',&
      &6x,'DELTUR',7x,'ZIURB',6x,'ZIMECH',6x,'ZICONV',6x,'URBPOP',&
      &7x,'URBHF',5x,'URBWSTR',7x,'WSTAR',&
      &4x,'Amb_Temp',8x,'UREF', 7x,'BOWEN',3x,'STABLE',3x,'MornTrns')

! Profile info for turbulence values
      write ( urbunt1, 611 )
      write ( urbunt1, 110 ) title1(1:68), title2(1:68)
      write ( urbunt1,"(' ')")
      write( urbunt1, 2223 )
2223  format (1x,'URBAN METEOROLOGY OUTPUT',//,&
      &1x,'IURB',2x, 'YR',2x,'MO',2x,'DY',2x,'HR',1x,'LVL',4x,'GRIDHT',&
      &4x,'GRIDSv',4x,'SvCURB',4x,'GRDSvU',&
      &4x,'GRIDSw',4x,'SwCURB',4x,'GRDSwU')

!RCO - D168 Debug files. Add ouput to new urban debug file
! Profile info for temperature values
      write ( urbunt2, 611 )
      write ( urbunt2, 110 ) title1(1:68), title2(1:68)
      write ( urbunt2,"(' ')")
      write( urbunt2, 2224 )
2224  format (1x,'URBAN METEOROLOGY OUTPUT',//,&
      &1x,'IURB',2x, 'YR',2x,'MO',2x,'DY',2x,'HR',1x,'LVL',1x,'NBELOW',&
      &4x,'GRIDHT',4x,'GRIDTG',4x,'GRDTGU',4x,'GRIDPT'4x,'GRDPTU')

   end if

!RCO D095 Added for BLP debug 12/8/21
   if (blpdbug) then
!        D140 Added BLP header to match the formatting for the other debug files Wood 9/29/22
      write(blpunt,9028) versn, title1(1:68), rundat
      if( evonly )then
! ---       Write header information for EVENT results
         write(blpunt,90213) c_metver,&
         &'BLP Debug File: EVENT Processing   ', runtim
      else
! ---       Write header information for NonEVENT results
         write(blpunt,90213) c_metver,&
         &'BLP Debug File: NonEVENT Processing', runtim
      endif
90213 format('*** AERMET - VERSION',a6,'  ***',3x,'*** ',&
      &a36,32x,' ***',8x,a8/)
!        Write the model options (MODOPS) to the debug output file
      write( blpunt,200 ) MODOPS_String(1:len_trim(MODOPS_String))
      write( blpunt,2225 )
2225  format (1x,'PLUME RISE HEIGHTS AND DISTANCES OUTPUT',//,&
      &1x,'SRCID',9x,&
      &'YR',2x,'MO',2x,'DY',2x,'HR',5x,'DH1',4x,'DH2',5x,'DH3',4x,&
      &'DH4',5x,'DH5',5x,'DH6',5x,'DH7',7x,'XF1',3x,'XF2',5x,'XF3',5x,&
      &'XF4',5x,'XF5',5x,'XF6',5x,'XF7',7x,'XFB',5x,'XFS',4x,'STAB',&
      &1x,'URBOBLEN',2x,'OBULEN',5x,'Hs',3x,'FINAL_HT')

   endif

!        D140 removed exisiting BLP header to match the formatting for the other debug files Wood 9/29/22
!CRCO D095 Added for BLP debug 12/8/21
!     IF (BLPDBUG) THEN
!C        Write the title(s) to the debug output file
!         WRITE ( BLPUNT, 612 )
!         WRITE ( BLPUNT, 111 ) TITLE1(1:68), TITLE2(1:68)
!         WRITE ( BLPUNT,"(' ')")
!  111    FORMAT ( ' Title: ',A68,/,'        ',A68,/)
!  612    FORMAT (' * * * * * * * BLP-RISE DEBUG FILE * * * * * * * ',//)
!         WRITE( BLPUNT, 2224 )
! 2224    FORMAT (1X,'PLUME RISE HEIGHTS AND DISTANCES OUTPUT',//,
!     & 2X,'SRCID',8x,
!     & 'YR',2X,'MO',2X,'DY',2X,'HR',5X,'DH1',5X,'DH2',5X,'DH3',5X,
!     & 'DH4',5X,'DH5',5X,'DH6',5X,'DH7',5X,'XF1',5X,'XF2',5X,'XF3',5X,
!     & 'XF4',5X,'XF5',5X,'XF6',5X,'XF7',7X,'XFB',5X,'XFS',1X,'STAB',
!     & 1X,'URBOBLEN',3X,'OBULEN',7X,'Hs',1X,'FINAL_HT')
!      ENDIF

!      D140 Wood 9/29/22 moved the END IF prior to the BLPDBUG if statement to separate the urban and buoyant line debug files
!       END IF

!     Open file for GDEP output from gas dry deposition algorithms,
!     but first check for potential file unit conflict
   if ((debug .or. deposdbg) .and. ldgas) then
      l_opened = .false.
      inquire (file='GDEP.DAT',opened=l_opened)
      if (.not.l_opened) then
         inquire (unit=gdepdbg,opened=l_opened)
         if (.not.l_opened) then
            open(unit=gdepdbg,file='GDEP.DAT',status='REPLACE')
         else
! ---          Unit is already opened, issue error message
            call errhdl(path,modnam,'E','501','GDEP.DAT')
            runerr = .true.
         end if
      else
! ---       Unit is already opened, issue error message
         call errhdl(path,modnam,'E','501','GDEP.DAT')
         runerr = .true.
      end if
   end if

!     Open file for PDEP output from particle dry deposition algorithms,
!     but first check for potential file unit conflict
   if ((debug .or. deposdbg) .and. ldpart) then
      l_opened = .false.
      inquire (file='PDEP.DAT',opened=l_opened)
      if (.not.l_opened) then
         inquire (unit=pdepdbg,opened=l_opened)
         if (.not.l_opened) then
            open(unit=pdepdbg,file='PDEP.DAT',status='REPLACE')
         else
! ---          Unit is already opened, issue error message
            call errhdl(path,modnam,'E','501','PDEP.DAT')
            runerr = .true.
         end if
      else
! ---       Unit is already opened, issue error message
         call errhdl(path,modnam,'E','501','PDEP.DAT')
         runerr = .true.
      end if
   end if

!     Deallocate Temporary Storage
   deallocate  (iwrk2, stat=idstat)
   if (idstat /= 0) then
      call errhdl(path,modnam,'E','409','Setup Arrays')
   end if
   if (.not. evonly) then
      deallocate  (zetmp1,zetmp2,zhtmp1,zhtmp2,zftmp1,zftmp2,&
      &stat=idstat)
      if (idstat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
      end if
   end if

!     Allocate Array Storage for Results                    ---   CALL ALLRESULT
   call allresult

   if (alloc_err) then
!        Error occurred during allocation of Results arrays.
!        Issue error message and skip initialization of results arrays.
      write(iounit,*) ' '
      write(iounit,*) '  ERROR OCCURRED DURING ALLOCATION OF RESULT',&
      &' ARRAYS!'
      write(iounit,10902) nsrc,ngrp,nrec,nsec,nqf,nbf,npdmax,nvmax,&
      &nurb,nolm,npsd,nblgrp,nnet,ixm,iym,nave,&   ! D41_Wood
      &ntyp,nhival,nhiann,nmxval,nyears
10902 format(/'   ARRAY PARAMETER SETTINGS: ',/&
      &'         NSRC   = ', i8,/&
      &'         NGRP   = ', i8,/&
      &'         NREC   = ', i8,/&
      &'         NSEC   = ', i8,/&
      &'         NQF    = ', i8,/&
      &'         NBF    = ', i8,/&
      &'         NPDMAX = ', i8,/&
      &'         NVMAX  = ', i8,/&
      &'         NURB   = ', i8,/&
      &'         NOLM   = ', i8,/&
      &'         NPSD   = ', i8,/&
      &'         NBLGRP = ', i8,/&                             ! D41_Wood
      &'         NNET   = ', i8,/&
      &'         IXM    = ', i8,/&
      &'         IYM    = ', i8,/&
      &'         NAVE   = ', i8,/&
      &'         NTYP   = ', i8,/&
      &'         NHIVAL = ', i8,/&
      &'         NHIANN = ', i8,/&
      &'         NMXVAL = ', i8,/&
      &'         NYEARS = ', i8)

      write(iounit,*)
      write(iounit,9057) store

!        Write error message to terminal
      write(*,*) ' '
      write(*,*) '  ERROR OCCURRED DURING ALLOCATION OF RESULT',&
      &' ARRAYS!'
      write(*,9057) store

      go to 9999

   else if (.not. evonly) then
!        No Errors During Allocation of Results Arrays
!        Initialize Results Arrays With Zeroes              ---   CALL RESINI
      call resini
   end if

!     Determine Number of Setup Messages by Message Type    ---   CALL TERRST
   call terrst

! --- Set up common for PRIME numerical rise algorithm      ---   CALL NUMPR1
   call numpr1

! --- Set up common for PRIME building cavity model         ---   CALL PRIME1
   call prime1

   if (.not.run .or. fatal .or. iwrn > 0) then
!        Write Out Summary Of Setup Error/Message Stats     ---   CALL SUMTBL
      write(iounit,9111)
9111  format(//2x,'*** Message Summary For AERMOD Model Setup ***'/)
      call sumtbl(iounit)
   end if

12345 continue

   if (fatal) then
      write(*,99111)
99111 format('+','Fatal Error Occurred During Setup Phase!')
      write(iounit,9112)
9112  format(/4x,'**************************************',&
      &/4x,'*** SETUP Finishes UN-successfully ***',&
      &/4x,'**************************************'/)
   else
      write(iounit,9113)
9113  format(/1x,'***********************************',&
      &/1x,'*** SETUP Finishes Successfully ***',&
      &/1x,'***********************************'/)

!        Print Summary of the Input Data                       ---   CALL INPSUM
      call inpsum

!        Write Headers to GDEP.DAT and PDEP.DAT Files for new deposition algorithms
      if ((debug .or. deposdbg) .and. ldgas) then
!           Write the model options (MODOPS) to the debug output file
         write ( gdepdbg, 200 )&
         &MODOPS_String(1:len_trim(MODOPS_String))
         write(gdepdbg,9901)
9901     format(1x,'YYMMDDHH',3x,'ISRC',4x,'Ra',12x,'Rb',12x,'Rc',&
         &12x,'Vdepg')
      end if
      if ((debug .or. deposdbg) .and. ldpart) then
         write(pdepdbg,9902)
!           Write the model options (MODOPS) to the debug output file
         write ( pdepdbg, 200 )&
         &MODOPS_String(1:len_trim(MODOPS_String))
9902     format(1x,'YYMMDDHH',3x,'ISRC',1x,'ICAT',2x,'Method No.',&
         &3x,'Ra',12x,'Rp',12x,'Vg(i)',9x,'Vdep(i)')
      end if

   end if

   if (.not.fatal .and. run .and. evonly) then
!        No Fatal Errors in Setup and RUN Option Selected and EVENT Processing

!        Process The Data For Each Event                    ---   CALL EVLOOP
      call evloop

   else if (.not.fatal .and. run .and. .not.evonly) then
!        No Fatal Errors in Setup and RUN Option Selected and Normal Processing

!        Reinitialize Results Arrays With Zeroes            ---   CALL RESINI
      call resini

      if (rstinp) then
!           Initialize Results Arrays from Re-start File    ---   CALL RSINIT
         call rsinit
      end if

!        Process The Hourly Meteorological Data             ---   CALL HRLOOP
      call hrloop

! ---    Check total precipitation if wet deposition is being used
      if ((wdplete .or. depos .or. wdep) .and.&
      &total_precip < 0.0001d0) then
! ---       Write warning message for no precip with wet deposition
         call errhdl(path,modnam,'W','496','WetDepos')
      end if

      if ((pm25ave .or. no2ave .or. so2ave .or. annual)&
      &.and. multyr&
      &.and. .not.runerr) then
! ---       Results arrays for MULTYEAR applications WITH ANNUAL average,
!           or other outputs averaged across years, need to be "dumped" to
!           SAVEFILE BEFORE calculating averages
! ---                                                       ---   CALL RSDUMP
         call rsdump

         if (seasonhr .and. .not.runerr) then
! ---          Calculate averages for season by hour-of-day results
            if (conc) then
               call shave
! ---             Check for values exceeding fixed-format field width (F13.8)
!                 without FILE_FORMAT = 'EXP'
               if (file_format /= 'EXP' .and.&
               &maxval(shvals) > 9999.99999999d0) then
                  call errhdl(path,modnam,'W','400','= EXP')
               end if
            end if
         end if

      end if

      if ( (pm25ave .or. no2ave .or. so2ave .or. annual)&
      &.and. .not.runerr) then
! ---       Compute averages of the High-N-High 24-hr PM25, 1-hr NO2,
!           1-hr SO2, and annual values
         if (numyrs > 0) then
            do igrp = 1, numgrp
               do irec = 1, numrec
                  if (pm25ave .or. no2ave .or. so2ave) then
                     sumhnh(irec,igrp,1:nhival) =&
                     &sumhnh(irec,igrp,1:nhival) / dble(numyrs)
                  end if
                  if (annual) then
                     do ityp = 1, numtyp
                        annval(irec,igrp,ityp) =&
                        &sumann(irec,igrp,ityp) /&
                        &dble(numyrs)
                     end do
                  end if
               end do
            end do
         else
            if( annual )then
!                 Write Error Message: Number of Years = 0 for ANNUAL
               call errhdl(path,modnam,'E','480','NUMYRS=0')
               runerr = .true.
            elseif( no2ave )then
!                 Write Error Message: Need complete years for 1-hr SO2/NO2 and 24-hr PM25
               call errhdl(path,modnam,'E','480','1hr NO2AVE')
               runerr = .true.
            elseif( so2ave )then
!                 Write Error Message: Need complete years for 1-hr SO2/NO2 and 24-hr PM25
               call errhdl(path,modnam,'E','480','1hr SO2AVE')
               runerr = .true.
            elseif( pm25ave )then
!                 Write Error Message: Need complete years for 1-hr SO2/NO2 and 24-hr PM25
               call errhdl(path,modnam,'E','480','24hr PM25AVE')
               runerr = .true.
            end if
         end if
         if (nremain /= 0) then
!              Write Warning Message: Met Data Remains After End of Last Year
            if (.not. L_SkipMessages) then
               write(dummy,'(I8)') nremain
               call errhdl(path,modnam,'W','481',dummy)
            end if
         end if
      end if

      if ((period.or.annual) .and. (.not. runerr) .and.&
      &ntothrs>0) then
! ---       PERIOD Average Selected and No Runtime/Meteorology Errors
         if (conc .and. period) then
!              Calculate Period Average Concentrations      ---   CALL PERAVE
            call perave
         end if
! ---       Check for values exceeding fixed-format field width (F13.5)
!           without FILE_FORMAT = 'EXP'
         if (file_format /= 'EXP' .and.&
         &maxval(annval) > 9999999.99999d0) then
            call errhdl(path,modnam,'W','400','= EXP')
         end if
         do ityp = 1, numtyp
!              Select Highest PERIOD Values by Source Group ---   CALL HIPER
            call hiper
         end do
!          JAT 9/21/2017: MODIFIED CALL TO PSTANN TO ONLY WHEN PERIOD AVERAGES
!          TO AVOID WRITING ANNUAL AVERAGE ACROSS MODELED PERIOD TO ANNUAL
!          POSTFILE WHEN ONLY INDIVIDUAL YEARS SHOULD BE WRITTEN TO ANNUAL POSTFILE
         if (anpost .and. period) then
!              Write PERIOD/ANNUAL Results to Post File     ---   CALL PSTANN
            call pstann
         end if
         if (anplot) then
!              Write PERIOD/ANNUAL Results to Plot File     ---   CALL PLTANN
            call pltann
         end if
      end if

      if (multyr .and. .not.runerr .and.&
      &.not.(annual .or. pm25ave .or. no2ave .or.&
      &so2ave)) then
! ---       Results arrays for MULTYEAR applications WITHOUT ANNUAL average,
!           or other outputs averaged across years, need to be "dumped" to
!           SAVEFILE AFTER calculating averages
! ---                                                       ---   CALL RSDUMP
         call rsdump
      end if

      if (.not.(multyr .and. (annual .or. pm25ave .or.&
      &no2ave .or.&
      &so2ave) ) .and.&
      &seasonhr .and. .not.runerr) then
         if (conc) then
            call shave
! ---          Check for values exceeding fixed-format field width (F13.8)
!              without FILE_FORMAT = 'EXP'
            if (file_format /= 'EXP' .and.&
            &maxval(shvals) > 9999.99999999d0) then
               call errhdl(path,modnam,'W','400','= EXP')
            end if
         end if
      end if

      if (plfile .and. (.not. runerr)) then
!           Write Short Term High Values to Plot File       ---   CALL PLOTFL
! ---       Check for values exceeding fixed-format field width (F13.5)
!           without FILE_FORMAT = 'EXP'
         if (file_format /= 'EXP') then
            if (pm25ave .or. no2ave .or. so2ave) then
               if (maxval(sumhnh) > 9999999.99999d0) then
                  call errhdl(path,modnam,'W','400','= EXP')
               end if
            else if (.not.pm25ave .and. .not.no2ave .and. .not.so2ave&
            &.and.maxval(hivalu)>9999999.99999d0) then
               call errhdl(path,modnam,'W','400','= EXP')
            end if
         end if
! ---       Call plotfile routine
         call plotfl
      end if

      if (.not. runerr) then
! ---       Check for values exceeding fixed-format field width (F13.5)
!           without FILE_FORMAT = 'EXP'
         if (.not.plfile .and. file_format /= 'EXP') then
            if (pm25ave .or. no2ave .or. so2ave) then
               if (maxval(sumhnh) > 9999999.99999d0) then
                  call errhdl(path,modnam,'W','400','= EXP')
               end if
            else if (.not.pm25ave .and. .not.no2ave .and. .not.so2ave&
            &.and.maxval(hivalu)>9999999.99999d0) then
               call errhdl(path,modnam,'W','400','= EXP')
            end if
         end if
!           Print Out Model Results                         ---   CALL OUTPUT
         call output
      end if

! ---    Check for MAXDCONT options to evaluate source group contributions
!        based on rank for PM2.5 24hr, NO2 1hr or SO2 1hr NAAQS
      if (.not. runerr .and.&
      &l_maxdcont .and.&
      &(pm25ave .or. no2ave .or. so2ave) ) then

         if (pvmrmdbg) then
! ---          PVMRM Debug option selected; print header record to delimit
!              debug information related to MAXDCONT processing
            write(pvmdbg,9001)
         end if

! ---       Allocate arrays to save receptor data;
!           also allocate array to store summed
!           contributions for max daily 1-hour averages
         allocate  (axr_sav(nrec), ayr_sav(nrec),&
         &azelev_sav(nrec), azflag_sav(nrec),&
         &azhill_sav(nrec),&
         &sumval_maxd(nval,ngrp,ngrp,nrec),&
         &stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation ',&
            &'Rec Arrays for MAXDCONT!'
         end if

! (Multiple_BuoyLines_D41_Wood)
!           Added second dimension to arrays for multiple buoyant lines
         if (l_blsource) then
            allocate (xr_scs_sav(nrec,nblgrp),&
            &yr_scs_sav(nrec,nblgrp),&
            &bl_rflag_sav(nrec,nblgrp), stat=ibstat)
         endif
         if (ibstat /= 0) then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation ',&
            &'BuoyLine Rec Arrays for MAXDCONT!'
         end if

         if( iastat /= 0 .or. ibstat /= 0) then
            go to 9999
         end if

         call maxdcont_loop

      end if

   else
! ---    FATAL error occurred during PRESETUP phase; initialize MODOPS string
!        before call to HEADER to avoid runtime error for undefined string
      MODOPS_String = ''

   end if

!     Determine Number of Errors/Messages by Message Type   ---   CALL TERRST
   call terrst

!     Write Summary of Message Stats for Model Execution    ---   CALL SUMTBL
   call header(iounit)
   write(iounit,9114)
9114 format(/1x,'*** Message Summary : AERMOD Model Execution ***'/)

   call sumtbl(iounit)

   if (summfile) then
!        Write Summary of Messages to optional SUMMFILE
      call header(isumunt)
      write(isumunt,9114)
      call sumtbl(isumunt)
   end if

!     Skip to here if error occurs during allocation of arrays
9999 continue

   if (fatal .or. runerr) then
      if (runerr) then
         write(*,99112)
99112    format('+','Fatal Error Occurred During Runtime Phase!')
      end if
      write(iounit,9115)
9115  format(/4x,'***************************************',&
      &/4x,'*** AERMOD Finishes UN-successfully ***',&
      &/4x,'***************************************'/)
   else
! ---    AERMOD finished without any "fatal" errors, but issue
!        warning to IOUNIT if MAXDCONT or EVENT processing
!        inconsistencies occurred, before message that AERMOD
!        finished successfully
      if (L_MAXDCONT_OrigConc_Warning) then
! ---       MAXDCONT processing inconsistency warning
         write(*,91161)
         write(iounit,91161)
91161    format(/4x,'NOTE: MAXDCONT Inconsistency Warning Issued!'/)
      else if (L_EVENT_OrigConc_Warning) then
! ---       EVENT processing inconsistency warning
         write(*,91162)
         write(iounit,91162)
91162    format(/4x,'NOTE: EVENT Inconsistency Warning(s) Issued!')
      end if
      write(iounit,9116)
9116  format(/4x,'************************************',&
      &/4x,'*** AERMOD Finishes Successfully ***',&
      &/4x,'************************************'/)
   end if

   if (errlst) then
!        OPEN and Write Out Permanent Error Message File    ---   CALL MSGWRT
      open(unit=ierwrt,file=msgfil,status='REPLACE',&
      &form='FORMATTED')
      call msgwrt
      close(ierwrt)
   end if

!     Close and Delete The Error Message And EVENT Temporary Files
   close(ierunt,status='DELETE')
   close(itevut,status='DELETE')

end program aermod

! JAT 12/14/17 subroutine usage added to write out command line argument options
subroutine usage
!***********************************************************************
!     Variable Declarations
   use main1
   implicit none
   write(*,*) "usage: 0, 1, or 2 args"
   write(*,*) ""
   write(*,*) "Usage: AERMOD "//versn//" takes either no or one"&
   &// " or two parameters."
   write(*,*) "       Either"
   write(*,*) "             AERMOD"
   write(*,*) "       Or"
   write(*,*) "             AERMOD plumetest.inp"
   write(*,*) "       Or"
   write(*,*) "             AERMOD plumetest.inp plumetest.out"
   write(*,*) ""
   write(*,*) "       The first parameter  is the .INP file name,"
!     &          // " with the .INP included."
   write(*,*) "       The second parameter is the .OUT file name,"
!     &          // " with the .OUT included."
   return
end subroutine usage


subroutine hrloop
!***********************************************************************
!                 HRLOOP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls Main Calculation Loop Through
!                 Hourly Meteorological Data
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Streamlined for Aircraft sources, by adding nested
!                    IF-statement instead of complicated IF-statements.
!                    M.G. Snyder, WSP 6/5/2023
!
!        MODIFIED:   Modified to handle the Aircraft's Engine parameters
!                    for VOLUME/AREA Source only for Aircraft Source Group.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   Incorporated non-DFAULT GRSM option for NO2
!                    CERC, 11/30/20
!
!        MODIFIED:   Incorporated Travel Time Reaction Method (TTRM)
!                    option for NO2 Carlos Szembek, AECOM
!                    Feb. 2021
!
!        MODIFIED:   Incorporated non-DFAULT/BETA ARM2 option for NO2
!                    Mark Podrez, RTP Environmental Associates, Inc.
!                    R. Brode, US EPA, OAQPS, AQMG, December 16, 2013
!
!        MODIFIED:  To include error handling for data mismatches between
!                   the hourly emissions and meteorological data files.
!                   Included error hanlding for end-of-file (EOF) for the
!                   meteorological data files occurring before the user-
!                   specified end-date (STARTEND keyword).  Also removed
!                   code related to "wet scimming" option, which is not
!                   supported in AERMOD.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:  To include the PVMRM and OLM options for
!                   modeling conversion of NOx to NO2.
!                   R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
!
!        MODIFIED:  To incorporate modifications to date processing
!                   for Y2K compliance, including use of date window
!                   variables (ISTRT_WIND and ISTRT_CENT) and calculation
!                   of 10-digit date variable (FULLDATE) with 4-digit
!                   year for date comparisons.
!                   Also modified to include SCIM option.
!                   R.W. Brode, PES, Inc., 5/12/99
!
!        MODIFIED:  To correct problems with the post-1997 PM10
!                   calculations involving leap years, and to
!                   add the year to the status message.
!                   R.W. Brode, PES, Inc. - 12/2/98
!
!        MODIFIED:  Changes to accommodate the post-1997 PM10
!                   calculations for average H4H 24-hour averages
!                   and ANNUAL averages.
!                   R.W. Brode, PES, Inc. - 8/14/98
!
!        MODIFIED:  Minor change to logic of IF block to correct
!                   potential problem with STARTEND keyword for
!                   non-sequential meteorological data sets.
!                   R.W. Brode, PES, Inc. - 4/22/96
!
!        MODIFIED:  To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  Source, Receptor and Setup Options
!
!        OUTPUTS: Update Hourly Results
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   character :: BGReadErr*5, BGEndErr*5
   character :: O3ReadErr*5, O3EndErr*5
   character :: NOxReadErr*5, NOxEndErr*5

   integer :: i, ilsave

   double precision :: rdum

!     Logical variable to identify READ errors in hourly BACKGROUND files
   logical :: L_BGReadErr, L_O3ReadErr, L_NOxReadErr

!     Variable Initializations
   modnam = 'HRLOOP'
   eof = .false.
   L_BGReadErr = .false.
   L_O3ReadErr = .false.
   L_NOxReadErr = .false.
   if (allocated(L_MorningTrans)) L_MorningTrans(:) = .false.
   kurdat   = 0
   kurpfl   = 0
   fulldate = 0

!     Begin Hourly LOOP
   hour_loop: do while (fulldate<=iedate .and. .not.eof)
!        Retrieve One Hour of Meteorology                   ---   CALL METEXT
!        Call to METEXT also determines the sector IDs for
!        BACKGRND and/or OZONE data and/or NOx data, IBGSECT, IO3SECT and INOXSECT
      call metext

! ---    Check for runtime error generated in call to METEXT;
!        Exit HOUR_LOOP if runtime error found
      if (runerr) then
         exit hour_loop
      end if

      if (fulldate>=isdate .and. fulldate<=iedate .and.&
      &( (.not.L_LeapYear.and.iproc (jday)==1) .or.&
      &(L_LeapYear.and.iprocl(jday)==1) ) .and.&
      &.not.eof) then
!           Increment counter for total number of hours processed
         if (.not.L_SkipMessages) ntothrs = ntothrs + 1
      else if (fulldate<iedate .and. iedate<2147123124 .and.&
      &eof) then
! ---       End of met data file(s) reached before user-specified End Date
!           Issue fatal error message
         call errhdl(path,modnam,'E','580','MET-DATA')
!           Exit hourly loop
         exit hour_loop
      else if (eof .or. fulldate > iedate) then
! ---       End of File or data period has been reached; EXIT hour loop
         exit hour_loop
      end if

!        Save ILINE as ILSAVE and Initialize ILINE
      ilsave = iline

      if (hourly) then
!           Process Hourly Emissions from File
!           Begin Source Loop
         do isrc = 1, numsrc
            if (qflag(isrc) == 'HOURLY') then
!*                Increment IQLINE counter to reflect line number of HOUREMIS file
               iqline = iqline + 1
!MKP              Check for aircraft source type for reading/setting
!                 aircraft plume rise parameters.
               if((aftsrc(isrc) == 'Y')) then
!*                  Retrieve AIRCRAFT Source Parameters for This Hour     ---   CALL AHRQREAD
                  call ahrqread(isrc)
               else
!*                  Retrieve Source Parameters for This Hour     ---   CALL HRQREAD
                  call hrqread(isrc)
               end if
!*                Check for Date and Time Consistency with Met Data;
!*                If Failed, Issue Fatal Error
               if (eof) then
!*                   Write Error Message - EOF reached in hourly emission file
                  call errhdl(path,modnam,'E','580','HOUREMIS')
                  runerr = .true.
               else if (fulldate /= fullhrq) then
!*                   WRITE Error Message - Date mismatch
                  write(dummy,'(I10.10)') fulldate
                  call errhdl(path,modnam,'E','455',dummy)
                  runerr = .true.
               end if
!*                Extract source parameters to standard arrays, if not RUNERR
               if (.not. runerr) then
                  call hrqext(isrc)
               end if

               if (.not.rstinp .and. l_maxdcont .and.&
               &fulldate>=isdate) then
! ---                Save hourly emissions for MAXDCONT option
                  aaqs(ihr_ndx,iyr_ndx,isrc) = aqs(isrc)

                  if (srctyp(isrc)(1:5) == 'POINT') then
                     aats(ihr_ndx,iyr_ndx,isrc) = ats(isrc)
                     aavs(ihr_ndx,iyr_ndx,isrc) = avs(isrc)
                  else if (srctyp(isrc) == 'VOLUME' .and.&
                  &l_hrlysig(isrc)) then
                     aahs(ihr_ndx,iyr_ndx,isrc)    = ahs(isrc)
                     aasyini(ihr_ndx,iyr_ndx,isrc) = asyini(isrc)
                     aaszini(ihr_ndx,iyr_ndx,isrc) = aszini(isrc)
                  else if (srctyp(isrc)(1:4) == 'AREA' .and.&
                  &l_hrlysig(isrc)) then
                     aahs(ihr_ndx,iyr_ndx,isrc)    = ahs(isrc)
                     aaszini(ihr_ndx,iyr_ndx,isrc) = aszini(isrc)
                  else if (srctyp(isrc) == 'LINE' .and.&
                  &l_hrlysig(isrc)) then
                     aahs(ihr_ndx,iyr_ndx,isrc)    = ahs(isrc)
                     aaszini(ihr_ndx,iyr_ndx,isrc) = aszini(isrc)
                  else if (srctyp(isrc) == 'BUOYLINE') then
                     aafp(ihr_ndx,iyr_ndx,isrc) = afp(isrc)
                  end if

!**  Added for Aircraft Plume Rise; UNC-IE !D151 - MGS 6/5/23
                  if (aftsrc(isrc) == 'Y') then
                     aamfuel(ihr_ndx,iyr_ndx,isrc) = amfuel(isrc)
                     aathrust(ihr_ndx,iyr_ndx,isrc) = athrust(isrc)
                     aavaa(ihr_ndx,iyr_ndx,isrc) = avaa(isrc)
                     aaafr(ihr_ndx,iyr_ndx,isrc) = aafr(isrc)
                     aabypr(ihr_ndx,iyr_ndx,isrc) = abypr(isrc)
                     aasrcangle(ihr_ndx,iyr_ndx,isrc)=asrcangle(isrc)
                     aarpwr(ihr_ndx,iyr_ndx,isrc) = arpwr(isrc)
                  end if
!**  End Aircraft Plume Rise insert; April 2023 !D151 - MGS 6/5/23

               end if

            end if
         end do
!*          End Source Loop
      end if

! ---    Check for runtime error generated in call to HRQREAD;
!        Exit HOUR_LOOP if runtime error found
      if (runerr) exit hour_loop

!*----
!        Save ILINE as ILSAVE and Initialize ILINE
      ilsave = iline

      if (l_backgrnd) then
! ---       Process Background Concentration inputs, starting with Hourly BG

         if (L_BGHourly) then
!*             Increment IBLINE counter to reflect line number of hourly BACKGRND file
            ibline = ibline + 1

!*             Retrieve hourly background concentrations      ---   CALL BGEXT
            call bgext(L_BGReadErr,BGReadErr,BGEndErr)

!*             Check for issues reading hourly BG file
            if (eof) then
!*                Write Error Message - EOF reached in hourly background file
               write(dummy,'(''BGFILE '',A5)') BGEndErr
               call errhdl(path,modnam,'E','580',dummy)
               runerr = .true.
            else if (L_BGReadErr) then
!*                Write Error Message - READ error in hourly background file
               write(dummy,'(''BGFILE '',A5)') BGReadErr
               call errhdl(path,modnam,'E','510',dummy)
               runerr = .true.
            end if

         else

! ---          Check for temporally-varying background to substitute for missing hours
            if (ibgsect > 0) then
!                 Valid IBGSECT value
               if (L_BGValues(ibgsect)) then
                  call bgval(ibgsect,bgconc)
               else
                  bgconc = 0.0d0
               end if
            else
! ---             IBGSECT is missing (calm or missing hour); set BGCONC = 0.0
!                 Note: this should result in a fatal error (452)
               bgconc = 0.0d0
            end if
         end if

      end if

! ---    Check for runtime error generated in call to BGEXT;
!        Exit HOUR_LOOP if runtime error found
      if (runerr) exit hour_loop

      if (l_backgrnd .and. .not.rstinp .and. l_maxdcont .and.&
      &fulldate>=isdate) then
! ---       Save hourly background concentration for MAXDCONT option
         abgconc(ihr_ndx,iyr_ndx) = bgconc
      end if

!*----
!        Retrive ILINE From ILSAVE
      iline = ilsave

!! Added for TTRM2
      if (runttrm2) then
         ttrmcompare(:,:,:,:) = 0.0
         cmeth = 1
      endif
!! End of TTRM2 insert; Nov. 2021

      if (pvmrm .or. olm .or. runttrm .or. grsm) then
!-----      Read Ozone Data File if available
         if (L_O3Hourly) then
!*             Increment IOLINE counter to reflect line number of Hourly O3 file
            ioline = ioline + 1

! ---          Extract O3 value from hourly data file; O3EXT also reads a record
!              for O3FILEs available for other sectors to keep files synchronized,
!              so premature EOF for one file should result in date mismatch error
            if (.not. eof) then
               call o3ext(L_O3ReadErr,O3ReadErr,O3EndErr)
            end if

            if (eof) then
!*                Write Error Message - EOF reached in hourly O3 file
               write(dummy,'(''O3FILE '',A5)') O3EndErr
               call errhdl(path,modnam,'E','580',dummy)
               runerr = .true.
            else if (L_O3ReadErr) then
!*                Write Error Message - EOF reached in hourly O3 file
               write(dummy,'(''O3FILE '',A5)') O3ReadErr
               call errhdl(path,modnam,'E','510',dummy)
               runerr = .true.
            end if

         else if (io3sect > 0) then
            if (l_o3values(io3sect)) then
! ---             Use ozone concentration from O3VALUES keyword
               call ozonvals(io3sect,o3conc)
            else if (l_o3val(io3sect)) then
! ---             Use single "background" O3 value from OZONEVAL keyword
               o3conc = o3back(io3sect)
            else
! ---             Set O3CONC to 0.0 for full conversion (subject to
!                 equilibrium ratio)
               o3conc = 0.0d0
            end if
         else
! ----         IO3SECT is 0 due to calm/missing hour; set O3CONC to 0.0
            o3conc = 0.0d0
         end if

         if (.not.rstinp .and. l_maxdcont .and.&
         &fulldate>=isdate) then
! ---          Save hourly ozone concentration for MAXDCONT option
!JAT  06/10/2020  ISSUE D47 ADDED FROM 19191
!             IF O3MISS IS TRUE, SET AO3CONC TO -9
!             OTHERWISE SET TO O3CONC
!             SETTING AO3CONC TO -9 MAKES MAXDCONT CONSISTENT WITH
!             BASE AERMOD RUN.
            if (o3miss) then
               ao3conc(ihr_ndx,iyr_ndx)=-9.0d0
            else
               ao3conc(ihr_ndx,iyr_ndx) = o3conc
            endif

!               AO3CONC(IHR_NDX,IYR_NDX) = O3CONC
         end if
      end if

!        CERC 11/30/20
      if (grsm) then
!-----      Read NOx Data File if available
         if (L_NOxHourly) then
!*             Increment INOXLINE counter to reflect line number of Hourly NOX file
            inoxline = inoxline + 1

! ---          Extract NOx value from hourly data file; NOXEXT also reads a record
!              for NOXFILEs available for other sectors to keep files synchronized,
!              so premature EOF for one file should result in date mismatch error
            if (.not. eof) then
               call noxext(L_NOXReadErr,NOxReadErr,NOxEndErr)
            end if

            if (eof) then
!*                Write Error Message - EOF reached in hourly NOx file
               write(dummy,'(''NOXFIL '',A5)') NOxEndErr
               call errhdl(path,modnam,'E','580',dummy)
               runerr = .true.
            else if (L_NOxReadErr) then
!*                Write Error Message - Error reading in hourly NOx file
               write(dummy,'(''NOXFIL '',A5)') NOxReadErr
               call errhdl(path,modnam,'E','510',dummy)
               runerr = .true.
            end if
         elseif (inoxsect > 0) then
            if (l_nox_vals(inoxsect)) then
! ---             Use NOX concentration from NOX_VALS keyword
               call varynoxvals(inoxsect,noxbgconc)
            else if (l_noxvalue(inoxsect)) then
! ---             Use single "background" NOX value from NOXVALUE keyword
               noxbgconc = noxback(inoxsect)
            else
! ---             Set NOXBGCONC to 0.0
               noxbgconc = 0.0d0
            end if
         else
! ----         INOXSECT is 0 due to calm/missing hour; set NOXBGCONC to 0.0
            noxbgconc = 0.0d0
         end if

         if (.not.rstinp .and. l_maxdcont .and.&
         &fulldate>=isdate) then
! ---          Save hourly NOX concentration for MAXDCONT option
            anoxbgconc(ihr_ndx,iyr_ndx) = noxbgconc
         end if
      end if

! ---    Check for runtime error generated in call to O3EXT;
!        Exit HOUR_LOOP if runtime error found
      if (runerr) exit hour_loop

!*----
!        Retrive ILINE From ILSAVE
      iline = ilsave

!*       Check for IHOUR = 1 and Write Update to the Screen For PC Version
      if ((ihour==1 .or. iline==1) .and. .not.nochkd) then
!*          Write Out Update to the Screen by Julian Day
         write(*,909) jday, iyr
909      format('+','Now Processing Data For Day No. ',i4,' of ',i4)
      else if (nochkd) then
!*          Write Out Update to the Screen by Hour
         write(*,910) kurdat
910      format('+','Now Processing Data For     ',i8.8)
      end if
!*----
!*#

      if (scim .and. .not.eof) then
         scimhr = .false.

!           User has specified SCIM option.  Check for whether current
!           hour is to be sampled, and whether to write sampled met
!           data to output file.

!           Keep track of total no. of hours.
!           Also, keep track of dry & wet, and calm & missing hours
!           Note:  Under SCIM option, IANHRS/IANCLM/IANMSG (see below) pertain
!                  to no. of hours sampled.
         nskiptot = nskiptot + 1

         if( iline <= 24 .and. ihour == nregstart )then
!              Current hour is to be sampled - first SCIM'd hour.
            ifirsthr = iline
            scimhr   = .true.
         else if( iline > nregstart .and.&
         &mod( iline-ifirsthr, nregint ) == 0 )then
!              Current hour is to be sampled - SCIM'd hour
            scimhr   = .true.
         else
!              Current hour is NOT to be sampled. Check for end of year first.
            call chk_endyr
            cycle hour_loop
         end if

         if (scimout) then
!              Write sampled meteorology to SCIM'd met data file
            call metsum
         end if
      end if

      if (fulldate>=isdate .and. fulldate<=iedate .and.&
      &( (.not.L_LeapYear.and.iproc (jday)==1) .or.&
      &(L_LeapYear.and.iprocl(jday)==1) ) .and.&
      &.not.eof .and. .not.runerr) then

! ---       Check for calm winds or missing met data, for which model
!           calculations cannot be made; increment counters for number
!           of hours, but do not include background concentrations, if
!           specified through the BACKGRND keyword.
         if (clmhr .and. clmpro) then
!              Check for Calm Hr & Processing and Increment Counters
            do iave = 1, numave
               numhrs(iave) = numhrs(iave) + 1
               numclm(iave) = numclm(iave) + 1
            end do
            if (period .or. annual) then
               if (.not.scim .or. (scim.and.scimhr)) then
                  ianhrs = ianhrs + 1
                  ianclm = ianclm + 1
               end if
            end if
            if (seasonhr) then
               nseahr(iseas,ihour) = nseahr(iseas,ihour) + 1
               nseacm(iseas,ihour) = nseacm(iseas,ihour) + 1
            end if
         else if (msghr .and. msgpro) then
!              Check for Missing Hour & Processing and Increment Counters
            do iave = 1, numave
               numhrs(iave) = numhrs(iave) + 1
               nummsg(iave) = nummsg(iave) + 1
            end do
            if (period .or. annual) then
               if (.not.scim .or. (scim.and.scimhr)) then
                  ianhrs = ianhrs + 1
                  ianmsg = ianmsg + 1
               end if
            end if
            if (seasonhr) then
               nseahr(iseas,ihour) = nseahr(iseas,ihour) + 1
               nseacm(iseas,ihour) = nseacm(iseas,ihour) + 1
            end if
         else if (zi <= 0.0d0) then
!              Write Out The Informational Message & Increment Counters
            if (.not. L_SkipMessages) then
               write(dummy,'(I8.8)') kurdat
               call errhdl(path,modnam,'I','470',dummy)
            end if
            do iave = 1, numave
               numhrs(iave) = numhrs(iave) + 1
            end do
            if (period .or. annual) then
               if (.not.scim .or. (scim.and.scimhr)) then
                  ianhrs = ianhrs + 1
               end if
            end if
            if (seasonhr) then
               nseahr(iseas,ihour) = nseahr(iseas,ihour) + 1
            end if
         else
!              Set CALCS Flag, Increment Counters & Calculate HRVAL
            calcs = .true.
            do iave = 1, numave
               numhrs(iave) = numhrs(iave) + 1
            end do
            if (period .or. annual) then
               if (.not.scim .or. (scim.and.scimhr)) then
                  ianhrs = ianhrs + 1
               end if
            end if
            if (seasonhr) then
               nseahr(iseas,ihour) = nseahr(iseas,ihour) + 1
            end if

!              Time/Date Marker for DEBUG Output
            if (debug) then
               write(dbgunt,*)
               write(dbgunt,*) '--------------------------------',&
               &'--------------------'
               write(dbgunt,*) '---  JDAY, IHOUR =  ',jday,ihour
               write(dbgunt,*) '--------------------------------',&
               &'--------------------'
            end if

! ---          Calculate CONC or DEPOS Values               ---   CALL CALC
            call calc
         end if

         if (.not.clmhr .and. .not.msghr) then
! ---          Non-calm, non-missing hour; apply NO2 options as appropriate
!! Added for TTRM2
!! If TTRM2 (TTRM with the compare option) is requested then
!! perform TTRM >> FIRST <<
            if (runttrm2) then
!!                Check if the METHOD flag is set to 1 for HRLOOP;
!!                if not, cycle through the other NO2 options
               if (cmeth == 1) then
! ---             Process Hourly Values for TTRM Option
                  call ttrm_calc
               endif
               cmeth = 2
!                 Flush HRVAL Arrays (1:NUMTYP)
               hrval(:)   = 0.0d0
            endif
            if (pvmrm .and. .not.psdcredit) then
! ---             Process Hourly Values for PVMRM Option
               call pvmrm_calc('ALLSRCS')

            else if (pvmrm .and. psdcredit) then
! ---             Process Hourly Values for PVMRM Option and PSD credits
! ---             Need to process two separate sets of sources - the
!                 increment consumption sources ('NAAQSRC') and the
!                 increment expanding sources ('ALLBASE')
               call pvmrm_calc('NAAQSRC')
               call pvmrm_calc('ALLBASE')

            else if (olm) then
! ---             Process Hourly Values for OLM Option
               call olm_calc

            else if (arm2) then
! ---             Process Hourly Values for ARM2 Option
               call arm2_calc
            else if (grsm) then
! ---             CERC 11/30/20 Process Hourly Values for GRSM Option
!! TTRM2 has not designed to be used with GRSM
               if(.not. runttrm2) then
                  call grsm_calc
               endif

!! End of TTRM2 insert; Nov. 2021
            else if (runttrm) then
               if (.not. runttrm2) then
! ---             Process Hourly Values for TTRM Option
                  call ttrm_calc
               endif
            end if
         end if

!           Begin Averaging Period LOOP
         do iave = 1, numave
!              Check for End of Averaging Period
            if (mod(ihour,kave(iave))==0 .or.&
            &(kave(iave)==720 .and. endmon)) then
               if (conc) then
!                    Calculate Applicable Averages          ---   CALL AVER
                  call aver
               end if
!                 Update High Value Arrays                  ---   CALL HIVALS
               call hivals

               if( (no2ave .or. so2ave) .and. kave(iave)==1 )then
! ---                Loop through SRCGRPs again to get max daily 1hr cumulative value
                  do igrp = 1, numgrp
                     do irec = 1, numrec
                        if (aveval(irec,igrp,iave,1) >&
                        &mxdval(irec,igrp)) then
                           mxdval(irec,igrp) = aveval(irec,igrp,iave,1)
                           imxdhr(irec,igrp) = ihour
                        end if
                     end do
                  end do
               end if

               if( pm25ave .and. mod(ihour,24)==0 .and.&
               &kave(iave)==24 )then
! ---                Loop through source groups again to get max daily 24-hr cumulative value
                  do igrp = 1, numgrp
                     do irec = 1, numrec
                        if (aveval(irec,igrp,iave,1) >&
                        &mxdval(irec,igrp)) then
                           mxdval(irec,igrp) = aveval(irec,igrp,iave,1)
                           imxdhr(irec,igrp) = ihour
                        end if
                     end do
                  end do
               end if

               if (daytab .and. idytab(iave)==1) then
                  do ityp = 1, numtyp
!                       Print Out Daily Value Tables        ---   CALL PRTDAY
                     call prtday
                  end do
               end if
               if (mxfile) then
!                    Write Max Values (>Thresh) to File     ---   CALL MAXFIL
                  call maxfil
               end if
               if (ppfile) then
!                    Write Values to Postprocessor File     ---   CALL POSTFL
                  call postfl
               end if
               if (txfile) then
!                    Write Values to TOXXFILE File (9/29/92) ---  CALL TOXXFL
                  call toxxfl
               end if
!                 Flush Block Average Values in AVEVAL Array for This IAVE
               aveval(1:numrec,1:numgrp,iave,1:numtyp) = 0.0d0
            end if
         end do
!           End Averaging Period LOOP

! ---       Check for PM25AVE, NO2AVE or SO2AVE to update daily
!           maximum value arrays; also output to MAXDAILY file,
!           if requested
         if (pm25ave .or. no2ave .or. so2ave) then
            if (mod(ihour,24)==0) then
! ---             End of day reached, call MXDLYFL
               call mxdlyfl
            end if
         end if

         if (rstsav .and. ihour==24) then
            ndays = ndays + 1
            if (ndays == incrst) then
!                 Save Results to File for Later Re-start   ---   CALL RSDUMP
               call rsdump
               ndays = 0
            end if
         end if

!           Flush HRVAL Arrays (1:NUMTYP)
         hrval(:)   = 0.0d0
         aerval(:)  = 0.0d0
         prmval(:)  = 0.0d0
         if (allocated(backave)) backave(:)  = 0.0d0
         if (allocated(backhr))  backhr(:,:) = 0.0d0

         if (pvmrm .or. olm .or. arm2 .or.&
         &runttrm .or. grsm) then
!              Flush CHI(NUMREC,NUMSRC,NUMTYP) Array
            chi(:,:,:) = 0.0d0
            if(grsm)then
               chi_ttravplm = 0.0d0
               chi_ttravpan = 0.0d0
               chi_ttravaer = 0.0d0
               chi_ttravprm = 0.0d0
               chi_ttravchm(:,:) = 0.0d0
               bldfac(:,:) = 0.0d0
               PRMVAL_Src1 = 0.0d0
            end if
            if (psdcredit) then
!                 Flush ABVAL(NUMREC,NUMTYP) and BCVAL(NUMREC,NUMTYP) Arrays
               abval(:,:) = 0.0d0
               bcval(:,:) = 0.0d0
            end if
         end if

      end if

!        Check for end of year of data for PM25, NO2, SO2, or MULTYR processing;
!        but skip if NOCHKD option or WARNCHKD option is used (this also includes
!        SCREEN option since SCREEN ==> NOCHKD)
      if (fulldate>isdate .and. .not.eof .and. .not.nochkd .and.&
      &.not.l_warnchkd .and.&
      &(pm25ave .or. no2ave .or. so2ave .or.&
      &annual .or. multyr)) then

         call chk_endyr

      elseif(fulldate==isdate .and..not.eof .and..not.nochkd .and.&
      &.not.l_warnchkd .and.&
      &(pm25ave .or. no2ave .or. so2ave .or.&
      &annual .or. multyr)) then

         nremain = nremain + 1

      end if

!        Reset CALCS and ENDMON Flags
      calcs  = .false.
      endmon = .false.

!        Save precipitation rates for two previous hours
      prec2 = prec1
      prec1 = Prate

   end do hour_loop
!     End Hourly LOOP

!     Check for TOXXFILE Option, Fill Buffer and Dump to File - 9/29/92
   if (txfile) then
      idum = 0
      rdum = 0.0d0
      do iave = 1, numave
         if (itoxfl(iave) == 1) then
!              Fill Rest of Buffer With Zeroes and Write to TOXXFILE
            do i = ipair+1, npair
               idconc(iave,i) = idum
               txconc(iave,i) = rdum
            end do
            write(itxunt(iave)) (idconc(iave,i),i=1,npair)
            write(itxunt(iave)) (txconc(iave,i),i=1,npair)
            close(itxunt(iave))
         end if
      end do
   end if

!     Write Out Update to the Screen for PC Version
   write(*,919)
919 format('+','Now Processing Output Options               ')

   return
end subroutine hrloop

subroutine julian(inyr,inmn,indy,jdy)
!***********************************************************************
!                 JULIAN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:    CONVERT YR/MN/DY DATE TO JULIAN DAY (1-366),
!                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
!                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
!                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:     YEAR,  INYR (2 OR 4 DIGIT)
!                    MONTH, INMN
!                    DAY,   INDY
!
!        OUTPUT:     JULIAN DAY,  JDY (1-366)
!
!        CALLED FROM:   DAYRNG
!
!        ERROR HANDLING:   Checks for Invalid Month or Day
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: nday(12), idymax(12)
   integer :: inyr, inmn, indy, jdy

!     Variable Initializations
   data nday/0,31,59,90,120,151,181,212,243,273,304,334/
   data idymax/31,29,31,30,31,30,31,31,30,31,30,31/
   modnam = 'JULIAN'
   jdy = 0

!     Check for Invalid Month or Day
   if (inmn<1 .or. inmn>12) then
!        WRITE Error Message    ! Invalid Month
      write(dummy,'(''MONTH = '',I2)') inmn
      call errhdl(path,modnam,'E','203',dummy)
      runerr = .true.
      go to 999
   else if (indy > idymax(inmn)) then
!        WRITE Error Message    ! Invalid Day
      write(dummy,'(''DAY='',I2,'' MO='',I2)') indy,inmn
      call errhdl(path,modnam,'E','203',dummy)
      runerr = .true.
      go to 999
   end if

!     Determine JULIAN Day Number; For Non-Leap Year First
   if ((mod(inyr,4) /= 0) .or.&
   &(mod(inyr,100) == 0 .and. mod(inyr,400) /= 0)) then
!        Not a Leap Year
      if (inmn/=2 .or. (inmn==2 .and. indy<=28)) then
         jdy = indy + nday(inmn)
      else
!           WRITE Error Message    ! Invalid Date; 2/29 in a Non-Leap Year
         write(dummy,'("YR= ",I4)') inyr
         call errhdl(path,modnam,'E','370',dummy)
         jdy = 60
         runerr = .true.
      end if
   else
!        Leap Year
      jdy = indy + nday(inmn)
      if (inmn > 2)  jdy = jdy + 1
   end if

999 continue

   return
end subroutine julian

subroutine gregor(inyr,inmn,jdy,idy)
!***********************************************************************
!                 GREGOR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:    CONVERT JULIAN DAY (1-366) TO DAY OF MONTH,
!                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
!                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
!                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:     YEAR,       INYR (2 OR 4 DIGIT)
!                    MONTH,      INMN
!                    JULIAN DAY, JDY (1-366)
!
!        OUTPUT:     DAY OF MONTH, IDY
!
!        CALLED FROM:   METEXT
!
!        ERROR HANDLING:   Checks for Invalid Month or Day
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: nday(12)
   integer :: inyr, inmn, idy, jdy

!     Variable Initializations
   data nday/0,31,59,90,120,151,181,212,243,273,304,334/
   modnam = 'GREGOR'

!     Check for Invalid Month or Julian Day
   if (inmn<1 .or. inmn>12) then
!        WRITE Error Message    ! Invalid Month
      call errhdl(path,modnam,'E','203','MONTH')
      go to 999
   else if (jdy<1 .or. jdy>366) then
!        WRITE Error Message    ! Invalid Julian Day
      call errhdl(path,modnam,'E','203','Juli Day')
      go to 999
   end if

!     Determine Day-of-Month Number; For Non-Leap Year First
   if ((mod(inyr,4) /= 0) .or.&
   &(mod(inyr,100)==0 .and. mod(inyr,400)/=0)) then
!        Not a Leap Year
      idy = jdy - nday(inmn)
   else
!        Leap Year
      idy = jdy - nday(inmn)
      if (inmn > 2)  idy = idy - 1
   end if

999 continue

   return
end subroutine gregor

subroutine hrqread (is)
!***********************************************************************
!*                  HRQREAD Module of AERMOD
!*
!*         PURPOSE: To Assign Hourly Source Parameters
!*
!*         PROGRAMMER:  Jayant Hardikar, Roger Brode
!*
!*         DATE:    September 15, 1993
!*
!*         INPUTS:  Current Source Number Being Processed
!*
!*         OUTPUTS: Source Arrays
!*
!*         Revision History:
!*
!*         MODIFIED:  Added possibility of RLINE or RLINEXT source type.
!*                    Wood, 03/18/2019
!*
!*         MODIFIED:  Added processing of hourly emissions file for
!*                    a RLINE source.
!*                    Wood, 7/20/2018
!*
!*         MODIFIED:  Added processing of hourly emissions file for
!*                    a buoyant line source.
!*                    Amec Foster Wheeler, 6/30/2015
!*
!*         MODIFIED:  Included check on length of FIELD(7) before assigning
!*                    to the HRSOID variable to avoid runtime error if
!*                    SRCIDs in HOUREMIS file exceed 12 character limit.
!*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 03/19/2014
!*
!*         MODIFIED:  Check for use of 4-digit year in HOUREMIS file, and
!*                    adjust if needed for comparison to KURDAT from the
!*                    met data file.
!*                    Incorporated options to specify hourly-varying
!*                    release heights and initial dispersion coefficients
!*                    for VOLUME and AREA sources.
!*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!*
!*         MODIFIED:  Corrected processing of missing parameters for
!*                    point sources to assign all parameters to 0.0 if
!*                    any of the parameters are missing, in conformance
!*                    with Section 3.3.9 of the AERMOD User's Guide.
!*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
!*
!*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES
!*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
!*
!*         CALLED FROM:  HRLOOP
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, is
   integer :: ihyear, ihmon, ihday, ihhour, ihyear2
   integer :: ilsave
   character (len=20) :: rdfrm

   character (len=12) :: hrsoid

!*    Variable Initializations
   modnam = 'HRQREAD'

!*    Assign IQLINE counter to ILINE for passing to ERRHDL if needed, save as ILSAVE first
   ilsave = iline
   iline  = iqline

!*    READ Record to Buffers, A'num' and 'num'A1, where num=ISTRG
!*    Length of ISTRG is Set in PARAMETER Statement in MAIN1
!     Setup READ format and ECHO format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   write(rdfrm,9100) istrg, istrg
9100 format('(A',i4.4,',T1,',i4.4,'A1)')
   read (ihremi,rdfrm,end=888,err=99) runst1, (runst(i), i=1, istrg)
!*
!*    Convert Lower Case to Upper Case Letters              ---   CALL LWRUPR
   call lwrupr
!*
!*    Define Fields on Card                                 ---   CALL DEFINE
   call define
!*
!*    Get the Contents of the Fields                        ---   CALL GETFLD
   call getfld
!*
!*    Check for number of fields - error if less than 7.
   if (ifc < 7) then
      write(dummy,'(I8)') kurdat
      call errhdl(path,modnam,'E','384',dummy)
      runerr = .true.
      go to 999
   end if
!*
!*    Assign the Fields to Local Varables and Check The Numerical Field
!*
!*    Date and time variables common to all source types
!*
   call stonum(field(3), ilen_fld, fnum, imit)
   ihyear = nint(fnum)
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208','HOUREMIS')
      runerr = .true.
      go to 999
   end if

   call stonum(field(4), ilen_fld, fnum, imit)
   ihmon = nint(fnum)
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208','HOUREMIS')
      runerr = .true.
      go to 999
   end if

   call stonum(field(5), ilen_fld, fnum, imit)
   ihday = nint(fnum)
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208','HOUREMIS')
      runerr = .true.
      go to 999
   end if

   call stonum(field(6), ilen_fld, fnum, imit)
   ihhour = nint(fnum)
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208','HOUREMIS')
      runerr = .true.
      go to 999
   end if

!      D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
   if (ihyear <= 99) then
      call cent_date(ihyear2,ihyear)
   end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C --- Check for use of 2-digit year in HOUREMIS file, adjust to 4-digit
!C     year for comparison with FULLDATE based on met data file
!      IF (IHYEAR .LE. 99) THEN
!         IHYEAR2 = IHYEAR
!         IF (IHYEAR2 .GE. ISTRT_WIND .and.
!     &                        IHYEAR2 .LE. 99) THEN
!            IHYEAR = ISTRT_CENT*100 + IHYEAR2
!         ELSE IF (IHYEAR2 .LT. ISTRT_WIND) THEN
!            IHYEAR = (ISTRT_CENT+1)*100 + IHYEAR2
!         END IF
!      END IF

! --- Calculate current date (YYYYMMDDHH) from HOUREMIS file record, FULLHRQ
   fullhrq = ihyear*1000000 + ihmon*10000 + ihday*100 + ihhour

! --- Assign source ID but check for field length > 12 first
   if( len_trim(field(7)) <= 12 ) then
      hrsoid = field(7)
   else
      hrsoid = field(7)(1:12)
   end if

!*    Check for Source ID Consistency ; If Failed Issue Error
   if ( hrsoid /= srcid(is) ) then
      write(dummy,'(A12)') srcid(is)
      call errhdl(path,modnam,'E','342',srcid(is))
      runerr = .true.
      go to 999
   end if

   if (ifc == 7) then
!*       All parameters missing for this hour/source - WRITE Warning Message
!*       Assign zeros to all parameters
      if (.not. L_SkipMessages) then
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','344',dummy)
      end if
      hrqs = 0.0d0
      hrts = 0.0d0
      hrvs = 0.0d0
      hrhs = 0.0d0
      hrsy = 0.0d0
      hrsz = 0.0d0
      hrfp = 0.0d0

! ------------------------ Begin correct # of parameters for source type

   else if (srctyp(is)(1:5) == 'POINT' .and. ifc==10) then
!*       Assign emission rate, exit temperature and exit velocity
!*       for POINT sources

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      call stodbl(field(9), ilen_fld, hrts, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
      end if

      call stodbl(field(10), ilen_fld, hrvs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
      end if

   else if (srctyp(is) == 'VOLUME' .and. ifc==11) then
!*       Assign emission rate, release height and initial sigmas
!*       for VOLUME source.
!*       Assign logical variable indicating hourly sigmas, L_HRLYSIG
      if (ilsave == 1) then
         l_hrlysig(is) = .true.
      else if (ilsave > 1 .and. .not. l_hrlysig(is)) then
!*          This volume source should not include hourly sigmas;
!*          issue error message
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','345',dummy)
         hrqs = 0.0d0
         runerr = .true.
         go to 999
      end if

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      call stodbl(field(9), ilen_fld, hrhs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
      end if

      call stodbl(field(10), ilen_fld, hrsy, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
      end if

      call stodbl(field(11), ilen_fld, hrsz, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
      end if

   else if (srctyp(is) == 'VOLUME' .and. ifc==8) then
!*       Assign emission rate for volume sources
!*       Check logical variable indicating hourly sigmas, L_HRLYSIG
      if (l_hrlysig(is)) then
!*          WRITE Error Message; Hourly Sigmas must be used for all hours
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','345',dummy)
         hrqs = 0.0d0
         runerr = .true.
         go to 999
      end if

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

   else if ((srctyp(is)(1:4)=='AREA' .or. srctyp(is)=='LINE'&
   &.or. srctyp(is)=='RLINE' .or. srctyp(is)=='RLINEXT')&
   &.and. ifc==10) then
!*       Assign emission rate for AREA and LINE sources
!*       Assign logical variable indicating hourly sigmas, L_HRLYSIG
      if (ilsave == 1) then
         l_hrlysig(is) = .true.
      else if (ilsave > 1 .and. .not. l_hrlysig(is)) then
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','345',dummy)
         hrqs = 0.0d0
         runerr = .true.
         go to 999
      end if

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      call stodbl(field(9), ilen_fld, hrhs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
      end if

      call stodbl(field(10), ilen_fld, hrsz, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
      end if

   else if ((srctyp(is)(1:4)=='AREA' .or. srctyp(is)=='LINE'&
   &.or. srctyp(is)=='RLINE' .or. srctyp(is)=='RLINEXT')&
   &.and. ifc==8) then
!*       Assign emission rate for AREA and LINE sources
!*       Check logical variable indicating hourly sigmas, L_HRLYSIG
      if (l_hrlysig(is)) then
!*          WRITE Error Message; Hourly Sigmas must be used for all hours
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','345',dummy)
         runerr = .true.
         hrqs = 0.0d0
         go to 999
      end if

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

   else if (srctyp(is) == 'OPENPIT' .and. ifc==8) then
!*       Assign emission rate for OPENPIT sources
      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

   else if (srctyp(is) == 'BUOYLINE' .and. ifc==9) then
!*       Assign emission rate (field 8) and average buoyancy parameter
!        (field 9) for BUOYANT LINE sources

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      call stodbl(field(9), ilen_fld, hrfp, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 999
      end if

! -------------------------- End of correct # parameters for source type
! -------------------------- Begin too many parameters for source type

   else if (srctyp(is)(1:5)=='POINT' .and. ifc>10) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','346',dummy)
      hrqs = 0.0d0
      hrts = 0.0d0
      hrvs = 0.0d0
      runerr = .true.

   else if (srctyp(is)=='VOLUME' .and.&
   &((l_hrlysig(is) .and. ifc>11) .or.&
   &(.not.l_hrlysig(is) .and. ifc>8))) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','346',dummy)
      hrqs = 0.0d0
      hrhs = 0.0d0
      hrsy = 0.0d0
      hrsz = 0.0d0
      runerr = .true.

   else if ((srctyp(is)(1:4)=='AREA' .or. srctyp(is)=='LINE'&
   &.or. srctyp(is)=='RLINE' .or. srctyp(is) == 'RLINEXT')&
   &.and. ((l_hrlysig(is) .and. ifc>10) .or.&
   &(.not.l_hrlysig(is) .and. ifc>8))) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','346',dummy)
      hrqs = 0.0d0
      hrhs = 0.0d0
      hrsz = 0.0d0
      runerr = .true.

   else if (srctyp(is)=='OPENPIT' .and. ifc>8) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','346',dummy)
      hrqs = 0.0d0
      runerr = .true.

   else if (srctyp(is) == 'BUOYLINE' .and. ifc>9) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','346',dummy)
      hrqs = 0.0d0
      hrfp = 0.0d0
      runerr = .true.

! -------------------------- End of too many parameters for source type
! -------------------------- Begin of too few parameters for source type

   else if (srctyp(is)(1:5) == 'POINT') then
!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','384',dummy)
      hrqs = 0.0d0
      hrts = 0.0d0
      hrvs = 0.0d0
      runerr = .true.

   else if (srctyp(is) == 'VOLUME' .and.&
   &((l_hrlysig(is) .and. ifc<11) .or.&
   &(.not.l_hrlysig(is) .and. ifc<8))) then
!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','384',dummy)
      hrqs = 0.0d0
      hrhs = 0.0d0
      hrsy = 0.0d0
      hrsz = 0.0d0
      runerr = .true.

   else if ((srctyp(is)(1:4)=='AREA' .or. srctyp(is)=='LINE'&
   &.or. srctyp(is)=='RLINE' .or. srctyp(is) == 'RLINEXT')&
   &.and. ((l_hrlysig(is) .and. ifc<10) .or.&
   &(.not.l_hrlysig(is) .and. ifc<8))) then
!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','384',dummy)
      hrqs = 0.0d0
      hrhs = 0.0d0
      hrsz = 0.0d0
      runerr = .true.

   else if (srctyp(is) == 'BUOYLINE') then
!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','384',dummy)
      hrqs = 0.0d0
      hrfp = 0.0d0
      runerr = .true.

! ---------------------------- End of too few parameters for source type

   else
!*       Problem processing HOUREMIS record - WRITE Error Message
!*       Assign zeros to emission rate
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','345',dummy)
      hrqs = 0.0d0
      runerr = .true.

   end if

   go to 999

!*    Write Error Message for Error Reading Hourly Emissions File
99 call errhdl(path,modnam,'E','510','HOUREMIS')
   runerr = .true.
   go to 999

888 continue

   eof = .true.

999 return
end subroutine hrqread

subroutine hrqext (is)
!***********************************************************************
!*                  HRQEXT Module of AERMOD
!*
!*         PURPOSE: To Assign Hourly Source Parameters
!*
!*         PROGRAMMER:  Jayant Hardikar, Roger Brode
!*
!*         DATE:    September 15, 1993
!*
!*         INPUTS:  Current Source Number Being Processed
!*
!*         OUTPUTS: Source Arrays
!*
!*         Revision History:
!*
!*         MODIFIED: Streamlined for Aircraft sources, by adding nested
!*                   IF-statement instead of complicated IF-statements.
!*                   M.G. Snyder WSP 6/5/2023
!*
!*         MODIFIED:  Modified to assign Aircraft Engine parameters to
!*                    calculate plume rise for VOLUME/AREA Sources
!*                    only for Aircraft Source Group.
!*                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                     04/01/2023
!*
!*         MODIFIED:  Included options for hourly-varying release height and
!*                    initial dispersion for RLINE and RLINEXT sources.
!*                    Wood 3/27/2019
!*
!*         MODIFIED:  Incorporated options to specify hourly-varying
!*                    release heights and initial dispersion coefficients
!*                    for VOLUME and AREA sources.
!*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!*
!*         MODIFIED:  Corrected processing of missing parameters for
!*                    point sources to assign all parameters to 0.0 if
!*                    any of the parameters are missing, in conformance
!*                    with Section 3.3.9 of the AERMOD User's Guide.
!*                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
!*
!*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES
!*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
!*
!*         CALLED FROM:  HRLOOP
!************************************************************************
!*
!*    Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12

   integer :: is

!*    Variable Initializations
   modnam = 'HRQEXT'

!*    Assign the Hourly Emission Parameters to the appropriate arrays
   if (evonly) then

      aqs(is) = ev_hrqs(is,ihour)

      if (srctyp(is)(1:5) == 'POINT') then
         ats(is) = ev_hrts(is,ihour)
         avs(is) = ev_hrvs(is,ihour)
      else if (srctyp(is) == 'VOLUME' .and. l_hrlysig(is)) then
         ahs(is)    = ev_hrhs(is,ihour)
         asyini(is) = ev_hrsy(is,ihour)
         aszini(is) = ev_hrsz(is,ihour)
      else if (srctyp(is)(1:4) == 'AREA' .and. l_hrlysig(is)) then
         ahs(is)    = ev_hrhs(is,ihour)
         aszini(is) = ev_hrsz(is,ihour)
      else if (srctyp(is) == 'LINE' .and. l_hrlysig(is)) then
         ahs(is)    = ev_hrhs(is,ihour)
         aszini(is) = ev_hrsz(is,ihour)
      else if (srctyp(is) == 'RLINE' .and. l_hrlysig(is)) then
         ahs(is)    = ev_hrhs(is,ihour)
         aszini(is) = ev_hrsz(is,ihour)
      else if (srctyp(is) == 'RLINEXT' .and. l_hrlysig(is)) then
         ahs(is)    = ev_hrhs(is,ihour)
         aszini(is) = ev_hrsz(is,ihour)
      else if (srctyp(is) == 'BUOYLINE') then
         afp(is)    = ev_hrfp(is,ihour)
      end if

!**  Added for Aircraft Plume Rise; UNC-IE !D151 - MGS 6/5/23
      if (aftsrc(is) == 'Y') then
         amfuel(is)    = ev_hrmfuel(is,ihour)
         athrust(is)   = ev_hrthrust(is,ihour)
         avaa(is)      = ev_hrvaa(is,ihour)
         aafr(is)      = ev_hrafr(is,ihour)
         abypr(is)     = ev_hrbypr(is,ihour)
         arpwr(is)     = ev_hrrpwr(is,ihour)
         asrcangle(is) = ev_hrsrcangle(is,ihour)
      end if
!**  End Aircraft Plume Rise insert; April 2023 !D151 - MGS 6/5/23

   else

      aqs(is) = hrqs

      if (srctyp(is)(1:5) == 'POINT') then
         ats(is) = hrts
         avs(is) = hrvs
      else if (srctyp(is) == 'VOLUME' .and. l_hrlysig(is)) then
         ahs(is)    = hrhs
         asyini(is) = hrsy
         aszini(is) = hrsz
      else if (srctyp(is)(1:4) == 'AREA' .and. l_hrlysig(is)) then
         ahs(is)    = hrhs
         aszini(is) = hrsz
      else if (srctyp(is) == 'LINE' .and. l_hrlysig(is)) then
         ahs(is)    = hrhs
         aszini(is) = hrsz
      else if (srctyp(is) == 'RLINE' .and. l_hrlysig(is)) then
         ahs(is)    = hrhs
         aszini(is) = hrsz
      else if (srctyp(is) == 'RLINEXT' .and. l_hrlysig(is)) then
         ahs(is)    = hrhs
         aszini(is) = hrsz
      else if (srctyp(is) == 'BUOYLINE') then
         afp(is)    = hrfp
      end if

!**  Added for Aircraft Plume Rise; UNC-IE !D151 - MGS 6/5/23
      if (aftsrc(is) == 'Y') then
         amfuel(is) = hrmfuel
         athrust(is) = hrthrust
         avaa(is) = hrvaa
         aafr(is)    = hrafr
         abypr(is) = hrbypr
         arpwr(is) = hrrpwr
         asrcangle(is) = hrsrcangle
      end if
!**  End Aircraft Plume Rise insert; April 2023 !D151 - MGS 6/5/23

   end if

!*    Perform QA Error Checking on Source Parameters

   if (srctyp(is)(1:5) == 'POINT') then
      if (ats(is) == 0.0d0) then
!*          Set Temperature to Small Negative Value for Ambient Releases
         ats(is) = -1.0d-5
      else if (ats(is) > 2000.0d0) then
!*          WRITE Warning Message:  Exit Temp. > 2000K
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRTS')
         end if
      else if ( (dabs(aqs(is))>0.0d0) .and.&
      &(ats(is)>0.0d0) .and. (ats(is)<200.0d0) .and.&
      &(avs(is)>200.0d0) ) then
!*          Exit temp < 200K (about -100F) and exit velocity > 200m/s
!*          with non-zero emissions; Incorrect units may have been
!*          used or ATS and AVS may have been switched;
!*          WRITE Fatal Error Message
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'E','320','HRTS')
            runerr = .true.
         end if
      else if ( (dabs(aqs(is))>0.0d0) .and.&
      &(ats(is)>0.0d0) .and. (ats(is)<200.0d0) ) then
!*          Exit temp < 200K (about -100F) with non-zero emissions;
!*          Incorrect units may have been used or ATS and AVS may
!*          have been switched;
!*          WRITE Warnign Message
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRTS')
         end if
      end if

      if (avs(is) < 0.0d0) then
!*          WRITE Warning Message:  Negative or Zero Exit Velocity
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','325',srcid(is))
         end if
!*          Set to Small Value to Avoid Zero-divide and Underflow
         avs(is) = 1.0d-5
      else if (avs(is) < 1.0d-5) then
!*          Set to Small Value to Avoid Zero-divide and Underflow
         avs(is) = 1.0d-5
      else if (avs(is) > 50.0d0) then
!*          WRITE Informational Message:  Exit Velocity > 50.0 m/s
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'I','320','HRVS')
         end if
      end if

   else if (srctyp(is) == 'VOLUME') then
      if (ahs(is) < 0.0d0) then
!           WRITE Error Message:  Negative Release Height
         call errhdl(path,modnam,'E','209','HRHS')
      else if (ahs(is) > 100.0d0) then
!           WRITE Warning Message:  Large Release Height (> 100M)
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRHS')
         end if
      else if (ahs(is) > 3000.0d0) then
!           WRITE Error Message:  Large Release Height (> 3000M)
         call errhdl(path,modnam,'E','324',srcid(is))
         runerr = .true.
      end if

      if (asyini(is) < 0.0d0) then
!           WRITE Warning Message:  Negative Initial Lateral Parameter
         call errhdl(path,modnam,'E','209','HRSY')
!           Set to Small Value to Avoid Zero-divide and Underflow
         asyini(is) = 1.0d-5
      else if (asyini(is) < 1.0d-5) then
!           WRITE Warning Message:  Small Initial Lateral Parameter
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRSY')
         end if
!           Set to Small Value to Avoid Zero-divide and Underflow
         asyini(is) = 1.0d-5
      else if (asyini(is) > 200.0d0) then
!           WRITE Warning Message:  Large Initial Lateral Parameter (> 200m)
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRSY')
         end if
      end if

      if (aszini(is) < 0.0d0) then
!           WRITE Warning Message:  Negative Initial Vertical Parameter
         call errhdl(path,modnam,'E','209','HRSZ')
!           Set to Small Value to Avoid Zero-divide and Underflow
         aszini(is) = 1.0d-5
      else if (aszini(is) < 1.0d-5) then
!           WRITE Warning Message:  Small Initial Lateral Parameter
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRSZ')
         end if
!           Set to Small Value to Avoid Zero-divide and Underflow
         aszini(is) = 1.0d-5
      else if (aszini(is) > 200.0d0) then
!           WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRSZ')
         end if
      end if

   else if (srctyp(is)(1:4)=='AREA' .or. srctyp(is)=='LINE'&
   &.or. srctyp(is)=='RLINE'&
   &.or. srctyp(is)=='RLINEXT') then
      if (ahs(is) < 0.0d0) then
!           WRITE Error Message:  Negative Release Height
         call errhdl(path,modnam,'E','209','HRHS')
      else if (ahs(is) > 100.0d0) then
!           WRITE Warning Message:  Large Release Height (> 100M)
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRHS')
         end if
      else if (ahs(is) > 3000.0d0) then
!           WRITE Error Message:  Large Release Height (> 3000M)
         call errhdl(path,modnam,'E','324',srcid(is))
         runerr = .true.
      end if

      if (aszini(is) < 0.0d0) then
!           WRITE Warning Message:  Negative Initial Vertical Parameter
         call errhdl(path,modnam,'E','209','HRSZ')
!           Set to Small Value to Avoid Zero-divide and Underflow
         aszini(is) = 1.0d-5
      else if (aszini(is) < 1.0d-5) then
!           WRITE Warning Message:  Small Initial Lateral Parameter
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRSZ')
         end if
!           Set to Small Value to Avoid Zero-divide and Underflow
         aszini(is) = 1.0d-5
      else if (aszini(is) > 200.0d0) then
!           WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         if (.not. L_SkipMessages) then
            call errhdl(path,modnam,'W','320','HRSZ')
         end if
      end if

   else if (srctyp(is)=='BUOYLINE') then
      if (afp(is) < 0.0d0) then
!           WRITE Error Message:  Negative Buoyancy Parameter
         call errhdl(path,modnam,'E','209','HRFP')
      end if

   end if

   return
end subroutine hrqext

subroutine o3ext(L_ReadErr,ReadErr,EndErr)
!***********************************************************************
!*                  O3EXT Module of AERMOD
!*
!*         PURPOSE: To extract hourly ozone data for PVMRM, OLM, TTRM,
!*                   or GRSM options
!*
!*         PROGRAMMER:  Roger W. Brode, PES, Inc.
!*
!*         DATE:    May 6, 2002
!*
!*         MODIFIED: Feb. 2021 for inclusion of Travel Time
!*         Reaction Method (TTRM)
!*
!*         INPUTS:
!*
!*         OUTPUTS:
!*
!*         CALLED FROM:  HRLOOP
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   character :: ReadErr*5, EndErr*5

   double precision :: o3sub(6), o3temp, o3max24, o3min

   integer :: io3yr, io3mn, io3dy, io3hr, io3yr2, i
   integer :: fullo3hr(6)
   logical :: L_ReadErr

!*    Variable Initializations
   modnam  = 'O3EXT'
   ReadErr = ''
   EndErr  = ''
   L_ReadErr = .false.
   fullo3hr(:) = 0

! --- Initialize logical for missing data to FALSE
   o3miss = .false.

!*    Assign IOLINE counter to ILINE for passing to ERRHDL if needed
   iline = ioline
!*
! --- Read a record in all hourly O3 files, but only read concentation
!     based on the applicable file for the current sector
!     First initialize O3CONC to 0.0, and O3TEMP, O3SUB and O3MIN to -99.
   o3conc   = 0.0d0
   o3temp   = -99.0d0
   o3sub(:) = -99.0d0
   o3min    = -99.0d0

   do i = 1, NUMO3Sects
! ---    Loop through O3SECTORs

! ---    Reinitialize O3SUB for this sector
      o3sub(i) = -99.0d0

! ---    Check for non-hourly O3 values to substitute for missing data
      if (l_o3values(i)) then
         call ozonvals(i,o3sub(i))
      else if (l_o3val(i)) then
         o3sub(i) = o3back(i)
      else
         o3sub(i) = 0.0d0
      end if

! ---    Check for hour O3FILE for this sector
      if (L_O3File(i)) then

! ---       Hourly O3 file available for current sector

         if (i == io3sect) then
! ---          This is the applicable sector for this hour; read next hour of O3 data

! ---          Initialize ReadErr and EndErr for this sector
            write(ReadErr,'(''SECT'',I1)') i

            if (o3form(i) == 'FREE') then
               read(io3unt(i),*,err=99,end=9991) io3yr, io3mn,&
               &io3dy, io3hr,&
               &o3conc
            else
!                 D001/D145 CRT 6/1/2023 - Delete erroneous read statement introduced with bug fix
!                  READ(IO3UNT(I),O3FORM(I),ERR=99,END=9991)
               read(io3unt(i),o3form(i),err=99,end=9991)&
               &io3yr, io3mn,&
               &io3dy, io3hr,&
               &o3conc
            end if

!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
            if(io3yr <= 99) then
               call cent_date(io3yr2,io3yr)
            end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---          Check for use of 2-digit year in OZONEFIL file, adjust to 4-digit
!C              year for comparison with FULLDATE based on met data file
!               IF (IO3YR .LE. 99) THEN
!                  IO3YR2 = IO3YR
!                  IF (IO3YR2 .GE. ISTRT_WIND .and.
!     &                                 IO3YR2 .LE. 99) THEN
!                     IO3YR  = ISTRT_CENT*100 + IO3YR2
!                  ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
!                     IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
!                  END IF
!               END IF

! ---          Calculate full date for this hour of O3 data
            fullo3hr(i) = io3yr*1000000 + io3mn*10000 + io3dy*100&
            &+ io3hr

            if (o3conc >= 0.0d0 .and. o3conc < 900.0d0) then
! ---             Valid hourly value; convert to ug/m3 if needed
               if (o3filunits == 'PPB') then
                  o3conc = o3conc * o3_ppb
               else if (o3filunits == 'PPM') then
                  o3conc = o3conc * o3_ppm
               end if
! ---             Valid hourly O3 value; check for application of O3MIN value
!RCO D074 Add check for NOMIN03 option to turn off minimum ozone 1/7/2021
               if (.not. nomino3) then
                  if (stable) then
!                       Use min of 40 ppb (78.4ug/m3) and max from previous 24 hrs
                     o3max24 = min ( 78.40d0,&
                     &maxval( O3_Max24hr(:,io3sect) ) )
!                       Adjust minimum O3 value based on OBULEN
                     if (obulen >  0.0d0 .and.&
                     &obulen <= 50.0d0) then
                        o3min = o3max24
                     else if (obulen > 250.0d0) then
                        o3min = 0.0d0
                     else
                        o3min = o3max24 * (250.d0 - obulen) /200.d0
                     end if
                  else
                     o3min = -9.0d0
                  end if
! ---                Save this hour's O3CONC (in ug/m3) to array of previous
!                    24 values, before applying minimum value
                  O3_Max24hr(io3hr,io3sect) = o3conc
                  o3conc = max( o3conc, o3min )
               end if
            else if (l_o3values(io3sect) .or.&
            &l_o3val(io3sect)) then
! ---             Hourly O3 value is missing; assign O3SUB value based on
!                 O3VALUES or OZONEVAL inputs
               o3conc = o3sub(io3sect)
! ---             Assign 0.0 to O3_Max24hr array for this hour
               O3_Max24hr(io3hr,io3sect) = 0.0d0
            else
! ---             Assign O3MISS logical to TRUE
               o3miss = .true.
! ---             Assign 0.0 to O3_Max24hr array for this sector
               O3_Max24hr(io3hr,io3sect) = 0.0d0
            end if

            go to 9992

9991        continue
!              End-of-file reached, set logical flag
            eof = .true.

            write(EndErr, '(''SECT'',I1)') i

9992        continue

         else
! ---          This is not applicable sector for this hour; read record with temp data

! ---          Initialize ReadErr and EndErr for this sector
            write(ReadErr,'(''SECT'',I1)') i

            if (o3form(i) == 'FREE') then
               read(io3unt(i),*,err=99,end=9993) io3yr, io3mn,&
               &io3dy, io3hr,&
               &o3temp
            else
               read(io3unt(i),o3form(i),err=99,end=9993)&
               &io3yr, io3mn,&
               &io3dy, io3hr,&
               &o3temp
            end if

!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
            if(io3yr <= 99) then
               call cent_date(io3yr2,io3yr)
            end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---          Check for use of 2-digit year in OZONEFIL file, adjust to 4-digit
!C              year for comparison with FULLDATE based on met data file
!               IF (IO3YR .LE. 99) THEN
!                  IO3YR2 = IO3YR
!                  IF (IO3YR2 .GE. ISTRT_WIND .and.
!     &                                 IO3YR2 .LE. 99) THEN
!                     IO3YR  = ISTRT_CENT*100 + IO3YR2
!                  ELSE IF (IO3YR2 .LT. ISTRT_WIND) THEN
!                     IO3YR  = (ISTRT_CENT+1)*100 + IO3YR2
!                  END IF
!               END IF
! ---          Calculate full date for this hour of O3 data
            fullo3hr(i) = io3yr*1000000 + io3mn*10000 + io3dy*100&
            &+ io3hr

            if (o3temp >= 0.0d0 .and. o3temp < 900.0d0) then
! ---             Valid hourly value; convert to ug/m3 if needed
               if (o3filunits == 'PPB') then
                  o3temp = o3temp * o3_ppb
               else if (o3filunits == 'PPM') then
                  o3temp = o3temp * o3_ppm
               end if
! ---             Save this hour's O3CONC (in ug/m3) to array of previous
!                 24 values for this sector
               O3_Max24hr(io3hr,i) = o3temp
            else if (l_o3values(i) .or.&
            &l_o3val(i)) then
! ---             Hourly O3 value is missing; assign O3SUB value;
!                 these have already been converted to ug/m3
               o3temp = o3sub(i)
! ---             Assign 0.0 to O3_Max24hr array so that substituted value will
!                 not be used in determining max value from previous 24 hours
               O3_Max24hr(io3hr,i) = 0.0d0
            else
! ---             Assign 0.0 to O3_Max24hr array
               O3_Max24hr(io3hr,i) = 0.0d0
            end if

            go to 9994

9993        continue
!              End-of-file reached, set logical flag
            eof = .true.

            write(EndErr, '(''SECT'',I1)') i

9994        continue

         end if

      end if

   end do   ! END of O3Sector Loop

   if (o3miss) then
! ---    No O3 value available for this hour; assign 0.0 to O3CONC
!        and issue informational message
      o3conc = 0.0d0
      if (.not. L_SkipMessages) then
         write(dummy,'(I10.10)') fulldate
         call errhdl(path,modnam,'I','459',dummy)
      end if
   end if

!*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
   do i = 1, NUMO3Sects
      if (L_O3File(i)) then
         if (fulldate /= fullo3hr(i)) then
!*             WRITE Error Message - Date mismatch
            write(dummy,'(I10.10,''S'',I1)') fulldate, i
            call errhdl(path,modnam,'E','457',dummy)
!*             Set RUNERR logical and skip to end
            runerr = .true.
            go to 1000
         end if
      end if
   end do

!     Date/Time consistency checks were ok; skip to end
   go to 1000

!*    Write Error Message for Error Reading Hourly Ozone File
99 continue
   L_ReadErr = .true.
   write(ReadErr,'(''SECT'',I1)') i
   call errhdl(path,modnam,'E','510',ReadErr)
   runerr = .true.

   go to 1000

   continue
!     End-of-file reached, set logical flag
   eof = .true.

! --- End of file reached on O3 file; check FULLDATE vs. FULLO3HR before returning
!*    for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
   do i = 1, NUMO3Sects
      if (L_O3File(i)) then
         if (fulldate /= fullo3hr(i)) then
!*             WRITE Error Message - Date mismatch
            write(dummy,'(I10.10,''S'',I1)') fulldate, i
            call errhdl(path,modnam,'E','457',dummy)
            runerr = .true.
         end if
      end if
   end do

1000 return
end subroutine o3ext

subroutine bgext(L_ReadErr,ReadErr,EndErr)
!***********************************************************************
!*                  BGEXT Module of AERMOD
!*
!*         PURPOSE: To extract hourly background concentrations
!*
!*         PROGRAMMER:  Roger W. Brode
!*
!*         DATE:        February 28, 2011
!*
!*         INPUTS:
!*
!*         OUTPUTS:
!*
!*         CALLED FROM:  HRLOOP
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   character :: ReadErr*5, EndErr*5

   integer :: ibgyr, ibgmn, ibgdy, ibghr, ibgyr2, i
   integer :: fullbghr(6)
   double precision :: BGHrVal, bgsub(6)
   logical :: L_ReadErr

!*    Variable Initializations
   modnam  = 'BGEXT'
   ReadErr = ''
   EndErr  = ''
   L_ReadErr = .false.

!*    Assign IBLINE counter to ILINE for passing to ERRHDL if needed
   iline = ibline
!*
! --- Read a record in all hourly BG files, but only read concentation
!     based on the applicable file for the current sector
!     First initialize BGHrVal to -99.0
   BGHrVal = -99.0d0

! --- Loop through all BACKGRND Sectors
   do i = 1, NUMBGSects

! ---    Reinitialize BGSUB for this sector
      bgsub(i) = 0.0d0

! ---    Check for temporally-varying background to substitute for missing hours
      if (L_BGValues(i)) then
         call bgval(i,bgsub(i))
      else
!           No temporally-varying background for this sector; set BGSUB = 0.0
         bgsub(i) = 0.0d0
      end if

! ---    Check for HOURLY BACKGRDN data for this sector and read the data
      if (L_BGFile(i)) then

! ---       Hourly BG file available for current sector

! ---       Check for whether this is the applicable sector for this hour
         if (i == ibgsect) then

! ---          Initialize ReadErr and EndErr for this sector
            write(ReadErr,'(''SECT'',I1)') i

            if (bgform(i) == 'FREE') then
               read(ibgunt(i),*,err=99,end=9991) ibgyr,ibgmn,&
               &ibgdy,ibghr,&
               &BGHrVal
            else
               read(ibgunt(i),bgform(i),err=99,end=9991)&
               &ibgyr,ibgmn,&
               &ibgdy,ibghr,&
               &BGHrVal
            end if

            if (BGHrVal > 0.0d0) then
! ---             Valid hourly value; convert to ug/m3 if needed
               if (pollut == 'NO2') then
                  if (BackUnits == 'PPB') then
                     BGHrVal = BGHrVal / no2_ppb
                  else if (BackUnits == 'PPM') then
                     BGHrVal = BGHrVal / no2_ppm
                  end if
               else if (pollut == 'SO2') then
                  if (BackUnits == 'PPB') then
                     BGHrVal = BGHrVal / so2_ppb
                  else if (BackUnits == 'PPM') then
                     BGHrVal = BGHrVal / so2_ppm
                  end if
               else if (pollut == 'CO') then
                  if (BackUnits == 'PPB') then
                     BGHrVal = BGHrVal * co_ppb
                  else if (BackUnits == 'PPM') then
                     BGHrVal = BGHrVal * co_ppm
                  end if
               end if
            end if

!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
            if (ibgyr <= 99) then
               call cent_date(ibgyr2,ibgyr)
            end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---          Check for use of 2-digit year in BACKGRND file, adjust to 4-digit
!C              year for comparison with FULLDATE based on met data file
!               IF (IBGYR .LE. 99) THEN
!                  IBGYR2 = IBGYR
!                  IF (IBGYR2 .GE. ISTRT_WIND .and.
!     &                                 IBGYR2 .LE. 99) THEN
!                     IBGYR  = ISTRT_CENT*100 + IBGYR2
!                  ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
!                     IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
!                  END IF
!               END IF

!*             Assign full date for this HOURLY BACKGRND file
            fullbghr(i) = ibgyr*1000000 + ibgmn*10000 + ibgdy*100 +&
            &ibghr

            go to 9992

9991        continue
!              End-of-file reached, set logical flag
            eof = .true.

            write(EndErr, '(''SECT'',I1)') i

9992        continue

         else
! ---          This is not applicable sector for this hour; read record without data
!
! ---          Initialize ReadErr and EndErr for this sector
            write(ReadErr,'(''SECT'',I1)') i

            if (bgform(i) == 'FREE') then
               read(ibgunt(i),*,err=99,end=9993) ibgyr, ibgmn,&
               &ibgdy, ibghr
            else
               read(ibgunt(i),bgform(i),err=99,end=9993)&
               &ibgyr, ibgmn,&
               &ibgdy, ibghr
            end if

!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
            if (ibgyr <= 99) then
               call cent_date(ibgyr2,ibgyr)
            end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---          Check for use of 2-digit year in BACKGRND file, adjust to 4-digit
!C              year for comparison with FULLDATE based on met data file
!               IF (IBGYR .LE. 99) THEN
!                  IBGYR2 = IBGYR
!                  IF (IBGYR2 .GE. ISTRT_WIND .and.
!     &                                 IBGYR2 .LE. 99) THEN
!                     IBGYR  = ISTRT_CENT*100 + IBGYR2
!                  ELSE IF (IBGYR2 .LT. ISTRT_WIND) THEN
!                     IBGYR  = (ISTRT_CENT+1)*100 + IBGYR2
!                  END IF
!               END IF

!*             Assign full date for this HOURLY BACKGRND file
            fullbghr(i) = ibgyr*1000000 + ibgmn*10000 + ibgdy*100 +&
            &ibghr

            go to 9994

9993        continue
!              End-of-file reached, set logical flag
            eof = .true.

            write(EndErr, '(''SECT'',I1)') i

9994        continue

         end if

      end if   ! End of L_BGFile IF Block

   end do      ! End of BGSector Loop

!*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
   do i = 1, NUMBGSects
      if (L_BGFile(i)) then
         if (fulldate /= fullbghr(i)) then
!*             WRITE Error Message - Date mismatch
            write(dummy,'(I10.10,''s'',I1)') fulldate, i
            call errhdl(path,modnam,'E','454',dummy)
            runerr = .true.
         end if
      end if
   end do

   if (runerr) return

   if (BGHrVal < 0.0d0) then
! ---    Hourly BGCONC is missing; look for substitution values
      if (ibgsect > 0) then
! ---       Valid BGSECT defined, check for hourly values for this
!           sector, and then for non-hourly values to substitute
         if (L_BGFile(ibgsect)) then
            if (L_BGValues(ibgsect)) then
!                 Hourly background value is missing but non-hourly
!                 values have been specified for substitution,
!                 which were processed in subroutine BGVAL;
               bgconc = bgsub(ibgsect)
!                 Write informational message
               write(dummy,'(I10.10,''s'',I1)') fulldate, ibgsect
               call errhdl(path,modnam,'I','453',dummy)
!                 Increment counter for number of missing BGval substitutions
               NSubBGHOUR = NSubBGHOUR + 1
            else
!                 Hourly background value is missing for this sector and no
!                 non-hourly values specified for substitution;
!                 Write Error message
               write(dummy,'(I10.10,''s'',I1)') fulldate, ibgsect
               call errhdl(path,modnam,'E','452',dummy)
               runerr = .true.
               go to 1000
            end if
         else
            if (L_BGValues(ibgsect)) then
!                 Hourly background value is missing but non-hourly
!                 values have been specified for substitution,
!                 which were processed in subroutine BGVAL;
               bgconc = bgsub(ibgsect)
            end if
         end if
      else
! ---       IBGSECT = 0 due to calm or msg hr; BGSUB = 0.0D0; exit
         bgconc = 0.0d0
      end if
   else
      bgconc = BGHrVal
   end if

   return

!*    Write Error Message for Error Reading Hourly Background File
99 continue
   L_ReadErr = .true.

   return

   continue
!     End-of-file reached, set logical flag
   eof = .true.

! --- End of file reached on hourly BACKGRND file; check FULLDATE vs. FULLBGHR
!     before returning
   do i = 1, NUMBGSects
      if (L_BGFile(i)) then
         if (fulldate /= fullbghr(i)) then
!*             WRITE Error Message - Date mismatch
            write(dummy,'(I10.10,''S'',I1)') fulldate, i
            call errhdl(path,modnam,'E','454',dummy)
            runerr = .true.
         end if
      end if
   end do

1000 return
end subroutine bgext

subroutine noxext(L_ReadErr,ReadErr,EndErr)
!***********************************************************************
!*                  NOXEXT Module of AERMOD
!*
!*         PURPOSE: To extract hourly NOx data for GRSM options
!*
!*         PROGRAMMER:  CERC
!*
!*         DATE:    November 2020
!*
!*         INPUTS:
!*
!*         OUTPUTS:
!*
!*         CALLED FROM:  HRLOOP
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   character :: ReadErr*5, EndErr*5

   double precision :: noxsub(6), noxtemp, noxmin

   integer :: inoxyr, inoxmn, inoxdy, inoxhr, inoxyr2, i
   integer :: fullnoxhr(6)
   logical :: L_ReadErr

!*    Variable Initializations
   modnam  = 'NOXEXT'
   ReadErr = ''
   EndErr  = ''
   L_ReadErr = .false.
   fullnoxhr(:) = 0

! --- Initialize logical for missing data to FALSE
   noxmiss = .false.

!*    Assign INOXLINE counter to ILINE for passing to ERRHDL if needed
   iline = inoxline
!*
! --- Read a record in all hourly NOx files, but only read concentation
!     based on the applicable file for the current sector
!     First initialize NOXBGCONC, NOXMIN to 0.0, and NOXTEMP, NOXSUB to  -99.
   noxbgconc   = 0.0d0
   noxtemp   = -99.0d0
   noxsub(:) = -99.0d0
   noxmin    = 0.0d0

   do i = 1, NUMNOxSects
! ---    Loop through NOXSECTORs

! ---    Reinitialize NOXSUB for this sector
      noxsub(i) = -99.0d0

! ---    Check for non-hourly NOX values to substitute for missing data
      if (l_nox_vals(i)) then
         call varynoxvals(i,noxsub(i))
      else if (l_noxvalue(i)) then
         noxsub(i) = noxback(i)
      else
         noxsub(i) = 0.0d0
      end if

! ---    Check for hour NOXFILE for this sector
      if (L_NOxFile(i)) then

! ---       Hourly NOx file available for current sector

         if (i == inoxsect) then
! ---          This is the applicable sector for this hour; read next hour of NOx data

! ---          Initialize ReadErr and EndErr for this sector
            write(ReadErr,'(''SECT'',I1)') i

            if (noxform(i) == 'FREE') then
               read(inoxunt(i),*,err=99,end=9991) inoxyr, inoxmn,&
               &inoxdy, inoxhr,&
               &noxbgconc
            else
               read(inoxunt(i),noxform(i),err=99,end=9991)&
               &inoxyr, inoxmn,&
               &inoxdy, inoxhr,&
               &noxbgconc
            end if

!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
            if (inoxyr <= 99) then
               call cent_date(inoxyr2,inoxyr)
            end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---          Check for use of 2-digit year in NOX_FILE file, adjust to 4-digit
!C              year for comparison with FULLDATE based on met data file
!               IF (INOXYR .LE. 99) THEN
!                  INOXYR2 = INOXYR
!                  IF (INOXYR2 .GE. ISTRT_WIND .and.
!     &                                 INOXYR2 .LE. 99) THEN
!                     INOXYR  = ISTRT_CENT*100 + INOXYR2
!                  ELSE IF (INOXYR2 .LT. ISTRT_WIND) THEN
!                     INOXYR  = (ISTRT_CENT+1)*100 + INOXYR2
!                  END IF
!               END IF

! ---          Calculate full date for this hour of NOx data
            fullnoxhr(i) = inoxyr*1000000 + inoxmn*10000 + inoxdy*100&
            &+ inoxhr

            if(noxbgconc >= 0.0d0)then
! ---             Valid hourly value; convert to ug/m3 if needed
! ---             using NO2 factors (NOx expressed as 'NOx as NO2')
               if (noxfilunits == 'PPB') then
                  noxbgconc = noxbgconc / no2_ppb
               else if (noxfilunits == 'PPM') then
                  noxbgconc = noxbgconc / no2_ppm
               end if
               !Ensure non-negative
               noxbgconc = max( noxbgconc, noxmin )
            else if (l_nox_vals(inoxsect) .or.&
            &l_noxvalue(inoxsect)) then
! ---             Hourly NOx value is missing; assign NOXSUB value based on
!                 NOX_VALS or NOXVALUE inputs
               noxbgconc = noxsub(inoxsect)
            else
! ---             Assign NOXMISS logical to TRUE
               noxmiss = .true.
            end if

            go to 9992

9991        continue
!              End-of-file reached, set logical flag
            eof = .true.

            write(EndErr, '(''SECT'',I1)') i

9992        continue

         else
! ---          This is not applicable sector for this hour; read record with temp data

! ---          Initialize ReadErr and EndErr for this sector
            write(ReadErr,'(''SECT'',I1)') i

            if (noxform(i) == 'FREE') then
               read(inoxunt(i),*,err=99,end=9993) inoxyr, inoxmn,&
               &inoxdy, inoxhr,&
               &noxtemp
            else
               read(inoxunt(i),noxform(i),err=99,end=9993)&
               &inoxyr, inoxmn,&
               &inoxdy, inoxhr,&
               &noxtemp
            end if
!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
            if (inoxyr <= 99) then
               call cent_date(inoxyr2,inoxyr)
            end if
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C ---          Check for use of 2-digit year in OZONEFIL file, adjust to 4-digit
!C              year for comparison with FULLDATE based on met data file
!               IF (INOXYR .LE. 99) THEN
!                  INOXYR2 = INOXYR
!                  IF (INOXYR2 .GE. ISTRT_WIND .and.
!     &                                 INOXYR2 .LE. 99) THEN
!                     INOXYR  = ISTRT_CENT*100 + INOXYR2
!                  ELSE IF (INOXYR2 .LT. ISTRT_WIND) THEN
!                     INOXYR  = (ISTRT_CENT+1)*100 + INOXYR2
!                  END IF
!               END IF

! ---          Calculate full date for this hour of O3 data
            fullnoxhr(i) = inoxyr*1000000 + inoxmn*10000 + inoxdy*100&
            &+ inoxhr

            if (noxtemp >= 0.0d0) then
! ---             Valid hourly value; convert to ug/m3 if needed
! ---             using NO2 factors (NOx expressed as 'NOx as NO2')
               if (noxfilunits == 'PPB') then
                  noxtemp = noxtemp / no2_ppb
               else if (noxfilunits == 'PPM') then
                  noxtemp = noxtemp / no2_ppm
               end if
            else if (l_nox_vals(i) .or.&
            &l_noxvalue(i)) then
! ---             Hourly NOx value is missing; assign NOXSUB value;
!                 these have already been converted to ug/m3
               noxtemp = noxsub(i)
            end if

            go to 9994

9993        continue
!              End-of-file reached, set logical flag
            eof = .true.

            write(EndErr, '(''SECT'',I1)') i

9994        continue

         end if

      end if

   end do   ! END of NOxSector Loop

   if (noxmiss) then
! ---    No NOx value available for this hour; assign 0.0 to NOXCONC
!        and issue informational message
      noxbgconc = 0.0d0
      if (.not. L_SkipMessages) then
         write(dummy,'(I10.10)') fulldate
         call errhdl(path,modnam,'I','609',dummy)
      end if
   end if

!*    Check for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
   do i = 1, NUMNOxSects
      if (L_NOxFile(i)) then
         if (fulldate /= fullnoxhr(i)) then
!*             WRITE Error Message - Date mismatch
            write(dummy,'(I10.10,''S'',I1)') fulldate, i
            call errhdl(path,modnam,'E','608',dummy)
!*             Set RUNERR logical and skip to end
            runerr = .true.
            go to 1000
         end if
      end if
   end do

!     Date/Time consistency checks were ok; skip to end
   go to 1000

!*    Write Error Message for Error Reading Hourly NOx File
99 continue
   L_ReadErr = .true.
   write(ReadErr,'(''SECT'',I1)') i
   call errhdl(path,modnam,'E','510',ReadErr)
   runerr = .true.

   go to 1000

!RCO 3/4/2021 Label Unused
! 999  CONTINUE
!     End-of-file reached, set logical flag
   eof = .true.

! --- End of file reached on NOX file; check FULLDATE vs. FULLNOXHR before returning
!*    for Date and Time Consistency Across all Sectors; If Failed, Issue Fatal Error
   do i = 1, NUMNOxSects
      if (L_NOXFile(i)) then
         if (fulldate /= fullnoxhr(i)) then
!*             WRITE Error Message - Date mismatch
            write(dummy,'(I10.10,''S'',I1)') fulldate, i
            call errhdl(path,modnam,'E','608',dummy)
            runerr = .true.
         end if
      end if
   end do

1000 return
end subroutine noxext

subroutine errhdl(pathwy,modnam,inertp,inercd,inpmsg)
!***********************************************************************
!                 ERRHDL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: A General Error Handling Procedure
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Sets upper limit on line number included in error
!                   message to avoid overflowing the field; also increased
!                   field length for last message field from 8 to 12 to
!                   accommodate 12 character source IDs.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        INPUTS:  Error Code, Occur Locations
!
!        OUTPUTS: Error Message, Error Statistics..etc.
!
!        CALLED FROM:  (This Is An Utility Programm)
!***********************************************************************
!
!     Variable Declarations
   use main1
   implicit none

   integer :: i, iline_prt
   character :: errmg1*50, pathwy*2, inertp*1, inercd*3, icode*3,&
   &inpmsg*(*), modnam*(*), tmpmod*6, tmpmsg*12
   logical :: found

!     Variable Initializations
   ierror = ierror + 1
   found = .false.
   i = 1

!     Check for Occurrence of 'E' Error Type, and Set FATAL Switch
   if (inertp == 'E') then
      fatal = .true.
      nfatal = nfatal + 1
      if (nfatal == 999) then
!           Number Of Fatal Errors Has Reached Limit of 999
         errmg1 = 'Number of Fatal Errors Has Reached Limit of 999'
         tmpmod = 'ERRHDL'
         icode  = '999'
         tmpmsg = ' '
         iline_prt = min(iline,99999999)
         write(ierunt,1111) pathwy,inertp,icode,iline_prt,tmpmod,&
         &errmg1,tmpmsg
         go to 999
      else if (nfatal > 999) then
!           Skip Any More Error WRITEs
         go to 999
      end if
   end if

!     Go To Match The Error Massage
   do while (.not.found .and. i<=ierrn)
      if (inercd == errcod(i)) then
         errmg1 = errmsg(i)
         found = .true.
      end if
      i = i + 1
   end do

   if (.not. found) then
      write(errmg1,1001)
1001  format('SYSTEM ERROR: MESSAGE NOT FOUND FOR THIS NUMBER!')
   end if

! --- Set upper limit on ILINE to avoid write error
   iline_prt = min(iline,99999999)
!     Write Out The Error Message
   write(ierunt,1111) pathwy,inertp,inercd,iline_prt,&
   &modnam(1:min(len_trim(modnam),12)),errmg1,&
   &inpmsg(1:min(len_trim(inpmsg),12))
1111 format(a2,1x,a1,a3,i8,1x,a12,': ',a50,1x,a12)

999 return
end subroutine errhdl

subroutine terrst
!***********************************************************************
!                 TERRST Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Determine Total Error/Message Statistics
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Corrected issues with determining number of calm
!                   and/or missing hours only for the data period that
!                   is processed.  Also increased field length for last
!                   message field from 8 to 12 to accommodate 12 character
!                   source IDs.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:  To remove reference to legacy ISCST3 option (HE>ZI)
!                   for plume height above mixing height.
!                   Determine number of calm and/or missing hours only
!                   for the data period processed, and determine number
!                   of hours processed.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        INPUTS:  Error Message Temporary File
!
!        OUTPUTS: Total Number of Messages by Message Type
!
!        CALLED FROM:  This is A Utility Program
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: ierrln
!     JAT D065 7/22/21 ICHR, ICDAT8 NOT USED
!      INTEGER :: ICYR, ICMN, ICDY, ICHR, ICDAT, ICDAT8, ICJDY
   integer :: icyr, icmn, icdy, icdat, icjdy
   character :: errtp*1, errcd*3, errmg1*50, errmg2*12, inpfld3*3
   character :: inpfld12*12

!     Variable Initialization
   modnam = 'TERRST'
   iftl = 0
   iwrn = 0
   info = 0
   iclm = 0
   imsg = 0
   icyr = 0
   icmn = 0
   icdy = 0
!     JAT D065 7/22/21 ICHR NOT USED
!      ICHR = 0
   icdat = 0
!     JAT D065 7/22/21 ICDAT8 NOT USED
!      ICDAT8 = 0
   icjdy  = 0
   dnum   = 0.0d0
   eof = .false.

!     Rewind the Temporary Error/Message File
   rewind ierunt

   do while (.not. eof)
      read(ierunt,1116,end=99,err=9999) path,errtp,errcd,ierrln,&
      &modnam,errmg1,errmg2

!        Convert error code from character string to number
      inpfld3 = errcd
      call stonum(inpfld3,3,fnum,imit)

      if (errtp == 'E') then
         iftl = iftl + 1
      else if (errtp == 'W') then
         iwrn = iwrn + 1
      else if (errtp == 'I') then
         info = info + 1
         if (nint(fnum) == 440) then
! ---          Determine if this calm hour is during period
!              of data processed; convert date field from
!              character string to number (using double precision)
            inpfld12 = errmg2
            call stodbl(inpfld12,12,dnum,imit)
            icdat = idnint(dnum)
            icyr  = icdat/1000000
            if (rstinp .or. imstat(6)==0 .or.&
            &(icdat>=isdate .and. icdat<=iedate) ) then
! ---             This hour is between start and end dates,
!                 or this is a restarted model run, now
!                 determine Julian day and check IPROC array
!                 for DAYRANGE.
               icmn = (icdat/10000) - (icdat/1000000)*100
               icdy = (icdat/100) - (icdat/10000)*100
               if (icmn>0 .and. icdy>0) then
                  call julian(icyr,icmn,icdy,icjdy)
               else
                  icjdy = 0
                  cycle
               end if
               if (iproc(icjdy) == 1) then
! ---                Message for Calm Hour, Increment Calm Counter
                  iclm = iclm + 1
               end if
            end if
         else if (nint(fnum) == 460) then
! ---          Determine if this missing hour is during period
!              of data processed;  convert date field from
!              character string to number (using double precision)
            inpfld12 = errmg2
            call stodbl(inpfld12,12,dnum,imit)
            icdat = idnint(dnum)
            icyr   = icdat/1000000
            if (rstinp .or. imstat(6)==0 .or.&
            &(icdat>=isdate .and. icdat<=iedate) ) then
! ---             This hour is between start and end dates,
!                 or this is a restarted model run, now
!                 determine Julian day and check IPROC array
!                 for DAYRANGE.
               icmn = (icdat/10000) - (icdat/1000000)*100
               icdy = (icdat/100) - (icdat/10000)*100
               if (icmn>0 .and. icdy>0) then
                  call julian(icyr,icmn,icdy,icjdy)
               else
                  icjdy = 0
                  cycle
               end if
               if (iproc(icjdy) == 1) then
! ---                Message for Missing Hour, Increment Missing Counter
                  imsg = imsg + 1
               end if
            end if
         end if
      end if

      go to 11
99    eof = .true.
11    continue
   end do

1116 format(a2,1x,a1,a3,i8,1x,a12,2x,a50,1x,a12)

!     Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF;
!     This Is Needed in Order To Allow For Additional Message Writes
   backspace ierunt

   go to 1000

!     WRITE Error Message: Error Reading Temp Error Message File
9999 call errhdl(path,modnam,'E','510','ERRORMSG')

1000 return
end subroutine terrst

subroutine sumtbl(iount)
!***********************************************************************
!                 SUMTBL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Print Out The Error Summary Table
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Increased field length for last message field from
!                   8 to 12 to accommodate 12 character source IDs.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:  To remove reference to legacy ISCST3 option (HE>ZI)
!                   for plume height above mixing height.
!                   Include the number of hours processed from the
!                   meteorological data file.
!                   R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        INPUTS:  Error Message Temporary File
!
!        OUTPUTS: Summary Of Errors
!
!        CALLED FROM:  This is A Utility Program
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   real    :: percent
   integer :: j, ierrln, iount
   character :: errtp*1, errcd*3, errmg1*50, errmg2*12

!     Variable Initialization
   modnam = 'SUMTBL'

!     Write Out The Total Error Statistics
   write(iount,*) ' --------- Summary of Total Messages --------'
   write(iount,*) ' '
   write(iount,9014) iftl
9014 format(' A Total of   ',i10,' Fatal Error Message(s)')
   write(iount,9015) iwrn
9015 format(' A Total of   ',i10,' Warning Message(s)')
   write(iount,9016) info
9016 format(' A Total of   ',i10,' Informational Message(s)')
   if (ntothrs > 0) then
      write(iount,90171) ntothrs
90171 format(/,' A Total of   ',i10,' Hours Were Processed')
      write(iount,9017) iclm
9017  format(/,' A Total of   ',i10,' Calm Hours Identified')
!        Calculate percentage of missing hours, and check for > 10 percent.
      percent = 100. * (float(imsg)/float(ntothrs))
      write(iount,9018) imsg, percent
9018  format(/,' A Total of   ',i10,' Missing Hours Identified (',&
      &f6.2,' Percent)')
      if (NSubBGHOUR > 0) then
         write(iount,99018) NSubBGHOUR
99018    format(/,' A Total of   ',i10,' Missing Hourly BACKGRND ',&
         &'Values Substituted')
      end if
      if (percent > 10.0) then
         write(iount,9019)
9019     format(/,' CAUTION!:  Number of Missing Hours Exceeds 10 ',&
         &'Percent of Total!',/,12x,'Data May Not Be ',&
         &'Acceptable for Regulatory Applications.',/,12x,&
         &'See Section 5.3.2 of "Meteorological Monitoring ',&
         &'Guidance',/,12x,'for Regulatory Modeling ',&
         &'Applications" (EPA-454/R-99-005).')
      end if
! ---    Output total precipipation if wet deposition algorithms are used
      if (wdplete .or. depos .or. wdep) then
         write(iount,9020) total_precip, total_precip/25.4d0
9020     format(/,' Met Data File Includes ',f10.2,' Millimeters (',&
         &f10.3,' Inches) of Precipitation')
      end if
   end if
   write(iount,*) ' '

!     Write Out All The Fatal Error Messages
   write(iount,*) ' '
   write(iount,*) '   ******** FATAL ERROR MESSAGES ******** '
   rewind ierunt
   eof = .false.
   j = 0
   do while (.not. eof)
      read(ierunt,1116,end=99,err=9999) path,errtp,errcd,ierrln,&
      &modnam,errmg1,errmg2
      if (errtp == 'E') then
         j = j + 1
         write(iount,1117) path,errtp,errcd,ierrln,modnam(1:12),&
         &errmg1,errmg2
      end if
      go to 11
99    eof = .true.
11    continue
   end do

!     If No Fatal Error Messages, Then Write 'NONE'
   if (j == 0) then
      write(iount,*) '              ***  NONE  ***         '
      write(iount,*) ' '
   end if

!     Write Out All The Warning Messages
   write(iount,*) ' '
   write(iount,*) '   ********   WARNING MESSAGES   ******** '
   rewind ierunt

! CRT, 12/9/2021  D036 Warnings - Comment EOF = True inside loop -
!     prematurely sets EOF flag to force exit loop
!     Update code needs to read to end of file to position
!     cursor for next write statement.
   eof = .false.
   j = 0
!CRT      DO WHILE (.NOT. EOF)
!CRT         READ(IERUNT,1116,END=999,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
!CRT     &                                      MODNAM,ERRMG1,ERRMG2
!CRT         IF (ERRTP .EQ. 'W') THEN
!CRT            J = J + 1
!CRT            IF (.NOT. NOWARN) THEN
!CRT               IF (J .LE. 999) THEN
!CRT                  WRITE(IOUNT,1117) PATH,ERRTP,ERRCD,IERRLN,
!CRT     &                              MODNAM(1:12),ERRMG1,ERRMG2
!CRT               ELSE
!CRT                  WRITE(IOUNT,*) 'More Than 999 Warning Messages ',
!CRT     &                           'Found.  See ERRORFIL Output for',
!CRT     &                           ' the Remainder.'
!CRT                  EOF = .TRUE.
!CRT               END IF
!CRT            END IF
!CRT         END IF
!CRT         GO TO 111
!CRT 999     EOF = .TRUE.
!CRT 111     CONTINUE
!CRT      END DO
   do while (.not. eof)
      read(ierunt,1116,end=999,err=9999) path,errtp,errcd,ierrln,&
      &modnam,errmg1,errmg2
      if (errtp == 'W') then
         j = j + 1

         if (.not. nowarn) then
            if (j <= 999) then
               write(iount,1117) path,errtp,errcd,ierrln,&
               &modnam(1:12),errmg1,errmg2
            end if
            if (j == 999) then
               write(iount,*) 'More Than 999 Warning Messages ',&
               &'Found.  See ERRORFIL Output for',&
               &' the Remainder.'
            end if
         end if
      end if
      go to 111
999   eof = .true.
111   continue
   end do

!     If No Warning Messages, Then Write 'NONE'
   if (j == 0) then
      write(iount,*) '              ***  NONE  ***        '
      write(iount,*) ' '
   else if (nowarn) then
      write(iount,*) ' ** WARNINGS SUPPRESSED BY NOWARN OPTION **'
      write(iount,*) ' '
   end if

1116 format(a2,1x,a1,a3,i8,1x,a12,2x,a50,1x,a12)
1117 format(1x,a2,1x,a1,a3,i8,1x,a12,': ',a50,1x,a12)

!     Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF;
!     This Is Needed in Order To Allow For Additional Message Writes
   backspace ierunt

   go to 1000

!     WRITE Error Message: Error Reading Temp Error Message File
9999 call errhdl(path,modnam,'E','510','ERRORMSG')

1000 return
end subroutine sumtbl

subroutine msgwrt
!***********************************************************************
!                 MSGWRT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To Print Out The Error Summary Table
!
!        PROGRAMMER: Jeff Wang
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Error Message File
!
!        OUTPUTS: The Error Message File
!
!        CALLED FROM:  This is A Utility Program
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: ierrln
   character :: errtp*1, errcd*3, errmg1*50, errmg2*12

!     Variable Initialization
   modnam = 'MSGWRT'

!     Write Out The Header Of The Message File
   write(ierwrt,*) ' '
   write(ierwrt,*) '   ************ Error Message List *************'
   write(ierwrt,*) ' '
   write(ierwrt,*) '   PW     --- Pathway                           '
   write(ierwrt,*) '   Code   --- Error Type + Error Code           '
   write(ierwrt,*) '   L#     --- The Line Number Where Error Occurs'
   write(ierwrt,*) '   ModNam --- Module Name In Which Error Occurs '
   write(ierwrt,*) '   Hints  --- Hints For The Possible Solution   '
   write(ierwrt,*) '   *********************************************'
   write(ierwrt,*) ' '
   write(ierwrt,1114)
   write(ierwrt,1115)
1114 format('PW CODE    L#      MODNAM     ',18x,'ERROR MESSAGES',22x,&
   &'HINTS')
1115 format('-- ---- ------- ------------- ',50('-'),' ------------')
   write(ierwrt,*) ' '
   rewind ierunt
   eof = .false.

   do while (.not. eof)
      read(ierunt,1116,end=99,err=999) path,errtp,errcd,ierrln,&
      &modnam,errmg1,errmg2
      write(ierwrt,1117) path,errtp,errcd,ierrln,&
      &modnam(1:12),errmg1,errmg2
      go to 11
99    eof = .true.
11    continue
   end do

1116 format(a2,1x,a1,a3,i8,1x,a12,2x,a50,1x,a12)
1117 format(a2,1x,a1,a3,i8,1x,a12,': ',a50,1x,a12)

   go to 1000

!     WRITE Error Message: Error Reading Temp Error Message File
999 call errhdl(path,modnam,'E','510','ERRORMSG')

1000 return
end subroutine msgwrt

subroutine pnpoly (px,py,x,y,n,inout)
!----------------------------------------------------------------------
!     Courtesy: Jay Sandhu
!               email: jsandhu@esri.com
!
!
! Please cite David H. Douglas, COLLECTED ALGORITHMS, Cambridge MA:
! Harvard Laboratory for Computer Graphics, 1974
!
! This is my reinvention buster.
! 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974
!
!>>>PNPY
!     .................................................................
!
!        SUBROUTINE PNPOLY
!
!        PURPOSE
!           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
!
!        USAGE
!           CALL PNPOLY (PX, PY, X, Y, N, INOUT )
!
!        DESCRIPTION OF THE PARAMETERS
!           PX      - X-COORDINATE OF POINT IN QUESTION.
!           PY      - Y-COORDINATE OF POINT IN QUESTION.
!           X       - N LONG VECTOR CONTAINING X-COORDINATES OF
!                     VERTICES OF POLYGON.
!           Y       - N LONG VECTOR CONTAINING Y-COORDINATES OF
!                     VERTICES OF POLYGON.
!           N       - NUMBER OF VERTICES IN THE POLYGON.
!           INOUT   - THE SIGNAL RETURNED:
!                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
!                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
!                      1 IF THE POINT IS INSIDE OF THE POLYGON.
!
!        REMARKS
!           THE VERTICES MAY BE LISTED IN CLOCKWISE OR ANTICLOCKWISE
!           ORDER.  FOR THIS SUBROUTINE A POINT IS CONSIDERED INSIDE
!           THE POLYGON IF IT IS LOCATED IN THE ENCLOSED AREA DEFINED
!           BY THE LINE FORMING THE POLYGON.
!           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
!           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
!           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
!           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
!           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
!           PNPOLY CAN HANDLE ANY NUMBER OF VERTICES IN THE POLYGON.
!           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 6/72.
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           A VERTICAL SEMI-INFINITE LINE IS DRAWN UP FROM THE POINT
!           IN QUESTION. IF IT CROSSES THE POLYGON AN ODD NUMBER OF
!           TIMES, THE POINT IS INSIDE THE POLYGON.
!
! --- Modified to use an Internal Function for EOR rather than a
!     Statement Function, which has been identified as obsolescent
! --- in Fortran 95.  R.W. Brode, EPA/OAQPS/AQMG, 10/19/2009
!     .................................................................
!

   implicit none

   integer :: i, j, n, inout
   double precision :: x(n), y(n), xi, yi, xj, yj, px, py
   logical :: ix, iy
   logical :: jx, jy
   logical :: l_eor

   l_eor = .false.

   inout=-1

   do i=1,n
      xi=x(i)-px
      yi=y(i)-py
!        CHECK WHETHER THE POINT IN QUESTION IS AT THIS VERTEX.
      if (xi==0.0d0 .and. yi==0.0d0) then
         inout = 0
         exit
      end if
!        J IS NEXT VERTEX NUMBER OF POLYGON.
      j=1+mod(i,n)
      xj=x(j)-px
      yj=y(j)-py
!        IS THIS LINE OF 0 LENGTH ?
      if (xi==xj .and. yi==yj) cycle
      ix=xi>=0.0d0
      iy=yi>=0.0d0
      jx=xj>=0.0d0
      jy=yj>=0.0d0
!        CHECK WHETHER (PX,PY) IS ON VERTICAL SIDE OF POLYGON.
      l_eor = eor(iy,jy)
      if (xi==0.0d0 .and. xj==0.0d0 .and. l_eor) then
         inout = 0
         exit
      end if
!        CHECK WHETHER (PX,PY) IS ON HORIZONTAL SIDE OF POLYGON.
      l_eor = eor(ix,jx)
      if (yi==0.0d0 .and. yj==0.0d0 .and. l_eor) then
         inout = 0
         exit
      end if
!        CHECK WHETHER BOTH ENDS OF THIS SIDE ARE COMPLETELY 1) TO RIGHT
!        OF, 2) TO LEFT OF, OR 3) BELOW (PX,PY).
      l_eor = eor(ix,jx)
      if (.not.((iy.or.jy).and.l_eor)) cycle
!        DOES THIS SIDE OBVIOUSLY CROSS LINE RISING VERTICALLY FROM (PX,PY)
      l_eor = eor(ix,jx)
      if (.not.(iy.and.jy.and.l_eor)) then
         if ((yi*xj-xi*yj)/(xj-xi) < 0.0d0) then
            cycle
         else if ((yi*xj-xi*yj)/(xj-xi) == 0.0d0) then
            inout = 0
            exit
         else
            inout = -inout
         end if
      else
         inout = -inout
      end if

   end do

!     "EXCLUSIVE OR" Internal FUNCTION, EOR:
contains
   logical function eor(ix,iy)
      logical :: ix, iy
      eor = (ix.or.iy) .and. .not.(ix.and.iy)
   end function eor

end subroutine pnpoly

subroutine allsetup
!***********************************************************************
!                 ALLSETUP Module
!
!        PURPOSE: Allocate Array Storage for SETUP
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    September 21, 1996
!
!        MODIFIED:  To include ARISE variables and allocate memory
!                   for these (Aircraft Plume Rise).
!                   Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                   04/01/2023
!
!        MODIFIED:  To include RLINEXT variable BDW_FLAG and allocate
!                   memory for it.
!                   Dianna Francisco & David Heist, EPA ORD, 2/12/21
!
!        MODIFIED:  To include arrays for GRSM NO2 option
!                   CERC, 11/30/20
!
!        MODIFIED:  To include RLINE parameters, MOVES units conversion,
!                   and temporary source and receptor coordinates
!                   for a RLINE source
!                   Wood, 07/20/18
!
!        MODIFIED:  To include CHIBL, PARTCH, buoyant line parameters,
!                   and source and receptor coordinates in a rotated
!                   system for a buoyant line source
!                   Amec Foster Wheeler, 06/30/15
!
!        MODIFIED:  To include ADSFACT, AWDSIN, and AWDCOS.
!                   R. W. Brode, MACTEC (f/k/a PES), Inc., 08/02/05
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!        CALLED FROM:  MAIN
!
!        ERROR HANDLING:   Checks for error allocating arrays
!***********************************************************************
!
!     Variable Declarations
   use main1
   use rline_data, only: rlsource, xrcp_rot, yrcp_rot, xsb_rot,&
   &ysb_rot, xse_rot, yse_rot, nrlines,&
   &rlfirsthr, rlemisconv, rlmovesconv,&
   &bdw_flag
   use buoyant_line
   implicit none
   character :: modnam*12

   integer :: iastat

!     Variable Initializations
   modnam = 'ALLSET'
   alloc_err = .false.

   if (NUMBGsects < 1) NUMBGsects = 1
   if (NUMO3sects < 1) NUMO3sects = 1
   if (NUMNOxsects < 1) NUMNOxsects = 1

   allocate  (kave(nave), chrave(nave), chidep(6,ntyp),&
   &outtyp(ntyp),stat=iastat)
   if (iastat /= 0) then
      call errhdl(path,modnam,'E','409','Setup Arrays')
      alloc_err = .true.
   end if
!CRT  D063 Add platform downwash arrays (PLATELV, PLATHB, PLATWB
!CRT       OSPLAT, SOPLAT
   allocate  (axs(nsrc), ays(nsrc), azs(nsrc), aqs(nsrc),&
   &ahs(nsrc), ats(nsrc), avs(nsrc), ads(nsrc),&
   &platelv(nsrc), plathb(nsrc), platwb(nsrc),&
   &osplat(nsrc), soplat(nsrc),&
   &asyini(nsrc), aszini(nsrc), afp(nsrc),&
   &axs1(nsrc), ays1(nsrc), axs2(nsrc), ays2(nsrc),&
   &awidth(nsrc), adsfact(nsrc), ndxstk(nsrc),&
   &awdsin(nsrc), awdcos(nsrc), aafv(nsrc),&
   &inpd(nsrc), eval(nsrc), urbsrc(nsrc),&
   &l_hrlysig(nsrc), l_flatsrc(nsrc),&
   &l_method2(nsrc), L_WakeMessage(nsrc),&
   &igroup(nsrc,ngrp), srcid(nsrc), srctyp(nsrc),&
   &sopcrd(nsrc), sogas(nsrc), o3vary(no3f,NUMO3sects),&
   &grpid(ngrp), qflag(nsrc), emilbl(ntyp),&
   &outlbl(ntyp), perlbl(ntyp), backgrnd(nbf,NUMBGsects),&
   &emifac(ntyp), grp_back(ngrp),&
   &noxvary(nnoxf,NumNOxsects),&
! Added for HBP, JAN 2023
   &hbpsrc(nsrc),&
! End HBP insert
   &stat=iastat)

!**  Added for Aircraft Plume Rise; UNC-IE
   allocate (aftsrc(nsrc), amfuel(nsrc), athrust(nsrc),&
   &avaa(nsrc), aafr(nsrc),&
   &abypr(nsrc), arpwr(nsrc),&
   &asrcangle(nsrc), stat=iastat)
!**  End Aircraft Plume Rise insert; April 2023

!     AECOM 4/13/2022 D113 Added for SIDEWASH
!WSP D174 - WSP changed NSWP to NSRC
!WSP      ALLOCATE (ABW(NSWP), ABL(NSWP), ABH(NSWP), ABA(NSWP),
!WSP     &          SWXS(NSWP), SWYS(NSWP), STAT=IASTAT)
   allocate (abw(nsrc), abl(nsrc), abh(nsrc), aba(nsrc),&
   &swxs(nsrc), swys(nsrc), stat=iastat)
!     end insert for SIDEWASH

   if (iastat /= 0) then
      call errhdl(path,modnam,'E','409','Setup Arrays')
      alloc_err = .true.
      write(iounit,*) '  Error Occurred During Allocation of ',&
      &'Basic Source Arrays!'

   end if

   if (nsec > 0) then
      allocate  (adsbh(nsec,nsrc), adsbw(nsec,nsrc),&
      &adsbl(nsec,nsrc), adsxadj(nsec,nsrc),&
      &adsyadj(nsec,nsrc),&
      &stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Source Downwash Arrays!'

      end if
   end if

   if (nqf > 0) then
      allocate  (qfact(nqf,nsrc), stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Source Emission Factor Arrays!'
      end if

   end if

   if (npdmax > 0) then
      allocate  (apdiam(npdmax,nsrc), aphi(npdmax,nsrc),&
      &apdens(npdmax,nsrc), avgrav(npdmax,nsrc),&
      &atstop(npdmax,nsrc),&
      &efrac(npdmax), qpart(npdmax),&
      &pdiam(npdmax), phi(npdmax), pdens(npdmax),&
      &vgrav(npdmax), tstop(npdmax), schmidt(npdmax),&
      &vdep(npdmax), scf(npdmax), wqcor(npdmax),&
      &dqcor(npdmax), pscvrt(npdmax), washout(npdmax),&
      &ecoll(npdmax), finemass(nsrc),&
      &stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Source Particle Deposition Arrays!'
      end if

   end if

   allocate  (pdiff(nsrc), pdiffw(nsrc), rmolwt(nsrc), alphas(nsrc),&
   &react(nsrc), henry(nsrc), rcli(nsrc),&
   &stat=iastat)
   if (iastat /= 0) then
      call errhdl(path,modnam,'E','409','Setup Arrays')
      alloc_err = .true.
      write(iounit,*) '  Error Occurred During Allocation of ',&
      &'Gas Deposition Arrays!'
   end if

! --- Allocate arrays for AREA sources based on NVMAX+1 to address issues with AREAPOLY sources
   if (nvmax > 0) then
      allocate  (axinit(nsrc), ayinit(nsrc), aangle(nsrc),&
      &axvert(nvmax+1,nsrc), ayvert(nvmax+1,nsrc),&
      &uvert(nvmax+1), vvert(nvmax+1), vnvert(nvmax+1),&
      &wvert(nvmax+1), uasegs(nvmax+1), ubsegs(nvmax+1),&
      &xvert(nvmax+1), yvert(nvmax+1),&
      &spa(nvmax+1,2),&
      &aalpha(nsrc), apdeff(nsrc), avolum(nsrc),&
      &radius(nsrc), nverts(nsrc), axcntr(nsrc),&
      &aycntr(nsrc),&
      &stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Area Source Arrays!'
      end if
   end if

   if (nrlines > 0) then
      allocate(rlsource(nsrc), xsb_rot(nsrc), ysb_rot(nsrc),&
      &xse_rot(nsrc), yse_rot(nsrc), rlemisconv(nsrc),&
      &bdw_flag(nsrc,2),stat=iastat)
!        Initialize MOVES input unit flag and FIRSTHR flag
      rlmovesconv = .false.
      rlfirsthr = .true.
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'RLINE Temporary Source Arrays!'
      end if
      allocate(xrcp_rot(nrec), yrcp_rot(nrec), stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'RLINE Temporary Receptor Arrays!'
      end if
   endif

! (Multiple_BuoyLines_D41_Wood)
!     Changed variable name NBLINES to NBLP
! (Buoyant Line Source Limit D_088)
!     Made array DEL allocatable

! --- Buoyant line allocation
!     NBLP = number of individual buoyant lines
   if (nblp > 0) then
      allocate (blineparms(nblp), xs_scs(nblp,129),&
      &ys_scs(nblp), xs_rcs(nblp,129),&
      &ys_rcs(nblp,129),del(nblp), stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Buoyant Line Arrays!'
      end if

      allocate (chibl(nrec), stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Concentration Arrays for Buoyant Line!'
      end if

! (Multiple_BuoyLines_D41_Wood)
!        Added dimensions to arrays for multiple buoyant lines;
!        also changed some scalar values to 1-D arrays
      allocate (xr_scs(nrec,nblgrp), yr_scs(nrec,nblgrp),&
      &xr_rcs(nrec), yr_rcs(nrec), bl_rflag(nrec,nblgrp),&
      &stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Receptor Arrays for Buoyant Line!'
      end if

      allocate (blavginp_grpid(NBLAVGINPalloc),&
      &blavginp_llen(NBLAVGINPalloc),&
      &blavginp_bwid(NBLAVGINPalloc),&
      &blavginp_bhgt(NBLAVGINPalloc),&
      &blavginp_bsep(NBLAVGINPalloc),&
      &blavginp_lwid(NBLAVGINPalloc),&
      &blavginp_fprm(NBLAVGINPalloc),&
      &nblingrp(nblgrp),angrad(nblgrp),&
      &hrlyblcount(nblgrp), xor(nblgrp), yor(nblgrp),&
      &tcor(nblgrp), sintcor(nblgrp), costcor(nblgrp),&
      &igrp_blp(nsrc,nblgrp), bl_grpid(nblgrp),&
      &l_blurban(nblgrp), bl_numurb(nblgrp),&
      &stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Arrays for Buoyant Line Groups!'
      end if
   end if

   if (nurb > 0) then
      allocate  (iurbgrp(nsrc,nurb), urbid(nurb), urbnam(nurb),&
      &urbpop(nurb), urbz0(nurb),&
      &ziurb(nurb), urbwstr(nurb), urbustr(nurb),&
      &urbobulen(nurb),&
      &grdswu(mxglvl,nurb), grdsvu(mxglvl,nurb),&
      &grdtgu(mxglvl,nurb), grdptu(mxglvl,nurb),&
      &L_MorningTrans(nurb),&
      &stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Urban Arrays!'
      end if
   end if

!**  Added for Aircraft Plume Rise; UNC-IE
   if (naft > 0) then
      allocate  (aftid(naft),stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Aircraft Arrays!'
      end if
   end if
!**  End Aircraft Plume Rise insert, April 2023

   if (evonly) then
      allocate  (ev_hrqs(nsrc,nhr), ev_hrts(nsrc,nhr),&
      &ev_hrvs(nsrc,nhr), ev_hrhs(nsrc,nhr),&
      &ev_hrsy(nsrc,nhr), ev_hrsz(nsrc,nhr),&
      &ev_hrfp(nsrc,nhr),&
      &evaper(neve), evdate(neve), evjday(neve),&
      &idxev(neve), axr(neve), ayr(neve), azelev(neve),&
      &azflag(neve), azhill(neve), evname(neve),&
      &evgrp(neve), EV_OrigConc(neve), stat=iastat)
!**  Added for Aircraft Plume Rise; UNC-IE
      allocate (ev_hrmfuel(nsrc,nhr), ev_hrthrust(nsrc,nhr),&
      &ev_hrvaa(nsrc,nhr), ev_hrafr(nsrc,nhr),&
      &ev_hrbypr(nsrc,nhr), ev_hrrpwr(nsrc,nhr),&
      &ev_hrsrcangle(nsrc,nhr), stat=iastat)
!**  End Aircraft Plume Rise insert; April 2023

      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'EVENT Processing Arrays!'
      end if
   end if

   if (.not. evonly) then
      allocate  (axr(nrec), ayr(nrec), azelev(nrec),&
      &azflag(nrec), azhill(nrec), iref(nrec),&
      &netid(nrec), rectyp(nrec),&
      &ndxarc(nrec), arcid(narc),&
      &ntid(nnet), nttyp(nnet),&
      &xcoord(ixm,nnet), ycoord(iym,nnet),&
      &xorig(nnet), yorig(nnet),&
      &netsta(nnet), netend(nnet),&
      &numxpt(nnet), numypt(nnet),&
!                   AECOM 4/13/2022 D113 Added for Sidewash
      &swxr(nrec), swyr(nrec),&
!                   End Sidewash insert
      &stat=iastat)

      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Receptor Arrays!'
      end if
   end if

   allocate  (nhiave(nval,nave), maxave(nave), imxval(nave),&
   &idytab(nave), maxfle(ngrp,nave),&
   &ipstfl(ngrp,nave), ipltfl(nval,ngrp,nave),&
   &ianpst(ngrp), ianplt(ngrp), inhi(nave),&
   &itoxfl(nave), irnkfl(nave), irkval(nave),&
   &thresh(ngrp,nave), toxthr(nave),&
   &imxunt(ngrp,nave), ipsunt(ngrp,nave),&
   &ipsfrm(ngrp,nave), iplunt(nval,ngrp,nave),&
   &iapunt(ngrp), ianfrm(ngrp), ippunt(ngrp),&
   &itxunt(nave), irkunt(nave), ielunt(nsrc),&
   &thrfil(ngrp,nave), pstfil(ngrp,nave),&
   &pltfil(nval,ngrp,nave), annpst(ngrp),&
   &annplt(ngrp), toxfil(nave), rnkfil(nave),&
   &evlfil(nsrc), iseahr(ngrp), seahrs(ngrp),&
   &ishunt(ngrp), imxdly(ngrp), imdunt(ngrp),&
   &maxdly(ngrp), imxdly_byyr(ngrp),&
   &imdunt_byyr(ngrp), maxdly_byyr(ngrp),&
   &maxdcont(ngrp), imxdcunt(ngrp),&
   &maxdcont_file(ngrp), mxd_rank(ngrp,2),&
   &maxd_thresh(ngrp),&
   &stat=iastat)
   if (iastat /= 0) then
      call errhdl(path,modnam,'E','409','Setup Arrays')
      alloc_err = .true.
      write(iounit,*) '  Error Occurred During Allocation of ',&
      &'Output Option Arrays!'
   end if

   allocate  (idconc(nave,npair), txconc(nave,npair), stat=iastat)
   if (iastat /= 0) then
      call errhdl(path,modnam,'E','409','Setup Arrays')
      alloc_err = .true.
      write(iounit,*) '  Error Occurred During Allocation of ',&
      &'TOXXFILE Arrays!'
   end if

   allocate  (workid(nsrc+1), iwrk2(nsrc,13), stat=iastat)
   if (iastat /= 0) then
      call errhdl(path,modnam,'E','409','Setup Arrays')
      alloc_err = .true.
      write(iounit,*) '  Error Occurred During Allocation of ',&
      &'Temporary Source Arrays!'
   end if

   if (.not. evonly) then
      allocate  (zetmp1(nrec), zetmp2(nrec),&
      &zhtmp1(nrec), zhtmp2(nrec),&
      &zftmp1(nrec), zftmp2(nrec), stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Temporary Receptor Arrays!'
      end if
   end if

   if (pvmrm .or. olm .or. arm2&
   &.or. runttrm .or. grsm) then
      allocate (ano2_ratio(nsrc), chi(nrec,nsrc,ntyp), stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Setup Arrays')
         alloc_err = .true.
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'PVMRM/OLM/ARM2/TTRM/GRSM CHI Array!'
      end if

      if (pvmrm .or. grsm) then
         allocate (hecntr(nrec,nsrc), hecntr3(nrec,nsrc),&
         &ueffs(nrec,nsrc), ueff3s(nrec,nsrc),&
         &epsef(nrec,nsrc), epsef3(nrec,nsrc),&
         &fopts(nrec,nsrc), ppfact(nrec,nsrc), stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'PVMRM/GRSM Source Data Arrays!'
         end if
      end if

      if (olm) then
         allocate (olmid(nolm), l_olmgrp(nsrc),&
         &igrp_olm(nsrc,nolm), stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'OLMGROUP Source Data Arrays!'
         end if
      end if

!     added for TTRM; AECOM
      if (runttrm) then
         allocate (ttrmout(nrec,nsrc,25), stat=iastat)
         allocate (ttrmsrc(nrec,nsrc), stat=iastat) ! TTRMSRC: reports stability & plume type for debug file
         allocate (ttrminst(ntyp), ttrmfrac(ntyp), ttrmsub(ntyp),&
         &ttrmno2(ntyp), l_ttrmsrctyp(nsrc),&
         &ttrmfrac_prm(ntyp),&
         &ttrmcompare(ngrp,nsrc,nrec,ntyp),&
         &ttrmfrac_aer(ntyp), stat=iastat)
!      Add a logical warning array L_TTRMSRCTYP for source types not currently configured for TTRM
!     end TTRM insert; Feb. 2021
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'TTRM Source Data Arrays!'
         end if
      end if

      if (psdcredit) then
         allocate (psdsrctyp(nsrc), psdid(npsd), l_psdgrp(nsrc),&
         &igrp_psd(nsrc,npsd),&
         &abval(nrec,ntyp), bcval(nrec,ntyp), stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'PSDCREDIT Source Data Arrays!'
         end if
      end if

!        CERC 11/30/20
      if (grsm) then
         allocate (chi_ttravchm(nrec,nsrc), stat=iastat)
         if (iastat /= 0)then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'GRSM Travel Time Arrays!'
         end if
         allocate (bldfac(nrec,nsrc), stat=iastat)
         if (iastat /= 0)then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'GRSM Building Factor Array!'
         end if
         allocate (ttravchm(nrec), stat=iastat)
         if (iastat /= 0)then
            call errhdl(path,modnam,'E','409','Setup Arrays')
            alloc_err = .true.
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'GRSM Travel Time Arrays!'
         end if
         !Initialisations
         chi_ttravchm(:,:)=0.0d0
         bldfac(:,:)=0.0d0
         ttravchm(:)=0.0d0
      end if

   end if

   return
end subroutine allsetup

subroutine allresult
!***********************************************************************
!                 ALLRESULT Module
!
!        PURPOSE: Allocate Array Storage for Results
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:       September 21, 1996
!
!        MODIFIED    Added storage requirement estimate and allocations
!                    for ARISE (Aircraft Plume Rise) option.
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED    Added storage requirement estimate and allocations
!                    for GRSM NO2 option.
!                    CERC, 11/30/20
!
!        MODIFIED:   Modified subroutine ALLRESULT to eliminate the arrays of
!                    profile met data (the observed data in the PROFFILE input)
!                    by hour-of-year, level, and year for use with the MAXDCONT
!                    option. The profile met data arrays are not needed for the
!                    MAXDCONT option and their removal reduces the memory
!                    requirements for that option. Additional adjustments to the
!                    array allocations for the MAXDCONT option were made based
!                    on the options used for a specific model run in order to
!                    minimize the memory requirements for the MAXDCONT option.
!                    Subroutine ALLRESULT was also modified to improve the
!                    accuracy of the memory storage estimates included on the
!                    first page of the AERMOD.OUT file.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
!
!        MODIFIED:   Added calculation of STORE, estimated memory
!                    storage requirements, to report if allocation
!                    errors occur.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!                    Changed parameter for allocating the number of
!                    high annual/period averages from NHIVAL to NHIANN.
!                    R.W. Brode, PES, Inc.,  4/3/98
!
!        INPUTS:
!
!
!        OUTPUTS:
!
!        CALLED FROM:  MAIN
!
!        ERROR HANDLING:   Checks for error allocating arrays
!***********************************************************************
!
!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12

   integer :: iastat
!     Declare Real Variables used to estimate memory requirements
   real    :: rsrc, rsec, rgrp, rrec, rurb, rarc, rave,&
!     JAT D065 7/22/21 REVE NOT USED
!     &           RHIVAL, RTYP, RMAXVAL, RNET, RXM , RYM , REVE, ROLM,
   &rhival, rtyp, rmaxval, rnet, rxm , rym , rolm,&
   &rpsd, rqf, rpdmax, rvmax, rpair, rhiann, rhimxdly,&
!     JAT D065 7/22/21 RBLP NOT USED
!     &           RYEARS, RBLP                                           ! D41_Wood
   &ryears,&                                         ! D41_Wood
   &raft                                                   ! Added for Aircraft Plume Rise; UNC-IE

!     Variable Initializations
   modnam = 'ALLRESULT'
   alloc_err = .false.

!     NARC was initially set to NREC prior to SETUP, now set NARC = NUMARC
   narc = numarc

! --- Assign maximum value from IRKVAL array for the OU RANKFILE option
!     and from the IMXVAL array for the OU MAXTABLE option to NMXVAL
   nmxval = max( maxval(irkval), maxval(imxval) )

!     Assign array limits to REAL for calculation of STORE
   rsrc   = real(nsrc)
   rsec   = real(nsec)
   rgrp   = real(ngrp)
   rrec   = real(nrec)
   rurb   = real(nurb)
   rarc   = real(narc)
   rave   = real(nave)
   rhival = real(nhival)
   rtyp   = real(ntyp)
   rmaxval= real(nmxval)
   rnet   = real(nnet)
   rxm    = real(ixm )
   rym    = real(iym )
!     JAT 7/22/21 REVE NOT USED
!      REVE   = REAL(NEVE)
   rolm   = real(nolm)
   rpsd   = real(npsd)
!     JAT D065 7/22/21 RBLP NOT USED
!      RBLP   = REAL(NBLGRP)                      ! (Multiple_BuoyLines_D41_Wood)
   rqf    = real(nqf)
   rpdmax = real(npdmax)
   rvmax  = real(nvmax)
   rpair  = real(npair)
   rhiann = real(nhiann)
   ryears = real(nyears)
   rhimxdly = real(nhimxdly)
   raft     = real(naft)                      ! Added for Aircraft Plume Rise; UNC-IE

!     Added for TTRM, AECOM; For use in the MAXDCONT array allocation
   if (runttrm) then
      rolm = real(1)
   end if
!     End of TTRM insert, Feb. 2021

   store = 0.0
   if (.not. evonly) then
!        Calculate Approximate Allocated Storage Requirements
      store = rsrc*(54.+rqf+5.*rsec+5.*rpdmax+2.*rvmax+&
!     &           0.5*(RGRP+RURB))+
      &0.5*(rgrp+rurb+raft))+&         !  Added for Aircraft Plume Rise; UNC-IE
      &rpdmax*14. +&
      &rrec*(9.+rhival*rgrp*rave*rtyp*1.75+rgrp*rave*rtyp+&
      &2.*rgrp*rtyp+rgrp) +&
      &rarc*20. + rnet*(9.+rxm+rym) +&
      &rhival*(rgrp*rave*rtyp*3.)+&
      &rmaxval*(rgrp*rave*rtyp*2.25) +&
      &rhiann*1.5*rgrp*rtyp +&
      &rave*(12.+2.*rpair+3.*rhival*rgrp+8.*rgrp) +&
      &rgrp*11. + rtyp*38. + rvmax*20.
      if (seasonhr) then
         store = store + ( 4.*24.*rrec*rgrp*rtyp )
      end if
      if (pvmrm .or. olm .or. runttrm .or.&
      &grsm .or. runttrm2) then
         !RSRC is for ANO2_RATIO, RSRC*RREC*RTYP is for CHI
         store = store + ( rsrc + rsrc*rrec*rtyp )
         if (pvmrm) then
            store = store + ( 8.*rsrc*rrec )
            if (psdcredit) then
               store = store + ( rsrc*(2.+rpsd) + rrec*2.*rtyp )
            end if
         else if (olm .or. runttrm) then
            store = store + ( 0.5*rolm*rsrc + rolm + rsrc )
         else if (grsm) then
            !For HECNTR, UEEFS, EPSEF etc:
            store = store + ( 8.*rsrc*rrec )
            !RREC is for TTRAVCHEM and 2*RREC*RSRC is for CHI_TTRAVCHM and BLDFAC:
            store = store + rrec + (2.*rrec*rsrc)
            !For arrays allocated in GRSM_CALC:
            store = store + ( 6.*rsrc )
         end if
      end if
      if (pm25ave .or. no2ave .or. so2ave) then
         store = store + rrec*rgrp*(1.5+1.5*rhimxdly+&
         &1.5*rhimxdly*ryears)
      end if
      if (l_maxdcont) then
         store = store + ryears*(19.*8784.+10.*8784.+&
         &7.*8784.*real(mxglvl))
         if (ldgas.or.ldpart.or.lwgas.or.lwpart.or.grsm) then
            store = store + ryears*14.*8784.
         end if
         if (nsec > 0) then
            store = store + ryears*8784.*real(mxglvl)
         end if
         if (pvmrm .or. grsm) then
            store = store + ryears*8784.*real(mxglvl)
         end if
         if (pvmrm .or. olm .or. runttrm .or. grsm) then
            !For Ozone background
            store = store + ryears*8784.
         end if
         if(grsm)then
            !For ANOXBGCONC
            store = store + ryears*8784.
         end if
         if (l_backgrnd) then
            store = store + ryears*8784.
         end if
         if (hourly) then
            store = store + ryears*real(2*8784)*rsrc
            if (npnt > 0) then
               store = store + ryears*real(2*8784)*rsrc
            end if
            if (nvol > 0 .or. nvmax > 0) then
               store = store + ryears*real(2*8784)*rsrc
            end if
         end if
         if (nurb > 0) then
            store = store + real(4*8784*mxglvl*nyears*nurb) +&
            &real(4*8784*mxglvl*nyears) +&
            &real(5*8784*nyears*nurb)
         end if
      end if
      store = store*8./1.048576e6 + 3.5
   end if

   allocate  (hrval(numtyp), aerval(numtyp), prmval(numtyp),&
   &stat=iastat)

! --- Allocate BACKAVE and BACKANN arrays to store total background contribution to GRP value
   allocate  (backave(numgrp), backann(numgrp), stat=iastat)

   if (narc > 0) then
      allocate  (arcmax(narc), qmax(narc), dxmax(narc), umax(narc),&
      &svmax(narc), swmax(narc), symax(narc), sy3mx(narc),&
      &u3max(narc), hemax(narc), arccl(narc), szmax(narc),&
      &chidmw(narc), chinmw(narc), chi3mw(narc),&
      &chidml(narc), chinml(narc), chi3ml(narc),&
      &hsblmx(narc),&
      &stat=iastat)
   end if

   if (.not. evonly) then
      allocate  (aveval(numrec,numgrp,numave,numtyp),&
      &hivalu(numrec,nhival,numgrp,numave,numtyp),&
      &hmax(nhival,numgrp,numave,numtyp),&
      &hmloc(nhival,numgrp,numave,numtyp),&
      &hmdate(nhival,numgrp,numave,numtyp),&
      &nhidat(numrec,nhival,numgrp,numave,numtyp),&
      &stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Result Array')
         alloc_err = .true.
         write(iounit,*) ' '
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Short Term Average Results Arrays!'
      end if

      if (period .or. annual) then
         allocate  (annval(numrec,numgrp,numtyp),&
         &amxval(nhiann,numgrp,numtyp),&
         &imxloc(nhiann,numgrp,numtyp),&
         &stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Result Array')
            alloc_err = .true.
            write(iounit,*) ' '
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'Long Term Average Results Arrays!'
         end if
      end if

      allocate  (rmxval(nmxval,numgrp,numave,numtyp),&
      &mxdate(nmxval,numgrp,numave,numtyp),&
      &mxloca(nmxval,numgrp,numave,numtyp),&
      &numhrs(numave), numclm(numave), nummsg(numave),&
      &stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Result Array')
         alloc_err = .true.
         write(iounit,*) ' '
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Overall Maximum Results Arrays!'
      end if

      if (seasonhr) then
         allocate (shvals(numrec,numgrp,4,24,numtyp),&
         &stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Result Array')
            alloc_err = .true.
            write(iounit,*) ' '
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'SEASONHR Results Array!'
         end if

         if (l_backgrnd) then
            allocate (backseashr(numgrp,4,24),stat=iastat)
            if (iastat /= 0) then
               call errhdl(path,modnam,'E','409','Result Array')
               alloc_err = .true.
               write(iounit,*) ' '
               write(iounit,*) '  Error Occurred During Allocation ',&
               &'of SEASONHR BACKGRND Array!'
            end if
         end if
      end if

      if (pm25ave .or. no2ave .or. so2ave) then
         nhimxdly = max( nhimxdly, nhival )
         allocate (mxdval(numrec,numgrp),&
         &himxdly(numrec,numgrp,nhimxdly),&
         &himxdly_byyr(numrec,numgrp,nhimxdly,nyears),&
         &imxdhr(numrec,numgrp),&
         &nhidatmxd(numrec,numgrp,nhimxdly),&
         &nhidatmxd_byyr(numrec,numgrp,nhimxdly,nyears),&
         &stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Result Array')
            alloc_err = .true.
            write(iounit,*) ' '
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'MAXDAILY Results Arrays!'
         end if
      end if

      allocate  (hclmsg(numrec,nhival,numgrp,numave,numtyp),&
      &mclmsg(nmxval,numgrp,numave,numtyp),&
      &hmclm(nhival,numgrp,numave,numtyp),stat=iastat)
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Result Array')
         alloc_err = .true.
         write(iounit,*) ' '
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'High Value Result Flag Arrays!'
      end if

      if (annual) then
         allocate  (sumann(numrec,numgrp,numtyp),&
         &stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Result Array')
            alloc_err = .true.
            write(iounit,*) ' '
            write(iounit,*) '  Error Occurred During Allocation of ',&
            &'ANNUAL Results Arrays!'
         end if
      end if

      if (pm25ave .or. no2ave .or. so2ave) then
         allocate  (sumhnh(numrec,numgrp,nhival),&
         &mxpmval(nmxpm,numgrp,nhival),&
         &mxpmloc(nmxpm,numgrp,nhival),&
         &stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Result Array')
            alloc_err = .true.
            write(iounit,*) ' '
            if (pm25ave) then
               write(iounit,*) '  Error Occurred During Allocation',&
               &' of PM-2.5 24-hr Results Arrays!'
            else if (no2ave) then
               write(iounit,*) '  Error Occurred During Allocation',&
               &' of NO2 1-hr Results Arrays!'
            else if (so2ave) then
               write(iounit,*) '  Error Occurred During Allocation',&
               &' of SO2 1-hr Results Arrays!'
            end if
         end if
      end if

   end if

   if (evonly) then
      allocate  (asfchf(nhr,1), auref(nhr,1),&
      &aurefht(nhr,1), ata(nhr,1),&
      &atrefht(nhr,1), awdref(nhr,1),&
      &austar(nhr,1), awstar(nhr,1),&
      &aziconv(nhr,1), azimech(nhr,1),&
      &aobulen(nhr,1), avptgzi(nhr,1),&
      &asfcz0(nhr,1), abowen(nhr,1),&
      &aalbedo(nhr,1), awnew(nhr,1),&
      &awold(nhr,1), aesta(nhr,1),&
      &akst(nhr,1), ablta(nhr,1),&
      &af2(nhr,1), aprec1(nhr,1), aqsw(nhr,1),&
      &aprate(nhr,1), arh(nhr,1), asfcp(nhr,1),&
      &aprec2(nhr,1), iapcode(nhr,1), nacloud(nhr,1),&
      &aclmhr(nhr,1), amsghr(nhr,1),&
      &aunstab(nhr,1), astable(nhr,1),&
      &aurbstab(nhr,1),&
      &anplvls(nhr,1), antglvl(nhr,1),&
      &ao3conc(nhr,1), abgconc(nhr,1),&
      &anoxbgconc(nhr,1),&
      &aaqs(nhr,1,nsrc), aahs(nhr,1,nsrc),&
      &aavs(nhr,1,nsrc), aats(nhr,1,nsrc),&
      &aafp(nhr,1,nsrc),&
      &aasyini(nhr,1,nsrc), aaszini(nhr,1,nsrc),&
      &aiflag(nhr,mxplvl,1),&
      &apflht(nhr,mxplvl,1), apflwd(nhr,mxplvl,1),&
      &apflws(nhr,mxplvl,1), apflta(nhr,mxplvl,1),&
      &apflsa(nhr,mxplvl,1), apflsw(nhr,mxplvl,1),&
      &apflsv(nhr,mxplvl,1), apfltg(nhr,mxplvl,1),&
      &apfltgz(nhr,mxplvl,1),&
      &ev_aveval(nsrc), hrvals(nhr,nsrc),&
      &grpval(ngrp,nhr), backhr(ngrp,nhr),&
      &grpave(ngrp),&
      &stat=iastat)
! Added for HBP; Jan. 2023
      if (hbplume) then
         allocate  (aziconvn(nhr,1), azimechn(nhr,1),&
         &stat=iastat)
      endif
! End HBP insert
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Result Array')
         alloc_err = .true.
         write(iounit,*) ' '
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'Event Arrays!'
      end if
   else if (l_maxdcont) then
      allocate (asfchf(8784,nyears), auref(8784,nyears),&
      &aurefht(8784,nyears), ata(8784,nyears),&
      &atrefht(8784,nyears), awdref(8784,nyears),&
      &austar(8784,nyears), awstar(8784,nyears),&
      &aziconv(8784,nyears), azimech(8784,nyears),&
      &aobulen(8784,nyears), avptgzi(8784,nyears),&
      &asfcz0(8784,nyears),&
      &akst(8784,nyears), ablta(8784,nyears),&
      &aclmhr(8784,nyears), amsghr(8784,nyears),&
      &aunstab(8784,nyears), astable(8784,nyears),&
      &aurbstab(8784,nyears),&
      &antglvl(8784,nyears),&
      &agridht(8784,mxglvl,nyears),&
      &agridwd(8784,mxglvl,nyears),&
      &agridws(8784,mxglvl,nyears),&
      &agridsw(8784,mxglvl,nyears),&
      &agridsv(8784,mxglvl,nyears),&
      &agridtg(8784,mxglvl,nyears),&
      &agridpt(8784,mxglvl,nyears),&
      &auatzi(8784,nyears),&
      &asvatzi(8784,nyears),&
      &aswatzi(8784,nyears),&
      &auavg(8784,nyears),&
      &asvavg(8784,nyears),&
      &aswavg(8784,nyears),&
      &aptatzi(8784,nyears),&
      &andx4zi(8784,nyears),&
      &arurustr(8784,nyears),&
      &arurobulen(8784,nyears),&
      &stat=iastat)
! Added for HBP; Jan. 2023
      if (hbplume) then
         allocate  (aziconvn(8784,nyears), azimechn(8784,nyears),&
         &stat=iastat)
      endif
! End HBP insert
      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Result Array')
         alloc_err = .true.
         write(iounit,*) ' '
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'MAXDCONT Arrays!'
      end if

! ---    Allocate arrays for NO2 options, PVMRM, OLM, GRSM, or TTRM
      if (pvmrm .or. grsm) then
         allocate( agrideps(8784,mxglvl,nyears),&
         &ao3conc(8784,nyears), stat=iastat )
      end if
      if (olm) then
         allocate( ao3conc(8784,nyears), stat=iastat )
      end if
      if(grsm)then
         allocate( anoxbgconc(8784,nyears), stat=iastat )
      end if
      if (runttrm) then
         allocate( ao3conc(8784,nyears), stat=iastat )
      end if
      if (l_backgrnd) then
         allocate( abgconc(8784,nyears),stat=iastat )
      end if
      if (nsec > 0) then
         allocate( agridrho(8784,mxglvl,nyears),stat=iastat )
      end if
      if (ldgas .or. ldpart .or. lwpart .or. lwgas .or. grsm) then
         allocate( abowen(8784,nyears),&
         &aalbedo(8784,nyears), awnew(8784,nyears),&
         &awold(8784,nyears), aesta(8784,nyears),&
         &af2(8784,nyears), aprec1(8784,nyears),&
         &aprec2(8784,nyears), aprate(8784,nyears),&
         &arh(8784,nyears), asfcp(8784,nyears),aqsw(8784,nyears),&
         &iapcode(8784,nyears), nacloud(8784,nyears),stat=iastat)
      end if

      if (iastat /= 0) then
         call errhdl(path,modnam,'E','409','Result Array')
         alloc_err = .true.
         write(iounit,*) ' '
         write(iounit,*) '  Error Occurred During Allocation of ',&
         &'MAXDCONT Arrays!'
      end if

      if (hourly) then
         allocate(&
         &aaqs(8784,nyears,nsrc), aahs(8784,nyears,nsrc),&
         &stat=iastat)
         if (npnt > 0) then
            allocate(&
            &aavs(8784,nyears,nsrc), aats(8784,nyears,nsrc),&
            &stat=iastat)
         end if
         if (nvol > 0 .or. nvmax > 0) then
            allocate(&
            &aasyini(8784,nyears,nsrc), aaszini(8784,nyears,nsrc),&
            &stat=iastat)
         end if
         if (nblp > 0) then
            allocate(&
            &aafp(8784,nyears,nsrc), stat=iastat)
         end if
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Result Array')
            alloc_err = .true.
            write(iounit,*) ' '
            write(iounit,*) '  Error Occurred During Allocation',&
            &' of MAXDCONT Arrays for HOUREMIS!'
         end if
      end if

      if (nurb > 0) then
         allocate(&
         &agrdswr(8784,mxglvl,nyears),&
         &agrdsvr(8784,mxglvl,nyears),&
         &agrdtgr(8784,mxglvl,nyears),&
         &agrdptr(8784,mxglvl,nyears),&
         &agrdswu(8784,mxglvl,nyears,nurb),&
         &agrdsvu(8784,mxglvl,nyears,nurb),&
         &agrdtgu(8784,mxglvl,nyears,nurb),&
         &agrdptu(8784,mxglvl,nyears,nurb),&
         &aziurb(8784,nyears,nurb),&
         &aurbwstr(8784,nyears,nurb),&
         &aurbustr(8784,nyears,nurb),&
         &aurbobulen(8784,nyears,nurb),&
         &AL_MorningTrans(8784,nyears,nurb),&
         &stat=iastat)
         if (iastat /= 0) then
            call errhdl(path,modnam,'E','409','Result Array')
            alloc_err = .true.
            write(iounit,*) ' '
            write(iounit,*) '  Error Occurred During Allocation',&
            &' of MAXDCONT Arrays for URBANOPT!'
         end if

      end if
   end if

   return
end subroutine allresult


subroutine datime ( dcall, tcall )
!***********************************************************************
!                 DATIME Module
!
!        PURPOSE: Obtain the system date and time
!
!        PROGRAMMER: Jim Paumier, PES, Inc.
!
!        DATE:    April 15, 1994
!
!        MODIFIED:   Uses Fortran 90 DATE_AND_TIME routine.
!                    R.W. Brode, PES, 8/14/98
!
!        INPUTS:  none
!
!        OUTPUTS: Date and time in character format
!
!        CALLED FROM:  RUNTIME
!***********************************************************************
!
!     Variable Declarations
   implicit none

   character :: dcall*8, tcall*8
   character :: cdate*8, ctime*10, czone*5
   integer :: idatetime(8)
   integer :: iptyr, iptmon, iptday, ipthr, iptmin, iptsec

   dcall = ' '
   tcall = ' '

!     Call Fortran 90 date and time routine
   call date_and_time (cdate, ctime, czone, idatetime)

!     Convert year to two digits and store array variables
   iptyr  = idatetime(1) - 100 * int(idatetime(1)/100)
   iptmon = idatetime(2)
   iptday = idatetime(3)
   ipthr  = idatetime(5)
   iptmin = idatetime(6)
   iptsec = idatetime(7)

!     Write Date and Time to Character Variables, DCALL & TCALL
   write(dcall, '(2(I2.2,"/"),I2.2)' ) iptmon, iptday, iptyr
   write(tcall, '(2(I2.2,":"),I2.2)' ) ipthr, iptmin, iptsec

   return
end subroutine datime

subroutine filopn
!***********************************************************************
!                 FILOPN Module
!
!        PURPOSE: Obtain the system date and time
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    December 6, 1994
!
!        MODIFIED:   Remove non-standard option for
!                    CARRIAGECONTROL='Fortran' to control
!                    page feed in aermod.out file.  ASCII form
!                    feed character is used in subroutine HEADER
!                    to insert page feed instead of using Fortan
!                    carriage control.
!                    R.W. Brode, USEPA/OAQPS/AQMG, October 19, 2009
!
!        INPUTS:  Input filename, INPFIL
!                 Output filename, OUTFIL
!
!        OUTPUTS: Openned files
!
!        CALLED FROM:  HEADER
!
!        ERROR HANDLING:   Checks errors openning files
!***********************************************************************
!
!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   modnam = 'FILOPN'

!     OPEN Input Runstream File, Unit INUNIT=7
   dummy = 'RUN-STRM'
!     JAT 12/14/17 use user-supplied filename
!      OPEN (UNIT=INUNIT,FILE='aermod.inp',ACTION='READ',ERR=99,
   open (unit=inunit,file=inpfil,action='READ',err=99,&
   &status='OLD')

!     OPEN Print Output File, Unit IOUNIT=8
   dummy = 'OUTPUT'
!     JAT 12/14/17 use user-supplied filename
!      OPEN (UNIT=IOUNIT,FILE='aermod.out',
   open (unit=iounit,file=outfil,&
   &err=99,status='REPLACE')

!     Write Out Update to the Screen
   write(*,909)
909 format('+','Now Processing SETUP Information')

   go to 1000

!     WRITE Error Message:  Error Opening File
99 call errhdl('  ',modnam,'E','500',dummy)

!     Check for Error Opening Runstream File and STOP
   if (dummy == 'RUN-STRM') then
      write(*,919)
919   format('+','Error Opening Runstream Input File!  Aborting.')
      stop
   end if

1000 continue

   return
end subroutine filopn

subroutine header(iount)
!***********************************************************************
!                 HEADER Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Control Page Feed and Header Information for
!                 Printed File Output
!
!        PROGRAMMER: Roger Brode, Jeff Wang
!
!        DATE:    September 28, 1993
!
!        MODIFIED:   Use ASCII form feed character [ACHAR(12)] for
!                    page feed in 'aermap.out' file rather then
!                    CARRIAGECONTROL='Fortran', which is not a
!                    standard Fortran option.
!                    Include adjustments to header format for large
!                    page numbers.
!                    Include output file unit argument to support
!                    output to main 'aermod.out' file and to the
!                    optional SUMMFILE.
!                    R.W. Brode, USEPA/OAQPS/AQMG, October 19, 2009
!
!        MODIFIED:   Replace DEPLETE parameter for plume depletion option
!                    with DDPLETE and WDPLETE in the list of model options
!                    for Wet & Dry depletion.
!                    D. Strimaitis, SRC - 11/8/93
!
!        MODIFIED:   Header modified for draft version of model with new
!                    area source and deposition algorithms - 9/28/93
!
!        MODIFIED:   To add DEPLETE parameter for plume depletion option
!                    to the list of model options
!                    D. Strimaitis, SRC - 2/15/93
!
!        INPUTS:  Page Number from COMMON
!
!        OUTPUTS: Page Feed and Header
!
!        CALLED FROM:  (This Is An Utility Program)
!***********************************************************************
!
!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
!RCO 3/4/2021 Removing unused variable
!      INTEGER :: I, J, IOUNT, ILEN
   integer :: j, iount, ilen
   character :: ffeed*1
! Unused: INTEGER :: I

!     Variable Initializations
   modnam = 'HEADER'

!*    FFEED is ASCII form-feed character
   j = 12
   ffeed  = achar(j)

!     Increment Page Number Counter
   if (iount == iounit) then
      ipage = ipage + 1
   else if (iount == isumunt) then
      ipgsum = ipgsum + 1
   end if

!     Write Header to Printed Output File
   write(iount,9028) ffeed, versn, title1(1:68), rundat
   if (iount == iounit) then
!        Adjust format statement based on page number
      if (ipage <= 999) then
         write(iount,9029) c_metver, title2(1:68), runtim, ipage
      else if (ipage <= 99999) then
         write(iount,90291) c_metver, title2(1:68), runtim, ipage
      else if (ipage <= 9999999) then
         write(iount,90292) c_metver, title2(1:68), runtim, ipage
      else
         write(iount,90292) c_metver, title2(1:68), runtim,&
         &min(ipage,99999999)
      end if
   else if (iount == isumunt) then
      write(iount,9029) c_metver, title2(1:68), runtim, ipgsum
   end if
   ilen = len_trim( MODOPS_String )
   if (ilen <= 110) then
      write(iount,9030) MODOPS_String(1:len_trim(MODOPS_String))
   else
      write(iount,9030) MODOPS_String(1:len_trim(MODOPS_String))
      write(iount,9040) MODOPS_String(len_trim(MODOPS_String)+1:)
   end if

9028 format(a1,1x,'*** AERMOD - VERSION ',a6,' ***',3x,'*** ',a68,&
   &' ***',8x,a8)
9029 format(1x,'*** AERMET - VERSION ',a6,' ***',3x,'*** ',a68,' ***',&
   &8x,a8,/t120,'PAGE',i4)
90291 format(1x,'*** AERMET - VERSION ',a6,' ***',3x,'*** ',a68,' ***',&
   &8x,a8,/t118,'PAGE',i6)
90292 format(1x,'*** AERMET - VERSION ',a6,' ***',3x,'*** ',a68,' ***',&
   &8x,a8,/t116,'PAGE',i8)
9030 format(1x,'*** MODELOPTs: ',1x,a:)
9040 format(4x,a:)

   return
end subroutine header

subroutine dcdlat ()
!***********************************************************************
!            DCDLAT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To decode the hemisphere and latitude from
!                 the character variable ALAT (record 1 in scalar file)
!
!        PROGRAMMER: Jim Paumier, PES, Inc.
!
!        DATE:       September 30, 1993
!
!        INPUTS:  ALAT, the character variable latitude from AERMET
!
!        ASSUMPTIONS:  The first field in the first record of the
!                      scalar input file contains the latitude
!
!        OUTPUTS: Hemisphere (NORTH or SOUTH), latitude and sign (TSIGN)
!                 for turning of wind with height
!
!        CALLED FROM:  HRLOOP
!***********************************************************************

!---- Variable declarations
   use main1
   implicit none
   character :: modnam*12

   integer      :: nors, sorn

!---- Data initialization
   modnam = 'DCDLAT'
   path   = 'ME'

!---- Determine if the letter 'N' or 'n' is in the latitude field
   nors = index(alat,'N') + index(alat,'n')

   if( nors /= 0 )then

!        The latitude is in the northern hemisphere; decode the latitude

      tsign = 1.0d0
      read( alat, '(F9.1)',err=1000 ) xlat

!        Write a message if the latitude is too far north

      if( xlat > 90.0d0  .or.  xlat < 0.0d0 )then
!           Write a warning to the user - latitude out-of-range
         call errhdl(path,modnam,'W','381',alat)
      end if

   else

!        The latitude may be in the southern hemisphere

      sorn = index(alat,'S') + index(alat,'s')
      if( sorn /= 0 )then
         tsign = -1.0d0
         read( alat, '(F9.1)',err=1000 ) xlat

         if( xlat > 90.0d0  .or.  xlat < 0.0d0 )then
!              Write a warning to the user - latitude out-of-range
            call errhdl(path,modnam,'W','381',alat)
         end if


      else
!           Write a warning to the user - error decoding the latitude
         call errhdl(path,modnam,'W','382',alat)

      end if

   end if

   go to 999

1000 continue
!     Write a warning to the user - error decoding the latitude
   call errhdl(path,modnam,'W','382',alat)

999 return
end subroutine dcdlat

subroutine preset
!***********************************************************************
!                 PRESET Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Preprocesses SETUP Information to Determine Data
!                 Storage Requirements
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 24, 1996
!
!        MODIFIED:   To include new options related to ARCFTOPT option
!                    (to identify the Aircraft sources).
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   To include new options related to GRSM NO2 option
!                    CERC, 11/30/20
!
!        MODIFIED:   Added new NUMYEARS option to specify the number
!                    of years of met data for allocating arrays for
!                    the MAXDCONT option. The default number of years
!                    is still five (5) years.
!                    Also modified to allow for the use of URBANSRC ALL
!                    on the SO pathway to indicate that all sources are
!                    to be treated as URBAN sources. This option assumes
!                    that only one (1) urban area has been defined using
!                    the CO URBANOPT keyword.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
!
!        MODIFIED:   Added calculation of STORE, estimated memory
!                    storage requirements, to report if allocation
!                    errors occur.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To include new options incorporated in version
!                    dated 06341.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
!
!        MODIFIED:   To check for NO ECHO in the input file.
!                    R.W. Brode, PES, Inc. - 12/2/98
!
!        INPUTS:  Input Runstream File
!
!        OUTPUTS: Array Sizes
!
!        CALLED FROM:   MAIN
!***********************************************************************
!
!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12

   integer :: i, j, k, isprd, ieprd
!     Declare Real Variables used to estimate memory requirements
!     JAT D065 7/22/21 NONE OF THE VARIABLES BELOW ARE USED IN THIS SUBROUTINE
!     THEY ARE ACTUALLY SET IN ALLRESULT
!      REAL    :: RSRC, RSEC, RGRP, RREC, RURB, RARC, RAVE,
!     &           RVAL, RTYP, RMAX, RNET, RXM , RYM , REVE, ROLM, RPSD,
!     &           RQF, RPDMAX, RVMAX, RPAIR, RHIANN

   logical :: nopath, nokey, L_O3data, L_NOxData
   character :: rdfrm*20
   character :: lprd*8, hprd*8, nchr1(10)*8, nchr2(10)*5
   logical :: rmark
! JAT 06/22/21 D065
! REMOVE INPFLD AS UNUSED VARIABLE
!      CHARACTER INPFLD*2, PATHWY(7)*2
   character :: pathwy(7)*2
   interface
      subroutine expath(inpfld,pathwy,ipn,nopath)
         character (len=2), intent(in) :: inpfld
         character (len=2), intent(in), dimension(:) :: pathwy
         integer, intent(in) :: ipn
         logical, intent(out) :: nopath
      end subroutine expath
   end interface

!     Variable Initializations
   data (nchr1(i),i=1,10) /'FIRST','SECOND','THIRD','FOURTH',&
   &'FIFTH','SIXTH','SEVENTH','EIGHTH',&
   &'NINTH','TENTH'/
   data (nchr2(i),i=1,10) /'1ST','2ND','3RD','4TH','5TH',&
   &'6TH','7TH','8TH','9TH','10TH'/

!     Variable Initializations
   modnam = 'PRESET'
   prevsrcid = '        '
   prevgrpid = '        '
   path  = '  '
   ppath = '  '
   eof = .false.
   nsec    = 0
   npdmax  = 0
   nqf     = 0
   nbf     = 0
   nurb    = 0
   nvmax   = 0
   iline   = 0
   naft  = 0                                               ! Added for Aircraft Plume Rise; UNC-IE

!     Initialize PATHWY array
   pathwy(1) = 'CO'
   pathwy(2) = 'SO'
   pathwy(3) = 'RE'
   pathwy(4) = 'ME'
   pathwy(5) = 'OU'
   pathwy(6) = '**'
   pathwy(7) = 'EV'

   ipnum  = 0
   ippnum = 0
!     Counters for the Receptor Groups
   irec = 0
   ista = .false.
   iend = .false.
   newid = .true.
!     Initialize logical for urban option and multiple urban areas
   l_preset_urban = .false.
   l_multurb      = .false.
!     Initialize logical for the 'URBANSRC ALL' option
   l_urban_all = .false.
!**  Added for Aircraft Plume Rise; UNC-IE
!     Initialize logical for aircraft option
   l_preset_arcft = .false.
!     Initialize logical for the 'ARCFTSRC ALL' option
   l_arcft_all = .false.
!**  End Aircraft insert; April 2022
! Added for HBP, JAN 2023
!     Initialize logical for the 'HBPSRC ALL' option
   l_hbp_all = .false.
! End insert for HBP
!     Initialize logical for sector-varying O3 data
   L_O3Sector = .false.
!     Initialize logical for O3 data inputs
   L_O3data = .false.
!     Initialize logical for sector-varying Background data
   L_BGSector = .false.
!     Initialize logical for BG data inputs
   l_backgrnd = .false.
!     Initialize logical for sector-varying NOx data
   L_NOxSector = .false.
!     Initialize logical for NOx data inputs
   L_NOxData = .false.
!     Initialize logical for BUOYLINE source
   l_blsource = .false.
!     Initialize file format to 'FIX'; will be overridden if
!     user specified 'EXP' format on OU FILEFORM keyword
   file_format = 'FIX'

! --- Initialize all NO2 options to .FALSE.
   arm2   = .false.
   olm    = .false.
   pvmrm  = .false.
   grsm  = .false.

! --- Initialize MODOPTS_String character variable
   MODOPS_String = ''

!     Setup READ format and ECHO format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   write(rdfrm,9100) istrg, istrg
9100 format('(A',i4.4,',T1,',i4.4,'A1)')

! --- First loop through Runstream Records to identify
!     O3SECTOR/NOXSECTR option and BGSECTOR/BACKGRND options
!     since they are interdependant;
!     This avoids imposing a requirement on the
!     order of these keywords:
!     LOOP Through Input Runstream Records
   do while (.not. eof)

!        Increment the Line Counter
      iline = iline + 1

!        READ Record to Buffers, as A'num' and 'num'A1 for 'num' = ISTRG.
!        Length of ISTRG is Set in PARAMETER Statement in MAIN1
      read (inunit,rdfrm,end=998) runst1, (runst(i), i = 1, istrg)

!        Check for blank input record and cycle
      if (len_trim(runst1) == 0) cycle

!        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
      call lwrupr

!        Define Fields on Card                              ---   CALL DEFINE
      call define

!        Get the Contents of the Fields                     ---   CALL GETFLD
      call getfld

!        If Blank Line, Then CYCLE to Next Card
      if (bline) go to 10

!        Check for 'NO ECHO' In First Two Fields
      if (field(1) == 'NO' .and. field(2) == 'ECHO') then
!           Skip record with NO ECHO during PRESET stage of processing
         go to 10
      end if

!        Extract Pathway ID From Field 1                    ---   CALL EXPATH
      call expath(field(1),pathwy,7,nopath)

!        For Invalid Pathway and Comment Lines Skip to Next Record
      if (nopath) then
!           Skip Error Message for PRESET stage of processing
         path = ppath
         go to 10
      else if (path == '**') then
         go to 10
      end if

!        Extract Keyword From Field 2                       ---   CALL EXKEY
      call exkey(field(2),nokey)

      if (nokey) then
!           No Keyword - Skip Error Message for PRESET stage
         pkeywd = keywrd
         go to 10
      end if

!        Save Current Path and Path Number as Previous Path and Number
      ppath = path
      ippnum = ipnum

!        First process cards to determine whether O3SECTOR and/or BGSECTOR keywords are used
      if (path == 'CO') then

         if (keywrd == 'O3SECTOR') then
! ---          Assign logical variable for O3SECTORs
            L_O3Sector = .true.
            cycle

         elseif (keywrd == 'OZONEFIL' .or.&
         &keywrd == 'O3VALUES' .or.&
         &keywrd == 'OZONEVAL') then
            L_O3Data = .true.
            cycle

         elseif (keywrd == 'EVENTFIL') then
            cycle

         elseif (keywrd == 'LOW_WIND') then
            low_wind = .true.

!           CERC 11/30/20
         elseif(keywrd == 'NOXSECTR') then
! ---          Assign logical variable for NOxSECTORs
            L_NOxSector = .true.
            cycle

         elseif (keywrd == 'NOX_FILE' .or.&
         &keywrd == 'NOX_VALS' .or.&
         &keywrd == 'NOXVALUE')then
            L_NOxData = .true.
            cycle

         end if

      else if (path == 'SO') then

         if (keywrd == 'BGSECTOR') then
! ---          Assign logical variable for BGSECTORs
            L_BGSector = .true.
            cycle
         else if (keywrd == 'BACKGRND') then
! ---          Assign logical variable for BACKGRND
            l_backgrnd = .true.
            cycle
         else if (keywrd == 'INCLUDED' .and.&
         &(.not.L_BGSector .or. .not.l_backgrnd)) then
! ---          Call PREINCLUD since BGSECTOR and/or BACKGRND, URBANSRC,
!              and/or BLPINPUT may be in an INCLUDED file
            call preinclud
            cycle
         else if (keywrd == 'URBANSRC') then
            L_Urban = .true.
            if (field(3) == 'ALL') then
               L_Rural = .false.
            end if
            cycle

!**  Added for Aircraft Plume Rise; UNC-IE
         else if (keywrd == 'ARCFTSRC') then
            L_Arcft = .true.
            cycle
!**  End Aircraft Plume Rise insert; April 2023

         else if (keywrd == 'BLPINPUT') then
            l_blsource = .true.
            cycle

         else if (keywrd == 'BLPGROUP') then
            l_blsource = .true.               ! (Multiple_BuoyLines_D41_Wood)
            cycle
         end if

! ---       Exit pre-PRESET loop if BGSECTOR and BACKGRND
!           have been specified
         if (L_BGSector .and. l_backgrnd) then
            exit
         end if

      else if (path == 'RE') then
! ---       CO and SO pathways already processed:
!           Exit pre-PRESET loop
         exit

      end if

      go to 10
998   eof = .true.
10    continue
   end do

! --- Check for O3SECTOR keyword without O3 data inputs
   if (L_O3Sector .and. .not.L_O3Data) then
! ----   Issue warning message for O3SECTOR without O3 data
      call errhdl(path,modnam,'W','166','O3SECTOR')
   end if
   if (L_BGSector .and. .not.l_backgrnd) then
! ----   Issue warning message for O3SECTOR without O3 data
      call errhdl(path,modnam,'W','166','BGSECTOR')
   end if
! --- Check for NOXSECTR keyword without NOX data inputs
   if (L_NOXSector .and. .not.L_NOXData) then
! ----   Issue warning message for NOXSECTR without NOX data
      call errhdl(path,modnam,'W','166','NOXSECTR')
   end if

! --- Rewind Input Runstream File for complete pre-setup
   rewind inunit

   path  = '  '
   ppath = '  '
   ipnum  = 0
   ippnum = 0
   eof = .false.

!     LOOP Through Input Runstream Records
   do while (.not. eof)

!        Increment the Line Counter
      iline = iline + 1

!        READ Record to Buffers, as A'num' and 'num'A1 for 'num' = ISTRG.
!        Length of ISTRG is Set in PARAMETER Statement in MAIN1
      read (inunit,rdfrm,end=999) runst1, (runst(i), i = 1, istrg)

!        Check for blank input record and cycle
      if (len_trim(runst1) == 0) cycle

!        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
      call lwrupr

!        Define Fields on Card                              ---   CALL DEFINE
      call define

!        Get the Contents of the Fields                     ---   CALL GETFLD
      call getfld

!        Check for 'NO ECHO' In First Two Fields
      if (field(1) == 'NO' .and. field(2) == 'ECHO') then
!           Skip record with NO ECHO during PRESET stage of processing
         cycle
      end if

!        Extract Pathway ID From Field 1                    ---   CALL EXPATH
      call expath(field(1),pathwy,7,nopath)

!        For Invalid Pathway and Comment Lines Skip to Next Record
      if (nopath) then
!           Skip Error Message for PRESET stage of processing
         path = ppath
         cycle
      else if (path == '**') then
         cycle
      end if

!        Extract Keyword From Field 2                       ---   CALL EXKEY
      call exkey(field(2),nokey)

      if (nokey) then
!           Invalid Keyword - Skip Error Message for PRESET stage
         pkeywd = keywrd
         cycle
      end if

!        Save Current Path and Path Number as Previous Path and Number
      ppath = path
      ippnum = ipnum

!        Process Cards to Determine Storage Requirements
      if (path == 'CO') then
         if (keywrd == 'MODELOPT') then
            do i = 3, ifc
               if (field(i) == 'CONC'  .or.&
               &field(i) == 'DEPOS' .or.&
               &field(i) == 'DDEP'  .or.&
               &field(i) == 'WDEP') then
                  ntyp = ntyp + 1
               end if
!                 Set PVMRM/OLM/ARM2/GRSM/TTRM logicals for use in ALLSETUP
               if (field(i) == 'PVMRM') then
                  pvmrm = .true.
               else if (field(i) == 'PSDCREDIT' )then
                  psdcredit = .true.
!----                Number of "source groups" will be set to 2 below for
!                    PSDCREDIT applications, to account for hardwired
!                    'NAAQS' and 'PSDINC' source groups, otherwise it
!                    would be overwritten in SRCSIZ
               else if (field(i) == 'OLM') then
                  olm = .true.
               else if (field(i) == 'ARM2') then
                  arm2 = .true.
               else if (field(i) == 'GRSM') then
                  grsm = .true.
               else if (field(i) == 'TTRM') then
                  runttrm = .true.
               else if (field(i) == 'TTRM2') then
                  runttrm = .true.
                  runttrm2 = .true.
               end if
!---              Check for ALPHA and BETA options
               if (field(i) == 'ALPHA') then
                  l_alpha = .true.
               else if (field(i) == 'BETA') then
                  beta = .true.
               end if
            end do

         else if (keywrd == 'AVERTIME') then
            do i = 3, ifc
               if (field(i)/='PERIOD' .and.&
               &field(i)/='ANNUAL') then
                  nave = nave + 1
               end if
            end do

         else if (keywrd == 'URBANOPT') then
            nurb = nurb + 1
!----          Set preliminary flag for URBAN option, used to allow flexibility in
!              order of CO pathway keywords for URBANOPT
            l_preset_urban = .true.
            if (nurb > 1) then
               l_multurb = .true.
            end if

!**  Added for Aircraft Plume Rise; UNC-IE
         else if (keywrd == 'ARCFTOPT') then
            naft = naft + 1
            l_preset_arcft = .true.
!**  End Aircraft Plume Rise insert; April 2023

         else if (keywrd == 'O3SECTOR') then
! ---          Assign logical variable for O3SECTORs
            L_O3Sector = .true.
! ---          Set maximum array limit for number of ozone sectors
            NUMO3Sects = ifc - 2

         else if (keywrd == 'O3VALUES') then
! ---          Set maximum array limit for temporally-varying
!              ozone concentrations for O3VALUES keyword
!              Assign field index to 4 if O3SECTOR is used, otherwise 3
            if (L_O3Sector) then
               k = 4
            else
               k = 3
            end if
            if (field(k) == 'ANNUAL') then
               no3f = max( no3f, 1)
            else if (field(k) == 'SEASON') then
               no3f = max( no3f, 4)
            else if (field(k) == 'MONTH') then
               no3f = max( no3f, 12)
            else if (field(k) == 'HROFDY') then
               no3f = max( no3f, 24)
            else if (field(k) == 'WSPEED') then
               no3f = max( no3f, 6)
            else if (field(k) == 'SEASHR') then
               no3f = max( no3f, 96)
            else if (field(k) == 'HRDOW') then
               no3f = max( no3f, 72)
            else if (field(k) == 'HRDOW7') then
               no3f = max( no3f, 168)
            else if (field(k) == 'SHRDOW') then
               no3f = max( no3f, 288)
            else if (field(k) == 'SHRDOW7') then
               no3f = max( no3f, 672)
            else if (field(k) == 'MHRDOW') then
               no3f = max( no3f, 864)
            else if (field(k) == 'MHRDOW7') then
               no3f = max( no3f, 2016)
            end if

!           CERC 11/30/20
         else if (keywrd == 'NOXSECTR') then
! ---          Assign logical variable for NOxSECTORs
            L_NOxSector = .true.
! ---          Set maximum array limit for number of NOx sectors
            NUMNOxSects = ifc - 2

         else if (keywrd == 'NOX_VALS') then
! ---          Set maximum array limit for temporally-varying
!              NOx concentrations for NOX_VALS keyword
!              Assign field index to 4 if NOXSECTR is used, otherwise 3
            if (L_NOxSector) then
               k = 4
            else
               k = 3
            end if
            if (field(k) == 'ANNUAL') then
               nnoxf = max( nnoxf, 1)
            else if (field(k) == 'SEASON') then
               nnoxf = max( nnoxf, 4)
            else if (field(k) == 'MONTH') then
               nnoxf = max( nnoxf, 12)
            else if (field(k) == 'HROFDY') then
               nnoxf = max( nnoxf, 24)
            else if (field(k) == 'WSPEED') then
               nnoxf = max( nnoxf, 6)
            else if (field(k) == 'SEASHR') then
               nnoxf = max( nnoxf, 96)
            else if (field(k) == 'HRDOW') then
               nnoxf = max( nnoxf, 72)
            else if (field(k) == 'HRDOW7') then
               nnoxf = max( nnoxf, 168)
            else if (field(k) == 'SHRDOW') then
               nnoxf = max( nnoxf, 288)
            else if (field(k) == 'SHRDOW7') then
               nnoxf = max( nnoxf, 672)
            else if (field(k) == 'MHRDOW') then
               nnoxf = max( nnoxf, 864)
            else if (field(k) == 'MHRDOW7') then
               nnoxf = max( nnoxf, 2016)
            end if

!           3/18/22 Wood, D127Added FRANMIN to LOW_WIND option
!CRT        4/11/2022 D131 FRAN Alpha - Add momentum balance option
         else if (keywrd == 'LOW_WIND') then
!----          Check for LOW_WIND keyword for users to adjust parameters used
!              with the LOW_WIND ALPHA option
            if( ifc == 3 )then
               L_UserSVmin = .true.
            else if( ifc == 4 )then
               L_UserSVmin = .true.
               L_UserWSmin = .true.
            else if( ifc == 5 )then
               L_UserSVmin = .true.
               L_UserWSmin = .true.
               L_UserFRANmax = .true.
            else if( ifc == 6 )then
               L_UserSVmin = .true.
               L_UserWSmin = .true.
               L_UserFRANmax = .true.
               L_UserSWmin = .true.
            else if( ifc == 7 )then
               L_UserSVmin = .true.
               L_UserWSmin = .true.
               L_UserFRANmax = .true.
               L_UserSWmin = .true.
               L_UserBigT = .true.
            else if( ifc == 8 )then
               L_UserSVmin = .true.
               L_UserWSmin = .true.
               L_UserFRANmax = .true.
               L_UserSWmin = .true.
               L_UserBigT = .true.
               L_UserFRANmin = .true.
            else if( ifc == 9 )then
               L_UserSVmin = .true.
               L_UserWSmin = .true.
               L_UserFRANmax = .true.
               L_UserSWmin = .true.
               L_UserBigT = .true.
               L_UserFRANmin = .true.
               if (field(ifc) == 'PBAL' .or.&
               &field(ifc) == 'PBALANCE') then
                  L_PBal = .true.
               end if
            end if
! ---          Assign LOW_WIND to MODOPS(22)
            modops(22) = 'LOW_WIND'
         end if

      else if (path == 'SO') then
         call srcsiz

      else if (path == 'RE') then
         evonly = .false.
         call recsiz

      else if (path == 'EV') then
         evonly = .true.
         if (keywrd == 'EVENTPER') then
            neve = neve + 1
         else if (keywrd == 'INCLUDED') then
            call preinclud
         end if

      else if (path == 'ME' .and. keywrd == 'SURFDATA') then
!           Read start year from SURFDATA card to establish date window
         call set_window

      else if (path == 'ME' .and. keywrd == 'NUMYEARS') then
! ---       Set number of years for allocating the MAXDCONT arrays
         if (ifc == 3) then
            call stonum(field(3),ilen_fld,fnum,imit)
            if (imit == 1) then
               nyears = nint(fnum)
            end if
         end if

      else if (path == 'OU') then
         if(keywrd == 'RECTABLE') then
!              Begin LOOP Through Fields
            do i = 4, ifc
! ---             Skip processing of fields if IFC > IFMAX
               if (i > ifmax) exit
!                 Retrieve The High Value
               call fsplit(path,keywrd,field(i),ilen_fld,'-',rmark,&
               &lprd,hprd)
               isprd = 0
               ieprd = 0
! ---             First check for simple numeric value
               call stonum(lprd,ilen_fld,fnum,imit)
               if (imit == 1) then
                  isprd = int(fnum)
               end if
               call stonum(hprd,ilen_fld,fnum,imit)
               if (imit == 1) then
                  ieprd = int(fnum)
               end if
! ---             Now check for character strings NCHR1 or NCHR2
               do j = 1, 10
                  if (lprd==nchr1(j) .or.&
                  &lprd==nchr2(j)) isprd = j
                  if (hprd==nchr1(j) .or.&
                  &hprd==nchr2(j)) ieprd = j
               end do
               if (isprd > 999 .or. ieprd > 999) then
!                    Write Error Message:Illegal Parameter Field
                  call errhdl(path,modnam,'E','203','HIVALU')
                  cycle
               end if
               if (isprd > nval) then
                  nval = isprd
               end if
               if (ieprd > nval) then
                  nval = ieprd
               end if
!              End LOOP Through Fields
            end do

         else if (keywrd == 'MAXTABLE' .or.&
         &keywrd == 'RANKFILE') then
!              Set Number of Maximum Values to Sort
            call stonum(field(4),ilen_fld,fnum,imit)
            if (imit /= 1) then
!                 Invalid Numerical Field
               go to 999
            end if
            inum = nint(fnum)
            if (inum > nmxval) then
               nmxval = inum
            end if

         else if (keywrd == 'SEASONHR') then
!              Set SEASONHR logical flag to account for SHVALS array needs
            seasonhr = .true.

         else if (keywrd == 'MAXDCONT') then
!              Set MAXDCONT logical flag to account for MAXDCONT array needs
            l_maxdcont = .true.

         else if (keywrd == 'FILEFORM' .and.&
         &field(3)(1:3) == 'EXP') then
! ---          Check for FILEFORM keyword with 'EXP' format in order to
!              include correct format in output file headers
            file_format = 'EXP'

         else if (keywrd == 'NOHEADER') then
!              Set NOHEADER logical flag to suppress output file headers
            do i = 3, ifc
               if (field(i) == 'ALL') then
!                    No headers for any ouput file type
                  L_NoHeader(:) = .true.
                  exit
               else if (field(i) == 'MAXIFILE') then
!                    No headers for MAXIFILE
                  L_NoHeader(1) = .true.
               else if (field(i) == 'POSTFILE') then
!                    No headers for POSTFILE
                  L_NoHeader(2) = .true.
               else if (field(i) == 'PLOTFILE') then
!                    No headers for PLOTFILE
                  L_NoHeader(3) = .true.
               else if (field(i) == 'SEASONHR') then
!                    No headers for SEASONHR
                  L_NoHeader(4) = .true.
               else if (field(i) == 'RANKFILE') then
!                    No headers for RANKFILE
                  L_NoHeader(5) = .true.
               else if (field(i) == 'MAXDAILY') then
!                    No headers for MAXDAILY
                  L_NoHeader(6) = .true.
               else if (field(i) == 'MXDYBYYR') then
!                    No headers for MXDYBYYR
                  L_NoHeader(7) = .true.
               else if (field(i) == 'MAXDCONT') then
!                    No headers for MAXDCONT
                  L_NoHeader(8) = .true.
               end if
            end do

         else if (keywrd == 'FINISHED') then
!              Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
!              to Statement 999 in Order to Avoid Reading a ^Z "End of File"
!              Marker That May Be Present For Some Editors.
            go to 999

         end if

      end if

!        Store the Current Keyword as the Previous Keyword
      pkeywd = keywrd

      go to 11
999   eof = .true.
11    continue
   end do

!---- Check for PSDCREDIT option and set number of source groups
   if (psdcredit) then
!----    Set number of "source groups" to 2 for PSDCREDIT applications,
!        to account for hardwired 'NAAQS' and 'PSDINC' source groups
      ngrp = 2
   end if

! --- Determine maximum number of vertices for AREA sources, including
!     AREAPOLY and AREACIRC source types, LINE sources, and OPENPIT sources.
   if (narea == 0 .and. nline == 0 .and. npit == 0) then
!        No area, line, or openpit sources, set NVMAX to 0
      nvmax = 0
   else
      if (npoly == 0 .and. ncirc == 0) then
!           No AREAPOLY or AREACIRC sources; initialize NVMAX to 8
!           for rectangular AREA, LINE, and/or OPENPIT sources
         nvmax = 8
      else
!           Assign NVMAX to at least 8 to handle rectangular AREA,
!           LINE, and/or OPENPIT sources
         nvmax = max( nvmax, 8 )
      end if
   end if
!     Calculate value of NVMAX2
   nvmax2 = nvmax*2

!     Rewind File and Reinitialize Line Number Counter for SETUP
   rewind inunit
   iline = 0
   pnetid = '        '

!     Ensure that certain array limits are not < 1.
   nsrc = max( nsrc, 1)
   ngrp = max( ngrp, 1)
   nrec = max( nrec, 1)
!     Set NARC = NREC temporarily for allocating setup arrays
   narc = nrec
   nave = max( nave, 1)
   ntyp = max( ntyp, 1)
   nnet = max( nnet, 1)
   ixm  = max( ixm , 1)
   iym  = max( iym , 1)

!     Assign array limits to REAL for calculation of STORE
!     JAT D065 7/22/21 NONE OF THE VARIABLES BELOW ARE USED IN THIS SUBROUTINE
!     THEY ARE ACTUALLY SET IN ALLRESULT
!      RSRC   = REAL(NSRC)
!      RSEC   = REAL(NSEC)
!      RGRP   = REAL(NGRP)
!      RREC   = REAL(NREC)
!      RURB   = REAL(NURB)
!      RARC   = REAL(NARC)
!      RAVE   = REAL(NAVE)
!      RVAL   = REAL(NVAL)
!      RTYP   = REAL(NTYP)
!      RMAX   = REAL(NMAX)
!      RNET   = REAL(NNET)
!      RXM    = REAL(IXM )
!      RYM    = REAL(IYM )
!      REVE   = REAL(NEVE)
!      ROLM   = REAL(NOLM)
!      RPSD   = REAL(NPSD)
!      RQF    = REAL(NQF)
!      RPDMAX = REAL(NPDMAX)
!      RVMAX  = REAL(NVMAX)
!      RPAIR  = REAL(NPAIR)
!      RHIANN = REAL(NHIANN)

   return
end subroutine preset

subroutine preinclud
!***********************************************************************
!*                PREINCLUD Module of the AMS/EPA Regulatory Model - AERMOD
!*
!*       PURPOSE: To read an external receptor/source file using the
!*                INCLUDED keyword.
!*
!*       PROGRAMMER: Roger Brode
!*
!*       DATE:    September 24, 1996
!*
!*       MODIFIED:
!*
!*       INPUTS:
!*
!*       OUTPUTS:
!*
!*
!*       CALLED FROM:   PRESET, SRCSIZ, RECSIZ
!***********************************************************************

!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, itempl
   logical :: nopath, nokey
   character :: rdfrm*20
! JAT 06/22/21 D065
! REMOVE INPFLD AS UNUSED VARIABLE
!      CHARACTER INPFLD*2, PATHWY(7)*2
   character :: pathwy(7)*2
   interface
      subroutine expath(inpfld,pathwy,ipn,nopath)
         character (len=2), intent(in) :: inpfld
         character (len=2), intent(in), dimension(:) :: pathwy
         integer, intent(in) :: ipn
         logical, intent(out) :: nopath
      end subroutine expath
   end interface

!*    Variable Initializations
   modnam = 'PREINCLUD'
   eof = .false.
   iline  = 0
   itempl = 0

!     Initialize PATHWY array
   pathwy(1) = 'CO'
   pathwy(2) = 'SO'
   pathwy(3) = 'RE'
   pathwy(4) = 'ME'
   pathwy(5) = 'OU'
   pathwy(6) = '**'
   pathwy(7) = 'EV'

!MGS  Set logical to indicate processing INCLUDED keyword
   l_preinc = .true.

!     Setup READ format and ECHO format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   write(rdfrm,9100) istrg, istrg
9100 format('(A',i4.4,',T1,',i4.4,'A1)')

   if (ifc == 3) then
!        Retrieve Included Filename as Character Substring to Maintain Case
      if ((loce(3)-locb(3)) <= (ilen_fld - 1) ) then
!           Retrieve Filename as Character Substring to Maintain Original Case
!           Also Check for Filename Larger Than ILEN_FLD Characters
         incfil = runst1(locb(3):loce(3))
         open (incunt,file=incfil,action='READ',status='OLD',err=99)
      else
!           WRITE Error Message:  INCFIL Field is Too Long
         write(dummy,'(I8)') ilen_fld
         call errhdl(path,modnam,'E','291',dummy)
         return
      end if

   else if (ifc >= 4) then
!        Too Many Parameters
      return
   else
!        No Parameters Specified
      return
   end if

   go to 1001

!     Write Out Error Message for File OPEN Error
99 call errhdl(path,modnam,'E','500','INCFILE ')
   return

1001 continue

!     LOOP Through Input Runstream Records
   do while (.not. eof)

!        Increment the Line Counter.  It was Initially Set to 1, to Handle
!        the Code in Subroutine DEFINE
      iline = iline + 1

!        READ Record to Buffers, as A'num' and 'num'A1 where 'num' = ISTRG.
!        Length of ISTRG is Set in PARAMETER Statement in MAIN1
      read (incunt,rdfrm,end=999,err=888) runst1,&
      &(runst(i), i = 1, istrg)

!        If ILINE=1, reset ILINE temporarily to avoid the
!        check for column shift in subroutine DEFINE
      if (iline == 1) then
         iline  = 2
         itempl = 1
      end if

!        Check for blank input record and cycle
      if (len_trim(runst1) == 0) cycle

!        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
      call lwrupr

!        Define Fields on Card                              ---   CALL DEFINE
      call define

!        Reset ILINE if needed
      if (itempl == 1) then
         iline  = 1
         itempl = 0
      end if

!        Get the Contents of the Fields                     ---   CALL GETFLD
      call getfld

!        Check for 'NO ECHO' In First Two Fields
      if (field(1) == 'NO' .and. field(2) == 'ECHO') then
!           Skip record with NO ECHO during PREINCLUD stage of processing
         cycle
      end if

!        Extract Pathway ID From Field 1                    ---   CALL EXPATH
      call expath(field(1),pathwy,7,nopath)

!        For Invalid Pathway and Comment Lines Skip to Next Record
      if (nopath) then
!           Skip Error Message for PREINCLUD stage of processing
         path = ppath
         cycle
      else if (path == '**') then
         cycle
      end if

!        Extract Keyword From Field 2                       ---   CALL EXKEY
      call exkey(field(2),nokey)

      if (nokey) then
!           Invalid Keyword - Skip Error Message for PREINCLUD stage
         pkeywd = keywrd
         cycle
      end if

!        Save Current Path and Path Number as Previous Path and Number
      ppath = path
      ippnum = ipnum

!        Process Input Card Based on Pathway
      if (path == 'SO') then
!           Process SOurce Pathway Cards                    ---   CALL SRCSIZ
         call srcsiz
      else if (path == 'RE') then
!           Process REceptor Pathway Cards                  ---   CALL RECSIZ
         call recsiz
      else if (path == 'EV') then
         if (keywrd == 'EVENTPER') then
            neve = neve + 1
         end if
      end if

!        Store the Current Keyword as the Previous Keyword
      pkeywd = keywrd

      go to 11
999   eof = .true.
      rewind incunt
11    continue

   end do
   eof = .false.

   go to 1002

888 continue
! --- Error occurred reading the included file, issue error message
   call errhdl(path,modnam,'E','510','INCLUDED')
   runerr = .true.

1002 continue

!     Close the INCLUDED File
   close (incunt)

!MGS  Set logical to indicate no longer processing INCLUDED keyword
   l_preinc = .false.
   return
end subroutine preinclud


subroutine srcsiz
!***********************************************************************
!                 SRCSIZ Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To preprocess source inputs to determine
!                 storage requirements
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 24, 1996
!
!        MODIFIED:   To include the Aircraft Sources (ARISE).
!                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!                    04/01/2023
!
!        MODIFIED:   Added logical (L_PREINC) when processing an INCLUDED file.
!                    When processing an INCLUDED file the MODELOPTs are not
!                    set, so it will not check for BETA when preprocessing
!                    INCLUDED files with RLINE or RLINEXT sources.
!                    Wood, 02/08/2021
!
!        MODIFIED:   Added possibility for RLINE or RLINEXT source type
!                    and check for required use of BETA flag.
!                    Wood, 03/18/2019
!
!        MODIFIED:   Modified to add RLINE source type and check
!                    for required use of BETA flag.
!                    Wood, 07/20/2018
!
!        MODIFIED:   Modified to add counter for number of lines in a
!                    buoyant line source
!                    Amec Foster Wheeler, 06/30/2015
!
!        MODIFIED:   Modified to allow for the use of URBANSRC ALL on
!                    the SO pathway to indicate that all sources are
!                    to be treated as URBAN sources. This option assumes
!                    that only one (1) urban area has been defined using
!                    the CO URBANOPT keyword.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
!
!        MODIFIED:   To include options to vary emissions by
!                    hour-of-day, and day-of-week (HRDOW and HRDOW7).
!                    Modified method for determining maximum number
!                    of vertices for AREAPOLY sources for more precise
!                    and efficient memory allocation.  Also included
!                    allocation of arrays for building downwash data.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To include new options incorporated in version
!                    dated 06341.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/2006
!
!        MODIFIED:   To include options to vary emissions by month,
!                    hour-of-day, and day-of-week (MHRDOW and MHRDOW7).
!                    R.W. Brode, MACTEC (f/k/a PES), Inc., 06/22/05
!
!        MODIFIED:   To include an option to vary emissions by season,
!                    hour-of-day, and day-of-week (SHRDOW).
!                    R.W. Brode, PES, 4/10/2000
!
!        INPUTS:  Pathway (RE) and Keyword
!
!        OUTPUTS: Receptor Arrays
!                 Receptor Setup Status Switches
!
!        CALLED FROM:   PRESET
!***********************************************************************

!     Variable Declarations
   use main1
   use rline_data
   use buoyant_line

   implicit none
   character :: modnam*12

   integer :: i, k, num_srcids
   character (len=12) :: tmpsrcid, savesrcid
   allocatable :: tmpsrcid(:), savesrcid(:)

   save tmpsrcid, savesrcid, num_srcids

!     Variable Initializations
   modnam = 'SRCSIZ'

   if (keywrd == 'STARTING') then
!        Initialize Counters and Set Status Switch
      nsrc = 0
      ngrp = 0
      nolm = 0
      npsd = 0
      nblgrp = 0                              ! (Multiple_BuoyLines_D41_Wood)
      nqf  = 0
      nbf  = 0
      nsec = 0
      npit = 0
      npnt = 0
      nvol = 0
      narea = 0
      npoly = 0
      ncirc = 0
      nline = 0
! ---    Add variable for number of RLINE sources
      nrlines = 0
! ---    Variable for total # buoyant lines, independent of BL groups
      nblp = 0
! ---    Add variable for number of urban sources
      nurbsrc = 0
! ---    D113 Added for SIDEWASH
      nswp = 0
      naftsrc = 0                           !  Added for Aircraft Plume Rise; UNC-IE
      nhbp = 0                            ! Added for HBP, JAN. 2023
      nvmax  = 0
      nvtemp = 0
      nptemp = 0
      npdmax = 0
      prevsrcid = '        '
      prevgrpid = '        '
! ---    D156 This routine is called twice when an include file is used. NUM_SRCIDS, TMPSRCID, and SAVESRCID
!        are used for AREACIRC source type. The following lines will clear out these three variables
!        so that the source IDs are not double counted the second time through. WSP 2/7/23
      num_srcids = 0
      if (allocated(tmpsrcid)) deallocate(tmpsrcid)
      if (allocated(savesrcid)) deallocate(savesrcid)

   else if (keywrd == 'LOCATION') then
      nsrc = nsrc + 1
      if (field(4)(1:5) == 'POINT') then
         npnt = npnt + 1
      else if (field(4) == 'VOLUME') then
         nvol = nvol + 1
      else if (field(4) == 'LINE') then
         nline = nline + 1

!        CRT, 3/25/2022, D113 Added for Sidewash source - requires ALPHA
      else if (field(4) == 'SWPOINT') then
!           Check if required ALPHA flag is present for SWPOINT sources
!           or if processing INCLUDED keyword (MODELOPTs not set yet!)
         if(l_alpha .or. l_preinc) then
            nswp = nswp + 1
         else
            call errhdl(path,modnam,'E','198',' SWPOINT')
         end if

! ---       D161 Add NO2 processing restriction for PVMRM, TTRM, TTRM2, GRSM and OLM 2/20/23 WSP
! ---       D161 Change errors to warnings so multiple sources can be run together if an
! ---            NO2 method is specified that is not implemented for a source, 4/27/2023 CRT
         if(pvmrm) call errhdl(path,modnam,'W','722',field(4))
         if(runttrm2) then
            call errhdl(path,modnam,'W','723',field(4))
         else if(runttrm) then
            call errhdl(path,modnam,'W','710',field(4))
         end if
         if(olm) call errhdl(path,modnam,'W','725',field(4))
         if(grsm) call errhdl(path,modnam,'W','724',field(4))
         if(arm2) call errhdl(path,modnam,'W','726',field(4))

      else if (field(4) == 'RLINE') then    !D150 RLINE requires the BETA keyword while RLINEXt requires the ALPHA keyword. Needed to separate them WSP 1/30/23
!D150         ELSE IF ((FIELD(4) .EQ. 'RLINE') .or.
!D150     &              (FIELD(4) .EQ. 'RLINEXT')) THEN
!           Check that the required BETA flag is present for RLINE sources
!        or if processing INCLUDED keyword (MODELOPTs not set yet!)

!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (begin)
!MGS            IF(BETA .or. L_PREINC) THEN  !D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP
         nrlines = nrlines + 1
!MGS            ELSE
!MGS               CALL ERRHDL(PATH,MODNAM,'E','199',' RLINE BETA Required')
!MGS            END IF
!MGS D182_Remove_BETA_flag_GRSM_RLINE_COARE_WSP (end)

! ---       D161 Add NO2 processing restriction for PVMRM, TTRM, TTRM2, GRSM and OLM 2/20/23 WSP
! ---       D161 Change errors to warnings so multiple sources can be run together if an
! ---            NO2 method is specified that is not implemented for a source, 4/27/2023 CRT
         if(pvmrm) call errhdl(path,modnam,'W','722',field(4))
         if(runttrm2) then
            call errhdl(path,modnam,'W','723',field(4))
         else if(runttrm) then
            call errhdl(path,modnam,'W','710',field(4))
         end if
         if(olm) call errhdl(path,modnam,'W','725',field(4))
         if(grsm) call errhdl(path,modnam,'W','724',field(4))

      else if (field(4) == 'RLINEXT') then
!           Check that the required ALPHA flag is present for RLINEXT sources
!           or if processing INCLUDED keyword (MODELOPTs not set yet!) D150 AKP 05/23/2023
         if(l_alpha .or. l_preinc) then
            nrlines = nrlines + 1
         else
!              D150 CRT 6/1/2023 - Update error message code to specify ALPHA required
!               CALL ERRHDL(PATH,MODNAM,'E','199',' RLINEXT')
            call errhdl(path,modnam,'E','198',' RLINEXT')
         end if

! ---       D161 Add NO2 processing restriction for PVMRM, TTRM, TTRM2, GRSM and OLM 2/20/23 WSP
! ---       D161 Change errors to warnings so multiple sources can be run together if an
! ---            NO2 method is specified that is not implemented for a source, 4/27/2023 CRT
         if(pvmrm) call errhdl(path,modnam,'W','722',field(4))
         if(runttrm2) then
            call errhdl(path,modnam,'W','723',field(4))
         else if(runttrm) then
            call errhdl(path,modnam,'W','710',field(4))
         end if
         if(olm) call errhdl(path,modnam,'W','725',field(4))
         if(grsm) call errhdl(path,modnam,'W','724',field(4))

      else if (field(4) == 'BUOYLINE') then
! (Multiple_BuoyLines_D41_Wood)
! ---       Increment number of individual buoyant lines (independent of
!             buoyant line groups that define BL sources)
         nblp = nblp + 1
! ---       D161 Add NO2 processing restriction for PVMRM, TTRM, TTRM2, GRSM and OLM 2/20/23 WSP
! ---       D161 Change errors to warnings so multiple sources can be run together if an
! ---            NO2 method is specified that is not implemented for a source, 4/27/2023 CRT
         if(pvmrm) call errhdl(path,modnam,'W','722',field(4))
         if(runttrm2) then
            call errhdl(path,modnam,'W','723',field(4))
         else if(runttrm) then
            call errhdl(path,modnam,'W','710',field(4))
         end if
         if(olm) call errhdl(path,modnam,'W','725',field(4))
         if(grsm) call errhdl(path,modnam,'W','724',field(4))
      else if (field(4)(1:4) == 'AREA') then
         narea = narea + 1
         nvmax = max( nvmax, 8 )
         if (field(4) == 'AREAPOLY') then
            npoly = npoly + 1
            nvmax = max( nvmax, 8 )
         else if (field(4) == 'AREACIRC') then
! ---          Increment counter for number of AREACIRC sources
            ncirc = ncirc + 1
! ---          Save AREACIRC source IDs in temporary arrays in order
!              to check SRCPARAM keyword inputs for NVERTS parameter.
!              First check allocation status of TMPSRCID array;
!              if allocated, then save TMPSRCID array, deallocate,
!              and reallocate based on current number of sources.
!              The end result of TMPSRCID will be an array with
! ---          only the source IDs for AREACIRC sources.
            if (allocated(tmpsrcid)) then
               if (allocated(savesrcid)) then
                  savesrcid = tmpsrcid
                  num_srcids = size(tmpsrcid)
                  deallocate (tmpsrcid)
                  allocate (tmpsrcid(nsrc))
                  tmpsrcid(1:num_srcids)  = savesrcid
                  tmpsrcid(num_srcids+1:) = ' '
               end if
            else
               allocate (tmpsrcid(nsrc))
               allocate (savesrcid(nsrc))
               num_srcids = nsrc
            end if
            tmpsrcid(nsrc) = field(3)
            num_srcids = nsrc
         end if
      else if (field(4) == 'OPENPIT') then
         npit = npit + 1
      else
! ---       Invalid SrcTyp
!RT  12/10/2021  Replace placeholder code 'CCC' with new code string
         call errhdl(path,modnam,'E','640',field(4))
      end if

   else if (keywrd == 'SRCPARAM') then

! ---    Check for AREACIRC sources with user-specified NVERTS
      do i = 1, nsrc
! ---       Exit loop if number of temporary AREACIRC source IDs
!           is less than current loop index (I)
         if (num_srcids < i) exit

         if( field(3) == tmpsrcid(i) ) then
! ---          This is an AREACIRC source: check for NVERTS input

            if (ifc >= 7) then

! ---             Set maximum number of vertices based on
!                 user-specified number for AREACIRC source
               call stodbl(field(7),ilen_fld,dnum,imit)
!                 Check The Numerical Field
               if (imit /= 1) then
                  call errhdl(path,modnam,'E','208',keywrd)
               else
! ---                Adjust NVMAX if needed based on number of
!                    vertices specified for this AREACIRC source +4
                  nvmax = max( nvmax, idnint(dnum)+4 )
               end if
            else
! ---             User did not specify number of vertices for
!                 this AREACIRC source; adjust NVMAX if needed
!                 based on default value of 20 for NVERTS (+4)
               nvmax = max( nvmax, 24 )
            end if
! ---          AREACIRC source ID was found, EXIT loop
            exit
         end if

      end do

   else if (keywrd == 'AREAVERT') then
! ---    Check for consistency of SRCID's
      if (field(3)(1:len_trim(field(3))) ==&
      &prevsrcid(1:len_trim(prevsrcid))) then
         nvtemp = nvtemp + ifc - 3
! ---       Set NVMAX based on current number of
!           vertices for this source (NVTEMP/2) + NVPOLY
!           (where the NVPOLY PARAMETER is assigned a value of 12)
!           to account for maximum number of sides
!           for transect through source
         nvmax  = max( nvmax, nvpoly+nint(float(nvtemp/2)) )
      else
! ---       This is first AREAVERT keyword for this AREAPOLY source.
!           Assign NVTEMP based on number of data fields specified.
         nvtemp = ifc - 3
         nvmax  = max( nvmax, nvpoly+nint(float(nvtemp/2)) )
         prevsrcid = field(3)
      end if

   else if (keywrd=='PARTDIAM') then
      if (field(3) == prevsrcid) then
         nptemp = nptemp + ifc - 3
! ---       Set NPDMAX based on current number of
!           particle size categories for this source
         npdmax = max( npdmax, nptemp )
      else
         nptemp = ifc - 3
         npdmax = max( npdmax, nptemp )
         prevsrcid = field(3)
      end if

   else if ((keywrd=='BUILDHGT' .or.&
   &keywrd=='BUILDWID' .or.&
   &keywrd=='BUILDLEN')) then
      nsec = 36

   else if (keywrd == 'METHOD_2') then
      npdmax = max( npdmax, 1 )

   else if (keywrd == 'EMISFACT') then
      if (field(4) == 'SEASON') then
         nqf = max( nqf, 4)
      else if (field(4) == 'MONTH') then
         nqf = max( nqf, 12)
      else if (field(4) == 'HROFDY') then
         nqf = max( nqf, 24)
      else if (field(4) == 'WSPEED') then
         nqf = max( nqf, 6)
      else if (field(4) == 'SEASHR') then
         nqf = max( nqf, 96)
      else if (field(4) == 'HRDOW') then
         nqf = max( nqf, 72)
      else if (field(4) == 'HRDOW7') then
         nqf = max( nqf, 168)
      else if (field(4) == 'SHRDOW') then
         nqf = max( nqf, 288)
      else if (field(4) == 'SHRDOW7') then
         nqf = max( nqf, 672)
      else if (field(4) == 'MHRDOW') then
         nqf = max( nqf, 864)
      else if (field(4) == 'MHRDOW7') then
         nqf = max( nqf, 2016)
      end if

   else if (keywrd == 'BGSECTOR') then
! ---    Assign logical variable for BGSECTORs
      L_BGSector = .true.
! ---    Set maximum array limit for number of BACKGRND sectors
      NUMBGSects = ifc - 2

   else if (keywrd == 'BACKGRND') then
      l_backgrnd = .true.
! ---    Assign field index to 4 if BGSECTOR is used, otherwise 3
      if (L_BGSector) then
         k = 4
      else
         k = 3
      end if
      if (field(k) == 'ANNUAL') then
         nbf = max( nbf, 1)
      else if (field(k) == 'SEASON') then
         nbf = max( nbf, 4)
      else if (field(k) == 'MONTH') then
         nbf = max( nbf, 12)
      else if (field(k) == 'HROFDY') then
         nbf = max( nbf, 24)
      else if (field(k) == 'WSPEED') then
         nbf = max( nbf, 6)
      else if (field(k) == 'SEASHR') then
         nbf = max( nbf, 96)
      else if (field(k) == 'HRDOW') then
         nbf = max( nbf, 72)
      else if (field(k) == 'HRDOW7') then
         nbf = max( nbf, 168)
      else if (field(k) == 'SHRDOW') then
         nbf = max( nbf, 288)
      else if (field(k) == 'SHRDOW7') then
         nbf = max( nbf, 672)
      else if (field(k) == 'MHRDOW') then
         nbf = max( nbf, 864)
      else if (field(k) == 'MHRDOW7') then
         nbf = max( nbf, 2016)
      end if

   else if (keywrd == 'URBANSRC' .and.&
   &ifc == 3 .and. field(3) == 'ALL'&
   &.and. l_preset_urban) then
      if (.not. l_multurb) then
!           Set logical for URBANSRC ALL option (not applicable for
!           multiple urban areas)
         l_urban_all = .true.
! ---       Assign number of urban sources
         nurbsrc = nsrc
      else
!           Issue ERROR message for URBANSRC ALL option with multiple
!           urban areas
         call errhdl(path,modnam,'E','279','URBANSRC ALL')
      end if

! --- Check for number of urbansrc
   else if (keywrd == 'URBANSRC' .and. .not.l_multurb) then
      nurbsrc = nurbsrc + ifc - 2

   else if (keywrd == 'URBANSRC' .and. l_multurb) then
      nurbsrc = nurbsrc + ifc - 3

!**  Added for Aircraft Plume Rise; UNC-IE
   else if (keywrd == 'ARCFTSRC' .and.&
   &ifc == 3 .and. field(3) == 'ALL'&
   &.and. l_preset_arcft) then
      l_arcft_all = .true.
! --- Assign number of AIRCRAFT sources
      naftsrc = nsrc
! --- Check for number of ARCFTSRC
   else if (keywrd == 'ARCFTSRC') then
      naftsrc = naftsrc + ifc - 2
!**  End Aircraft Plume Rise insert; April 2023

! Added for HBP, JAN 2023
   else if (keywrd == 'HBPSRCID' .and.&
   &ifc == 3 .and. field(3) == 'ALL') then
      l_hbp_all = .true.
! End HBP insert

   else if (keywrd == 'OLMGROUP') then
      if (nolm == 0) prevgrpid = '        '
      if (field(3) /= prevgrpid) then
         nolm = nolm + 1
         prevgrpid = field(3)
      end if

   else if (keywrd == 'PSDGROUP') then
      if (npsd == 0) prevgrpid = '        '
      if (field(3) /= prevgrpid) then
         npsd = npsd + 1
         prevgrpid = field(3)
      end if

! (Multiple_BuoyLines_D41_Wood)
!     Process BLPGROUP keyword for multiple buoyant lines
   else if (keywrd == 'BLPINPUT') then
! ---    Average buoyant line source average input parameters specified
! ---       Increment number of buoyant line sources)
      NBLAVGINPalloc = NBLAVGINPalloc + 1

!     Process BLPGROUP keyword for multiple buoyant lines
   else if (keywrd == 'BLPGROUP') then
! ---    A buoyant line source group specified
      if (nblgrp == 0) prevgrpid = '        '
      if (field(3) /= prevgrpid) then
! ---       Increment number of buoyant line sources)
         nblgrp = nblgrp + 1
         prevgrpid = field(3)
      end if

   else if (keywrd == 'SRCGROUP') then
      if (ngrp == 0) prevgrpid = '        '
      if (field(3) /= prevgrpid) then
         ngrp = ngrp + 1
         prevgrpid = field(3)
      end if

   else if (keywrd == 'INCLUDED') then
      call preinclud

   else if (keywrd == 'FINISHED') then

! (Multiple_BuoyLines_D41_Wood)
! ---    For legacy input control files that have a buoyant line source
!         and no BLPGROUP keyword(s); NBLGRP used for array allocation
      if (nblp > 0 .and. nblgrp == 0) then
         nblgrp = 1
      end if
      return

   end if

   return
end subroutine srcsiz

recursive subroutine recsiz
!***********************************************************************
!                 RECSIZ Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To preprocess receptor inputs to determine
!                 storage requirements
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 24, 1996
!
!        MODIFIED:   To include the recursive keyword for D112
!                    Wood - 9/29/22
!
!        INPUTS:  Pathway (RE) and Keyword
!
!        OUTPUTS: Receptor Arrays
!                 Receptor Setup Status Switches
!
!        CALLED FROM:   PRESET
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'RECSIZ'

   if (keywrd == 'STARTING') then
!        Initialize Counters and Set Status Switch
      nrec = 0
      nnet = 0
      ixm  = 0
      iym  = 0
      pxsoid = ' '
      ista = .false.
   else if (keywrd == 'GRIDCART') then
!        Process Cartesian Grid Receptor Network            ---   CALL PRECART
      call precart
   else if (keywrd == 'GRIDPOLR') then
!        Process Polar Receptor Network                     ---   CALL PREPOLR
      call prepolr
   else if (keywrd == 'DISCCART') then
      nrec = nrec + 1
   else if (keywrd == 'EVALCART') then
      nrec = nrec + 1
   else if (keywrd == 'DISCPOLR') then
      nrec = nrec + 1
   else if (keywrd == 'INCLUDED') then
      call preinclud
   else if (keywrd == 'FINISHED') then
      return
   end if

   return
end subroutine recsiz

subroutine precart
!***********************************************************************
!                 PRECART Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Cartesian Grid Receptor Network Inputs
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:    September 24, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Cartesian Grid Receptor Network Inputs
!
!        CALLED FROM:   RECSIZ
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'PRECART'

!     READ in the Netid and Nettype
   if (ifc < 3) then
!        Missing Data Field
      go to 999
   end if
   netidt = field(3)
   if (.not.newid .and. (netidt=='    ' .or.&
   &netidt=='XYINC' .or. netidt=='XPNTS' .or.&
   &netidt=='YPNTS' .or. netidt=='ELEV' .or.&
   &netidt=='HILL'  .or.&
   &netidt=='FLAG'  .or. netidt=='END')) then
      netidt = pnetid
      ktype = field(3)
   else if (.not.newid .and. netidt==pnetid) then
      ktype = field(4)
   else if (newid .and. netidt/=' ') then
      newid = .false.
      ktype = field(4)
!        The Keyword Counter
      nnet = nnet + 1
   else
!        Invalid Secondary Keyword
      go to 999
   end if

!     Start to Set Up the Network
   if (ktype == 'STA') then
!        Initialize Logical Control Variables
      ista = .true.
      iend = .false.
      newid = .false.
      recerr = .false.
!        Set Counters of Calculation Field
      icount = 0
      jcount = 0
   else if (ktype == 'XYINC') then
!        Set the Uniform Spacing Receptor Network           ---   CALL PREGENCAR
      call pregencar
   else if (ktype=='XPNTS' .or. ktype=='YPNTS') then
!        Set the Non-uniform Spacing Receptor Network       ---   CALL PREXYPNTS
      call prexypnts
   else if (ktype == 'END') then
      iend = .true.
      if (.not. recerr) then
         nrec = nrec + icount*jcount
      end if
      ista = .false.
      newid = .true.

   else if (ktype/='ELEV' .and. ktype/='FLAG' .and.&
   &ktype/='HILL') then
!        Invalid Secondary Keyword
      recerr = .true.
      go to 999

   end if

   pnetid = netidt

999 return
end subroutine precart

subroutine pregencar
!***********************************************************************
!                 PREGENCAR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Generates Cartesian Grid Receptor Network With
!                 Uniform Spacing
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 24, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Cartesian Grid Receptor Network With Uniform
!                 Spacing
!
!        CALLED FROM:   PRECART
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, k
   real    :: tempr(6)
!     JAT 7/22/21 D065 XDELTA AND YDELTA NOT USED
!      DOUBLE PRECISION :: TEMPD(6), XDELTA, YDELTA
   double precision :: tempd(6)
   logical :: error

!     Variable Initializations
   modnam = 'PREGENCAR'
   error = .false.

!     Check for Location of Secondary Keyword, XYINC
   do i = 1, ifc
      if (field(i) == 'XYINC') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Missing Parameter
      recerr = .true.
      go to 999
   else if (ifc > isc+5) then
!        Too Many Parameters
      recerr = .true.
      go to 999
   else if (ifc < isc+5) then
!        Too Few Parameters
      recerr = .true.
      go to 999
   end if

!     Input The Numerical Values
   do k = 1,6
      if (k == 2 .or. k == 5) then
         call stonum(field(isc + k-1),ilen_fld,tempr(k),imit)
!           Check The Numerical Field
         if (imit == -1) then
            error = .true.
            recerr = .true.
         end if
      else
         call stodbl(field(isc + k-1),ilen_fld,tempd(k),imit)
!           Check The Numerical Field
         if (imit == -1) then
            error = .true.
            recerr = .true.
         end if
      end if
   end do

   if (error) then
      error = .false.
      go to 999
   end if

!     Assign Values to Appropriate Variables for Generated Network
   xint   = tempd(1)
   icount = nint(tempr(2))
!     JAT 7/22/21 D065 XDELTA NOT USED
!      XDELTA = TEMPD(3)
   yint   = tempd(4)
   jcount = nint(tempr(5))
!     JAT 7/22/21 D065 YDELTA NOT USED
!      YDELTA = TEMPD(6)

!     Assign Them to the Coordinate Arrays
   if (icount > ixm) then
      ixm = icount
   end if
   if (jcount > iym) then
      iym = jcount
   end if

999 return
end subroutine pregencar

subroutine prexypnts
!***********************************************************************
!                 PREXYPNTS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Cartesian Grid x,y Input Value
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 24, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Cartesian Grid x,y Input Value
!
!        CALLED FROM:   PRECART
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, jset

!     Variable Initializations
   modnam = 'PREXYPNTS'

   if (ktype == 'XPNTS') then
!        Check for Location of Secondary Keyword, XPNTS
      do i = 1, ifc
         if (field(i) == 'XPNTS') then
            isc = i + 1
         end if
      end do

!        Determine Whether There Are Enough Parameter Fields
      if (ifc == isc-1) then
!           Missing Parameter
         recerr = .true.
         go to 999
      end if

      iset = icount
      do i = isc, ifc
         call stonum(field(i),ilen_fld,fnum,imit)
!           Check The Numerical Field
         if (imit == -1) then
            recerr = .true.
         end if
         iset = iset + 1
         if (iset > ixm) then
            ixm = iset
         end if
      end do
      icount = iset

   else if (ktype == 'YPNTS') then
!        Check for Location of Secondary Keyword, YPNTS
      do i = 1, ifc
         if (field(i) == 'YPNTS') then
            isc = i + 1
         end if
      end do

!        Determine Whether There Are Enough Parameter Fields
      if (ifc == isc-1) then
!           Missing Parameter
         recerr = .true.
         go to 999
      end if

      jset = jcount
      do i = isc, ifc
         call stonum(field(i),ilen_fld,fnum,imit)
!           Check The Numerical Field
         if (imit == -1) then
            recerr = .true.
         end if
         jset = jset + 1
         if (jset > iym) then
            iym = jset
         end if
      end do
      jcount = jset

   end if

999 return
end subroutine prexypnts

subroutine prepolr
!***********************************************************************
!                 PREPOLR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Polar Grid Receptor Network Inputs
!
!        PROGRAMMER:  Roger Brode
!
!        DATE:    September 24, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Receptor Network Inputs
!
!        CALLED FROM:   RECSIZ
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'PREPOLR'

   if (ifc < 3) then
!        Missing Data Field
      go to 999
   end if

!     READ in the Netid and Nettype
   netidt = field(3)
   if (.not.newid .and. (netidt=='    ' .or.&
   &netidt=='ORIG' .or. netidt=='DIST' .or.&
   &netidt=='DDIR' .or. netidt=='ELEV' .or.&
   &netidt=='HILL' .or.&
   &netidt=='FLAG' .or. netidt=='GDIR' .or.&
   &netidt=='END')) then
      netidt = pnetid
      ktype = field(3)
   else if (.not.newid .and. netidt==pnetid) then
      ktype = field(4)
   else if (newid .and. netidt/='    ') then
      newid = .false.
      ktype = field(4)
!        The Keyword Counter
      nnet = nnet + 1
   else
!        Invalid Secondary Keyword
      recerr = .true.
      go to 999
   end if

!     Start to Set Up the Network
   if (ktype == 'STA') then
      ista = .true.
      iend = .false.
      newid = .false.
      recerr = .false.
      icount = 0
      jcount = 0
   else if (ktype == 'DIST') then
!        Read in the Distance Set                           ---   CALL PREPOLDST
      call prepoldst
   else if (ktype == 'GDIR') then
      call pregenpol
   else if (ktype == 'DDIR') then
      call preradrng
   else if (ktype == 'END') then
      iend = .true.
!        Get the Final Result
      if (.not. recerr) then
         nrec = nrec + icount*jcount
      end if
      ista = .false.
      newid = .true.

   else if (ktype/='ELEV' .and. ktype/='FLAG' .and.&
   &ktype/='HILL' .and. ktype/='ORIG') then
!        Invalid Secondary Keyword
      recerr = .true.
      go to 999

   end if

   pnetid = netidt

999 return
end subroutine prepolr

subroutine prepoldst
!***********************************************************************
!                 PREPOLDST Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Gets Distances for the Polar Network
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 24, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Network Distance Input Value
!
!        CALLED FROM:   PREPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i

!     Variable Initializations
   modnam = 'PREPOLDST'

!     Skip the Unrelated Fields
   do i = 1, ifc
      if (field(i) == 'DIST') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Missing Parameter
      recerr = .true.
      go to 999
   end if

   iset = icount

   do i = isc, ifc
      call stonum(field(i),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         recerr = .true.
      end if
      iset = iset + 1
      if (iset > ixm) then
         ixm = iset
      end if
   end do

   icount = iset

999 return
end subroutine prepoldst

subroutine pregenpol
!***********************************************************************
!                 PREGENPOL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Generates Polar Receptor Network With
!                 Uniform Spacing
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 24, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Receptor Network With Uniform Direction Spacing
!
!        CALLED FROM:   PREPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, k
!     JAT 7/22/21 D065 DIRINI AND DIRINC NOT USED
!      DOUBLE PRECISION :: TEMPP(3), DIRINI, DIRINC
   double precision :: tempp(3)
   logical :: error

!     Variable Initializations
   modnam = 'PREGENPOL'
   error = .false.

!     Check for the Location of the Secondary Keyword, GDIR
   do i = 1, ifc
      if (field(i) == 'GDIR') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Missing Parameter
      recerr = .true.
      go to 999
   else if (ifc < isc+2) then
!        Not Enough Parameters
      recerr = .true.
      go to 999
   else if (ifc > isc+2) then
!        Too Many Parameters
      recerr = .true.
      go to 999
   end if

!     Input Numerical Values
   do k = 1, 3
      call stodbl(field(isc + k-1),ilen_fld,tempp(k),imit)
!        Check The Numerical Field
      if (imit == -1) then
         recerr = .true.
         error = .true.
      end if
   end do

   if (error) then
      error = .false.
      go to 999
   end if

   jcount = idnint(tempp(1))
!     JAT 7/22/21 D065 DIRINI AND DIRINC NOT USED
!      DIRINI = TEMPP(2)
!      DIRINC = TEMPP(3)

!     Assign Them to the Coordinate Arrays
   if (jcount > iym) then
      iym = jcount
   end if

999 return
end subroutine pregenpol

subroutine preradrng
!***********************************************************************
!                 PRERADRNG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Non-Uniform Polar Network Value
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    September 24, 1996
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Network Directions in Non-Uniform Spacing
!
!        CALLED FROM:   PREPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i

!     Variable Initializations
   modnam = 'PRERADRNG'

!     Skip the non-useful Fields
   do i = 1, ifc
      if (field(i) == 'DDIR') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      recerr = .true.
      go to 999
   end if

   iset = jcount

   do i = isc, ifc
      call stonum(field(i),ilen_fld,fnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         recerr = .true.
      end if
      iset = iset + 1
      if (iset > iym) then
         iym = iset
      end if
   end do

   jcount = iset

999 return
end subroutine preradrng

subroutine set_window
!***********************************************************************
!                 SET_WINDOW Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Preprocess Meteorology Surface Data Card (SURFDATA)
!                 to Set Date Window for Y2K Fixes
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    April 29, 1999
!
!        MODIFICATIONS:
!
!                    To subtract 1 from ISTRT_WIND in case data file
!                    contains data from end of previous year.
!                    R.W. Brode, PES, Inc.  8/28/01
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Starting Century, ISTRT_CENT                    [I4]
!                 Starting Year for 2-digit Window, ISTRT_WIND    [I4]
!
!        ERROR HANDLING:   Checks for Too Few Parameters;
!                          Checks for Invalid Numeric Fields;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   PRESET
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'SET_WINDOW'

   if (ifc < 4) then
      go to 999
   else if (ifc > 7) then
      go to 999
   end if

   call stonum(field(4),ilen_fld,fnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      go to 999
   end if
   isyear = nint(fnum)
!     Adjusted the if statement to set default window to 1950 only when the year is between 50 and 99 Wood 9/15/22
   if (isyear >= 50 .and. isyear < 100) then
!      IF (ISYEAR .LT. 100) THEN
!        Write warning message for 2-digit year, and set default "windowing"
!        variables, ISTRT_CENT (=19) and ISTRT_WIND (=50).
      if (.not. L_SkipMessages) then
         call errhdl(path,modnam,'W','360',keywrd)
      end if
      istrt_cent = 19
      istrt_wind = 50
!     Added a catch for dates which could occur in 20XX, values must be between 00 and 49 Wood 9/15/22
   else if (isyear < 50) then
!        Write warning message for 2-digit year, and set default "windowing"
      if (.not. L_SkipMessages) then
         call errhdl(path,modnam,'W','360',keywrd)
      end if
      istrt_cent = 20
      istrt_wind = 00
   else
!        Determine starting century (ISTRT_CENT) and starting year for
!        window (ISTRT_WIND) from 4-digit input
      istrt_cent = isyear/100
      istrt_wind = isyear - istrt_cent*100

!        Subtract 1 from ISTRT_WIND in case data file contains data
!        from end of previous year
      istrt_wind = istrt_wind - 1
      if (istrt_wind < 0) then
         istrt_wind = 0
      end if
!        Check for year .ge. 2148 to avoid integer overflow on FULLDATE
      if (istrt_cent >= 21 .and. istrt_wind >= 48) then
         call errhdl(path,modnam,'E','365',keywrd)
         istrt_cent = 21
         istrt_wind = 47
      end if
   end if
   go to 1000

999 continue
!     For error in processing assume 1900 for start century and 50 for window
   istrt_cent = 19
   istrt_wind = 50

1000 return
end subroutine set_window

subroutine chk_endyr
!***********************************************************************
!                 CHK_ENDYR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Checks date for "end-of-year" for use in ANNUAL
!                 averages and PM-2.5 processing.
!
!        PROGRAMMER: Roger Brode
!
!        DATE:
!
!        MODIFIED:   To allow user-specified rank for PM2.5 processing
!                    to accommodate latest guidance for PM2.5 modeling.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!                    To check for allocation status prior to
!                    initializing allocatable arrays, and include
!                    maximum annual value arrays.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Plant Boundary Receptor Location Inputs
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: iend_day, n

!     Variable Initializations
   modnam = 'CHK_ENDYR'

   if( (iendmn==2.and.ienddy==29.and.imonth==2) .and.&
   &(mod(iyr,4)/=0) .or.&
   &(mod(iyr,100)==0 .and. mod(iyr,400)/=0)) then
!        Set End Day to 28 for non-leap year February
      iend_day = 28
   else
      iend_day = ienddy
   end if

   if (imonth==iendmn .and. iday==iend_day .and.&
   &ihour==iendhour) then
!        End of year reached, increment counter and store High-N-High (HNH) values
      numyrs = numyrs + 1
!        Reset hour counter for MAXDCONT met data arrays
      ihr_ndx = 0
      if (annual) then
! ---       Calculate ANNUAL averages
         call perave
! ---       Check for ANNUAL POSTFILE
         if( anpost )then
            call pstann
         endif
! ---       Sum the annual averages
         sumann(:,:,:) = sumann(:,:,:) + annval(:,:,:)
!           Re-initialize the annual counters and array
         ianhrs  = 0
         ianclm  = 0
         ianmsg  = 0
         nskiptot = 0
         if (allocated(annval))  annval  = 0.0d0
         if (allocated(amxval))  amxval  = 0.0d0
         if (allocated(imxloc))  imxloc  = 0
      end if
      if ((pm25ave .or. no2ave .or. so2ave) .and. numave>=1) then
! ---       Sum the High-N-High 24-hour values for PM-2.5,
!           or High-N-High 1-hour values for NO2
         do n = 1, nval
            sumhnh(1:numrec,1:numgrp,n) =&
            &sumhnh(1:numrec,1:numgrp,n) +&
            &himxdly(1:numrec,1:numgrp,n)
            if (numyrs <= nyears) then
               himxdly_byyr(1:numrec,1:numgrp,n,numyrs) =&
               &himxdly(1:numrec,1:numgrp,n)
               nhidatmxd_byyr(1:numrec,1:numgrp,n,numyrs) =&
               &nhidatmxd(1:numrec,1:numgrp,n)
            else
! ---             Write Error Message        ! Too many years
               write(dummy,'(''NYR='',I4)') nyears
               call errhdl(path,modnam,'E','482',dummy)
               runerr = .true.
            end if

            if (mxdaily_byyr .and. nhiave(n,1) == 1) then
               call mxdybyyr(n)
            endif
         end do
!           Re-initialize the MAXDAILY Value Arrays used for
!           PM25/NO2/SO2 Processing averaged across years
         if (allocated(himxdly))   himxdly   = 0.0d0
         if (allocated(nhidatmxd)) nhidatmxd = 0
      end if
      nremain = 0
   else
!        Increment counter for number of hours remaining after
!        the end of the last year
      nremain = nremain + 1
   end if

   return
end subroutine chk_endyr

subroutine maxdcont_loop
!***********************************************************************
!                MAXDCONT_LOOP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE:    Control "post-processing" for OU MAXDCONT option
!
!        PROGRAMMER: Roger W. Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED:   Modified to add 'saved' arrays for buoyant line source
!                    Amec Foster Wheeler, 06/30/2015
!
!        MODIFIED:   Modified subroutine MAXDCONT_LOOP to correct problems
!                    with the MAXDCONT option for applications that vary
!                    emissions (EMISFACT), background ozone data (O3VALUES),
!                    or background concentrations (BACKGRND) with a day-of-week
!                    component, e.g., SHRDOW or SHRDOW7, etc. Also modified
!                    subroutine MAXDCONT_LOOP to include checks on the
!                    consistency between results in the SUMVAL_MAXD and
!                    SUMHNH arrays for the "target" source group under
!                    the MAXDCONT option.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM:   MAIN
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12
   integer :: i

! JAT 06/22/21 D065
! REMOVE NDAY AS UNUSED VARIABLE
!      INTEGER :: NDAY(12)
   integer :: ijdy
   integer :: isdate_sav, iedate_sav
   integer :: jgrp, ival, ir, ig
   integer :: icyr, icyr2, imn, idy, ihr, icjday, ipjday
   logical :: hit_thresh
! Unused: INTEGER :: INYR, INMN, INDY, JDY

!     Variable Initializations
! JAT 06/22/21 DO65
! SINCE NDAY HAS BEEN REMOVED, COMMENT OUT INITIALIZATION OF NDAY
!      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/

!     Variable Initializations

   modnam = 'MAXDCONT_LOOP'
   path   = 'CN'

   hit_thresh = .false.

! --- Reinitialize NUMHRS, NUMCLM and NUMMSG
   numhrs(:) = 0
   numclm(:) = 0
   nummsg(:) = 0
!     Initialize __VAL arrays (1:NUMTYP)
   hrval   = 0.0d0
   aerval  = 0.0d0
   aveval  = 0.0d0

!     JAT 9/20/18 added from 18081
!     move NUMREC=1 to here from below
! --- Reset number of receptors (NUMREC) to 1
!     for use in max daily contribution analyses
   numrec = 1
   if (allocated(backave)) backave(:) = 0.0d0

!     JAT 9/20/18
!     Deallocate CHI and reallocate to use NUMREC
!     original CHI allocation is CHI(NREC,NSRC,NUMTYP)
!     for this subroutine it is NUMREC, NSRC,NUMTYP
!     this is to correct problem with MAXCHI and CHI
!     in subroutine PVMRM_CALC during MAXDCONT processing
!     normal processing of PVMRM_CALC during routine AERMOD run
!     is unaffected
   if (allocated(chi)) then
      deallocate(chi)
      allocate(chi(numrec,nsrc,numtyp))
      chi(:,:,:) = 0.0d0
   endif
   if(grsm)then
      chi_ttravplm = 0.0d0
      chi_ttravpan = 0.0d0
      chi_ttravaer = 0.0d0
      chi_ttravprm = 0.0d0
      deallocate(chi_ttravchm)
      allocate(chi_ttravchm(numrec,nsrc))
      chi_ttravchm(:,:) = 0.0d0
      deallocate(bldfac)
      allocate(bldfac(numrec,nsrc))
      bldfac(:,:) = 0.0d0
      PRMVAL_Src1 = 0.0d0
   end if

   if(runttrm2)then
      deallocate(ttrmcompare)
      allocate(ttrmcompare(ngrp,nsrc,numrec,numtyp))
      ttrmcompare(:,:,:,:) = 0.0d0
   end if

! --- Copy standard receptor arrays to "saved" arrays
   axr_sav = axr
   ayr_sav = ayr
   azelev_sav = azelev
   azhill_sav = azhill
   azflag_sav = azflag

!     For buoyant line sources - save results of first tranlation/rotation
   if (l_blsource) then
      xr_scs_sav = xr_scs
      yr_scs_sav = yr_scs
      bl_rflag_sav = bl_rflag
   end if

! --- Save original start and end dates (ISDATE and IEDATE)
!     for processing messages in TERRST; these variables
!     include 4-digit year (for comparisons to FULLDATE)
   isdate_sav = isdate
   iedate_sav = iedate


! --- Initialized SUMVAL_MAXD array for max daily contributions
   sumval_maxd = 0.0d0

! --- Set logical flag to skip messages while re-processing
!     the meteorological, BACKGRND, and ozone data
   L_SkipMessages = .true.

! --- Loop through "target" source groups for
!     max daily contributions option (OU MAXDCONT)
   do jgrp = 1, numgrp

      if (maxdcont(jgrp) > 0) then
! ---       Max daily contribution results selected
!           for this source group

! ---       Loop through user-specified ranks for max daily
!           contribution analysis for this source group
         do ival = mxd_rank(jgrp,1), mxd_rank(jgrp,2)

! ---          Write message to screen indicating current Rank
            write(*,909) grpid(jgrp), ival
909         format&
            &('+','Now Processing MAXDCONT for Group ',a8,' and Rank No. ',i4)

            do ir = 1, nrec
! ---             Loop through receptors; but skip receptor if
!                 value is below user-specified threshold
               if (sumhnh(ir,jgrp,ival) <= 0.0d0 .or.&
               &sumhnh(ir,jgrp,ival) < maxd_thresh(jgrp)) cycle

!                  IF (BL_RFLAG(IR) .EQ. .true.) CYCLE

! ---             Assign data from the "saved" arrays
!                 to array index 1 for standard arrays
               axr(1) = axr_sav(ir)
               ayr(1) = ayr_sav(ir)
               azelev(1) = azelev_sav(ir)
               azhill(1) = azhill_sav(ir)
               azflag(1) = azflag_sav(ir)
! (Multiple_BuoyLines_D41_Wood)
!                 Added a second dimension to represent BL groups
               if (l_blsource) then
                  xr_scs(1,1:numblgrps) = xr_scs_sav(ir,1:numblgrps)
                  yr_scs(1,1:numblgrps) = yr_scs_sav(ir,1:numblgrps)
                  bl_rflag(1,1:numblgrps) =&
                  &bl_rflag_sav(ir,1:numblgrps)
               end if
! ---             Loop through number of years processed for
!                 max daily contribution analysis
               do i = 1, numyrs
! ---                Assign "start date" and "end date" for
!                    max daily 1-hr value associated with
!                    this rank
                  iedate = nhidatmxd_byyr(ir,jgrp,ival,i)
!                    Convert start date from 8-digits to 10-digits
                  icyr2  = iedate/1000000

!       D001 Call LONG_DATE and determine the longform date Wood 9/15/22
                  call long_date(nhidatmxd_byyr(ir,jgrp,ival,i),iedate,icyr2,icyr)

! ---  D001 remove original calculation of year Wood 9/15/22
!                     IF (ICYR2 .GE. ISTRT_WIND .and.
!     &                                    ICYR2 .LE. 99) THEN
!                        ICYR   = ISTRT_CENT*100 + ICYR2
!                        IEDATE = ISTRT_CENT*100000000 +
!     &                           NHIDATMXD_BYYR(IR,JGRP,IVAL,I)
!                     ELSE IF (ICYR2 .LT. ISTRT_WIND) THEN
!                        ICYR   = (ISTRT_CENT+1)*100 + ICYR2
!                        IEDATE = (ISTRT_CENT+1)*100000000 +
!     &                           NHIDATMXD_BYYR(IR,JGRP,IVAL,I)
!                     END IF

                  if (no2ave .or. so2ave) then
! ---                   Assign start date to end date for NO2 or SO2
!                       since we're only processing 1 hour at a time
                     isdate = iedate
                  else if (pm25ave) then
! ---                   Subtract 23 from end date for PM2.5 since
!                       these will always be 24-hour averages
                     isdate = iedate - 23
                  end if

                  do fulldate = isdate, iedate
! ---                   Calculate IHR_NDX, hour-of-year array index
!                       to extract met data for MAXDCONT option,
!                       based on FULLDATE.
                     imn = (fulldate/10000) -&
                     &(fulldate/1000000)*100
                     idy = (fulldate/100) -&
                     &(fulldate/10000)*100
                     ihr =  fulldate - (fulldate/100)*100
                     call julian(icyr,imn,idy,ijdy)

! ---                   Determine julian day for current year based on
!                       orignial start month/day
                     call julian(icyr,ismn,isdy,icjday)


! ---                   Check for invalid Julian days (=0), indicating a
!                       potential problem with the NHIDATMXD_BYYR array.
!                       An error message will already have been issued in
!                       subroutine Julian, but exit loop to avoid Fortran
!                       runtime errors since IHR_NDX may not be valid.
                     if (ijdy == 0 .or. icjday == 0) then
                        exit
                     end if

                     if (isjday == 1) then
! ---                      Data starts on Jan. 1
                        ihr_ndx = 24*(ijdy-isjday)+(ihr-ishr)+1
                     else if (ijdy >= icjday) then
! ---                      Data does not start on Jan. 1, but "event"
!                          jday is .ge. start jday
                        ihr_ndx = 24*(ijdy-icjday)+(ihr-ishr)+1
                     else
! ---                      Data does not start on Jan. 1, and "event"
!                          jday is .lt. start jday; calculation of the
!                          "hour-of-year" index must account for potential
!                          influence of leap year.
! ---                      Determine julian day for previous year based on
!                          start month/day
                        call julian(icyr-1,ismn,isdy,ipjday)
                        if (ipjday > icjday) then
! ---                        Account for leap year
                           ihr_ndx = 24*(ijdy-ipjday+366)+(ihr-ishr)+1
                        else
! ---                        No leap year adjustment needed
                           ihr_ndx = 24*(ijdy-ipjday+365)+(ihr-ishr)+1
                        end if
                     end if
! ---                   Assign year arrary index for extracting met data
                     iyr_ndx = i

! ---                   Set other global date variables for this hour
                     imonth = imn
                     iday   = idy
                     ihour  = ihr
                     iyear  = icyr2    ! 2-digit year
                     iyr    = icyr     ! 4-digit year
                     kurdat = icyr2*1000000 + imonth*10000 +&
                     &iday*100 + ihour
                     kurpfl = kurdat

! ---                   Call MAXDCALC subroutine to calculate
!                       max daily contributions for this hour
                     call maxdcalc

                  end do

                  do ig = 1, numgrp
! ---                   Loop through source groups to determine
!                       contributions to target source group
                     sumval_maxd(ival,ig,jgrp,ir) =&
                     &sumval_maxd(ival,ig,jgrp,ir) +&
                     &aveval(1,ig,1,1)
                  end do

! ---                Reinitialize AVEVAL array
                  aveval(:,:,:,:) = 0.0d0
! ---                Reinitialize NUMHRS, NUMCLM and NUMMSG
                  numhrs(:) = 0
                  numclm(:) = 0
                  nummsg(:) = 0
!                    Initialize __VAL arrays (1:NUMTYP)
                  hrval   = 0.0d0
                  aerval  = 0.0d0
!
                  if (allocated(chi)) chi(:,:,:) = 0.0d0
                  if(grsm)then
                     chi_ttravplm = 0.0d0
                     chi_ttravpan = 0.0d0
                     chi_ttravaer = 0.0d0
                     chi_ttravprm = 0.0d0
                     chi_ttravchm(:,:) = 0.0d0
                     bldfac(:,:) = 0.0d0
                     PRMVAL_Src1 = 0.0d0
                  end if
                  if(runttrm2)then
                     ttrmcompare(:,:,:,:) = 0.0d0
                  endif

               end do  ! end loop on years

               do ig = 1, numgrp
! ---                Divide sumval_maxd results by number of years
!                    to get averages across number of years modeled
                  sumval_maxd(ival,ig,jgrp,ir) =&
                  &sumval_maxd(ival,ig,jgrp,ir)/&
                  &dble(numyrs)
               end do

! ---             Check for consistency of SUMVAL_MAXD and SUMHNH arrays for
!                 "target" source group under MAXDCONT option
               if (sumhnh(ir,jgrp,ival) > 0.0d0) then
                  if( dabs( (sumval_maxd(ival,jgrp,jgrp,ir) -&
                  &sumhnh(ir,jgrp,ival)) )/&
                  &sumhnh(ir,jgrp,ival) > 5.0d-6) then
! ---                   Arrays don't match; issue warning message indicating
!                       potential coding error. A warning to the default
!                       output unit will also be issued at the end of the run.
                     write(dummy,'("G",I3.3,"R",I2.2,"R",I4.4)')&
                     &min(jgrp,999), min(ival,99), min(ir,9999)
                     call errhdl(path,modnam,'W','498',dummy)
                     L_MAXDCONT_OrigConc_Warning = .true.
                  end if
               end if
!
            end do   ! end loop over receptors

! ---          Call subroutine to write MAXDCONT output file
!              for this source group and rank
            call maxdcnt_file(jgrp,ival)

! ---          Check for value below threshold for MAXDCONT
            if (maxval(sumhnh(1:nrec,jgrp,ival)) <&
            &maxd_thresh(jgrp)) then
! ---             All values for this rank are below threshold;
!                 reset upper bound of ranks to process, set flag
!                 to indicate that threshold was reached, and
!                 EXIT the loop over ranks
               mxd_rank(jgrp,2) = ival
               hit_thresh = .true.
               exit
            end if

         end do   ! end loop over ranks

! ---       Check for whether MAXD_THRESH specified by user was
!           not reached
         if (maxd_thresh(jgrp) > 0.0d0 .and.&
         &.not.hit_thresh) then
! ---          User-specified threshold was not reached within the
!              range of ranks analyzed, based on the RECTABLE keyword;
!              issue warning message
            write(dummy,'(''GRP '',A8)') grpid(jgrp)
            call errhdl(path,modnam,'W','415',dummy)
         end if
! ---       Reset HIT_THRESH flag for next source group
         hit_thresh = .false.

      end if

   end do    ! end loop over source groups

! --- Copy saved receptor arrays to standard arrays
   axr = axr_sav
   ayr = ayr_sav
   azelev = azelev_sav
   azhill = azhill_sav
   azflag = azflag_sav

   if (l_blsource) then
      xr_scs = xr_scs_sav
      yr_scs = yr_scs_sav
      bl_rflag = bl_rflag_sav
   end if

! --- Reset number of receptors
   numrec = nrec
! --- Reset original start and end dates
   isdate = isdate_sav
   iedate = iedate_sav

!     JAT 9/20/18 added from 18081
!     reset CHI to original dimensions
   if (allocated(chi)) then
      deallocate(chi)
      allocate(chi(nrec,nsrc,numtyp))
      chi(:,:,:) = 0.0d0
   endif
   if(grsm)then
      chi_ttravplm = 0.0d0
      chi_ttravpan = 0.0d0
      chi_ttravaer = 0.0d0
      chi_ttravprm = 0.0d0
      deallocate(chi_ttravchm)
      allocate(chi_ttravchm(nrec,nsrc))
      chi_ttravchm(:,:) = 0.0d0
      deallocate(bldfac)
      allocate(bldfac(nrec,nsrc))
      bldfac(:,:) = 0.0d0
      PRMVAL_Src1 = 0.0d0
   end if

   if(runttrm2)then
      deallocate(ttrmcompare)
      allocate(ttrmcompare(ngrp,nsrc,nrec,nrec))
      ttrmcompare(:,:,:,:) = 0.0d0
   end if

   return
end subroutine maxdcont_loop

subroutine maxd_metext
!***********************************************************************
!                MAXD_METEXT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls extraction of meteorological data from
!                 arrays for use with OU MAXDCONT option
!
!        PROGRAMMER: Roger W. Brode
!
!        DATE:       February 28, 2011
!
!        MODIFIED:   Modified to PG stability parameter for the buoyant
!                    line source
!                    Amec Foster Wheeler, 06/30/2015
!
!        INPUTS:
!
!        OUTPUTS:
!
!        CALLED FROM:   MAXDCALC
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12
   integer :: i

!     Variable Initializations
   modnam = 'MAXD_METEXT'
   path   = 'MX'

!     Save Value of Last YR/MN/DY/HR and Previous Hour
   ipdate = kurdat
   ipyear = iyr
   iphour = ihour

!     Set Meteorological Variables for This Hour
   sfchf  = asfchf(ihr_ndx,iyr_ndx)
   uref   = auref(ihr_ndx,iyr_ndx)
   urefht = aurefht(ihr_ndx,iyr_ndx)
   ta     = ata(ihr_ndx,iyr_ndx)
   trefht = atrefht(ihr_ndx,iyr_ndx)
   wdref  = awdref(ihr_ndx,iyr_ndx)
   ustar  = austar(ihr_ndx,iyr_ndx)
   wstar  = awstar(ihr_ndx,iyr_ndx)
   ziconv = aziconv(ihr_ndx,iyr_ndx)
   zimech = azimech(ihr_ndx,iyr_ndx)
   obulen = aobulen(ihr_ndx,iyr_ndx)
   vptgzi = avptgzi(ihr_ndx,iyr_ndx)
   sfcz0  = asfcz0(ihr_ndx,iyr_ndx)
   kst    = nint( akst(ihr_ndx,iyr_ndx) )
   blta   = ablta(ihr_ndx,iyr_ndx)
! Added for HBP; JAN. 2023
   if (hbplume) then
      ziconvn = aziconvn(ihr_ndx,iyr_ndx)
      zimechn = azimechn(ihr_ndx,iyr_ndx)
   endif
! End HBP insert
   if (ldgas .or. ldpart .or. lwpart .or. lwgas .or. grsm) then
      bowen  = abowen(ihr_ndx,iyr_ndx)
      albedo = aalbedo(ihr_ndx,iyr_ndx)
      ipcode = iapcode(ihr_ndx,iyr_ndx)
      prate  = aprate(ihr_ndx,iyr_ndx)
      rh     = arh(ihr_ndx,iyr_ndx)
      sfcp   = asfcp(ihr_ndx,iyr_ndx)
      ncloud = nacloud(ihr_ndx,iyr_ndx)
      qsw    = aqsw(ihr_ndx,iyr_ndx)
      Wnew   = AWnew(ihr_ndx,iyr_ndx)
      f2     = Af2(ihr_ndx,iyr_ndx)
      EsTa   = AEsTa(ihr_ndx,iyr_ndx)
      Prec1  = APrec1(ihr_ndx,iyr_ndx)
      Prec2  = APrec2(ihr_ndx,iyr_ndx)
   end if

   rurustr   = arurustr(ihr_ndx,iyr_ndx)
   rurobulen = arurobulen(ihr_ndx,iyr_ndx)

   clmhr = aclmhr(ihr_ndx,iyr_ndx)
   msghr = amsghr(ihr_ndx,iyr_ndx)

   unstab  = aunstab(ihr_ndx,iyr_ndx)
   stable  = astable(ihr_ndx,iyr_ndx)
   urbstab = aurbstab(ihr_ndx,iyr_ndx)

   ndx4zi = andx4zi(ihr_ndx,iyr_ndx)
   uatzi  = auatzi(ihr_ndx,iyr_ndx)
   svatzi = asvatzi(ihr_ndx,iyr_ndx)
   swatzi = aswatzi(ihr_ndx,iyr_ndx)
   uavg   = auavg(ihr_ndx,iyr_ndx)
   svavg  = asvavg(ihr_ndx,iyr_ndx)
   swavg  = aswavg(ihr_ndx,iyr_ndx)
   ptatzi = aptatzi(ihr_ndx,iyr_ndx)

   gridwd(1:mxglvl)  = agridwd(ihr_ndx,1:mxglvl,iyr_ndx)
   gridws(1:mxglvl)  = agridws(ihr_ndx,1:mxglvl,iyr_ndx)
   gridsw(1:mxglvl)  = agridsw(ihr_ndx,1:mxglvl,iyr_ndx)
   gridsv(1:mxglvl)  = agridsv(ihr_ndx,1:mxglvl,iyr_ndx)
   gridtg(1:mxglvl)  = agridtg(ihr_ndx,1:mxglvl,iyr_ndx)
   gridpt(1:mxglvl)  = agridpt(ihr_ndx,1:mxglvl,iyr_ndx)
   if (nsec > 0) then
      gridrho(1:mxglvl) = agridrho(ihr_ndx,1:mxglvl,iyr_ndx)
   end if
   if (pvmrm .or. grsm) then
      grideps(1:mxglvl) = agrideps(ihr_ndx,1:mxglvl,iyr_ndx)
   end if

   if (nurb > 0) then
      grdswr(1:mxglvl) = agrdswr(ihr_ndx,1:mxglvl,iyr_ndx)
      grdsvr(1:mxglvl) = agrdsvr(ihr_ndx,1:mxglvl,iyr_ndx)
      grdtgr(1:mxglvl) = agrdtgr(ihr_ndx,1:mxglvl,iyr_ndx)
      grdptr(1:mxglvl) = agrdptr(ihr_ndx,1:mxglvl,iyr_ndx)

      do i = 1, nurb
         grdswu(1:mxglvl,i) = agrdswu(ihr_ndx,1:mxglvl,iyr_ndx,i)
         grdsvu(1:mxglvl,i) = agrdsvu(ihr_ndx,1:mxglvl,iyr_ndx,i)
         grdtgu(1:mxglvl,i) = agrdtgu(ihr_ndx,1:mxglvl,iyr_ndx,i)
         grdptu(1:mxglvl,i) = agrdptu(ihr_ndx,1:mxglvl,iyr_ndx,i)
         ziurb(i)           = aziurb(ihr_ndx,iyr_ndx,i)
         urbwstr(i)         = aurbwstr(ihr_ndx,iyr_ndx,i)
         urbustr(i)         = aurbustr(ihr_ndx,iyr_ndx,i)
         urbobulen(i)       = aurbobulen(ihr_ndx,iyr_ndx,i)
         L_MorningTrans(i)  = AL_MorningTrans(ihr_ndx,iyr_ndx,i)
      end do
   end if

   if (.not.clmhr .and. .not.msghr) then
! ---    Set Meteorological Variables for Current Hour
      call set_metdata
   end if

   return
end subroutine maxd_metext

subroutine ahrqread (is)
!***********************************************************************
!*                  AHRQREAD Module of AERMOD
!*
!*         PURPOSE: To Assign Hourly Source Parameters
!*
!*         PROGRAMMER:  Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!*                      M.K. Porter EPA/OAQPS/AQMG
!*                      M.G. Snyder WSP (updated/streamlined on 6/1/23)
!*
!*         DATE:    5/23/2023
!*
!*         INPUTS:  Current AIRCRAFT Source Number Being Processed
!*
!*         OUTPUTS: Source Arrays
!*
!*         Revision History:
!*
!*         MODIFIED:  Streamlined original AHRQREAD to only handle aicraft
!*                    sources. M.G. Snyder 6/1/2023
!*
!*         MODIFIED:  Modified to issue fatal error when aircraft plume
!*                    is turned on and hourly parameters are missing for
!*                    area and/or volume sources.  AHRQREAD was adapted
!*                    from HRQREAD originally submitted by UNC-IE.
!*                    M.K. Porter EPA/OAQPS/AQMG 5/23/2023
!*
!*         MODIFIED:  Modified HRQREAD to read the Aircraft Engine parameters to
!*                    calculate the  plume rise for VOLUME/AREA Source
!*                    only for Aircraft Source Group.
!*                    Gavendra Pandey; UNC-IE, Chapel Hill, NC, USA
!*                    04/01/2023
!*
!*         STARTED WITH HRQREAD (last modified 03/18/2019)
!*
!*         CALLED FROM:  HRLOOP
!************************************************************************
!*
!*    Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, is
   integer :: ihyear, ihmon, ihday, ihhour, ihyear2
   integer :: ilsave
   character (len=20) :: rdfrm

   character (len=12) :: hrsoid

!*    Variable Initializations
   modnam = 'AHRQREAD'

!* Catch any sourcetypes not defined as aircraft
   if ((srctyp(is)(1:5) == 'POINT') .or.&
   &(srctyp(is) == 'OPENPIT') .or.&
   &(srctyp(is) == 'LINE') .or.&
   &(srctyp(is) == 'RLINE') .or.&
   &(srctyp(is) == 'RLINEXT') .or.&
   &(srctyp(is) == 'BUOYLINE')) then
!*       These sources are not defined under aircraft category
!*       - WRITE Error Message
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','833','AFTSRC')
      runerr = .true.
      go to 9990
   end if

!*    Assign IQLINE counter to ILINE for passing to ERRHDL if needed, save as ILSAVE first
   ilsave = iline
   iline  = iqline

!*    READ Record to Buffers, A'num' and 'num'A1, where num=ISTRG
!*    Length of ISTRG is Set in PARAMETER Statement in MAIN1
!     Setup READ format and ECHO format for runstream record,
!     based on the ISTRG PARAMETER (set in MAIN1)
   write(rdfrm,9100) istrg, istrg
9100 format('(A',i4.4,',T1,',i4.4,'A1)')
   read (ihremi,rdfrm,end=888,err=99) runst1, (runst(i), i=1, istrg)
!*
!*    Convert Lower Case to Upper Case Letters              ---   CALL LWRUPR
   call lwrupr
!*
!*    Define Fields on Card                                 ---   CALL DEFINE
   call define
!*
!*    Get the Contents of the Fields                        ---   CALL GETFLD
   call getfld
!*
!*    Check for number of fields - error if less than 7.
   if (ifc < 7) then
      write(dummy,'(I8)') kurdat
      call errhdl(path,modnam,'E','384',dummy)
      runerr = .true.
      go to 9990
   end if
!*
!*    Assign the Fields to Local Varables and Check The Numerical Field
!*
!*    Date and time variables common to all source types
!*
   call stonum(field(3), ilen_fld, fnum, imit)
   ihyear = nint(fnum)
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208','HOUREMIS')
      runerr = .true.
      go to 9990
   end if

   call stonum(field(4), ilen_fld, fnum, imit)
   ihmon = nint(fnum)
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208','HOUREMIS')
      runerr = .true.
      go to 9990
   end if

   call stonum(field(5), ilen_fld, fnum, imit)
   ihday = nint(fnum)
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208','HOUREMIS')
      runerr = .true.
      go to 9990
   end if

   call stonum(field(6), ilen_fld, fnum, imit)
   ihhour = nint(fnum)
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208','HOUREMIS')
      runerr = .true.
      go to 9990
   end if

! --- Check for use of 2-digit year in HOUREMIS file, adjust to 4-digit
!     year for comparison with FULLDATE based on met data file
   if (ihyear <= 99) then
      ihyear2 = ihyear
      if (ihyear2 >= istrt_wind .and.&
      &ihyear2 <= 99) then
         ihyear = istrt_cent*100 + ihyear2
      else if (ihyear2 < istrt_wind) then
         ihyear = (istrt_cent+1)*100 + ihyear2
      end if
   end if

! --- Calculate current date (YYYYMMDDHH) from HOUREMIS file record, FULLHRQ
   fullhrq = ihyear*1000000 + ihmon*10000 + ihday*100 + ihhour

! --- Assign source ID but check for field length > 12 first
   if( len_trim(field(7)) <= 12 ) then
      hrsoid = field(7)
   else
      hrsoid = field(7)(1:12)
   end if

!*    Check for Source ID Consistency ; If Failed Issue Error
   if ( hrsoid /= srcid(is) ) then
      write(dummy,'(A12)') srcid(is)
      call errhdl(path,modnam,'E','342',srcid(is))
      runerr = .true.
      go to 9990
   end if

   if (ifc == 7) then
!*       All parameters missing for this hour/source - WRITE Warning Message
!*       Assign zeros to all parameters
      if (.not. L_SkipMessages) then
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','344',dummy)
      end if
!**  Added for Aircraft Plume Rise; UNC-IE
!MGS         IF ( AFTSRC(IS) .EQ. 'Y') THEN !already KNOW it is an aircraft source
      hrqs = 0.0d0 !added by MGS 6/1/23
      hrmfuel    = 0.0d0
      hrthrust   = 0.0d0
      hrvaa      = 0.0d0
      hrafr      = 0.0d0
      hrbypr     = 0.0d0
      hrrpwr     = 0.0d0
      hrsrcangle = 0.0d0
!MGS         END IF

! -------------------------- Begin too few parameters for source type
!MKP Check for missing fields for volume and area sources when Aircraft
! Plume Rise is specified.  Minimum of 15 fields required without HRLYSIG.
! Aircraft new fields (7 total): MFUEL,THRUST,VAA,AFR,BYPR,RPWR,SRCANGLE
   else if ((srctyp(is) == 'VOLUME') .and.&
   &(ifc <= 11)) then
      call errhdl(path,modnam,'E','807',srcid(is))
      runerr = .true.
      go to 9990
!MGS Changed check to only check first 4 characters for AREA (D151 - 6/1/23)
!MGS      ELSE IF (SRCTYP(IS) .EQ. 'AREA' .and.
   else if ((srctyp(is)(1:4) == 'AREA') .and.&
   &(ifc <= 10)) then
      call errhdl(path,modnam,'E','807',srcid(is))
      runerr = .true.
      go to 9990
! -------------------------- End too few parameters for source type

! -------------------------- Begin correct # parameters for source type
   else if ((srctyp(is) == 'VOLUME') .and.&
   &(ifc == 18)) then
!*      Assign emission rate, release height, initial sigmas and
!*      Engine parameters for aircraft sources plume rise
!*      for VOLUME source.
!*       Assign logical variable indicating hourly sigmas, L_HRLYSIG
      if (ilsave == 1) then
         l_hrlysig(is) = .true.
      else if (ilsave > 1 .and. .not. l_hrlysig(is)) then
!*          This volume source should not include hourly sigmas;
!*          issue error message
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','345',dummy)
         hrqs = 0.0d0
         runerr = .true.
         go to 9990
      end if

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      call stodbl(field(9), ilen_fld, hrhs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
      end if

      call stodbl(field(10), ilen_fld, hrsy, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
      end if
      call stodbl(field(11), ilen_fld, hrsz, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
      end if

      call stodbl(field(12), ilen_fld, hrmfuel, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrmfuel < 0.0d0 ) then
!*          Assume fuel burn rates are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrmfuel = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','826',dummy)
      end if

      call stodbl(field(13), ilen_fld, hrthrust, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrthrust < 0.0d0 ) then
!*          Assume thrusts are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrthrust = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','827',dummy)
      end if

      call stodbl(field(14), ilen_fld, hrvaa, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrvaa < 0.0d0 ) then
!*          Assume aircraft velocities are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrvaa = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','828',dummy)
      end if
      call stodbl(field(15), ilen_fld, hrafr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrafr < 0.0d0 ) then
!*          Assume air fuel ratios are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrafr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','829',dummy)
      end if

      call stodbl(field(16), ilen_fld, hrbypr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator,
! ---    whereas BYPR = -999 is for shaft-based engines aircraft and
! ---    BYPR greater than 0 for turbine-based engines aircraft
      else if ( hrbypr < 0.0d0 .and. hrbypr /= -999.0d0 ) then
!*          Assume bypass ratios are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrbypr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','830',dummy)
      end if

      call stodbl(field(17), ilen_fld, hrrpwr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator,
! ---    whereas RPWR = -99999 is for turbine-based engines aircraft
! ---    and greater than 0.0 for shaft-based engines aircraft
      else if ( hrrpwr < 0.0d0 .and. hrrpwr /= -99999.0d0 )then
!*          Assume rated power are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrrpwr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','831',dummy)
      end if

      call stodbl(field(18), ilen_fld, hrsrcangle, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for unrealistics or negative values, could be a missing indicator,
      else if ( hrsrcangle < -20.0d0 .or.&
      &hrsrcangle >  20.0d0 ) then
!*          Assume source angles are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrsrcangle = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','832',dummy)
      end if

   else if ((srctyp(is) == 'VOLUME') .and.&
   &(ifc == 15)) then
!*       Assign emission rate and engine parameters for
!*       Aircraft volume sources
!*       Check logical variable indicating hourly sigmas, L_HRLYSIG
      if (l_hrlysig(is)) then
!*          WRITE Error Message; Hourly Sigmas must be used for all hours
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','345',dummy)
         hrqs = 0.0d0
         runerr = .true.
         go to 9990
      end if

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      call stodbl(field(9), ilen_fld, hrmfuel, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrmfuel < 0.0d0 ) then
!*          Assume fuel burn rates are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrmfuel = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','826',dummy)
      end if

      call stodbl(field(10), ilen_fld, hrthrust, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrthrust < 0.0d0 ) then
!*          Assume thrusts are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrthrust = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','827',dummy)
      end if

      call stodbl(field(11), ilen_fld, hrvaa, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrvaa < 0.0d0 ) then
!*          Assume aircraft velocities are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrvaa = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','828',dummy)
      end if

      call stodbl(field(12), ilen_fld, hrafr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrafr < 0.0d0 ) then
!*          Assume air fuel ratios are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrafr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','829',dummy)
      end if

      call stodbl(field(13), ilen_fld, hrbypr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator,
! ---    whereas BYPR = -999 is for shaft-based engines aircraft and
! ---    BYPR greater than 0 for turbine-based engines aircraft
      else if ( hrbypr < 0.0d0 .and. hrbypr /= -999.0d0 ) then
!*          Assume bypass ratios are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrbypr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','830',dummy)
      end if

      call stodbl(field(14), ilen_fld, hrrpwr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator,
! ---    whereas RPWR = -99999 is for turbine-based engines aircraft
! ---    and greater than 0.0 for shaft-based engines aircraft
      else if ( hrrpwr < 0.0d0 .and. hrrpwr /= -99999.0d0 )then
!*          Assume rated power are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrrpwr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','831',dummy)
      end if

      call stodbl(field(15), ilen_fld, hrsrcangle, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for unrealistics or negative values, could be a missing indicator,
      else if ( hrsrcangle < -20.0d0 .or.&
      &hrsrcangle >  20.0d0 ) then
!*          Assume source angles are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrsrcangle = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','832',dummy)
      end if

   else if ((srctyp(is)(1:4) == 'AREA') .and.&
   &(ifc==17)) then
!*       Assign emission rate, release height, initial sigma and
!*      Engine parameters for aircraft sources plume rise
!*       for Area source.
!*       Assign logical variable indicating hourly sigmas, L_HRLYSIG
      if (ilsave == 1) then
         l_hrlysig(is) = .true.
      else if (ilsave > 1 .and. .not. l_hrlysig(is)) then
!*          This Area source should not include hourly sigmas;
!*          issue error message
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','345',dummy)
         hrqs = 0.0d0
         runerr = .true.
         go to 9990
      end if

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      call stodbl(field(9), ilen_fld, hrhs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
      end if

      call stodbl(field(10), ilen_fld, hrsz, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
      end if

      call stodbl(field(11), ilen_fld, hrmfuel, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrmfuel < 0.0d0 ) then
!*          Assume fuel burn rates are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrmfuel = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','826',dummy)
      end if

      call stodbl(field(12), ilen_fld, hrthrust, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrthrust < 0.0d0 ) then
!*          Assume thrusts are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrthrust = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','827',dummy)
      end if

      call stodbl(field(13), ilen_fld, hrvaa, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrvaa < 0.0d0 ) then
!*          Assume aircraft velocities are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrvaa = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','828',dummy)
      end if

      call stodbl(field(14), ilen_fld, hrafr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrafr < 0.0d0 ) then
!*          Assume air fuel ratios are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrafr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','829',dummy)
      end if

      call stodbl(field(15), ilen_fld, hrbypr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator,
! ---    whereas BYPR = -9990 is for shaft-based engines aircraft and
! ---    BYPR greater than 0 for turbine-based engines aircraft
      else if ( hrbypr < 0.0d0 .and. hrbypr /= -999.0d0 ) then
!*          Assume bypass ratios are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrbypr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','830',dummy)
      end if

      call stodbl(field(16), ilen_fld, hrrpwr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator,
! ---    whereas RPWR = -999099 is for turbine-based engines aircraft
! ---    and greater than 0.0 for shaft-based engines aircraft
      else if ( hrrpwr < 0.0d0 .and. hrrpwr /= -99999.0d0 )then
!*          Assume rated power are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrrpwr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','831',dummy)
      end if

      call stodbl(field(17), ilen_fld, hrsrcangle, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for unrealistics or negative values, could be a missing indicator,
      else if ( hrsrcangle < -20.0d0 .or.&
      &hrsrcangle >  20.0d0 ) then
!*          Assume source angles are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrsrcangle = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','832',dummy)
      end if

   else if ((srctyp(is)(1:4) == 'AREA') .and.&
   &(ifc==15)) then
!*       Assign emission rate and engine parameters for
!*       Aircraft Area sources
!*       Check logical variable indicating hourly sigmas, L_HRLYSIG
      if (l_hrlysig(is)) then
!*          WRITE Error Message; Hourly Sigmas must be used for all hours
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','345',dummy)
         hrqs = 0.0d0
         runerr = .true.
         go to 9990
      end if

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      call stodbl(field(9), ilen_fld, hrmfuel, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrmfuel < 0.0d0 ) then
!*          Assume fuel burn rates are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrmfuel = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','826',dummy)
      end if

      call stodbl(field(10), ilen_fld, hrthrust, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrthrust < 0.0d0 ) then
!*          Assume thrusts are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrthrust = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','827',dummy)
      end if

      call stodbl(field(11), ilen_fld, hrvaa, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrvaa < 0.0d0 ) then
!*          Assume aircraft velocities are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrvaa = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','828',dummy)
      end if

      call stodbl(field(12), ilen_fld, hrafr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator
      else if ( hrafr < 0.0d0 ) then
!*          Assume air fuel ratios are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrafr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','829',dummy)
      end if

      call stodbl(field(13), ilen_fld, hrbypr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator,
! ---    whereas BYPR = -9990 is for shaft-based engines aircraft and
! ---    BYPR greater than 0 for turbine-based engines aircraft
      else if ( hrbypr < 0.0d0 .and. hrbypr /= -999.0d0 ) then
!*          Assume bypass ratios are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrbypr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','830',dummy)
      end if

      call stodbl(field(14), ilen_fld, hrrpwr, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for negative values, could be a missing indicator,
! ---    whereas RPWR = -99999 is for turbine-based engines aircraft
! ---    and greater than 0.0 for shaft-based engines aircraft
      else if ( hrrpwr < 0.0d0 .and. hrrpwr /= -99999.0d0 )then
!*          Assume rated power are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrrpwr = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','831',dummy)
      end if

      call stodbl(field(15), ilen_fld, hrsrcangle, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for unrealistics or negative values, could be a missing indicator,
      else if ( hrsrcangle < -20.0d0 .or.&
      &hrsrcangle >  20.0d0 ) then
!*          Assume source angles are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrsrcangle = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','832',dummy)
      end if
! -------------------------- End of correct # parameters for source type

! -------------------------- Begin too many parameters for source type
!**  Added for Aircraft Plume Rise; UNC-IE
   else if ( srctyp(is) == 'VOLUME' .and.&
   &ifc > 18 .or. ifc == 16 ) then
      if ( ifc > 16 ) then
         l_hrlysig(is) = .true.
      else if ( ifc == 16 ) then
         l_hrlysig(is) = .false.
      end if

      if ( ifc > 18 .and. l_hrlysig(is) ) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','825',dummy)
         hrqs = 0.0d0
         hrhs = 0.0d0
         hrsy = 0.0d0
         hrsz = 0.0d0
         hrmfuel = 0.0d0
         hrthrust = 0.0d0
         hrvaa = 0.0d0
         hrafr = 0.0d0
         hrbypr = 0.0d0
         hrrpwr = 0.0d0
         hrsrcangle = 0.0d0
      else if ( ifc >= 16 .and..not.l_hrlysig(is) ) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','825',dummy)
         hrqs = 0.0d0
         hrmfuel = 0.0d0
         hrthrust = 0.0d0
         hrvaa = 0.0d0
         hrafr = 0.0d0
         hrbypr = 0.0d0
         hrrpwr = 0.0d0
         hrsrcangle = 0.0d0
      end if
      runerr = .true.
!**  End Aircraft Plume Rise insert; April 2023

!**  Added for Aircraft Plume Rise; UNC-IE
   else if ( srctyp(is) (1:4) == 'AREA' .and.&
   &ifc > 17 .or. ifc == 16 ) then
      if ( ifc > 16 ) then
         l_hrlysig(is) = .true.
      else if ( ifc == 16 ) then
         l_hrlysig(is) = .false.
      end if

      if ( ifc > 17 .and. l_hrlysig(is) ) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','825',dummy)
         hrqs = 0.0d0
         hrhs = 0.0d0
         hrsz = 0.0d0
         hrmfuel = 0.0d0
         hrthrust = 0.0d0
         hrvaa = 0.0d0
         hrafr = 0.0d0
         hrbypr = 0.0d0
         hrrpwr = 0.0d0
         hrsrcangle = 0.0d0
      else if ( ifc >= 16 .and..not.l_hrlysig(is) ) then
!*       Too many parameters - WRITE Error Message
!*       Assign zeros to all parameters
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','825',dummy)
         hrqs = 0.0d0
         hrmfuel = 0.0d0
         hrthrust = 0.0d0
         hrvaa = 0.0d0
         hrafr = 0.0d0
         hrbypr = 0.0d0
         hrrpwr = 0.0d0
         hrsrcangle = 0.0d0
      end if
      runerr = .true.
!**  End Aircraft Plume Rise insert; April 2023
! -------------------------- End of too many parameters for source type

! -------------------------- Begin of too few parameters for source type
!**  Added for Aircraft Plume Rise; UNC-IE
   else if ( srctyp(is) == 'VOLUME' .and.&
   &ifc == 8 .or. ifc == 9 .or.&
   &ifc == 10 .or. ifc == 11) then

!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'W','824',dummy)

!*       Assign emission rate for volume sources

      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      hrmfuel = 0.0d0
      hrthrust = 0.0d0
      hrvaa = 0.0d0
      hrafr = 0.0d0
      hrbypr = 0.0d0
      hrrpwr = 0.0d0
      hrsrcangle = 0.0d0
!         RUNERR = .TRUE.

   else if ( srctyp(is) == 'VOLUME' .and.&
   &ifc == 17 .or. ifc == 14 .or.&
   &ifc == 13 .or. ifc == 12) then
      if ( ifc > 16 ) then
         l_hrlysig(is) = .true.
      else if ( ifc == 14 .or.&
      &ifc == 13 .or. ifc == 12) then
         l_hrlysig(is) = .false.
      end if

      if ( ifc < 18 .and. l_hrlysig(is) ) then
!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','824',dummy)
         hrqs = 0.0d0
         hrhs = 0.0d0
         hrsy = 0.0d0
         hrsz = 0.0d0
         hrmfuel = 0.0d0
         hrthrust = 0.0d0
         hrvaa = 0.0d0
         hrafr = 0.0d0
         hrbypr = 0.0d0
         hrrpwr = 0.0d0
         hrsrcangle = 0.0d0
      else if ( ifc >= 12 .and..not.l_hrlysig(is) ) then
!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','824',dummy)
         hrqs = 0.0d0
         hrmfuel = 0.0d0
         hrthrust = 0.0d0
         hrvaa = 0.0d0
         hrafr = 0.0d0
         hrbypr = 0.0d0
         hrrpwr = 0.0d0
         hrsrcangle = 0.0d0
      end if
      runerr = .true.
!**  End Aircraft Plume Rise insert; April 2023

!**  Added for Aircraft Plume Rise; UNC-IE
   else if ( srctyp(is) (1:4) == 'AREA' .and.&
   &ifc == 8 .or. ifc == 9   .or.&
   &ifc == 10 ) then

!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'W','824',dummy)

!*       Assign emission rate for area sources
      call stodbl(field(8), ilen_fld, hrqs, imit)
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208','HOUREMIS')
         runerr = .true.
         go to 9990
! ---    Check for large negative values, could be a missing indicator
      else if ( hrqs <= -90.0d0 ) then
!*          Assume emissions are missing; assign value of 0.0 and issue Warning;
!*          Note that AERMOD User's Guide (p. 3-38, 3-39) implies that blanks
!*          should be used for missing values.
         hrqs = 0.0d0
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'W','341',dummy)
      end if

      hrmfuel = 0.0d0
      hrthrust = 0.0d0
      hrvaa = 0.0d0
      hrafr = 0.0d0
      hrbypr = 0.0d0
      hrrpwr = 0.0d0
      hrsrcangle = 0.0d0
!         RUNERR = .TRUE.

   else if ( srctyp(is) (1:4) == 'AREA' .and.&
   &ifc == 16 .or. ifc == 14 .or.&
   &ifc == 13 .or. ifc == 12) then
      if ( ifc > 15 ) then
         l_hrlysig(is) = .true.
      else if ( ifc == 14 .or.&
      &ifc == 13 .or. ifc == 12) then
         l_hrlysig(is) = .false.
      end if

      if ( ifc < 17 .and. l_hrlysig(is) ) then
!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','824',dummy)
         hrqs = 0.0d0
         hrhs = 0.0d0
         hrsz = 0.0d0
         hrmfuel = 0.0d0
         hrthrust = 0.0d0
         hrvaa = 0.0d0
         hrafr = 0.0d0
         hrbypr = 0.0d0
         hrrpwr = 0.0d0
         hrsrcangle = 0.0d0
      else if ( ifc >= 12 .and..not.l_hrlysig(is) ) then
!*       Some missing parameters - WRITE Error Message
!*       Assign zeros to all parameters
         write(dummy,'(I10.10)') fullhrq
         call errhdl(path,modnam,'E','824',dummy)
         hrqs = 0.0d0
         hrmfuel = 0.0d0
         hrthrust = 0.0d0
         hrvaa = 0.0d0
         hrafr = 0.0d0
         hrbypr = 0.0d0
         hrrpwr = 0.0d0
         hrsrcangle = 0.0d0
      end if
      runerr = .true.
!**  End Aircraft Plume Rise insert; April 2023
! ---------------------------- End of too few parameters for source type

   else
!*       Problem processing HOUREMIS record - WRITE Error Message
!*       Assign zeros to emission rate
      write(dummy,'(I10.10)') fullhrq
      call errhdl(path,modnam,'E','345',dummy)
      hrqs = 0.0d0
      runerr = .true.
   end if

   go to 9990

!*    Write Error Message for Error Reading Hourly Emissions File
99 call errhdl(path,modnam,'E','510','HOUREMIS')
   runerr = .true.
   go to 9990

888 continue

   eof = .true.

9990 return
end subroutine ahrqread
