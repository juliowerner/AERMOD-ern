subroutine metext
!***********************************************************************
!                METEXT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Controls Extraction and Quality Assurance of
!                 One Hour of Meteorological Data
!
!        PROGRAMMER: ROGER BRODE, JEFF WANG
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
!
!        DATE:    November 8, 1993
!
!        Revision History:
!
!        MODIFIED:   Modified to store additional variables needed
!                    for MAXDCONT option when the URBAN option is
!                    being used.  Previous version could report
!                    erroneous source group contributions in the
!                    MAXDCONT file for any application that included
!                    urban sources.
!
!                    Modified to only increment hour index and year
!                    index if MAXDCONT option is being used. This
!                    change avoids unnecessary runtime error if the
!                    met data file includes a single hour beyond
!                    the maximum number of years specified by the
!                    NYEARS PARAMETER, which is initialized to 5 in
!                    the MAIN1 module.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 12/19/2011
!
!        MODIFIED:   Modified code for setting the month, day, and hour
!                    for the "end of the year", based on the first hour
!                    of the meteorological data file, to resolve potential
!                    problems for PM-2.5 and ANNUAL average applications
!                    in which the first hour of the file is not 01.
!
!                    Modified to include check for end-of-file (EOF) on
!                    first data record, and to assign file type for possible
!                    end-of-file error messages.
!
!                    Included code to check for presence of additional
!                    variables in the surface meteorological file needed
!                    for use of the deposition options when required.
!                    Also checks for minimum number of data fields for
!                    applications without deposition.
!
!                    Include checks for potential conflicts between
!                    first date of met data file being later than
!                    start date from re-start file or user-specified
!                    start date on STARTEND keyword, with fatal error
!                    message for conflict with INITFILE restarts and
!                    warning for MULTYEAR restarts.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Modified code for processing multi-year data
!                    files to determine if header record is present
!                    between years for concatenated files.  Use presence
!                    of colon (':') as only criterion for header record.
!                    Use warning messages if UAIR and SURF IDs don't
!                    match input runstream file for multiple years since
!                    AERMOD allows mismatch for single year files.
!                    Modified check for stable or missing hours in
!                    calculation of solar irradiance (QSW) for use
!                    in deposition calculations.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   Modified code for reading the header record of the
!                    surface file to use a character variable for the
!                    AERMET version date field, in order to allow for
!                    the future use of screening meteorology that is not
!                    directly linked to a specific version under the
!                    of the AERMET processor under the SCREEN option.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
!        (DATE:    February 15, 1993)
!
!        MODIFIED:   To avoid potential math error due to negative
!                    ambient temperatures in calculating the square
!                    root of the stability parameter, RTOFS - 4/19/93
!
!        MODIFIED:
!        7/27/94     J. Paumier, PES, Inc.
!                    The variables for displacement height, ZDM and
!                    AZDM(), were removed from the input to and output
!                    from ISC-COMPDEP.  The following format statements
!                    also were affected: 9009, 9026, 9032, 9033
!
!*       7/27/94     J. Hardikar, PES, Inc.
!*                   Added code to calculate reference wind speed at 10m
!*                   to be used for OPENPIT source algorithms
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance, including use of date window
!                    variables (ISTRT_WIND and ISTRT_CENT) and calculation
!                    of 10-digit date variable (FULLDATE) with 4-digit
!                    year for date comparisons.
!                    Also moved call to METDAT to allow use of single
!                    METDAT routine for normal and EVENT processing.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        MODIFIED:   To remove support for unformatted meteorological
!                    data files.
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED:   To correct potential problem with check for
!                    concatenated data files.
!                    R.W. Brode, PES, Inc., 9/15/2000
!
!        MODIFIED:   To incorporate additional variables for dry
!                    and wet deposition, and to remove formatted
!                    read for surface file.  Surface file is now
!                    read FREE format.
!                    R.W. Brode, PES, Inc., 9/29/2003
!
!        MODIFIED:   Moved call to SUB. METDAT ahead of call to
!                    SUB. SET_METDATA to avoid potential problem
!                    with negative precipitation for first hour
!                    of data.
!                    R.W. Brode, MACTEC, 10/26/2004
!
!        INPUTS:  Meteorology File Specifications
!
!        OUTPUTS: Meteorological Variables for One Hour
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line
   implicit none
   character :: modnam*12

!---- Constants used in the computation of QSW
   double precision, parameter :: c1=5.31d-13, c2=60.0d0, c3=1.12d0,&
   &stefb= 5.67d-08
   double precision :: rn, Es25
   integer :: idymax(12), ijday, jflag, level
!     JAT D065 8/9/21
!     IOSI SET BUT NOT USED
!      INTEGER :: IUSI, ISSI, IOSI
   integer :: iusi, issi
   character (len=8)   :: cusi, cssi, cosi
   character (len=6)   :: Temp_METVER
   character (len=256) :: buffer
   integer :: i, nfld
! Unused:      INTEGER :: IYR4
! Unused:       CHARACTER (LEN=6)   :: SPEC1, SPEC2, SPEC3
! JAT 06/22/21 D065
! REMOVE NTURB_Warnings AS UNUSED VARIABLE
!      INTEGER :: NTURB_Warnings

!     Variable Initializations
   data idymax/31,29,31,30,31,30,31,31,30,31,30,31/
   data Temp_METVER/'      '/

! --- Initialize variable to track warnings regarding use of turbulence
!     data with ADJ_U* option
! JAT 06/22/21 D065
! REMOVE NTURB_Warnings INITIALIZATION AS UNUSED VARIABLE
!      DATA NTURB_Warnings/0/

   modnam = 'METEXT'
   path   = 'MX'

!     Save Value of Last YR/MN/DY/HR and Previous Hour
   ipdate = kurdat
   ipyear = iyr
   iphour = ihour

!     Initialize USTAR, OBULEN, SFCZ0, QSW, IPCODE, AND PRATE to ZERO for hour
   ustar  = 0.0d0
   obulen = 0.0d0
   sfcz0  = 0.0d0
!jop  ZDM    = 0.0D0
   qsw    = 0.0d0
   ipcode = 0
   prate  = 0.0d0

!     JAT D070 intialize reset_sa and reset_sw
   reset_sa=.false.
   reset_sw=.false.

! --- Increment line counter for messages related to met data file
   iline = iline + 1

! --- Set 'DUMMY' variable = 'SURFFILE' for error handling
   dummy = 'SURFFILE'

   if (imonth == 12 .and. iday == 31 .and. ihour == 24) then
!        End of year has been reached - check for presence of header
!        record at beginning of next year for multi-year data files.
      read(mfunit,'(A256)',err=998,end=1000,iostat=ioerrn) buffer

      if (index(buffer,':') == 0) then
!           Record does not contain colon. Assume it must be regular
!           met data record, so backspace met file before proceeding.
         backspace mfunit
      else
!           Record contains colon. Assume it is a header record;
!           check for AERMET version date, C_METVER, and also check
!           station IDs before proceeding to flag potential
!           use of different stations in multi-year data files.
!           Convert UAIR and SURF character IDs to integers.
! ---       First extract AERMET version date, Temp_METVER
         NumHeaders = NumHeaders + 1
         if( index(buffer,'VERSION:') /= 0 )then
!              Extract AERMET version date
            read(buffer(index(buffer,'VERSION:')+8:&
            &index(buffer,'VERSION:')+13),'(A6)')&
            &Temp_METVER
         else
!              AERMET version not found in header record, issue fatal error message
            call errhdl(path,modnam,'E','395','No Version')
         endif
         if (Temp_METVER /= c_metver) then
!              AERMET version not found in header record, or different AERMET versions
!              were used; issue fatal error message
            call errhdl(path,modnam,'E','395',Temp_METVER)
         endif

! ---       Now extract UA, SF, and OS station IDs from header record
         if( index(buffer,'UA_ID:') >= 0 )then
            read(buffer(index(buffer,'UA_ID:')+7:&
            &index(buffer,'UA_ID:')+15),'(A)') cusi
         else
            cusi = '        '
         end if
         call stonum(cusi,8,fnum,imit)
         if (imit == 1) then
            iusi = nint(fnum)
         else
            iusi = 0
         end if

         if( index(buffer,'SF_ID:') >= 0 )then
            read(buffer(index(buffer,'SF_ID:')+7:&
            &index(buffer,'SF_ID:')+15),'(A)') cssi
         else
            cssi = '        '
         end if
         call stonum(cssi,8,fnum,imit)
         if (imit == 1) then
            issi = nint(fnum)
         else
            issi = 0
         end if

         if( index(buffer,'OS_ID:') >= 0 )then
            read(buffer(index(buffer,'OS_ID:')+7:&
            &index(buffer,'OS_ID:')+15),'(A)') cosi
         else
            cosi = '        '
         end if
         call stonum(cosi,8,fnum,imit)
!           JAT D065 IOSI NOT USED OTHER THAN BEING SET HERE
!           COMMENT OUT THIS CODE
!            IF (IMIT .EQ. 1) THEN
!               IOSI = NINT(FNUM)
!            ELSE
!               IOSI = 0
!            END IF

! ----      Check for consistency between UA and SF station IDs in header record
!           with values input by user on the ME pathway
         if (issi /= idsurf) then
!              Write Warning Message:  SURFDATA id mismatch
            call errhdl(path,modnam,'W','530','SURFDATA')
         end if
         if (iusi /= iduair) then
!              Write Warning Message:  UAIRDATA id mismatch
            call errhdl(path,modnam,'W','530','UAIRDATA')
         end if
      end if

      go to 1001

!        Error reading 'header record' - assume that header record is
!        missing.  Backspace met file and continue processing.
998   backspace mfunit

   end if


1001 continue

!
!---- READ surface scaling meteorology data based on free format
!

   if( ldpart .or. lwpart .or. ldgas .or. lwgas .or. grsm )then
!        Check for deposition variables on first data record
!        CERC 11/30/20 Calculation of QSW also needed for GRSM
      if (iline == 1) then
         nfld  = 0
         infld = .false.
         read(mfunit,'(A256)',err=99,end=1000,iostat=ioerrn) buffer
! ---       Check for wind data source/adjustment flag from version
!           11059 of AERMET and later
!           modified 12/11/17 to account for use of MMIF straight to AERMOD
!           so user doesn't get misleading warning message
!           JAT 5/8/2020 ADDED FROM VERSION 19191
!           Add MMIF-OS to the list of flags below to
!           avoid getting outdated met version warning
!           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
!           ACCOMODATE NEW AERMET USING GENERIC PROG MET
         if( index(buffer,'NAD') > 0 .or.&
         &index(buffer,'ADJ') > 0 .or.&
         &index(buffer,'MIFF-Mod') > 0 .or.&
         &index(buffer,'MMIF-OS') > 0 .or.&
         &index(buffer,'PROG-Mod') > 0 .or.&  !JAT D077
         &index(buffer,'PROG-OS') > 0 )then !JAT D077
            L_NAD_ADJ_Flags = .true.
         endif
         do i = 1, len_trim(buffer)
            if (.not.infld .and. buffer(i:i)/=' ' .and.&
            &buffer(i:i)/=',') then
!                 Set Mark of in a Field
               infld = .true.
!                 Increment the Field Counter
               nfld = nfld + 1
            else if (infld .and. (buffer(i:i)==' ' .or.&
            &buffer(i:i)==',')) then
!                 Location is the End of a Field
!                 Set Mark of Not In a field
               infld = .false.
            end if
         end do
         if (nfld < 25 .and. (ldpart .or. lwpart .or.&
         &ldgas .or. lwgas .or.&
         &grsm) ) then
!              Met record does not have enough fields,
!              deposition variables may be missing
            call errhdl(path,modnam,'E','495','with DEP')
            runerr = .true.
            eof = .true.
            go to 99
         else
            backspace mfunit
         end if

      else if (iline > 1) then
         read(mfunit,'(A256)',err=99,end=1000,iostat=ioerrn) buffer
! ---       Check for wind data source/adjustment flag from version
!           11059 of AERMET and later
!           JAT add MMIF to AERMOD in flag check 12/11/17
!           JAT 5/8/2020 ADDED FROM VERSION 19191
!           Add MMIF-OS to the list of flags below to
!           avoid getting outdated met version warning
!           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
!           ACCOMODATE NEW AERMET USING GENERIC PROG MET
         if( index(buffer,'NAD') > 0 .or.&
         &index(buffer,'ADJ') > 0 .or.&
         &index(buffer,'MIFF-Mod') > 0 .or.&
         &index(buffer,'MMIF-OS') > 0 .or.&
         &index(buffer,'PROG-Mod') > 0 .or.& !JAT D077
         &index(buffer,'PROG-OS') > 0)then !JAT D077
            L_NAD_ADJ_Flags = .true.
         else
! ---          Wind data source/adjustment flag is missing
            L_NAD_ADJ_Flags = .false.
         endif
         backspace mfunit

      endif

! ---    Read record from ASCII scalar parameter file using FREE format
!        with deposition variables
!
      read( mfunit, *, end=1000, err=99, iostat=ioerrn ) iyear,&
      &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
      &vptgzi, ziconv, zimech, obulen, sfcz0, bowen, albedo,&
      &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
      &sfcp, ncloud

!        Calculate solar irradiance, QSW, from Heat Flux, Bowen ratio,
!        albedo and cloud cover, for use in gas deposition algorithm.
      if (obulen>0.0d0 .or. obulen<-99990.0d0 .or.&
      &ta<0.0d0 .or.&
      &albedo==1.0d0 .or. bowen==0.0d0) then
!           Hour is stable or missing or inappropriate surface chars.
         qsw = 0.0d0
      else
         rn  = (1.0d0 + 1.0d0/bowen)*sfchf/0.9d0
         qsw = (rn*(1.0d0+c3) - c1*ta**6 + stefb*ta**4 -&
         &c2*0.1d0*dble(ncloud))/&
         &(1.0d0-albedo)
      end if
!
!        Set variables for dry deposition
      if (ldpart .or. ldgas) then
         if (Ta<0.0d0 .or. prate<0.0d0) then
            Wnew = Wold
         else
! ...          Compute saturation vapor pressure based on CMAQ formula
            EsTa = 0.6112d0 * dexp(19.83d0 - 5417.4d0/Ta)
            Es25 = 3.167d0
            Wnew = Wold+Prec1-0.5d0*f2*EsTa/Es25
            Wold = Wnew
            f2 = Wnew/200.d0
            if (f2<=0.01d0) f2 = 0.01d0
            if (f2>1.0d0) f2 = 1.0d0
         end if
      end if

   else
!        Read record from ASCII scalar parameter file without deposition
!        parameters, using FREE format
!        Check for number of data fields on first data record
      if (iline == 1) then
         nfld  = 0
         infld = .false.
         read(mfunit,'(A256)',err=99,end=1000,iostat=ioerrn) buffer
! ---       Check for wind data source/adjustment flag from version
!           11059 of AERMET and later
!           modified 12/11/17 to account for use of MMIF straight to AERMOD
!           so user doesn't get misleading warning message
!           JAT 5/8/2020 ADDED FROM VERSION 19191
!           Add MMIF-OS to the list of flags below to
!           avoid getting outdated met version warning
!           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
!           ACCOMODATE NEW AERMET USING GENERIC PROG MET
         if( index(buffer,'NAD') > 0 .or.&
         &index(buffer,'ADJ') > 0 .or.&
         &index(buffer,'MIFF-Mod') > 0 .or.&
         &index(buffer,'MMIF-OS') > 0 .or.&
         &index(buffer,'PROG-Mod') > 0 .or.& !JAT D077
         &index(buffer,'PROG-OS') > 0)then !JAT D077
            L_NAD_ADJ_Flags = .true.
         endif
         do i = 1, len_trim(buffer)
            if (.not.infld .and. buffer(i:i)/=' ' .and.&
            &buffer(i:i)/=',') then
!                 Set Mark of in a Field
               infld = .true.
!                 Increment the Field Counter
               nfld = nfld + 1
            else if (infld .and. (buffer(i:i)==' ' .or.&
            &buffer(i:i)==',')) then
!                 Location is the End of a Field
!                 Set Mark of Not In a field
               infld = .false.
            end if
         end do
         if (nfld < 20) then
!              Met record does not include enough variables
            call errhdl(path,modnam,'E','495','Non-DEP ')
            runerr = .true.
            eof = .true.
            go to 99
         else
            backspace mfunit
         end if

      else if (iline > 1) then
         read(mfunit,'(A256)',err=99,end=1000,iostat=ioerrn) buffer
! ---       Check for wind data source/adjustment flag from version
!           11059 of AERMET and later
!           modified 12/11/17 to account for use of MMIF straight to AERMOD
!           so user doesn't get misleading warning message
!           JAT 5/8/2020 ADDED FROM VERSION 19191
!           Add MMIF-OS to the list of flags below to
!           avoid getting outdated met version warning
!           JAT 1/14/21 ISSUE D077 ADD PROG-OS and PROG-Mod to
!           ACCOMODATE NEW AERMET USING GENERIC PROG MET
         if( index(buffer,'NAD') > 0 .or.&
         &index(buffer,'ADJ') > 0 .or.&
         &index(buffer,'MIFF-Mod') > 0 .or.&
         &index(buffer,'MMIF-OS') > 0 .or.&
         &index(buffer,'PROG-Mod') > 0 .or.& !JAT D077
         &index(buffer,'PROG-OS') > 0)then !JAT D077
            L_NAD_ADJ_Flags = .true.
         else
! ---          Wind data source/adjustment flag is missing
            L_NAD_ADJ_Flags = .false.
         endif

         backspace mfunit

      end if
!
      read( mfunit, *, end=1000, err=99, iostat=ioerrn ) iyear,&
      &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
      &vptgzi, ziconv, zimech, obulen, sfcz0, bowen, albedo,&
      &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
      &sfcp, ncloud
!  ****** FOR HIGHLY BUOYANT PLUME ****** added code JAN 2023--kja
! ** Get next hours mixing heights in needed
! ** Read next hour to get next mixing height for unstable conditions
      if (hbplume) then
         if (obulen < 0.0d0 .and. obulen > -9.9d4) then
            read( mfunit, *, end=1006, err=99, iostat=ioerrn ) iyear,&
            &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
            &vptgzi, ziconvn, zimechn, obulen, sfcz0, bowen, albedo,&
            &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
            &sfcp, ncloud
! ** Check for missing next hour IE. OBULEN =-99999.0
            if(obulen < -9.9d4) goto 1006
            backspace mfunit
            backspace mfunit
            read( mfunit, *, end=1000, err=99, iostat=ioerrn ) iyear,&
            &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
            &vptgzi, ziconv, zimech, obulen, sfcz0, bowen, albedo,&
            &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
            &sfcp, ncloud
            goto 1003
1006        ziconvn = -999.0d0
            zimechn = -999.0d0
            backspace mfunit
            backspace mfunit
            read( mfunit, *, end=1000, err=99, iostat=ioerrn ) iyear,&
            &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
            &vptgzi, ziconv, zimech, obulen, sfcz0, bowen, albedo,&
            &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
            &sfcp, ncloud
1003        continue
         else
            ziconvn = -999.0d0
            zimechn = -999.0d0
         endif
      endif
!  **************************************  added code end --kja

   end if

! --- Check for L_NAD_ADJ_Flags; if surface file header shows current
!     version date (i.e., L_OldMetVer=.F.), but the wind data
!     source/adj flags are missing (i.e., L_NAD_ADJ_Flags = .FALSE.)
!     issue warning message, but only issue warning once.
   if( .not. L_OldMetVer .and. .not. screen .and.&
   &imetmsg==0 .and. .not. L_NAD_ADJ_Flags )then
! ---    Set L_OldMetVer = .T.
      L_OldMetVer = .true.
      call errhdl(path,modnam,'W','394','No NAD/ADJ')
      imetmsg = imetmsg + 1
   endif

!     Set the stability logical variables, which are needed in COMPTG
   if( obulen > 0.0d0 ) then
      unstab = .false.
      stable = .true.
   else
      unstab = .true.
      stable = .false.
   endif

!---- Initialize the profile data to missing;
!     READ profile data based on free format
!
   call pflini ()
   level = 1
   jflag = 0
!     Read record from ASCII profile file using FREE format; compute
!     sigma_V from sigma_A and wind speed

! --- Set 'DUMMY' variable = 'PROFFILE' for error handling
   dummy = 'PROFFILE'

! --- Initialize logical variable to track for turbulence data
   L_TurbData = .false.

! --- First loop through PROFFILE to determine if turbulence data
!     are present
   do while( jflag == 0 )
      read( mpunit, *, end=1000, err=98, iostat=ioerrn ) kyear,&
      &kmonth, kday, khour, pflht(level), jflag,&
      &pflwd(level), pflws(level), pflta(level),&
      &pflsa(level), pflsw(level)

!        Convert the data to the required units
      call pflcnv (level)

! ---    Check for observed turbulence parameters in PROFFILE file
      if( (pflsa(level)>0.0d0 .and. pflsa(level)<99.0d0) .or.&
      &(pflsw(level)>0.0d0 .and. pflsw(level)<99.0d0) )then
         L_TurbData = .true.
      endif

!        Set the number of profile levels to current index, store
!        the 'top of profile' flag, and increment level if not at top
!        Check that the level does not exceed the maximum allowable
      nplvls = level
      iflag(level) = jflag

      if( jflag == 0 )then
         level = level + 1

         if( level > mxplvl )then
            if( .not. pflerr )then
!                 WRITE Error Message: Number of profile levels
!                                      exceeds maximum allowable
               write(dummy,'(I8)') mxplvl
               call errhdl(path,modnam,'E','465',dummy)
               pflerr = .true.
               runerr = .true.
            end if

!              Limit the number of levels to the maximum allowable
            level = mxplvl
         end if

      end if

! ---    Check for observed turbulence parameters in PROFFILE file
      if( pflsa(level)>0.0d0 .and. pflsa(level)<99.0d0 )then
         L_TurbData = .true.
         L_Got_SigA = .true.
      endif

      if( pflsw(level)>0.0d0 .and. pflsw(level)<99.0d0 )then
         L_TurbData = .true.
         L_Got_SigW = .true.
      endif

   enddo

! --- Need to check for TURB with DFAULT
! --- and adjusted U*, IF both turbulence and
! --- adjusted u* present, error !JAT 10/24/16
!!! --- We should always include turbulence data in AERMOD header
   if( dfault .and. L_TurbData .and. L_AdjUstar )then
      if( L_Got_SigA .and. L_Got_SigW )then
         dummy = 'SigA & SigW'
      elseif( L_Got_SigA)then
         dummy = 'SigA'
      elseif( L_Got_SigW )then
         dummy = 'SigW'
      endif
      call errhdl(path,modnam,'E','401',dummy)
      runerr = .true.
   endif
! --- Always include turbulence data in AERMOD header      ! RWB
   if( L_TurbData )then
      if( L_Got_SigA .and. L_Got_SigW )then
         dummy = 'SigA & SigW'
      elseif( L_Got_SigA )then
         dummy = 'SigA'
      elseif( L_Got_SigW )then
         dummy = 'SigW'
      endif
! ---    Issue error message if TurbData is used with ADJ_U*
      if( L_AdjUstar .and. dfault )then
         call errhdl(path,modnam,'E','401',dummy)
         runerr = .true.
      endif
   endif

!     Compute the vertical potential temperature gradient profile
   if( .not. runerr ) then
      ntglvl = 0
      call comptg ()
   endif

   if (iline == 1) then
!        Write Out Sample of the Meteorology Data
!        (Up to the First 24 Hours)                         ---   CALL METDAT
      jday = ijday      ! Assign IJDAY to global variable JDAY
      if( .not. L_SkipMessages ) call metdat(iounit)
      if (summfile) then
! ---       Include sample of meteorological data in SUMMFILE
         if( .not. L_SkipMessages ) call metdat(isumunt)
      end if
   end if

!     Set Meteorological Variables for Current Hour
   call set_metdata

   if (iline == 1) then
! ---    First hour of met data file; check for IHOUR .ne. 01 with short-term averages
      if (ihour > 1 .and. (pm25ave .or. no2ave .or. so2ave)) then
         if (l_maxdcont) then
!              Write Error Message: MAXDCONT option requires data file to begin with hour 1
            write(dummy,'(''First Hr= '',I2.2)') ihour
            call errhdl(path,modnam,'E','491',dummy)
!              Assign RUNERR logical to .T.
            runerr = .true.
         else if (no2ave .or. so2ave) then
!              Write Warning Message: 1hr NO2 & SO2 modeling should begin with hour 1
            write(dummy,'(''First Hr= '',I2.2)') ihour
            call errhdl(path,modnam,'W','488',dummy)
         else if (pm25ave) then
!              Write Warning Message: Short-term averages for first calendar day may not be valid
            call errhdl(path,modnam,'W','488','for 1st Day')
         end if
      else if (ihour > 1 .and. (numave>1 .or.&
      &(numave==1 .and.&
      &kave(1)/=1)) ) then
!           Write Warning Message: Short-term averages for first calendar day may not be valid
         call errhdl(path,modnam,'W','488','for 1st Day')
      end if

! ---    Check for start year based on met data file matching start year based on
!        ME SURFDATA keyword; note that IYR based on met data and ISYEAR based on
!        ME SURFDATA should both be 4-digits (a warning message will be issued if
!        ME SURFDATA input is not 4-digits)

      if (iyr /= isyear .and. imstat(7)==0) then
! ---       Issue warning message that year specified on SURFDATA does not match first year
!           of data file; if DAYRANGE keyword not used (IMSTAT(7)=0), adjust ISYEAR to match
!           data file (IYR))
         write(dummy,'(''StartYR '',I4)') iyr
         call errhdl(path,modnam,'W','492',dummy)
         isyear = iyr

      else if (iyr /= isyear .and. imstat(7)>0) then
! ---       Issue ERROR message that year specified on SURFDATA does not match first year
!           of data file when DAYRANGE keyword is being used (IMSTAT(7)>0).
         write(dummy,'(''StartYR '',I4)') iyr
         call errhdl(path,modnam,'E','493',dummy)
         isyear = iyr

         runerr = .true.

      end if

      if (pm25ave .or. no2ave .or. so2ave .or.&
      &annual .or. multyr) then
! ---       If PM-2.5 averaging, NO2 1-hour averaging, ANNUAL average, or MULTYEAR,
!           then need to set the variables for the "end-of-year" check if the STARTEND
!           keyword is not used
         if (imstat(6) == 0) then
! ---          Determine MN, DY, and HR for end-of-the-year check.
!              Subtract one from start hour to set end hour for the year of data;
!              adjust start day and month if needed.
            isyr = iyear
            ishr = ihour
            isdy = iday
            ismn = imonth

!              D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
            call cent_date(iyear,isyr)      !Convert ISYR to four digits
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C              Convert ISYR to Four Digits
!               IF (ISYR .GE. ISTRT_WIND .and. ISYR .LE. 99) THEN
!                  ISYR = ISTRT_CENT*100 + ISYR
!               ELSE IF (ISYR .LT. ISTRT_WIND) THEN
!                  ISYR = (ISTRT_CENT+1)*100 + ISYR
!               END IF

            call julian (isyr,ismn,isdy,isjday)

            if (ihour > 1) then
               iendhour = ihour - 1
               ienddy   = iday
               iendmn   = imonth
            else
               iendhour = 24
               if (iday > 1) then
                  ienddy = iday - 1
                  iendmn = imonth
               else
                  iendmn = imonth - 1
                  if (iendmn == 0) iendmn = 12
                  ienddy = idymax(iendmn)
               end if
            end if
! ---          Determine ISDATE based on first hour of data file,
!              unless this is a restarted run
            if (.not.rstinp) isdate = fulldate
         end if
      end if

! ---    Check for potential conflicts between "start dates" and
!        first date of met data file
      if (.not.multyr .and. imstat(6) == 1 .and.&
      &fulldate > isdate) then
! ---       Write Error Message:  Met data file starts later than
!           user-specified start date (ISDATE)
         write(dummy,'(I10.10)') fulldate
         call errhdl(path,modnam,'E','483',dummy)
         runerr = .true.
      else if (.not.multyr .and. rstinp .and.&
      &fulldate > isdate) then
! ---       Write Error Message:  Met data file starts later than
!           start date (ISDATE) for Re-started model run
         write(dummy,'(I10.10)') isdate-(isdate/100000000)*100000000
         call errhdl(path,modnam,'E','484',dummy)
         runerr = .true.
      else if (multyr .and. rstinp .and.&
      &fulldate > isdate) then
! ---       Write Warning Message:  Met data file starts later than
!           start date (ISDATE) for restarted MULTYEAR run, indicating
!           gap between years of met data
         write(dummy,'(I10.10)') isdate-(isdate/100000000)*100000000
         call errhdl(path,modnam,'W','485',dummy)
      else if (multyr .and. rstinp .and. imstat(6)==0 .and.&
      &fulldate < isdate) then
! ---       Write Error Message:  Met data file starts earlier than
!           start date (ISDATE) for restarted MULTYEAR run, without the
!           STARTEND keyword, indicating a date overlap between years
!           of met data
         write(dummy,'(I10.10)') isdate-(isdate/100000000)*100000000
         call errhdl(path,modnam,'E','487',dummy)
         runerr = .true.
      end if

   end if

! --- Increment index for hour-of-year and year to save met data for
!     MAXDCONT option
   if (fulldate >= isdate .and. l_maxdcont) then
      ihr_ndx = ihr_ndx + 1
      iyr_ndx = numyrs + 1

      if (iyr_ndx > nyears) then
! ---       Year index exceeds maximum array limit, set
!           by NYEARS parameter in modules.f
! ---       Write Error Message        ! Too many years
         write(dummy,'(''NYEARS='',I4)') nyears
         call errhdl(path,modnam,'E','482',dummy)
         runerr = .true.
         go to 999
      end if

! ---    Store hourly met data to arrays for MAXDCONT option
      if (.not.rstinp .and. l_maxdcont) then
         asfchf(ihr_ndx,iyr_ndx)  =  sfchf
         auref(ihr_ndx,iyr_ndx)   =  uref
         aurefht(ihr_ndx,iyr_ndx) =  urefht
         ata(ihr_ndx,iyr_ndx)     =  ta
         atrefht(ihr_ndx,iyr_ndx) =  trefht
         awdref(ihr_ndx,iyr_ndx)  =  wdref
         austar(ihr_ndx,iyr_ndx)  =  ustar
         awstar(ihr_ndx,iyr_ndx)  =  wstar
         aziconv(ihr_ndx,iyr_ndx) =  ziconv
         azimech(ihr_ndx,iyr_ndx) =  zimech
         aobulen(ihr_ndx,iyr_ndx) =  obulen
         avptgzi(ihr_ndx,iyr_ndx) =  vptgzi
         asfcz0(ihr_ndx,iyr_ndx)  =  sfcz0
         akst(ihr_ndx,iyr_ndx)    =  kst
! Added for HBP; JAN. 2023
         if (hbplume) then
            aziconvn(ihr_ndx,iyr_ndx) = ziconvn
            azimechn(ihr_ndx,iyr_ndx) = zimechn
         endif
! End HBP Insert
         if (ldgas .or. ldpart .or. lwpart .or. lwgas .or.&
         &grsm)then
            abowen(ihr_ndx,iyr_ndx)  =  bowen
            aalbedo(ihr_ndx,iyr_ndx) =  albedo
            iapcode(ihr_ndx,iyr_ndx) =  ipcode
            aprate(ihr_ndx,iyr_ndx)  =  prate
            arh(ihr_ndx,iyr_ndx)     =  rh
            asfcp(ihr_ndx,iyr_ndx)   =  sfcp
            nacloud(ihr_ndx,iyr_ndx) =  ncloud
            aqsw(ihr_ndx,iyr_ndx)    =  qsw
            AWnew(ihr_ndx,iyr_ndx)   =  Wnew
            Af2(ihr_ndx,iyr_ndx)     =  f2
            AEsTa(ihr_ndx,iyr_ndx)   =  EsTa
            APrec1(ihr_ndx,iyr_ndx)  =  Prec1
            APrec2(ihr_ndx,iyr_ndx)  =  Prec2
         end if
         if (l_blsource) then
            ablta(ihr_ndx,iyr_ndx)  = ta
         end if
         arurustr(ihr_ndx,iyr_ndx)   = rurustr
         arurobulen(ihr_ndx,iyr_ndx) = rurobulen

         aclmhr(ihr_ndx,iyr_ndx)   =  clmhr
         amsghr(ihr_ndx,iyr_ndx)   =  msghr
         aunstab(ihr_ndx,iyr_ndx)  =  unstab
         astable(ihr_ndx,iyr_ndx)  =  stable
         aurbstab(ihr_ndx,iyr_ndx) =  urbstab

         andx4zi(ihr_ndx,iyr_ndx) = ndx4zi
         auatzi(ihr_ndx,iyr_ndx)  = uatzi
         asvatzi(ihr_ndx,iyr_ndx) = svatzi
         aswatzi(ihr_ndx,iyr_ndx) = swatzi
         auavg(ihr_ndx,iyr_ndx)   = uavg
         asvavg(ihr_ndx,iyr_ndx)  = svavg
         aswavg(ihr_ndx,iyr_ndx)  = swavg
         aptatzi(ihr_ndx,iyr_ndx) = ptatzi

         agridwd(ihr_ndx,1:mxglvl,iyr_ndx) = gridwd(1:mxglvl)
         agridws(ihr_ndx,1:mxglvl,iyr_ndx) = gridws(1:mxglvl)
         agridsw(ihr_ndx,1:mxglvl,iyr_ndx) = gridsw(1:mxglvl)
         agridsv(ihr_ndx,1:mxglvl,iyr_ndx) = gridsv(1:mxglvl)
         agridtg(ihr_ndx,1:mxglvl,iyr_ndx) = gridtg(1:mxglvl)
         agridpt(ihr_ndx,1:mxglvl,iyr_ndx) = gridpt(1:mxglvl)
         if (nsec > 0) then
            agridrho(ihr_ndx,1:mxglvl,iyr_ndx) = gridrho(1:mxglvl)
         end if
         if (pvmrm .or. grsm) then
            agrideps(ihr_ndx,1:mxglvl,iyr_ndx) = grideps(1:mxglvl)
         end if
         if (nurb > 0) then
            agrdswr(ihr_ndx,1:mxglvl,iyr_ndx) = grdswr(1:mxglvl)
            agrdsvr(ihr_ndx,1:mxglvl,iyr_ndx) = grdsvr(1:mxglvl)
            agrdtgr(ihr_ndx,1:mxglvl,iyr_ndx) = grdtgr(1:mxglvl)
            agrdptr(ihr_ndx,1:mxglvl,iyr_ndx) = grdptr(1:mxglvl)

            do i = 1, nurb
               agrdswu(ihr_ndx,1:mxglvl,iyr_ndx,i)=grdswu(1:mxglvl,i)
               agrdsvu(ihr_ndx,1:mxglvl,iyr_ndx,i)=grdsvu(1:mxglvl,i)
               agrdtgu(ihr_ndx,1:mxglvl,iyr_ndx,i)=grdtgu(1:mxglvl,i)
               agrdptu(ihr_ndx,1:mxglvl,iyr_ndx,i)=grdptu(1:mxglvl,i)
!RWB              Add variables for URBAN option
               aziurb(ihr_ndx,iyr_ndx,i)     = ziurb(i)
               aurbwstr(ihr_ndx,iyr_ndx,i)   = urbwstr(i)
               aurbustr(ihr_ndx,iyr_ndx,i)   = urbustr(i)
               aurbobulen(ihr_ndx,iyr_ndx,i) = urbobulen(i)
!RWB              Also include L_MorningTrans array for urban morning transition
               AL_MorningTrans(ihr_ndx,iyr_ndx,i) = L_MorningTrans(i)
            end do
         end if

      end if

   end if

   go to 999

!---- End-of-file and error handling for METEXT
!
!     WRITE Error Messages:  Error Reading Met Data File

98 call errhdl(path,modnam,'E','510','PROFFILE')
   runerr = .true.
   go to 999

99 call errhdl(path,modnam,'E','510','SURFFILE')
   runerr = .true.
   go to 999

1000 eof = .true.

! --- Check for EOF on first data record, ILINE=1
   if (iline == 1) then
!        Write Error Message for EOF on first data record
      call errhdl(path,modnam,'E','580',dummy)
      runerr = .true.
   end if

999 return
end subroutine metext

subroutine set_metdata
!***********************************************************************
!                 SET_METDATA Module of AERMOD Model
!
!        PURPOSE: Sets the meteorological data variables for current hour
!
!        PROGRAMMER: ROGER BRODE
!
!        DATE:    May 12, 1999
!
!        MODIFIED:   To include IPROCL array for identifying which
!                    Julian days to process for Leap Years (based on
!                    the year specified on the ME SURFDATA keyword)
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 06/30/2015
!
!                    To include call to GRDEPS, for calculation of
!                    gridded turbulence dissipation rate for use in the
!                    PVMRM algorithm.
!                    R. W. Brode, MACTEC (f/k/a PES), Inc., 07/27/04
!
!                    To include determination of the day-of-week index
!                    (1 for Weekday [M-F], 2 for Saturday, 3 for Sunday)
!                    for use in the option to vary emissions by season,
!                    hour-of-day, and day-of-week (SHRDOW).
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   METEXT
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line

   implicit none
   character :: modnam*12

!     Declare Arrays for Use With Day/Date Calcs
! JAT 06/22/21 D065
! REMOVE ISEA_NDX AS UNUSED VARIABLE
   integer :: nday(12) !, ISEA_NDX(12)
   integer :: i, nl, numsw
   double precision :: sumsw, fvref
! Unused:       INTEGER ::  IA, IY, IM, ID,

!     Variable Initializations
   data nday/31,59,90,120,151,181,212,243,273,304,334,365/
! JAT 06/22/21 D065
! REMOVE ISEA_NDX INITITALIZATION AS UNUSED VARIABLE
!      DATA ISEA_NDX/1,1,2,2,2,3,3,3,4,4,4,1/

   modnam = 'SET_METDATA'

!---- Set lower limit of 1.0 meter for mixing heights
   if (ziconv>=0.0d0 .and. ziconv<1.0d0) ziconv = 1.0d0
   if (zimech>=0.0d0 .and. zimech<1.0d0) zimech = 1.0d0

!     Set the date variables for this hour
   call set_dates

   if (month .and. ihour == 24) then
!        Check for the End of the Month
      if (imonth == 1 .or. (mod(iyr,4) /= 0) .or.&
      &(mod(iyr,100) == 0 .and. mod(iyr,400) /= 0)) then
!           Not a Leap Year OR Month = January
         if (jday == nday(imonth)) then
            endmon = .true.
         end if
      else
!           Leap Year AND Month > January
         if (jday == nday(imonth)+1) then
            endmon = .true.
         end if
      end if
   end if

!     Check Data for Calms, Missing, Out-of-Range Values    ---   CALL METCHK
   if (.not. L_SkipMessages) call metchk

!     Limit ZI to 4000 meters.
   if (ziconv > 4000.d0) ziconv = 4000.d0
   if (zimech > 4000.d0) zimech = 4000.d0
!     Select appropriate mixing height from convective and mechanical values
   if (.not.msghr .and. .not.clmhr .and. obulen<0.0d0) then
      zi = max ( ziconv, zimech )
   else if (.not.msghr .and. .not.clmhr) then
      zi = zimech
   else
      zi = -999.0d0
   end if
!---- Set lower limit of 1.0 meter for mixing height
   if (zi>=0.0d0 .and. zi<1.0d0) zi = 1.0d0

! --- Assign ZI to ZIRUR for URBAN option
   if (urban) zirur = zi

!     Apply ROTANG Adjustment to Wind Direction
   if (dabs(rotang) > 0.0000001d0) then
      wdref = wdref - rotang
      if (wdref <= 0.0d0) then
         wdref = wdref + 360.0d0
      end if
      do nl = 1, nplvls
         if( pflwd(nl) > 0.0d0 )then
            pflwd(nl) = pflwd(nl) - rotang

            if( pflwd(nl) <= 0.0d0 )then
               pflwd(nl) = pflwd(nl) + 360.0d0
            endif

         endif
      end do
   end if

!---- Save the ambient temperature from the 'surface' file to a separate
!      variable for the bouyant line algorithms
   if (l_blsource) then
      blta = ta
   endif

!---- Initialize urban stable flag to false.
   if(.not.L_SkipMessages) urbstab = .false.

!
!---- Check the RUNERR flag - if it is FALSE, then there is sufficient
!     data to continue processing the data.
!     Also skip this IF-THEN block if L_SkipMessages is TRUE, which
!     indicates that this call is during the MAXDCONT "post-processing"
!     stage since the gridded profiles and other data have been retrieved
!     from arrays.
   if( .not. runerr .and. .not.L_SkipMessages )then
!
      if( .not. clmhr  .and.  .not. msghr )then
!           Set the stability logical variables
         if( obulen > 0.0d0 )then
            unstab = .false.
            stable = .true.
         else
            unstab = .true.
            stable = .false.
         endif


         if (fulldate>=isdate .and.&
         &( (L_LeapYear .and. iprocl(jday)==1) .or.&
         &(.not.L_LeapYear .and. iproc( jday)==1)) ) then
!
!              Initialize the gridded profile arrays
            gridsv = -99.0d0
            gridsw = -99.0d0
            gridws = -99.0d0
            gridwd = -99.0d0
            gridtg = -99.0d0
            gridpt = -99.0d0
            if (urban) then
               grdsvr = -99.0d0
               grdsvu = -99.0d0
               grdswr = -99.0d0
               grdswu = -99.0d0
               grdtgr = -99.0d0
               grdtgu = -99.0d0
               grdptr = -99.0d0
               grdptu = -99.0d0
            end if

!              Compute gridded profile of epsilon for PVMRM or GRSM option
            if (pvmrm .or. grsm) then
               grideps = -99.0d0
            end if

!              Get the index from the array of gridded heights that
!              corresponds to the height immediately below ZI
            call locate( gridht, 1, mxglvl, zi, ndx4zi )

!              Compute THETA_STAR and DTHDZ for the gridded
!              potential temperature gradient

            call tginit ()
!
!              Profile all variables here except sv and sw; defer sv
!              and sw until u at zi is known.
!
            call grdws ()
            call grdwd ()
            call grdptg()
            call grdpt ()

!----------    Compute density profile for PRIME
            call grdden

!----------    Compute the parameter values at ZI; if ZI is above the
!              highest gridded profile level, use the value at the high-
!              est level
            if( ndx4zi < mxglvl )then
               call gintrp( gridht(ndx4zi), gridws(ndx4zi),&
               &gridht(ndx4zi+1), gridws(ndx4zi+1),&
               &zi, uatzi )
               call gintrp( gridht(ndx4zi), gridpt(ndx4zi),&
               &gridht(ndx4zi+1), gridpt(ndx4zi+1),&
               &zi, ptatzi )

            else
               uatzi  = gridws(mxglvl)
               ptatzi = gridpt(mxglvl)

            endif
!
!              Add turbulence variables here
!
            call grdsv ()

!              Obtain residual turbulence value before calling GRDSW
            numsw = 0
            sumsw = 0.0d0

            do i = 1, nplvls
               if (pflht(i)>=zi .and. pflsw(i)>=0.0d0) then
                  numsw = numsw + 1
                  sumsw = sumsw + pflsw(i)
               end if
            end do

            if (numsw > 0) then
               swrmax = sumsw / dble(numsw)
            else
               swrmax = 0.02d0 * uatzi
            end if

            call grdsw ()

            if( ndx4zi < mxglvl )then
               call gintrp( gridht(ndx4zi), gridsv(ndx4zi),&
               &gridht(ndx4zi+1), gridsv(ndx4zi+1),&
               &zi, svatzi )
               call gintrp( gridht(ndx4zi), gridsw(ndx4zi),&
               &gridht(ndx4zi+1), gridsw(ndx4zi+1),&
               &zi, swatzi )
            else
               svatzi = gridsv(mxglvl)
               swatzi = gridsw(mxglvl)
            end if

!---           Compute the overbar (average) quantities for sigma_V, sigma_W,
!              and wind speed, from the surface up to ZI (formerly done in METINI)
            call ziaver (mxglvl,gridht,gridsv,zi,ndx4zi,svavg,svatzi)
            call ziaver (mxglvl,gridht,gridsw,zi,ndx4zi,swavg,swatzi)
            call ziaver (mxglvl,gridht,gridws,zi,ndx4zi,uavg,uatzi)

!              Compute gridded profile of epsilon for PVMRM/GRSM option
            if (pvmrm .or. grsm) then
               call grdeps
            end if

!              Compute Urban Profiles if Needed
            if (urban) then
               call urbcalc
               call grdurban
            end if

         end if
! JAT D070 WRITE MESSAGE THAT SIGMA-THETA OR SIGMA-W CHANGED DO NOT DO
! FOR NOTURB, NOSA, AND NOSW (UNDERSTOOD ALL HOURS ARE RESET)
         write(dummy,'(I10.10)') fulldate
         if (reset_sa .and. (turbopts(2) .or. turbopts(3) .or.&
         &turbopts(6) .or. turbopts(8)))&
         &call errhdl(path,modnam,'I','445',dummy)
         if (reset_sw .and. (turbopts(2) .or. turbopts(3) .or.&
         &turbopts(7) .or. turbopts(9)))&
         &call errhdl(path,modnam,'I','446',dummy)
      endif

   end if

! --- Assign sector ID for direction-varying background O3 and NOx
!     based on flow vector from surface file, FVREF;
!     first check for valid wind direction
   if (wdref <= 0.0d0 .or. wdref > 360.0d0) then
! ---    Hour is calm or missing; set IO3SECT = 0, INOXSECT = 0
      io3sect = 0
      inoxsect = 0
   else
! ---    Valid wind direction is available
      fvref = wdref + 180.0d0
      if (fvref > 360.0d0) then
         fvref = fvref - 360.0d0
      end if
      if (L_O3Sector) then
         if (fvref < o3sect(1) .or.&
         &fvref >= o3sect(NUMO3Sects) ) then
            io3sect = NUMO3Sects
         else
            do i = 1, NUMO3Sects-1
               if (fvref >= o3sect(i) .and.&
               &fvref < o3sect(i+1)) then
                  io3sect = i
                  exit
               end if
            end do
         end if
      else
         io3sect = 1
      end if
!        CERC 11/30/20
      if (L_NOxSector) then
         if (fvref < NOxSECT(1) .or.&
         &fvref >= NOxSECT(NUMNOxSects) ) then
            INOxSECT = NUMNOxSects
         else
            do i = 1, NUMNOxSects-1
               if (fvref >= noxsect(i) .and.&
               &fvref < noxsect(i+1)) then
                  inoxsect = i
                  exit
               end if
            end do
         end if
      else
         inoxsect = 1
      end if
   end if

! --- Assign sector ID for direction-varying background
!     based on flow vector from surface file, FVREF;
!     first check for valid wind direction
   if (wdref <= 0.0d0 .or. wdref > 360.0d0) then
! ---    Hour is calm or missing; set IBGSECT = 0
      ibgsect = 0
   else
! ---    Valid wind direction is available
      fvref = wdref + 180.0d0
      if (fvref > 360.0d0) then
         fvref = fvref - 360.0d0
      end if
      if (L_BGSector) then
         if (fvref < bgsect(1) .or.&
         &fvref >= bgsect(NUMBGSects) ) then
            ibgsect = NUMBGSects
         else
            do i = 1, NUMBGSects-1
               if (fvref >= bgsect(i) .and.&
               &fvref < bgsect(i+1)) then
                  ibgsect = i
                  exit
               end if
            end do
         end if
      else
         ibgsect = 1
      end if
   end if

!     Set Appropriate Wind Speed Category Index
   if (uref <= ucat(1)) then
      iucat = 1
   else if (uref <= ucat(2)) then
      iucat = 2
   else if (uref <= ucat(3)) then
      iucat = 3
   else if (uref <= ucat(4)) then
      iucat = 4
   else if (uref <= ucat(5)) then
      iucat = 5
   else
      iucat = 6
   end if

   if (fastarea .or. l_blsource) then
!        Set Stability Category based on Golder (1972) for use with
!        FASTAREA Area Source Optimizations (formerly the TOXICS option)
      call ltopg( kst )
   else
! ---    Assign D stability as default (KST=4)
      kst = 4
   end if

   if (msghr .and. .not. L_SkipMessages) then
      if (.not. msgpro) then
!           Set Flag for Runtime Met. Error to Prevent Further Calculations
         runerr = .true.
!           WRITE Error Message:  Missing Meteorological Data
         write(dummy,'(I10.10)') fulldate
         call errhdl(path,modnam,'E','460',dummy)
      else if (.not. L_SkipMessages) then
!           WRITE Informational Message:  Missing Meteorological Data
         write(dummy,'(I10.10)') fulldate
         call errhdl(path,modnam,'I','460',dummy)
      end if
   end if

   return
end subroutine set_metdata

subroutine set_dates
!***********************************************************************
!                 SET_DATES Module of AERMOD Model
!
!        PURPOSE: Sets the date variables for current hour
!
!        PROGRAMMER: ROGER BRODE
!
!        DATE:    May 12, 1999
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   SET_METDATA
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none

! JAT 06/22/21 D065
! REMOVE NDAY AS UNUSED VARIABLE
!      INTEGER :: NDAY(12), ISEA_NDX(12)
   integer :: isea_ndx(12)
   integer :: ia, iy, im, id
   character :: modnam*12
! Unused:       INTEGER :: I, NL

!     Variable Initializations
! JAT 06/22/21 D065
! REMOVE NDAY INITIALIZATION AS UNUSED VARIABLE
!      DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
   data isea_ndx/1,1,2,2,2,3,3,3,4,4,4,1/

   modnam = 'SET_DATES'

   if (.not. L_SkipMessages) then
! ---    This call is not part of MAXDCONT processing,
!        otherwise date calculations are skipped.

!        D001 Call CENT_DATE to determine the current Julian Day and Calculate Current Gregorian Date First Convert Year to 4-Digit Value Wood 9/15/22
      call cent_date(iyear,iyr)
! ---  D001 remove original calculation of 4-Digit year Wood 9/15/22
!C        Determine The Current Julian Day and Calculate Current Gregorian Date
!C        First Convert Year to 4-Digit Value
!         IF (IYEAR .GE. ISTRT_WIND .and. IYEAR .LE. 99) THEN
!            IYR = ISTRT_CENT*100 + IYEAR
!         ELSE IF (IYEAR .LT. ISTRT_WIND) THEN
!            IYR = (ISTRT_CENT+1)*100 + IYEAR
!         ELSE
!C           Input IYEAR must be 4-digit:  Save to IYR and convert to 2-digit
!            IYR   = IYEAR
!            IYEAR = IYR - 100 * (IYR/100)
!         END IF

! ---    Assign L_LeapYear variable
      if ((mod(iyr,4) /= 0) .or.&
      &(mod(iyr,100) == 0 .and. mod(iyr,400) /= 0)) then
!           Not a Leap Year
         L_LeapYear = .false.
      else
!           Leap Year
         L_LeapYear = .true.
      end if

! ---    Save previous Julian Day value
      if (jday >= 1) then
         jday_prev = jday
      else
         jday_prev = 0
      end if

!        Determine Julian Day (Day of Year) Number, JDAY    ---   CALL JULIAN
      call julian(iyr,imonth,iday,jday)

!        Calculate 8-digit Integer Variable for Current Date/Hour, KURDAT
!        and 10-digit Integer Variable (with 4-digit year), FULLDATE;
!        IYEAR = 2-digit year and IYR = 4-digit year
      kurdat = iyear*1000000 + imonth*10000 + iday*100 + ihour
      if (iyr >= 2148) then
!           Write Error Message:  Input Year is > 2147.
         write(dummy,'("YR= ",I4)') iyr
         call errhdl(path,modnam,'E','365',dummy)
         runerr = .true.
         fulldate = 2147123124
      else
         fulldate = iyr*1000000 + imonth*10000 + iday*100 + ihour
      end if

!        Check for 4-digit year input for profile data
      if (kyear >= 100) then
         kyear = kyear - 100 * (kyear/100)
      end if
      kurpfl = kyear*1000000 + kmonth*10000 + kday*100 + khour

   end if

!     Determine SEASON index
   iseas = isea_ndx(imonth)

   if (L_DayOfWeekOpts) then
! ---    An option requiring day-of-week is being used (EMISFACT/O3VALUES/BACKGRND)
!        Determine Day of Week (1 = Weekday [M-F], 2 = Saturday, 3 = Sunday).
!        Based on "Frequently Asked Questions about Calendars," Version 2.2,
!        by Claus Tondering, April 9, 2000, available on the web at URL
!        http://www.tondering.dk/claus/calendar.html
      ia = (14-imonth)/12
      iy = iyr - ia
      im = imonth + 12*ia - 2
      id = mod( (iday + iy + iy/4 - iy/100 + iy/400 + (31*im)/12), 7)
      if (id >= 1 .and. id <= 5) then
!           This is a weekday
         iday_of_week = 1
      else if (id == 6) then
!           This is a Saturday
         iday_of_week = 2
      else if (id == 0) then
!           This is a Sunday
         iday_of_week = 3
      end if
      if (id == 0) then
!           This is a Sunday
         iday_of_week7 = 7
      else
!           This is weekday or Saturday
         iday_of_week7 = id
      end if
   else
! ---    Use 1 as default
      iday_of_week  = 1
      iday_of_week7 = 1
   end if

   return
end subroutine set_dates

subroutine metchk
!***********************************************************************
!                 METCHK Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Performs Various Checks and Quality Assurance of
!                 One Hour of Meteorological Data
!
!        PROGRAMMER: JEFF WANG, ROGER BRODE
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To skip date sequence checking for EVENT processing,
!                    which is handled separately by EV_CHKDAT.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   METEXT
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'METCHK'
   clmhr  = .false.
   msghr  = .false.

   if (.not.nochkd .and. .not.evonly) then
!----    Check date for record out of sequence on the surface
!        scaling file - NOCHKD=.TRUE. means no date check   ---   CALL CHKDAT
      call chkdat
   end if

!---- Compare date & time in the surface and profile files  ---   CALL CMPDAT
   call cmpdat

!---- Check Data for Calm Winds                             ---   CALL CHKCLM
   call chkclm

   if (.not. clmhr) then
!----    Check data for missing data indicators             ---   CALL CHKMSG
      call chkmsg
   end if

!---- Check Data for Out-of-Range Values                    ---   CALL METQA
   call metqa

   return
end subroutine metchk

subroutine chkdat
!***********************************************************************
!                 CHKDAT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Checks Meteorological Data for Record Out of Sequence
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Include additional checks to identify data gaps
!                    that consist of complete days and/or complete
!                    years.  A data gap of complete year(s) is treated
!                    as a non-fatal warning, since a year of met data
!                    may be skipped due to data capture problems.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
!
!        MODIFIED:   Include option to issue Warning rather than
!                    Fatal Error message for meteorological record
!                    out of sequence (WARNCHKD option on MODELOPT).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To remove support for unformatted meteorological
!                    data files.
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED:   To incorporate modifications to date processing
!                    for Y2K compliance.  Specifically, allow for
!                    transition from KURDAT=99123124 to KURDAT=00010101
!                    for new century.
!                    R.W. Brode, PES, Inc., 5/12/99
!
!        INPUTS:  Date Variable
!
!        OUTPUTS: Date Error Messages
!
!        CALLED FROM:   METCHK
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'CHKDAT'

! --- Check for Met Data Record Out of Sequence;
!     IPDATE and KURDAT are 8-digit date variables (YYMMDDHH) for
!     the previous hour and current hour, respectively
   if (ipdate > 0) then
      if (kurdat <= ipdate) then
! ---       Previous date is .LE. current date; check for date crossing
!           century mark.
         if (kurdat/=10101 .or. ipdate/=99123124) then
! ---          Record Out of Sequence; current date is same or earlier
!              than previous date
            write(dummy,'(I8.8)') kurdat
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
               runerr = .true.
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         end if
      else if (ihour>1) then
! ---       Current hour > 01
         if ((kurdat-ipdate) == 1) then
! ---          Record is in sequence - continue with processing
            continue
         else if ((kurdat-ipdate) < 23) then
! ---          Gap is within the same day; issue message with date
!              of the gap and a second message with number of hours
!              within the gap
            write(dummy,'(I8.8)') kurdat
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
               runerr = .true.
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
            write(dummy,'(I3,'' hour gap'')') kurdat-ipdate-1
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
               runerr = .true.
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         else
! ---          Gap extends beyond the current day; issue message with
!              date of the gap
            write(dummy,'(I8.8)') kurdat
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
               runerr = .true.
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         end if
      else if (ihour==1 .and. iphour==24) then
! ---       Current hour is 01 and previous hour is 24; look for
!           gaps between days
         if (jday>1 .and. jday-jday_prev==1 .and.&
         &iyr==ipyear) then
! ---          No gap between days within same year; continue processing
            continue
         else if (jday>1 .and. jday-jday_prev>1 .and.&
         &iyr==ipyear) then
! ---          Record Out of Sequence; gap of full day(s); issue message
!              with date of gap and second message with # of days in gap
            write(dummy,'(I8.8)') kurdat
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
               runerr = .true.
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
            write(dummy,'(I3,'' day gap'')') jday-jday_prev-1
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         else if (jday>1 .and. jday-jday_prev>1 .and.&
         &iyr>ipyear) then
! ---          Record Out of Sequence; gap between years; issue message
!              with date of gap and second message with # of days in gap
            write(dummy,'(I8.8)') kurdat
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
               runerr = .true.
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
            write(dummy,'(I2,'' yr;'',I3,'' dy'')') iyr-ipyear,&
            &jday-jday_prev-1
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         else if (jday>1 .and. jday_prev>=jday .and.&
         &iyr>ipyear) then
! ---          Record Out of Sequence; gap between years; issue message
!              with date of gap and second message with # of days in gap
            write(dummy,'(I8.8)') kurdat
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
               runerr = .true.
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
            write(dummy,'(I2,'' yr;'',I3,'' dy'')') iyr-ipyear-1,&
            &(365-jday_prev)+jday-1
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         else if (jday==1 .and. jday_prev>=1 .and.&
         &(iyr-ipyear==1) )then
! ---          Check for gap at end of previous year, accounting for leap years
            if(  (mod(ipyear,4) /= 0) .or.&
            &(mod(ipyear,100) == 0 .and.&
            &mod(ipyear,400) /= 0) )then
! ---             Previous year is not a leap year, check for previous JDAY < 365
               if (jday_prev<365 ) then
! ---                Record Out of Sequence; gap at end of previous non-leap year;
!                    issue two warning messages, first with standard warning
!                    indicating location of gap and second warning indicating
!                    number of days in gap
                  write(dummy,'(I8.8)') kurdat
                  if (.not. l_warnchkd) then
! ---                   Write Error Message - Record out of sequence
                     call errhdl(path,modnam,'E','450',dummy)
                     runerr = .true.
                  else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---                   Write Warning Message - Record out of sequence
                     call errhdl(path,modnam,'W','450',dummy)
                  end if
                  write(dummy,'(I3,'' day gap'')') 365-jday_prev
                  if (.not. l_warnchkd) then
! ---                   Write Error Message - Record out of sequence
                     call errhdl(path,modnam,'E','450',dummy)
                  else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---                   Write Warning Message - Record out of sequence
                     call errhdl(path,modnam,'W','450',dummy)
                  end if
               end if
            else if (jday_prev < 366) then
! ---             Previous year is a leap year, and previous JDAY < 366;
!                 Record Out of Sequence; gap at end of previous leap year;
!                 issue two warning messages, first with standard warning
!                 indicating location of gap and second warning indicating
!                 number of days in gap
               write(dummy,'(I8.8)') kurdat
               if (.not. l_warnchkd) then
! ---                Write Error Message - Record out of sequence
                  call errhdl(path,modnam,'E','450',dummy)
                  runerr = .true.
               else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---                Write Warning Message - Record out of sequence
                  call errhdl(path,modnam,'W','450',dummy)
               end if
               write(dummy,'(I3,'' day gap'')') 366-jday_prev
               if (.not. l_warnchkd) then
! ---                Write Error Message - Record out of sequence
                  call errhdl(path,modnam,'E','450',dummy)
               else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---                Write Warning Message - Record out of sequence
                  call errhdl(path,modnam,'W','450',dummy)
               end if
            end if
         else if (ihour==1 .and. jday==1 .and.&
         &jday_prev>=1 .and.&
         &(iyr-ipyear)>1) then
! ---          Record Out of Sequence; gap of at least 1 complete year.
!              First check for additional gaps at the end of the previous
!              year, which would be an error (unless WARNCHKD is specified).
!
! ---          Check for gap at end of previous year, accounting for leap years
            if(  (mod(ipyear,4) /= 0) .or.&
            &(mod(ipyear,100) == 0 .and.&
            &mod(ipyear,400) /= 0) )then
! ---             Previous year is not a leap year, check for previous JDAY < 365
               if (jday_prev<365 ) then
! ---                Record Out of Sequence; gap at end of previous non-leap year;
!                    issue two warning messages, first with standard warning
!                    indicating location of gap and second warning indicating
!                    number of days in gap
                  write(dummy,'(I8.8)') kurdat
                  if (.not. l_warnchkd) then
! ---                   Write Error Message - Record out of sequence
                     call errhdl(path,modnam,'E','450',dummy)
                     runerr = .true.
                  else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---                   Write Warning Message - Record out of sequence
                     call errhdl(path,modnam,'W','450',dummy)
                  end if
                  write(dummy,'(I3,'' day gap'')') 365-jday_prev
                  if (.not. l_warnchkd) then
! ---                   Write Error Message - Record out of sequence
                     call errhdl(path,modnam,'E','450',dummy)
                  else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---                   Write Warning Message - Record out of sequence
                     call errhdl(path,modnam,'W','450',dummy)
                  end if
               end if
            else if (jday_prev < 366) then
! ---             Previous year is a leap year, and previous JDAY < 366;
!                 Record Out of Sequence; gap at end of previous leap year;
!                 issue two warning messages, first with standard warning
!                 indicating location of gap and second warning indicating
!                 number of days in gap
               write(dummy,'(I8.8)') kurdat
               if (.not. l_warnchkd) then
! ---                Write Error Message - Record out of sequence
                  call errhdl(path,modnam,'E','450',dummy)
                  runerr = .true.
               else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---                Write Warning Message - Record out of sequence
                  call errhdl(path,modnam,'W','450',dummy)
               end if
               write(dummy,'(I3,'' day gap'')') 366-jday_prev
               if (.not. l_warnchkd) then
! ---                Write Error Message - Record out of sequence
                  call errhdl(path,modnam,'E','450',dummy)
               else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---                Write Warning Message - Record out of sequence
                  call errhdl(path,modnam,'W','450',dummy)
               end if
            end if

! ---          Now issue two warning messages regarding full year(s)
!              data gap, first with standard warning indicating location
!              of gap and second warning indicating number of years in gap
            write(dummy,'(I8.8)') kurdat
            if (.not. l_warnchkd) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
            write(dummy,'(I3,'' year gap'')') iyr-ipyear-1
            if (.not. l_warnchkd) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         end if

      else if (ihour==1 .and. iphour/=24) then
! ---       Record Out of Sequence - gap between days; issue first
!           message with date of data gap, with a second message
!           with # hours/days in gap
         write(dummy,'(I8.8)') kurdat
         if (.not. l_warnchkd) then
! ---          Write Error Message - Record out of sequence
            call errhdl(path,modnam,'E','450',dummy)
            runerr = .true.
         else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---          Write Warning Message - Record out of sequence
            call errhdl(path,modnam,'W','450',dummy)
         end if
         if (jday>1 .and. jday-jday_prev>1 .and.&
         &iyr==ipyear) then
! ---          Gap of at least 1 day; issue message with # of hours
!              and # of days.
            write(dummy,'(I2,''hr & '',I3,''dy'')') 24-iphour,&
            &jday-jday_prev-1
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         else if (jday==1 .and. jday_prev>=1 .and.&
         &(iyr-ipyear==1) )then
! ---          Record Out of Sequence; gap of full day(s); issue message
!              with date of gap and second message with # of days in gap
            write(dummy,'(I8.8)') kurdat
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
               runerr = .true.
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
            write(dummy,'(I3,'' day gap'')') jday-jday_prev-1
            if (.not. l_warnchkd) then
! ---             Write Error Message - Record out of sequence
               call errhdl(path,modnam,'E','450',dummy)
            else if (l_warnchkd .and. .not.L_SkipMessages) then
! ---             Write Warning Message - Record out of sequence
               call errhdl(path,modnam,'W','450',dummy)
            end if
         end if
      end if
   end if

   return
end subroutine chkdat

subroutine cmpdat
!***********************************************************************
!             CMPDAT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Compares the date and time from the scalar and profile
!                 files
!
!        PROGRAMMER: Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Date variables
!
!        OUTPUTS: Date error messages
!
!        ASSUMPTIONS:   <none>
!
!        CALLED FROM:   METCHK
!***********************************************************************

!---- Variable declarations
   use main1
   implicit none
   character :: modnam*12

!---- Variable initializations
   modnam = 'CMPDAT'

!---- Check for a date mismatch between the scalar and profile files
!
   if (kurdat /= kurpfl) then
!        WRITE Error Message - Date mismatch
      write(dummy,'(I8.8)') kurdat
      call errhdl(path,modnam,'E','456',dummy)
      runerr = .true.
!
   end if

   return
end subroutine cmpdat
!
subroutine chkclm
!***********************************************************************
!                 CHKCLM Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Checks One Hour Meteorological Data for Calm Winds
!
!        PROGRAMMER: ROGER BRODE, JEFF WANG
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Calm Hour Flag, CLMHR, and Message
!
!        CALLED FROM:   METCHK
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'CHKCLM'

!     Check Data for Calm Winds, defined as UREF = 0.0
   if (uref == 0.0d0) then
      clmhr = .true.
      if (.not. L_SkipMessages) then
!           WRITE Informational Message: Calm Hour
         write(dummy,'(I10.10)') fulldate
         call errhdl(path,modnam,'I','440',dummy)
      end if
   end if

   return
end subroutine chkclm

subroutine chkmsg
!***********************************************************************
!                 CHKMSG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Checks One Hour Meteorological Data for Missing Data
!
!        PROGRAMMER: JEFF WANG
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To Add Upper Bound on USTAR Range Check - 12/07/2006
!
!        MODIFIED:  To Change Wind Direction Range Check - 10/26/2004
!
!        MODIFIED:  To Change Temperature Range Check - 9/29/92
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   METCHK
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'CHKMSG'

!---- Check Data for Missing Data Indicators
!
!     Wind speed (meters/second)
   if( uref >= 90.0d0 .or. uref < 0.0d0 )then
      msghr = .true.
!
!     Wind direction (degrees from north)
   else if( (wdref > 900.0d0)  .or.  (wdref <= -9.0d0) )then
      msghr = .true.
!
!     Ambient temperature (kelvins)
   else if( (ta > 900.0d0)  .or.  (ta <= 0.0d0) )then
      msghr = .true.
!
!     Monin-Obukhov length (meters)
   else if( obulen  <  -99990.0d0 )then
      msghr = .true.

!     Convective Mixing height (meters)
   else if( obulen < 0.0d0 .and.&
   &((ziconv > 90000.0d0)  .or.  (ziconv < 0.0d0)) )then
      msghr = .true.
!
!     Mechanical Mixing height (meters)
   else if( (zimech > 90000.0d0)  .or.  (zimech < 0.0d0) )then
      msghr = .true.
!
!     Surface friction velocity (meters/second)
   else if( ustar  <  0.0d0 .or. ustar >= 9.0d0 )then
      msghr = .true.

!     Convective velocity scale (meters/second)
   else if( wstar < 0.0d0 .and.&
   &(obulen < 0.0d0 .and. obulen > -99990.0d0) )then
      msghr = .true.
!
   end if

   return
end subroutine chkmsg

subroutine metqa
!***********************************************************************
!                 METQA Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Performs Quality Assurance Checks for
!                 One Hour of Meteorological Data
!
!        PROGRAMMER: JEFF WANG, ROGER BRODE
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  Added range check for vertical potential temperature
!                   gradient above ZI (variable VPTGZI).  Also applies
!                   minimum value of 0.005 K/m for consistency with
!                   AERMET.
!                   R. Brode, MACTEC/PES, 10/17/2005
!
!        MODIFIED:  Included check for absolute values of Monin-Obukhov
!                   length (OBULEN) less than 1.0.  Adjustment of OBULEN
!                   is made to limit ABS(OBULEN) .GE. 1.0.  The sign of
!                   OBULEN is assigned the opposite of the sign of the
!                   heat flux if OBULEN is 0.0.  This limit on OBULEN is
!                   already applied in AERMET, so this change will
!                   only affect input data generated by other means.
!                   R. Brode, MACTEC/PES, 10/26/2004
!
!        MODIFIED:  To adjust warning limit for USTAR from 2.0 to 4.0,
!                   adjust warning limit for WSTAR from 3.0 to 4.0, and
!                   to minimize duplication of warning messages for
!                   missing hours.
!                   R. Brode, MACTEC/PES, 10/26/2004
!
!        MODIFIED:  To check for errors reading surface variables for
!                   new deposition algorithms.  R. Brode, PES, 12/6/94
!
!        MODIFIED:  To Change Temperature Range Check Lower Limit To
!                   230 K - 9/29/92
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Meteorological Data Error and Status Switches
!
!        CALLED FROM:   METCHK
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12
   integer :: nl

!     Variable Initializations
   modnam = 'METQA'

!---- Check Data for Out-of-Range Values:

!---- Wind direction:
   if( wdref == 0.0d0)then
      wdref = 360.0d0
   endif

   do nl = 1, nplvls
      if( pflwd(nl) == 0.0d0 )then
         pflwd(nl) = 360.0d0
      end if
   end do

   if( .not. L_SkipMessages )then
      if( (wdref<  0.0d0 .and. wdref>-9.0d0)  .or.&
      &(wdref>360.0d0 .and. wdref<900.0d0) )then
!           WRITE Warning Message: Invalid Wind Dir'n
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','410',dummy)
      end if

!----    Wind speed range:
      if( uref<0.0d0 .and. uref>-9.0d0)then
!           WRITE Warning Message: Invalid Wind Speed;
!           This case is already flagged as missing hour,
!           but not with standard missing data code
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','420',dummy)
      end if
!
      if( uref > 30.0d0 .and. uref < 90.0d0)then
!           WRITE Warning Message: Wind Speed Over 30m/s
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','420',dummy)
      end if

!----    Wind data reference height:
      if( urefht  >  100.0d0 )then
!
!           -----------------------------------------------
!           Height of the wind data to be used in the
!           computation is greater than 100m -  warn the user
!           -----------------------------------------------

         write ( dummy, '(I8.8)' ) kurdat
         call errhdl(path,modnam,'W','475',dummy)

      else if( urefht < 0.001d0 .and.  .not.clmhr .and.&
      &.not.msghr)then
!
!           -----------------------------------------------
!           Height of the wind data to be used in the
!           computation is LT 0.001 for non-calm and
!           non-missing hour - this is an invalid entry;
!           issue fatal error message
!           -----------------------------------------------

         write ( dummy, '(I8.8)' ) kurdat
         call errhdl(path,modnam,'E','474',dummy)

         runerr = .true.

      endif

!----    Ambient temperature:
      if( (ta < 220.0d0 .and. ta > 0.0d0)  .or.&
      &(ta > 330.0d0 .and. ta < 900.0d0) )then
!           WRITE Warning Message: Ambient Temperature May be Out-of-Range
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','430',dummy)
      end if

!----    Friction velocity (meters/second):
      if( ustar > 4.0d0 )then
!           WRITE Warning Message: Friction velocity may be too large
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','432',dummy)
      end if

   end if

!---- Convective velocity (meters/second):
   if( wstar > 4.00d0 )then
!        WRITE Warning Message: Convective velocity may be too large
      if( .not. L_SkipMessages )then
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','438',dummy)
      end if
   else if( wstar == 0.0d0 )then
!        WRITE Warning Message: Convective velocity = 0.0, set to 0.001
      if( .not. L_SkipMessages )then
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','438',dummy)
      end if
      wstar = 0.001d0
   end if

!---- Monin-Obukhov length (meters):
   if( dabs(obulen) < 1.00d0 )then
!        WRITE Warning Message: Monin-Obukhov length is too small
      if( .not. L_SkipMessages )then
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','439',dummy)
      end if
!        Set ABS(OBULEN) = 1.0D0
      if (obulen < 0.0d0) then
         obulen = -1.0d0
      else if (obulen > 0.0d0) then
         obulen =  1.0d0
      else
         obulen = -1.0d0 * dsign( 1.0d0, sfchf )
      end if
   end if

!---- Vertical potential temperature gradient above ZI (K/m)
   if( ziconv > 0.0d0 .and. obulen < 0.0d0 .and.&
   &obulen > -99990.0d0 .and. vptgzi < 0.005d0 )then
!        WRITE Warning Message: VPTGZI less than or equal to 0.005 K/m
      if( .not. L_SkipMessages )then
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','441',dummy)
      end if
!        Adjust value to 0.005
      vptgzi = 0.005d0
   else if( ziconv > 0.0d0 .and. obulen < 0.0d0 .and.&
   &obulen > -99990.0d0 .and. vptgzi > 0.10d0 )then
!        WRITE Warning Message: VPTGZI is greater than 0.10 K/m
      if( .not. L_SkipMessages )then
         write(dummy,'(I8.8)') kurdat
         call errhdl(path,modnam,'W','442',dummy)
      end if
   end if

!---- Surface roughness length (m):
   if (sfcz0 < 0.0001d0) then
      if (.not.msghr .and. .not.clmhr) then
!           WRITE Warning Message:  Surface roughness length out-of-range
         if( .not. L_SkipMessages )then
            write(dummy,'(I8.8)') kurdat
            call errhdl(path,modnam,'W','435',dummy)
         end if
      end if
!        Set to 0.0001 to avoid divide-by-zero error
      sfcz0 = 0.0001d0
   end if

!---- Check for precipitation rate out of range
   if (prate < 0.0d0 .or. prate > 900.0d0) then
!        Assume precipitation is missing, set to 0.0
      prate = 0.0d0
   else
! ---    Calculate total precipitation
      total_precip = total_precip + prate
   end if

   return
end subroutine metqa

subroutine metdat(iount)
!***********************************************************************
!                 METDAT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Print Out The Summary Of The Meteorology Data
!
!        PROGRAMMER: JEFF WANG
!        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
!
!        DATE:    November 8, 1993
!
!        MODIFIED:   To include output file unit argument to support
!                    output to main 'aermod.out' file and to the
!                    optional SUMMFILE.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To include met data version date from surface file.
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
!
!        MODIFIED:   To remove support for unformatted meteorological
!                    data files.
!                    R.W. Brode, PES, Inc., 4/10/2000
!
!        MODIFIED BY R.W. Brode, PES, to avoid print string > 132 chars.
!        (DATE:    December 29, 1997)
!
!        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
!        (DATE:    February 15, 1993)
!
!        INPUTS:  Meteorology Input Data
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   METEXT, MEREAD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, ilmax, ijday, iount

!     Variable Initializations
   modnam = 'METDAT'

!---- WRITE Out Header Information
   call header(iount)
   write(iount,9011)
   ilmax = min( 80, ilen_fld )
   write(iount,9016) metinp(1:ilmax), c_metver, proinp(1:ilmax),&
   &metfrm, profrm
   write(iount,9020) idsurf, iduair, sfname, uaname,&
   &isyear, iuyear
   write(iount,9024)
   if (ldpart .or. ldgas .or. lwpart .or. lwgas .or. grsm) then
      write(iount,99025)
   else
      write(iount,9025)
   end if

!---- Since the first record has been read, write out the data to
!     IOUNIT, then read the next record from the scalar file

   do i = 1, 24
!----    Loop through first 24 hours of data file

!        We use the IF..ELSE structure because the global variable
!        for Julian day that is JDAY, not IJDAY. This avoids overwriting
!        JDAY read in METEXT.
      if( i == 1 )then

         if( ldpart .or. lwpart .or. ldgas .or. lwgas .or.&
         &grsm)then
            write(iount,99026) iyear, imonth, iday, ihour,&
            &sfchf, ustar, wstar, vptgzi, ziconv, zimech, obulen,&
            &sfcz0, bowen, albedo, uref, wdref, urefht, ta, trefht,&
            &ipcode, prate, rh, sfcp, ncloud
         else
            write(iount,9026) iyear, imonth, iday, jday, ihour,&
            &sfchf, ustar, wstar, vptgzi, ziconv, zimech, obulen,&
            &sfcz0, bowen, albedo, uref, wdref, urefht, ta, trefht
         end if

      else
         if (iyear >= 100) then
            iyear = iyear - 100 * (iyear/100)
         end if
         if( ldpart .or. lwpart .or. ldgas .or. lwgas .or.&
         &grsm)then
            write(iount,99026) iyear, imonth, iday, ihour,&
            &sfchf, ustar, wstar, vptgzi, ziconv, zimech, obulen,&
            &sfcz0, bowen, albedo, uref, wdref, urefht, ta, trefht,&
            &ipcode, prate, rh, sfcp, ncloud
         else
            write(iount,9026) iyear, imonth, iday, ijday, ihour,&
            &sfchf, ustar, wstar, vptgzi, ziconv, zimech, obulen,&
            &sfcz0, bowen, albedo, uref, wdref, urefht, ta, trefht
         end if

      end if

      if( ldpart .or. lwpart .or. ldgas .or. lwgas .or. grsm )then
!           Read record from ASCII scalar parameter file using FREE format
!           with deposition variables
         read( mfunit, *, end=999, err=99, iostat=ioerrn) iyear,&
         &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
         &vptgzi, ziconv, zimech, obulen, sfcz0, bowen, albedo,&
         &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
         &sfcp, ncloud
!
      else
!           Read hourly records from ASCII file using FREE format
!           without deposition variables
         read( mfunit, *, end=999, err=99, iostat=ioerrn ) iyear,&
         &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
         &vptgzi, ziconv, zimech, obulen, sfcz0, bowen, albedo,&
         &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
         &sfcp, ncloud
!
      end if
!
   end do
!
!---- REWIND met file, skip first record (with the latitude &
!     longitude), and read first data record to reset variables
!     to the first hour in the file.
!
999 continue
   rewind mfunit
   read( mfunit, '(I2)' )  idum
!
   if( ldpart .or. lwpart .or. ldgas .or. lwgas .or. grsm )then
!        Read record from ASCII scalar parameter file using FREE format
!        with deposition variables
      read( mfunit, *, end=999, err=99, iostat=ioerrn) iyear,&
      &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
      &vptgzi, ziconv, zimech, obulen, sfcz0, bowen, albedo,&
      &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
      &sfcp, ncloud

!
   else
!        Read hourly records from ASCII file using FREE format
!        without deposition variables
      read( mfunit, *, end=999, err=99, iostat=ioerrn ) iyear,&
      &imonth, iday, ijday, ihour, sfchf, ustar, wstar,&
      &vptgzi, ziconv, zimech, obulen, sfcz0, bowen, albedo,&
      &uref, wdref, urefht, ta, trefht, ipcode, prate, rh,&
      &sfcp, ncloud

!
   end if

!---- Write the first hour of profile data to IOUNIT; only 1 hour
!        is written because there can be up to 50 levels of data, which
!        could create a large amount of output.

   if( nplvls > 10 )then
      call header(iount)
   endif

   write (iount, 9034)
   write (iount, 9035)
   do i = 1,nplvls
      write (iount, 9036) kyear, kmonth, kday, khour, pflht(i),&
      &iflag(i), pflwd(i), pflws(i), pflta(i), pflsa(i),&
      &pflsw(i), pflsv(i)

   end do
   write (iount,9037)

   go to 9999
!
!---- FORMAT statements
!
9011 format(/36x,'*** UP TO THE FIRST 24 HOURS OF ',&
   &'METEOROLOGICAL DATA ***'/)
9016 format(3x,'Surface file:   ',a80,3x,'Met Version: ',a6,&
   &/,3x,'Profile file:   ',a80,&
   &/,3x,'Surface format: ',a105,&
   &/,3x,'Profile format: ',a105 )
9020 format(3x,'Surface station no.: ',i8,18x,&
   &'Upper air station no.: ',i8/18x,'Name: ',a40,3x,&
   &'Name: ',a40/18x,'Year: ',i6,37x,'Year: ',i6)
9024 format (/' First 24 hours of scalar data')
9025 format (' YR',' MO', ' DY', ' JDY', ' HR', '     H0',&
   &'     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',&
   &'  M-O LEN', '    Z0', '  BOWEN', ' ALBEDO',&
   &'  REF WS', '   WD', '     HT', '  REF TA', '     HT',&
   &/61('- '))
99025 format (' YR',' MO', ' DY', ' HR', '     H0',&
   &'     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',&
   &'  M-O LEN', '  Z0 ', 'BOWEN', '  ALB',&
   &'  REF WS', '   WD', '   HT', '  REF TA', '  HT',&
   &' IPCOD',' PRATE','  RH',' SFCP',' CCVR'&
   &/66('- '))
9026 format ( 1x,3(i2.2,1x),i3,1x,i2.2,1x,f6.1,1x,3(f6.3,1x),&
   &2(f5.0,1x),f8.1,1x,f5.2,1x,2(f6.2,1x),f7.2,1x,f5.0,&
   &3(1x,f6.1) )
99026 format ( 1x,3(i2.2,1x),i2.2,1x,f6.1,1x,3(f6.3,1x),&
   &2(f5.0,1x),f8.1,3f5.2,1x,f7.2,1x,f5.0,&
   &1x,f4.0,1x,f6.1,1x,f4.0,i3,f7.2,f6.0,f6.0,i3)
9034 format (//,' First hour of profile data')
9035 format ( ' YR', ' MO', ' DY', ' HR', ' HEIGHT', ' F', '  WDIR',&
   &'    WSPD',' AMB_TMP', ' sigmaA', '  sigmaW',&
   &'  sigmaV' )
9036 format (1x, 4(i2.2,1x),f6.1,1x,i1,1x,f5.0,1x,f7.2,1x,f7.1,1x,&
   &f6.1,1x,f7.2,1x,f7.2)
9037 format (/ ' F indicates top of profile (=1) or below (=0)')

!---- WRITE Error Message:  Error Reading Met Data Input File
!
99 call errhdl(path,modnam,'E','510','SURFFILE')
   runerr = .true.

9999 return
end subroutine metdat

subroutine metsum
!***********************************************************************
!                 METSUM Module of AERMOD Model
!
!        PURPOSE: Print Out The Summary Of The Meteorology Data
!                 Sampled Using the SCIM Option
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    April 14, 1998
!
!        MODIFIED:  To output missing temperatures correctly in the
!                   SCIM met data file.
!                   R.W. Brode, PES, Inc., - 02/25/02
!
!        INPUTS:  Meteorology Input Data
!
!        OUTPUTS: Printed Model Outputs
!
!        CALLED FROM:   HRLOOP
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   double precision :: pfltemp   ! Add PFLTEMP to
   integer :: i, ilmax

!     Variable Initializations
   modnam = 'METSUM'

!     WRITE Out Header Information
   if (iline == ifirsthr) then
      write(isunit,9011)
!        Write Surface Data, including user-specified SCIMBYHR parameters
      write(isunit,'(A,1x,I3)')  ' Start Hr: ', ifirsthr
      write(isunit,'(A,1x,I3)')  ' Interval: ', nregint
      write(isunit,*)
      ilmax = min( 80, ilen_fld )
      write(isunit,9016) metinp(1:ilmax), metfrm
      write(isunit,9020) idsurf, iduair, sfname, uaname,&
      &isyear, iuyear

!        Write column headers, depending on whether deposition is included,
!        consistent with WRITE statements below.
! ---    Use FORMAT 98025 for all cases
      write(isunit,98025)

!        Write Profile Data
      write(ipunit,99011)
!        Write Surface Data, including user-specified SCIMBYHR parameters
      write(ipunit,'(A,1x,I3)') ' Start Hr: ', ifirsthr
      write(ipunit,'(A,1x,I3)') ' Interval: ', nregint
      write(ipunit,*)
      ilmax = min( 80, ilen_fld )
      write(ipunit,99016) proinp(1:ilmax), profrm
      write(ipunit,99020) idsurf, iduair, sfname, uaname,&
      &isyear, iuyear
      write(ipunit,99025)
   end if

!     Write surface file record, depending on whether deposition is included
   write(isunit,98026) iyear, imonth, iday, ihour,&
   &sfchf, ustar, wstar, vptgzi, ziconv, zimech, obulen,&
   &sfcz0, bowen, albedo, uref, wdref, urefht, ta, trefht,&
   &ipcode, prate, rh, sfcp, ncloud

   do i = 1,nplvls
      if (pflta(i) == -999.0d0) then
         pfltemp = pflta(i)
      else
         pfltemp = pflta(i)-dctodk
      end if
      write (ipunit, 99026) kyear, kmonth, kday, khour, pflht(i),&
      &iflag(i), pflwd(i), pflws(i), pfltemp,&
      &pflsw(i), pflsv(i)
   end do

9011 format(/1x,'*** SUMMARY OF THE SAMPLED SURFACE ',&
   &'METEOROLOGICAL DATA USED WITH THE SCIM OPTION ***'/)
9016 format(1x,'Surface file:   ',a80,&
   &/,1x,'Surface format: ',a105)
9020 format(1x,'SURFACE STATION NO.: ',i6,20x,&
   &'UPPER AIR STATION NO.: ',i6/16x,'NAME: ',a40,3x,&
   &'NAME: ',a40/16x,'YEAR: ',i6,37x,'YEAR: ',i6/)
! Unused: 9025 FORMAT (' YR',' MO', ' DY', ' JDY', ' HR', '     H0',
!     &          '     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',
!     &          '  M-O LEN', '    Z0', '  BOWEN', ' ALBEDO',
!     &          '  REF WS', '   WD', '     HT', '  REF TA', '     HT',
!     &         /61(' -'))
98025 format (' YR',' MO', ' DY', ' HR', '     H0',&
   &'     U*', '     W*', '  DT/DZ', ' ZICNV', ' ZIMCH',&
   &'  M-O LEN', '   Z0 ', 'BOWEN', '  ALB',&
   &'  REF WS', '   WD', '   HT', '  REF TA', '  HT',&
   &' IPCOD',' PRATE','  RH',' SFCP',' CCVR'&
   &/66(' -'))
! Unused: 9026 FORMAT (1X, 3(I2.2,1X),I3,1X,I2.2,1X,F6.1,1X,3(F6.3,1X),
!     &        2(F5.0,1X),F8.1,1X,F5.3,1X,2(F6.2,1X),F7.2,1X,F5.0,
!     &        3(1X,F6.1) )
98026 format ( 1x,3(i2.2,1x),i2.2,1x,f6.1,1x,3(f6.3,1x),&
   &2(f5.0,1x),f8.1,1x,f5.3,2f5.2,1x,f7.2,1x,f5.0,&
   &1x,f4.0,1x,f6.1,1x,f4.0,i3,f7.2,f6.0,f6.0,i3,f7.3)

99011 format(/1x,'*** SUMMARY OF THE SAMPLED PROFILE ',&
   &'METEOROLOGICAL DATA USED WITH THE SCIM OPTION ***'/)
99016 format(1x,'Profile file:   ',a80,&
   &/,1x,'Profile format: ',a105)
99020 format(1x,'SURFACE STATION NO.: ',i6,20x,&
   &'UPPER AIR STATION NO.: ',i6/16x,'NAME: ',a40,3x,&
   &'NAME: ',a40/16x,'YEAR: ',i6,37x,'YEAR: ',i6/)
99025 format ( ' YR', ' MO', ' DY', ' HR', ' HEIGHT', ' F', '  WDIR',&
   &'    WSPD',' AMB_TMP', ' sigmaA', '  sigmaW' ,&
   &/29(' -'))
99026 format (1x, 4(i2.2,1x),f6.1,1x,i1,1x,f5.0,1x,f7.2,1x,f7.1,1x,&
   &f6.1,1x,f7.2)


   return
end subroutine metsum


subroutine pflcnv( level )
!***********************************************************************
!             PFLCNV Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Converts data in profile file to the required units
!
!        PROGRAMMER: Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        INPUTS:  Hourly profile data
!
!        OUTPUTS: Hourly profile data converted to required units
!
!        Revisions:
!                    R. Brode, PES, Inc.                  25 Feb 2002
!                    Modify upper limit on temperature from 900 to 90
!                    so that a value of 99.0 will be treated as
!                    missing
!
!                    R. Brode, PES, Inc.                  22 Jan 1998
!                    Check for wind direction > 900. and recode
!                    as missing (-999.0).
!
!                    J. Paumier, PES, Inc                 16 Dec 1994
!                    Fixed the logic in determining if ambient
!                    temperature is missing in the conversion from
!                    Celsius to kelvin
!
!        ASSUMPTIONS:
!
!        CALLED FROM:  METEXT
!***********************************************************************

!---- Variable Declarations
!
   use main1
   implicit none
   character :: modnam*12

   logical          :: L_Turb_Warn
   integer          :: level, N_Got_SigA, N_Got_SigW, N_Turb_Warn
   double precision :: sigrad, epsil, usubv

!
!---- Variable Initializations
!
   data N_Got_SigA/0/, N_Got_SigW/0/, N_Turb_Warn/0/
   data L_Turb_Warn/.false./
   modnam = 'PFLCNV'
!-----------------------------------------------------------------------

!     JAT 1/29/21 D070 TURBULENCE OPTIONS
!     RESET PFLSA AND/OR PFLSW BASED ON TURBULENCE OPTIONS
   write(dummy,'(I10.10)') fulldate

   if (pflsa(level) < 99.0d0 .and. (turbopts(1) .or.&
   &(turbopts(2) .and. stable) .or. (turbopts(3) .and. unstab)&
   &.or. turbopts(4) .or. (turbopts(6) .and. stable) .or.&
   &(turbopts(8) .and. unstab))) then
      pflsa(level)=99.0d0
      if (.not. reset_sa) reset_sa=.true.

   endif
   if (pflsw(level) < 99.0d0 .and. (turbopts(1) .or.&
   &(turbopts(2) .and. stable) .or. (turbopts(3) .and. unstab)&
   &.or. turbopts(5) .or. (turbopts(7) .and. stable) .or.&
   &(turbopts(9) .and. unstab))) then
      pflsw(level)=99.0d0
      if (.not. reset_sw) reset_sw=.true.

   endif

!     Change the missing value indicator for wind speed to -99.0

   if( pflws(level)  < 0.0d0  .or.&
   &pflws(level)  > 90.0d0 )then
      pflws(level) =  -99.0d0
   endif

!     Change the wind direction from 0.0 to 360.0 if wind speed is nonzero

   if(( pflws(level) > 0.0d0 .and. pflws(level) <= 90.0d0 ).and.&
   &pflwd(level) == 0.0d0 )then
      pflwd(level) = 360.0d0
   endif

   if( pflwd(level) > 900.0d0 .or. pflwd(level) < 0.0d0 )then
      pflwd(level) = -999.0d0
   endif

   if( pflws(level) == 0.0d0  .and.&
   &pflwd(level) == 0.0d0 )then
      pflws(level) = -99.0d0
      pflwd(level) = -999.0d0
   endif

!     Compute sigmaV from nonmissing wind speed and sigmaTHETA
   if( pflws(level) > 0.0d0  .and.&
   &pflsa(level) >= 0.0d0  .and.&
   &pflsa(level) < 99.0d0 )then
      sigrad = pflsa(level) * dtorad
      epsil = dsin(sigrad) * ( 1.0d0 - gsigv * sigrad )           ! GSIGV = 0.073864D0 (in MODULES.f)
      usubv = pflws(level) * dsqrt( 1.0d0 - epsil*epsil )
      pflsv(level) = sigrad * usubv
! ---       Compare to minimum value PARAMETER, SVMIN
      pflsv(level) = max( svmin, pflsv(level) )
      L_Got_SigA = .true.
      N_Got_SigA = N_Got_SigA + 1
   else
      pflsv(level) = -99.0d0
   endif

! --- Check for sigmaW data
   if( pflsw(level) > 0.0d0 .and.&
   &pflsw(level) < 90.0d0 )then
      L_Got_SigW = .true.
      N_Got_SigW = N_Got_SigW + 1
   endif

! --- Check for turbulence data with ADJ_U
   if( L_AdjUstar .and. dfault )then
! ---    Issue FATAL error if observed turbulence data are
!        used with L_AdjUstar with RegDFAULT option
      if( L_Got_SigA .and. L_Got_SigW )then
         if( N_Got_SigA == 1 .and. N_Got_SigW == 1) then
            call errhdl(path,modnam,'E','401','SigA & SigW')
            fatal = .true.
         endif
      elseif( L_Got_SigA .and. .not.L_Got_SigW )then
         if( N_Got_SigA == 1 ) then
            call errhdl(path,modnam,'E','401','SigA Data')
            fatal = .true.
         endif
      elseif( .not.L_Got_SigA .and. L_Got_SigW )then
         if( N_Got_SigW == 1 ) then
            call errhdl(path,modnam,'E','401','SigW Data')
            fatal = .true.
         endif
      endif
   elseif( L_AdjUstar )then
! ---    Issue Warning error if observed turbulence data are used
!        with L_AdjUstar and Non-DFAULT; limit to one warning
      if( L_Got_SigA .and. L_Got_SigW )then
         if( N_Got_SigA == 1 .and. N_Got_SigW == 1) then
            call errhdl(path,modnam,'W','401','SigA & SigW')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            modops(1)  = 'NonDFAULT'
            modops(26) = 'SigA&SigW'
         endif
      elseif( L_Got_SigA .and. .not.L_Got_SigW )then
         if( N_Got_SigA == 1 ) then
            call errhdl(path,modnam,'W','401','SigA Data')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            modops(1)  = 'NonDFAULT'
            modops(26) = 'SigA Data'
         endif
      elseif( .not.L_Got_SigA .and. L_Got_SigW )then
         if( N_Got_SigW == 1 ) then
            call errhdl(path,modnam,'W','401','SigW Data')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            modops(1)  = 'NonDFAULT'
            modops(26) = 'SigW Data'
         endif
      endif
      if( .not. L_Turb_Warn .and. (L_Got_Siga .or. L_Got_SigW) )then
         L_Turb_Warn = .true.
         N_Turb_Warn = N_Turb_Warn + 1
         if( N_Turb_Warn == 1) then
            call errhdl(path,modnam,'W','402','Option')
         endif
      endif
   else
! ---    ADJ_U* is NOT being used, however include use of turbulence,
! ---    SigA and/or SigW in the MODOPS array even with without ADJ_U*
      if( L_Got_SigA .and. L_Got_SigW )then
         if( N_Got_SigA == 1 .and. N_Got_SigW == 1) then
            call errhdl(path,modnam,'W','403','SigA & SigW')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            modops(26) = 'SigA&SigW'
         endif
      elseif( L_Got_SigA .and. .not.L_Got_SigW )then
         if( N_Got_SigA == 1 ) then
            call errhdl(path,modnam,'W','403','SigA Data')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            modops(26) = 'SigA Data'
         endif
      elseif( .not.L_Got_SigA .and. L_Got_SigW )then
         if( N_Got_SigW == 1 ) then
            call errhdl(path,modnam,'W','403','SigW Data')
! ---          Assign character string regarding use of turbulence
!              data on MODOPS string
            modops(26) = 'SigW Data'
         endif
      endif

!JAT ISSUE D030: COMMENT OUT DUPLICATE CODE BELOW
!         IF SIGMA-A IS INCLUDED, THE CALCULATION
!         HAS ALREADY BEEN DONE OUTSIDE THIS IF/ELSE
! ---    Make calculation                                            ! 20190709- Poss. duplicate code - Review block below
!         IF( PFLSA(LEVEL) .GT. 0.0D0  .and.
!     &       PFLSA(LEVEL) .LT. 99.0D0 )THEN   !! May need to reconsider upper bound for SigA
!            SIGRAD = PFLSA(LEVEL) * DTORAD
!            EPSIL = DSIN(SIGRAD) * ( 1.0D0 - GSIGV * SIGRAD )        ! GSIGV = 0.073864D0 (in MODULES.f)
!            USUBV = PFLWS(LEVEL) * DSQRT( 1.0D0 - EPSIL*EPSIL )
!            PFLSV(LEVEL) = SIGRAD * USUBV
!C ---       Compare to minimum value PARAMETER
!            PFLSV(LEVEL) = MAX( SVMIN, PFLSV(LEVEL) )
!         ENDIF                                                       ! 20190709- End review
!JAT  ISSUE D030: END COMMENT OUT
   endif

!     Convert temperature from degrees Celsius to kelvins

   if( pflta(level)  > -90.0d0  .and.&
   &pflta(level)  <  90.0d0 )then
      pflta(level) = pflta(level) + dctodk

   else
      pflta(level) = -999.0d0

   endif

!     Change the missing value indicator for sigmaW to -99.0

   if( pflsw(level)  < 0.0d0  .or.&
   &pflsw(level)  > 90.0d0 )then
      pflsw(level) =  -99.0d0
   else
!        Compare to minimum value PARAMETER, SWMIN = 0.02
      pflsw(level) = max( swmin, pflsw(level) )
   endif

   return
end subroutine pflcnv

subroutine pflini ()
!***********************************************************************
!             PFLINI Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Initializes the observed profile arrays to missing
!
!        PROGRAMMER: Jim Paumier, PES, Inc.
!
!        DATE:    September 30, 1993
!
!        INPUTS:  None
!
!        OUTPUTS: Initialized observed profile arrays
!
!        ASSUMPTIONS:
!
!        CALLED FROM:  METEXT
!***********************************************************************
!
!---- Variable Declarations
!
   use main1
   implicit none
   character :: modnam*12
! Unused:      INTEGER :: I

!
!---- Variable Initializations
!
   modnam = 'PFLINI'
   path   = 'MX'

!.......................................................................
!     Initialize arrays (1:MXGLVL)
   iflag(:)  = 0
   pflht(:)  = -99.0d0
   pflws(:)  = -99.0d0
   pflwd(:)  = -99.0d0
   pflta(:)  = -99.0d0
   pflsa(:)  = -99.0d0
   pflsw(:)  = -99.0d0
   pflsv(:)  = -99.0d0
   pfltg(:)  = -99.0d0
   pfltgz(:) = -99.0d0

   return
end subroutine pflini

subroutine ziaver ( nlvls,hts,parray,zi,ndxblw,pblavg,valzi )
!***********************************************************************
!             ZIAVER Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     To compute the average value of the parameter between
!                the surface and the mixing height
!
!   Input:       Number of levels in the profile (NLVLS)
!                Array of gridded profile heights (HTS)
!                Parameter array (PARRAY)
!                Boundary layer height (ZI) (or stack height, if higher)
!                Index of the level gridded profile height immediately
!                   below ZI (NDXBLW)
!                Value of parameter at ZI (VALZI)
!
!   Output:      Average value of parameter in boundary layer (PBLAVG);
!
!   Called by:   METEXT
!
!   Assumptions: If the mixing height (ZI) is above the highest
!                profile height (5000 m), then we assume the profile
!                is constant (= PARRAY(NLVLS)) above ZI and compute
!                the average accordingly.
!
!
!   Programmer:  Jim Paumier PES, Inc.
!
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   Reference(s): Inhomgeneous Boundary Layer, A. Venkatram,
!                 June 25, 1993 (GOLF document #5)
!
!***********************************************************************
!
!---- Variable declarations
!
   implicit none

   integer   :: ndxblw, nlvls, i
   double precision  :: hts(nlvls), parray(nlvls), zi, sum, pblavg,&
   &valzi
!
!---- Data dictionary
!
!---- Data initializations
!
!.......................................................................

   sum = 0.0d0

!---- Sum over each layer of the gridded profile (PARRAY) to the level
!     immediately below ZI

   do i = 2, ndxblw
      sum = sum + (hts(i) - hts(i-1)) * 0.5d0 *&
      &(parray(i) + parray(i-1))
   end do

!---- Finish the summation

   if( ndxblw < nlvls )then
!------- Add the area between the level below ZI and ZI to the
!        sum and compute the average.

      sum = sum + (zi - hts(ndxblw) ) * 0.5d0 *&
      &(valzi + parray(ndxblw) )
      pblavg = sum / zi

   else
!----    ZI is above the top level (5000 m), assume the parameter is
!        constant above that level and sum accordingly and compute
!        the average
      sum = sum + (zi - hts(nlvls)) * parray(nlvls)
      pblavg = sum / zi
   endif

   return
end subroutine ziaver

subroutine gintrp ( htbelo,vbelow, htabov,vabove, reqdht,value )
!***********************************************************************
!             GINTRP Module of the AMS/EPA Regulatory Model - AERMOD
!
!   Purpose:     A generalized interpolation routine
!
!   Input:       Height below the required height (HTBELO)
!                Value below the required height (VBELOW)
!                Height above the required height (HTBELO)
!                Value above the required height (VBELOW)
!                Height at which a value is required (REQDHT)
!
!   Output:      Value of the parameter at the required level (VALUE)
!
!   Called by:   Utility routine called by many modules
!
!   Assumptions:
!
!   Programmer:  Jim Paumier, PES, Inc.
!
!   Date:        September 30, 1993
!
!   Revision history:
!                <none>
!
!   Reference(s):
!
!***********************************************************************
!
!---- Variable declarations
!
   implicit none
   double precision :: value, htbelo, vbelow, htabov, vabove, reqdht
!
!---- Data dictionary
!
!---- Data initializations
!
!.......................................................................
!
!---- Interpolate

   value = vbelow + ( (reqdht - htbelo) / (htabov - htbelo) ) *&
   &(vabove - vbelow)

   return
end subroutine gintrp

subroutine urbcalc
!***********************************************************************
!             URBCALC Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Calculates Parameters for Urban Stable Boundary Layer
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    June 11, 1996
!
!
!        MODIFIED:  Calculate an urban ustar by setting equivalence
!                   between convective sigma-w based on urban wtar
!                   and mechanical sigma-w based on urban ustar at
!                   a height of 7 times the urban roughness length.
!                   R.W. Brode, PES, Inc., - 06/10/02
!
!        MODIFIED:  To set the value for Z_iuo at 400m instead of
!                   500m in calculation of ZIURB, based on observed data.
!                   R.W. Brode, PES, Inc., - 04/08/02
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Urban Mixing Height, Heat Flux, and "Convective
!                 Velocity Scale"
!
!        ASSUMPTIONS:  <none>
!
!        CALLED FROM:  METEXT
!***********************************************************************

!---- Variable declarations
   use main1
   implicit none
   character :: modnam*12
!     JAT D065 8/9/21, RHO CALCULATED BUT NOT USED
!      DOUBLE PRECISION :: DELTUR, URBHF, RHO, HT7Z0
   double precision :: deltur, urbhf, ht7z0, rho
   double precision :: urboblsav, urbustrsav
!---- Assign the reference mixing height, REFZI, based on a reference
!     population of 2 million.
!     CP     = specific heat capacity of dry air
   double precision, parameter :: refzi = 400.0d0, cp = 1004


!---- Variable initializations
   modnam = 'URBCALC'

!     Save Rural values of USTAR and OBULEN
   rurustr   = ustar
   rurobulen = obulen

!     Loop Through Urban Areas
   do iurb = 1, numurb

!        Compute Urban-Rural Temperature Difference, DELTUR;
!RWB     DELTUR = DELTRUR * (0.1046 * LOG(URBPOP/REFPOP) + 0.9983)
!RWB     using DELTRUR = 12.0 K for the reference population
!RWB     (REFPOP) of 2.0D+6 (assigned in MODULE MAIN1).
!RWB     Use rounded values for parameters
      deltur = deltrur * (0.1d0 * dlog(urbpop(iurb)/refpop) + 1.0d0)

      if (stable) then
! ---       Compute Urban Convective Mixing Height
         ziurb(iurb) = refzi * (urbpop(iurb)/refpop) ** 0.25d0
         L_MorningTrans(iurb) = .false.
      else if (L_UrbanTransition) then
! ---       Compute Urban pseudo-Convective Mixing Height for morning transition
         ziurb(iurb) = refzi * (urbpop(iurb)/refpop) ** 0.25d0
! ---       Check for ZICONV > ZIURB; if so then disable morning transition
         if (ziconv > ziurb(iurb)) then
            L_MorningTrans(iurb) = .false.
            cycle
         else
            L_MorningTrans(iurb) = .true.
         end if
! JAT D136_URBAN_TRANS; ADD ELSE STATEMENT TO CYCLE IF THE NOURBANTRAN INVOKED AND
! HOUR IS CONVECTIVE
! THIS WILL KEEP THE URBAN DEBUG FILE FROM HAVING NaNs WHEN FIRST HOUR OF MET FILE IS
! CONVECTIVE
      else
         L_MorningTrans(iurb) = .false.
         cycle
      end if

!     JAT D065 8/9/21, RHO CALCULATED BUT NOT USED
      rho    = 101325.d0/(287.04d0*ta)

!        Compute Urban Heat Flux, and recalculate Monin-Obukhov length
      urbhf  = 0.03d0 * deltur * ustar * rho * cp


!        Compute Urban WSTAR
      urbwstr(iurb) = ((g/ta/rho/cp) * urbhf * ziurb(iurb)) ** third


!        Compute Urban USTAR; first set height for equivalence between
!        convective and mechanical sigma-w as 7 times the maximum of the
!        rural and urban surface roughness length.
      ht7z0 = 7.0d0* max( urbz0(iurb), sfcz0 )
      urbustrsav = urbwstr(iurb) *&
      &dsqrt(1.6d0*(ht7z0/ziurb(iurb))**(2.0d0*third))/&
      &(1.3d0*dsqrt(1.0d0-ht7z0/max(ziurb(iurb),zimech)))
      urbustr(iurb) = max( ustar, urbustrsav)

!        Compute equivalent Monin-Obukhov length
      urbobulen(iurb) = -((rho*cp*ta*urbustr(iurb)**3)/&
      &(0.4d0*g*urbhf))
      urboblsav = urbobulen(iurb)

!RCO D095, compare compute MOL to original MOL, pick the more
!    neutral for nighttime
!CRCO D120 Check for urban transition, use most convective value
      if (L_MorningTrans(iurb)) then
!During morning transition hours, RUROBULEN is already negative, so most
!convective value will be maximum of the two negative values
         urbobulen(iurb) = max(urbobulen(iurb),rurobulen)
      else
!During stable conditions, RUROBULEN is positive, while URBOBULEN. Take the
!largest of the absolute value of the two to get the most neutral value.
!Using the positive values to replace downstream calculations in various places
!that were making the convective URBOBULEN stable/neutral.
         urbobulen(iurb) = max(abs(urbobulen(iurb)),rurobulen)
      end if

!RCO D095 Added for urban debug 8/3/2021
      if (urbdbug) then
         write(urbunt,3333) iurb,iyear,imonth,iday,ihour,&
         &urbobulen(iurb),urboblsav,rurobulen,&
         &urbustr(iurb),urbustrsav,&
         &ustar,deltur,ziurb(iurb),zimech,ziconv,&
         &urbpop(iurb),urbhf,urbwstr(iurb),wstar,&
         &ta,uref,bowen,&
         &stable,L_MorningTrans(iurb)
3333     format(1x,5(2x,i2),17(f12.2),(7x,l2),(9x,l2))
      endif
! End URBDBUG insert
   end do

   return
end subroutine urbcalc

subroutine grdurban
!***********************************************************************
!             GRDURBAN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Computes Urban Profiles
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:    June 11, 1996
!
!        MODIFIED:   Changed subroutine name from GRDURB to GRDURBAN.
!                    R. Brode, PES, 11/21/97
!
!        INPUTS:  Meteorological Variables for One Hour
!
!        OUTPUTS: Urban Profiles of sigma-v, sigma-w, Tlz, VPTG
!
!        ASSUMPTIONS:  <none>
!
!        CALLED FROM:  METEXT
!***********************************************************************

!---- Variable declarations
   use main1
   implicit none
   character :: modnam*12
   integer :: i
   double precision :: zdcrs, sv2, svurb, sv2dcr, val2, atzi, sw2,&
   &swurb

!---- Variable initializations
   modnam = 'GRDURBAN'

!     Save Rural Profiles

!     Save gridded profile arrays to 'rural' arrays (1:MXGLVL)
   grdsvr(:) = gridsv(:)
   grdswr(:) = gridsw(:)
   grdtgr(:) = gridtg(:)
   grdptr(:) = gridpt(:)

!     Loop Through Urban Areas
   do iurb = 1, numurb

      if (.not.stable .and. .not.L_MorningTrans(iurb)) cycle

      zdcrs  =  at1pt2 * ziurb(iurb)

!        Loop Through Grid Levels
      do i = 1, mxglvl

         sv2 = 0.35d0 * urbwstr(iurb)**2
!
         if( gridht(i)  <=  ziurb(iurb) )then
            svurb = dsqrt( sv2 )

         elseif( gridht(i) > ziurb(iurb) .and.&
         &gridht(i) <= zdcrs )then
!              COMPUTE sigmaV at 1.2*ZI
            sv2dcr = min( sv2, 0.25d0 )
!              INTERPOLATE between value of SV2 at ZI and at 1.2*ZI
            call gintrp ( ziurb(iurb), sv2, zdcrs, sv2dcr, gridht(i),&
            &val2 )
            svurb = dsqrt( val2 )

         else   ! requested height is above 1.2*urban mixing height
            atzi  = dsqrt( sv2 )
            svurb = min( atzi, 0.5d0 )

         endif
!

         if( gridht(i)  <=  0.1d0*ziurb(iurb) )then
            sw2 = 1.6d0 * ( gridht(i)/ziurb(iurb) )**(2.0d0*third) *&
            &urbwstr(iurb)**2
            swurb  = dsqrt( sw2 )

         elseif( gridht(i)>0.1d0*ziurb(iurb) .and.&
         &gridht(i)<=ziurb(iurb) )then
            swurb = dsqrt( 0.35d0 * urbwstr(iurb)**2 )

         else   ! requested height is above urban mixing height
            sw2 = 0.35d0 * urbwstr(iurb)**2 *&
            &dexp(-(6.d0*(gridht(i)-ziurb(iurb))/ziurb(iurb)))
            swurb = dsqrt( sw2 )

         endif
!
         grdsvu(i,iurb) = dsqrt(gridsv(i)**2 + svurb**2)
         grdswu(i,iurb) = dsqrt(gridsw(i)**2 + swurb**2)


!RCO D095 Added for urban debug 8/3/2021
         if (urbdbug) then
            if (i < 17) then
               write(urbunt1,3333) iurb,iyear,imonth,iday,ihour,i,&
               &gridht(i),&
               &gridsv(i),svurb,grdsvu(i,iurb),&
               &gridsw(i),swurb,grdswu(i,iurb)
3333           format(1x,6(2x,i2),9(f10.2))
            endif
         endif
! End URBDBUG insert

         if (gridht(i) <= ziurb(iurb)) then
            grdtgu(i,iurb) = 1.0d-5
         else
            grdtgu(i,iurb) = gridtg(i)
         end if

      end do

   end do

!---- Compute potential temperature profile from urban Dtheta/Dz profile
   call grdpturb

   return
end subroutine grdurban

subroutine grdpturb
!=======================================================================
!                GRDPTURB module of the AERMOD Dispersion Model
!
!   Purpose:     To construct a profile of gridded values of
!                potential temperature for URBAN cases
!
!   Input:       Profile of gridded potential temperature gradients
!                Temperature at the reference height
!                Profile of grid heights
!
!   Output:      Potential temperature profile at the grid heights.
!
!   Assumptions: There is at least one grid level below the reference
!                temperature height (which should be satisfied
!                because the lowest grid level is 0.5 meters)
!
!   Called by:   METEXT
!
!   Programmer:  Jim Paumier                          30 Sept 1993
!                Pacific Environmental Services
!
!   Revision history:
!        12/10/97  - R. Brode, PES, Inc.
!                    Corrected the order of array indices used for profiling
!                    potential temperature above the reference height.
!        12/16/94  - J. Paumier, PES, Inc.
!                  - CALL LOCATE to get the number of levels below the
!                    temperature reference height, replacing the original
!                    method which relied on grid heights being every 10 m
!
!-----------------------------------------------------------------------
!
!---- Variable declarations

   use main1
   implicit none
   character :: modnam*12
   integer :: l, nbelow, i
   double precision :: ptref

!---- Data definitions
!
!
!---- Data initializations
   modnam = 'GRDPTURB'
!
!
!.......................................................................
!

!---- Determine the grid level below the temperature reference
!     height (as defined in the scalar file)               ---- CALL LOCATE

   call locate( gridht, 1, mxglvl, trefht, nbelow )

!---- Compute the potential temperature at the reference level
!     using the reference temperature (TA), the reference
!     temperature height (TREFHT), and the base elevation
!     of the measurement site specified on the ME PROFBASE
!     keyword (ZBASE)

   ptref = ta + govrcp * (trefht + zbase)

!---- Loop Through Urban Areas
   do iurb = 1, numurb

      if (.not.stable .and. .not.L_MorningTrans(iurb)) cycle

!----    Compute the potential temperature at the grid level below
!        the temperature reference height

      grdptu(nbelow,iurb) = ptref -&
      &0.5d0 * (grdtgu(nbelow+1,iurb) + grdtgu(nbelow,iurb)) *&
      &(trefht - gridht(nbelow))


!----    Compute Potential Temp Values for Grid Levels Below Reference Ht.
      do l = nbelow-1, 1, -1

         grdptu(l,iurb) = grdptu(l+1,iurb) - 0.5d0 *&
         &(grdtgu(l+1,iurb) + grdtgu(l,iurb)) *&
         &(gridht(l+1) - gridht(l))

      end do


!----    Compute Potential Temp Values for Grid Levels Above Reference Ht.
      do l = nbelow+1, mxglvl

         grdptu(l,iurb) = grdptu(l-1,iurb) + 0.5d0 *&
         &(grdtgu(l,iurb) + grdtgu(l-1,iurb)) *&
         &(gridht(l) - gridht(l-1))

      end do

!RCO D168 Debug files. Added for temp profile for urban debug 3/17/2023
      if (urbdbug) then
         do i = 1, mxglvl
            write(urbunt2,3333) iurb,iyear,imonth,iday,ihour,i,nbelow,&
            &gridht(i),gridtg(i),grdtgu(i,iurb),&
            &gridpt(i),grdptu(i,iurb)
3333        format(1x,6(2x,i2),1(5x,i2),1(f10.2),2(e10.3),2(f10.4))
         end do
      endif
! End URBDBUG insert


   end do

   return
end subroutine grdpturb

!CRFL
!CRFL  Subroutine METDEB added to improve debug output of meteorological
!CRFL  data.
!CRFL

subroutine metdeb
!***********************************************************************
!             METDEB Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Writes debug output of this hour's meteorological
!                 data.
!
!        PROGRAMMERS:  Bob Paine (developer) and Russ Lee (implementation)
!
!        DATE:    August 18, 1994;  Revised September 26, 1994.
!
!        MODIFIED:   To include value for Theta* (THSTAR) in the METEOR
!                    debug option.
!                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 06/30/2015
!
!        INPUTS:  Meteorological data input to model
!
!        OUTPUTS: Meteorological data output to debug file
!
!        ASSUMPTIONS:  None
!
!        CALLED FROM:  HRLOOP
!***********************************************************************

!---- Variable declarations
   use main1
   implicit none
   character :: modnam*12
   integer :: i, ndist

!---- Variable initializations
   modnam = 'METDEB'

   if (methdr) then
!----    Modified to use 4-digit year (IYR) instead of 2-digit year (IYEAR)
      write (dbmunt, 6115) iyr, imonth, iday, ihour, zi, ta, ustar,&
      &wstar, obulen, sfcz0, thstar, uavg, svavg, swavg, uatzi,&
      &svatzi, swatzi, vptgzi
      write (dbmunt, 6118)
      do i = mxglvl, 1, -1
         write (dbmunt, 6120) i, gridht(i), gridwd(i), gridws(i),&
         &gridsv(i), gridsw(i), gridpt(i), gridtg(i)
      end do
      write (dbmunt, 6116)
      methdr = .false.
   endif

! --- Adjust for distances larger than output field
   if( dabs(x) > 999999.0d0 )then
      if( x < 0.0d0 )then
         ndist = -999999
      else
         ndist =  999999
      endif
   else
      ndist = idnint(x)
   endif

   if( stable  .or.  (unstab .and. (hs >= zi) ) )then
      write (dbmunt, 6131) irec, ndist, ueff, sveff, sweff
   else if(ppf >= 1.0d0) then
      write (dbmunt, 6132) irec, ndist, ueff3, sveff3, sweff3
   else if(ppf <= 0.0d0) then
      write (dbmunt, 6133) irec, ndist, ueffd, sveffd, sweffd,&
      &ueffn, sveffn, sweffn
   else
      write (dbmunt, 6134) irec, ndist, ueffd, sveffd, sweffd,&
      &ueffn, sveffn, sweffn, ueff3, sveff3, sweff3
   endif
!
6115 format( 1x, 80('-'),//,'  SURFACE AND PROFILE MET DATA:',/,&
   &t48, 'MONIN-     SFC',/,t17,&
   &'MIXING   TEMP                  OBUKHOV   ROUGH.',/,t17,&
   &'HEIGHT   @ HS    U*      W*    LENGTH    LENGTH    THSTAR',/,&
   &'  YR  MO DA HR',&
   &'    (M)    (K)   (M/S)   (M/S)    (M)       (M)',//,&
   &1x,i4,3i3,2x,f6.1,2x,f5.1,1x,f6.3,1x,f7.3,2x,f7.1,3x,f7.3,3x,&
   &f7.4///,&
   &' <--AVG: SFC TO ZI---> <--------VALUE AT ZI-------->',/,&
   &'   U    SIG-V  SIG-W      U    SIG-V  SIG-W   VPTG',/,&
   &' (M/S)  (M/S)  (M/S)    (M/S)  (M/S)  (M/S)   (K/M)',//,&
   &1x,f5.2,2(2x,f5.2),3x,f5.2,2x,f5.2,2x,f5.2,&
   &1x,f7.4,//)
6116 format(//,1x, '            <-STABLE/DIRECT EFF. VALUES-> ',&
   &'<-INDIRECT EFF. VALUES-> <-PENETRATED EFF. VALUES->',&
   &/,' RECEPT  DIST.    U    SIG-V  SIG-W   ',&
   &'       U    SIG-V  SIG-W          U    SIG-V  SIG-W',/)
!RJP 6117 FORMAT (1X,I5,1X,F6.0,3(2X,F5.2),1X,F7.0,3(2X,F5.2),2(1X,F7.0))
6118 format(5x,' GRID     WIND    WIND                    POT.',&
   &/,5x,'HEIGHT    DIR.    SPEED   SIG-V   SIG-W   TEMP.',&
   &'  VPTG',/,&
   &5x,' (M)     (DEG)    (M/S)   (M/S)   (M/S)    (K)  ',&
   &' (K/M)',/)
6120 format (i4,f7.1,2x,f6.1,2x,f7.2,1x,f7.2,1x,f7.2,1x,f6.2,&
   &1x,f9.6)

!RJP  Add new FORMAT statements here.

6131 format (i5,1x,i7,1x,3(2x,f5.2))
6132 format (i5,1x,i7,1x,54x,3(2x,f5.2))
6133 format (i5,1x,i7,1x,2(3(2x,f5.2),6x))
6134 format (i5,1x,i7,1x,3(3(2x,f5.2),6x))

   return
end subroutine metdeb

subroutine grdeps
!***********************************************************************
!             GRDEPS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Computes profile of epsilon, turbulence dissipation
!                 rate, for PVMRM or GRSM option
!
!        PROGRAMMER: Roger Brode, PES, Inc.
!
!        DATE:       May 13, 2002
!
!        INPUTS:
!
!        OUTPUTS:
!
!        ASSUMPTIONS:
!
!        CALLED FROM:  METEXT
!***********************************************************************

!---- Variable declarations
   use main1
   implicit none
   character :: modnam*12
   integer :: i
   double precision :: tsublr
   double precision, parameter :: ar1 = 0.46d0

!---- Variable initializations
   modnam = 'GRDEPS'

!     Loop Through Grid Levels
   do i = 1, mxglvl

      tsublr = ar1 * zi/gridsw(i)
      grideps(i) = 0.78d0 * gridsw(i)*gridsw(i)/tsublr

   end do

   return
end subroutine grdeps
