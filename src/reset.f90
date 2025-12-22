recursive subroutine recard
!***********************************************************************
!                 RECARD Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: To process REceptor Pathway card images
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   Removed obsolete reference to IRSTAT(7).
!                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
!
!        MODIFIED:   To remove some restrictions on the order of
!                    the BOUNDELV keyword - 9/29/92
!
!        MODIFIED:   To include the recursive keyword for D112
!                    Wood - 9/29/22
!
!        INPUTS:  Pathway (RE) and Keyword
!
!        OUTPUTS: Receptor Arrays
!                 Receptor Setup Status Switches
!
!        CALLED FROM:   SETUP
!***********************************************************************

!     Variable Declarations
   use main1
   use buoyant_line

   implicit none
   character :: modnam*12

   integer :: ilsave, kk

!     Variable Initializations
   modnam = 'RECARD'

   if (keywrd == 'STARTING') then
!        Initialize Counters and Set Status Switch
      irec = 0
      innet = 0
      numrec = 0
      numarc = 0
      irxr = 0
      iryr = 0
      irze = 0
      irzh = 0
      irzf = 0
      pxsoid = ' '
      pesoid = ' '
      ista = .false.
      irstat(1) = irstat(1) + 1
      if (irstat(1) /= 1) then
!           Error Message: Repeat Starting In Same Pathway
         call errhdl(path,modnam,'E','135',keywrd)
      end if
!        Flush the Working Arrays
      zetmp1(:) = 0.0d0
      zetmp2(:) = 0.0d0
      zftmp1(:) = 0.0d0
      zftmp2(:) = 0.0d0
   else if (keywrd == 'GRIDCART') then
!        Set Status Switch
      irstat(2) = irstat(2) + 1
!        Process Cartesian Grid Receptor Network            ---   CALL RECART
      call recart
   else if (keywrd == 'GRIDPOLR') then
!        Set Status Switch
      irstat(3) = irstat(3) + 1
!        Process Polar Receptor Network                     ---   CALL REPOLR
      call repolr
   else if (keywrd == 'DISCCART') then
!        Set Status Switch
      irstat(4) = irstat(4) + 1
!        Process Discrete Cartesian Receptor Locations      ---   CALL DISCAR
      call discar
   else if (keywrd == 'DISCPOLR') then
!        Set Status Switch
      irstat(5) = irstat(5) + 1
!        Process Discrete Polar Receptor Locations          ---   CALL DISPOL
      call dispol
   else if (keywrd == 'EVALCART') then
!        Set Status Switch
      irstat(8) = irstat(8) + 1
      if (psdcredit) then
!           Write Error Message: EVALCART not valid with PSDCREDIT option
         call errhdl(path,modnam,'E','147',keywrd)
      else
!           Process Discrete Cartesian Receptor Locations   ---   CALL EVCART
         call evcart
      end if
   else if (keywrd == 'ELEVUNIT') then
!        Set Status Switch
      irstat(9) = irstat(9) + 1
      if (irstat(9) /= 1) then
!           WRITE Error Message: Repeat Non-repeatable Keyword
         call errhdl(path,modnam,'E','135',keywrd)
      else if (irstat(2) > 0 .or. irstat(3) > 0 .or.&
      &irstat(4) > 0 .or. irstat(5) > 0 .or.&
      &irstat(8) > 0) then
!           Write Error Message: ELEVUNIT must be first card after STARTING
         call errhdl(path,modnam,'E','152','  RE')
      else
!           Process Elevation Units for Source Elevations   ---   CALL REELUN
         call reelun
      end if
   else if (keywrd == 'INCLUDED') then
!        Set Status Switch
      irstat(11) = irstat(11) + 1
!        Save ILINE as ISAVE
      ilsave = iline
!        Process the Included Receptor File                 ---   CALL INCLUD
      call includ
!        Retrieve ILINE From ISAVE
      iline = ilsave
   else if (keywrd == 'FINISHED') then
!        Set Status Switch
      irstat(50) = irstat(50) + 1
      if (irstat(50) /= 1) then
!           Error Message: Repeat Finished In Same Pathway
         call errhdl(path,modnam,'E','135',keywrd)
         go to 999
      end if
!        Write Out The Error Message: Mandatory Keyword Missing
      if (irstat(1) == 0)then
         call errhdl(path,modnam,'E','130','STARTING')
      end if

      if (ista) then
!           WRITE Error Message:  Missing END Keyword for a Grid Network
         call errhdl(path,modnam,'E','175',pnetid)
      end if

!        Set Total Number of Receptors for This Run, NUMREC
      numrec = irxr
      if (numrec == 0) then
!           WRITE Error Message:  No Receptors Defined
         call errhdl(path,modnam,'E','185','NUMREC=0')
      end if

!        Reinitialize ZFLAG array if needed
      if (.not. flgpol) then
         do irec = 1, numrec
            azflag(irec) = 0.0d0
         end do
      end if

! ---    Check for missing receptor elevations, coded as -9999.0,
!        and convert from FEET to METERS if needed
      do irec = 1, numrec
         if (azelev(irec) < -9998.99d0) then
!              WRITE Error Message:  Receptor elevation is missing
            write(dummy,'(I8)') irec
            call errhdl(path,modnam,'E','259',dummy)
         else if (azhill(irec) < -9998.99d0) then
!              WRITE Error Message:  Receptor hill height scale is missing
            write(dummy,'(I8)') irec
            call errhdl(path,modnam,'E','259',dummy)
         else if (reelev == 'FEET') then
            azelev(irec) = 0.3048d0*azelev(irec)
            azhill(irec) = 0.3048d0*azhill(irec)
         end if
      end do

! ---    If there is a buoyant line source, the receptors need to be
!          translated and rotated so they are oriented the same as the
!          translation and rotation for the buoyant line source - use
!          the buoyant line source endpoints to determine translation
!          and rotation. This translation/rotation is only performed once.
!
!          NOTE1: The translation/rotation is BL source group dependent.
!
!          NOTE2: this is an initial rotation based on the orientation of
!           buoyant line source relative to the x-y axes (with +y
!           pointing north); the receptors will be rotated again each
!           hour based on the wind direction)

! Multiple_BuoyLines_D41_Wood
!        Process receptors based on BL sources/groups

      if (nbltotal>= 1) then
         bl_rflag = .false.
         do kk = 1, numblgrps
            call blrecp(kk)                      !  ---   CALL BLRECP
         end do
      end if

   else
!        Write Error Message:  Invalid Keyword for This Pathway
      call errhdl(path,modnam,'E','110',keywrd)
   end if

999 return
end subroutine recard

subroutine reelun
!***********************************************************************
!                 REELUN Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Process Elevation Units Option for Receptors
!                 From Runstream Input Image
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 22, 1994
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Receptor Elevation Units Switch
!
!        ERROR HANDLING:   Checks for Invalid Parameters;
!                          Checks for No Parameters;
!                          Checks for Too Many Parameters
!
!        CALLED FROM:   RECARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

!     Variable Initializations
   modnam = 'REELUN'

   if (ifc == 3) then
      if (field(3) == 'METERS') then
         reelev = 'METERS'
      else if (field(3) == 'FEET') then
         reelev = 'FEET'
      else
!           WRITE Error Message  ! Invalid Parameter
         call errhdl(path,modnam,'E','203','RE_ELEV')
      end if
   else if (ifc > 3) then
!        WRITE Error Message     ! Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
   else
!        WRITE Error Message     ! No Parameters
      call errhdl(path,modnam,'E','200','ElevUnit')
   end if

   return
end subroutine reelun

subroutine recart
!***********************************************************************
!                 RECART Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Cartesian Grid Receptor Network Inputs
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To Fix Error Checking - Compare NETIDT With
!                    Full Secondary Keywords - 9/29/92
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Cartesian Grid Receptor Network Inputs
!
!        CALLED FROM:   RECARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i

!     Variable Initializations
   modnam = 'RECART'

!     READ in the Netid and Nettype
   if (ifc < 3) then
!        Write Error Message: Missing Data Field
      call errhdl(path,modnam,'E','200',keywrd)
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
      innet = innet + 1
      if (innet > nnet) then
!           WRITE Error Message:  Too Many Networks
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NNET='',I7)') nnet
         call errhdl(path,modnam,'E','290',dummy)
         recerr = .true.
         go to 999
      end if
      incset = 0
      ixyset = 0
      ievset = 0
      ifgset = 0
   else
!        Error Message: Invalid Secondary Keyword
      call errhdl(path,modnam,'E','170',pnetid)
      recerr = .true.
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
      ize = 0
      izh = 0
      izf = 0
      idc1 = irxr
!        Check for Previous Grid Network With Same ID
      do i = 1, innet-1
         if (field(3) == ntid(i)) then
!              WRITE Warning Message:  Duplicate Network ID
            call errhdl(path,modnam,'W','252',ntid(i))
         end if
      end do
   else if (ktype == 'XYINC') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Error Message:Conflict Secondary Keyword
      if (ixyset /= 0) then
         call errhdl(path,modnam,'E','180',netidt)
      end if
!        Set the Uniform Spacing Receptor Network           ---   CALL GENCAR
      call gencar
      incset = incset + 1
   else if (ktype=='XPNTS' .or. ktype=='YPNTS') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Error Message:Conflict Secondary Keyword
      if (incset /= 0) then
         call errhdl(path,modnam,'E','180',netidt)
      end if
!        Set the Non-uniform Spacing Receptor Network       ---   CALL XYPNTS
      call xypnts
      ixyset = ixyset + 1
   else if (ktype == 'ELEV') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Read in and set the Terrain Elevation              ---   CALL TERHGT
      call terhgt
      ievset = ievset + 1
   else if (ktype == 'HILL') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Read in and set the Terrain Elevation              ---   CALL HILHGT
      call hilhgt
      ihlset = ihlset + 1
   else if (ktype == 'FLAG') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Read in and set the Flagpole Receptor              ---   CALL FLGHGT
      call flghgt
      ifgset = ifgset + 1
   else if (ktype == 'END') then
      iend = .true.
!        Get The Final Results
      if (.not. ista) then
!           Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      else if (.not. recerr) then
         call setcar
      end if
      ista = .false.
      newid = .true.
!        Check If The Secondary Parameter Has Been Specified
      if (ixyset==0 .and. incset==0) then
!           WRITE Error Message: Missing (X,Y) Point Setting
         call errhdl(path,modnam,'E','212',netidt)
      end if

!        Warning: Elevated Terrain Inputs Inconsistent With Options
      if (elev .and. (ievset==0 .or. ihlset==0)) then
         call errhdl(path,modnam,'W','214',netidt)
         irze = irxr
         irzh = irze
      else if (flat .and. ievset/=0) then
         call errhdl(path,modnam,'W','213',netidt)
         irze = irxr
         irzh = irze
      else if (flat .and. ievset==0) then
         irze = irxr
         irzh = irze
      end if

!        Warning: Flagpole Receptor Inputs Inconsistent With Options
      if (flgpol .and. ifgset==0) then
         call errhdl(path,modnam,'W','216',netidt)
         irzf = irxr
      else if (.not.flgpol .and. ifgset/=0) then
         call errhdl(path,modnam,'W','215',netidt)
         irzf = irxr
      else if (.not.flgpol .and. ifgset==0) then
         irzf = irxr
      end if

!        Check If The Number of Elev & Flag Is Match
      if (elev .and. ievset/=0) then
         if (icount*jcount /= ize) then
!              Write Out The Error Message: No. Of ELEV not match
            call errhdl(path,modnam,'E','218','ELEV')
         end if
         if (icount*jcount /= izh) then
!              Write Out The Error Message: No. Of ZHILL not match
            call errhdl(path,modnam,'E','218','ZHILL')
         end if
      end if
      if (flgpol .and. ifgset/=0) then
         if (icount*jcount /= izf) then
!              Write Out The Error Message: No. Of FLAG not match
            call errhdl(path,modnam,'E','218','FLAG')
         end if
      end if

   else
!        Error Message: Invalid Secondary Keyword
      call errhdl(path,modnam,'E','170',netidt)
      recerr = .true.
      go to 999

   end if

   pnetid = netidt

999 return
end subroutine recart

subroutine gencar
!***********************************************************************
!                 GENCAR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Generates Cartesian Grid Receptor Network With
!                 Uniform Spacing
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Cartesian Grid Receptor Network With Uniform
!                 Spacing
!
!        CALLED FROM:   RECART
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k
   double precision :: xdelta, ydelta, tempp(6)
   logical :: error

!     Variable Initializations
   modnam = 'GENCAR'
   error = .false.

!     Check for Location of Secondary Keyword, XYINC
   do i = 1, ifc
      if (field(i) == 'XYINC') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','200',keywrd)
      recerr = .true.
      go to 999
   else if (ifc > isc+5) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',ktype)
      recerr = .true.
      go to 999
   else if (ifc < isc+5) then
!        Error Message: Too Few Parameters
      call errhdl(path,modnam,'E','201',ktype)
      recerr = .true.
      go to 999
   end if

!     Input The Numerical Values
   do k = 1,6
      call stodbl(field(isc + k-1),ilen_fld,tempp(k),imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         error = .true.
         recerr = .true.
      end if
   end do

   if (error) then
      error = .false.
      go to 999
   end if

!     Assign Values to Appropriate Variables for Generated Network
   xint   = tempp(1)
   icount = idnint(tempp(2))
   xdelta = tempp(3)
   yint   = tempp(4)
   jcount = idnint(tempp(5))
   ydelta = tempp(6)

!     Assign Them to the Coordinate Arrays
   if (icount <= ixm) then
      do i = 1, icount
         xcoord(i,innet) = xint + xdelta*dble(i-1)
      end do
   else
!        Error Msg: Maximum Number Of X-coordinates Exceeded
!        This shouldn't occur since limits are dynamically allocated
      write(dummy,'(''IXM ='',I7)') ixm
      call errhdl(path,modnam,'E','290',dummy)
   end if
   if (jcount <= iym) then
      do j = 1, jcount
         ycoord(j,innet) = yint + ydelta*dble(j-1)
      end do
   else
!        Error Msg: Maximum Number Of Y-coordinates Exceeded
!        This shouldn't occur since limits are dynamically allocated
      write(dummy,'(''IYM ='',I7)') iym
      call errhdl(path,modnam,'E','290',dummy)
   end if

999 return
end subroutine gencar

subroutine xypnts
!***********************************************************************
!                 XYPNTS Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Cartesian Grid x,y Input Value
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To Fix Error Checking - Change Limit for DO 15
!                    To 'JSET -1' - 9/29/92
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Cartesian Grid x,y Input Value
!
!        CALLED FROM:   RECART
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, jset

!     Variable Initializations
   modnam = 'XYPNTS'

   if (ktype == 'XPNTS') then
!        Check for Location of Secondary Keyword, XPNTS
      do i = 1, ifc
         if (field(i) == 'XPNTS') then
            isc = i + 1
         end if
      end do

!        Determine Whether There Are Enough Parameter Fields
      if (ifc == isc-1) then
!           Error Message: Missing Parameter
         call errhdl(path,modnam,'E','200',keywrd)
         recerr = .true.
         go to 999
      end if

      iset = icount
      do i = isc, ifc
         call stodbl(field(i),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            recerr = .true.
         end if
         iset = iset + 1
         if (iset <= ixm) then
            xcoord(iset,innet) = dnum
            do j = 1, iset-1
               if (dnum == xcoord(j,innet)) then
!                    WRITE Warning Message:  X-Coord Specified More Than Once
                  call errhdl(path,modnam,'W','250',netidt)
               end if
            end do
         else
!              Error Msg: Maximum Number Of X-coordinates Exceeded
!              This shouldn't occur since limits are dynamically allocated
            write(dummy,'(''IXM ='',I7)') ixm
            call errhdl(path,modnam,'E','290',dummy)
            recerr = .true.
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
!           Error Message: Missing Parameter
         call errhdl(path,modnam,'E','200',keywrd)
         recerr = .true.
         go to 999
      end if

      jset = jcount

      do i = isc, ifc
         call stodbl(field(i),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
            recerr = .true.
         end if
         jset = jset + 1
         if (jset <= iym) then
            ycoord(jset,innet) = dnum
            do j = 1, jset-1
               if (dnum == ycoord(j,innet)) then
!                    WRITE Warning Message:  Y-Coord Specified More Than Once
                  call errhdl(path,modnam,'W','250',netidt)
               end if
            end do
         else
!              Error Msg: Maximum Number Of Y-coordinates Exceeded
!              This shouldn't occur since limits are dynamically allocated
            write(dummy,'(''IYM ='',I7)') iym
            call errhdl(path,modnam,'E','290',dummy)
            recerr = .true.
         end if
      end do
      jcount = jset
   end if

999 return
end subroutine xypnts

subroutine setcar
!***********************************************************************
!                 SETCAR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Setup the Final Cartesian Grid Receptor Network Inputs
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  The GRIDCART Sub-pathway Input Parameters
!
!        OUTPUTS: Cartesian Grid Receptor Network Inputs
!
!        CALLED FROM:   RECART
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, jset

!     Variable Initializations
   modnam = 'SETCAR'

   if (icount/=0 .and. jcount/=0) then
!        Setup The Coordinate Of The Receptors
      netsta(innet) = irxr + 1
      iset = irxr
      jset = iryr
      do j = 1, jcount
         do i = 1, icount
            iset = iset + 1
            jset = jset + 1
            if (iset > nrec) then
!                 Error Msg: Maximum Number Of Receptor Exceeded
!                 This shouldn't occur since limits are dynamically allocated
               write(dummy,'(''NREC='',I7)') nrec
               call errhdl(path,modnam,'E','290',dummy)
               go to 999
            end if
            if (icount > ixm) then
!                 Error Msg: Maximum Number Of X-coordinates Exceeded
!                 This shouldn't occur since limits are dynamically allocated
               write(dummy,'(''IXM ='',I7)') ixm
               call errhdl(path,modnam,'E','290',dummy)
               go to 999
            end if
            if (jcount > iym) then
!                 Error Msg: Maximum Number Of Y-coordinates Exceeded
!                 This shouldn't occur since limits are dynamically allocated
               write(dummy,'(''IYM ='',I7)') iym
               call errhdl(path,modnam,'E','290',dummy)
               go to 999
            end if
            axr(iset) = xcoord(i,innet)
            ayr(jset) = ycoord(j,innet)
         end do
      end do
      irxr = iset
      iryr = jset
      netend(innet) = irxr
      numxpt(innet) = icount
      numypt(innet) = jcount
      ntid(innet)   = netidt
      nttyp(innet)  = 'GRIDCART'
!        Define ITAB, NXTOX, NYTOX Variables for TOXXFILE Option, 9/29/92
      if (itab < 0) then
!           First Receptor Network Defined - Set Variables
         itab  = 2
         nxtox = icount
         nytox = jcount
      else
!           Previous Receptors Have Been Defined - Reset ITAB = 0
         itab = 0
      end if
   end if

!     Setup The AZELEV Array
   call sbyval(zetmp1,zetmp2,ize)
   iset = irze
   do i = 1, ize
      iset = iset + 1
      if (iset > nrec) then
!           Error Msg: Maximum Number Of Receptor Exceeded
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NREC='',I7)') nrec
         call errhdl(path,modnam,'E','290',dummy)
         go to 999
      end if
      azelev(iset) = zetmp2(i)
   end do
   irze = iset

!     Setup The AZHILL Array
   call sbyval(zhtmp1,zhtmp2,izh)
   iset = irzh
   do i = 1, izh
      iset = iset + 1
      if (iset > nrec) then
!           Error Msg: Maximum Number Of Receptor Exceeded
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NREC='',I7)') nrec
         call errhdl(path,modnam,'E','290',dummy)
         go to 999
      end if
      azhill(iset) = zhtmp2(i)
   end do
   irzh = iset

!     Setup The AZFLAG Aarry
   call sbyval(zftmp1,zftmp2,izf)
   iset = irzf
   do i = 1, izf
      iset = iset + 1
      if (iset > nrec) then
!           Error Msg: Maximum Number Of Receptor Exceeded
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NREC='',I7)') nrec
         call errhdl(path,modnam,'E','290',dummy)
         go to 999
      end if
      azflag(iset) = zftmp2(i)
   end do
   irzf = iset

   do i = idc1+1, irxr
      netid(i) = netidt
      rectyp(i) = 'GC'
   end do

999 return
end subroutine setcar

subroutine repolr
!***********************************************************************
!                 REPOLR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Polar Grid Receptor Network Inputs
!
!        PROGRAMMER:  Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Receptor Network Inputs
!
!        CALLED FROM:   RECARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer       :: i
   integer, save :: iorset, ixrset, idrset, igrset

!     Variable Initializations
   modnam = 'REPOLR'

   if (ifc < 3) then
!        Write Error Message: Missing Data Field
      call errhdl(path,modnam,'E','200',keywrd)
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
      innet = innet + 1
      if (innet > nnet) then
!           WRITE Error Message:  Too Many Networks
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NNET='',I7)') nnet
         call errhdl(path,modnam,'E','290',dummy)
         recerr = .true.
         go to 999
      end if
      iorset = 0
      ixrset = 0
      idrset = 0
      igrset = 0
      ievset = 0
      ifgset = 0
   else
!        Error Message: Invalid Secondary Keyword
      call errhdl(path,modnam,'E','170',pnetid)
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
      ize = 0
      izh = 0
      izf = 0
      idc1 = irxr
!        Check for Previous Grid Network With Same ID
      do i = 1, innet-1
         if (field(3) == ntid(i)) then
!              WRITE Warning Message:  Duplicate Network ID
            call errhdl(path,modnam,'W','252',ntid(i))
         end if
      end do
   else if (ktype == 'ORIG') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Error Message: Conflict Secondary Keyword
      if (iorset /= 0) then
         call errhdl(path,modnam,'E','160',netidt)
      end if
!        Read In XINT, YINT                                 ---   CALL POLORG
      call polorg
      iorset = iorset + 1
   else if (ktype == 'DIST') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Read in the Distance Set                           ---   CALL POLDST
      call poldst
      ixrset = ixrset + 1
   else if (ktype == 'GDIR') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Error Message: Conflict Secondary Keyword
      if (idrset /= 0) then
         call errhdl(path,modnam,'E','180',netidt)
      end if
!        Set the Uniform Spacing Receptor Network           ---   CALL GENPOL
      call genpol
      igrset = igrset + 1
   else if (ktype == 'DDIR') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Error Message: Conflict Secondary Keyword
      if (igrset /= 0) then
         call errhdl(path,modnam,'E','180',netidt)
      end if
!        Set the Non-uniform Spacing Receptor Network       ---   CALL RADRNG
      call radrng
      idrset = idrset + 1
   else if (ktype == 'ELEV') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Read in and set the Terrain Elevation              ---   CALL TERHGT
      call terhgt
      ievset = ievset + 1
   else if (ktype == 'HILL') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Read in and set the Terrain Elevation              ---   CALL HILHGT
      call hilhgt
      ihlset = ihlset + 1
   else if (ktype == 'FLAG') then
      if (.not. ista) then
!*          Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      end if
!        Read in and set the Flagpole Receptor              ---   CALL FLGHGT
      call flghgt
      ifgset = ifgset + 1
   else if (ktype == 'END') then
      iend = .true.
!        Get the Final Result
      if (.not. ista) then
!           Write Error: MISSING STA OF THE BLOCK DATA
         call errhdl(path,modnam,'E','200','  STA   ')
      else if (.not. recerr) then
         call setpol
      end if
      ista = .false.
      newid = .true.
!        Check If The Secondary Parameter Has Been Specified
!        Warning Message: Missing (Xin,Yin) Point Setting
      if (iorset == 0) then
         call errhdl(path,modnam,'W','220',netidt)
         xint = 0.0d0
         yint = 0.0d0
      end if
!        Error Message: Missing Distance Point Setting
      if (ixrset == 0) then
         call errhdl(path,modnam,'E','221',netidt)
      end if
!        Error Message: Missing Degree Or Rad Setting
      if (igrset==0 .and. idrset==0) then
         call errhdl(path,modnam,'E','221',netidt)
      end if

!        Warning: Elevated Terrain Inputs Inconsistent With Options
      if (elev .and. (ievset==0 .or. ihlset==0)) then
         call errhdl(path,modnam,'W','214',netidt)
         irze = irxr
         irzh = irze
      else if (flat .and. ievset/=0) then
         call errhdl(path,modnam,'W','213',netidt)
         irze = irxr
         irzh = irze
      else if (flat .and. ievset==0) then
         irze = irxr
         irzh = irze
      end if

!        Warning: Flagpole Receptor Inputs Inconsistent With Options
      if (flgpol .and. ifgset==0) then
         call errhdl(path,modnam,'W','216',netidt)
         irzf = irxr
      else if (.not.flgpol .and. ifgset/=0) then
         call errhdl(path,modnam,'W','215',netidt)
         irzf = irxr
      else if (.not.flgpol .and. ifgset==0) then
         irzf = irxr
      end if

!        Check If The Number of Elev & Flag Is Match
      if (elev .and. ievset/=0) then
         if (icount*jcount /= ize) then
!              Write Out The Error Message: No. Of ELEV not match
            call errhdl(path,modnam,'E','218','ELEV')
         end if
         if (icount*jcount /= izh) then
!              Write Out The Error Message: No. Of ZHILL not match
            call errhdl(path,modnam,'E','218','ZHILL')
         end if
      end if
      if (flgpol .and. ifgset/=0) then
         if (icount*jcount /= izf) then
!              Write Out The Error Message: No. Of FLAG not match
            call errhdl(path,modnam,'E','218','FLAG')
         end if
      end if

   else
!        Error Message: Invalid Secondary Keyword
      call errhdl(path,modnam,'E','170',netidt)
      recerr = .true.
      go to 999

   end if

   pnetid = netidt

999 return
end subroutine repolr

subroutine polorg
!***********************************************************************
!                 POLORG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Input The Original of The Polar Network
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Network Origin  Coordinates
!
!        CALLED FROM:   REPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, isdx
   character (len=12) :: soid
   logical :: found

!     Variable Initializations
   modnam = 'POLORG'
   found = .false.

!     Check for the Location of the Secondary Keyword, ORIG
   do i = 1, ifc
      if (field(i) == 'ORIG') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','200',keywrd)
      recerr = .true.
      go to 999
   else if (ifc > isc+1) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',ktype)
      recerr = .true.
      go to 999
   end if

   if (ifc == isc) then
!*       Identify Origin Associated With a Source ID
!*       First check for length of SRCID field <=12
      if ((loce(isc)-locb(isc)) <= 11) then
!*          Retrieve Source ID Character Substring
         soid = field(isc)
      else
!*          WRITE Error Message:  Source ID Field is Too Long
         call errhdl(path,modnam,'E','230',field(isc)(1:12))
         recerr = .true.
         go to 999
      end if
!*       Check for valid SRCID
      call sindex(srcid,nsrc,soid,isdx,found)
      if (.not. found) then
!           Error Message: Source ID Does Not Match Existing Sources
         call errhdl(path,modnam,'E','300',keywrd)
         recerr = .true.
      else
         xint = axs(isdx)
         yint = ays(isdx)
      end if

   else
!        Input Numerical Values, XINT and YINT
      call stodbl(field(isc),ilen_fld,xint,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         recerr = .true.
      end if

      call stodbl(field(isc + 1),ilen_fld,yint,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         recerr = .true.
      end if
   end if

999 return
end subroutine polorg

subroutine poldst
!***********************************************************************
!                 POLDST Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Gets Distances for the Polar Network
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Network Distance Input Value
!
!        CALLED FROM:   REPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j

!     Variable Initializations
   modnam = 'POLDST'

!     Skip the Unrelated Fields
   do i = 1, ifc
      if (field(i) == 'DIST') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','200',keywrd)
      recerr = .true.
      go to 999
   end if

   iset = icount

   do i = isc, ifc
      call stodbl(field(i),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         recerr = .true.
      end if
      iset = iset + 1
      if (iset <= ixm) then
!           Store Distance to XCOORD Array and Check for Previous Occurrence
         xcoord(iset,innet) = dnum
         do j = 1, iset-1
            if (dnum == xcoord(j,innet)) then
!                 WRITE Warning Message:  Distance Specified More Than Once
               call errhdl(path,modnam,'W','250',netidt)
            end if
         end do
      else
!           Error Msg: Maximum Number Of X-coordinates Exceeded
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''IXM ='',I7)') ixm
         call errhdl(path,modnam,'E','290',dummy)
         recerr = .true.
      end if
   end do

   icount = iset

999 return
end subroutine poldst

subroutine genpol
!***********************************************************************
!                 GENPOL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Generates Polar Receptor Network With
!                 Uniform Spacing
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Receptor Network With Uniform Direction Spacing
!
!        CALLED FROM:   REPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, k
   double precision :: dirini, dirinc, tempp(3)
   logical :: error

!     Variable Initializations
   modnam = 'GENPOL'
   error = .false.

!     Check for the Location of the Secondary Keyword, GDIR
   do i = 1, ifc
      if (field(i) == 'GDIR') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','200',keywrd)
      recerr = .true.
      go to 999
   else if (ifc < isc+2) then
!        Error Message: Not Enough Parameters
      call errhdl(path,modnam,'E','201',ktype)
      recerr = .true.
      go to 999
   else if (ifc > isc+2) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',ktype)
      recerr = .true.
      go to 999
   end if

!     Input Numerical Values
   do k = 1, 3
      call stodbl(field(isc + k-1),ilen_fld,tempp(k),imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         recerr = .true.
         error = .true.
      end if
   end do

   if (error) then
      error = .false.
      go to 999
   end if

   jcount = idnint(tempp(1))
   dirini = tempp(2)
   dirinc = tempp(3)

!     Assign Them to the Coordinate Arrays
   if (jcount <= iym) then
      do j = 1, jcount
         ycoord(j,innet) = (dirini + dirinc*dble(j-1))
         if (ycoord(j,innet) > 360.0d0) then
            ycoord(j,innet) = ycoord(j,innet) - 360.0d0
         else if (ycoord(j,innet) <= 0.0d0) then
            ycoord(j,innet) = ycoord(j,innet) + 360.0d0
         end if
      end do
   else
!        Error Msg: Maximum Number Of Y-coordinates Exceeded
!        This shouldn't occur since limits are dynamically allocated
      write(dummy,'(''IYM ='',I7)') iym
      call errhdl(path,modnam,'E','290',dummy)
      recerr = .true.
   end if

999 return
end subroutine genpol

subroutine radrng
!***********************************************************************
!                 RADRNG Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Non-Uniform Polar Network Value
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Polar Network Directions in Non-Uniform Spacing
!
!        CALLED FROM:   REPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j

!     Variable Initializations
   modnam = 'RADRNG'

!     Skip the non-useful Fields
   do i = 1, ifc
      if (field(i) == 'DDIR') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','200',keywrd)
      recerr = .true.
      go to 999
   end if

   iset = jcount

   do i = isc, ifc
      call stodbl(field(i),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit /= 1) then
         call errhdl(path,modnam,'E','208',keywrd)
         recerr = .true.
      end if
      iset = iset + 1
      if (iset <= iym) then
!           Store Direction to YCOORD Array, Adjust to 0-360 Range if Needed,
!           and Check for Previous Occurrence
         ycoord(iset,innet) = dnum
         if (ycoord(iset,innet) > 360.0d0) then
            ycoord(iset,innet) = ycoord(iset,innet) - 360.0d0
         else if (ycoord(iset,innet) <= 0.0d0) then
            ycoord(iset,innet) = ycoord(iset,innet) + 360.0d0
         end if
         do j = 1, iset-1
            if (dnum == ycoord(j,innet)) then
!                 WRITE Warning Message:  Direction Specified More Than Once
               call errhdl(path,modnam,'W','250',netidt)
            end if
         end do
      else
!           Error Msg: Maximum Number Of Y-coordinates Exceeded
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''IYM ='',I7)') iym
         call errhdl(path,modnam,'E','290',dummy)
         recerr = .true.
      end if
   end do

   jcount = iset

999 return
end subroutine radrng

subroutine setpol
!***********************************************************************
!                 SETPOL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Setup the Final Polar Receptor Network Inputs
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  The GRIDPOLR Sub-pathway Input Parameters
!
!        OUTPUTS: Polar Receptor Network Arrays
!
!        CALLED FROM:   REPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j, jset
   double precision :: ytemp

!     Variable Initializations
   modnam = 'SETPOL'

   if (icount/=0 .and. jcount/=0) then
!        Setup The Coordinate Of The Receptors
      netsta(innet) = irxr + 1
      iset = irxr
      jset = iryr
      do j = 1, jcount
         do i = 1, icount
            iset = iset + 1
            jset = jset + 1
            if (iset > nrec) then
!                 Error Msg: Maximum Number Of Receptor Exceeded
!                 This shouldn't occur since limits are dynamically allocated
               write(dummy,'(''NREC='',I7)') nrec
               call errhdl(path,modnam,'E','290',dummy)
               go to 999
            end if
            if (icount > ixm) then
!                 Error Msg: Maximum Number Of X-coordinates Exceeded
!                 This shouldn't occur since limits are dynamically allocated
               write(dummy,'(''IXM ='',I7)') ixm
               call errhdl(path,modnam,'E','290',dummy)
               recerr = .true.
            end if
            if (jcount > iym) then
!                 Error Msg: Maximum Number Of Y-coordinates Exceeded
!                 This shouldn't occur since limits are dynamically allocated
               write(dummy,'(''IYM ='',I7)') iym
               call errhdl(path,modnam,'E','290',dummy)
               recerr = .true.
            end if
            ytemp = ycoord(j,innet) * dtorad
            axr(iset) = xint + xcoord(i,innet)*dsin(ytemp)
            ayr(jset) = yint + xcoord(i,innet)*dcos(ytemp)
         end do
      end do
      irxr = iset
      iryr = jset
      xorig(innet)  = xint
      yorig(innet)  = yint
      netend(innet) = irxr
      numxpt(innet) = icount
      numypt(innet) = jcount
      ntid(innet)   = netidt
      nttyp(innet)  = 'GRIDPOLR'
!        Define ITAB, NXTOX, NYTOX Variables for TOXXFILE Option, 9/29/92
      if (itab < 0) then
!           First Receptor Network Defined - Set Variables
         itab  = 1
         nxtox = icount
         nytox = jcount
      else
!           Previous Receptors Have Been Defined - Reset ITAB = 0
         itab = 0
      end if
   end if

!     Setup The AZELEV Array
   call sbyval(zetmp1,zetmp2,ize)
   iset = irze
   do i = 1, ize
      iset = iset + 1
      if (iset > nrec) then
!           Error Msg: Maximum Number Of Receptor Exceeded
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NREC='',I7)') nrec
         call errhdl(path,modnam,'E','290',dummy)
         go to 999
      end if
      azelev(iset) = zetmp2(i)
   end do
   irze = iset

!     Setup The AZHILL Array
   call sbyval(zhtmp1,zhtmp2,izh)
   iset = irzh
   do i = 1, izh
      iset = iset + 1
      if (iset > nrec) then
!           Error Msg: Maximum Number Of Receptor Exceeded
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NREC='',I7)') nrec
         call errhdl(path,modnam,'E','210',dummy)
         go to 999
      end if
      azhill(iset) = zhtmp2(i)
   end do
   irzh = iset

!     Setup The AZFLAG Array
   call sbyval(zftmp1,zftmp2,izf)
   iset = irzf
   do i = 1, izf
      iset = iset + 1
      if (iset > nrec) then
!           Error Msg: Maximum Number Of Receptor Exceeded
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NREC='',I7)') nrec
         call errhdl(path,modnam,'E','290',dummy)
         go to 999
      end if
      azflag(iset) = zftmp2(i)
   end do
   irzf = iset

   do i = idc1+1, irxr
      netid(i) = netidt
      rectyp(i) = 'GP'
   end do

999 return
end subroutine setpol

subroutine terhgt
!***********************************************************************
!                 TERHGT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Elevated Terrain Inputs for Receptor Network
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To trap on array subscript out-of-bounds
!                    when saving inputs to temporary arrays,
!                    which can occur if there's an input error
!                    in defining the receptor grid.
!                    R.W. Brode, PES, 4/2/99
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Elevated Terrain Input for a Receptor Network
!
!        CALLED FROM:   RECART
!                       REPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j
   double precision :: row

!     Variable Initializations
   modnam = 'TERHGT'

!     Check for the Location of the Secondary Keyword, ELEV
   do i = 1, ifc
      if (field(i) == 'ELEV') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','223',ktype)
      recerr = .true.
      go to 999
   else if (ifc == isc) then
!        Error Message: Missing Numerical Field
      call errhdl(path,modnam,'E','200',keywrd)
      recerr = .true.
      go to 999
   end if

   call stodbl(field(isc),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      recerr = .true.
   end if
   row = dnum

   iset = ize

   do i = isc+1, ifc
      call stodbl(field(i),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         call errhdl(path,modnam,'E','208',keywrd)
         recerr = .true.
      end if
      do j = 1, imit
         iset = iset + 1
         if (iset <= nrec) then
            zetmp1(iset) = row
            zetmp2(iset) = dnum
         end if
      end do
   end do

   ize = iset

999 return
end subroutine terhgt

subroutine hilhgt
!***********************************************************************
!                 HILHGT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Hill Height Scale Inputs for Receptor Network
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    May 31, 1995
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Hill Height Scale Input for a Receptor Network
!
!        CALLED FROM:   RECART
!                       REPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j
   double precision :: row

!     Variable Initializations
   modnam = 'HILHGT'

!     Check for the Location of the Secondary Keyword, ELEV
   do i = 1, ifc
      if (field(i) == 'HILL') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','223',ktype)
      recerr = .true.
      go to 999
   else if (ifc == isc) then
!        Error Message: Missing Numerical Field
      call errhdl(path,modnam,'E','200',keywrd)
      recerr = .true.
      go to 999
   end if

   call stodbl(field(isc),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      recerr = .true.
   end if
   row = dnum

   iset = izh

   do i = isc+1, ifc
      call stodbl(field(i),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         call errhdl(path,modnam,'E','208',keywrd)
         recerr = .true.
      end if
      do j = 1, imit
         iset = iset + 1
         if (iset <= nrec) then
            zhtmp1(iset) = row
            zhtmp2(iset) = dnum
         end if
      end do
   end do

   izh = iset

999 return
end subroutine hilhgt

subroutine flghgt
!***********************************************************************
!                 FLGHGT Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Flagpole Receptor Heights for Receptor Network
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:   To trap on array subscript out-of-bounds
!                    when saving inputs to temporary arrays,
!                    which can occur if there's an input error
!                    in defining the receptor grid.
!                    R.W. Brode, PES, 4/2/99
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Flagpole Receptor Heights for a Receptor Network
!
!        CALLED FROM:   RECART
!                       REPOLR
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, j
   double precision :: row

!     Variable Initializations
   modnam = 'FLGHGT'

!     Check for the Location of the Secondary Keyword, FLAG
   do i = 1, ifc
      if (field(i) == 'FLAG') then
         isc = i + 1
      end if
   end do

!     Determine Whether There Are Enough Parameter Fields
   if (ifc == isc-1) then
!        Error Message: Missing Parameter
      call errhdl(path,modnam,'E','223',ktype)
      recerr = .true.
      go to 999
   else if (ifc == isc) then
!        Error Message: Missing Numerical Field
      call errhdl(path,modnam,'E','200',keywrd)
      recerr = .true.
      go to 999
   end if

   call stodbl(field(isc),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
      recerr = .true.
   end if
   row = dnum

   iset = izf

   do i = isc+1, ifc
      call stodbl(field(i),ilen_fld,dnum,imit)
!        Check The Numerical Field
      if (imit == -1) then
         call errhdl(path,modnam,'E','208',keywrd)
         recerr = .true.
      end if
      do j = 1, imit
         iset = iset + 1
         if (iset <= nrec) then
            zftmp1(iset) = row
            zftmp2(iset) = dnum
         end if
      end do
   end do

   izf = iset

999 return
end subroutine flghgt

subroutine discar
!***********************************************************************
!                 DISCAR Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To include warning message for ELEV and .non.FLGPOL
!                   case, R.W. Brode, MACTEC/PES, 8/25/03
!
!        MODIFIED:  To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Discrete Cartesian Receptor Location Inputs
!
!        CALLED FROM:   RECARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i1, i2, i3, i4, i5

!     Variable Initializations
   modnam = 'DISCAR'
   i1 = irxr
   i2 = iryr
   i3 = irze
   i4 = irzf
   i5 = irzh
!     Determine Whether There Are Too Few Or Too Many Parameter Fields
   if (ifc < 4) then
!        WRITE Error Message: Missing Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc > 7) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   else if (elev .and. flgpol .and. ifc<7) then
!        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
      call errhdl(path,modnam,'W','228',keywrd)
   else if (elev .and. .not.flgpol .and. ifc < 6) then
!        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
      call errhdl(path,modnam,'W','228',keywrd)
   else if (elev .and. .not.flgpol .and. ifc > 6) then
!        WRITE Warning Message: Parameter Ignored, ZFLAG
      call errhdl(path,modnam,'W','229',keywrd)
   else if (flgpol .and. .not.elev .and. ifc > 5) then
!        WRITE Warning Message: Parameter Ignored, ZELEV & ZHILL
      call errhdl(path,modnam,'W','229',keywrd)
   else if (.not.elev .and. .not.flgpol .and. ifc > 4) then
!        WRITE Warning Message: Parameters Ignored, ZELEV ZHILL & ZFLAG
      call errhdl(path,modnam,'W','229',keywrd)
   end if

!     Check Whether The Maximum Number of Receptors is Exceeded
   if (i1==nrec .or. i2==nrec .or. i3==nrec .or.&
   &i4==nrec .or. i5==nrec) then
!        Error Msg: Maximum Number Of Receptors Exceeded
!        This shouldn't occur since limits are dynamically allocated
      write(dummy,'(''NREC='',I7)') nrec
      call errhdl(path,modnam,'E','290',dummy)
      go to 999
   end if

!     READ XCOORD,YCOORD,ELEV,ZHILL,FLAG And Assign Them to Different
!     Arrays

   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   else
      axr(i1 + 1) = dnum
   end if

   call stodbl(field(4),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   else
      ayr(i2 + 1) = dnum
   end if

   if (elev .and. flgpol) then
      if (ifc >= 5) then
         call stodbl(field(5),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azelev(i3 + 1) = dnum
         end if
      end if
      if (ifc >= 6) then
         call stodbl(field(6),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azhill(i5 + 1) = dnum
         end if
      end if
      if (ifc == 7) then
         call stodbl(field(7),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azflag(i4 + 1) = dnum
         end if
      end if
   else if (elev .and. .not.flgpol) then
      if (ifc >= 5) then
         call stodbl(field(5),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azelev(i3 + 1) = dnum
         end if
      end if
      if (ifc >= 6) then
         call stodbl(field(6),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azhill(i5 + 1) = dnum
         end if
      endif
   else if (flgpol .and. .not.elev) then
      if (ifc == 5) then
         call stodbl(field(5),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azflag(i4 + 1) = dnum
         end if
      else if (ifc == 7) then
         call stodbl(field(7),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azflag(i4 + 1) = dnum
         end if
      end if
   end if

   irxr = i1 + 1
   iryr = i2 + 1
   irze = i3 + 1
   irzf = i4 + 1
   irzh = i5 + 1
   netid(irxr) = ' '
   rectyp(irxr) = 'DC'
!     Reset ITAB Variable for TOXXFILE Option, 9/29/92
   itab = 0

999 return
end subroutine discar

subroutine dispol
!***********************************************************************
!                 DISPOL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Discrete Polar Receptor Location Inputs
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        MODIFIED:  To include warning message for ELEV and .non.FLGPOL
!                   case, R.W. Brode, MACTEC/PES, 8/25/03
!
!        MODIFIED:  To Include TOXXFILE Option - 9/29/92
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Discrete Polar Receptor Location Inputs
!
!        CALLED FROM:   RECARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i1, i2, i3, i4, i5, isdx
   double precision :: range, direct
   character (len=12) :: soid
   logical :: found

!     Variable Initializations
   modnam = 'DISPOL'
   i1 = irxr
   i2 = iryr
   i3 = irze
   i4 = irzf
   i5 = irzh

!     Determine Whether There Are Too Few Or Too Many Parameter Fields
   if (ifc < 5) then
!        WRITE Error Message: Missing Parameters
      call errhdl(path,modnam,'E','200',keywrd)
      go to 999
   else if (ifc > 8) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   else if (elev .and. flgpol .and. ifc<8) then
!        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
      call errhdl(path,modnam,'W','228',keywrd)
   else if (elev .and. .not.flgpol .and. ifc < 7) then
!        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
      call errhdl(path,modnam,'W','228',keywrd)
   else if (elev .and. .not.flgpol .and. ifc > 7) then
!        WRITE Warning Message: Parameter Ignored, ZFLAG
      call errhdl(path,modnam,'W','229',' ZFLAG ')
   else if (flgpol .and. .not.elev .and. ifc > 6) then
!        WRITE Warning Message: Parameter Ignored, ZELEV & ZHILL
      call errhdl(path,modnam,'W','229',keywrd)
   else if (.not.elev .and. .not.flgpol .and. ifc > 5) then
!        WRITE Warning Message: Parameters Ignored, ZELEV ZHILL & ZFLAG
      call errhdl(path,modnam,'W','229',keywrd)
   end if

!     Check Whether The Maximum Number of Receptors is Exceeded
   if (i1==nrec .or. i2==nrec .or. i3==nrec .or.&
   &i4==nrec .or. i5==nrec) then
!        Error Msg: Maximum Number Of Receptors Exceeded
!        This shouldn't occur since limits are dynamically allocated
      write(dummy,'(''NREC='',I7)') nrec
      call errhdl(path,modnam,'E','290',dummy)
      go to 999
   end if

!     READ SRCID,RANGE,DIRECT,ELEV,FLAG

!*    First check for length of SRCID field <=12
   if ((loce(3)-locb(3)) <= 11) then
!*       Retrieve Source ID Character Substring
      soid = field(3)
   else
!*       WRITE Error Message:  Source ID Field is Too Long
      call errhdl(path,modnam,'E','230',field(3)(1:12))
      recerr = .true.
      go to 999
   end if

   call stodbl(field(4),ilen_fld,range,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   end if

   call stodbl(field(5),ilen_fld,direct,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   else if (direct > 360.0d0) then
      direct = direct - 360.0d0
   else if (direct <= 0.0d0) then
      direct = direct + 360.0d0
   end if

   if (elev .and. flgpol) then
      if (ifc >= 6) then
         call stodbl(field(6),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azelev(i3 + 1) = dnum
         end if
      end if
      if (ifc >= 7) then
         call stodbl(field(7),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azhill(i5 + 1) = dnum
         end if
      end if
      if (ifc == 8) then
         call stodbl(field(8),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azflag(i4 + 1) = dnum
         end if
      end if
   else if (elev .and. .not.flgpol) then
      if (ifc >= 6) then
         call stodbl(field(6),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azelev(i3 + 1) = dnum
         end if
      end if
      if (ifc >= 7) then
         call stodbl(field(7),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azhill(i5 + 1) = dnum
         end if
      endif
   else if (flgpol .and. .not.elev) then
      if (ifc == 6) then
         call stodbl(field(6),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azflag(i4 + 1) = dnum
         end if
      else if (ifc == 8) then
         call stodbl(field(8),ilen_fld,dnum,imit)
!           Check The Numerical Field
         if (imit /= 1) then
            call errhdl(path,modnam,'E','208',keywrd)
         else
            azflag(i4 + 1) = dnum
         end if
      end if
   end if

!     Assign Them to Different Arrays,
!     Retrieve The Origin From Source Coordinates

   call sindex(srcid,nsrc,soid,isdx,found)
   if (.not. found) then
!        Error Message: Source ID Not Match
      call errhdl(path,modnam,'E','300',keywrd)
   else
      axr(i1 + 1) = axs(isdx) + range*dsin(direct*dtorad)
      ayr(i2 + 1) = ays(isdx) + range*dcos(direct*dtorad)
      irxr = i1 + 1
      iryr = i2 + 1
      irze = i3 + 1
      irzf = i4 + 1
      irzh = i5 + 1
!        Reset ITAB Variable for TOXXFILE Option, 9/29/92
      itab = 0
   end if

   netid(irxr)  = ' '
   rectyp(irxr) = 'DP'
   iref(irxr)   = isdx

999 return
end subroutine dispol

subroutine sbyval(arrin1,arrin2,inx)
!***********************************************************************
!                 SBYVAL Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Sort Array By Its 'Index Value'
!
!        PROGRAMMER: Jeff Wang, Roger Brode
!
!        DATE:    March 2, 1992
!
!        INPUTS:  ARRIN1: 'Index Array',  ARRIN2: 'Value Array'
!                 INX: Number of Values to Sort
!
!        OUTPUTS: Sorted Array
!
!        CALLED FROM: (This Is A Utility Program)
!***********************************************************************
!
!     Variable Declarations
   use main1
   implicit none
   character :: modnam*12

   integer :: i, inx, jc, imin
!     Declare Input Arrays as Assumed-Size Arrays (Currently Dimensioned NREC
!     in Calling Routines)
   double precision :: arrin1(*), arrin2(*), rmin, temp1, temp2

!     Variable Initialization
   modnam = 'SBYVAL'
   jc = 1

   do while (jc <= inx)
!        Find out The First Minimum In the Array
      rmin = arrin1(jc)
      imin = jc
      do i = jc, inx
         if (arrin1(i) < rmin) then
            imin = i
            rmin = arrin1(i)
         end if
      end do
!        Swap The Selected Array Elements
      temp1 = arrin1(jc)
      temp2 = arrin2(jc)
      arrin1(jc) = arrin1(imin)
      arrin2(jc) = arrin2(imin)
      arrin1(imin) = temp1
      arrin2(imin) = temp2
!        Increment The Counter
      jc = jc + 1
   end do

   return
end subroutine sbyval


subroutine evcart
!***********************************************************************
!                 EVCART Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
!                 for Use with the EVALFILE Option
!
!        PROGRAMMER: Roger Brode
!
!        DATE:    November 29, 1993
!
!        INPUTS:  Input Runstream Image Parameters
!
!        OUTPUTS: Discrete Cartesian Receptor Location Inputs
!                 With 'Arc' Grouping ID
!
!        CALLED FROM:   RECARD
!***********************************************************************

!     Variable Declarations
   use main1
   implicit none
   integer :: i1, i2, i3, i4, i5, j
   character :: modnam*12

   logical :: found

!     Variable Initializations
   modnam = 'EVCART'
   i1 = irxr
   i2 = iryr
   i3 = irze
   i4 = irzf
   i5 = irzh

!     Determine Whether There Are Too Few Or Too Many Parameter Fields
   if (ifc < 8) then
!        WRITE Error Message: Missing Parameters
      call errhdl(path,modnam,'E','201',keywrd)
      go to 999
   else if (ifc > 9) then
!        Error Message: Too Many Parameters
      call errhdl(path,modnam,'E','202',keywrd)
      go to 999
   end if

!     Check Whether The Maximum Number of Receptors is Exceeded
   if (i1==nrec .or. i2==nrec .or. i3==nrec .or.&
   &i4==nrec .or. i5==nrec) then
!        Error Msg: Maximum Number Of Receptors Exceeded
!        This shouldn't occur since limits are dynamically allocated
      write(dummy,'(''NREC='',I7)') nrec
      call errhdl(path,modnam,'E','290',dummy)
      go to 999
   end if

!     READ XCOORD,YCOORD,ELEV,HILLZ,FLAG And Assign Them to Different
!     Arrays

   call stodbl(field(3),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   else
      axr(i1 + 1) = dnum
   end if

   call stodbl(field(4),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   else
      ayr(i2 + 1) = dnum
   end if

   call stodbl(field(5),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   else
      azelev(i3 + 1) = dnum
   end if

   call stodbl(field(6),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   else
      azhill(i5 + 1) = dnum
   end if

   call stodbl(field(7),ilen_fld,dnum,imit)
!     Check The Numerical Field
   if (imit /= 1) then
      call errhdl(path,modnam,'E','208',keywrd)
   else
      azflag(i4 + 1) = dnum
   end if

!     Read ARCID Field, First Check for Previous Occurrence of This ARCID
   found = .false.
   j = 1
   do while (.not.found .and. j<=numarc)
      if (field(8) == arcid(j)) then
         found = .true.
         ndxarc(i1 + 1) = j
      end if
      j = j + 1
   end do
   if (.not. found) then
      numarc = numarc + 1
      if (numarc > narc) then
!           Write Error Message:  Too Many ARCs
!           This shouldn't occur since limits are dynamically allocated
         write(dummy,'(''NARC ='',I6)') narc
         call errhdl(path,modnam,'E','290',dummy)
         go to 999
      else
         arcid(numarc)  = field(8)
         ndxarc(i1 + 1) = numarc
      end if
   end if

   irxr = i1 + 1
   iryr = i2 + 1
   irze = i3 + 1
   irzf = i4 + 1
   irzh = i5 + 1
   netid(irxr) = ' '
   rectyp(irxr) = 'DC'
!     Reset ITAB Variable for TOXXFILE Option, 9/29/92
   itab = 0

999 return
end subroutine evcart

subroutine blrecp(kk)
!***********************************************************************
!                 BLRECP Module of the AMS/EPA Regulatory Model - AERMOD
!
!        PURPOSE: Performs a translation and initial rotation of the
!                 receptors when buoyant line sources are processed
!
!        PROGRAMMER: Amec Foster Wheeler
!
!        DATE:    March 31, 2016
!
!        MODIFIED: Wood, December 2019 (Multiple_BuoyLines_D41_Wood)
!                  Many modifications to process receptors by buoyant
!                  line source group; argument KK identifies which BL
!                  group is being processed
!
!        INPUTS:  Receptor locations, translation and rotation parameters
!                 KK = Buoyant line source group number
!
!        OUTPUTS: Translated and rotated receptors with flag set
!                 indicating if a receptor is inside or outside the
!                 rectangular footprint defined by a buoyant line group
!
!        CALLED FROM:   RECARD
!***********************************************************************

! ---    If there is one or more buoyant line sources, the receptors
!          need to be translated and rotated so they are oriented the
!          same as the translation and rotation for each buoyant line
!          source - use the buoyant line source endpoints to determine
!          translation and rotation
!          (NOTE: this is an intial rotation based on the orientation of
!           buoyant line source relative to the x-y axes (with +y
!           pointing north); the receptors will be rotated again each
!           hour based on the wind direction)
!
!           XOR, YOR, COSTCOR, and SINTCOR are calculated in BL_ROTATE1
!            when the sources are processed; these parameters are
!            declared in the BUOYANT_LINE module
!
!           AERMOD does NOT allow the RE pathway to be processed before
!            the SO pathway so the necessary parameters for the rotation
!            are already defined.
!
!        BL_RFLAG indicates if a receptor is inside (.true.) or outside
!         (.false.) of the rectangle defined by the minimum and maximum
!         extents of the translation/first rotation of the buoyant line
!
!     Variable Declarations
   use main1
   use buoyant_line
   implicit none

! JAT 06/22/21 D065
! REMOVE NVRECT AS UNUSED VARIABLE
   integer :: i, kk, nrecin, lnum, nr !, NVRECT                         ! Multiple_BuoyLines_D41_Wood
   double precision :: ex,ey, xlmin, xlmax, ylmin, ylmax
!      DOUBLE PRECISION :: BLREC_X(4), BLREC_Y(4)                        ! Multiple_BuoyLines_D41_Wood
   character :: modnam*12
! Unused: INTEGER :: ILSAVE

!     Variable Initializations
   modnam = 'BLRECEP'
   nrecin = 0
! D41_Wood      BL_RFLAG = .false.

! --- Translate and rotate receptors for buoyant line source
   do i = 1,numrec
!        Translate
      xr_scs(i,kk) = axr(i) - xor(kk)
      yr_scs(i,kk) = ayr(i) - yor(kk)
      ex = xr_scs(i,kk)
      ey = yr_scs(i,kk)

!        Initial rotation
      ey = -ex*sintcor(kk) + ey*costcor(kk)
      yr_scs(i,kk) = ey
      ex = (ex + ey*sintcor(kk))/costcor(kk)
      xr_scs(i,kk) = ex
   end do

!     Determine the min, max extents of the bouyant line source
!      after translation and first rotation by BL group.
!      These are used to determine if a receptor is inside the
!      'footprint' of the buoyant lines, the so-called exclusion zone
!      for the BL source, and are excluded from the calculations by
!      setting a flag for each receptor
!      The first loop establishes the initial minimum and maximum values
!      for X and Y by finding the smallest X, Y for the maximum and the
!      largest X, Y for the minimum.  The second loop then compares each
!      lines beginning and end points to XLMIN, XLMAX, YLAMIN, YLMAX.

! Multiple_BuoyLines_D41_Wood
!     Determine the exclusion zone by BL group
   do lnum = 1,nbltotal
      if (blineparms(lnum)%iblpgrpnum == kk) then
         ylmax = ys_scs(lnum)
         xlmax = blineparms(lnum)%xend_tr1
         xlmin = blineparms(lnum)%xbeg_tr1
         exit
      end if
   end do

   do lnum = nbltotal,1,-1
      if (blineparms(lnum)%iblpgrpnum == kk) then
         ylmin = ys_scs(lnum)
         exit
      end if
   end do

   do lnum = 1,nbltotal
      if (blineparms(lnum)%iblpgrpnum == kk) then
         xlmin = min(xlmin,blineparms(lnum)%xbeg_tr1)
         xlmax = max(xlmax,blineparms(lnum)%xend_tr1)
         ylmin = min(ylmin,ys_scs(lnum))
         ylmax = max(ylmax,ys_scs(lnum))
      end if
   end do

!     Define the vertices of the rectangular footprint defined by a
!     buoyant line source that defines the exclusion zone -
!     coordinates are specified counterclockwise

! Multiple_BuoyLines_D41_Wood
!     The following are never used - commented out for now
!      NVRECT = 4
!      BLREC_X(1) = XLMIN
!      BLREC_X(2) = XLMAX
!      BLREC_X(3) = XLMAX
!      BLREC_X(4) = XLMIN
!      BLREC_Y(1) = YLMIN
!      BLREC_Y(2) = YLMIN
!      BLREC_Y(3) = YLMAX
!      BLREC_Y(4) = YLMAX

!     Set the flag buoyant line source flag to TRUE if the receptor
!      is inside the rectangular footprint defined by a buoyant line
!      source (exclusion zone)
!
! Multiple_BuoyLines_D41_Wood
!     Set the receptor flag by BL group
   do nr = 1,numrec
      if( yr_scs(nr,kk) <= (ylmax + 0.1d0) .and.&
      &yr_scs(nr,kk) >= (ylmin - 0.1d0) .and.&
      &xr_scs(nr,kk) <= (xlmax + 0.1d0) .and.&
      &xr_scs(nr,kk) >= (xlmin - 0.1d0)) then
         nrecin = nrecin + 1
         bl_rflag(nr,kk) = .true.
      end if
   end do

   write(dummy,'(I3," (",A6,")")') nrecin, bl_grpid(kk)
   call errhdl(path,modnam,'W','476',dummy)

   return
end subroutine blrecp
