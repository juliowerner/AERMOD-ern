subroutine uninam( prenam, defnam, comnam, lcom )
!
!  Combine prenam and defnam to form a unique combined name, comnam.
!  If prenam_defnam already exists, add a date extension to prenam.
!  If necessary, expand the date to include seconds and milliseconds.
!
   character(len=*) :: prenam, defnam, comnam
   integer :: lpre, ldef, lcom, i
   logical :: jexist
   character(len=8) :: dfield
   character(len=10) :: tfield
   integer, parameter  :: nmax=1000000
!
   lpre = len( prenam )
   ldef = len( defnam )
   if( lpre == 0 ) then
      comnam = defnam
      lcom = ldef
      return
   end if
   comnam = prenam // '_' // defnam
   lcom = lpre + 1 + ldef
   inquire( file=comnam, exist=jexist )
   if( jexist ) then
      call date_and_time( dfield, tfield )
      comnam = prenam // '_' // dfield // '_' // defnam
      lcom = lcom + 9
      inquire( file=comnam, exist=jexist )
      if( jexist ) then
         comnam = prenam // '_' // dfield // tfield(1:6) // '_'&
         &// defnam
         lcom = lcom + 6
         inquire( file=comnam, exist=jexist )
         if( jexist ) then
            comnam = prenam // '_' // dfield // tfield // '_'&
            &// defnam
            lcom = lcom + 4
            i = 1
            inquire( file=comnam, exist=jexist )
            do while( jexist .and. i < nmax )
               call date_and_time( dfield, tfield )
               comnam = prenam // '_' // dfield // tfield // '_'&
               &// defnam
               i = i + 1
               inquire( file=comnam, exist=jexist )
            end do
         end if
      end if
   end if
   return
end subroutine uninam
