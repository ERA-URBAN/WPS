   program geogrid_clc

   implicit none

   integer :: i,k,j
   integer :: max_time
   integer :: ounit
   integer :: version            ! Format version (must =5 for WPS format)
   integer :: nx, ny             ! x- and y-dimensions of 2-d array
   real :: startlat, startlon    ! Lat/lon of point in array indicated by 
                                 ! startloc string
   real :: cellsize
   real :: deltalat, deltalon    ! Grid spacing, degrees
   real :: missvalue

   real,allocatable :: rarray(:,:), iarray(:,:)

   integer :: iproj              ! Code for projection of data in array:
                                 !     0 = cylindrical equidistant
                                 !     1 = Mercator
                                 !     3 = Lambert conformal conic
                                 !     4 = Gaussian
                                 !     5 = Polar stereographic


   real :: xfcst                 ! Forecast hour of data
   real :: xlvl                  ! Vertical level of data in 2-d array

   character :: head12

   logical :: is_wind_grid_rel   ! Flag indicating whether winds are                                     
                                 !    relative to source grid (TRUE) or 
                                 !    relative to earth (FALSE)

   character (len=8)  :: startloc      ! Which point in array is given by 
                                       ! startlat/startlon; set either                                     
                                       ! to 'SWCORNER' or 'CENTER  '

   character (len=9)  :: field   ! Name of the field

   character (len=24),allocatable :: hdate(:) ! Valid date for data YYYY:MM:DD_HH:00:00

   character (len=25) :: units                           ! Units of data
   character (len=32) :: map_source                      ! Source model / originating center
   character (len=46) :: desc                            ! Short description of data
   character (len=35),allocatable :: filenamein(:)
   character (len=17),allocatable :: filenameout(:)
   character (len=24) :: hdatein

   real :: earth_radius

   ounit = 11        
   version = 5
   iproj = 0 
   xfcst = 0.0
   xlvl = 200100.0 
   is_wind_grid_rel = .TRUE.
   startloc = "SWCORNER"
   field = "ANTHEAT2D"
   units = "W/m2"
   map_source = "LUCY 3.1"
   desc = "Anthropogenic heat"
   earth_radius = 6370.

        
   open(10,file='list_ANT',status='old')
   read(10,*)max_time 
   allocate(filenamein(max_time))
   allocate(filenameout(max_time))
   allocate(hdate(max_time))
   do i=1,max_time
     read(10,'(A19,1x,A35)')hdatein,filenamein(i)
     hdate(i) = hdatein 
     write(filenameout(i),'(A4,A4,A1,A2,A1,A2,A1,A2)')'ANT:',hdatein(1:4),'-',hdatein(6:7),'-',hdatein(9:10),'_',hdatein(12:13)
   end do 
   close(10)

   ! Loop over all files   
   do k=1,max_time 

       open(10,file=filenamein(k),status='old')

       read(10,*) head12, nx
       read(10,*) head12, ny
       read(10,*) head12, startlon
       read(10,*) head12, startlat
       read(10,*) head12, cellsize
       read(10,*) head12, missvalue
            
       deltalat = cellsize
       deltalon = cellsize

       allocate(rarray(nx,ny))
       allocate(iarray(nx,ny)) 

       ! read in the data
       do j = 1,ny
          read(10,*)iarray(:,j)
       end do

       close(10)

       ! reverse the data so that it begins at the lower-left corner
       do j = 1,ny
          rarray(:,j) = iarray(:,ny-(j-1))
       enddo

       do i=1,nx
         do j=1,ny
           if (rarray(i,j) <  0.) then
              rarray(i,j) = 0.
           end if
         enddo
       enddo       


       open(unit=ounit,file=filenameout(k),form='unformatted')

       !  1) WRITE FORMAT VERSION
       write(unit=ounit) version

       !  2) WRITE METADATA

       ! Cylindrical equidistant
       write(unit=ounit) hdate(k), xfcst, map_source, field, &
                         units, desc, xlvl, nx, ny, iproj

       write(unit=ounit) startloc, startlat, startlon, &
                         deltalat, deltalon, earth_radius

       !  3) WRITE WIND ROTATION FLAG 
       write(unit=ounit) is_wind_grid_rel

       !  4) WRITE 2-D ARRAY OF DATA
       write(unit=ounit) rarray

       ! Finish
       close(unit=ounit)
       deallocate(rarray)
       deallocate(iarray) 

   end do

   end program

