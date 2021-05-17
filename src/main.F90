program getinten
 use arrays
 implicit none
#ifdef MPI
       include 'mpif.h'

#endif
       integer nx, nz
       parameter (nx = 512, nz = 512)
 
       integer ncid, ier 
       character*256  filename 
       character*128 folder, entfolder 
       character*6 filenumber, outfilnum 
       character*50 numberx, testn
       character*80 ffile

       integer i,j,k,m, p
       integer dnx, nxstart, nxfin

       integer myrank, sizee, comm 
#ifdef MPI
       integer ierr,  error
       integer status
       integer nlayer
       real(kind=8) time1, time2

!------ initiate MPI
!       Initialize MPI.
!

        call MPI_Init(ierr)
        comm = MPI_COMM_WORLD
!
!  Determine this process's ID.
!
        call MPI_Comm_rank( comm, myrank, ierr )
!  Find out how many processes are active.
!
        call MPI_Comm_size( comm, sizee,  ierr )
        time1 = MPI_WTIME()
        
       if (mod(nx,sizee) .ne. 0)  then
         print*, 'nx can not be devided without remainers'
         call mpi_abort(comm, ierr)
       endif


       dnx = nx/sizee
       nxstart = myrank*dnx+1
       nxfin = (myrank+1)*dnx
       call set_arrays(dnx, nz, 1221)
        
#else
       myrank = 0
       sizee = 1
       nxstart =1 
       nxfin = nx
       call set_arrays(nx, nz, 1221)
#endif


! -------- initialize

       read(*,*) folder
       read(*,*) filenumber
!       folder="/scratch/bhasari/Final_atlas_results_asplund/MR/D000/mu=010/D000_1D_SLICE_snap_"    
!       filenumber='123000'

!       entfolder=trim(folder)//trim(filenumber)
       entfolder=trim(folder)
       outfilnum=trim(filenumber)

! get the right files and all folders

       do i = nxstart, nxfin  
        call str(int(i), numberx)
        filename=trim(entfolder)//'/7.'//trim(numberx)

        open(unit=2, status='old', file=filename, form='formatted')

        do j = 1, nz 
           do k = 1, 1221 
            read(2, *) buffarr(i-nxstart+1,j,k)
           end do 

        end do 
        close(unit=2)


       end do 
       read(*,*) outfilnum
!        outfilnum='123000'
       ffile = 'result_Int.'//trim(outfilnum)//'.nc'
       if (myrank .eq. 0) then 
        call  create_netcdf(ncid, ffile, nx, nz, 1221, ier)
       endif 
#ifdef MPI
        call MPI_Bcast(ncid, 1, MPI_integer, 0 , comm, ier ) 
#endif 
        call  write_netcdf(ncid, myrank, sizee, 'Intensity', buffarr, dnx,  nx, nz, 1221, comm,  ier)
       if (myrank .eq. 0 ) then 
        call  close_netcdf(ncid, ier)
       endif 

end
       
!----------------------------------------------------------------------------
! writing numbers to strings!
subroutine str(i, s)
      implicit none
      character*50, intent(out) :: s
      integer, intent(in) :: i

      write (s, *) i
      s = adjustl(s)
      s = trim(s)
end subroutine
!
