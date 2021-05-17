Module arrays

 implicit none
 integer nnew
 parameter (nnew = 1221) 
 integer n3 
 parameter (n3 = 1071 )

 real(kind=8), allocatable :: buffarr(:,:,:)
 real(kind=4), allocatable :: Inten(:,:,:)
! real(kind=4), allocatable :: Inten2(:,:,:) 
! real(kind=4), allocatable :: Inten3(:,:,:)

 CONTAINS

 subroutine set_arrays(nx, nz, nnew) 

 implicit none
 integer nx, nz, nnew


 allocate(buffarr(nx,nz,nnew))
 allocate(Inten(nx, nz, n3))
! allocate(Inten2(nx, nz, n3))
! allocate(Inten3(nx, nz, n3))

 buffarr  = 0.0d0
 Inten = 0.0  
! Inten2 = 0.0
! Inten3 = 0.0

 end subroutine set_arrays 


 subroutine close_arrays 
  implicit none

  deallocate(buffarr)
  deallocate(Inten) 
!  deallocate(Inten2)
!  deallocate(Inten3) 

 end subroutine close_arrays 

 end module arrays

