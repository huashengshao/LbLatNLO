MODULE handyG
  ! this is a dummy file for the handyG (quadruple precision)
  ! which will only be used when there is no handyG found
  ! it will not be used in fact (just for the compilation purpose)
  IMPLICIT NONE
  type inum
     complex(kind(1E0_16)) :: c
     integer(1) :: i0
  end type inum
  interface toinum
     module procedure toinum_cmplxs, toinum_cmplx, toinum_real, toinum_reals, toinum_int
  end interface toinum
  real(kind(1E0_16)),parameter::zero = 10E0_16**(-precision(1E0_16))  ! values smaller than this count as zero
  integer(1), parameter :: di0 = +1
CONTAINS
  SUBROUTINE clearcache
    RETURN
  END SUBROUTINE clearcache

  FUNCTION TOINUM_cmplxs(z, s)
    complex(kind(1E0_16)) :: z
    type(inum) :: toinum_cmplxs
    integer(1),optional :: s
    integer(1) :: ss
    if (present(s)) then
       ss = s
    else
       ss = di0
    endif
    toinum_cmplxs = inum(z, ss)
    if (abs(imagpart(z))>zero) then
       toinum_cmplxs%i0 = int(sign(1E0_16, imagpart(z)),1)
    endif
  END FUNCTION TOINUM_cmplxs

  FUNCTION TOINUM_cmplx(z, s)
    complex(kind(1E0_16)) :: z(:)
    type(inum) :: toinum_cmplx(size(z))
    integer(1),optional :: s
    integer(1) :: ss
    integer i
    if (present(s)) then
       ss = s
    else
       ss = di0
    endif
    do i=1,size(z)
       toinum_cmplx(i) = inum(z(i), ss)
       if (abs(imagpart(z(i)))>zero) then
          toinum_cmplx(i)%i0 = int(sign(1E0_16, imagpart(z(i))),1)
       endif
    enddo
  END FUNCTION TOINUM_cmplx

  FUNCTION TOINUM_real(z, s)
    real(kind(1E0_16)) :: z(:)
    type(inum) :: toinum_real(size(z))
    integer(1),optional :: s
    integer(1) :: ss
    integer i
    if (present(s)) then
       ss = s
    else
       ss = di0
    endif
    do i=1,size(z)
       toinum_real(i) = inum(z(i), ss)
    enddo
  END FUNCTION TOINUM_real

  FUNCTION TOINUM_int(z, s)
    integer :: z(:)
    type(inum) :: toinum_int(size(z))
    integer(1),optional :: s
    integer(1) :: ss
    integer i
    if (present(s)) then
       ss = s
    else
       ss = di0
    endif
    do i=1,size(z)
       toinum_int(i) = inum(z(i), ss)
    enddo
  END FUNCTION TOINUM_int

  FUNCTION TOINUM_reals(z, s)
    real(kind(1E0_16)) :: z
    type(inum) :: toinum_reals
    integer(1),optional :: s
    integer(1) :: ss
    if (present(s)) then
       ss = s
    else
       ss = di0
    endif
    toinum_reals = inum(z, ss)
  END FUNCTION TOINUM_reals

  FUNCTION G(z_flat,y) result(res)
    type(inum) :: z_flat(:), y
    complex(kind(1E0_16))::res
    res=cmplx(0E0_16,0E0_16,kind=16)
    return
  END FUNCTION G
END MODULE handyG
