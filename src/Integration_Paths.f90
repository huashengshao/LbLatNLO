MODULE Integration_Paths
  INTEGER::ipath=1
CONTAINS
  ! set the paths in xsb and xtb
  SUBROUTINE SetPath_st(xsb1,xsb0,xtb1,xtb0)
    IMPLICIT NONE
    double precision xsb1,xtb1,xsb0,xtb0
    double precision xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st/xsbv1,xsbv0,xtbv1,xtbv0
    xsbv1=xsb1
    xtbv1=xtb1
    xsbv0=xsb0
    xtbv0=xtb0
    RETURN
  END SUBROUTINE SetPath_st

  SUBROUTINE GetPath_Euclid_st(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    double precision t
    double precision xsb,xtb,dxsbdt,dxtbdt
    double precision xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xsbv0)+& 
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xsbv1
       xtb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xtbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xtbv1
       dxsbdt=630d0*(1d0-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630d0*(1d0-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xsbv0-& 
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xsbv1
       xtb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xtbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xtbv1
       dxsbdt=140d0*(1d0-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140d0*(1d0-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1d0+t)**2*(1d0+2d0*t)*xsbv0-t**2*(-3d0+2d0*t)*xsbv1
       xtb=(-1d0+t)**2*(1d0+2d0*t)*xtbv0-t**2*(-3d0+2d0*t)*xtbv1
       dxsbdt=6d0*(1d0-t)*t*(xsbv1-xsbv0)
       dxtbdt=6d0*(1d0-t)*t*(xtbv1-xtbv0)
    endif

!    xsb=xsbv1*t+xsbv0*(1d0-t)
!    xtb=xtbv1*t+xtbv0*(1d0-t)
!    dxsbdt=xsbv1-xsbv0
!    dxtbdt=xtbv1-xtbv0
    RETURN
  END SUBROUTINE GetPath_Euclid_st

  SUBROUTINE GetPath_Phys1_st(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    double precision t
    double precision xsb,xtb,dxsbdt,dxtbdt
    double precision xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xsbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xsbv1
       xtb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xtbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xtbv1
       dxsbdt=630d0*(1d0-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630d0*(1d0-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xsbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xsbv1
       xtb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xtbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xtbv1
       dxsbdt=140d0*(1d0-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140d0*(1d0-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1d0+t)**2*(1d0+2d0*t)*xsbv0-t**2*(-3d0+2d0*t)*xsbv1
       xtb=(-1d0+t)**2*(1d0+2d0*t)*xtbv0-t**2*(-3d0+2d0*t)*xtbv1
       dxsbdt=6d0*(1d0-t)*t*(xsbv1-xsbv0)
       dxtbdt=6d0*(1d0-t)*t*(xtbv1-xtbv0)
    endif
    RETURN
  END SUBROUTINE GetPath_Phys1_st

  SUBROUTINE GetPath_Phys2_st(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    double precision t
    double precision xsb,xtb,dxsbdt,dxtbdt
    double precision xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xsbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xsbv1
       xtb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xtbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xtbv1
       dxsbdt=630d0*(1d0-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630d0*(1d0-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xsbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xsbv1
       xtb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xtbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xtbv1
       dxsbdt=140d0*(1d0-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140d0*(1d0-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1d0+t)**2*(1d0+2d0*t)*xsbv0-t**2*(-3d0+2d0*t)*xsbv1
       xtb=(-1d0+t)**2*(1d0+2d0*t)*xtbv0-t**2*(-3d0+2d0*t)*xtbv1
       dxsbdt=6d0*(1d0-t)*t*(xsbv1-xsbv0)
       dxtbdt=6d0*(1d0-t)*t*(xtbv1-xtbv0)
    endif
    RETURN
  END SUBROUTINE GetPath_Phys2_st

  SUBROUTINE GetPath_Phys3_st(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    double precision t
    double precision xsb,xtb,dxsbdt,dxtbdt
    double precision xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xsbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xsbv1
       xtb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xtbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xtbv1
       dxsbdt=630d0*(1d0-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630d0*(1d0-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xsbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xsbv1
       xtb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xtbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xtbv1
       dxsbdt=140d0*(1d0-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140d0*(1d0-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1d0+t)**2*(1d0+2d0*t)*xsbv0-t**2*(-3d0+2d0*t)*xsbv1
       xtb=(-1d0+t)**2*(1d0+2d0*t)*xtbv0-t**2*(-3d0+2d0*t)*xtbv1
       dxsbdt=6d0*(1d0-t)*t*(xsbv1-xsbv0)
       dxtbdt=6d0*(1d0-t)*t*(xtbv1-xtbv0)
    endif
    RETURN
  END SUBROUTINE GetPath_Phys3_st

  SUBROUTINE GetPath_Phys4_st(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    double precision t
    double precision xsb,xtb,dxsbdt,dxtbdt
    double precision xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xsbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xsbv1
       xtb=-((-1d0+t)**5*(1d0+5d0*t+15d0*t**2+35d0*t**3+70d0*t**4)*xtbv0)+&
            t**5*(126d0-420d0*t+540d0*t**2-315d0*t**3+70d0*t**4)*xtbv1
       dxsbdt=630d0*(1d0-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630d0*(1d0-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xsbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xsbv1
       xtb=(-1d0+t)**4*(1d0+4d0*t+10d0*t**2+20d0*t**3)*xtbv0-&
            t**4*(-35d0+84d0*t-70d0*t**2+20d0*t**3)*xtbv1
       dxsbdt=140d0*(1d0-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140d0*(1d0-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1d0+t)**2*(1d0+2d0*t)*xsbv0-t**2*(-3d0+2d0*t)*xsbv1
       xtb=(-1d0+t)**2*(1d0+2d0*t)*xtbv0-t**2*(-3d0+2d0*t)*xtbv1
       dxsbdt=6d0*(1d0-t)*t*(xsbv1-xsbv0)
       dxtbdt=6d0*(1d0-t)*t*(xtbv1-xtbv0)
    endif
    RETURN
  END SUBROUTINE GetPath_Phys4_st

  ! set the paths in xsb and xtb
  SUBROUTINE SetPath_st_qp(xsb1,xsb0,xtb1,xtb0)
    IMPLICIT NONE
    real*16 xsb1,xtb1,xsb0,xtb0
    real*16 xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st_qp/xsbv1,xsbv0,xtbv1,xtbv0
    xsbv1=xsb1
    xtbv1=xtb1
    xsbv0=xsb0
    xtbv0=xtb0
    RETURN
  END SUBROUTINE SetPath_st_qp

  SUBROUTINE GetPath_Euclid_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    real*16 t
    real*16 xsb,xtb,dxsbdt,dxtbdt
    real*16 xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st_qp/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2&
            +35.0E0_16*t**3+70.0E0_16*t**4)*xsbv0)+& 
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2&
            -315.0E0_16*t**3+70.0E0_16*t**4)*xsbv1
       xtb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2&
            +35.0E0_16*t**3+70.0E0_16*t**4)*xtbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2&
            -315.0E0_16*t**3+70.0E0_16*t**4)*xtbv1
       dxsbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xsbv0-& 
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xsbv1
       xtb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xtbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xtbv1
       dxsbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xsbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xsbv1
       xtb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xtbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xtbv1
       dxsbdt=6.0E0_16*(1.0E0_16-t)*t*(xsbv1-xsbv0)
       dxtbdt=6.0E0_16*(1.0E0_16-t)*t*(xtbv1-xtbv0)
    endif

    RETURN
  END SUBROUTINE GetPath_Euclid_st_qp
  
  SUBROUTINE GetPath_Phys1_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    real*16 t
    real*16 xsb,xtb,dxsbdt,dxtbdt
    real*16 xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st_qp/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2+35.0E0_16*t**3+70.0E0_16*t**4)*xsbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2-315.0E0_16*t**3+70.0E0_16*t**4)*xsbv1
       xtb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2+35.0E0_16*t**3+70.0E0_16*t**4)*xtbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2-315.0E0_16*t**3+70.0E0_16*t**4)*xtbv1
       dxsbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xsbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xsbv1
       xtb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xtbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xtbv1
       dxsbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xsbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xsbv1
       xtb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xtbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xtbv1
       dxsbdt=6.0E0_16*(1.0E0_16-t)*t*(xsbv1-xsbv0)
       dxtbdt=6.0E0_16*(1.0E0_16-t)*t*(xtbv1-xtbv0)
    endif
    RETURN
  END SUBROUTINE GetPath_Phys1_st_qp

  SUBROUTINE GetPath_Phys2_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    real*16 t
    real*16 xsb,xtb,dxsbdt,dxtbdt
    real*16 xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st_qp/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2+35.0E0_16*t**3+70.0E0_16*t**4)*xsbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2-315.0E0_16*t**3+70.0E0_16*t**4)*xsbv1
       xtb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2+35.0E0_16*t**3+70.0E0_16*t**4)*xtbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2-315.0E0_16*t**3+70.0E0_16*t**4)*xtbv1
       dxsbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xsbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xsbv1
       xtb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xtbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xtbv1
       dxsbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xsbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xsbv1
       xtb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xtbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xtbv1
       dxsbdt=6.0E0_16*(1.0E0_16-t)*t*(xsbv1-xsbv0)
       dxtbdt=6.0E0_16*(1.0E0_16-t)*t*(xtbv1-xtbv0)
    endif
    RETURN
  END SUBROUTINE GetPath_Phys2_st_qp

  SUBROUTINE GetPath_Phys3_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    real*16 t
    real*16 xsb,xtb,dxsbdt,dxtbdt
    real*16 xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st_qp/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2+35.0E0_16*t**3+70.0E0_16*t**4)*xsbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2-315.0E0_16*t**3+70.0E0_16*t**4)*xsbv1
       xtb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2+35.0E0_16*t**3+70.0E0_16*t**4)*xtbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2-315.0E0_16*t**3+70.0E0_16*t**4)*xtbv1
       dxsbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xsbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xsbv1
       xtb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xtbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xtbv1
       dxsbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xsbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xsbv1
       xtb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xtbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xtbv1
       dxsbdt=6.0E0_16*(1.0E0_16-t)*t*(xsbv1-xsbv0)
       dxtbdt=6.0E0_16*(1.0E0_16-t)*t*(xtbv1-xtbv0)
    endif
    RETURN
  END SUBROUTINE GetPath_Phys3_st_qp

  SUBROUTINE GetPath_Phys4_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
    IMPLICIT NONE
    real*16 t
    real*16 xsb,xtb,dxsbdt,dxtbdt
    real*16 xsbv1,xsbv0,xtbv1,xtbv0
    common/path_Euc_st_qp/xsbv1,xsbv0,xtbv1,xtbv0
    if(ipath.eq.1)then
       xsb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2+35.0E0_16*t**3+70.0E0_16*t**4)*xsbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2-315.0E0_16*t**3+70.0E0_16*t**4)*xsbv1
       xtb=-((-1.0E0_16+t)**5*(1.0E0_16+5.0E0_16*t+15.0E0_16*t**2+35.0E0_16*t**3+70.0E0_16*t**4)*xtbv0)+&
            t**5*(126.0E0_16-420.0E0_16*t+540.0E0_16*t**2-315.0E0_16*t**3+70.0E0_16*t**4)*xtbv1
       dxsbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xsbv1-xsbv0)
       dxtbdt=630.0E0_16*(1.0E0_16-t)**4*t**4*(xtbv1-xtbv0)
    elseif(ipath.eq.2)then
       xsb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xsbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xsbv1
       xtb=(-1.0E0_16+t)**4*(1.0E0_16+4.0E0_16*t+10.0E0_16*t**2+20.0E0_16*t**3)*xtbv0-&
            t**4*(-35.0E0_16+84.0E0_16*t-70.0E0_16*t**2+20.0E0_16*t**3)*xtbv1
       dxsbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xsbv1-xsbv0)
       dxtbdt=140.0E0_16*(1.0E0_16-t)**3*t**3*(xtbv1-xtbv0)
    else
       xsb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xsbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xsbv1
       xtb=(-1.0E0_16+t)**2*(1.0E0_16+2.0E0_16*t)*xtbv0-t**2*(-3.0E0_16+2.0E0_16*t)*xtbv1
       dxsbdt=6.0E0_16*(1.0E0_16-t)*t*(xsbv1-xsbv0)
       dxtbdt=6.0E0_16*(1.0E0_16-t)*t*(xtbv1-xtbv0)
    endif
    RETURN
  END SUBROUTINE GetPath_Phys4_st_qp
END MODULE Integration_Paths
