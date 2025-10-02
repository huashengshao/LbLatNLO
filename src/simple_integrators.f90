MODULE simple_integrators
  IMPLICIT NONE
  PRIVATE
  PUBLIC::trapezoid_integrator,simpson_integrator
  PUBLIC::trapezoid_Cintegrator,simpson_Cintegrator
  PUBLIC::trapezoid_Cintegrator_qp
  ! double-exponential Quadrature for one-dim integrations
  PUBLIC::DEQuadrature_integrator_ini,DEQuadrature_integrator
  PUBLIC::DEQuadrature_Cintegrator
  PUBLIC::DEQuadrature_integrator_ini_qp ! quadruple precision
  PUBLIC::DEQuadrature_Cintegrator_qp    ! quadruple precision
  PUBLIC::DEQuadrature_integrator_inf_nonosci_ini
  PUBLIC::DEQuadrature_integrator_inf_nonosci
  PUBLIC::DEQuadrature_Cintegrator_inf_nonosci
  PUBLIC::DEQuadrature_integrator_inf_osci_ini
  PUBLIC::DEQuadrature_integrator_inf_osci
  PUBLIC::DEQuadrature_Cintegrator_inf_osci
  ! end of double-exponential Quadrature for one-dim integrations
CONTAINS
  !-------------------------------------------------------------
  ! The following are simple one-dimensional integrations
  !-------------------------------------------------------------

  SUBROUTINE trapezoid_integrator(n,fxn,end_val,res)
    !===========================================================
    ! Integration of fxn(x) on [0,end_val]
    !-----------------------------------------------------------
    ! IN:
    ! n - number of intervals
    ! fxn - Function to be integrate
    ! end_val - Upper limit of integration
    ! OUT:
    ! res - Result of integration
    !============================================================
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(OUT)::res
    REAL(KIND(1d0)),EXTERNAL::fxn
    INTEGER,INTENT(IN)::n ! it is total number of intervals in the x
    REAL(KIND(1d0)),INTENT(IN)::end_val ! the upper value of integration
    REAL(KIND(1d0))::u,h
    INTEGER::i
    res=0d0
    DO i=0,n
       u=(end_val*i)/n
       IF(i.EQ.0.OR.i.EQ.n)THEN
          res=res+fxn(u)
       ELSE
          res=res+2d0*fxn(u)
       ENDIF
    ENDDO
    h=end_val/n
    res=(h/2d0)*res
    RETURN
  END SUBROUTINE trapezoid_integrator

  SUBROUTINE trapezoid_Cintegrator(n,fxn,end_val,res)
    !===========================================================
    ! Integration of the complex function fxn(x) on [0,end_val]
    !-----------------------------------------------------------
    ! IN:
    ! n - number of intervals
    ! fxn - Function to be integrate (double complex)
    !     - the argument of fxn should be real
    ! end_val - Upper limit of integration
    ! OUT:
    ! res - Result of integration (double complex)
    !============================================================
    IMPLICIT NONE
    COMPLEX(KIND(1d0)),INTENT(OUT)::res
    COMPLEX(KIND(1d0)),EXTERNAL::fxn
    INTEGER,INTENT(IN)::n ! it is total number of intervals in the x
    REAL(KIND(1d0)),INTENT(IN)::end_val ! the upper value of integration
    REAL(KIND(1d0))::u,h
    INTEGER::i
    res=dcmplx(0d0,0d0)
    DO i=0,n
       u=(end_val*i)/n
       IF(i.EQ.0.OR.i.EQ.n)THEN
          res=res+fxn(u)
       ELSE
          res=res+dcmplx(2d0,0d0)*fxn(u)
       ENDIF
    ENDDO
    h=end_val/n
    res=dcmplx(h/2d0,0d0)*res
    RETURN
  END SUBROUTINE trapezoid_Cintegrator

  ! quadruple precision of trapezoid_Cintegrator
  SUBROUTINE trapezoid_Cintegrator_qp(n,fxn,end_val,res)
    !===========================================================
    ! Integration of the complex function fxn(x) on [0,end_val]
    !-----------------------------------------------------------
    ! IN:
    ! n - number of intervals
    ! fxn - Function to be integrate (complex*32)
    !     - the argument of fxn should be real*16
    ! end_val - Upper limit of integration
    ! OUT:
    ! res - Result of integration (complex*32)
    !============================================================
    IMPLICIT NONE
    COMPLEX(KIND(1E0_16)),INTENT(OUT)::res
    COMPLEX(KIND(1E0_16)),EXTERNAL::fxn
    INTEGER,INTENT(IN)::n ! it is total number of intervals in the x
    REAL(KIND(1E0_16)),INTENT(IN)::end_val ! the upper value of integration
    REAL(KIND(1E0_16))::u,h
    INTEGER::i
    res=cmplx(0E0_16,0E0_16,kind=16)
    DO i=0,n
       u=(end_val*i)/n
       IF(i.EQ.0.OR.i.EQ.n)THEN
          res=res+fxn(u)
       ELSE
          res=res+cmplx(2E0_16,0E0_16,kind=16)*fxn(u)
       ENDIF
    ENDDO
    h=end_val/n
    res=cmplx(h/2E0_16,0E0_16,kind=16)*res
    RETURN
  END SUBROUTINE trapezoid_Cintegrator_qp

  SUBROUTINE simpson_integrator(f,a,b,integral,n)
    !==========================================================
    ! Integration of f(x) on [a,b]
    ! Method: Simpson rule for n intervals
    ! written by: Alex Godunov (October 2009)
    !----------------------------------------------------------
    ! IN:
    ! f   - Function to integrate (supplied by a user)
    ! a  - Lower limit of integration
    ! b  - Upper limit of integration
    ! n   - number of intervals
    ! OUT:
    ! integral - Result of integration
    !==========================================================
    IMPLICIT NONE
    REAL(KIND(1d0)),EXTERNAL::f
    REAL(KIND(1d0)),INTENT(IN)::a, b
    REAL(KIND(1d0)),INTENT(OUT)::integral
    REAL(KIND(1d0))::s
    REAL(KIND(1d0))::h, x
    INTEGER::ninit,i
    INTEGER,INTENT(INOUT)::n
    ! if n is odd we add +1 to make it even
    IF((n/2)*2.ne.n) n=n+1
    ! loop over n (number of intervals)
    s = 0.0D0
    h = (b-a)/DBLE(n)
    DO i=2, n-2, 2
       x   = a+DBLE(i)*h
       s = s + 2.0*f(x) + 4.0*f(x+h)
    ENDDO
    integral = (s + f(a) + f(b) + 4.0*f(a+h))*h/3.0
    RETURN
  END SUBROUTINE simpson_integrator

  SUBROUTINE simpson_Cintegrator(f,a,b,integral,n)
    !==========================================================
    ! Integration of complex function f(x) on [a,b]
    ! Method: Simpson rule for n intervals
    ! written by: Alex Godunov (October 2009)
    !----------------------------------------------------------
    ! IN:
    ! f   - Function to integrate (supplied by a user, double complex)
    !     - the argument of f should be real
    ! a  - Lower limit of integration
    ! b  - Upper limit of integration
    ! n   - number of intervals
    ! OUT:
    ! integral - Result of integration (double complex)
    !==========================================================
    IMPLICIT NONE
    COMPLEX(KIND(1d0)),EXTERNAL::f
    REAL(KIND(1d0)),INTENT(IN)::a, b
    COMPLEX(KIND(1d0)),INTENT(OUT)::integral
    COMPLEX(KIND(1d0))::s
    REAL(KIND(1d0))::h, x
    INTEGER::ninit,i
    INTEGER,INTENT(INOUT)::n
    ! if n is odd we add +1 to make it even
    IF((n/2)*2.ne.n) n=n+1
    ! loop over n (number of intervals)
    s = dcmplx(0.0D0,0.0D0)
    h = (b-a)/DBLE(n)
    DO i=2, n-2, 2
       x =a+DBLE(i)*h
       s = s+dcmplx(2d0,0d0)*f(x)+dcmplx(4d0,0d0)*f(x+h)
    ENDDO
    integral = (s+f(a)+f(b)+dcmplx(4d0,0d0)*f(a+h))*dcmplx(h/3d0,0d0)
    RETURN
  END SUBROUTINE simpson_Cintegrator

  ! Tanh-sinh quadrature (see https://en.wikipedia.org/wiki/Tanh-sinh_quadrature#cite_note-1)
  ! It is originally from https://www.kurims.kyoto-u.ac.jp/~ooura/intde.html
  ! also at /Users/hua-shengshao/Physics/DGLAPSolver/integration/tanh_sinh_quadrature/intde
  ! The subroutines inside intde2.f
  ! Double Exponential (DE) Quadrature
  ! Numerical Automatic Integrator for Improper Integral
  !     method    : Double Exponential (DE) Transformation
  !     dimension : one
  !     table     : use
  ! subroutines
  !     DEQuadrature_integrator  : integrator of f(x) over (a,b), original intde.
  !     DEQuadrature_integrator_inf_nonosci : integrator of f(x) over (a,infinity),
  !                                           f(x) is non oscillatory function, original intdei.
  !     DEQuadrature_integrator_inf_osci : integrator of f(x) over (a,infinity),
  !                                        f(x) is oscillatory function, original intdeo.
  !
  !
  ! DEQuadrature_integrator
  !     [description]
  !         integral = integral of f(x) over (a,b)
  !     [declaration]
  !         external f
  !     [usage]
  !         call DEQuadrature_integrator_ini(lenaw, tiny, eps, aw)  ! initialization of aw, original intdeini
  !         ...
  !         call DEQuadrature_integrator(f, a, b, aw, integral, error)
  !     [parameters]
  !         lenaw     : length of aw (integer)
  !         tiny      : minimum value that 1/tiny does not
  !                     overflow (real*8)
  !         eps       : relative error requested (real*8)
  !         aw        : points and weights of the quadrature
  !                     formula, aw(0...lenaw-1) (real*8)
  !         f         : integrand f(x) (real*8 function)
  !         a         : lower limit of integration (real*8)
  !         b         : upper limit of integration (real*8)
  !         integral  : approximation to the integral (real*8)
  !         error     : estimate of the absolute error (real*8)
  !     [remarks]
  !             lenaw > 1000,
  !             IEEE double :
  !                 lenaw = 8000
  !                 tiny = 1.0d-307
  !         function
  !             f(x) needs to be analytic over (a,b).
  !         relative error
  !             eps is relative error requested excluding
  !             cancellation of significant digits.
  !             i.e. eps means : (absolute error) /
  !                              (integral_a^b |f(x)| dx).
  !             eps does not mean : (absolute error) / integral.
  !         error message
  !             error >= 0 : normal termination.
  !             error < 0  : abnormal termination.
  !                        i.e. convergent error is detected :
  !                            1. f(x) or (d/dx)^n f(x) has
  !                               discontinuous points or sharp
  !                               peaks over (a,b).
  !                               you must divide the interval
  !                               (a,b) at this points.
  !                            2. relative error of f(x) is
  !                               greater than eps.
  !                            3. f(x) has oscillatory factor
  !                               and frequency of the oscillation
  !                               is very high.
  !
  subroutine DEQuadrature_integrator_ini(lenaw,tiny,eps,aw)
    implicit none
    integer,intent(in)::lenaw
    real(kind(1d0)),intent(in)::tiny,eps
    real(kind(1d0)),dimension(0:lenaw-1),intent(out)::aw
    real(kind(1d0))::efs,hoff
    integer::noff,nk,k,j
    real(kind(1d0))::pi2,tinyln,epsln,h0,ehp,ehm,h,t,ep,em,xw,wg
    logical::firstloop,secondloop,thirdloop
    ! ---- adjustable parameter ----
    efs=0.1d0
    hoff=8.5d0
    ! ------------------------------
    pi2=2*atan(1.0d0)
    tinyln=-log(tiny)
    epsln=1-log(efs*eps)
    h0=hoff/epsln
    ehp=exp(h0)
    ehm=1/ehp
    aw(2)=eps
    aw(3)=exp(-ehm*epsln)
    aw(4)=sqrt(efs*eps)
    noff=5
    aw(noff)=0.5d0
    aw(noff+1)=h0
    aw(noff+2)=pi2*h0*0.5d0
    h=2
    nk=0
    k=noff+3
    firstloop=.TRUE.
    secondloop=.TRUE.
    thirdloop=.TRUE.
    !10 continue
    DO WHILE(firstloop.OR.2*k-noff-3.le.lenaw)
       firstloop=.FALSE.
       t=h*0.5d0
       !20     continue
       DO WHILE(secondloop.OR.t.lt.1)
          secondloop=.FALSE.
          em=exp(h0*t)
          ep=pi2*em
          em=pi2/em
          j=k
          !30         continue
          DO WHILE(thirdloop.OR.(ep.lt.tinyln.and.j.le.lenaw-3))
             thirdloop=.FALSE.
             xw=1/(1+exp(ep-em))
             wg=xw*(1-xw)*h0
             aw(j)=xw
             aw(j+1)=wg*4
             aw(j+2)=wg*(ep+em)
             ep=ep*ehp
             em=em*ehm
             j=j+3
          ENDDO
          t=t+h
          k=k+nk
       ENDDO
       h=h*0.5d0
       if(nk.eq.0)then
          if(j.gt.lenaw-6)j=j-3
          nk=j-noff
          k=k+nk
          aw(1)=nk
       endif
    ENDDO
    aw(0)=k-3
    RETURN
  end subroutine DEQuadrature_integrator_ini

  subroutine DEQuadrature_integrator(f,a,b,aw,integral,error)
    implicit none
    real(kind(1d0)),external::f
    real(kind(1d0)),intent(in)::a,b
    real(kind(1d0)),dimension(0:*),intent(in)::aw
    real(kind(1d0)),intent(out)::integral,error
    integer::noff,lenawm,nk,k,j,jtmp,jm,m,klim
    real(kind(1d0))::epsh,ba,ir,xa,fa,fb,errt,errh,errd,h,iback,irback
    logical::firstloop,secondloop,thirdloop,fourthloop
    noff=5
    lenawm=int(aw(0)+0.5d0)
    nk=int(aw(1)+0.5d0)
    epsh=aw(4)
    ba=b-a
    integral=f((a+b)*aw(noff))
    ir=integral*aw(noff+1)
    integral=integral*aw(noff+2)
    error=abs(integral)
    k=nk+noff
    j=noff
    firstloop=.TRUE.
    ! 10 continue
    DO WHILE(firstloop.OR.(aw(j).gt.epsh.and.j.lt.k))
       firstloop=.FALSE.
       j=j+3
       xa=ba*aw(j)
       fa=f(a+xa)
       fb=f(b-xa)
       ir=ir+(fa+fb)*aw(j+1)
       fa=fa*aw(j+2)
       fb=fb*aw(j+2)
       integral=integral+(fa+fb)
       error=error+(abs(fa)+abs(fb))
    ENDDO
    errt=error*aw(3)
    errh=error*epsh
    errd=1+2*errh
    jtmp=j
    do while(abs(fa).gt.errt.and.j.lt.k)
       j=j+3
       fa=f(a+ba*aw(j))
       ir=ir+fa*aw(j+1)
       fa=fa*aw(j+2)
       integral=integral+fa
    end do
    jm=j
    j=jtmp
    do while(abs(fb).gt.errt.and.j.lt.k)
       j=j+3
       fb=f(b-ba*aw(j))
       ir=ir+fb*aw(j+1)
       fb=fb*aw(j+2)
       integral=integral+fb
    end do
    if(j.lt.jm)jm=j
    jm=jm-(noff+3)
    h=1
    m=1
    klim=k+nk
    do while(errd.gt.errh.and.klim.le.lenawm)
       iback=integral
       irback=ir
       secondloop=.TRUE.
       !20     continue
       do while(secondloop.OR.k.lt.klim)
          secondloop=.FALSE.
          jtmp=k+jm
          do j=k+3,jtmp,3
             xa=ba*aw(j)
             fa=f(a+xa)
             fb=f(b-xa)
             ir=ir+(fa+fb)*aw(j+1)
             integral=integral+(fa+fb)*aw(j+2)
          end do
          k=k+nk
          j=jtmp
          thirdloop=.TRUE.
          ! 30         continue
          do while(thirdloop.OR.(abs(fa).gt.errt.and.j.lt.k))
             thirdloop=.FALSE.
             j=j+3
             fa=f(a+ba*aw(j))
             ir=ir+fa*aw(j+1)
             fa=fa*aw(j+2)
             integral=integral+fa
          enddo
          j=jtmp
          ! 40         continue
          fourthloop=.TRUE.
          do while(fourthloop.OR.(abs(fb).gt.errt.and.j.lt.k))
             fourthloop=.FALSE.
             j=j+3
             fb=f(b-ba*aw(j))
             ir=ir+fb*aw(j+1)
             fb=fb*aw(j+2)
             integral=integral+fb
          enddo
       enddo
       errd=h*(abs(integral-2*iback)+abs(ir-2*irback))
       h=h*0.5d0
       m=m*2
       klim=2*klim-noff
    enddo
    integral=integral*(h*ba)
    if (errd.gt.errh) then
       error=-errd*(m*abs(ba))
    else
       error=error*aw(2)*(m*abs(ba))
    end if
    return
  end subroutine DEQuadrature_integrator

  subroutine DEQuadrature_Cintegrator(f,a,b,aw,integral,error)
    implicit none
    complex(kind(1d0)),external::f
    real(kind(1d0)),intent(in)::a,b
    real(kind(1d0)),dimension(0:*),intent(in)::aw
    complex(kind(1d0)),intent(out)::integral
    real(kind(1d0)),intent(out)::error
    integer::noff,lenawm,nk,k,j,jtmp,jm,m,klim
    real(kind(1d0))::epsh,ba,xa,errt,errh,errd,h
    complex(kind(1d0))::ir,fa,fb,iback,irback
    logical::firstloop,secondloop,thirdloop,fourthloop
    noff=5
    lenawm=int(aw(0)+0.5d0)
    nk=int(aw(1)+0.5d0)
    epsh=aw(4)
    ba=b-a
    integral=f((a+b)*aw(noff))
    ir=integral*aw(noff+1)
    integral=integral*aw(noff+2)
    error=abs(integral)
    k=nk+noff
    j=noff
    firstloop=.TRUE.
    ! 10 continue                                                                          
    DO WHILE(firstloop.OR.(aw(j).gt.epsh.and.j.lt.k))
       firstloop=.FALSE.
       j=j+3
       xa=ba*aw(j)
       fa=f(a+xa)
       fb=f(b-xa)
       ir=ir+(fa+fb)*aw(j+1)
       fa=fa*aw(j+2)
       fb=fb*aw(j+2)
       integral=integral+(fa+fb)
       error=error+(abs(fa)+abs(fb))
    ENDDO
    errt=error*aw(3)
    errh=error*epsh
    errd=1+2*errh
    jtmp=j
    do while(abs(fa).gt.errt.and.j.lt.k)
       j=j+3
       fa=f(a+ba*aw(j))
       ir=ir+fa*aw(j+1)
       fa=fa*aw(j+2)
       integral=integral+fa
    end do
    jm=j
    j=jtmp
    do while(abs(fb).gt.errt.and.j.lt.k)
       j=j+3
       fb=f(b-ba*aw(j))
       ir=ir+fb*aw(j+1)
       fb=fb*aw(j+2)
       integral=integral+fb
    end do
    if(j.lt.jm)jm=j
    jm=jm-(noff+3)
    h=1
    m=1
    klim=k+nk
    do while(errd.gt.errh.and.klim.le.lenawm)
       iback=integral
       irback=ir
       secondloop=.TRUE.
       !20     continue
       do while(secondloop.OR.k.lt.klim)
          secondloop=.FALSE.
          jtmp=k+jm
          do j=k+3,jtmp,3
             xa=ba*aw(j)
             fa=f(a+xa)
             fb=f(b-xa)
             ir=ir+(fa+fb)*aw(j+1)
             integral=integral+(fa+fb)*aw(j+2)
          end do
          k=k+nk
          j=jtmp
          thirdloop=.TRUE.
          ! 30         continue
          do while(thirdloop.OR.(abs(fa).gt.errt.and.j.lt.k))
             thirdloop=.FALSE.
             j=j+3
             fa=f(a+ba*aw(j))
             ir=ir+fa*aw(j+1)
             fa=fa*aw(j+2)
             integral=integral+fa
          enddo
          j=jtmp
          ! 40         continue
          fourthloop=.TRUE.
          do while(fourthloop.OR.(abs(fb).gt.errt.and.j.lt.k))
             fourthloop=.FALSE.
             j=j+3
             fb=f(b-ba*aw(j))
             ir=ir+fb*aw(j+1)
             fb=fb*aw(j+2)
             integral=integral+fb
          enddo
       enddo
       errd=h*(abs(integral-2*iback)+abs(ir-2*irback))
       h=h*0.5d0
       m=m*2
       klim=2*klim-noff
    enddo
    integral=integral*(h*ba)
    if (errd.gt.errh) then
       error=-errd*(m*abs(ba))
    else
       error=error*aw(2)*(m*abs(ba))
    end if
    return
  end subroutine DEQuadrature_Cintegrator

  subroutine DEQuadrature_integrator_ini_qp(lenaw,tiny,eps,aw)
    implicit none
    integer,intent(in)::lenaw
    real(kind(1E0_16)),intent(in)::tiny,eps
    real(kind(1E0_16)),dimension(0:lenaw-1),intent(out)::aw
    real(kind(1E0_16))::efs,hoff
    integer::noff,nk,k,j
    real(kind(1E0_16))::pi2,tinyln,epsln,h0,ehp,ehm,h,t,ep,em,xw,wg
    logical::firstloop,secondloop,thirdloop
    ! ---- adjustable parameter ----
    efs=0.1E0_16
    hoff=8.5E0_16
    ! ------------------------------
    pi2=2*atan(1.0E0_16)
    tinyln=-log(tiny)
    epsln=1-log(efs*eps)
    h0=hoff/epsln
    ehp=exp(h0)
    ehm=1/ehp
    aw(2)=eps
    aw(3)=exp(-ehm*epsln)
    aw(4)=sqrt(efs*eps)
    noff=5
    aw(noff)=0.5E0_16
    aw(noff+1)=h0
    aw(noff+2)=pi2*h0*0.5E0_16
    h=2
    nk=0
    k=noff+3
    firstloop=.TRUE.
    secondloop=.TRUE.
    thirdloop=.TRUE.
    DO WHILE(firstloop.OR.2*k-noff-3.le.lenaw)
       firstloop=.FALSE.
       t=h*0.5E0_16
       DO WHILE(secondloop.OR.t.lt.1)
          secondloop=.FALSE.
          em=exp(h0*t)
          ep=pi2*em
          em=pi2/em
          j=k
          DO WHILE(thirdloop.OR.(ep.lt.tinyln.and.j.le.lenaw-3))
             thirdloop=.FALSE.
             xw=1/(1+exp(ep-em))
             wg=xw*(1-xw)*h0
             aw(j)=xw
             aw(j+1)=wg*4
             aw(j+2)=wg*(ep+em)
             ep=ep*ehp
             em=em*ehm
             j=j+3
          ENDDO
          t=t+h
          k=k+nk
       ENDDO
       h=h*0.5E0_16
       if(nk.eq.0)then
          if(j.gt.lenaw-6)j=j-3
          nk=j-noff
          k=k+nk
          aw(1)=nk
       endif
    ENDDO
    aw(0)=k-3
    RETURN
  end subroutine DEQuadrature_integrator_ini_qp

  subroutine DEQuadrature_Cintegrator_qp(f,a,b,aw,integral,error)
    implicit none
    complex(kind(1E0_16)),external::f
    real(kind(1E0_16)),intent(in)::a,b
    real(kind(1E0_16)),dimension(0:*),intent(in)::aw
    complex(kind(1E0_16)),intent(out)::integral
    real(kind(1E0_16)),intent(out)::error
    integer::noff,lenawm,nk,k,j,jtmp,jm,m,klim
    real(kind(1E0_16))::epsh,ba,xa,errt,errh,errd,h
    complex(kind(1E0_16))::ir,fa,fb,iback,irback
    logical::firstloop,secondloop,thirdloop,fourthloop
    noff=5
    lenawm=int(aw(0)+0.5E0_16)
    nk=int(aw(1)+0.5E0_16)
    epsh=aw(4)
    ba=b-a
    integral=f((a+b)*aw(noff))
    ir=integral*aw(noff+1)
    integral=integral*aw(noff+2)
    error=abs(integral)
    k=nk+noff
    j=noff
    firstloop=.TRUE.
    DO WHILE(firstloop.OR.(aw(j).gt.epsh.and.j.lt.k))
       firstloop=.FALSE.
       j=j+3
       xa=ba*aw(j)
       fa=f(a+xa)
       fb=f(b-xa)
       ir=ir+(fa+fb)*aw(j+1)
       fa=fa*aw(j+2)
       fb=fb*aw(j+2)
       integral=integral+(fa+fb)
       error=error+(abs(fa)+abs(fb))
    ENDDO
    errt=error*aw(3)
    errh=error*epsh
    errd=1+2*errh
    jtmp=j
    do while(abs(fa).gt.errt.and.j.lt.k)
       j=j+3
       fa=f(a+ba*aw(j))
       ir=ir+fa*aw(j+1)
       fa=fa*aw(j+2)
       integral=integral+fa
    end do
    jm=j
    j=jtmp
    do while(abs(fb).gt.errt.and.j.lt.k)
       j=j+3
       fb=f(b-ba*aw(j))
       ir=ir+fb*aw(j+1)
       fb=fb*aw(j+2)
       integral=integral+fb
    end do
    if(j.lt.jm)jm=j
    jm=jm-(noff+3)
    h=1
    m=1
    klim=k+nk
    do while(errd.gt.errh.and.klim.le.lenawm)
       iback=integral
       irback=ir
       secondloop=.TRUE.
       do while(secondloop.OR.k.lt.klim)
          secondloop=.FALSE.
          jtmp=k+jm
          do j=k+3,jtmp,3
             xa=ba*aw(j)
             fa=f(a+xa)
             fb=f(b-xa)
             ir=ir+(fa+fb)*aw(j+1)
             integral=integral+(fa+fb)*aw(j+2)
          end do
          k=k+nk
          j=jtmp
          thirdloop=.TRUE.
          do while(thirdloop.OR.(abs(fa).gt.errt.and.j.lt.k))
             thirdloop=.FALSE.
             j=j+3
             fa=f(a+ba*aw(j))
             ir=ir+fa*aw(j+1)
             fa=fa*aw(j+2)
             integral=integral+fa
          enddo
          j=jtmp
          fourthloop=.TRUE.
          do while(fourthloop.OR.(abs(fb).gt.errt.and.j.lt.k))
             fourthloop=.FALSE.
             j=j+3
             fb=f(b-ba*aw(j))
             ir=ir+fb*aw(j+1)
             fb=fb*aw(j+2)
             integral=integral+fb
          enddo
       enddo
       errd=h*(abs(integral-2*iback)+abs(ir-2*irback))
       h=h*0.5E0_16
       m=m*2
       klim=2*klim-noff
    enddo
    integral=integral*(h*ba)
    if (errd.gt.errh) then
       error=-errd*(m*abs(ba))
    else
       error=error*aw(2)*(m*abs(ba))
    end if
    return
  end subroutine DEQuadrature_Cintegrator_qp

  ! DEQuadrature_integrator_inf_nonosci
  !     [description]
  !         integral = integral of f(x) over (a,infinity),
  !             f(x) has not oscillatory factor.
  !     [declaration]
  !         external f
  !     [usage]
  !         call DEQuadrature_integrator_inf_nonosci_ini(lenaw, tiny, eps, aw)  ! initialization of aw
  !         ...
  !         call DEQuadrature_integrator_inf_nonosci(f, a, aw, integral, error)
  !     [parameters]
  !         lenaw     : length of aw (integer)
  !         tiny      : minimum value that 1/tiny does not
  !                     overflow (real*8)
  !         eps       : relative error requested (real*8)
  !         aw        : points and weights of the quadrature
  !                     formula, aw(0...lenaw-1) (real*8)
  !         f         : integrand f(x) (real*8 function)
  !         a         : lower limit of integration (real*8)
  !         integral  : approximation to the integral (real*8)
  !         error     : estimate of the absolute error (real*8)
  !     [remarks]
  !         initial parameters
  !             lenaw > 1000,
  !             IEEE double :
  !                 lenaw = 8000
  !                 tiny = 1.0d-307
  !             IEEE quadruple :
  !                 lenaw = 16000
  !                 tiny = 1.0E-4931_16  
  !         function
  !             f(x) needs to be analytic over (a,infinity).
  !         relative error
  !             eps is relative error requested excluding
  !             cancellation of significant digits.
  !             i.e. eps means : (absolute error) /
  !                              (integral_a^infinity |f(x)| dx).
  !             eps does not mean : (absolute error) / integral.
  !         error message
  !             err >= 0 : normal termination.
  !             err < 0  : abnormal termination.
  !                        i.e. convergent error is detected :
  !                            1. f(x) or (d/dx)^n f(x) has
  !                               discontinuous points or sharp
  !                               peaks over (a,infinity).
  !                               you must divide the interval
  !                               (a,infinity) at this points.
  !                            2. relative error of f(x) is
  !                               greater than eps.
  !                            3. f(x) has oscillatory factor
  !                               and decay of f(x) is very slow
  !                               as x -> infinity.
  !
  subroutine DEQuadrature_integrator_inf_nonosci_ini(lenaw,tiny,eps,aw)
    implicit none
    integer,intent(in)::lenaw
    real(kind(1d0)),intent(in)::tiny,eps
    real(kind(1d0)),dimension(0:lenaw-1),intent(out)::aw
    real(kind(1d0))::efs,hoff
    integer::noff,nk,k,j
    real(kind(1d0))::pi4,tinyln,epsln,h0,ehp,ehm,h,t,ep,em,xp,xm,wp,wm
    logical::firstloop,secondloop,thirdloop
    ! ---- adjustable parameter ----
    efs=0.1d0
    hoff=11.0d0
    ! ------------------------------
    pi4=atan(1.0d0)
    tinyln=-log(tiny)
    epsln=1-log(efs * eps)
    h0=hoff/epsln
    ehp=exp(h0)
    ehm=1/ehp
    aw(2)=eps
    aw(3)=exp(-ehm*epsln)
    aw(4)=sqrt(efs*eps)
    noff=5
    aw(noff)=1
    aw(noff+1)=4*h0
    aw(noff+2)=2*pi4*h0
    h=2
    nk=0
    k=noff+6
    firstloop=.TRUE.
    secondloop=.TRUE.
    thirdloop=.TRUE.
    !10 continue
    DO WHILE(firstloop.OR.2*k-noff-6.le.lenaw)
       firstloop=.FALSE.
       t=h*0.5d0
       !20     continue
       DO WHILE(secondloop.OR.t.lt.1)
          secondloop=.FALSE.
          em=exp(h0*t)
          ep=pi4*em
          em=pi4/em
          j=k
          !30         continue
          DO WHILE(thirdloop.OR.(ep.lt.tinyln.and.j.le.lenaw-6))
             thirdloop=.FALSE.
             xp=exp(ep-em)
             xm=1/xp
             wp=xp*((ep+em)*h0)
             wm=xm*((ep+em)*h0)
             aw(j)=xm
             aw(j+1)=xp
             aw(j+2)=xm*(4*h0)
             aw(j+3)=xp*(4*h0)
             aw(j+4)=wm
             aw(j+5)=wp
             ep=ep*ehp
             em=em*ehm
             j=j+6
          ENDDO
          t=t+h
          k=k+nk
       ENDDO
       h=h*0.5d0
       if(nk.eq.0)then
          if(j.gt.lenaw-12)j=j-6
          nk=j-noff
          k=k+nk
          aw(1)=nk
       end if
    ENDDO
    aw(0)=k-6
    return
  end subroutine DEQuadrature_integrator_inf_nonosci_ini

  subroutine DEQuadrature_integrator_inf_nonosci(f,a,aw,integral,error)
    implicit none
    real(kind(1d0)),external::f
    real(kind(1d0)),intent(in)::a
    real(kind(1d0)),dimension(0:*),intent(in)::aw
    real(kind(1d0)),intent(out)::integral,error
    integer::noff,lenawm,nk,k,j,jtmp,jm,m,klim
    real(kind(1d0))::epsh,ir,fp,fm,errt,errh,errd,h,iback,irback
    logical::firstloop,secondloop,thirdloop,fourthloop
    noff=5
    lenawm=int(aw(0)+0.5d0)
    nk=int(aw(1)+0.5d0)
    epsh=aw(4)
    integral=f(a+aw(noff))
    ir=integral*aw(noff+1)
    integral=integral*aw(noff+2)
    error=abs(integral)
    k=nk+noff
    j=noff
    firstloop=.TRUE.
    !10 continue
    DO WHILE(firstloop.OR.(aw(j).gt.epsh.and.j.lt.k))
       firstloop=.FALSE.
       j=j+6
       fm=f(a+aw(j))
       fp=f(a+aw(j+1))
       ir=ir+(fm*aw(j+2)+fp*aw(j+3))
       fm=fm*aw(j+4)
       fp=fp*aw(j+5)
       integral=integral+(fm+fp)
       error=error+(abs(fm)+abs(fp))
    ENDDO
    errt=error*aw(3)
    errh=error*epsh
    errd=1+2*errh
    jtmp=j
    do while(abs(fm).gt.errt.and.j.lt.k)
       j=j+6
       fm=f(a+aw(j))
       ir=ir+fm*aw(j+2)
       fm=fm*aw(j+4)
       integral=integral+fm
    end do
    jm=j
    j=jtmp
    do while(abs(fp).gt.errt.and.j.lt.k)
       j=j+6
       fp=f(a+aw(j+1))
       ir=ir+fp*aw(j+3)
       fp=fp*aw(j+5)
       integral=integral+fp
    end do
    if(j.lt.jm)jm=j
    jm=jm-(noff+6)
    h=1
    m=1
    klim=k+nk
    do while(errd.gt.errh.and.klim.le.lenawm)
       iback=integral
       irback=ir
       secondloop=.TRUE.
       !20     continue
       do while(secondloop.OR.k.lt.klim)
          secondloop=.FALSE.
          jtmp=k+jm
          do j=k+6, jtmp, 6
             fm=f(a+aw(j))
             fp=f(a+aw(j+1))
             ir=ir+(fm*aw(j+2)+fp*aw(j+3))
             integral=integral+(fm*aw(j+4)+fp*aw(j+5))
          end do
          k=k+nk
          j=jtmp
          thirdloop=.TRUE.
          !30         continue
          do while(thirdloop.OR.(abs(fm).gt.errt.and.j.lt.k))
             thirdloop=.FALSE.
             j=j+6
             fm=f(a+aw(j))
             ir=ir+fm*aw(j+2)
             fm=fm*aw(j+4)
             integral=integral+fm
          enddo
          j=jtmp
          fourthloop=.TRUE.
          !40         continue
          do while(fourthloop.OR.(abs(fp).gt.errt.and.j.lt.k))
             fourthloop=.FALSE.
             j=j+6
             fp=f(a+aw(j+1))
             ir=ir+fp*aw(j+3)
             fp=fp*aw(j+5)
             integral=integral+fp
          enddo          
       enddo
       errd=h*(abs(integral-2*iback)+abs(ir-2*irback))
       h=h*0.5d0
       m=m*2
       klim=2*klim-noff
    enddo
    integral=integral*h
    if (errd.gt.errh) then
       error=-errd*m
    else
       error=error*(aw(2)*m)
    end if
    return
  end subroutine DEQuadrature_integrator_inf_nonosci

  subroutine DEQuadrature_Cintegrator_inf_nonosci(f,a,aw,integral,error)
    implicit none
    complex(kind(1d0)),external::f
    real(kind(1d0)),intent(in)::a
    real(kind(1d0)),dimension(0:*),intent(in)::aw
    complex(kind(1d0)),intent(out)::integral
    real(kind(1d0)),intent(out)::error
    integer::noff,lenawm,nk,k,j,jtmp,jm,m,klim
    real(kind(1d0))::epsh,errt,errh,errd,h
    complex(kind(1d0))::ir,fp,fm,iback,irback
    logical::firstloop,secondloop,thirdloop,fourthloop
    noff=5
    lenawm=int(aw(0)+0.5d0)
    nk=int(aw(1)+0.5d0)
    epsh=aw(4)
    integral=f(a+aw(noff))
    ir=integral*aw(noff+1)
    integral=integral*aw(noff+2)
    error=abs(integral)
    k=nk+noff
    j=noff
    firstloop=.TRUE.
    !10 continue
    DO WHILE(firstloop.OR.(aw(j).gt.epsh.and.j.lt.k))
       firstloop=.FALSE.
       j=j+6
       fm=f(a+aw(j))
       fp=f(a+aw(j+1))
       ir=ir+(fm*aw(j+2)+fp*aw(j+3))
       fm=fm*aw(j+4)
       fp=fp*aw(j+5)
       integral=integral+(fm+fp)
       error=error+(abs(fm)+abs(fp))
    ENDDO
    errt=error*aw(3)
    errh=error*epsh
    errd=1+2*errh
    jtmp=j
    do while(abs(fm).gt.errt.and.j.lt.k)
       j=j+6
       fm=f(a+aw(j))
       ir=ir+fm*aw(j+2)
       fm=fm*aw(j+4)
       integral=integral+fm
    end do
    jm=j
    j=jtmp
    do while(abs(fp).gt.errt.and.j.lt.k)
       j=j+6
       fp=f(a+aw(j+1))
       ir=ir+fp*aw(j+3)
       fp=fp*aw(j+5)
       integral=integral+fp
    end do
    if(j.lt.jm)jm=j
    jm=jm-(noff+6)
    h=1
    m=1
    klim=k+nk
    do while(errd.gt.errh.and.klim.le.lenawm)
       iback=integral
       irback=ir
       secondloop=.TRUE.
       !20     continue
       do while(secondloop.OR.k.lt.klim)
          secondloop=.FALSE.
          jtmp=k+jm
          do j=k+6, jtmp, 6
             fm=f(a+aw(j))
             fp=f(a+aw(j+1))
             ir=ir+(fm*aw(j+2)+fp*aw(j+3))
             integral=integral+(fm*aw(j+4)+fp*aw(j+5))
          end do
          k=k+nk
          j=jtmp
          thirdloop=.TRUE.
          !30         continue
          do while(thirdloop.OR.(abs(fm).gt.errt.and.j.lt.k))
             thirdloop=.FALSE.
             j=j+6
             fm=f(a+aw(j))
             ir=ir+fm*aw(j+2)
             fm=fm*aw(j+4)
             integral=integral+fm
          enddo
          j=jtmp
          fourthloop=.TRUE.
          !40         continue
          do while(fourthloop.OR.(abs(fp).gt.errt.and.j.lt.k))
             fourthloop=.FALSE.
             j=j+6
             fp=f(a+aw(j+1))
             ir=ir+fp*aw(j+3)
             fp=fp*aw(j+5)
             integral=integral+fp
          enddo
       enddo
       errd=h*(abs(integral-2*iback)+abs(ir-2*irback))
       h=h*0.5d0
       m=m*2
       klim=2*klim-noff
    enddo
    integral=integral*h
    if (errd.gt.errh) then
       error=-errd*m
    else
       error=error*(aw(2)*m)
    end if
    return
  end subroutine DEQuadrature_Cintegrator_inf_nonosci

  ! DEQuadrature_integrator_inf_osci
  !     [description]
  !         integral = integral of f(x) over (a,infinity),
  !             f(x) has oscillatory factor :
  !             f(x) = g(x) * sin(omega * x + theta) as x -> infinity.
  !     [declaration]
  !         external f
  !     [usage]
  !         call DEQuadrature_integrator_inf_osci_ini(lenaw,tiny,eps,aw)  ! initialization of aw, original intdeoini
  !         ...
  !         call DEQuadrature_integrator_inf_osci(f,a,omega,aw,integral,error)
  !     [parameters]
  !         lenaw     : length of aw (integer)
  !         tiny      : minimum value that 1/tiny does not
  !                     overflow (real*8)
  !         eps       : relative error requested (real*8)
  !         aw        : points and weights of the quadrature
  !                     formula, aw(0...lenaw-1) (real*8)
  !         f         : integrand f(x) (real*8 function)
  !         a         : lower limit of integration (real*8)
  !         omega     : frequency of oscillation (real*8)
  !         integral  : approximation to the integral (real*8)
  !         error     : estimate of the absolute error (real*8)
  !     [remarks]
  !         initial parameters
  !             lenaw > 1000,
  !             IEEE double :
  !                 lenaw = 8000
  !                 tiny = 1.0d-307
  !         function
  !             f(x) needs to be analytic over (a,infinity).
  !         relative error
  !             eps is relative error requested excluding
  !             cancellation of significant digits.
  !             i.e. eps means : (absolute error) /
  !                              (integral_a^R |f(x)| dx).
  !             eps does not mean : (absolute error) / integral.
  !         error message
  !             error >= 0 : normal termination.
  !             error < 0  : abnormal termination.
  !                        i.e. convergent error is detected :
  !                            1. f(x) or (d/dx)^n f(x) has
  !                               discontinuous points or sharp
  !                               peaks over (a,infinity).
  !                               you must divide the interval
  !                               (a,infinity) at this points.
  !                            2. relative error of f(x) is
  !                               greater than eps.
  !
  subroutine DEQuadrature_integrator_inf_osci_ini(lenaw,tiny,eps,aw)
    implicit none
    integer,intent(in)::lenaw
    real(kind(1d0)),intent(in)::tiny,eps
    real(kind(1d0)),dimension(0:lenaw-1),intent(out)::aw
    integer::lmax
    real(kind(1d0))::efs,enoff,pqoff,ppoff
    integer::noff0,nk0,noff,k,nk,j
    real(kind(1d0))::pi4,tinyln,epsln,frq4,per2,pp,pq,ehp,ehm,h,t,ep,em,tk,xw,wg,xa
    logical::firstloop,secondloop,thirdloop
    ! ---- adjustable parameter ----
    lmax=5
    efs=0.1d0
    enoff=0.40d0
    pqoff=2.9d0
    ppoff=-0.72d0
    ! ------------------------------
    pi4=atan(1.0d0)
    tinyln=-log(tiny)
    epsln=1-log(efs*eps)
    frq4=1/(2*pi4)
    per2=4*pi4
    pq=pqoff/epsln
    pp=ppoff-log(pq*pq*frq4)
    ehp=exp(2*pq)
    ehm=1/ehp
    aw(3)=lmax
    aw(4)=eps
    aw(5)=sqrt(efs*eps)
    noff0=6
    nk0=1+int(enoff*epsln)
    aw(1)=nk0
    noff=2*nk0+noff0
    wg=0
    xw=1
    do k=1, nk0
       wg=wg+xw
       aw(noff-2*k)=wg
       aw(noff-2*k+1)=xw
       xw=xw*(nk0-k)/k
    end do
    wg=per2/wg
    do k = noff0, noff-2, 2
       aw(k)=aw(k)*wg
       aw(k+1)=aw(k+1)*wg
    end do
    xw=exp(pp-2*pi4)
    aw(noff)=sqrt(xw*(per2*0.5d0))
    aw(noff+1)=xw*pq
    aw(noff+2)=per2*0.5d0
    h=2
    nk=0
    k=noff+3
    firstloop=.TRUE.
    !10 continue
    do while(firstloop.OR.(2*k-noff-3.le.lenaw))
       firstloop=.FALSE.
       t=h*0.5d0
       secondloop=.TRUE.
       !20     continue
       do while(secondloop.OR.t.lt.1)
          secondloop=.FALSE.
          em=exp(2*pq*t)
          ep=pi4*em
          em=pi4/em
          tk=t
          j=k
          thirdloop=.TRUE.
          !30         continue
          do while(thirdloop.OR.(ep.lt.tinyln.and.j.le.lenaw-3))
             thirdloop=.FALSE.
             xw=exp(pp-ep-em)
             wg=sqrt(frq4*xw+tk*tk)
             xa=xw/(tk+wg)
             wg=(pq*xw*(ep-em)+xa)/wg
             aw(j)=xa
             aw(j+1)=xw*pq
             aw(j+2)=wg
             ep=ep*ehp
             em=em*ehm
             tk=tk+1
             j=j+3
          enddo
          t=t+h
          k=k+nk
       enddo
       h=h*0.5d0
       if(nk.eq.0)then
          if(j.gt.lenaw-6)j=j-3
          nk=j-noff
          k=k+nk
          aw(2)=nk
       end if
    enddo
    aw(0)=k-3
    return
  end subroutine DEQuadrature_integrator_inf_osci_ini

  subroutine DEQuadrature_integrator_inf_osci(f,a,omega,aw,integral,error)
    implicit none
    real(kind(1d0)),external::f
    real(kind(1d0)),intent(in)::a,omega
    real(kind(1d0)),dimension(0:*),intent(in)::aw
    real(kind(1d0)),intent(out)::integral,error
    integer::lenawm,nk0,noff0,nk,noff,lmax,m,k,j,jm,l
    real(kind(1d0))::eps,per,perw,w02,ir,h,iback,irback,t,tk,xa,fm,fp,errh,s0,s1,s2,errd
    logical::firstloop,secondloop,thirdloop,fourthloop
    lenawm=int(aw(0)+0.5d0)
    nk0=int(aw(1)+0.5d0)
    noff0=6
    nk=int(aw(2)+0.5d0)
    noff=2*nk0+noff0
    lmax=int(aw(3)+0.5d0)
    eps=aw(4)
    per=1/abs(omega)
    w02=2*aw(noff+2)
    perw=per*w02
    integral=f(a+aw(noff)*per)
    ir=integral*aw(noff+1)
    integral=integral*aw(noff+2)
    error=abs(integral)
    h=2
    m=1
    k=noff
    firstloop=.TRUE.
    !10 continue
    do while(firstloop.OR.(errd.gt.errh.and.2*k-noff.le.lenawm))
       firstloop=.FALSE.
       iback=integral
       irback=ir
       t=h*0.5d0
       secondloop=.TRUE.
       !20     continue
       do while(secondloop.OR.t.lt.1)
          secondloop=.FALSE.
          if(k.eq.noff)then
             tk=1
             k=k+nk
             j=noff
             thirdloop=.TRUE.
             !30             continue
             do while(thirdloop.OR.(aw(j).gt.eps.and.j.lt.k))
                thirdloop=.FALSE.
                j=j+3
                xa=per*aw(j)
                fm=f(a+xa)
                fp=f(a+xa+perw*tk)
                ir=ir+(fm+fp)*aw(j+1)
                fm=fm*aw(j+2)
                fp=fp*(w02-aw(j+2))
                integral=integral+(fm+fp)
                error=error+(abs(fm)+abs(fp))
                tk=tk+1
             enddo
             errh=error*aw(5)
             error=error*eps
             jm=j-noff
          else
             tk=t
             do j=k+3, k+jm, 3
                xa=per*aw(j)
                fm=f(a+xa)
                fp=f(a+xa+perw*tk)
                ir=ir+(fm+fp)*aw(j+1)
                fm=fm*aw(j+2)
                fp=fp*(w02-aw(j+2))
                integral=integral+(fm+fp)
                tk=tk+1
             end do
             j=k+jm
             k=k+nk
          endif
          do while(abs(fm).gt.error.and.j.lt.k)
             j=j+3
             fm=f(a+per*aw(j))
             ir=ir+fm*aw(j+1)
             fm=fm*aw(j+2)
             integral=integral+fm
          end do
          fm=f(a+perw*tk)
          s2=w02*fm
          integral=integral+s2
          if (abs(fp).gt.error.or.abs(s2).gt.error)then
             l=0
             fourthloop=.TRUE.
             !40             continue
             DO WHILE(fourthloop)
                l=l+1
                s0=0
                s1=0
                s2=fm*aw(noff0+1)
                do j=noff0+2, noff-2, 2
                   tk=tk+1
                   fm=f(a+perw*tk)
                   s0=s0+fm
                   s1=s1+fm*aw(j)
                   s2=s2+fm*aw(j+1)
                end do
                if (s2.le.error.or.l.ge.lmax)then
                   fourthloop=.FALSE.
                   exit
                endif
                integral=integral+w02*s0
             ENDDO
             integral=integral+s1
             if(s2.gt.error)error=s2
          end if
          t=t+h
       enddo
       if(m.eq.1)then
          errd=1+2*errh
       else
          errd=h*(abs(integral-2*iback)+abs(ir-2*irback))
       end if
       h=h*0.5d0
       m=m*2
    enddo
    integral=integral*(h*per)
    if(errd.gt.errh)then
       error=-errd*per
    else
       error=error*(per*m*0.5d0)
    end if
    return
  end subroutine DEQuadrature_integrator_inf_osci

  subroutine DEQuadrature_Cintegrator_inf_osci(f,a,omega,aw,integral,error)
    implicit none
    complex(kind(1d0)),external::f
    real(kind(1d0)),intent(in)::a,omega
    real(kind(1d0)),dimension(0:*),intent(in)::aw
    complex(kind(1d0)),intent(out)::integral
    real(kind(1d0)),intent(out)::error
    integer::lenawm,nk0,noff0,nk,noff,lmax,m,k,j,jm,l
    real(kind(1d0))::eps,per,perw,w02,h,t,tk,xa,errh,errd
    complex(kind(1d0))::ir,iback,irback,fm,fp,s0,s1,s2
    logical::firstloop,secondloop,thirdloop,fourthloop
    lenawm=int(aw(0)+0.5d0)
    nk0=int(aw(1)+0.5d0)
    noff0=6
    nk=int(aw(2)+0.5d0)
    noff=2*nk0+noff0
    lmax=int(aw(3)+0.5d0)
    eps=aw(4)
    per=1/abs(omega)
    w02=2*aw(noff+2)
    perw=per*w02
    integral=f(a+aw(noff)*per)
    ir=integral*aw(noff+1)
    integral=integral*aw(noff+2)
    error=abs(integral)
    h=2
    m=1
    k=noff
    firstloop=.TRUE.
    !10 continue
    do while(firstloop.OR.(errd.gt.errh.and.2*k-noff.le.lenawm))
       firstloop=.FALSE.
       iback=integral
       irback=ir
       t=h*0.5d0
       secondloop=.TRUE.
       !20     continue
       do while(secondloop.OR.t.lt.1)
          secondloop=.FALSE.
          if(k.eq.noff)then
             tk=1
             k=k+nk
             j=noff
             thirdloop=.TRUE.
             !30             continue
             do while(thirdloop.OR.(aw(j).gt.eps.and.j.lt.k))
                thirdloop=.FALSE.
                j=j+3
                xa=per*aw(j)
                fm=f(a+xa)
                fp=f(a+xa+perw*tk)
                ir=ir+(fm+fp)*aw(j+1)
                fm=fm*aw(j+2)
                fp=fp*(w02-aw(j+2))
                integral=integral+(fm+fp)
                error=error+(abs(fm)+abs(fp))
                tk=tk+1
             enddo
             errh=error*aw(5)
             error=error*eps
             jm=j-noff
          else
             tk=t
             do j=k+3, k+jm, 3
                xa=per*aw(j)
                fm=f(a+xa)
                fp=f(a+xa+perw*tk)
                ir=ir+(fm+fp)*aw(j+1)
                fm=fm*aw(j+2)
                fp=fp*(w02-aw(j+2))
                integral=integral+(fm+fp)
                tk=tk+1
                end do
             j=k+jm
             k=k+nk
          endif
          do while(abs(fm).gt.error.and.j.lt.k)
             j=j+3
             fm=f(a+per*aw(j))
             ir=ir+fm*aw(j+1)
             fm=fm*aw(j+2)
             integral=integral+fm
          end do
          fm=f(a+perw*tk)
          s2=w02*fm
          integral=integral+s2
          if (abs(fp).gt.error.or.abs(s2).gt.error)then
             l=0
             fourthloop=.TRUE.
             !40             continue
             DO WHILE(fourthloop)
                l=l+1
                s0=0
                s1=0
                s2=fm*aw(noff0+1)
                do j=noff0+2, noff-2, 2
                   tk=tk+1
                   fm=f(a+perw*tk)
                   s0=s0+fm
                   s1=s1+fm*aw(j)
                   s2=s2+fm*aw(j+1)
                end do
                if (abs(s2).le.error.or.l.ge.lmax)then
                   fourthloop=.FALSE.
                   exit
                endif
                integral=integral+w02*s0
             ENDDO
             integral=integral+s1
             if(abs(s2).gt.error)error=abs(s2)
          end if
          t=t+h
       enddo
       if(m.eq.1)then
          errd=1+2*errh
       else
          errd=h*(abs(integral-2*iback)+abs(ir-2*irback))
       end if
       h=h*0.5d0
       m=m*2
    enddo
    integral=integral*(h*per)
    if(errd.gt.errh)then
       error=-errd*per
    else
       error=error*(per*m*0.5d0)
       end if
    return
  end subroutine DEQuadrature_Cintegrator_inf_osci

END MODULE simple_integrators
