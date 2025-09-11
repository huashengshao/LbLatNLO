MODULE simple_integrators
  IMPLICIT NONE
  PRIVATE
  PUBLIC::trapezoid_integrator,simpson_integrator
  PUBLIC::trapezoid_Cintegrator,simpson_Cintegrator
  PUBLIC::trapezoid_Cintegrator_qp
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
END MODULE simple_integrators
