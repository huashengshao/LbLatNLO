MODULE nielsen_generalized_polylog_wrapper
  USE nielsen_generalized_polylog
  IMPLICIT NONE
CONTAINS

  FUNCTION li5(x)
    IMPLICIT NONE
    REAL(KIND(1d0))::li5
    REAL(KIND(1d0)),INTENT(IN)::x
    li5=Nielsen_PolyLog(4,1,x)
    RETURN
  END FUNCTION li5

  FUNCTION RLi5P(a)
    USE Func_PSI
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::RLi5P
    REAL(KIND(1d0)),INTENT(IN)::a
    REAL(KIND(1d0))::b
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    REAL(KIND(1d0)),PARAMETER::zeta4=1.08232323371113819151600369654d0
    COMPLEX(KIND(1d0))::loga
    IF(a.LE.1d0)THEN
       RLi5P=dcmplx(li5(a),0d0)
    ELSE
       b=1d0/a
       loga=RLOGP(a)
       RLi5P=li5(b)+2d0*zeta4*loga+zeta2/3d0*loga**3&
            +pipi/24d0*(a-1d0)*loga**4/RSQRTM(-(a-1d0)**2)&
            -loga**5/120d0
    ENDIF
    RETURN
  END FUNCTION RLi5P

  FUNCTION RLi5M(a)
    USE Func_PSI
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::RLi5M
    REAL(KIND(1d0)),INTENT(IN)::a
    REAL(KIND(1d0))::b
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    REAL(KIND(1d0)),PARAMETER::zeta4=1.08232323371113819151600369654d0
    COMPLEX(KIND(1d0))::loga
    IF(a.LE.1d0)THEN
       RLi5M=dcmplx(li5(a),0d0)
    ELSE
       b=1d0/a
       loga=RLOGM(a)
       RLi5M=li5(b)+2d0*zeta4*loga+zeta2/3d0*loga**3&
            +pipi/24d0*(a-1d0)*loga**4/RSQRTP(-(a-1d0)**2)&
            -loga**5/120d0
    ENDIF
    RETURN
  END FUNCTION RLi5M

  FUNCTION li4(x)
    IMPLICIT NONE
    REAL(KIND(1d0))::li4
    REAL(KIND(1d0)),INTENT(IN)::x
    li4=Nielsen_PolyLog(3,1,x)
    RETURN
  END FUNCTION li4

  FUNCTION RLi4P(a)
    USE Func_PSI
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::RLi4P
    REAL(KIND(1d0)),INTENT(IN)::a
    REAL(KIND(1d0))::b
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    REAL(KIND(1d0)),PARAMETER::zeta4=1.08232323371113819151600369654d0
    COMPLEX(KIND(1d0))::loga
    IF(a.LE.1d0)THEN
       RLi4P=dcmplx(li4(a),0d0)
    ELSE
       b=1d0/a
       loga=RLOGP(a)
       RLi4P=-li4(b)+2d0*zeta4+zeta2*loga**2&
            +pipi/6d0*(a-1d0)*loga**3/RSQRTM(-(a-1d0)**2)&
            -loga**4/24d0
    ENDIF
    RETURN
  END FUNCTION RLi4P

  FUNCTION RLi4M(a)
    USE Func_PSI
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::RLi4M
    REAL(KIND(1d0)),INTENT(IN)::a
    REAL(KIND(1d0))::b
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    REAL(KIND(1d0)),PARAMETER::zeta4=1.08232323371113819151600369654d0
    COMPLEX(KIND(1d0))::loga
    IF(a.LE.1d0)THEN
       RLi4M=dcmplx(li4(a),0d0)
    ELSE
       b=1d0/a
       loga=RLOGM(a)
       RLi4M=-li4(b)+2d0*zeta4+zeta2*loga**2&
            +pipi/6d0*(a-1d0)*loga**3/RSQRTP(-(a-1d0)**2)&
            -loga**4/24d0
    ENDIF
    RETURN
  END FUNCTION RLi4M

  FUNCTION li3(x)
    IMPLICIT NONE
    REAL(KIND(1d0))::li3
    REAL(KIND(1d0)),INTENT(IN)::x
    li3=Nielsen_PolyLog(2,1,x)
    RETURN
  END FUNCTION li3

  FUNCTION RLi3P(a)
    USE Func_PSI
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::RLi3P
    REAL(KIND(1d0)),INTENT(IN)::a
    REAL(KIND(1d0))::b
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    COMPLEX(KIND(1d0))::loga
    IF(a.LE.1d0)THEN
       RLi3P=dcmplx(li3(a),0d0)
    ELSE
       b=1d0/a
       loga=RLOGP(a)
       RLi3P=li3(b)-1d0/6d0*loga**3&
            +pipi*RSQRTM(-(a-1d0)**2)/(2d0*(1d0-a))*loga**2&
            +2d0*zeta2*loga
    ENDIF
    RETURN
  END FUNCTION RLi3P

  FUNCTION RLi3M(a)
    USE Func_PSI
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::RLi3M
    REAL(KIND(1d0)),INTENT(IN)::a
    REAL(KIND(1d0))::b
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    COMPLEX(KIND(1d0))::loga
    IF(a.LE.1d0)THEN
       RLi3M=dcmplx(li3(a),0d0)
    ELSE
       b=1d0/a
       loga=RLOGM(a)
       RLi3M=li3(b)-1d0/6d0*loga**3&
            +pipi*RSQRTP(-(a-1d0)**2)/(2d0*(1d0-a))*loga**2&
            +2d0*zeta2*loga
    ENDIF
    RETURN
  END FUNCTION RLi3M

  FUNCTION li2_S11(x)
    IMPLICIT NONE
    REAL(KIND(1d0))::li2_S11
    REAL(KIND(1d0)),INTENT(IN)::x
    li2_S11=Nielsen_PolyLog(1,1,x)
    RETURN
  END FUNCTION li2_S11

END MODULE nielsen_generalized_polylog_wrapper
