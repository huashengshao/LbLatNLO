MODULE kinetics
  IMPLICIT NONE
CONTAINS
  double precision function sumdot2(p1,p2,coef)
    implicit none
    double precision p1(0:3),p2(0:3)
    double precision psum(0:3)
    double precision coef
    integer i
    do i=0,3
       psum(i)=p1(i)+coef*p2(i)
    enddo
    sumdot2=psum(0)**2
    do i=1,3
       sumdot2=sumdot2-psum(i)**2
    enddo
  end function sumdot2

  double complex function sqrt1(xs)
    implicit none
    double precision xs
    sqrt1=sqrt(dcmplx(xs*(xs-4d0),0d0))
    return
  end function sqrt1

  double complex function sqrt2(xt)
    implicit none
    double precision xt
    sqrt2=sqrt(dcmplx(xt*(xt-4d0),0d0))
    return
  end function sqrt2

  double complex function sqrt3(xs,xt)
    implicit none
    double precision xs,xt
    sqrt3=sqrt(dcmplx(xs*xt*(xs*xt-4d0*xs-4d0*xt),0d0))
    return
  end function sqrt3

  double complex function sqrt4(xs,xt)
    implicit none
    double precision xs,xt
    sqrt4=sqrt(dcmplx(xs*(xs*(xt-1d0)**2-4d0*xt**2),0d0))
    return
  end function sqrt4

  complex*32 function sqrt1_QP(xs)
    implicit none
    real*16 xs
    sqrt1_QP=sqrt(cmplx(xs*(xs-4E0_16),0E0_16,kind=16))
    return
  end function sqrt1_QP

  complex*32 function sqrt2_QP(xt)
    implicit none
    real*16 xt
    sqrt2_QP=sqrt(cmplx(xt*(xt-4E0_16),0E0_16,kind=16))
    return
  end function sqrt2_QP

  complex*32 function sqrt3_QP(xs,xt)
    implicit none
    real*16 xs,xt
    sqrt3_QP=sqrt(cmplx(xs*xt*(xs*xt-4E0_16*xs-4E0_16*xt),0E0_16,kind=16))
    return
  end function sqrt3_QP

  complex*32 function sqrt4_QP(xs,xt)
    implicit none
    real*16 xs,xt
    sqrt4_QP=sqrt(cmplx(xs*(xs*(xt-1E0_16)**2-4E0_16*xt**2),0E0_16,kind=16))
    return
  end function sqrt4_QP

  FUNCTION prapidity(p)
    ! psudo-rapidity
    REAL(KIND(1d0)),DIMENSION(0:3)::p
    REAL(KIND(1d0))::prapidity,c
    IF(p(1)**2+p(2)**2+p(3)**2.LE.0d0)THEN
       prapidity = 0d0
       RETURN
    ENDIF
    c=p(3)/SQRT(p(1)**2+p(2)**2+p(3)**2)
    IF(ABS(c).EQ.1d0)THEN
       prapidity = 0d0
    ELSE
       prapidity=0.5d0*LOG((1D0+c)/(1D0-c))
    ENDIF
  END FUNCTION prapidity

  FUNCTION rapidity(p)
    ! rapidity
    REAL(KIND(1d0)),DIMENSION(0:3)::p
    REAL(KIND(1d0))::rapidity,c
    IF(p(0).EQ.0d0)THEN
       rapidity = 0d0
       RETURN
    ENDIF
    c=p(3)/ABS(p(0))
    IF(ABS(c).GE.1d0)THEN
       rapidity =0d0
    ELSE
       rapidity=0.5d0*DLOG((1d0+c)/(1d0-c))
    ENDIF
  END FUNCTION rapidity

  FUNCTION transverse(p)
    REAL(KIND(1d0)),DIMENSION(0:3)::p
    REAL(KIND(1d0))::transverse
    transverse=SQRT(p(1)**2+p(2)**2)
  END FUNCTION transverse

  FUNCTION getrapidity(en,pl)
    IMPLICIT NONE
    REAL(KIND(1d0))::getrapidity
    REAL(KIND(1d0)),INTENT(IN)::en,pl
    REAL(KIND(1d0)),PARAMETER::tiny=1.d-8
    REAL(KIND(1d0))::xplus,xminus,y
    xplus=en+pl
    xminus=en-pl
    IF(xplus.GT.tiny.AND.xminus.GT.tiny)THEN
       IF( (xplus/xminus).GT.tiny.AND.(xminus/xplus).GT.tiny)THEN
          y=0.5d0*LOG( xplus/xminus  )
       ELSE
          y=SIGN(1.d0,pl)*1.d8
       ENDIF
    ELSE
       y=SIGN(1.d0,pl)*1.d8
    ENDIF
    getrapidity=y
    RETURN
  END FUNCTION getrapidity

  SUBROUTINE BOOST(Q,PBOO,PCM,PLB)
    ! momentums are in normal representation with fourth comp is the zero comp
    ! Boost PCM via PBOO(PBOO^2=Q^2) to PLB
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::Q
    REAL(KIND(1d0)),DIMENSION(0:3),INTENT(IN)::PBOO,PCM
    REAL(KIND(1d0)),DIMENSION(0:3),INTENT(OUT)::PLB
    REAL(KIND(1d0))::FACT
    INTEGER::J
    PLB(0)=(PBOO(0)*PCM(0)+PBOO(3)*PCM(3)+PBOO(2)*PCM(2)+PBOO(1)*PCM(1))/Q
    FACT=(PLB(0)+PCM(0))/(Q+PBOO(0))
    DO J=1,3
       PLB(J)=PCM(J)+FACT*PBOO(J)
    ENDDO
  END SUBROUTINE BOOST

  SUBROUTINE BOOSTL(Q,PBOO,P)
    ! momentums are in normal representation with fourth comp is the zero comp
    ! Boost P via PBOO(PBOO^2=Q^2) to PLB,and set to P
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::Q
    REAL(KIND(1d0)),DIMENSION(0:3),INTENT(IN)::PBOO
    REAL(KIND(1d0)),DIMENSION(0:3),INTENT(INOUT)::P
    REAL(KIND(1d0)),DIMENSION(0:3)::PCM,PLB
    REAL(KIND(1d0))::FACT
    INTEGER::J
    PCM(0:3)=P(0:3)
    PLB(0)=(PBOO(0)*PCM(0)+PBOO(3)*PCM(3)+PBOO(2)*PCM(2)+PBOO(1)*PCM(1))/Q
    FACT=(PLB(0)+PCM(0))/(Q+PBOO(0))
    DO J=1,3
       PLB(J)=PCM(J)+FACT*PBOO(J)
    ENDDO
    P(0:3)=PLB(0:3)
  END SUBROUTINE BOOSTL

  SUBROUTINE ROTATEL(PROT,P)
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(3),INTENT(IN)::PROT
    REAL(KIND(1d0)),DIMENSION(0:3),INTENT(INOUT)::P
    REAL(KIND(1d0)),DIMENSION(3)::PTMP
    REAL(KIND(1d0))::costh,sinth,cosphi,sinphi
    REAL(KIND(1d0))::pp,pt
    pp=PROT(1)**2+PROT(2)**2+PROT(3)**2
    IF(pp.LT.0d0)RETURN
    pp=DSQRT(pp)
    costh=PROT(3)/pp
    sinth=DSQRT(1d0-costh**2)
    pt=PROT(1)**2+PROT(2)**2
    pt=DSQRT(pt)
    IF(pt.EQ.0d0)THEN
       cosphi=1d0
       sinphi=0d0
    ELSE
       cosphi=PROT(1)/pt
       sinphi=PROT(2)/pt
    ENDIF
    PTMP(1:3)=P(1:3)
    ! rotate via y aix by th
    PTMP(1)=costh*P(1)+sinth*P(3)
    PTMP(3)=-sinth*P(1)+costh*P(3)
    P(1:3)=PTMP(1:3)
    ! rotate via z aix by phi
    PTMP(1)=cosphi*P(1)-sinphi*P(2)
    PTMP(2)=sinphi*P(1)+cosphi*P(2)
    P(1:3)=PTMP(1:3)
    RETURN
  END SUBROUTINE ROTATEL

  FUNCTION ph4(px,py,pz)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::px,py,pz
    REAL(KIND(1d0))::ph4,S2,S
    INTEGER::init=0
    REAL(KIND(1d0)),PARAMETER::pi=3.14159265358979323846264338328d0
    SAVE init
    IF(init.EQ.0)THEN
       init=1
    ENDIF
    IF(px**2+py**2.LE.0d0)THEN
       IF(pz.GE.0d0)ph4=0d0
       IF(pz.LT.0)ph4=pi
       RETURN
    ENDIF
    S2=px**2/(px**2+py**2)
    S=DSQRT(S2)
    IF(S.GT.1d0)THEN
       WRITE(*,*)'PH4(X) WARNING S=',S
       ph4=0
       IF(px.LT.0d0)ph4=pi
       RETURN
    ENDIF
    IF(px.LT.0)S=-DSQRT(S2)
    ph4=DACOS(S)
    IF(py.LT.0)ph4=2*pi-ph4
    RETURN
  END FUNCTION ph4
END MODULE kinetics
