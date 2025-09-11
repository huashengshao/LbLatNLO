MODULE GPL_wrapper
  ! this is a wrapper for evaluating GPLs
  IMPLICIT NONE
  include 'GPL_tools.inc'
  ! GPLs_tool: 1 (FastGPL), 2 (handyG)
  ! Now, I force FastGPL for double precision
  ! and handyG for quadruple precision
  integer::GPLs_tool=1
  integer::GPLs_qp_tool=2
  INTERFACE GPL
     MODULE PROCEDURE GPL_rx
     MODULE PROCEDURE GPL_cx
  END INTERFACE
  INTERFACE GPL_QP
     MODULE PROCEDURE GPL_rx_QP
     MODULE PROCEDURE GPL_cx_QP
  END INTERFACE
CONTAINS
  FUNCTION GPL_rx(a,s,x,ix,itool,clearcacheQ)
    !USE handyG
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::GPL_rx
    COMPLEX(KIND(1d0)),INTENT(IN),DIMENSION(:)::a
    INTEGER,INTENT(IN),DIMENSION(SIZE(a))::s
    REAL(KIND(1d0)),INTENT(IN)::x
    ! ix = +-1 for the sign of imagnary part of x, i.e., x+i*ix*0
    INTEGER,INTENT(IN),OPTIONAL::ix
    ! itool: 1 (FastGPL, default), 2 (handyG)
    INTEGER,INTENT(IN),OPTIONAL::itool
    INTEGER::itool_used,ix_used
    COMPLEX(KIND(1d0)),DIMENSION(SIZE(a))::a2
    INTEGER,DIMENSION(SIZE(a))::s2
    REAL(KIND(1d0))::x2
    REAL(KIND(1d0)),PARAMETER::pi=3.14159265358979323846264338328d0
    LOGICAL,INTENT(IN),OPTIONAL::clearcacheQ
    LOGICAL::clearcache_used
    LOGICAL::use_eq73,largethanone
    COMPLEX(KIND(1d0)),EXTERNAL::fastgpl_g
    !TYPE(inum),DIMENSION(SIZE(a))::weights
    INTEGER::na,I
    na=SIZE(a)
    IF(PRESENT(ix))THEN
       ix_used=ix
    ELSE
       ix_used=1
    ENDIF
    IF(PRESENT(itool))THEN
       itool_used=itool
    ELSE
       itool_used=1
    ENDIF
    IF(PRESENT(clearcacheQ))THEN
       clearcache_used=clearcacheQ
    ELSE
       clearcache_used=.FALSE.
    ENDIF
    IF(itool_used.NE.1)THEN
       WRITE(*,*)"ERROR: only 1 (FastGPL) is available in DP"
       STOP
    ENDIF
    IF(itool_used.EQ.1.AND..NOT.FastGPL_tool)THEN
       WRITE(*,*)"ERROR: FastGPL has been disabled"
       STOP
    ENDIF
    ! use FastGPL (2112.04122)
    ! keep in mind:
    ! For best performance and precision, it is recommended 
    ! to analytically transform the expressions to
    ! * Remove trailing zeros;
    ! * Normalize the argument to x=1;
    ! * Set Im(ai) to zero and set si appropriately 
    !   if Im(ai) comes from i*eps prescription
    if(all(abs(a(1:na)).eq.0d0))then
       ! calculate G(0,...,0;x)
       if(x.EQ.0d0)then
          print *, "error: encountering G(0,...,0;0)"
          stop
       elseif(x.gt.0d0)then
          GPL_rx=1d0/factorial(na)*dcmplx(DLOG(x)**na,0d0)
       else                
          GPL_rx=1d0/factorial(na)*dcmplx(DLOG(-x),ix_used*pi)**na
       endif
       return
    endif
    ! special G's that yields segmentation fault
    ! use eq.(73) in hep-ph/0410259 [Hoelder convolution with p=1/2]
    if(na.ge.2.and.x.eq.1d0.and.abs(a(1)-dcmplx(1d0,0d0)).gt.1d-8)then
       use_eq73=.FALSE.
       largethanone=.FALSE.
       do i=1,na
          if(.not.use_eq73.and.abs(abs(a(i))-1d0).lt.1d-10.and.dimag(a(i)).ne.0d0)then
             use_eq73=.TRUE.
          elseif(abs(a(i)).gt.0d0)then
             largethanone=.TRUE.
          elseif(use_eq73.and.largethanone)then
             exit
          endif
       enddo
       if(use_eq73.and.largethanone)then
          GPL_rx=GPL_Hoelder(a,s,1)
          return
       endif
    endif
    if(x.LT.0d0)then
       DO i=1,na
          a2(i)=-a(i)
          s2(i)=-s(i)
       ENDDO
       x2=-x
       ! FastGPL only allows positive real x
       GPL_rx=fastgpl_g(na,a2,s2,x2,clearcache_used)
    else
       GPL_rx=fastgpl_g(na,a,s,x,clearcache_used)
    endif
    RETURN
  END FUNCTION GPL_RX

  FUNCTION GPL_rx_QP(a0,s,x,ix,itool,clearcacheQ)
    USE handyG
    IMPLICIT NONE
    COMPLEX(KIND(1E0_16))::GPL_rx_QP
    COMPLEX(KIND(1E0_16)),INTENT(IN),DIMENSION(:)::a0
    INTEGER,INTENT(IN),DIMENSION(SIZE(a0))::s
    REAL(KIND(1E0_16)),INTENT(IN)::x
    ! ix = +-1 for the sign of imagnary part of x, i.e., x+i*ix*0
    INTEGER,INTENT(IN),OPTIONAL::ix
    ! itool: 1 (FastGPL, default), 2 (handyG)
    INTEGER,INTENT(IN),OPTIONAL::itool
    INTEGER::itool_used,ix_used
    COMPLEX(KIND(1E0_16)),DIMENSION(SIZE(a0))::a
    REAL(KIND(1E0_16))::x2
    REAL(KIND(1E0_16)),PARAMETER::pi=3.1415926535897932384626433832795028841971693993751E0_16
    LOGICAL,INTENT(IN),OPTIONAL::clearcacheQ
    LOGICAL::clearcache_used
    LOGICAL::use_eq73,largethanone
    COMPLEX(KIND(1E0_16)),EXTERNAL::fastgpl_g
    TYPE(inum),DIMENSION(SIZE(a))::weights
    INTEGER::na,I
    na=SIZE(a0)
    a(1:na)=a0(1:na)
    IF(PRESENT(ix))THEN
       ix_used=ix
    ELSE
       ix_used=1
    ENDIF
    IF(PRESENT(itool))THEN
       itool_used=itool
    ELSE
       itool_used=2
    ENDIF
    IF(PRESENT(clearcacheQ))THEN
       clearcache_used=clearcacheQ
    ELSE
       clearcache_used=.FALSE.
    ENDIF
    IF(itool_used.NE.2)THEN
       WRITE(*,*)"ERROR: only 2 (handyG) are available for QP"
       STOP
    ENDIF
    IF(itool_used.EQ.2.AND..NOT.HandyG_tool)THEN
       WRITE(*,*)"ERROR: HandyG (quadruple precision) has been disabled"
       STOP
    ENDIF
    IF(itool_used.EQ.1)THEN
       ! use FastGPL (2112.04122)
       ! keep in mind:
       ! For best performance and precision, it is recommended 
       ! to analytically transform the expressions to
       ! * Remove trailing zeros;
       ! * Normalize the argument to x=1;
       ! * Set Im(ai) to zero and set si appropriately 
       !   if Im(ai) comes from i*eps prescription
       WRITE(*,*)"ERROR: FastGPL does not provide QP"
       STOP
    ELSE
       ! use handyG (1909.01656)

       ! special G's that yields segmentation fault
       ! to avoid segmentation fault
       ! only happen in quadruple precision (not in double precision)
       ! it happens that |a(I)| is slightly below 1
       DO I=1,na
          if(1E0_16-abs(a(I)).LT.1E-20_16.and.abs(a(I)).LT.1E0_16)then
             if(real(a(I),kind=16).ge.0E0_16)then
                a(I)=cmplx(sqrt(1E0_16-imagpart(a(I))**2),imagpart(a(I)),kind=16)
             else
                a(I)=cmplx(-sqrt(1E0_16-imagpart(a(I))**2),imagpart(a(I)),kind=16)
             endif
          endif
       ENDDO

       ! special G's that yields segmentation fault
       ! use eq.(73) in hep-ph/0410259 [Hoelder convolution with p=1/2]
       if(na.ge.2.and.x.eq.1E0_16.and.abs(a(1)-cmplx(1E0_16,0E0_16,kind=16)).gt.1E-10_16)then
         use_eq73=.FALSE.
         largethanone=.FALSE.
         do i=1,na
            if(.not.use_eq73.and.abs(abs(a(i))-1E0_16).lt.1E-12_16.and.imagpart(a(i)).ne.0E0_16)then
              use_eq73=.TRUE.
            elseif(abs(a(i)).gt.0E0_16)then
              largethanone=.TRUE.
            elseif(use_eq73.and.largethanone)then
              exit
            endif
         enddo
         if(use_eq73.and.largethanone)then
            GPL_rx_QP=GPL_Hoelder_QP(a,s,2)
            return
         endif
       endif
       IF(clearcache_used)CALL clearcache
       DO I=1,na
          weights(I)=TOINUM(a(I),int(s(I),1))
       ENDDO
       GPL_rx_QP=G(weights,inum(x,ix_used*di0))
    ENDIF
    RETURN
  END FUNCTION GPL_RX_QP

  FUNCTION GPL_CX(a,s,x,ix,itool,clearcacheQ)
    !USE handyG
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::GPL_CX
    COMPLEX(KIND(1d0)),INTENT(IN),DIMENSION(:)::a
    INTEGER,INTENT(IN),DIMENSION(SIZE(a))::s
    COMPLEX(KIND(1d0)),INTENT(IN)::x
    ! ix = +-1 for the sign of imagnary part of x, i.e., x+i*ix*0
    INTEGER,INTENT(IN),OPTIONAL::ix
    ! itool: 1 (FastGPL, default), 2 (handyG)
    INTEGER,INTENT(IN),OPTIONAL::itool
    INTEGER::itool_used,ix_used
    LOGICAL,INTENT(IN),OPTIONAL::clearcacheQ
    LOGICAL::clearcache_used
    LOGICAL::use_eq73,largethanone
    COMPLEX(KIND(1d0)),EXTERNAL::fastgpl_g
    !TYPE(inum),DIMENSION(SIZE(a))::weights
    INTEGER::na,I
    REAL(KIND(1d0))::xx,xx2
    REAL(KIND(1d0)),PARAMETER::pi=3.14159265358979323846264338328d0
    COMPLEX(KIND(1d0)),DIMENSION(SIZE(a))::a2
    INTEGER,DIMENSION(SIZE(a))::s2
    na=SIZE(a)
    IF(PRESENT(ix))THEN
       ix_used=ix
    ELSE
       ix_used=1
    ENDIF
    IF(PRESENT(itool))THEN
       itool_used=itool
    ELSE
       itool_used=1
    ENDIF
    IF(PRESENT(clearcacheQ))THEN
       clearcache_used=clearcacheQ
    ELSE
       clearcache_used=.FALSE.
    ENDIF
    IF(itool_used.NE.1)THEN
       WRITE(*,*)"ERROR: only 1 (FastGPL) is available in DP"
       STOP
    ENDIF
    IF(itool_used.EQ.1.AND..NOT.FastGPL_tool)THEN
       WRITE(*,*)"ERROR: FastGPL has been disabled"
       STOP
    ENDIF
    ! use FastGPL (2112.04122)
    ! keep in mind:
    ! For best performance and precision, it is recommended
    ! to analytically transform the expressions to
    ! * Remove trailing zeros;
    ! * Normalize the argument to x=1;
    ! * Set Im(ai) to zero and set si appropriately
    !   if Im(ai) comes from i*eps prescription
    if(all(abs(a(1:na)).eq.0d0))then
       ! calculate G(0,...,0;x)
       if(abs(x).EQ.0d0)then
          print *, "error: encountering G(0,...,0;0)"
          stop
       elseif(dimag(x).eq.0d0)then
          xx=dreal(x)
          if(xx.gt.0d0)then
             GPL_cx=1d0/factorial(na)*dcmplx(DLOG(xx)**na,0d0)
          else
             GPL_cx=1d0/factorial(na)*dcmplx(DLOG(-xx),ix_used*pi)**na
          endif
       else
          GPL_cx=1d0/factorial(na)*CDLOG(x)**na
       endif
       return
    endif
    ! special G's that yields segmentation fault
    ! use eq.(73) in hep-ph/0410259 [Hoelder convolution with p=1/2]
    if(na.ge.2.and.x.eq.dcmplx(1d0,0d0).and.abs(a(1)-dcmplx(1d0,0d0)).gt.1d-8)then
       use_eq73=.FALSE.
       largethanone=.FALSE.
       do i=1,na
          if(.not.use_eq73.and.abs(abs(a(i))-1d0).lt.1d-10.and.dimag(a(i)).ne.0d0)then
             use_eq73=.TRUE.
          elseif(abs(a(i)).gt.0d0)then
             largethanone=.TRUE.
          elseif(use_eq73.and.largethanone)then
             exit
          endif
       enddo
       if(use_eq73.and.largethanone)then
          GPL_cx=GPL_Hoelder(a,s,1)
          return
       endif
    endif
    if(dimag(x).eq.0d0)then
       xx=dreal(x)
       !print *, "G arg=",a(1:na),x
       if(xx.LT.0d0)then
          DO i=1,na
             a2(i)=-a(i)
             s2(i)=-s(i)
          ENDDO
          xx2=-xx
          ! FastGPL only allows positive real x
          GPL_cx=fastgpl_g(na,a2,s2,xx2,clearcache_used)
       else
          GPL_cx=fastgpl_g(na,a,s,xx,clearcache_used)
       endif
       !print *, "GPL=",GPL_cx
    else
       WRITE(*,*)"Error: FastGPL does not support complex argument x"
       WRITE(*,*)"Error: please do the following first at the analytical level"
       WRITE(*,*)"Error: 1) Removing trailing zeros;"
       WRITE(*,*)"Error: 2) Normalize the argument to x=1;"
       WRITE(*,*)"Error: 3) if Im(ai) is zero, set si appropriately from i*eps prescription"
       STOP
    endif
    return
  END FUNCTION GPL_CX

  FUNCTION GPL_CX_QP(a0,s,x,ix,itool,clearcacheQ)
    USE handyG
    IMPLICIT NONE
    COMPLEX(KIND(1E0_16))::GPL_CX_QP
    COMPLEX(KIND(1E0_16)),INTENT(IN),DIMENSION(:)::a0
    INTEGER,INTENT(IN),DIMENSION(SIZE(a0))::s
    COMPLEX(KIND(1E0_16)),INTENT(IN)::x
    ! ix = +-1 for the sign of imagnary part of x, i.e., x+i*ix*0
    INTEGER,INTENT(IN),OPTIONAL::ix
    ! itool: 1 (FastGPL, default), 2 (handyG)
    INTEGER,INTENT(IN),OPTIONAL::itool
    INTEGER::itool_used,ix_used
    LOGICAL,INTENT(IN),OPTIONAL::clearcacheQ
    LOGICAL::clearcache_used
    LOGICAL::use_eq73,largethanone
    COMPLEX(KIND(1E0_16)),EXTERNAL::fastgpl_g
    TYPE(inum),DIMENSION(SIZE(a0))::weights
    INTEGER::na,I
    COMPLEX(KIND(1E0_16)),DIMENSION(SIZE(a0))::a
    REAL(KIND(1E0_16))::xx,xx2
    REAL(KIND(1E0_16)),PARAMETER::pi=3.1415926535897932384626433832795028841971693993751E0_16
    na=SIZE(a0)
    a(1:na)=a0(1:na)
    IF(PRESENT(ix))THEN
       ix_used=ix
    ELSE
       ix_used=1
    ENDIF
    IF(PRESENT(itool))THEN
       itool_used=itool
    ELSE
       itool_used=2
    ENDIF
    IF(PRESENT(clearcacheQ))THEN
       clearcache_used=clearcacheQ
    ELSE
       clearcache_used=.FALSE.
    ENDIF
    IF(itool_used.NE.2)THEN
       WRITE(*,*)"ERROR: only 2 (handyG) are available for QP"
       STOP
    ENDIF
    IF(itool_used.EQ.2.AND..NOT.HandyG_tool)THEN
       WRITE(*,*)"ERROR: HandyG (quadruple precision) has been disabled"
       STOP
    ENDIF
    IF(itool_used.EQ.1)THEN
       ! use FastGPL (2112.04122)
       ! keep in mind:
       ! For best performance and precision, it is recommended
       ! to analytically transform the expressions to
       ! * Remove trailing zeros;
       ! * Normalize the argument to x=1;
       ! * Set Im(ai) to zero and set si appropriately
       !   if Im(ai) comes from i*eps prescription
       WRITE(*,*)"ERROR: FastGPL does not provide QP"
       STOP
    else
       ! use handyG (1909.01656)

       ! special G's that yields segmentation fault
       ! to avoid segmentation fault
       ! only happen in quadruple precision (not in double precision)
       ! it happens that |a(I)| is slightly below 1
       DO I=1,na
          if(1E0_16-abs(a(I)).LT.1E-20_16.and.abs(a(I)).LT.1E0_16)then
             if(real(a(I),kind=16).ge.0E0_16)then
                a(I)=cmplx(sqrt(1E0_16-imagpart(a(I))**2),imagpart(a(I)),kind=16)
             else
                a(I)=cmplx(-sqrt(1E0_16-imagpart(a(I))**2),imagpart(a(I)),kind=16)
             endif
          endif
       ENDDO

       ! use eq.(73) in hep-ph/0410259 [Hoelder convolution with p=1/2] 
       if(na.ge.2.and.x.eq.cmplx(1E0_16,0E0_16,kind=16).and.abs(a(1)-cmplx(1E0_16,0E0_16,kind=16)).gt.1E-10_16)then
         use_eq73=.FALSE.
         largethanone=.FALSE.
         do i=1,na
            if(.not.use_eq73.and.abs(abs(a(i))-1E0_16).lt.1E-12_16.and.imagpart(a(i)).ne.0E0_16)then
              use_eq73=.TRUE.
            elseif(abs(a(i)).gt.0E0_16)then
              largethanone=.TRUE.
            elseif(use_eq73.and.largethanone)then
              exit
            endif
         enddo
         if(use_eq73.and.largethanone)then
            GPL_cx_QP=GPL_Hoelder_QP(a,s,2)
            return
         endif
       endif
       IF(clearcache_used)CALL clearcache
       DO I=1,na
          weights(I)=TOINUM(a(I),int(s(I),1))
       ENDDO
       GPL_CX_QP=G(weights,TOINUM(x,int(ix_used,1)))
    endif
    return
  END FUNCTION GPL_CX_QP

  ! use eq.(73) in hep-ph/0410259
  ! Hoelder convolution with p=1/2
  FUNCTION GPL_Hoelder(a,s,itool)
    !use handyG
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::GPL_Hoelder
    COMPLEX(KIND(1d0)),INTENT(IN),DIMENSION(:)::a
    INTEGER,INTENT(IN),DIMENSION(SIZE(a))::s
    INTEGER::na,i,j
    COMPLEX(KIND(1d0)),DIMENSION(SIZE(a))::zm
    INTEGER,DIMENSION(SIZE(a))::szm
    COMPLEX(KIND(1d0))::gg1,gg2
    COMPLEX(KIND(1d0)),EXTERNAL::fastgpl_g
    ! itool: 1 (FastGPL, default), 2 (handyG)
    INTEGER,INTENT(IN),OPTIONAL::itool
    INTEGER::itool_used
    !TYPE(inum),DIMENSION(SIZE(a))::weights
    na=SIZE(a)
    IF(PRESENT(itool))THEN
       itool_used=itool
    ELSE
       itool_used=1
    ENDIF
    IF(itool_used.EQ.1.AND..NOT.FastGPL_tool)THEN
       WRITE(*,*)"ERROR: FastGPL has been disabled"
       STOP
    ENDIF
    DO i=1,na
       zm(i)=2d0*a(i)
       szm(i)=s(i)
    ENDDO
    ! use FastGPL
    GPL_Hoelder=fastgpl_g(na,zm,szm,1d0,.FALSE.)
    DO i=1,na
       zm(i)=2d0*(1d0-a(na-i+1))
       szm(i)=-s(na-i+1)
    ENDDO
    ! use FastGPL
    GPL_Hoelder=GPL_Hoelder+(-1d0)**na*fastgpl_g(na,zm,szm,1d0,.FALSE.)
    DO j=1,na-1
       DO i=1,j
          zm(i)=2d0*(1d0-a(j-i+1))
          szm(i)=-s(j-i+1)
       enddo
       ! use FastGPL
       gg1=fastgpl_g(j,zm(1:j),szm(1:j),1d0,.FALSE.)
       do i=j+1,na
          zm(i)=2d0*a(i)
          szm(i)=s(i)
       enddo
       ! use FastGPL
       gg2=fastgpl_g(na-j,zm(j+1:na),szm(j+1:na),1d0,.FALSE.)
       GPL_Hoelder=GPL_Hoelder+(-1d0)**j*gg1*gg2
    ENDDO
    return
  END FUNCTION GPL_HOELDER

  ! use eq.(73) in hep-ph/0410259
  ! Hoelder convolution with p=1/2
  FUNCTION GPL_Hoelder_QP(a,s,itool)
    use handyG
    IMPLICIT NONE
    COMPLEX(KIND(1E0_16))::GPL_Hoelder_QP
    COMPLEX(KIND(1E0_16)),INTENT(IN),DIMENSION(:)::a
    INTEGER,INTENT(IN),DIMENSION(SIZE(a))::s
    INTEGER::na,i,j
    COMPLEX(KIND(1E0_16)),DIMENSION(SIZE(a))::zm
    INTEGER,DIMENSION(SIZE(a))::szm
    COMPLEX(KIND(1E0_16))::gg1,gg2
    COMPLEX(KIND(1E0_16)),EXTERNAL::fastgpl_g
    ! itool: 1 (FastGPL, default), 2 (handyG)
    INTEGER,INTENT(IN),OPTIONAL::itool
    INTEGER::itool_used
    TYPE(inum),DIMENSION(SIZE(a))::weights
    na=SIZE(a)
    IF(PRESENT(itool))THEN
       itool_used=itool
    ELSE
       itool_used=2
    ENDIF
    IF(itool_used.EQ.2.AND..NOT.HandyG_tool)THEN
       WRITE(*,*)"ERROR: HandyG (quadruple precision) has been disabled"
       STOP
    ENDIF
    DO i=1,na
       zm(i)=2E0_16*a(i)
       szm(i)=s(i)
    ENDDO
    if(itool_used.eq.1)then
       ! use FastGPL
       WRITE(*,*)"ERROR: FastGPL does not provide QP"
       STOP
    else
       ! use handyG
       DO I=1,na
          weights(I)=TOINUM(zm(I),int(szm(I),1))
       ENDDO
       GPL_Hoelder_QP=G(weights,TOINUM(1E0_16,int(1,1)))
    endif
    DO i=1,na
       zm(i)=2E0_16*(1E0_16-a(na-i+1))
       szm(i)=-s(na-i+1)
    ENDDO
    if(itool_used.eq.1)then
       ! use FastGPL
       WRITE(*,*)"ERROR: FastGPL does not provide QP"
       STOP
    else
       ! use handyG
       DO I=1,na
          weights(I)=TOINUM(zm(I),int(szm(I),1))
       ENDDO
       GPL_Hoelder_QP=GPL_Hoelder_QP+(-1E0_16)**na*G(weights,TOINUM(1E0_16,int(1,1)))
    endif
    DO j=1,na-1
       DO i=1,j
          zm(i)=2E0_16*(1E0_16-a(j-i+1))
          szm(i)=-s(j-i+1)
       enddo
       if(itool_used.eq.1)then
          ! use FastGPL
          WRITE(*,*)"ERROR: FastGPL does not provide QP"
          STOP
       else
          ! use handyG
          do i=1,j
             weights(i)=TOINUM(zm(i),int(szm(i),1))
          enddo
          gg1=G(weights(1:j),TOINUM(1E0_16,int(1,1)))
       endif
       do i=j+1,na
          zm(i)=2E0_16*a(i)
          szm(i)=s(i)
       enddo
       if(itool_used.eq.1)then
          ! use FastGPL
          WRITE(*,*)"ERROR: FastGPL does not provide QP"
          STOP
       else
          ! use handyG
          do i=j+1,na
             weights(i)=TOINUM(zm(i),int(szm(i),1))
          enddo
          gg2=G(weights(j+1:na),TOINUM(1E0_16,int(1,1)))
       endif
       GPL_Hoelder_QP=GPL_Hoelder_QP+(-1E0_16)**j*gg1*gg2
    ENDDO
    return
  END FUNCTION GPL_HOELDER_QP

  ! This gives us n! (n factorial)
  FUNCTION factorial(n)
    integer,intent(in)::n
    real(kind(1d0))::factorial
    integer::i
    if (n.lt.0) stop 'factorial is singular for negative integers'
    factorial = 1d0
    if(n.le.1)return
    if(n.eq.2)then
       factorial=2d0
       return
    elseif(n.eq.3)then
       factorial=6d0
       return
    elseif(n.eq.4)then
       factorial=24d0
       return
    elseif(n.eq.5)then
       factorial=120d0
       return
    elseif(n.eq.6)then
       factorial=720d0
       return
    elseif(n.eq.7)then
       factorial=5040d0
       return
    elseif(n.eq.8)then
       factorial=40320d0
       return
    elseif(n.eq.9)then
       factorial=362880d0
       return
    elseif(n.ge.10)then
       factorial=3628800d0
       if(n.gt.10)then
          do i=11,n
             factorial=factorial*i
          enddo
       endif
    endif
    return
  end FUNCTION factorial
END MODULE GPL_wrapper
