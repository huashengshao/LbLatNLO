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

  ! it is initially from https://github.com/Expander/polylogarithm
  !> @author Alexander Voigt

  ! Li3(z) with complex argument z
  FUNCTION cdli3(z)
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::cdli3
    COMPLEX(KIND(1d0)),INTENT(IN)::z
    COMPLEX(KIND(1d0))::u,u2,u4,u8,c0,c1,lmz,rest
    REAL(KIND(1d0))::rz,iz,nz,pz,lnz,arg
    REAL(KIND(1d0)),PARAMETER::PI=3.1415926535897932D0
    REAL(KIND(1d0)),PARAMETER::zeta2=1.6449340668482264D0
    REAL(KIND(1d0)),PARAMETER::zeta3=1.2020569031595943D0
    REAL(KIND(1d0)),PARAMETER::bf(18) = (/           &
         1.0D0                 , -3.0D0/8.0D0           , &
         17.0D0/216.0D0        , -5.0D0/576.0D0         , &
         1.2962962962962963D-04,  8.1018518518518519D-05, &
         -3.4193571608537595D-06, -1.3286564625850340D-06, &
         8.6608717561098513D-08,  2.5260875955320400D-08, &
         -2.1446944683640648D-09, -5.1401106220129789D-10, &
         5.2495821146008294D-11,  1.0887754406636318D-11, &
         -1.2779396094493695D-12, -2.3698241773087452D-13, &
         3.1043578879654623D-14,  5.2617586299125061D-15  /)
    REAL(KIND(1d0)),PARAMETER::cs(7) = (/            &
         -3.4722222222222222D-03,  1.1574074074074074D-05, &
         -9.8418997228521038D-08,  1.1482216343327454D-09, &
         -1.5815724990809166D-11,  2.4195009792525152D-13, &
         -3.9828977769894877D-15                           /)
    rz=real(z)
    iz=aimag(z)

    if(iz.eq.0)then
       if(rz.le.1)then
          cdli3=dcmplx(dli3(rz),iz)
          return
       else
          lnz=log(rz)
          cdli3=dcmplx(dli3(rz),-0.5D0*PI*lnz**2)
          return
       endif
    endif
    nz=hypot(rz,iz)
    pz=datan2(iz,rz)
    lnz=log(nz)

    if(lnz**2+pz**2.lt.1)then ! |log(z)| < 1
       u=dcmplx(lnz,pz) ! log(z)
       u2=u**2
       u4=u2**2
       u8=u4**2
       c0=zeta3+u*(zeta2-u2/12)
       c1=0.25D0*(3-2*pos_cdlog(-u))
       cdli3 =                                              &
            c0 +                                            &
            c1*u2 +                                         &
            u4*(cs(1) + u2*cs(2)) +                         &
            u8*(cs(3) + u2*cs(4) + u4*(cs(5) + u2*cs(6))) + &
            u8*u8*cs(7)
       return
    endif
    if(nz.le.1)then
       u=-pos_cdlog(1-z)
       rest=0
    else ! nz > 1
       if(pz.gt.0)then
          arg=pz-PI
       else
          arg=pz+PI
       endif
       lmz=dcmplx(lnz,arg) ! log(-z)
       u=-pos_cdlog(1-1/z)
       rest=-lmz*(lmz**2/6+zeta2)
    endif

    u2 = u**2
    u4 = u2**2
    u8 = u4**2

    cdli3 =                                                 &
         rest +                                             &
         u*bf(1) +                                          &
         u2*(bf(2) + u*bf(3)) +                             &
         u4*(bf(4) + u*bf(5) + u2*(bf(6) + u*bf(7))) +      &
         u8*(bf(8) + u*bf(9) + u2*(bf(10) + u*bf(11)) +     &
         u4*(bf(12) + u*bf(13) + u2*(bf(14) + u*bf(15)))) + &
         u8*u8*(bf(16) + u*bf(17) + u2*bf(18))

    return
  END FUNCTION cdli3

  ! Li3(x) with real argument x
  FUNCTION dli3(x)
    IMPLICIT NONE
    REAL(KIND(1d0))::dli3
    REAL(KIND(1d0)),INTENT(IN)::x
    REAL(KIND(1d0))::l
    REAL(KIND(1d0)),PARAMETER::zeta2=1.6449340668482264D0
    REAL(KIND(1d0)),PARAMETER::zeta3=1.2020569031595943D0

    ! transformation to [-1,0] and [0,1/2]
    if(x.lt.-1)then
       l=log(-x)
       dli3=dli3_neg(1/x)-l*(zeta2+1.0D0/6*l**2)
    elseif(x.eq.-1)then
       dli3=-0.75D0*zeta3
    elseif(x.lt.0)then
       dli3=dli3_neg(x)
    elseif(x.eq.0)then
       dli3=x
    elseif(x.lt.0.5D0)then
       dli3=dli3_pos(x)
    elseif(x.eq.0.5D0)then
       dli3 = 0.53721319360804020D0
    elseif(x.lt.1)then
       l=log(x)
       dli3=-dli3_neg(1-1/x)-dli3_pos(1-x)+&
            zeta3+l*(zeta2+l*(-0.5D0*log(1-x)+1.0D0/6*l))
    elseif(x.eq.1)then
       dli3=zeta3
    elseif(x.lt.2)then
       l=log(x)
       dli3=-dli3_neg(1-x)-dli3_pos(1-1/x)+&
            zeta3+l*(zeta2+l*(-0.5D0*log(x-1)+1.0D0/6*l))
    else ! x >= 2.0D0
       l=log(x)
       dli3=dli3_pos(1/x)+l*(2*zeta2-1.0D0/6*l**2)
    endif
    return
  END FUNCTION dli3

  ! Li_3(x) for x in [-1,0]
  FUNCTION dli3_neg(x)
    IMPLICIT NONE
    REAL(KIND(1d0))::dli3_neg
    REAL(KIND(1d0)),INTENT(IN)::x
    REAL(KIND(1d0))::x2,x4,p,q
    REAL(KIND(1d0)),PARAMETER::cp(6) = (/                     &
         0.9999999999999999795D+0, -2.0281801754117129576D+0, &
         1.4364029887561718540D+0, -4.2240680435713030268D-1, &
         4.7296746450884096877D-2, -1.3453536579918419568D-3 /)
    REAL(KIND(1d0)),PARAMETER::cq(7) = (/                     &
         1.0000000000000000000D+0, -2.1531801754117049035D+0, &
         1.6685134736461140517D+0, -5.6684857464584544310D-1, &
         8.1999463370623961084D-2, -4.0756048502924149389D-3, &
         3.4316398489103212699D-5                            /)

    x2=x*x
    x4=x2*x2
    p=cp(1)+x*cp(2)+x2*(cp(3)+x*cp(4))+             &
         x4*(cp(5)+x*cp(6))
    q=cq(1)+x*cq(2)+x2*(cq(3)+x*cq(4))+             &
         x4*(cq(5)+x*cq(6)+x2*cq(7))
    
    dli3_neg=x*p/q
    return
  END FUNCTION dli3_neg

  ! Li_3(x) for x in [0,1/2]
  FUNCTION dli3_pos(x)
    IMPLICIT NONE
    REAL(KIND(1d0))::dli3_pos
    REAL(KIND(1d0)),INTENT(IN)::x
    REAL(KIND(1d0))::x2,x4,p,q
    REAL(KIND(1d0)),PARAMETER::cp(6) = (/                     &
         0.9999999999999999893D+0, -2.5224717303769789628D+0, &
         2.3204919140887894133D+0, -9.3980973288965037869D-1, &
         1.5728950200990509052D-1, -7.5485193983677071129D-3 /)
    REAL(KIND(1d0)),PARAMETER::cq(7) = (/                     &
         1.0000000000000000000D+0, -2.6474717303769836244D+0, &
         2.6143888433492184741D+0, -1.1841788297857667038D+0, &
         2.4184938524793651120D-1, -1.8220900115898156346D-2, &
         2.4927971540017376759D-4                            /)

    x2=x*x
    x4=x2*x2
    p=cp(1)+x*cp(2)+x2*(cp(3)+x*cp(4))+             &
         x4*(cp(5)+x*cp(6))
    q=cq(1)+x*cq(2)+x2*(cq(3)+x*cq(4))+             &
         x4*(cq(5)+x*cq(6)+x2*cq(7))
    
    dli3_pos=x*p/q
    
    return
  END FUNCTION dli3_pos

  !> @note Points on the branch cut are treated differently from log(z):
  !> Points with Im(z) == -0D0 are mapped to Im(z) == 0D0
  function pos_cdlog(z)
    implicit none
    complex(kind(1d0))::pos_cdlog
    complex(kind(1d0)),intent(in)::z
    real(kind(1d0))::re,im

    re=real(z)
    im=aimag(z)

    if(im.eq.0.and.re.gt.0)then
       pos_cdlog=dcmplx(log(re),0.0D0)
    elseif(im.eq.0)then
       pos_cdlog=dcmplx(log(-re),3.14159265358979324D0)
    else
       pos_cdlog=log(z)
    endif
    
    return
  end function pos_cdlog

  ! Li4(z) with complex argument z
  FUNCTION cdli4(z)
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::cdli4
    COMPLEX(KIND(1d0)),INTENT(IN)::z
    COMPLEX(KIND(1d0))::u,u2,u4,u8,c3,lmz,r
    REAL(KIND(1d0))::rz,iz,nz,pz,lnz,arg,sgn
    REAL(KIND(1d0)),PARAMETER::PI=3.1415926535897932D0
    REAL(KIND(1d0)),PARAMETER::PI2=9.8696044010893586D0
    REAL(KIND(1d0)),PARAMETER::PI4=97.409091034002437D0
    REAL(KIND(1d0)),PARAMETER::zeta4=1.0823232337111382D0
    REAL(KIND(1d0)),PARAMETER::c1=1.2020569031595943D0
    REAL(KIND(1d0)),PARAMETER::c2=0.82246703342411322D0
    REAL(KIND(1d0)),PARAMETER::c4=-1D0/48D0
    REAL(KIND(1d0)),PARAMETER::bf(18) = (/                &
         1.0D0                 , -7.0D0/16.0D0          , &
         1.1651234567901235D-01, -1.9820601851851852D-02, &
         1.9279320987654321D-03, -3.1057098765432099D-05, &
         -1.5624009114857835D-05,  8.4851235467732066D-07,&
         2.2909616603189711D-07, -2.1832614218526917D-08, &
         -3.8828248791720156D-09,  5.4462921032203321D-10,&
         6.9608052106827254D-11, -1.3375737686445215D-11, &
         -1.2784852685266572D-12,  3.2605628580248922D-13,&
         2.3647571168618257D-14, -7.9231351220311617D-15  /)
    REAL(KIND(1d0)),PARAMETER::cs(7) = (/                 &
         -6.9444444444444444D-04, 1.6534391534391534D-06, &
         -1.0935444136502338D-08, 1.0438378493934049D-10, &
         -1.2165942300622435D-12, 1.6130006528350101D-14, &
         -2.3428810452879340D-16                          /)
    
    rz=real(z)
    iz=aimag(z)
    
    if(iz.eq.0)then
       if(rz.le.1)then
          cdli4=dcmplx(dli4(rz),iz)
          return
       else
          lnz=log(rz)
          cdli4=dcmplx(dli4(rz),-1D0/6D0*PI*lnz**3)
          return
       endif
    endif
    
    nz=hypot(rz,iz)
    pz=datan2(iz,rz)
    lnz=log(nz)
    
    if(lnz**2+pz**2.lt.1)then ! |log(z)| < 1
       u=dcmplx(lnz,pz) ! log(z)
       u2=u**2
       u4=u2**2
       u8=u4**2
       c3=(11D0/6-pos_cdlog(-u))/6
       cdli4=zeta4+u2*(c2+u2*c4)+                           &
            u * (                                           &
            c1 +                                            &
            c3*u2 +                                         &
            u4*(cs(1) + u2*cs(2)) +                         &
            u8*(cs(3) + u2*cs(4) + u4*(cs(5) + u2*cs(6))) + &
            u8*u8*cs(7))
       return
    endif
    
    if(nz.le.1)then
       u=-pos_cdlog(1-z)
       r=0
       sgn=1
    else ! nz > 1
       if(pz.gt.0)then
          arg=pz-PI
       else
          arg=pz+PI
       endif
       lmz=dcmplx(lnz, arg) ! log(-z)
       u=-pos_cdlog(1-1/z)
       r=(-7*PI4+lmz**2*(-30*PI2-15*lmz**2))/360
       sgn=-1
    endif
    
    u2=u**2
    u4=u2**2
    u8=u4**2
   
    cdli4=                                                      &
         r + sgn * (                                            &
         u*bf(1) +                                              &
         u2*(bf(2) + u*bf(3)) +                                 &
         u4*(bf(4) + u*bf(5) + u2*(bf(6) + u*bf(7))) +          &
         u8*(bf(8) + u*bf(9) + u2*(bf(10) + u*bf(11)) +         &
         u4*(bf(12) + u*bf(13) + u2*(bf(14) + u*bf(15)))) +     &
         u8*u8*(bf(16) + u*bf(17) + u2*bf(18))                 )
    
    return
  END FUNCTION cdli4

  FUNCTION dli4(x)
    IMPLICIT NONE
    REAL(KIND(1d0))::dli4
    REAL(KIND(1d0)),INTENT(INOUT)::x
    REAL(KIND(1d0))::xorig
    REAL(KIND(1d0))::app,rest,sgn,l,l2
    REAL(KIND(1d0)),PARAMETER::zeta2=1.6449340668482264D0
    REAL(KIND(1d0)),PARAMETER::zeta4=1.0823232337111382D0

    xorig=x
    ! transform x to [-1,1]
    if(x.lt.-1)then
       l=log(-x)
       l2=l**2
       x=1/x
       rest=-7.0D0/4*zeta4+l2*(-0.5D0*zeta2-1.0D0/24*l2)
       sgn=-1
    elseif(x.eq.-1)then
       dli4=-7.0D0/8*zeta4
       return
    elseif(x.eq.0)then
       dli4=x
       return
    elseif(x.lt.1)then
       rest=0
       sgn=1
    elseif(x.eq.1)then
       dli4=zeta4
       return
    else ! x > 1
       l=log(x)
       l2=l**2
       x=1/x
       rest=2*zeta4+l2*(zeta2-1.0D0/24*l2)
       sgn=-1
    endif

    if(x.lt.0)then
       app=dli4_neg(x)
    elseif(x.lt.0.5D0)then
       app=dli4_half(x)
    elseif(x.lt.0.8D0)then
       app=dli4_mid(x)
    else ! x <= 1
       app=dli4_one(x)
    endif

    dli4 = rest + sgn*app

    ! recover the original x
    x=xorig

    return
  END FUNCTION dli4

  ! Li_4(x) for x in [8/10,1]
  function dli4_one(x)
    implicit none
    real(kind(1d0))::dli4_one
    real(kind(1d0)),intent(in)::x
    real(kind(1d0))::l,l2
    real(kind(1d0)),parameter::zeta2=1.6449340668482264D0
    real(kind(1d0)),parameter::zeta3=1.2020569031595943D0
    real(kind(1d0)),parameter::zeta4=1.0823232337111382D0

    l=log(x)
    l2=l**2

    dli4_one=zeta4+l*(zeta3+l*(0.5D0*zeta2+l*(11.0D0/36    &
         - 1.0D0/6*log(-l) + l*(-1.0D0/48 + l*(-1.0D0/1440 &
         + l2*(1.0D0/604800 - 1.0D0/91445760*l2))))))
    
    return
  end function dli4_one

  ! Li_4(x) for x in [1/2,8/10]
  function dli4_mid(x)
    implicit none
    real(kind(1d0))::dli4_mid
    real(kind(1d0)),intent(in)::x
    real(kind(1d0))::x2,x4,p,q
    real(kind(1d0)),parameter::cp(7) = (/                     &
         3.2009826406098890447D-9, 9.9999994634837574160D-1,  &
         -2.9144851228299341318D+0, 3.1891031447462342009D+0, &
         -1.6009125158511117090D+0, 3.5397747039432351193D-1, &
         -2.5230024124741454735D-2                          /)
    real(kind(1d0)),parameter::cq(7) = (/                     &
         1.0000000000000000000D+0, -2.9769855248411488460D+0, &
         3.3628208295110572579D+0, -1.7782471949702788393D+0, &
         4.3364007973198649921D-1, -3.9535592340362510549D-2, &
         5.7373431535336755591D-4                           /)

    x2=x*x
    x4=x2*x2
    p=cp(1)+x*cp(2)+x2*(cp(3)+x*cp(4))+             &
         x4*(cp(5)+x*cp(6)+x2*cp(7))
    q=cq(1)+x*cq(2)+x2*(cq(3)+x*cq(4))+             &
         x4*(cq(5)+x*cq(6)+x2*cq(7))
    
    dli4_mid=p/q

    return
  end function dli4_mid

  ! Li_4(x) for x in [0,1/2]
  function dli4_half(x)
    implicit none
    real(kind(1d0))::dli4_half
    real(kind(1d0)),intent(in)::x
    real(kind(1d0))::x2,x4,p,q
    real(kind(1d0)),parameter::cp(6) = (/                     &
         1.0000000000000000414D+0, -2.0588072418045364525D+0, &
         1.4713328756794826579D+0, -4.2608608613069811474D-1, &
         4.2975084278851543150D-2, -6.8314031819918920802D-4 /)
    real(kind(1d0)),parameter::cq(6) = (/                     &
         1.0000000000000000000D+0, -2.1213072418045207223D+0, &
         1.5915688992789175941D+0, -5.0327641401677265813D-1, &
         6.1467217495127095177D-2, -1.9061294280193280330D-3 /)

    x2=x*x
    x4=x2*x2
    p=cp(1)+x*cp(2)+x2*(cp(3)+x*cp(4))+    &
         x4*(cp(5)+x*cp(6))
    q=cq(1)+x*cq(2)+x2*(cq(3)+x*cq(4))+    &
         x4*(cq(5)+x*cq(6))
    
    dli4_half=x*p/q
    return
  end function dli4_half

  ! Li_4(x) for x in [-1,0]
  function dli4_neg(x)
    implicit none
    real(kind(1d0))::dli4_neg
    real(kind(1d0)),intent(in)::x
    real(kind(1d0))::x2,x4,p,q
    real(kind(1d0)),parameter::cp(6) = (/                     &
         0.9999999999999999952D+0, -1.8532099956062184217D+0, &
         1.1937642574034898249D+0, -3.1817912243893560382D-1, &
         3.2268284189261624841D-2, -8.3773570305913850724D-4 /)
    real(kind(1d0)),parameter::cq(7) = (/                     &
         1.0000000000000000000D+0, -1.9157099956062165688D+0, &
         1.3011504531166486419D+0, -3.7975653506939627186D-1, &
         4.5822723996558783670D-2, -1.8023912938765272341D-3, &
         1.0199621542882314929D-5                            /)

    x2=x*x
    x4=x2*x2
    p=cp(1)+x*cp(2)+x2*(cp(3)+x*cp(4))+             &
         x4*(cp(5)+x*cp(6))
    q=cq(1)+x*cq(2)+x2*(cq(3)+x*cq(4))+             &
         x4*(cq(5)+x*cq(6)+x2*cq(7))

    dli4_neg=x*p/q
    
    return
  end function dli4_neg

  ! Li5(z) with complex argument z
  function cdli5(z)
    implicit none
    complex(kind(1d0))::cdli5
    complex(kind(1d0)),intent(in)::z
    complex(kind(1d0))::u,u2,u4,u8,c4,lmz,rest
    real(kind(1d0))::rz,iz,nz,pz,lnz,arg
    real(kind(1d0)),parameter::PI=3.1415926535897932D0
    real(kind(1d0)),parameter::PI2=9.8696044010893586D0
    real(kind(1d0)),parameter::PI4=97.409091034002437D0
    real(kind(1d0)),parameter::zeta5=1.0369277551433699D0
    real(kind(1d0)),parameter::c1=1.0823232337111382D0 ! zeta(4)
    real(kind(1d0)),parameter::c2=0.60102845157979714D0 ! zeta(3)/2
    real(kind(1d0)),parameter::c3=0.27415567780803774D0
    real(kind(1d0)),parameter::c5=-1D0/240
    real(kind(1d0)),parameter::bf(19) = (/                &
         1.0D0                 , -15.0D0/32.0D0         , &
         1.3953189300411523D-01, -2.8633777006172840D-02, &
         4.0317412551440329D-03, -3.3985018004115226D-04, &
         4.5445184621617666D-06,  2.3916808048569012D-06, &
         -1.2762692600122747D-07, -3.1628984306505932D-08,&
         3.2848118445335192D-09,  4.7613713995660579D-10, &
         -8.0846898171909830D-11, -7.2387648587737207D-12,&
         1.9439760115173968D-12,  1.0256978405977236D-13, &
         -4.6180551009884830D-14, -1.1535857196470580D-15,&
         1.0903545401333394D-15                         /)
    real(kind(1d0)),parameter::cs(6) = (/                 &
         -1.1574074074074074D-04, 2.0667989417989418D-07, &
         -1.0935444136502338D-09, 8.6986487449450412D-12, &
         -8.6899587861588824D-14, 1.0081254080218813D-15/)

    rz=real(z)
    iz=aimag(z)

    if(iz.eq.0)then
       if(rz.eq.0)then
          cdli5=dcmplx(rz,iz)
          return
       endif
       if(rz.eq.1)then
          cdli5=dcmplx(zeta5,iz)
          return
       endif
       if(rz.eq.-1)then
          cdli5=dcmplx(-15*zeta5/16,iz)
          return
       endif
    endif

    nz=hypot(rz,iz)
    pz=datan2(iz,rz)
    lnz=log(nz)

    if(lnz**2+pz**2.lt.1)then ! |log(z)| < 1
       u=dcmplx(lnz,pz) ! log(z)
       u2=u**2
       c4=(25D0/12-pos_cdlog(-u))/24
       cdli5 =                  &
            zeta5 + u * c1 +    &
            u2 * (c2 + u * c3 + &
            u2 * (c4 + u * c5 + &
            u2 * (cs(1) +       &
            u2 * (cs(2) +       &
            u2 * (cs(3) +       &
            u2 * (cs(4) +       &
            u2 * (cs(5) +       &
            u2 * (cs(6)))))))))
       return
    endif

    if(nz.le.1)then
       u=-pos_cdlog(1-z)
       rest=0
    else ! nz > 1
       if(pz.gt.0)then
          arg=pz-PI
       else
          arg=pz+PI
       endif
       lmz=dcmplx(lnz,arg) ! log(-z)
       u=-pos_cdlog(1-1/z)
       rest=-lmz*(7*PI4+lmz**2*(10*PI2+3*lmz**2))/360
    endif

    u2 = u**2
    u4 = u2**2
    u8 = u4**2

    cdli5 =                                                 &
         rest +                                             &
         u*bf(1) +                                          &
         u2*(bf(2) + u*bf(3)) +                             &
         u4*(bf(4) + u*bf(5) + u2*(bf(6) + u*bf(7))) +      &
         u8*(bf(8) + u*bf(9) + u2*(bf(10) + u*bf(11)) +     &
         u4*(bf(12) + u*bf(13) + u2*(bf(14) + u*bf(15)))) + &
         u8*u8*(bf(16) + u*bf(17) + u2*(bf(18) + u*bf(19)))

    return
  end function cdli5

  ! Li6(z) with complex argument z
  function cdli6(z)
    implicit none
    complex(kind(1d0))::cdli6
    complex(kind(1d0)),intent(in)::z
    complex(kind(1d0))::u,u2,u4,u8,c5,lmz,r
    real(kind(1d0))::rz,iz,nz,pz,lnz,arg,sgn
    real(kind(1d0)),parameter::PI=3.1415926535897932D0
    real(kind(1d0)),parameter::PI2=9.8696044010893586D0
    real(kind(1d0)),parameter::PI4=97.409091034002437D0
    real(kind(1d0)),parameter::PI6=961.38919357530444D0
    real(kind(1d0)),parameter::zeta6=1.0173430619844491D0
    real(kind(1d0)),parameter::c1=1.0369277551433699D0 ! zeta(5)
    real(kind(1d0)),parameter::c2=0.54116161685556910D0
    real(kind(1d0)),parameter::c3=0.20034281719326571D0
    real(kind(1d0)),parameter::c4=0.068538919452009435D0
    real(kind(1d0)),parameter::c6=-1D0/1440
    real(kind(1d0)),parameter::bf(18) = (/                &
         1.0D0                 , -31.0D0/64.0D0         , &
         1.5241340877914952D-01, -3.4365555877057613D-02, &
         5.7174797239368999D-03, -6.8180453746570645D-04, &
         4.9960361948734493D-05, -4.9166051196039048D-07, &
         -3.0632975161302164D-07,  1.4414599270849095D-08,&
         3.7272438230924107D-09, -3.7300867345487607D-10, &
         -5.1246526816085832D-11,  9.0541930956636683D-12,&
         6.7381882615512517D-13, -2.1215831150303135D-13, &
         -6.8408811719011698D-15,  4.8691178462005581D-15/)
    real(kind(1d0)),parameter::cs(5) = (/                 &
         -1.6534391534391534D-05, 2.2964432686654909D-08, &
         -9.9413128513657614D-11, 6.6912682653423394D-13, &
         -5.7933058574392549D-15                         /)

    rz = real(z)
    iz = aimag(z)

    if(iz.eq.0)then
       if(rz.eq.0)then
          cdli6=dcmplx(rz,iz)
          return
       endif
       if(rz.eq.1)then
          cdli6=dcmplx(zeta6,iz)
          return
       endif
       if(rz.eq.-1)then
          cdli6=dcmplx(-31*zeta6/32,iz)
          return
       endif
    endif

    nz=hypot(rz,iz)
    pz=datan2(iz,rz)
    lnz=log(nz)

    if(lnz**2+pz**2.lt.1) then ! |log(z)| < 1
       u=dcmplx(lnz,pz) ! log(z)
       u2=u**2
       c5=(137D0/60-pos_cdlog(-u))/120
       cdli6 = zeta6 + u * c1 + &
            u2 * (c2 + u * c3 + &
            u2 * (c4 + u * c5 + &
            u2 * (c6 +          &
            u * (cs(1) +        &
            u2 * (cs(2) +       &
            u2 * (cs(3) +       &
            u2 * (cs(4) +       &
            u2 * (cs(5)))))))))
       return
    endif

    if(nz.le.1)then
       u=-pos_cdlog(1-z)
       r=0
       sgn=1
    else ! nz > 1
       if(pz.gt.0)then
          arg=pz-PI
       else
          arg=pz+PI
       endif
       lmz=dcmplx(lnz,arg) ! log(-z)
       u=-pos_cdlog(1-1/z)
       r=-31*PI6/15120+lmz**2*(-7*PI4/720+lmz**2*(-PI2/144-lmz**2/720))
       sgn=-1
    endif

    u2=u**2
    u4=u2**2
    u8=u4**2

    cdli6 =                                                 &
         r + sgn * (                                        &
         u*bf(1) +                                          &
         u2*(bf(2) + u*bf(3)) +                             &
         u4*(bf(4) + u*bf(5) + u2*(bf(6) + u*bf(7))) +      &
         u8*(bf(8) + u*bf(9) + u2*(bf(10) + u*bf(11)) +     &
         u4*(bf(12) + u*bf(13) + u2*(bf(14) + u*bf(15)))) + &
         u8*u8*(bf(16) + u*bf(17) + u2*bf(18)))

    return
  end function cdli6

  function cdli2(z)
    implicit none
    complex(kind(1d0))::cdli2
    complex(kind(1d0)),intent(in)::z
    complex(kind(1d0))::rest,u,u2,u4,sum
    real(kind(1d0))::rz,iz,nz,sgn
    real(kind(1d0)),parameter::PI=3.14159265358979324D0
    real(kind(1d0)),parameter::bf(10) = (/ &
         - 1.0D0/4.0D0,                    &
         + 1.0D0/36.0D0,                   &
         - 1.0D0/3600.0D0,                 &
         + 1.0D0/211680.0D0,               &
         - 1.0D0/10886400.0D0,             &
         + 1.0D0/526901760.0D0,            &
         - 4.0647616451442255D-11,         &
         + 8.9216910204564526D-13,         &
         - 1.9939295860721076D-14,         &
         + 4.5189800296199182D-16         /)

    rz = real(z)
    iz = aimag(z)

    ! special cases
    
    if(iz.eq.0)then
       if(rz.le.1)cdli2=dcmplx(dli2(rz),iz)
       if(rz.gt.1)cdli2=dcmplx(dli2(rz),-PI*log(rz))
       return
    endif

    nz=rz**2+iz**2

    if(nz.lt.EPSILON(1D0))then
       cdli2=z*(1+0.25D0*z)
       return
    endif

    ! transformation to |z| < 1, Re(z) <= 0.5
    if(rz.le.0.5D0)then
       if(nz.gt.1)then
          u=-cdlog1p(-1/z)
          rest=-0.5D0*log(-z)**2-PI**2/6
          sgn=-1
       else ! nz <= 1
          u=-cdlog1p(-z)
          rest=0
          sgn=1
       endif
    else ! rz > 0.5D0
       if(nz.le.2*rz)then
          u=-log(z)
          rest=u*cdlog1p(-z)+PI**2/6
          sgn=-1
       else ! nz > 2*rz
          u=-cdlog1p(-1/z)
          rest=-0.5D0*log(-z)**2-PI**2/6
          sgn=-1
       endif
    endif

    u2=u**2
    u4=u2**2
    sum=                                                     &
         u +                                                 &
         u2 * (bf(1) +                                       &
         u  * (bf(2) +                                       &
         u2 * (                                              &
         bf(3) +                                             &
         u2*bf(4) +                                          &
         u4*(bf(5) + u2*bf(6)) +                             &
         u4*u4*(bf(7) + u2*bf(8) + u4*(bf(9) + u2*bf(10))))))

    cdli2 = sgn*sum + rest
    
    return
  end function cdli2

  function dli2(x)
    implicit none
    real(kind(1d0))::dli2
    real(kind(1d0)),intent(in)::x
    real(kind(1d0))::y,r,s,y2,y4,p,q,l
    real(kind(1d0)),parameter::PI=3.14159265358979324D0
    real(kind(1d0)),parameter::cp(6) = (/ &
         0.9999999999999999502D+0,        &
         -2.6883926818565423430D+0,       &
         2.6477222699473109692D+0,        &
         -1.1538559607887416355D+0,       &
         2.0886077795020607837D-1,        &
         -1.0859777134152463084D-2      /)
    real(kind(1d0)),parameter::cq(7) = (/ &
         1.0000000000000000000D+0,        &
         -2.9383926818565635485D+0,       &
         3.2712093293018635389D+0,        &
         -1.7076702173954289421D+0,       &
         4.1596017228400603836D-1,        &
         -3.9801343754084482956D-2,       &
         8.2743668974466659035D-4       /)

    ! transform to [0, 1/2]
    if(x.lt.-1)then
       l=log(1-x)
       y=1/(1-x)
       r=-PI**2/6+l*(0.5D0*l-log(-x))
       s=1
    elseif(x.eq.-1)then
       dli2=-PI**2/12
       return
    elseif(x.lt.0)then
       y=x/(x-1)
       r=-0.5D0*log(1-x)**2
       s=-1
    elseif(x.eq.0)then
       dli2=x
       return
    elseif(x.lt.0.5D0)then
       y=x
       r=0
       s=1
    elseif(x.lt.1)then
       y=1-x
       r=PI**2/6-log(x)*log(y)
       s=-1
    elseif(x.eq.1)then
       dli2=PI**2/6
       return
    elseif(x.lt.2)then
       l=log(x)
       y=1-1/x
       r=PI**2/6-l*(log(y)+0.5D0*l)
       s=1
    else
       y=1/x
       r=PI**2/3-0.5D0*log(x)**2
       s=-1
    endif

    y2=y*y
    y4=y2*y2
    p=cp(1)+y*cp(2)+y2*(cp(3)+y*cp(4))+      &
         y4*(cp(5)+y*cp(6))
    q=cq(1)+y*cq(2)+y2*(cq(3)+y*cq(4))+      &
         y4*(cq(5)+y*cq(6)+y2*cq(7))

    dli2=r+s*y*p/q

    return
  end function dli2

  !> @brief Implementation of log(1 + z) for complex z
  !> @param z complex argument
  !> @return log(1 + z)
  function cdlog1p(z)
    implicit none
    complex(kind(1d0))::cdlog1p
    complex(kind(1d0)),intent(in)::z
    complex(kind(1d0))::u
    real(kind(1d0))::re,im

    u=1+z
    re=real(u)
    im=aimag(u)

    if(re.eq.1.and.im.eq.0)then
       cdlog1p=z
    elseif(re.le.0)then
       cdlog1p=log(u)
    else
       cdlog1p=log(u)*(z/(u-1))
    endif
    return
  end function cdlog1p

  ! Clausen functions
  ! from https://github.com/Expander/polylogarithm              
  ! author Alexander Voigt
  ! Implemented as rational function approximation. 
  
  ! Cl2(theta)=Im[Li2(e^{i\theta})]
  function dcl2(x)
    implicit none
    real(kind(1d0))::dcl2
    real(kind(1d0)),intent(inout)::x
    real(kind(1d0))::xorig
    real(kind(1d0))::y,z,z2,z4,p,q,p0,p1,sgn
    real(kind(1d0)),parameter::PI=3.14159265358979324D0
    real(kind(1d0)),parameter::PI2=2*PI,PIH=PI/2,PI28=PI*PI/8
    real(kind(1d0)),parameter::cp(4) = (/ &
         1.3888888888888889D-2,           &
         -4.3286930203743071D-4,          &
         3.2779814789973427D-6,           &
         -3.6001540369575084D-9         /)
    real(kind(1d0)),parameter::cq(4) = (/ &
         1.0000000000000000D+0,           &
         -3.6166589746694121D-2,          &
         3.6015827281202639D-4,           &
         -8.3646182842184428D-7         /)
    real(kind(1d0)),parameter::cr(6) = (/ &
         6.4005702446195512D-1,           &
         -2.0641655351338783D-1,          &
         2.4175305223497718D-2,           &
         -1.2355955287855728D-3,          &
         2.5649833551291124D-5,           &
         -1.4783829128773320D-7         /)
    real(kind(1d0)),parameter::cs(6) = (/ &
         1.0000000000000000D+0,           &
         -2.5299102015666356D-1,          &
         2.2148751048467057D-2,           &
         -7.8183920462457496D-4,          &
         9.5432542196310670D-6,           &
         -1.8184302880448247D-8         /)

    sgn=1

    xorig=x
    
    if(x.lt.0)then
       x=-x
       sgn=-1
    endif

    if(x.ge.PI2)then
       x=mod(x,PI2)
    endif

    if(x.gt.PI)then
       p0=6.28125D0
       p1=0.0019353071795864769253D0
       x=(p0-x)+p1
       sgn=-sgn
    endif

    if(x.eq.0)then
       dcl2=x
    elseif(x.eq.PI)then
       dcl2=0
    elseif(x.lt.PIH)then
       y=x*x
       z=y*y
       p=cp(1)+y*cp(2)+z*(cp(3)+y*cp(4))
       q=cq(1)+y*cq(2)+z*(cq(3)+y*cq(4))
       dcl2=sgn*x*(1-log(x)+y*p/q)
    else
       y=PI-x
       z=y*y-PI28
       z2=z*z
       z4=z2*z2
       p=cr(1)+z*cr(2)+z2*(cr(3)+z*cr(4))+ &
            z4*(cr(5)+z*cr(6))
       q=cs(1)+z*cs(2)+z2*(cs(3)+z*cs(4))+ &
            z4*(cs(5)+z*cs(6))
       dcl2=sgn*y*p/q
    endif

    ! recover x
    x=xorig
    
    return
  end function dcl2

  ! Cl3(theta)=Re[Li3(e^{i\theta})]
  function dcl3(x)
    implicit none
    real(kind(1d0))::dcl3
    real(kind(1d0)),intent(inout)::x
    real(kind(1d0))::xorig
    real(kind(1d0))::y,z,z2,z4,p,q,p0,p1
    real(kind(1d0)),parameter::PI=3.14159265358979324D0
    real(kind(1d0)),parameter::PI2=2*PI,PIH=PI/2,PI28=PI*PI/8
    real(kind(1d0)),parameter::zeta3=1.2020569031595943D0
    real(kind(1d0)),parameter::cp(4) = (/ &
         -7.5000000000000001D-1,          &
         1.5707637881835541D-2,           &
         -3.5426736843494423D-5,          &
         -2.4408931585123682D-7         /)
    real(kind(1d0)),parameter::cq(4) = (/ &
         1.0000000000000000D+0,           &
         -2.5573146805410089D-2,          &
         1.5019774853075050D-4,           &
         -1.0648552418111624D-7         /)
    real(kind(1d0)),parameter::cr(6) = (/ &
         -4.9017024647634973D-1,          &
         4.1559155224660940D-1,           &
         -7.9425531417806701D-2,          &
         5.9420152260602943D-3,           &
         -1.8302227163540190D-4,          &
         1.8027408929418533D-6          /)
    real(kind(1d0)),parameter::cs(6) = (/ &
         1.0000000000000000D+0,           &
         -1.9495887541644712D-1,          &
         1.2059410236484074D-2,           &
         -2.5235889467301620D-4,          &
         1.0199322763377861D-6,           &
         1.9612106499469264D-9          /)

    xorig=x
    
    if(x.lt.0)then
       x=-x
    endif

    if(x.ge.PI2)then
       x=mod(x,PI2)
    endif

    if(x.gt.PI)then
       p0=6.28125D0
       p1=0.0019353071795864769253D0
       x=(p0-x)+p1
    endif

    if(x.eq.0)then
       dcl3=zeta3
    elseif(x.lt.PIH)then
       y=x*x
       z=y*y
       p=cp(1)+y*cp(2)+z*(cp(3)+y*cp(4))
       q=cq(1)+y*cq(2)+z*(cq(3)+y*cq(4))
       dcl3=zeta3+y*(p/q+log(x)/2)
    else
       y=PI-x
       z=y*y-PI28
       z2=z*z
       z4=z2*z2
       p=cr(1)+z*cr(2)+z2*(cr(3)+z*cr(4))+ &
            z4*(cr(5)+z*cr(6))
       q=cs(1)+z*cs(2)+z2*(cs(3)+z*cs(4))+ &
            z4*(cs(5)+z*cs(6))
       dcl3=p/q
    endif

    ! recover x
    x=xorig
    
    return
  end function dcl3

  ! Cl4(theta)=Im[Li4(e^{i\theta})]
  function dcl4(x)
    implicit none
    real(kind(1d0))::dcl4
    real(kind(1d0)),intent(inout)::x
    real(kind(1d0))::xorig
    real(kind(1d0))::y,z,z2,z4,p,q,p0,p1,sgn
    real(kind(1d0)),parameter::PI=3.14159265358979324D0
    real(kind(1d0)),parameter::PI2=2*PI,PIH=PI/2,PI28=PI*PI/8
    real(kind(1d0)),parameter::zeta3=1.2020569031595943D0
    real(kind(1d0)),parameter::cp(4) = (/ &
         -3.0555555555555556D-1,          &
         6.0521392328447206D-3,           &
         -1.9587493942041528D-5,          &
         -3.1137343767030358D-8         /)
    real(kind(1d0)),parameter::cq(4) = (/ &
         1.0000000000000000D+0,           &
         -2.2079728398400851D-2,          &
         1.0887447112236682D-4,           &
         -6.1847621370547954D-8         /)
    real(kind(1d0)),parameter::cr(6) = (/ &
         7.6223911686491336D-1,           &
         -2.4339587368267260D-1,          &
         2.8715364937979943D-2,           &
         -1.5368612510964667D-3,          &
         3.6261044225761673D-5,           &
         -2.8557977333851308D-7         /)
    real(kind(1d0)),parameter::cs(6) = (/ &
         1.0000000000000000D+0,           &
         -1.7465715261403233D-1,          &
         9.5439417991615653D-3,           &
         -1.7325070821666274D-4,          &
         5.9283675098376635D-7,           &
         9.4127575773361230D-10         /)

    sgn=1

    xorig=x

    if(x.lt.0)then
       x=-x
       sgn=-1
    endif

    if(x.ge.PI2)then
       x=mod(x,PI2)
    endif

    if(x.gt.PI)then
       p0=6.28125D0
       p1=0.0019353071795864769253D0
       x=(p0-x)+p1
       sgn=-sgn
    endif

    if(x.eq.0)then
       dcl4=x
    elseif(x.eq.PI)then
       dcl4=0
    elseif(x.lt.PIH)then
       y=x*x
       z=y*y
       p=cp(1)+y*cp(2)+z*(cp(3)+y*cp(4))
       q=cq(1)+y*cq(2)+z*(cq(3)+y*cq(4))
       dcl4=sgn*x*(zeta3+y*(p/q+log(x)/6))
    else
       y=PI-x
       z=y*y-PI28
       z2=z*z
       z4=z2*z2
       p=cr(1)+z*cr(2)+z2*(cr(3)+z*cr(4))+ &
            z4*(cr(5)+z*cr(6))
       q=cs(1)+z*cs(2)+z2*(cs(3)+z*cs(4))+ &
            z4*(cs(5)+z*cs(6))
       dcl4=sgn*y*p/q
    endif

    ! recover x
    x=xorig
    return
  end function dcl4

  ! Cl5(theta)=Re[Li5(e^{i\theta})]
  function dcl5(x)
    real(kind(1d0))::dcl5
    real(kind(1d0)),intent(inout)::x
    real(kind(1d0))::xorig
    real(kind(1d0))::y,z,z2,z4,p,q,p0,p1
    real(kind(1d0)),parameter::PI=3.14159265358979324D0
    real(kind(1d0)),parameter::PI2=2*PI,PIH=PI/2,PI28=PI*PI/8
    real(kind(1d0)),parameter::zeta5=1.0369277551433699D0
    real(kind(1d0)),parameter::cp(4) = (/ &
         1.0369277551433699D+0,           &
         -6.1354800479984468D-1,          &
         9.4076401395712763D-2,           &
         -9.4056155866704436D-4         /)
    real(kind(1d0)),parameter::cq(5) = (/ &
         1.0000000000000000D+0,           &
         -1.2073698633244778D-2,          &
         1.3703409625482991D-5,           &
         -1.9701280330628469D-9,          &
         2.1944550184416500D-11         /)
    real(kind(1d0)),parameter::cr(6) = (/ &
         -4.5930112735784898D-1,          &
         4.3720705508867954D-1,           &
         -7.5895226486465095D-2,          &
         5.2244176912488065D-3,           &
         -1.5677716622013956D-4,          &
         1.6641624171748576D-6          /)
    real(kind(1d0)),parameter::cs(6) = (/ &
         1.0000000000000000D+0,           &
         -1.2211486825401188D-1,          &
         3.8940070749313620D-3,           &
         -2.2674805547074318D-5,          &
         -7.4383354448335299D-8,          &
         -3.4131758392216437D-10        /)

    xorig=x

    if(x.lt.0)then
       x=-x
    endif

    if(x.ge.PI2)then
       x=mod(x,PI2)
    endif

    if(x.gt.PI)then
       p0=6.28125D0
       p1=0.0019353071795864769253D0
       x=(p0-x)+p1
    endif

    if(x.eq.0)then
       dcl5=zeta5
    elseif(x.lt.PIH)then
       y=x*x
       z=y*y
       p=cp(1)+y*cp(2)+z*(cp(3)+y*cp(4))
       q=cq(1)+y*cq(2)+z*(cq(3)+y*cq(4)+z*cq(5))
       dcl5=p/q-1.0D0/24*z*log(x)
    else
       y=PI-x
       z=y*y-PI28
       z2=z*z
       z4=z2*z2
       p=cr(1)+z*cr(2)+z2*(cr(3)+z*cr(4))+ &
            z4*(cr(5)+z*cr(6))
       q=cs(1)+z*cs(2)+z2*(cs(3)+z*cs(4))+ &
            z4*(cs(5)+z*cs(6))
       dcl5=p/q
    endif

    ! recover x
    x=xorig
    
    return
  end function dcl5

  ! Cl6(theta)=Im[Li6(e^{i\theta})]
  function dcl6(x)
    implicit none
    real(kind(1d0))::dcl6
    real(kind(1d0)),intent(inout)::x
    real(kind(1d0))::xorig
    real(kind(1d0))::y,z,z2,z4,p,q,p0,p1,sgn
    real(kind(1d0)),parameter::PI=3.14159265358979324D0
    real(kind(1d0)),parameter::PI2=2*PI,PIH=PI/2,PI28=PI*PI/8
    real(kind(1d0)),parameter::zeta3=1.2020569031595943D0
    real(kind(1d0)),parameter::cp(4) = (/ &
         1.0369277551433699D+0,           &
         -2.0871954441071750D-1,          &
         2.0652251045312954D-2,           &
         -1.3834381382568400D-4         /)
    real(kind(1d0)),parameter::cq(4) = (/ &
         1.0000000000000000D+0,           &
         -8.0784096827362542D-3,          &
         5.8074568862993102D-6,           &
         -5.1960620033050114D-10        /)
    real(kind(1d0)),parameter::cr(5) = (/ &
         7.9544504578027050D-1,           &
         -1.9255025309738589D-1,          &
         1.5805208288846591D-2,           &
         -5.4175380521534706D-4,          &
         6.7577493541009068D-6          /)
    real(kind(1d0)),parameter::cs(6) = (/ &
         1.0000000000000000D+0,           &
         -7.0798422394109274D-2,          &
         7.1744189715634762D-4,           &
         3.9098747334347093D-6,           &
         3.5669441618295266D-8,           &
         2.5315391843409925D-10         /)

    xorig=x

    sgn = 1

    if(x.lt.0)then
       x=-x
       sgn=-1
    endif

    if(x.ge.PI2)then
       x=mod(x,PI2)
    endif

    if(x.gt.PI)then
       p0=6.28125D0
       p1=0.0019353071795864769253D0
       x=(p0-x)+p1
       sgn=-sgn
    endif

    if(x.eq.0)then
       dcl6=x
    elseif(x.eq.PI)then
       dcl6=0
    elseif(x.lt.PIH)then
       y=x*x
       z=y*y
       p=cp(1)+y*cp(2)+z*(cp(3)+y*cp(4))
       q=cq(1)+y*cq(2)+z*(cq(3)+y*cq(4))
       dcl6=sgn*x*(p/q-1.0D0/120*z*log(x))
    else
       y=PI-x
       z=y*y-PI28
       z2=z*z
       z4=z2*z2
       p=cr(1)+z*cr(2)+z2*(cr(3)+z*cr(4))+ &
            z4*cr(5)
       q=cs(1)+z*cs(2)+z2*(cs(3)+z*cs(4))+ &
            z4*(cs(5)+z*cs(6))
       dcl6=sgn*y*p/q
    endif

    ! recover x
    x=xorig
    
    return
  end function dcl6
  
END MODULE nielsen_generalized_polylog_wrapper
