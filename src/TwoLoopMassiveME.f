      ! get the form factors from helicity amplitudes
      subroutine Get_FormFactors_From_HelAmps(shat,that
     $     ,uhat,amp,FFs)
      implicit none
      double precision shat,that,uhat
      double complex FFs(5)
      double complex amp(5)
      integer ihel
      ! AS(s,t,u)
      FFs(1)=0.5d0*(amp(1)+4d0*amp(2)+amp(3)+amp(4)+amp(5))
      ! DBhat111(s,t,u)
      FFs(2)=-0.5d0/uhat*(-amp(1)-amp(3)+amp(4)+amp(5))
      ! DBhat111(t,u,s)
      FFs(3)=-0.5d0/shat*(-amp(1)+amp(3)+amp(4)-amp(5))
      ! DBhat111(u,s,t)
      FFs(4)=-0.5d0/that*(-amp(1)+amp(3)-amp(4)+amp(5))
      ! DChat2111(s,t,u)
      FFs(5)=-0.5d0/(shat*uhat)*(amp(1)-4d0*amp(2)+amp(3)+amp(4)+amp(5))
      return
      end

      subroutine Get_TwoLoop_HelAmp_Massive(xs,xt,xu,loopba,amp)
      use kinetics
      implicit none
      integer NDIM
      parameter (NDIM=84)
      double precision xs,xt,xu
      double precision xs2,xt2,xu2
      double precision xs3,xt3,xu3
      double complex loopba(NDIM)
      double complex amp(5)
      double complex sqrtss(12)
      double precision ratss(144)
      double precision rat1_UT1L
      external rat1_UT1L
      double precision rat1_UT2L
      external rat1_UT2L
      double precision rat2_UT2L
      external rat2_UT2L
      double precision rat3_UT2L
      external rat3_UT2L
      double precision rat4_UT2L
      external rat4_UT2L
      double precision rat5_UT2L
      external rat5_UT2L
      double precision rat6_UT2L
      external rat6_UT2L
      double precision rat7_UT2L
      external rat7_UT2L
      double precision rat8_UT2L
      external rat8_UT2L
      double precision rat9_UT2L
      external rat9_UT2L
      double precision rat10_UT2L
      external rat10_UT2L
      double precision rat11_UT2L
      external rat11_UT2L
      double precision rat12_UT2L
      external rat12_UT2L
      double precision rat13_UT2L
      external rat13_UT2L
      double precision rat14_UT2L
      external rat14_UT2L
      double precision rat15_UT2L
      external rat15_UT2L
      double precision rat16_UT2L
      external rat16_UT2L
      double precision rat17_UT2L
      external rat17_UT2L
      double precision rat18_UT2L
      external rat18_UT2L
      double precision rat19_UT2L
      external rat19_UT2L
      double precision rat20_UT2L
      external rat20_UT2L
      double precision rat21_UT2L
      external rat21_UT2L
      double precision rat22_UT2L
      external rat22_UT2L
      double precision rat23_UT2L
      external rat23_UT2L
      double precision rat24_UT2L
      external rat24_UT2L
      double precision rat25_UT2L
      external rat25_UT2L
      double precision rat26_UT2L
      external rat26_UT2L
      double precision rat27_UT2L
      external rat27_UT2L
      double precision rat28_UT2L
      external rat28_UT2L
      double precision rat29_UT2L
      external rat29_UT2L
      double precision rat30_UT2L
      external rat30_UT2L
      double precision rat31_UT2L
      external rat31_UT2L
      integer i
      xs2=xs**2
      xt2=xt**2
      xu2=xu**2
      xs3=xs**3
      xt3=xt**3
      xu3=xu**3
      sqrtss(1)=sqrt1(xs)
      sqrtss(2)=sqrt1(xt)
      sqrtss(3)=sqrt1(xu)
      sqrtss(4)=sqrt3(xs,xt)
      sqrtss(5)=sqrt3(xs,xu)
      sqrtss(6)=sqrt3(xt,xu)
      sqrtss(7)=sqrt4(xs,xt)
      sqrtss(8)=sqrt4(xs,xu)
      sqrtss(9)=sqrt4(xt,xs)
      sqrtss(10)=sqrt4(xt,xu)
      sqrtss(11)=sqrt4(xu,xs)
      sqrtss(12)=sqrt4(xu,xt)
      ratss(1)=rat1_UT1L(xs,xt,xu)
      ratss(2)=rat1_UT1L(xt,xu,xs)
      ratss(3)=rat1_UT1L(xu,xs,xt)
      ratss(4)=rat1_UT2L(xs,xt,xu)
      ratss(5)=rat1_UT2L(xt,xu,xs)
      ratss(6)=rat1_UT2L(xu,xs,xt)
      ratss(7)=rat2_UT2L(xs,xt,xu)
      ratss(8)=rat2_UT2L(xt,xu,xs)
      ratss(9)=rat2_UT2L(xu,xs,xt)
      ratss(10)=rat3_UT2L(xs,xt,xu)
      ratss(11)=rat3_UT2L(xt,xu,xs)
      ratss(12)=rat3_UT2L(xu,xs,xt)
      ratss(13)=rat4_UT2L(xs,xt,xu)
      ratss(14)=rat4_UT2L(xt,xu,xs)
      ratss(15)=rat4_UT2L(xu,xs,xt)
      ratss(16)=rat5_UT2L(xs,xt,xu)
      ratss(17)=rat5_UT2L(xt,xu,xs)
      ratss(18)=rat5_UT2L(xu,xs,xt)
      ratss(19)=rat6_UT2L(xs,xt,xu)
      ratss(20)=rat6_UT2L(xt,xu,xs)
      ratss(21)=rat6_UT2L(xu,xs,xt)
      ratss(22)=rat7_UT2L(xs,xt,xu)
      ratss(23)=rat7_UT2L(xt,xu,xs)
      ratss(24)=rat7_UT2L(xu,xs,xt)
      ratss(25)=rat8_UT2L(xs,xt,xu)
      ratss(26)=rat8_UT2L(xt,xu,xs)
      ratss(27)=rat8_UT2L(xu,xs,xt)
      ratss(28)=rat9_UT2L(xs,xt,xu)
      ratss(29)=rat9_UT2L(xs,xu,xt)
      ratss(30)=rat9_UT2L(xt,xs,xu)
      ratss(31)=rat9_UT2L(xt,xu,xs)
      ratss(32)=rat9_UT2L(xu,xs,xt)
      ratss(33)=rat9_UT2L(xu,xt,xs)
      ratss(34)=rat10_UT2L(xs,xt,xu)
      ratss(35)=rat10_UT2L(xt,xu,xs)
      ratss(36)=rat10_UT2L(xu,xs,xt)
      ratss(37)=rat11_UT2L(xs,xt,xu)
      ratss(38)=rat11_UT2L(xs,xu,xt)
      ratss(39)=rat11_UT2L(xt,xs,xu)
      ratss(40)=rat11_UT2L(xt,xu,xs)
      ratss(41)=rat11_UT2L(xu,xs,xt)
      ratss(42)=rat11_UT2L(xu,xt,xs)
      ratss(43)=rat12_UT2L(xs,xt,xu)
      ratss(44)=rat12_UT2L(xs,xu,xt)
      ratss(45)=rat12_UT2L(xt,xs,xu)
      ratss(46)=rat12_UT2L(xt,xu,xs)
      ratss(47)=rat12_UT2L(xu,xs,xt)
      ratss(48)=rat12_UT2L(xu,xt,xs)
      ratss(49)=rat13_UT2L(xs,xt,xu)
      ratss(50)=rat13_UT2L(xs,xu,xt)
      ratss(51)=rat13_UT2L(xt,xs,xu)
      ratss(52)=rat13_UT2L(xt,xu,xs)
      ratss(53)=rat13_UT2L(xu,xs,xt)
      ratss(54)=rat13_UT2L(xu,xt,xs)
      ratss(55)=rat14_UT2L(xt,xs,xu)
      ratss(56)=rat14_UT2L(xt,xu,xs)
      ratss(57)=rat14_UT2L(xu,xs,xt)
      ratss(58)=rat15_UT2L(xs,xt,xu)
      ratss(59)=rat15_UT2L(xs,xu,xt)
      ratss(60)=rat15_UT2L(xt,xs,xu)
      ratss(61)=rat15_UT2L(xt,xu,xs)
      ratss(62)=rat15_UT2L(xu,xs,xt)
      ratss(63)=rat15_UT2L(xu,xt,xs)
      ratss(64)=rat16_UT2L(xs,xt,xu)
      ratss(65)=rat16_UT2L(xs,xu,xt)
      ratss(66)=rat16_UT2L(xt,xs,xu)
      ratss(67)=rat16_UT2L(xt,xu,xs)
      ratss(68)=rat16_UT2L(xu,xs,xt)
      ratss(69)=rat16_UT2L(xu,xt,xs)
      ratss(70)=rat17_UT2L(xs,xt,xu)
      ratss(71)=rat17_UT2L(xs,xu,xt)
      ratss(72)=rat17_UT2L(xu,xs,xt)
      ratss(73)=rat18_UT2L(xs,xt,xu)
      ratss(74)=rat18_UT2L(xs,xu,xt)
      ratss(75)=rat18_UT2L(xt,xs,xu)
      ratss(76)=rat18_UT2L(xt,xu,xs)
      ratss(77)=rat18_UT2L(xu,xs,xt)
      ratss(78)=rat18_UT2L(xu,xt,xs)
      ratss(79)=rat19_UT2L(xs,xt,xu)
      ratss(80)=rat19_UT2L(xs,xu,xt)
      ratss(81)=rat19_UT2L(xt,xs,xu)
      ratss(82)=rat19_UT2L(xt,xu,xs)
      ratss(83)=rat19_UT2L(xu,xs,xt)
      ratss(84)=rat19_UT2L(xu,xt,xs)
      ratss(85)=rat20_UT2L(xs,xt,xu)
      ratss(86)=rat20_UT2L(xs,xu,xt)
      ratss(87)=rat20_UT2L(xt,xs,xu)
      ratss(88)=rat20_UT2L(xt,xu,xs)
      ratss(89)=rat20_UT2L(xu,xs,xt)
      ratss(90)=rat20_UT2L(xu,xt,xs)
      ratss(91)=rat21_UT2L(xs,xt,xu)
      ratss(92)=rat21_UT2L(xs,xu,xt)
      ratss(93)=rat21_UT2L(xt,xs,xu)
      ratss(94)=rat21_UT2L(xt,xu,xs)
      ratss(95)=rat21_UT2L(xu,xs,xt)
      ratss(96)=rat21_UT2L(xu,xt,xs)
      ratss(97)=rat22_UT2L(xs,xt,xu)
      ratss(98)=rat22_UT2L(xs,xu,xt)
      ratss(99)=rat22_UT2L(xt,xs,xu)
      ratss(100)=rat22_UT2L(xt,xu,xs)
      ratss(101)=rat22_UT2L(xu,xs,xt)
      ratss(102)=rat22_UT2L(xu,xt,xs)
      ratss(103)=rat23_UT2L(xs,xt,xu)
      ratss(104)=rat23_UT2L(xs,xu,xt)
      ratss(105)=rat23_UT2L(xt,xs,xu)
      ratss(106)=rat23_UT2L(xt,xu,xs)
      ratss(107)=rat23_UT2L(xu,xs,xt)
      ratss(108)=rat23_UT2L(xu,xt,xs)
      ratss(109)=rat24_UT2L(xt,xs,xu)
      ratss(110)=rat24_UT2L(xt,xu,xs)
      ratss(111)=rat24_UT2L(xu,xs,xt)
      ratss(112)=rat25_UT2L(xt,xs,xu)
      ratss(113)=rat25_UT2L(xt,xu,xs)
      ratss(114)=rat25_UT2L(xu,xs,xt)
      ratss(115)=rat26_UT2L(xs,xt,xu)
      ratss(116)=rat26_UT2L(xs,xu,xt)
      ratss(117)=rat26_UT2L(xu,xs,xt)
      ratss(118)=rat27_UT2L(xs,xt,xu)
      ratss(119)=rat27_UT2L(xs,xu,xt)
      ratss(120)=rat27_UT2L(xt,xs,xu)
      ratss(121)=rat27_UT2L(xt,xu,xs)
      ratss(122)=rat27_UT2L(xu,xs,xt)
      ratss(123)=rat27_UT2L(xu,xt,xs)
      ratss(124)=rat28_UT2L(xs,xt,xu)
      ratss(125)=rat28_UT2L(xs,xu,xt)
      ratss(126)=rat28_UT2L(xt,xs,xu)
      ratss(127)=rat28_UT2L(xt,xu,xs)
      ratss(128)=rat28_UT2L(xu,xs,xt)
      ratss(129)=rat28_UT2L(xu,xt,xs)
      ratss(130)=rat29_UT2L(xs,xt,xu)
      ratss(131)=rat29_UT2L(xt,xu,xs)
      ratss(132)=rat29_UT2L(xu,xt,xs)
      ratss(133)=rat30_UT2L(xs,xt,xu)
      ratss(134)=rat30_UT2L(xs,xu,xt)
      ratss(135)=rat30_UT2L(xt,xs,xu)
      ratss(136)=rat30_UT2L(xt,xu,xs)
      ratss(137)=rat30_UT2L(xu,xs,xt)
      ratss(138)=rat30_UT2L(xu,xt,xs)
      ratss(139)=rat31_UT2L(xs,xt,xu)
      ratss(140)=rat31_UT2L(xs,xu,xt)
      ratss(141)=rat31_UT2L(xt,xs,xu)
      ratss(142)=rat31_UT2L(xt,xu,xs)
      ratss(143)=rat31_UT2L(xu,xs,xt)
      ratss(144)=rat31_UT2L(xu,xt,xs)
      ! --++,++-- (2 -> 2)
      amp(1)=4d0*(3D0+(8D0*loopba(17))/sqrtss(4) + 
     -    (8D0*loopba(39))/sqrtss(4) - 
     -    (2D0*(-2D0 + xs)*loopba(21))/
     -     (sqrtss(1)*sqrtss(4)) - 
     -    (2D0*(-2D0 + xt)*loopba(41))/
     -     (sqrtss(2)*sqrtss(4)) + 
     -    (8D0*loopba(28))/sqrtss(5) + 
     -    (8D0*loopba(49))/sqrtss(5) - 
     -    (2D0*(-2D0 + xs)*loopba(32))/
     -     (sqrtss(1)*sqrtss(5)) - 
     -    (2D0*(-2D0 + xu)*loopba(51))/
     -     (sqrtss(3)*sqrtss(5)) - 
     -    4D0*(-loopba(1)**4/(4d0*xs2) - 
     -       loopba(2)**4/(4d0*xt2) - 
     -       loopba(3)**4/(4d0*xu2) + 
     -       (-(1D0/xs) + 2D0/xt + 2D0/xu)*loopba(4) + 
     -       (2D0/xs - 1D0/xt + 2D0/xu)*loopba(5) + 
     -       (2D0/xs + 2D0/xt - 1D0/xu)*loopba(6) + 
     -       (-8D0*loopba(18) - 
     -          12D0*loopba(1)*loopba(33) - 
     -          loopba(36) + 
     -          2D0*loopba(1)*loopba(37) - 
     -          4D0*(loopba(34) - 
     -             (5D0*loopba(36))/12d0 + 
     -             (loopba(1)*
     -                (-6D0*loopba(33) + loopba(37))
     -                )/6d0))/(2d0*xs*xt) + 
     -       (-12D0*loopba(2)*loopba(10) - 
     -          loopba(13) + 
     -          2D0*loopba(2)*loopba(14) - 
     -          4D0*(loopba(11) - 
     -             (5D0*loopba(13))/12d0 + 
     -             (loopba(2)*
     -                (-6D0*loopba(10) + loopba(14))
     -                )/6d0) - 8D0*loopba(44))/
     -        (2d0*xt*xu) + 
     -       (-12D0*loopba(3)*loopba(22) - 
     -          loopba(24) + 
     -          2D0*loopba(3)*loopba(25) - 
     -          4D0*(loopba(23) - 
     -             (5D0*loopba(24))/12d0 + 
     -             (loopba(3)*
     -                (-6D0*loopba(22) + loopba(25))
     -                )/6d0) - 8D0*loopba(50))/
     -        (2d0*xs*xu) + 
     -       (2*(((xs*xt + 4*xu)*loopba(16))/xu + 
     -            loopba(20)))/sqrtss(4) + 
     -       (2D0*(((4D0*xt + xs*xu)*loopba(27))/xt + 
     -            loopba(31)))/sqrtss(5) + 
     -       (2D0*(((4D0*xs + xt*xu)*loopba(42))/xs + 
     -            loopba(46)))/sqrtss(6)) + 
     -    (8D0*loopba(43))/sqrtss(6) + 
     -    (8D0*loopba(52))/sqrtss(6) - 
     -    (2D0*(-2D0 + xt)*loopba(47))/
     -     (sqrtss(2)*sqrtss(6)) - 
     -    (2D0*(-2D0 + xu)*loopba(54))/
     -     (sqrtss(3)*sqrtss(6)))
      ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+ (2 -> 2)
      amp(2)=-((6D0/xs + 4D0/xt + 6D0/xu)*loopba(15)) + 
     -  (4D0/xs + 4D0/xt + 8D0/xu)*
     -   (-loopba(3)**4/8d0 + loopba(19)) - 
     -  (6D0/xs + 6D0/xt + 4D0/xu)*loopba(26) + 
     -  (4D0/xs + 8D0/xt + 4D0/xu)*
     -   (-loopba(2)**4/8d0 + loopba(30)) - 
     -  (4D0/xs + 6D0/xt + 6D0/xu)*loopba(38) + 
     -  (8D0/xs + 4D0/xt + 4D0/xu)*
     -   (-loopba(1)**4/8d0 + loopba(45)) - 
     -  (4D0/xs + 6D0/xt + 6D0/xu)*(loopba(67)+loopba(68))-
     -  (6D0/xs + 4D0/xt + 6D0/xu)*(loopba(69)+loopba(70))-
     -  (6D0/xs + 6D0/xt + 4D0/xu)*(loopba(71)+loopba(72))-
     -  (2D0/xt - 2D0/xu)*(loopba(79)-loopba(80))-
     -  (2D0/xs - 2D0/xu)*(loopba(81)-loopba(82))-
     -  (2D0/xs - 2D0/xt)*(loopba(83)-loopba(84))+
     -  (-2D0*loopba(1)*loopba(33)+ 
     -     (loopba(1)*loopba(37))/3d0)*ratss(7) + 
     -  (-2D0*loopba(2)*loopba(10) + 
     -     (loopba(2)*loopba(14))/3d0)*ratss(8) + 
     -  (-2D0*loopba(3)*loopba(22) + 
     -     (loopba(3)*loopba(25))/3d0)*ratss(9) + 
     -  loopba(13)*ratss(16) + 
     -  loopba(24)*ratss(17) + 
     -  loopba(36)*ratss(18) + 
     -  loopba(5)*ratss(19) + 
     -  loopba(6)*ratss(20) + 
     -  loopba(4)*ratss(21) + 
     -  loopba(11)*ratss(22) + 
     -  loopba(23)*ratss(23) + 
     -  loopba(34)*ratss(24) + 
     -  loopba(18)*ratss(25) + 
     -  loopba(44)*ratss(26) + 
     -  loopba(50)*ratss(27) + 
     -  (((4D0*(-2D0 + xs))/xu - (2D0*xu)/xt)*
     -     loopba(73))/sqrtss(1) + 
     -  (((4D0*(-2D0 + xs))/xt - (2D0*xt)/xu)*
     -     loopba(74))/sqrtss(1) + 
     -  (2*loopba(1)*ratss(1) + 
     -     (loopba(33) - loopba(37)/6d0)*ratss(6))/
     -   sqrtss(1) - 
     -  (loopba(1)**3*ratss(10))/(2d0*sqrtss(1)) + 
     -  (loopba(12)*ratss(13))/sqrtss(1) + 
     -  (((4D0*(-2D0 + xt))/xu - (2D0*xu)/xs)*
     -     loopba(75))/sqrtss(2) + 
     -  (((4D0*(-2D0 + xt))/xs - (2D0*xs)/xu)*
     -     loopba(76))/sqrtss(2) + 
     -  (2D0*loopba(2)*ratss(2) + 
     -     (loopba(10) - loopba(14)/6d0)*ratss(4))/
     -   sqrtss(2) - 
     -  (loopba(2)**3*ratss(11))/(2d0*sqrtss(2)) + 
     -  (loopba(35)*ratss(14))/sqrtss(2) + 
     -  (((-2D0*xt)/xs + (4D0*(-2D0 + xu))/xt)*
     -     loopba(77))/sqrtss(3) + 
     -  (((-2D0*xs)/xt + (4D0*(-2D0 + xu))/xs)*
     -     loopba(78))/sqrtss(3) + 
     -  (2D0*loopba(3)*ratss(3) + 
     -     (loopba(22) - loopba(25)/6d0)*ratss(5))/
     -   sqrtss(3) - 
     -  (loopba(3)**3*ratss(12))/(2d0*sqrtss(3)) + 
     -  (loopba(48)*ratss(15))/sqrtss(3) - 
     -  (2D0*xs*xt*loopba(7))/sqrtss(4) + 
     -  ((32D0+(16D0*xs*xt)/xu)*loopba(17))/
     -   sqrtss(4) - 
     -  ((32D0+(16D0*xs*xt)/xu)*loopba(20))/
     -   sqrtss(4) + 
     -  ((32D0+(16D0*xs*xt)/xu)*loopba(39))/
     -   sqrtss(4) + 
     -  (loopba(16)*ratss(34))/sqrtss(4) + 
     -  (loopba(61)*ratss(37))/sqrtss(4) + 
     -  (loopba(63)*ratss(39))/sqrtss(4) - 
     -  (4D0*(-2D0+xs)*(2D0+(xs*xt)/xu)*loopba(21))/
     -   (sqrtss(1)*sqrtss(4)) - 
     -  (4D0*(-2D0+xt)*(2D0+(xs*xt)/xu)*loopba(41))/
     -   (sqrtss(2)*sqrtss(4)) - 
     -  (2D0*xs*xu*loopba(9))/sqrtss(5) + 
     -  ((32D0+(16D0*xs*xu)/xt)*loopba(28))/
     -   sqrtss(5) - 
     -  ((32D0+(16D0*xs*xu)/xt)*loopba(31))/
     -   sqrtss(5) + 
     -  ((32D0+(16D0*xs*xu)/xt)*loopba(49))/
     -   sqrtss(5) + 
     -  (loopba(27)*ratss(36))/sqrtss(5) + 
     -  (loopba(62)*ratss(38))/sqrtss(5) + 
     -  (loopba(65)*ratss(41))/sqrtss(5) - 
     -  (4D0*(-2D0+xs)*(2D0+(xs*xu)/xt)*loopba(32))/
     -   (sqrtss(1)*sqrtss(5)) - 
     -  (4D0*(-2D0+xu)*(2D0+(xs*xu)/xt)*loopba(51))/
     -   (sqrtss(3)*sqrtss(5)) - 
     -  (2D0*xt*xu*loopba(8))/sqrtss(6) + 
     -  ((32D0+(16D0*xt*xu)/xs)*loopba(43))/
     -   sqrtss(6) - 
     -  ((32D0+(16D0*xt*xu)/xs)*loopba(46))/
     -   sqrtss(6) + 
     -  ((32D0+(16D0*xt*xu)/xs)*loopba(52))/
     -   sqrtss(6) + 
     -  (loopba(42)*ratss(35))/sqrtss(6) + 
     -  (loopba(64)*ratss(40))/sqrtss(6) + 
     -  (loopba(66)*ratss(42))/sqrtss(6) - 
     -  (4D0*(-2D0+xt)*(2D0+(xt*xu)/xs)*loopba(47))/
     -   (sqrtss(2)*sqrtss(6)) - 
     -  (4D0*(-2D0+xu)*(2D0+(xt*xu)/xs)*loopba(54))/
     -   (sqrtss(3)*sqrtss(6)) + 
     -  (loopba(55)*ratss(28))/sqrtss(7) + 
     -  (loopba(56)*ratss(29))/sqrtss(8) + 
     -  (loopba(57)*ratss(30))/sqrtss(9) + 
     -  (loopba(58)*ratss(31))/sqrtss(10) + 
     -  (loopba(59)*ratss(32))/sqrtss(11) + 
     -  (loopba(60)*ratss(33))/sqrtss(12)
      ! ++++,---- (2 -> 2)
      amp(3)=4d0+(4d0*loopba(1)**4)/xs2 + 
     -  (8D0*(8D0+xs-xt2-xu2)*loopba(45))/
     -   xs2 - (8D0*(4D0+xs-xt2+3D0*xu)*
     -     (loopba(69) + loopba(81)/2d0))/xs2 - 
     -  (8D0*(4D0+xs+3D0*xt-xu2)*
     -     (loopba(71) + loopba(83)/2d0))/xs2 + 
     -  (loopba(15) + loopba(72))*ratss(43) + 
     -  (loopba(26) + loopba(70))*ratss(44) + 
     -  loopba(18)*ratss(49) + 
     -  loopba(29)*ratss(50) + 
     -  (loopba(44) + loopba(53))*ratss(56) + 
     -  (loopba(3)**4/4d0 - loopba(84))*
     -   ratss(58) + 
     -  (loopba(2)**4/4d0 - loopba(82))*
     -   ratss(59) + loopba(5)*ratss(64) + 
     -  loopba(6)*ratss(65) + 
     -  loopba(4)*ratss(72) + 
     -  (-6D0*loopba(34) + loopba(36))*ratss(117) + 
     -  loopba(13)*ratss(118) + 
     -  loopba(24)*ratss(119) + 
     -  loopba(11)*ratss(124) + 
     -  loopba(23)*ratss(125) + 
     -  (-2D0*loopba(1)*loopba(33) + 
     -     (loopba(1)*loopba(37))/3d0)*ratss(130) 
     -   - 2*loopba(2)*loopba(10)*ratss(135) - 
     -  2D0*loopba(3)*loopba(22)*ratss(137) + 
     -  loopba(2)*loopba(14)*ratss(139) + 
     -  loopba(3)*loopba(25)*ratss(140) + 
     -  (4d0*xs2*(-16d0+xs2)*loopba(1)**3)/
     -   ((xs*xt+4D0*xu)*(4D0*xt+xs*xu)*sqrtss(1))
     -    + (4d0*(4d0*xs+12d0*xt-xt3-xu2)*
     -     loopba(75))/(xs2*sqrtss(2)) + 
     -  (loopba(10)*ratss(73))/sqrtss(2) + 
     -  (loopba(35)*ratss(79))/sqrtss(2) + 
     -  (loopba(14)*ratss(85))/sqrtss(2) + 
     -  4d0*(xs2+2d0*xt*xu)*
     -   (((-4d0+xs)*loopba(1))/
     -      (xt*xu*sqrtss(1)) + 
     -     ((-4d0+xt)*loopba(2))/
     -      (xs*xu*sqrtss(2)) + 
     -     ((-4d0+xu)*loopba(3))/(xs*xt*sqrtss(3))
     -     ) - (2d0*(-4d0*xs+xt2+xu2)*
     -     (((-2d0+xt)*loopba(76))/sqrtss(2) + 
     -       ((-2d0+xu)*loopba(78))/sqrtss(3)))/
     -   xs2 + (4d0*(4d0*xs-xt2+12d0*xu - 
     -       xu3)*loopba(77))/(xs2*sqrtss(3)) 
     -   + (loopba(22)*ratss(74))/sqrtss(3) + 
     -  (loopba(48)*ratss(80))/sqrtss(3) + 
     -  (loopba(25)*ratss(86))/sqrtss(3) + 
     -  (16d0*xs2*loopba(61))/
     -   ((xs*xt+4d0*xu)*sqrtss(4)) + 
     -  (loopba(16)*ratss(103))/sqrtss(4) + 
     -  (4d0*(-2d0+xs)*(-2d0+xt)*loopba(41))/
     -   (sqrtss(2)*sqrtss(4)) - 
     -  16d0*xs*(loopba(7)/sqrtss(4) + 
     -     loopba(9)/sqrtss(5)) + 
     -  16d0*(-2d0+xs)*
     -   (loopba(20)/sqrtss(4) + 
     -     loopba(31)/sqrtss(5)) + 
     -  (4d0*(-2d0+xs)**2*
     -     (loopba(21)/sqrtss(4) + 
     -       loopba(32)/sqrtss(5)))/sqrtss(1) - 
     -  16d0*(-2d0+xs)*
     -   (loopba(17)/sqrtss(4) + 
     -     loopba(39)/sqrtss(4) + 
     -     loopba(28)/sqrtss(5) + 
     -     loopba(49)/sqrtss(5)) + 
     -  (16d0*xs2*loopba(62))/
     -   ((4d0*xt+xs*xu)*sqrtss(5)) + 
     -  (loopba(27)*ratss(104))/sqrtss(5) + 
     -  (4d0*(-2d0+xs)*(-2d0+xu)*loopba(51))/
     -   (sqrtss(3)*sqrtss(5)) - 
     -  (8d0*(2d0*xs+xt*xu)*loopba(8))/sqrtss(6) + 
     -  (loopba(42)*ratss(110))/sqrtss(6) + 
     -  (ratss(113)*(loopba(43) - loopba(46) + 
     -       loopba(52) - 
     -       ((-2d0+xt)*loopba(47))/
     -        (4d0*sqrtss(2)) - 
     -       ((-2d0+xu)*loopba(54))/(4d0*sqrtss(3))
     -       ))/sqrtss(6) + 
     -  (8d0*xs*(2d0*xs+(-4d0+xs)*xt2)*
     -     loopba(55))/
     -   (xt*(xs*xt+4d0*xu)*sqrtss(7)) + 
     -  (8*xs*(2d0*xs+(-4d0+xs)*xu2)*
     -     loopba(56))/
     -   (xu*(4d0*xt+xs*xu)*sqrtss(8)) + 
     -  (loopba(57)*ratss(91))/sqrtss(9) + 
     -  (loopba(58)*ratss(100))/sqrtss(10) + 
     -  (loopba(59)*ratss(92))/sqrtss(11) + 
     -  (loopba(60)*ratss(102))/sqrtss(12)
      ! +--+,-++- (2 -> 2)
      amp(4)=4d0+(4d0*loopba(3)**4)/xu2 + 
     -  (8d0*(8d0-xs2-xt2+xu)*loopba(19))/
     -   xu2-(8d0*(4d0-xs2+3d0*xt+xu)*
     -     (loopba(68) + loopba(80)/2d0))/xu2 - 
     -  (8d0*(4d0+3d0*xs-xt2+xu)*
     -     (loopba(70) + loopba(82)/2d0))/xu2 + 
     -  (loopba(38) + loopba(69))*ratss(47) + 
     -  (loopba(15) + loopba(67))*ratss(48) + 
     -  loopba(50)*ratss(53) + 
     -  loopba(53)*ratss(54) + 
     -  (loopba(18) + loopba(40))*ratss(55) + 
     -  (loopba(2)**4/4d0 - loopba(81))*
     -   ratss(62) + 
     -  (loopba(1)**4/4d0 - loopba(79))*
     -   ratss(63) + loopba(4)*ratss(68) + 
     -  loopba(5)*ratss(69) + 
     -  loopba(6)*ratss(71) + 
     -  (-6*loopba(23) + loopba(24))*ratss(116) + 
     -  loopba(36)*ratss(122) + 
     -  loopba(13)*ratss(123) + 
     -  loopba(34)*ratss(128) + 
     -  loopba(11)*ratss(129) + 
     -  (-2d0*loopba(3)*loopba(22) + 
     -     (loopba(3)*loopba(25))/3d0)*ratss(132) 
     -   - 2d0*loopba(1)*loopba(33)*ratss(134) - 
     -  2d0*loopba(2)*loopba(10)*ratss(136) + 
     -  loopba(1)*loopba(37)*ratss(143) + 
     -  loopba(2)*loopba(14)*ratss(144) + 
     -  (4d0*(12d0*xs-xs3-xt2+4d0*xu)*
     -     loopba(74))/(xu2*sqrtss(1)) + 
     -  (loopba(33)*ratss(77))/sqrtss(1) + 
     -  (loopba(12)*ratss(83))/sqrtss(1) + 
     -  (loopba(37)*ratss(89))/sqrtss(1) - 
     -  (2d0*(xs2+xt2-4d0*xu)*
     -     (((-2d0+xs)*loopba(73))/sqrtss(1) + 
     -       ((-2d0+xt)*loopba(75))/sqrtss(2)))/
     -   xu2 + (4d0*(-xs2+12d0*xt-xt3 + 
     -       4d0*xu)*loopba(76))/(xu2*sqrtss(2)) 
     -   + (loopba(10)*ratss(78))/sqrtss(2) + 
     -  (loopba(35)*ratss(84))/sqrtss(2) + 
     -  (loopba(14)*ratss(90))/sqrtss(2) + 
     -  4d0*(2d0*xs*xt+xu2)*
     -   (((-4d0+xs)*loopba(1))/
     -      (xt*xu*sqrtss(1)) + 
     -     ((-4d0+xt)*loopba(2))/
     -      (xs*xu*sqrtss(2)) + 
     -     ((-4d0+xu)*loopba(3))/(xs*xt*sqrtss(3))
     -     ) + (4d0*xu2*(-16d0+xu2)*
     -     loopba(3)**3)/
     -   ((4d0*xt+xs*xu)*(4d0*xs+xt*xu)*sqrtss(3))
     -    - (8d0*(xs*xt+2d0*xu)*loopba(7))/
     -   sqrtss(4) + 
     -  (loopba(16)*ratss(109))/sqrtss(4) + 
     -  (ratss(112)*(loopba(17) - loopba(20) + 
     -       loopba(39) - 
     -       ((-2d0+xs)*loopba(21))/
     -        (4d0*sqrtss(1)) - 
     -       ((-2d0+xt)*loopba(41))/(4d0*sqrtss(2))
     -       ))/sqrtss(4) + 
     -  (16d0*xu2*loopba(65))/
     -   ((4d0*xt+xs*xu)*sqrtss(5)) + 
     -  (loopba(27)*ratss(107))/sqrtss(5) + 
     -  (4D0*(-2d0+xs)*(-2D0+xu)*loopba(32))/
     -   (sqrtss(1)*sqrtss(5)) - 
     -  16D0*xu*(loopba(9)/sqrtss(5) + 
     -     loopba(8)/sqrtss(6)) + 
     -  16D0*(-2D0+xu)*
     -   (loopba(31)/sqrtss(5) + 
     -     loopba(46)/sqrtss(6)) - 
     -  16D0*(-2D0+xu)*
     -   (loopba(28)/sqrtss(5) + 
     -     loopba(49)/sqrtss(5) + 
     -     loopba(43)/sqrtss(6) + 
     -     loopba(52)/sqrtss(6)) + 
     -  (4D0*(-2D0+xu)**2*
     -     (loopba(51)/sqrtss(5) + 
     -       loopba(54)/sqrtss(6)))/sqrtss(3) + 
     -  (16D0*xu2*loopba(66))/
     -   ((4D0*xs+xt*xu)*sqrtss(6)) + 
     -  (loopba(42)*ratss(108))/sqrtss(6) + 
     -  (4D0*(-2D0+xt)*(-2D0+xu)*loopba(47))/
     -   (sqrtss(2)*sqrtss(6)) + 
     -  (loopba(55)*ratss(97))/sqrtss(7) + 
     -  (loopba(56)*ratss(95))/sqrtss(8) + 
     -  (loopba(57)*ratss(99))/sqrtss(9) + 
     -  (loopba(58)*ratss(96))/sqrtss(10) + 
     -  (8D0*xu*(xs2*(-4D0+xu)+2D0*xu)*
     -     loopba(59))/
     -   (xs*(4D0*xt+xs*xu)*sqrtss(11)) + 
     -  (8D0*xu*(xt2*(-4D0+xu)+2D0*xu)*
     -     loopba(60))/
     -   (xt*(4D0*xs+xt*xu)*sqrtss(12))
      ! +-+-,-+-+
      ! t<->u from amp(4)
      amp(5)=4d0+(4d0*loopba(2)**4)/xt2 + 
     -  (8D0*(8D0-xs2+xt-xu2)*loopba(30))/
     -   xt2-(8D0*(4D0-xs2+xt+3D0*xu)*
     -     (loopba(67) + loopba(79)/2d0))/xt2 - 
     -  (8D0*(4D0+3D0*xs+xt-xu2)*
     -     (loopba(72) + loopba(84)/2d0))/xt2 + 
     -  (loopba(38) + loopba(71))*ratss(45) + 
     -  (loopba(26) + loopba(68))*ratss(46) + 
     -  loopba(40)*ratss(51) + 
     -  loopba(44)*ratss(52) + 
     -  (loopba(29) + loopba(50))*ratss(57) + 
     -  (loopba(3)**4/4d0 - loopba(83))*
     -   ratss(60) + 
     -  (loopba(1)**4/4d0 - loopba(80))*
     -   ratss(61) + loopba(4)*ratss(66) + 
     -  loopba(6)*ratss(67) + 
     -  loopba(5)*ratss(70) + 
     -  (-6D0*loopba(11) + loopba(13))*ratss(115) + 
     -  loopba(36)*ratss(120) + 
     -  loopba(24)*ratss(121) + 
     -  loopba(34)*ratss(126) + 
     -  loopba(23)*ratss(127) + 
     -  (-2D0*loopba(2)*loopba(10) + 
     -     (loopba(2)*loopba(14))/3d0)*ratss(131) 
     -   - 2D0*loopba(1)*loopba(33)*ratss(133) - 
     -  2D0*loopba(3)*loopba(22)*ratss(138) + 
     -  loopba(1)*loopba(37)*ratss(141) + 
     -  loopba(3)*loopba(25)*ratss(142) + 
     -  (4D0*(12D0*xs-xs3+4D0*xt-xu2)*
     -     loopba(73))/(xt2*sqrtss(1)) + 
     -  (loopba(33)*ratss(75))/sqrtss(1) + 
     -  (loopba(12)*ratss(81))/sqrtss(1) + 
     -  (loopba(37)*ratss(87))/sqrtss(1) + 
     -  (4D0*xt2*(-16D0+xt2)*loopba(2)**3)/
     -   ((xs*xt+4D0*xu)*(4D0*xs+xt*xu)*sqrtss(2))
     -    + 4D0*(xt2+2D0*xs*xu)*
     -   (((-4D0+xs)*loopba(1))/
     -      (xt*xu*sqrtss(1)) + 
     -     ((-4D0+xt)*loopba(2))/
     -      (xs*xu*sqrtss(2)) + 
     -     ((-4D0+xu)*loopba(3))/(xs*xt*sqrtss(3))
     -     ) - (2D0*(xs2-4D0*xt+xu2)*
     -     (((-2D0+xs)*loopba(74))/sqrtss(1) + 
     -       ((-2D0+xu)*loopba(77))/sqrtss(3)))/
     -   xt2 + (4D0*(-xs2 + 4D0*xt + 12D0*xu - 
     -       xu3)*loopba(78))/(xt2*sqrtss(3)) 
     -   + (loopba(22)*ratss(76))/sqrtss(3) + 
     -  (loopba(48)*ratss(82))/sqrtss(3) + 
     -  (loopba(25)*ratss(88))/sqrtss(3) + 
     -  (16D0*xt2*loopba(63))/
     -   ((xs*xt+4D0*xu)*sqrtss(4)) + 
     -  (loopba(16)*ratss(105))/sqrtss(4) + 
     -  (4D0*(-2D0+xs)*(-2D0+xt)*loopba(21))/
     -   (sqrtss(1)*sqrtss(4)) - 
     -  (8D0*(2D0*xt+xs*xu)*loopba(9))/sqrtss(5) + 
     -  (loopba(27)*ratss(111))/sqrtss(5) + 
     -  (ratss(114)*(loopba(28) - loopba(31) + 
     -       loopba(49) - 
     -       ((-2D0+xs)*loopba(32))/
     -        (4d0*sqrtss(1)) - 
     -       ((-2D0+xu)*loopba(51))/(4d0*sqrtss(3))
     -       ))/sqrtss(5) - 
     -  16D0*xt*(loopba(7)/sqrtss(4) + 
     -     loopba(8)/sqrtss(6)) + 
     -  16D0*(-2D0+xt)*
     -   (loopba(20)/sqrtss(4) + 
     -     loopba(46)/sqrtss(6)) + 
     -  (4D0*(-2D0+xt)**2*
     -     (loopba(41)/sqrtss(4) + 
     -       loopba(47)/sqrtss(6)))/sqrtss(2) - 
     -  16D0*(-2D0+xt)*
     -   (loopba(17)/sqrtss(4) + 
     -     loopba(39)/sqrtss(4) + 
     -     loopba(43)/sqrtss(6) + 
     -     loopba(52)/sqrtss(6)) + 
     -  (16D0*xt2*loopba(64))/
     -   ((4D0*xs+xt*xu)*sqrtss(6)) + 
     -  (loopba(42)*ratss(106))/sqrtss(6) + 
     -  (4D0*(-2D0+xt)*(-2D0+xu)*loopba(54))/
     -   (sqrtss(3)*sqrtss(6)) + 
     -  (loopba(55)*ratss(93))/sqrtss(7) + 
     -  (loopba(56)*ratss(98))/sqrtss(8) + 
     -  (8D0*xt*(xs2*(-4D0+xt)+2D0*xt)*
     -     loopba(57))/
     -   (xs*(xs*xt+4D0*xu)*sqrtss(9)) + 
     -  (8D0*xt*(2D0*xt+(-4D0+xt)*xu2)*
     -     loopba(58))/
     -   (xu*(4D0*xs+xt*xu)*sqrtss(10)) + 
     -  (loopba(59)*ratss(101))/sqrtss(11) + 
     -  (loopba(60)*ratss(94))/sqrtss(12)
      return
      end

      ! rational coefficients in UT basis
      double precision function rat1_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat1_UT2L=-3d0*(xj-4d0)*((xi-xk-xi*xj)/(xj*(xi-1d0)**2-4d0*xi**2)
     $     +(xk-xi-xk*xj)/(xj*(xk-1d0)**2-4d0*xk**2))
      return
      end

      double precision function rat2_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat2_UT2L=2d0/xi+9d0/xj+36d0/(xi*xj)+30d0/xk-8d0/(xi*xk)+ 
     -     (112d0+15d0*xi+13d0*xj)/(2d0*(xi*xj+4d0*xk))
     -     +(20d0+xi-2d0*xj)/(4d0*xj+xi*xk)
      return
      end

      double precision function rat3_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat3_UT2L=(-2d0*(-4d0+xi)**2*(-xj+xk)**2)/
     $     ((xi*xj+4d0*xk)*(4d0*xj+xi*xk))
      return
      end

      double precision function rat4_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat4_UT2L=(2d0*(-4d0+xi)*(xj**2+xk**2))/(xi*xj*xk)
      return
      end

      double precision function rat5_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat5_UT2L=(-93d0/xi-68d0/xj-88d0/(xi*xj)-135d0/xk-168d0/(xi*xk)- 
     -    (116d0+8d0*xi+21d0*xj)/(xi*xj+4d0*xk)
     -     -(44d0+xi+13d0*xj)/(4d0*xi+xj*xk))/12d0
      return
      end

      double precision function rat6_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat6_UT2L=-4d0+4d0/xj+(16d0-6d0*(-3d0+xi)*xj-6d0*xk)
     -     /(xi*(-1d0+xj)**2-4d0*xj2)+ 
     -     (2d0+xi*(-3d0+xj)+xk)/(-4d0*xi2+(-1d0+xi)**2*xj)+ 
     -     (16d0-6d0*xi-6d0*xj*(-3d0+xk))/(-4d0*xj2+(-1d0+xj)**2*xk)+ 
     -     (2d0+xi+(-3d0+xj)*xk)/(xj*(-1d0+xk)**2-4d0*xk2)+ 
     -     (4d0*xj*(xi**3-2d0*xi*xk+xk**3))/(xi2*xk2)
      return
      end

      double precision function rat7_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision rat5_UT2L
      external rat5_UT2L
      rat7_UT2L=-6d0*rat5_UT2L(xi,xj,xk)-32d0*(1d0/xi+1d0/xj+1d0/xk)
      return
      end

      double precision function rat8_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat8_UT2L=8d0/xi+8d0/xj-88d0/(xi*xj)-34d0/xk
     -     -(72d0-9d0*xk)/(xi*xj+4d0*xk)
      return
      end

      double precision function rat9_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xj2
      xj2=xj**2
      rat9_UT2L=37d0-8d0*xi-(2d0*xi*(1d0+xj))/(xi*(-1d0+xj)**2-4d0*xj2)+
     -  (-8d0+(17d0-9d0*xi)*xi)/xk-(2d0*(4d0+xk))/xj-(-4d0*xi+(28d0
     -     +9d0*xi)*xk)/(xi*xj+4d0*xk)
      return
      end

      double precision function rat10_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      xixj=xi*xj
      rat10_UT2L=56d0+(4d0*(1d0+2d0*xi)*xj)/(-4d0*xi2+(-1d0+xi)**2*xj)
     -     +(4d0*xi*(1d0+2d0*xj))/(xi*(-1d0+xj)**2-4d0*xj2)+ 
     -     (4d0*xixj*(xixj-(-4d0+xk)*xk))/xk2
      return
      end

      double precision function rat11_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat11_UT2L=4d0*(1d0-xj)-(2d0*xi**2)/xk
     -     -(4d0*(4d0*xk+xi*xk))/(xi*xj+4d0*xk)
      return
      end

      double precision function rat12_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat12_UT2L=4d0+(8d0*(1d0+xj))/xi+(8d0*(-4d0+3d0*xj+xj**2))/xi**2
      return
      end

      double precision function rat13_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat13_UT2L=-12d0+48d0/xj-128d0/(xi*xj)+(24d0*(3d0+xj))/xi 
     -     -(108d0+24d0*xj)/xi**2-16d0/xk+(36d0*(4d0-xi))/(xi*xj+4d0*xk)
      return
      end

      double precision function rat14_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat14_UT2L=24d0+(36d0-48d0*xi*xj)/xk**2
     -     -102d0/xk+(-64d0+34d0*xk)/(xi*xj)
      return
      end

      double precision function rat15_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat15_UT2L=2d0+(4d0+4d0*xj)/xi+(4d0*(-4d0+3d0*xj+xj**2))/xi**2
      return
      end

      double precision function rat16_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat16_UT2L=38d0-(152d0-48d0*xj)/xi-48d0/xj-(8d0*xj2)/xi2- 
     -     (4d0-12d0*xi)/(-4d0*xi2+(-1d0+xi)**2*xj)+(4d0*xj2)/xk2
     -     -(48d0+8d0*xj)/xk-(8d0*(4d0-5d0*xi+5d0*xj-2d0*xj*xk))
     -     /(-4d0*xj2+(-1d0+xj)**2*xk)-(4d0+2d0*xi-2d0*(1d0+xj)*xk)
     -     /(xj*(-1d0+xk)**2-4d0*xk2)
      return
      end

      double precision function rat17_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat17_UT2L=4d0*(4d0+8d0/xj+xj2/xi2-(4d0*(2d0+xj))/(xi*(-1d0+xj)**2
     -     -4d0*xj2)+xj2/xk2-(4d0*(-3d0+xj)*xj)/(xi*xk)-(4d0*(2d0+xj))
     -     /(-4d0*xj2+(-1d0+xj)**2*xk))
      return
      end

      double precision function rat18_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xk2
      xi2=xi**2
      xk2=xk**2
      rat18_UT2L=(-4d0+xj)*xj*(40d0/xi+20d0/xj
     -     +(3d0*(-1d0+xi)**2*(1d0+xi))/(xi*(-4d0*xi2+(-1d0+xi)**2*xj))
     -     -(3d0*(1d0+xk)**2-3d0*xi*(3d0+xk2))/
     -     (xi*(xj*(-1d0+xk)**2-4d0*xk2)))
      return
      end

      double precision function rat19_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xk2
      xi2=xi**2
      xk2=xk**2
      rat19_UT2L=(-4d0+xj)*(2d0+(4d0*(-2d0+xj))/xi-(12d0*xj)/xi2)
      return
      end

      double precision function rat20_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xk2
      xi2=xi**2
      xk2=xk**2
      rat20_UT2L=(-8d0*(-4d0+xj)*(xj-xk))/(3d0*xi)- 
     -     ((-4d0+xj)*xj*(40d0/xi+20d0/xj+(3d0*(-1d0+xi)**2*(1d0+xi))
     -     /(xi*(-4d0*xi2+(-1d0+xi)**2*xj))-(3d0*(1d0+xk)**2
     -     -3d0*xi*(3d0+xk2))/(xi*(xj*(-1d0+xk)**2-4d0*xk2))))/6d0
      return
      end

      double precision function rat21_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2
      xi2=xi**2
      xj2=xj**2
      rat21_UT2L=4d0*(33d0-4d0*xi+xi*xj-2d0*xj2-(xj*(9d0+2d0*xj))/xi2
     -     +(xj*(19d0+4d0*xj))/xi+(2d0*xj-2d0*xi*(xi-xk))/(-4d0*xi2
     -     +(-1d0+xi)**2*xj)+3d0*xk)
      return
      end

      double precision function rat22_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat22_UT2L=8d0-6d0*xj+6d0*xj2+(4d0*xj*(1d0+xj)**2*(9d0+4d0*xj))
     -     /xk2+(4d0*(9d0+xj*(-11d0+5d0*xj+5d0*xj2)))/xk-2d0*xk
     -     +2d0*xj*xk+(4d0*(-xj+xk)*(-xj+xi*xj+xk))
     -     /(xi*(-1d0+xj)**2-4d0*xj2)
      return
      end

      double precision function rat23_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat23_UT2L=-264d0-4d0*xi*(-2d0+xj)+8d0*xj*(3d0+xj)+
     -     (8d0*(8d0*xi2-3d0*(1d0-xi)*xj))/(-4d0*xi2+(-1d0+xi)**2*xj)
     -     -(4d0*xj2**2)/xk2+(4d0*xj2*(14d0+xj))/xk
      return
      end

      double precision function rat24_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat24_UT2L=4d0*xixj*(xixj+4d0*xk)*(2d0/xk2+(3d0*(-6d0+xk))
     -     /(xixj*xk)-(-2d0+xi*(-1d0+xj)+xk)/(xj*(-4d0*xi2
     -     +(-1d0+xi)**2*xj))-(-2d0+(-1d0+xi)*xj+xk)
     -     /(xi*(xi*(-1d0+xj)**2-4d0*xj2)))
      return
      end

      double precision function rat25_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat25_UT2L=32d0-8d0*xixj + (16*xi2*xj2)/xk2+(64d0*xixj)/xk-16d0*xk
      return
      end

      double precision function rat26_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat26_UT2L=(28d0-90d0/xj2-74d0/xj+(-32d0+19d0*xj)/(xi*xk)
     -     +(12d0*(4d0-xj))/(xixj+4d0*xk)+(12d0*(4d0-xj))
     -     /(4d0*xi+xj*xk))/3d0
      return
      end

      double precision function rat27_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat27_UT2L=(37d0+45d0/xj-32d0/(xixj)+(73d0*(-1d0+2d0*xj))/xi+
     -     (2d0*(-63d0-175d0*xj+40d0*xj2))/xi2+1d0/xk-32d0/(xi*xk)- 
     -     (16d0*(-4d0+xi))/(xixj+4d0*xk))/6d0
      return
      end

      double precision function rat28_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat28_UT2L=-45d0+(105d0-162d0*xj)/xi+3d0/xj+32d0/(xixj)
     -     +(126d0+350d0*xj-48d0*xj2)/xi2- 
     -     1d0/xk+32d0/(xi*xk)+(16d0*(-4d0+xi))/(xixj+4d0*xk)
      return
      end

      double precision function rat29_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat29_UT2L=8d0+252d0/xi2-52d0/xi+(8d0*(-8d0+xi))/(xj*xk)
     -     +(30d0*(-4d0+xi))/(xixj+4d0*xk)+
     -     (30d0*(-4d0+xi))/(4d0*xj+xi*xk)
      return
      end

      double precision function rat30_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat30_UT2L=-10d0+29d0/xi-32d0/(xixj)+(23d0-20d0*xk)/xj+25d0/xk
     -     -32d0/(xj*xk)+(8d0*(-4d0+xj))/(xixj+4d0*xk)
     -     +(90d0-52d0*xk-40d0*xk2)/xj2
      return
      end

      double precision function rat31_UT2L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat31_UT2L=(-26d0+(59d0-52d0*xj)/xi+21d0/xj-32d0/(xixj)
     -     +(90d0+52d0*xj-40d0*xj2)/xi2+25d0/xk-32d0/(xi*xk)
     -     +(8d0*(-4d0+xi))/(xixj+4d0*xk))/3d0
      return
      end

      ! get the form factors from helicity amplitudes
      subroutine Get_FormFactors_From_HelAmps_QP(shat,that
     $     ,uhat,amp,FFs)
      implicit none
      real*16 shat,that,uhat
      complex*32 FFs(5)
      complex*32 amp(5)
      integer ihel
      ! AS(s,t,u)
      FFs(1)=0.5E0_16*(amp(1)+4E0_16*amp(2)+amp(3)+amp(4)+amp(5))
      ! DBhat111(s,t,u)
      FFs(2)=-0.5E0_16/uhat*(-amp(1)-amp(3)+amp(4)+amp(5))
      ! DBhat111(t,u,s)
      FFs(3)=-0.5E0_16/shat*(-amp(1)+amp(3)+amp(4)-amp(5))
      ! DBhat111(u,s,t)
      FFs(4)=-0.5E0_16/that*(-amp(1)+amp(3)-amp(4)+amp(5))
      ! DChat2111(s,t,u)
      FFs(5)=-0.5E0_16/(shat*uhat)*(amp(1)-4E0_16*amp(2)
     $     +amp(3)+amp(4)+amp(5))
      return
      end

      subroutine Get_TwoLoop_HelAmp_Massive_QP(xs,xt,xu,loopba,amp)
      use kinetics
      implicit none
      integer NDIM
      parameter (NDIM=84)
      real*16 xs,xt,xu
      real*16 xs2,xt2,xu2
      real*16 xs3,xt3,xu3
      complex*32 loopba(NDIM)
      complex*32 amp(5)
      complex*32 sqrtss(12)
      real*16 ratss(144)
      real*16 rat1_UT1L_QP
      external rat1_UT1L_QP
      real*16 rat1_UT2L_QP
      external rat1_UT2L_QP
      real*16 rat2_UT2L_QP
      external rat2_UT2L_QP
      real*16 rat3_UT2L_QP
      external rat3_UT2L_QP
      real*16 rat4_UT2L_QP
      external rat4_UT2L_QP
      real*16 rat5_UT2L_QP
      external rat5_UT2L_QP
      real*16 rat6_UT2L_QP
      external rat6_UT2L_QP
      real*16 rat7_UT2L_QP
      external rat7_UT2L_QP
      real*16 rat8_UT2L_QP
      external rat8_UT2L_QP
      real*16 rat9_UT2L_QP
      external rat9_UT2L_QP
      real*16 rat10_UT2L_QP
      external rat10_UT2L_QP
      real*16 rat11_UT2L_QP
      external rat11_UT2L_QP
      real*16 rat12_UT2L_QP
      external rat12_UT2L_QP
      real*16 rat13_UT2L_QP
      external rat13_UT2L_QP
      real*16 rat14_UT2L_QP
      external rat14_UT2L_QP
      real*16 rat15_UT2L_QP
      external rat15_UT2L_QP
      real*16 rat16_UT2L_QP
      external rat16_UT2L_QP
      real*16 rat17_UT2L_QP
      external rat17_UT2L_QP
      real*16 rat18_UT2L_QP
      external rat18_UT2L_QP
      real*16 rat19_UT2L_QP
      external rat19_UT2L_QP
      real*16 rat20_UT2L_QP
      external rat20_UT2L_QP
      real*16 rat21_UT2L_QP
      external rat21_UT2L_QP
      real*16 rat22_UT2L_QP
      external rat22_UT2L_QP
      real*16 rat23_UT2L_QP
      external rat23_UT2L_QP
      real*16 rat24_UT2L_QP
      external rat24_UT2L_QP
      real*16 rat25_UT2L_QP
      external rat25_UT2L_QP
      real*16 rat26_UT2L_QP
      external rat26_UT2L_QP
      real*16 rat27_UT2L_QP
      external rat27_UT2L_QP
      real*16 rat28_UT2L_QP
      external rat28_UT2L_QP
      real*16 rat29_UT2L_QP
      external rat29_UT2L_QP
      real*16 rat30_UT2L_QP
      external rat30_UT2L_QP
      real*16 rat31_UT2L_QP
      external rat31_UT2L_QP
      integer i
      xs2=xs**2
      xt2=xt**2
      xu2=xu**2
      xs3=xs**3
      xt3=xt**3
      xu3=xu**3
      sqrtss(1)=sqrt1_QP(xs)
      sqrtss(2)=sqrt1_QP(xt)
      sqrtss(3)=sqrt1_QP(xu)
      sqrtss(4)=sqrt3_QP(xs,xt)
      sqrtss(5)=sqrt3_QP(xs,xu)
      sqrtss(6)=sqrt3_QP(xt,xu)
      sqrtss(7)=sqrt4_QP(xs,xt)
      sqrtss(8)=sqrt4_QP(xs,xu)
      sqrtss(9)=sqrt4_QP(xt,xs)
      sqrtss(10)=sqrt4_QP(xt,xu)
      sqrtss(11)=sqrt4_QP(xu,xs)
      sqrtss(12)=sqrt4_QP(xu,xt)
      ratss(1)=rat1_UT1L_QP(xs,xt,xu)
      ratss(2)=rat1_UT1L_QP(xt,xu,xs)
      ratss(3)=rat1_UT1L_QP(xu,xs,xt)
      ratss(4)=rat1_UT2L_QP(xs,xt,xu)
      ratss(5)=rat1_UT2L_QP(xt,xu,xs)
      ratss(6)=rat1_UT2L_QP(xu,xs,xt)
      ratss(7)=rat2_UT2L_QP(xs,xt,xu)
      ratss(8)=rat2_UT2L_QP(xt,xu,xs)
      ratss(9)=rat2_UT2L_QP(xu,xs,xt)
      ratss(10)=rat3_UT2L_QP(xs,xt,xu)
      ratss(11)=rat3_UT2L_QP(xt,xu,xs)
      ratss(12)=rat3_UT2L_QP(xu,xs,xt)
      ratss(13)=rat4_UT2L_QP(xs,xt,xu)
      ratss(14)=rat4_UT2L_QP(xt,xu,xs)
      ratss(15)=rat4_UT2L_QP(xu,xs,xt)
      ratss(16)=rat5_UT2L_QP(xs,xt,xu)
      ratss(17)=rat5_UT2L_QP(xt,xu,xs)
      ratss(18)=rat5_UT2L_QP(xu,xs,xt)
      ratss(19)=rat6_UT2L_QP(xs,xt,xu)
      ratss(20)=rat6_UT2L_QP(xt,xu,xs)
      ratss(21)=rat6_UT2L_QP(xu,xs,xt)
      ratss(22)=rat7_UT2L_QP(xs,xt,xu)
      ratss(23)=rat7_UT2L_QP(xt,xu,xs)
      ratss(24)=rat7_UT2L_QP(xu,xs,xt)
      ratss(25)=rat8_UT2L_QP(xs,xt,xu)
      ratss(26)=rat8_UT2L_QP(xt,xu,xs)
      ratss(27)=rat8_UT2L_QP(xu,xs,xt)
      ratss(28)=rat9_UT2L_QP(xs,xt,xu)
      ratss(29)=rat9_UT2L_QP(xs,xu,xt)
      ratss(30)=rat9_UT2L_QP(xt,xs,xu)
      ratss(31)=rat9_UT2L_QP(xt,xu,xs)
      ratss(32)=rat9_UT2L_QP(xu,xs,xt)
      ratss(33)=rat9_UT2L_QP(xu,xt,xs)
      ratss(34)=rat10_UT2L_QP(xs,xt,xu)
      ratss(35)=rat10_UT2L_QP(xt,xu,xs)
      ratss(36)=rat10_UT2L_QP(xu,xs,xt)
      ratss(37)=rat11_UT2L_QP(xs,xt,xu)
      ratss(38)=rat11_UT2L_QP(xs,xu,xt)
      ratss(39)=rat11_UT2L_QP(xt,xs,xu)
      ratss(40)=rat11_UT2L_QP(xt,xu,xs)
      ratss(41)=rat11_UT2L_QP(xu,xs,xt)
      ratss(42)=rat11_UT2L_QP(xu,xt,xs)
      ratss(43)=rat12_UT2L_QP(xs,xt,xu)
      ratss(44)=rat12_UT2L_QP(xs,xu,xt)
      ratss(45)=rat12_UT2L_QP(xt,xs,xu)
      ratss(46)=rat12_UT2L_QP(xt,xu,xs)
      ratss(47)=rat12_UT2L_QP(xu,xs,xt)
      ratss(48)=rat12_UT2L_QP(xu,xt,xs)
      ratss(49)=rat13_UT2L_QP(xs,xt,xu)
      ratss(50)=rat13_UT2L_QP(xs,xu,xt)
      ratss(51)=rat13_UT2L_QP(xt,xs,xu)
      ratss(52)=rat13_UT2L_QP(xt,xu,xs)
      ratss(53)=rat13_UT2L_QP(xu,xs,xt)
      ratss(54)=rat13_UT2L_QP(xu,xt,xs)
      ratss(55)=rat14_UT2L_QP(xt,xs,xu)
      ratss(56)=rat14_UT2L_QP(xt,xu,xs)
      ratss(57)=rat14_UT2L_QP(xu,xs,xt)
      ratss(58)=rat15_UT2L_QP(xs,xt,xu)
      ratss(59)=rat15_UT2L_QP(xs,xu,xt)
      ratss(60)=rat15_UT2L_QP(xt,xs,xu)
      ratss(61)=rat15_UT2L_QP(xt,xu,xs)
      ratss(62)=rat15_UT2L_QP(xu,xs,xt)
      ratss(63)=rat15_UT2L_QP(xu,xt,xs)
      ratss(64)=rat16_UT2L_QP(xs,xt,xu)
      ratss(65)=rat16_UT2L_QP(xs,xu,xt)
      ratss(66)=rat16_UT2L_QP(xt,xs,xu)
      ratss(67)=rat16_UT2L_QP(xt,xu,xs)
      ratss(68)=rat16_UT2L_QP(xu,xs,xt)
      ratss(69)=rat16_UT2L_QP(xu,xt,xs)
      ratss(70)=rat17_UT2L_QP(xs,xt,xu)
      ratss(71)=rat17_UT2L_QP(xs,xu,xt)
      ratss(72)=rat17_UT2L_QP(xu,xs,xt)
      ratss(73)=rat18_UT2L_QP(xs,xt,xu)
      ratss(74)=rat18_UT2L_QP(xs,xu,xt)
      ratss(75)=rat18_UT2L_QP(xt,xs,xu)
      ratss(76)=rat18_UT2L_QP(xt,xu,xs)
      ratss(77)=rat18_UT2L_QP(xu,xs,xt)
      ratss(78)=rat18_UT2L_QP(xu,xt,xs)
      ratss(79)=rat19_UT2L_QP(xs,xt,xu)
      ratss(80)=rat19_UT2L_QP(xs,xu,xt)
      ratss(81)=rat19_UT2L_QP(xt,xs,xu)
      ratss(82)=rat19_UT2L_QP(xt,xu,xs)
      ratss(83)=rat19_UT2L_QP(xu,xs,xt)
      ratss(84)=rat19_UT2L_QP(xu,xt,xs)
      ratss(85)=rat20_UT2L_QP(xs,xt,xu)
      ratss(86)=rat20_UT2L_QP(xs,xu,xt)
      ratss(87)=rat20_UT2L_QP(xt,xs,xu)
      ratss(88)=rat20_UT2L_QP(xt,xu,xs)
      ratss(89)=rat20_UT2L_QP(xu,xs,xt)
      ratss(90)=rat20_UT2L_QP(xu,xt,xs)
      ratss(91)=rat21_UT2L_QP(xs,xt,xu)
      ratss(92)=rat21_UT2L_QP(xs,xu,xt)
      ratss(93)=rat21_UT2L_QP(xt,xs,xu)
      ratss(94)=rat21_UT2L_QP(xt,xu,xs)
      ratss(95)=rat21_UT2L_QP(xu,xs,xt)
      ratss(96)=rat21_UT2L_QP(xu,xt,xs)
      ratss(97)=rat22_UT2L_QP(xs,xt,xu)
      ratss(98)=rat22_UT2L_QP(xs,xu,xt)
      ratss(99)=rat22_UT2L_QP(xt,xs,xu)
      ratss(100)=rat22_UT2L_QP(xt,xu,xs)
      ratss(101)=rat22_UT2L_QP(xu,xs,xt)
      ratss(102)=rat22_UT2L_QP(xu,xt,xs)
      ratss(103)=rat23_UT2L_QP(xs,xt,xu)
      ratss(104)=rat23_UT2L_QP(xs,xu,xt)
      ratss(105)=rat23_UT2L_QP(xt,xs,xu)
      ratss(106)=rat23_UT2L_QP(xt,xu,xs)
      ratss(107)=rat23_UT2L_QP(xu,xs,xt)
      ratss(108)=rat23_UT2L_QP(xu,xt,xs)
      ratss(109)=rat24_UT2L_QP(xt,xs,xu)
      ratss(110)=rat24_UT2L_QP(xt,xu,xs)
      ratss(111)=rat24_UT2L_QP(xu,xs,xt)
      ratss(112)=rat25_UT2L_QP(xt,xs,xu)
      ratss(113)=rat25_UT2L_QP(xt,xu,xs)
      ratss(114)=rat25_UT2L_QP(xu,xs,xt)
      ratss(115)=rat26_UT2L_QP(xs,xt,xu)
      ratss(116)=rat26_UT2L_QP(xs,xu,xt)
      ratss(117)=rat26_UT2L_QP(xu,xs,xt)
      ratss(118)=rat27_UT2L_QP(xs,xt,xu)
      ratss(119)=rat27_UT2L_QP(xs,xu,xt)
      ratss(120)=rat27_UT2L_QP(xt,xs,xu)
      ratss(121)=rat27_UT2L_QP(xt,xu,xs)
      ratss(122)=rat27_UT2L_QP(xu,xs,xt)
      ratss(123)=rat27_UT2L_QP(xu,xt,xs)
      ratss(124)=rat28_UT2L_QP(xs,xt,xu)
      ratss(125)=rat28_UT2L_QP(xs,xu,xt)
      ratss(126)=rat28_UT2L_QP(xt,xs,xu)
      ratss(127)=rat28_UT2L_QP(xt,xu,xs)
      ratss(128)=rat28_UT2L_QP(xu,xs,xt)
      ratss(129)=rat28_UT2L_QP(xu,xt,xs)
      ratss(130)=rat29_UT2L_QP(xs,xt,xu)
      ratss(131)=rat29_UT2L_QP(xt,xu,xs)
      ratss(132)=rat29_UT2L_QP(xu,xt,xs)
      ratss(133)=rat30_UT2L_QP(xs,xt,xu)
      ratss(134)=rat30_UT2L_QP(xs,xu,xt)
      ratss(135)=rat30_UT2L_QP(xt,xs,xu)
      ratss(136)=rat30_UT2L_QP(xt,xu,xs)
      ratss(137)=rat30_UT2L_QP(xu,xs,xt)
      ratss(138)=rat30_UT2L_QP(xu,xt,xs)
      ratss(139)=rat31_UT2L_QP(xs,xt,xu)
      ratss(140)=rat31_UT2L_QP(xs,xu,xt)
      ratss(141)=rat31_UT2L_QP(xt,xs,xu)
      ratss(142)=rat31_UT2L_QP(xt,xu,xs)
      ratss(143)=rat31_UT2L_QP(xu,xs,xt)
      ratss(144)=rat31_UT2L_QP(xu,xt,xs)
      ! --++,++-- (2 -> 2)
      amp(1)=4E0_16*(3E0_16+(8E0_16*loopba(17))/sqrtss(4) + 
     -    (8E0_16*loopba(39))/sqrtss(4) - 
     -    (2E0_16*(-2E0_16 + xs)*loopba(21))/
     -     (sqrtss(1)*sqrtss(4)) - 
     -    (2E0_16*(-2E0_16 + xt)*loopba(41))/
     -     (sqrtss(2)*sqrtss(4)) + 
     -    (8E0_16*loopba(28))/sqrtss(5) + 
     -    (8E0_16*loopba(49))/sqrtss(5) - 
     -    (2E0_16*(-2E0_16 + xs)*loopba(32))/
     -     (sqrtss(1)*sqrtss(5)) - 
     -    (2E0_16*(-2E0_16 + xu)*loopba(51))/
     -     (sqrtss(3)*sqrtss(5)) - 
     -    4E0_16*(-loopba(1)**4/(4E0_16*xs2) - 
     -       loopba(2)**4/(4E0_16*xt2) - 
     -       loopba(3)**4/(4E0_16*xu2) + 
     -       (-(1E0_16/xs) + 2E0_16/xt + 2E0_16/xu)*loopba(4) + 
     -       (2E0_16/xs - 1E0_16/xt + 2E0_16/xu)*loopba(5) + 
     -       (2E0_16/xs + 2E0_16/xt - 1E0_16/xu)*loopba(6) + 
     -       (-8E0_16*loopba(18) - 
     -          12E0_16*loopba(1)*loopba(33) - 
     -          loopba(36) + 
     -          2E0_16*loopba(1)*loopba(37) - 
     -          4E0_16*(loopba(34) - 
     -             (5E0_16*loopba(36))/12E0_16 + 
     -             (loopba(1)*
     -                (-6E0_16*loopba(33) + loopba(37))
     -                )/6E0_16))/(2E0_16*xs*xt) + 
     -       (-12E0_16*loopba(2)*loopba(10) - 
     -          loopba(13) + 
     -          2E0_16*loopba(2)*loopba(14) - 
     -          4E0_16*(loopba(11) - 
     -             (5E0_16*loopba(13))/12E0_16 + 
     -             (loopba(2)*
     -                (-6E0_16*loopba(10) + loopba(14))
     -                )/6E0_16) - 8E0_16*loopba(44))/
     -        (2E0_16*xt*xu) + 
     -       (-12E0_16*loopba(3)*loopba(22) - 
     -          loopba(24) + 
     -          2E0_16*loopba(3)*loopba(25) - 
     -          4E0_16*(loopba(23) - 
     -             (5E0_16*loopba(24))/12E0_16 + 
     -             (loopba(3)*
     -                (-6E0_16*loopba(22) + loopba(25))
     -                )/6E0_16) - 8E0_16*loopba(50))/
     -        (2E0_16*xs*xu) + 
     -       (2*(((xs*xt + 4*xu)*loopba(16))/xu + 
     -            loopba(20)))/sqrtss(4) + 
     -       (2E0_16*(((4E0_16*xt + xs*xu)*loopba(27))/xt + 
     -            loopba(31)))/sqrtss(5) + 
     -       (2E0_16*(((4E0_16*xs + xt*xu)*loopba(42))/xs + 
     -            loopba(46)))/sqrtss(6)) + 
     -    (8E0_16*loopba(43))/sqrtss(6) + 
     -    (8E0_16*loopba(52))/sqrtss(6) - 
     -    (2E0_16*(-2E0_16 + xt)*loopba(47))/
     -     (sqrtss(2)*sqrtss(6)) - 
     -    (2E0_16*(-2E0_16 + xu)*loopba(54))/
     -     (sqrtss(3)*sqrtss(6)))
      ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+ (2 -> 2)
      amp(2)=-((6E0_16/xs + 4E0_16/xt + 6E0_16/xu)*loopba(15)) + 
     -  (4E0_16/xs + 4E0_16/xt + 8E0_16/xu)*
     -   (-loopba(3)**4/8E0_16 + loopba(19)) - 
     -  (6E0_16/xs + 6E0_16/xt + 4E0_16/xu)*loopba(26) + 
     -  (4E0_16/xs + 8E0_16/xt + 4E0_16/xu)*
     -   (-loopba(2)**4/8E0_16 + loopba(30)) - 
     -  (4E0_16/xs + 6E0_16/xt + 6E0_16/xu)*loopba(38) + 
     -  (8E0_16/xs + 4E0_16/xt + 4E0_16/xu)*
     -   (-loopba(1)**4/8E0_16 + loopba(45)) - 
     -  (4E0_16/xs + 6E0_16/xt + 6E0_16/xu)*(loopba(67)+loopba(68))-
     -  (6E0_16/xs + 4E0_16/xt + 6E0_16/xu)*(loopba(69)+loopba(70))-
     -  (6E0_16/xs + 6E0_16/xt + 4E0_16/xu)*(loopba(71)+loopba(72))-
     -  (2E0_16/xt - 2E0_16/xu)*(loopba(79)-loopba(80))-
     -  (2E0_16/xs - 2E0_16/xu)*(loopba(81)-loopba(82))-
     -  (2E0_16/xs - 2E0_16/xt)*(loopba(83)-loopba(84))+
     -  (-2E0_16*loopba(1)*loopba(33)+ 
     -     (loopba(1)*loopba(37))/3E0_16)*ratss(7) + 
     -  (-2E0_16*loopba(2)*loopba(10) + 
     -     (loopba(2)*loopba(14))/3E0_16)*ratss(8) + 
     -  (-2E0_16*loopba(3)*loopba(22) + 
     -     (loopba(3)*loopba(25))/3E0_16)*ratss(9) + 
     -  loopba(13)*ratss(16) + 
     -  loopba(24)*ratss(17) + 
     -  loopba(36)*ratss(18) + 
     -  loopba(5)*ratss(19) + 
     -  loopba(6)*ratss(20) + 
     -  loopba(4)*ratss(21) + 
     -  loopba(11)*ratss(22) + 
     -  loopba(23)*ratss(23) + 
     -  loopba(34)*ratss(24) + 
     -  loopba(18)*ratss(25) + 
     -  loopba(44)*ratss(26) + 
     -  loopba(50)*ratss(27) + 
     -  (((4E0_16*(-2E0_16 + xs))/xu - (2E0_16*xu)/xt)*
     -     loopba(73))/sqrtss(1) + 
     -  (((4E0_16*(-2E0_16 + xs))/xt - (2E0_16*xt)/xu)*
     -     loopba(74))/sqrtss(1) + 
     -  (2*loopba(1)*ratss(1) + 
     -     (loopba(33) - loopba(37)/6E0_16)*ratss(6))/
     -   sqrtss(1) - 
     -  (loopba(1)**3*ratss(10))/(2E0_16*sqrtss(1)) + 
     -  (loopba(12)*ratss(13))/sqrtss(1) + 
     -  (((4E0_16*(-2E0_16 + xt))/xu - (2E0_16*xu)/xs)*
     -     loopba(75))/sqrtss(2) + 
     -  (((4E0_16*(-2E0_16 + xt))/xs - (2E0_16*xs)/xu)*
     -     loopba(76))/sqrtss(2) + 
     -  (2E0_16*loopba(2)*ratss(2) + 
     -     (loopba(10) - loopba(14)/6E0_16)*ratss(4))/
     -   sqrtss(2) - 
     -  (loopba(2)**3*ratss(11))/(2E0_16*sqrtss(2)) + 
     -  (loopba(35)*ratss(14))/sqrtss(2) + 
     -  (((-2E0_16*xt)/xs + (4E0_16*(-2E0_16 + xu))/xt)*
     -     loopba(77))/sqrtss(3) + 
     -  (((-2E0_16*xs)/xt + (4E0_16*(-2E0_16 + xu))/xs)*
     -     loopba(78))/sqrtss(3) + 
     -  (2E0_16*loopba(3)*ratss(3) + 
     -     (loopba(22) - loopba(25)/6E0_16)*ratss(5))/
     -   sqrtss(3) - 
     -  (loopba(3)**3*ratss(12))/(2E0_16*sqrtss(3)) + 
     -  (loopba(48)*ratss(15))/sqrtss(3) - 
     -  (2E0_16*xs*xt*loopba(7))/sqrtss(4) + 
     -  ((32E0_16+(16E0_16*xs*xt)/xu)*loopba(17))/
     -   sqrtss(4) - 
     -  ((32E0_16+(16E0_16*xs*xt)/xu)*loopba(20))/
     -   sqrtss(4) + 
     -  ((32E0_16+(16E0_16*xs*xt)/xu)*loopba(39))/
     -   sqrtss(4) + 
     -  (loopba(16)*ratss(34))/sqrtss(4) + 
     -  (loopba(61)*ratss(37))/sqrtss(4) + 
     -  (loopba(63)*ratss(39))/sqrtss(4) - 
     -  (4E0_16*(-2E0_16+xs)*(2E0_16+(xs*xt)/xu)*loopba(21))/
     -   (sqrtss(1)*sqrtss(4)) - 
     -  (4E0_16*(-2E0_16+xt)*(2E0_16+(xs*xt)/xu)*loopba(41))/
     -   (sqrtss(2)*sqrtss(4)) - 
     -  (2E0_16*xs*xu*loopba(9))/sqrtss(5) + 
     -  ((32E0_16+(16E0_16*xs*xu)/xt)*loopba(28))/
     -   sqrtss(5) - 
     -  ((32E0_16+(16E0_16*xs*xu)/xt)*loopba(31))/
     -   sqrtss(5) + 
     -  ((32E0_16+(16E0_16*xs*xu)/xt)*loopba(49))/
     -   sqrtss(5) + 
     -  (loopba(27)*ratss(36))/sqrtss(5) + 
     -  (loopba(62)*ratss(38))/sqrtss(5) + 
     -  (loopba(65)*ratss(41))/sqrtss(5) - 
     -  (4E0_16*(-2E0_16+xs)*(2E0_16+(xs*xu)/xt)*loopba(32))/
     -   (sqrtss(1)*sqrtss(5)) - 
     -  (4E0_16*(-2E0_16+xu)*(2E0_16+(xs*xu)/xt)*loopba(51))/
     -   (sqrtss(3)*sqrtss(5)) - 
     -  (2E0_16*xt*xu*loopba(8))/sqrtss(6) + 
     -  ((32E0_16+(16E0_16*xt*xu)/xs)*loopba(43))/
     -   sqrtss(6) - 
     -  ((32E0_16+(16E0_16*xt*xu)/xs)*loopba(46))/
     -   sqrtss(6) + 
     -  ((32E0_16+(16E0_16*xt*xu)/xs)*loopba(52))/
     -   sqrtss(6) + 
     -  (loopba(42)*ratss(35))/sqrtss(6) + 
     -  (loopba(64)*ratss(40))/sqrtss(6) + 
     -  (loopba(66)*ratss(42))/sqrtss(6) - 
     -  (4E0_16*(-2E0_16+xt)*(2E0_16+(xt*xu)/xs)*loopba(47))/
     -   (sqrtss(2)*sqrtss(6)) - 
     -  (4E0_16*(-2E0_16+xu)*(2E0_16+(xt*xu)/xs)*loopba(54))/
     -   (sqrtss(3)*sqrtss(6)) + 
     -  (loopba(55)*ratss(28))/sqrtss(7) + 
     -  (loopba(56)*ratss(29))/sqrtss(8) + 
     -  (loopba(57)*ratss(30))/sqrtss(9) + 
     -  (loopba(58)*ratss(31))/sqrtss(10) + 
     -  (loopba(59)*ratss(32))/sqrtss(11) + 
     -  (loopba(60)*ratss(33))/sqrtss(12)
      ! ++++,---- (2 -> 2)
      amp(3)=4E0_16+(4E0_16*loopba(1)**4)/xs2 + 
     -  (8E0_16*(8E0_16+xs-xt2-xu2)*loopba(45))/
     -   xs2 - (8E0_16*(4E0_16+xs-xt2+3E0_16*xu)*
     -     (loopba(69) + loopba(81)/2E0_16))/xs2 - 
     -  (8E0_16*(4E0_16+xs+3E0_16*xt-xu2)*
     -     (loopba(71) + loopba(83)/2E0_16))/xs2 + 
     -  (loopba(15) + loopba(72))*ratss(43) + 
     -  (loopba(26) + loopba(70))*ratss(44) + 
     -  loopba(18)*ratss(49) + 
     -  loopba(29)*ratss(50) + 
     -  (loopba(44) + loopba(53))*ratss(56) + 
     -  (loopba(3)**4/4E0_16 - loopba(84))*
     -   ratss(58) + 
     -  (loopba(2)**4/4E0_16 - loopba(82))*
     -   ratss(59) + loopba(5)*ratss(64) + 
     -  loopba(6)*ratss(65) + 
     -  loopba(4)*ratss(72) + 
     -  (-6E0_16*loopba(34) + loopba(36))*ratss(117) + 
     -  loopba(13)*ratss(118) + 
     -  loopba(24)*ratss(119) + 
     -  loopba(11)*ratss(124) + 
     -  loopba(23)*ratss(125) + 
     -  (-2E0_16*loopba(1)*loopba(33) + 
     -     (loopba(1)*loopba(37))/3E0_16)*ratss(130) 
     -   - 2*loopba(2)*loopba(10)*ratss(135) - 
     -  2E0_16*loopba(3)*loopba(22)*ratss(137) + 
     -  loopba(2)*loopba(14)*ratss(139) + 
     -  loopba(3)*loopba(25)*ratss(140) + 
     -  (4E0_16*xs2*(-16E0_16+xs2)*loopba(1)**3)/
     -   ((xs*xt+4E0_16*xu)*(4E0_16*xt+xs*xu)*sqrtss(1))
     -    + (4E0_16*(4E0_16*xs+12E0_16*xt-xt3-xu2)*
     -     loopba(75))/(xs2*sqrtss(2)) + 
     -  (loopba(10)*ratss(73))/sqrtss(2) + 
     -  (loopba(35)*ratss(79))/sqrtss(2) + 
     -  (loopba(14)*ratss(85))/sqrtss(2) + 
     -  4E0_16*(xs2+2E0_16*xt*xu)*
     -   (((-4E0_16+xs)*loopba(1))/
     -      (xt*xu*sqrtss(1)) + 
     -     ((-4E0_16+xt)*loopba(2))/
     -      (xs*xu*sqrtss(2)) + 
     -     ((-4E0_16+xu)*loopba(3))/(xs*xt*sqrtss(3))
     -     ) - (2E0_16*(-4E0_16*xs+xt2+xu2)*
     -     (((-2E0_16+xt)*loopba(76))/sqrtss(2) + 
     -       ((-2E0_16+xu)*loopba(78))/sqrtss(3)))/
     -   xs2 + (4E0_16*(4E0_16*xs-xt2+12E0_16*xu - 
     -       xu3)*loopba(77))/(xs2*sqrtss(3)) 
     -   + (loopba(22)*ratss(74))/sqrtss(3) + 
     -  (loopba(48)*ratss(80))/sqrtss(3) + 
     -  (loopba(25)*ratss(86))/sqrtss(3) + 
     -  (16E0_16*xs2*loopba(61))/
     -   ((xs*xt+4E0_16*xu)*sqrtss(4)) + 
     -  (loopba(16)*ratss(103))/sqrtss(4) + 
     -  (4E0_16*(-2E0_16+xs)*(-2E0_16+xt)*loopba(41))/
     -   (sqrtss(2)*sqrtss(4)) - 
     -  16E0_16*xs*(loopba(7)/sqrtss(4) + 
     -     loopba(9)/sqrtss(5)) + 
     -  16E0_16*(-2E0_16+xs)*
     -   (loopba(20)/sqrtss(4) + 
     -     loopba(31)/sqrtss(5)) + 
     -  (4E0_16*(-2E0_16+xs)**2*
     -     (loopba(21)/sqrtss(4) + 
     -       loopba(32)/sqrtss(5)))/sqrtss(1) - 
     -  16E0_16*(-2E0_16+xs)*
     -   (loopba(17)/sqrtss(4) + 
     -     loopba(39)/sqrtss(4) + 
     -     loopba(28)/sqrtss(5) + 
     -     loopba(49)/sqrtss(5)) + 
     -  (16E0_16*xs2*loopba(62))/
     -   ((4E0_16*xt+xs*xu)*sqrtss(5)) + 
     -  (loopba(27)*ratss(104))/sqrtss(5) + 
     -  (4E0_16*(-2E0_16+xs)*(-2E0_16+xu)*loopba(51))/
     -   (sqrtss(3)*sqrtss(5)) - 
     -  (8E0_16*(2E0_16*xs+xt*xu)*loopba(8))/sqrtss(6) + 
     -  (loopba(42)*ratss(110))/sqrtss(6) + 
     -  (ratss(113)*(loopba(43) - loopba(46) + 
     -       loopba(52) - 
     -       ((-2E0_16+xt)*loopba(47))/
     -        (4E0_16*sqrtss(2)) - 
     -       ((-2E0_16+xu)*loopba(54))/(4E0_16*sqrtss(3))
     -       ))/sqrtss(6) + 
     -  (8E0_16*xs*(2E0_16*xs+(-4E0_16+xs)*xt2)*
     -     loopba(55))/
     -   (xt*(xs*xt+4E0_16*xu)*sqrtss(7)) + 
     -  (8*xs*(2E0_16*xs+(-4E0_16+xs)*xu2)*
     -     loopba(56))/
     -   (xu*(4E0_16*xt+xs*xu)*sqrtss(8)) + 
     -  (loopba(57)*ratss(91))/sqrtss(9) + 
     -  (loopba(58)*ratss(100))/sqrtss(10) + 
     -  (loopba(59)*ratss(92))/sqrtss(11) + 
     -  (loopba(60)*ratss(102))/sqrtss(12)
      ! +--+,-++- (2 -> 2)
      amp(4)=4E0_16+(4E0_16*loopba(3)**4)/xu2 + 
     -  (8E0_16*(8E0_16-xs2-xt2+xu)*loopba(19))/
     -   xu2-(8E0_16*(4E0_16-xs2+3E0_16*xt+xu)*
     -     (loopba(68) + loopba(80)/2E0_16))/xu2 - 
     -  (8E0_16*(4E0_16+3E0_16*xs-xt2+xu)*
     -     (loopba(70) + loopba(82)/2E0_16))/xu2 + 
     -  (loopba(38) + loopba(69))*ratss(47) + 
     -  (loopba(15) + loopba(67))*ratss(48) + 
     -  loopba(50)*ratss(53) + 
     -  loopba(53)*ratss(54) + 
     -  (loopba(18) + loopba(40))*ratss(55) + 
     -  (loopba(2)**4/4E0_16 - loopba(81))*
     -   ratss(62) + 
     -  (loopba(1)**4/4E0_16 - loopba(79))*
     -   ratss(63) + loopba(4)*ratss(68) + 
     -  loopba(5)*ratss(69) + 
     -  loopba(6)*ratss(71) + 
     -  (-6*loopba(23) + loopba(24))*ratss(116) + 
     -  loopba(36)*ratss(122) + 
     -  loopba(13)*ratss(123) + 
     -  loopba(34)*ratss(128) + 
     -  loopba(11)*ratss(129) + 
     -  (-2E0_16*loopba(3)*loopba(22) + 
     -     (loopba(3)*loopba(25))/3E0_16)*ratss(132) 
     -   - 2E0_16*loopba(1)*loopba(33)*ratss(134) - 
     -  2E0_16*loopba(2)*loopba(10)*ratss(136) + 
     -  loopba(1)*loopba(37)*ratss(143) + 
     -  loopba(2)*loopba(14)*ratss(144) + 
     -  (4E0_16*(12E0_16*xs-xs3-xt2+4E0_16*xu)*
     -     loopba(74))/(xu2*sqrtss(1)) + 
     -  (loopba(33)*ratss(77))/sqrtss(1) + 
     -  (loopba(12)*ratss(83))/sqrtss(1) + 
     -  (loopba(37)*ratss(89))/sqrtss(1) - 
     -  (2E0_16*(xs2+xt2-4E0_16*xu)*
     -     (((-2E0_16+xs)*loopba(73))/sqrtss(1) + 
     -       ((-2E0_16+xt)*loopba(75))/sqrtss(2)))/
     -   xu2 + (4E0_16*(-xs2+12E0_16*xt-xt3 + 
     -       4E0_16*xu)*loopba(76))/(xu2*sqrtss(2)) 
     -   + (loopba(10)*ratss(78))/sqrtss(2) + 
     -  (loopba(35)*ratss(84))/sqrtss(2) + 
     -  (loopba(14)*ratss(90))/sqrtss(2) + 
     -  4E0_16*(2E0_16*xs*xt+xu2)*
     -   (((-4E0_16+xs)*loopba(1))/
     -      (xt*xu*sqrtss(1)) + 
     -     ((-4E0_16+xt)*loopba(2))/
     -      (xs*xu*sqrtss(2)) + 
     -     ((-4E0_16+xu)*loopba(3))/(xs*xt*sqrtss(3))
     -     ) + (4E0_16*xu2*(-16E0_16+xu2)*
     -     loopba(3)**3)/
     -   ((4E0_16*xt+xs*xu)*(4E0_16*xs+xt*xu)*sqrtss(3))
     -    - (8E0_16*(xs*xt+2E0_16*xu)*loopba(7))/
     -   sqrtss(4) + 
     -  (loopba(16)*ratss(109))/sqrtss(4) + 
     -  (ratss(112)*(loopba(17) - loopba(20) + 
     -       loopba(39) - 
     -       ((-2E0_16+xs)*loopba(21))/
     -        (4E0_16*sqrtss(1)) - 
     -       ((-2E0_16+xt)*loopba(41))/(4E0_16*sqrtss(2))
     -       ))/sqrtss(4) + 
     -  (16E0_16*xu2*loopba(65))/
     -   ((4E0_16*xt+xs*xu)*sqrtss(5)) + 
     -  (loopba(27)*ratss(107))/sqrtss(5) + 
     -  (4E0_16*(-2E0_16+xs)*(-2E0_16+xu)*loopba(32))/
     -   (sqrtss(1)*sqrtss(5)) - 
     -  16E0_16*xu*(loopba(9)/sqrtss(5) + 
     -     loopba(8)/sqrtss(6)) + 
     -  16E0_16*(-2E0_16+xu)*
     -   (loopba(31)/sqrtss(5) + 
     -     loopba(46)/sqrtss(6)) - 
     -  16E0_16*(-2E0_16+xu)*
     -   (loopba(28)/sqrtss(5) + 
     -     loopba(49)/sqrtss(5) + 
     -     loopba(43)/sqrtss(6) + 
     -     loopba(52)/sqrtss(6)) + 
     -  (4E0_16*(-2E0_16+xu)**2*
     -     (loopba(51)/sqrtss(5) + 
     -       loopba(54)/sqrtss(6)))/sqrtss(3) + 
     -  (16E0_16*xu2*loopba(66))/
     -   ((4E0_16*xs+xt*xu)*sqrtss(6)) + 
     -  (loopba(42)*ratss(108))/sqrtss(6) + 
     -  (4E0_16*(-2E0_16+xt)*(-2E0_16+xu)*loopba(47))/
     -   (sqrtss(2)*sqrtss(6)) + 
     -  (loopba(55)*ratss(97))/sqrtss(7) + 
     -  (loopba(56)*ratss(95))/sqrtss(8) + 
     -  (loopba(57)*ratss(99))/sqrtss(9) + 
     -  (loopba(58)*ratss(96))/sqrtss(10) + 
     -  (8E0_16*xu*(xs2*(-4E0_16+xu)+2E0_16*xu)*
     -     loopba(59))/
     -   (xs*(4E0_16*xt+xs*xu)*sqrtss(11)) + 
     -  (8E0_16*xu*(xt2*(-4E0_16+xu)+2E0_16*xu)*
     -     loopba(60))/
     -   (xt*(4E0_16*xs+xt*xu)*sqrtss(12))
      ! +-+-,-+-+
      ! t<->u from amp(4)
      amp(5)=4E0_16+(4E0_16*loopba(2)**4)/xt2 + 
     -  (8E0_16*(8E0_16-xs2+xt-xu2)*loopba(30))/
     -   xt2-(8E0_16*(4E0_16-xs2+xt+3E0_16*xu)*
     -     (loopba(67) + loopba(79)/2E0_16))/xt2 - 
     -  (8E0_16*(4E0_16+3E0_16*xs+xt-xu2)*
     -     (loopba(72) + loopba(84)/2E0_16))/xt2 + 
     -  (loopba(38) + loopba(71))*ratss(45) + 
     -  (loopba(26) + loopba(68))*ratss(46) + 
     -  loopba(40)*ratss(51) + 
     -  loopba(44)*ratss(52) + 
     -  (loopba(29) + loopba(50))*ratss(57) + 
     -  (loopba(3)**4/4E0_16 - loopba(83))*
     -   ratss(60) + 
     -  (loopba(1)**4/4E0_16 - loopba(80))*
     -   ratss(61) + loopba(4)*ratss(66) + 
     -  loopba(6)*ratss(67) + 
     -  loopba(5)*ratss(70) + 
     -  (-6E0_16*loopba(11) + loopba(13))*ratss(115) + 
     -  loopba(36)*ratss(120) + 
     -  loopba(24)*ratss(121) + 
     -  loopba(34)*ratss(126) + 
     -  loopba(23)*ratss(127) + 
     -  (-2E0_16*loopba(2)*loopba(10) + 
     -     (loopba(2)*loopba(14))/3E0_16)*ratss(131) 
     -   - 2E0_16*loopba(1)*loopba(33)*ratss(133) - 
     -  2E0_16*loopba(3)*loopba(22)*ratss(138) + 
     -  loopba(1)*loopba(37)*ratss(141) + 
     -  loopba(3)*loopba(25)*ratss(142) + 
     -  (4E0_16*(12E0_16*xs-xs3+4E0_16*xt-xu2)*
     -     loopba(73))/(xt2*sqrtss(1)) + 
     -  (loopba(33)*ratss(75))/sqrtss(1) + 
     -  (loopba(12)*ratss(81))/sqrtss(1) + 
     -  (loopba(37)*ratss(87))/sqrtss(1) + 
     -  (4E0_16*xt2*(-16E0_16+xt2)*loopba(2)**3)/
     -   ((xs*xt+4E0_16*xu)*(4E0_16*xs+xt*xu)*sqrtss(2))
     -    + 4E0_16*(xt2+2E0_16*xs*xu)*
     -   (((-4E0_16+xs)*loopba(1))/
     -      (xt*xu*sqrtss(1)) + 
     -     ((-4E0_16+xt)*loopba(2))/
     -      (xs*xu*sqrtss(2)) + 
     -     ((-4E0_16+xu)*loopba(3))/(xs*xt*sqrtss(3))
     -     ) - (2E0_16*(xs2-4E0_16*xt+xu2)*
     -     (((-2E0_16+xs)*loopba(74))/sqrtss(1) + 
     -       ((-2E0_16+xu)*loopba(77))/sqrtss(3)))/
     -   xt2 + (4E0_16*(-xs2 + 4E0_16*xt + 12E0_16*xu - 
     -       xu3)*loopba(78))/(xt2*sqrtss(3)) 
     -   + (loopba(22)*ratss(76))/sqrtss(3) + 
     -  (loopba(48)*ratss(82))/sqrtss(3) + 
     -  (loopba(25)*ratss(88))/sqrtss(3) + 
     -  (16E0_16*xt2*loopba(63))/
     -   ((xs*xt+4E0_16*xu)*sqrtss(4)) + 
     -  (loopba(16)*ratss(105))/sqrtss(4) + 
     -  (4E0_16*(-2E0_16+xs)*(-2E0_16+xt)*loopba(21))/
     -   (sqrtss(1)*sqrtss(4)) - 
     -  (8E0_16*(2E0_16*xt+xs*xu)*loopba(9))/sqrtss(5) + 
     -  (loopba(27)*ratss(111))/sqrtss(5) + 
     -  (ratss(114)*(loopba(28) - loopba(31) + 
     -       loopba(49) - 
     -       ((-2E0_16+xs)*loopba(32))/
     -        (4E0_16*sqrtss(1)) - 
     -       ((-2E0_16+xu)*loopba(51))/(4E0_16*sqrtss(3))
     -       ))/sqrtss(5) - 
     -  16E0_16*xt*(loopba(7)/sqrtss(4) + 
     -     loopba(8)/sqrtss(6)) + 
     -  16E0_16*(-2E0_16+xt)*
     -   (loopba(20)/sqrtss(4) + 
     -     loopba(46)/sqrtss(6)) + 
     -  (4E0_16*(-2E0_16+xt)**2*
     -     (loopba(41)/sqrtss(4) + 
     -       loopba(47)/sqrtss(6)))/sqrtss(2) - 
     -  16E0_16*(-2E0_16+xt)*
     -   (loopba(17)/sqrtss(4) + 
     -     loopba(39)/sqrtss(4) + 
     -     loopba(43)/sqrtss(6) + 
     -     loopba(52)/sqrtss(6)) + 
     -  (16E0_16*xt2*loopba(64))/
     -   ((4E0_16*xs+xt*xu)*sqrtss(6)) + 
     -  (loopba(42)*ratss(106))/sqrtss(6) + 
     -  (4E0_16*(-2E0_16+xt)*(-2E0_16+xu)*loopba(54))/
     -   (sqrtss(3)*sqrtss(6)) + 
     -  (loopba(55)*ratss(93))/sqrtss(7) + 
     -  (loopba(56)*ratss(98))/sqrtss(8) + 
     -  (8E0_16*xt*(xs2*(-4E0_16+xt)+2E0_16*xt)*
     -     loopba(57))/
     -   (xs*(xs*xt+4E0_16*xu)*sqrtss(9)) + 
     -  (8E0_16*xt*(2E0_16*xt+(-4E0_16+xt)*xu2)*
     -     loopba(58))/
     -   (xu*(4E0_16*xs+xt*xu)*sqrtss(10)) + 
     -  (loopba(59)*ratss(101))/sqrtss(11) + 
     -  (loopba(60)*ratss(94))/sqrtss(12)
      return
      end

      ! rational coefficients in UT basis
      real*16 function rat1_UT1L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat1_UT1L_QP=2E0_16*(xi-4E0_16)*(xi*xj-xk**2)/(xj*xk)
      return
      end

      real*16 function rat1_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat1_UT2L_QP=-3E0_16*(xj-4E0_16)*((xi-xk-xi*xj)
     $     /(xj*(xi-1E0_16)**2-4E0_16*xi**2)
     $     +(xk-xi-xk*xj)/(xj*(xk-1E0_16)**2-4E0_16*xk**2))
      return
      end

      real*16 function rat2_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat2_UT2L_QP=2E0_16/xi+9E0_16/xj+36E0_16/(xi*xj)
     -     +30E0_16/xk-8E0_16/(xi*xk)+ 
     -     (112E0_16+15E0_16*xi+13E0_16*xj)/(2E0_16*(xi*xj+4E0_16*xk))
     -     +(20E0_16+xi-2E0_16*xj)/(4E0_16*xj+xi*xk)
      return
      end

      real*16 function rat3_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat3_UT2L_QP=(-2E0_16*(-4E0_16+xi)**2*(-xj+xk)**2)/
     $     ((xi*xj+4E0_16*xk)*(4E0_16*xj+xi*xk))
      return
      end

      real*16 function rat4_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat4_UT2L_QP=(2E0_16*(-4E0_16+xi)*(xj**2+xk**2))/(xi*xj*xk)
      return
      end

      real*16 function rat5_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat5_UT2L_QP=(-93E0_16/xi-68E0_16/xj-88E0_16/(xi*xj)
     -     -135E0_16/xk-168E0_16/(xi*xk)- 
     -    (116E0_16+8E0_16*xi+21E0_16*xj)/(xi*xj+4E0_16*xk)
     -     -(44E0_16+xi+13E0_16*xj)/(4E0_16*xi+xj*xk))/12E0_16
      return
      end

      real*16 function rat6_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat6_UT2L_QP=-4E0_16+4E0_16/xj+(16E0_16
     -     -6E0_16*(-3E0_16+xi)*xj-6E0_16*xk)
     -     /(xi*(-1E0_16+xj)**2-4E0_16*xj2)+ 
     -     (2E0_16+xi*(-3E0_16+xj)+xk)/(-4E0_16*xi2+(-1E0_16+xi)**2*xj)+ 
     -     (16E0_16-6E0_16*xi-6E0_16*xj*(-3E0_16+xk))
     -     /(-4E0_16*xj2+(-1E0_16+xj)**2*xk)+ 
     -     (2E0_16+xi+(-3E0_16+xj)*xk)/(xj*(-1E0_16+xk)**2-4E0_16*xk2)+ 
     -     (4E0_16*xj*(xi**3-2E0_16*xi*xk+xk**3))/(xi2*xk2)
      return
      end

      real*16 function rat7_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 rat5_UT2L_QP
      external rat5_UT2L_QP
      rat7_UT2L_QP=-6E0_16*rat5_UT2L_QP(xi,xj,xk)
     -     -32E0_16*(1E0_16/xi+1E0_16/xj+1E0_16/xk)
      return
      end

      real*16 function rat8_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat8_UT2L_QP=8E0_16/xi+8E0_16/xj-88E0_16/(xi*xj)-34E0_16/xk
     -     -(72E0_16-9E0_16*xk)/(xi*xj+4E0_16*xk)
      return
      end

      real*16 function rat9_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xj2
      xj2=xj**2
      rat9_UT2L_QP=37E0_16-8E0_16*xi-(2E0_16*xi*(1E0_16+xj))
     -     /(xi*(-1E0_16+xj)**2-4E0_16*xj2)+
     -     (-8E0_16+(17E0_16-9E0_16*xi)*xi)/xk-(2E0_16*(4E0_16+xk))/xj
     -     -(-4E0_16*xi+(28E0_16
     -     +9E0_16*xi)*xk)/(xi*xj+4E0_16*xk)
      return
      end

      real*16 function rat10_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      xixj=xi*xj
      rat10_UT2L_QP=56E0_16+(4E0_16*(1E0_16+2E0_16*xi)*xj)
     -     /(-4E0_16*xi2+(-1E0_16+xi)**2*xj)
     -     +(4E0_16*xi*(1E0_16+2E0_16*xj))
     -     /(xi*(-1E0_16+xj)**2-4E0_16*xj2)+ 
     -     (4E0_16*xixj*(xixj-(-4E0_16+xk)*xk))/xk2
      return
      end

      real*16 function rat11_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat11_UT2L_QP=4E0_16*(1E0_16-xj)-(2E0_16*xi**2)/xk
     -     -(4E0_16*(4E0_16*xk+xi*xk))/(xi*xj+4E0_16*xk)
      return
      end

      real*16 function rat12_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat12_UT2L_QP=4E0_16+(8E0_16*(1E0_16+xj))/xi
     -     +(8E0_16*(-4E0_16+3E0_16*xj+xj**2))/xi**2
      return
      end

      real*16 function rat13_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat13_UT2L_QP=-12E0_16+48E0_16/xj-128E0_16/(xi*xj)
     -     +(24E0_16*(3E0_16+xj))/xi 
     -     -(108E0_16+24E0_16*xj)/xi**2-16E0_16/xk
     -     +(36E0_16*(4E0_16-xi))/(xi*xj+4E0_16*xk)
      return
      end

      real*16 function rat14_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat14_UT2L_QP=24E0_16+(36E0_16-48E0_16*xi*xj)/xk**2
     -     -102E0_16/xk+(-64E0_16+34E0_16*xk)/(xi*xj)
      return
      end

      real*16 function rat15_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      rat15_UT2L_QP=2E0_16+(4E0_16+4E0_16*xj)/xi
     -     +(4E0_16*(-4E0_16+3E0_16*xj+xj**2))/xi**2
      return
      end

      real*16 function rat16_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat16_UT2L_QP=38E0_16-(152E0_16-48E0_16*xj)/xi
     -     -48E0_16/xj-(8E0_16*xj2)/xi2- 
     -     (4E0_16-12E0_16*xi)/(-4E0_16*xi2+(-1E0_16+xi)**2*xj)
     -     +(4E0_16*xj2)/xk2
     -     -(48E0_16+8E0_16*xj)/xk-(8E0_16*(4E0_16-5E0_16*xi
     -     +5E0_16*xj-2E0_16*xj*xk))
     -     /(-4E0_16*xj2+(-1E0_16+xj)**2*xk)
     -     -(4E0_16+2E0_16*xi-2E0_16*(1E0_16+xj)*xk)
     -     /(xj*(-1E0_16+xk)**2-4E0_16*xk2)
      return
      end

      real*16 function rat17_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat17_UT2L_QP=4E0_16*(4E0_16+8E0_16/xj+xj2/xi2
     -     -(4E0_16*(2E0_16+xj))/(xi*(-1E0_16+xj)**2
     -     -4E0_16*xj2)+xj2/xk2-(4E0_16*(-3E0_16+xj)*xj)/(xi*xk)
     -     -(4E0_16*(2E0_16+xj))
     -     /(-4E0_16*xj2+(-1E0_16+xj)**2*xk))
      return
      end

      real*16 function rat18_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xk2
      xi2=xi**2
      xk2=xk**2
      rat18_UT2L_QP=(-4E0_16+xj)*xj*(40E0_16/xi+20E0_16/xj
     -     +(3E0_16*(-1E0_16+xi)**2*(1E0_16+xi))
     -     /(xi*(-4E0_16*xi2+(-1E0_16+xi)**2*xj))
     -     -(3E0_16*(1E0_16+xk)**2-3E0_16*xi*(3E0_16+xk2))/
     -     (xi*(xj*(-1E0_16+xk)**2-4E0_16*xk2)))
      return
      end

      real*16 function rat19_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xk2
      xi2=xi**2
      xk2=xk**2
      rat19_UT2L_QP=(-4E0_16+xj)*(2E0_16+(4E0_16*(-2E0_16+xj))/xi
     -     -(12E0_16*xj)/xi2)
      return
      end

      real*16 function rat20_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xk2
      xi2=xi**2
      xk2=xk**2
      rat20_UT2L_QP=(-8E0_16*(-4E0_16+xj)*(xj-xk))/(3E0_16*xi)- 
     -     ((-4E0_16+xj)*xj*(40E0_16/xi+20E0_16/xj
     -     +(3E0_16*(-1E0_16+xi)**2*(1E0_16+xi))
     -     /(xi*(-4E0_16*xi2+(-1E0_16+xi)**2*xj))-(3E0_16*(1E0_16+xk)**2
     -     -3E0_16*xi*(3E0_16+xk2))
     -     /(xi*(xj*(-1E0_16+xk)**2-4E0_16*xk2))))/6E0_16
      return
      end

      real*16 function rat21_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2
      xi2=xi**2
      xj2=xj**2
      rat21_UT2L_QP=4E0_16*(33E0_16-4E0_16*xi+xi*xj-2E0_16*xj2
     -     -(xj*(9E0_16+2E0_16*xj))/xi2
     -     +(xj*(19E0_16+4E0_16*xj))/xi
     -     +(2E0_16*xj-2E0_16*xi*(xi-xk))/(-4E0_16*xi2
     -     +(-1E0_16+xi)**2*xj)+3E0_16*xk)
      return
      end

      real*16 function rat22_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat22_UT2L_QP=8E0_16-6E0_16*xj+6E0_16*xj2
     -     +(4E0_16*xj*(1E0_16+xj)**2*(9E0_16+4E0_16*xj))
     -     /xk2+(4E0_16*(9E0_16+xj*(-11E0_16+5E0_16*xj+5E0_16*xj2)))/xk
     -     -2E0_16*xk
     -     +2E0_16*xj*xk+(4E0_16*(-xj+xk)*(-xj+xi*xj+xk))
     -     /(xi*(-1E0_16+xj)**2-4E0_16*xj2)
      return
      end

      real*16 function rat23_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat23_UT2L_QP=-264E0_16-4E0_16*xi*(-2E0_16+xj)
     -     +8E0_16*xj*(3E0_16+xj)+
     -     (8E0_16*(8E0_16*xi2-3E0_16*(1E0_16-xi)*xj))
     -     /(-4E0_16*xi2+(-1E0_16+xi)**2*xj)
     -     -(4E0_16*xj2**2)/xk2+(4E0_16*xj2*(14E0_16+xj))/xk
      return
      end

      real*16 function rat24_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat24_UT2L_QP=4E0_16*xixj*(xixj+4E0_16*xk)*(2E0_16/xk2
     -     +(3E0_16*(-6E0_16+xk))
     -     /(xixj*xk)-(-2E0_16+xi*(-1E0_16+xj)+xk)/(xj*(-4E0_16*xi2
     -     +(-1E0_16+xi)**2*xj))-(-2E0_16+(-1E0_16+xi)*xj+xk)
     -     /(xi*(xi*(-1E0_16+xj)**2-4E0_16*xj2)))
      return
      end

      real*16 function rat25_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat25_UT2L_QP=32E0_16-8E0_16*xixj 
     -     + (16*xi2*xj2)/xk2+(64E0_16*xixj)/xk-16E0_16*xk
      return
      end

      real*16 function rat26_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat26_UT2L_QP=(28E0_16-90E0_16/xj2-74E0_16/xj
     -     +(-32E0_16+19E0_16*xj)/(xi*xk)
     -     +(12E0_16*(4E0_16-xj))/(xixj+4E0_16*xk)+(12E0_16*(4E0_16-xj))
     -     /(4E0_16*xi+xj*xk))/3E0_16
      return
      end

      real*16 function rat27_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat27_UT2L_QP=(37E0_16+45E0_16/xj-32E0_16/(xixj)
     -     +(73E0_16*(-1E0_16+2E0_16*xj))/xi+
     -     (2E0_16*(-63E0_16-175E0_16*xj+40E0_16*xj2))/xi2
     -     +1E0_16/xk-32E0_16/(xi*xk)- 
     -     (16E0_16*(-4E0_16+xi))/(xixj+4E0_16*xk))/6E0_16
      return
      end

      real*16 function rat28_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat28_UT2L_QP=-45E0_16+(105E0_16-162E0_16*xj)/xi
     -     +3E0_16/xj+32E0_16/(xixj)
     -     +(126E0_16+350E0_16*xj-48E0_16*xj2)/xi2- 
     -     1E0_16/xk+32E0_16/(xi*xk)
     -     +(16E0_16*(-4E0_16+xi))/(xixj+4E0_16*xk)
      return
      end

      real*16 function rat29_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat29_UT2L_QP=8E0_16+252E0_16/xi2-52E0_16/xi
     -     +(8E0_16*(-8E0_16+xi))/(xj*xk)
     -     +(30E0_16*(-4E0_16+xi))/(xixj+4E0_16*xk)+
     -     (30E0_16*(-4E0_16+xi))/(4E0_16*xj+xi*xk)
      return
      end

      real*16 function rat30_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat30_UT2L_QP=-10E0_16+29E0_16/xi-32E0_16/(xixj)
     -     +(23E0_16-20E0_16*xk)/xj+25E0_16/xk
     -     -32E0_16/(xj*xk)+(8E0_16*(-4E0_16+xj))/(xixj+4E0_16*xk)
     -     +(90E0_16-52E0_16*xk-40E0_16*xk2)/xj2
      return
      end

      real*16 function rat31_UT2L_QP(xi,xj,xk)
      implicit none
      real*16 xi,xj,xk
      real*16 xi2,xj2,xk2,xixj
      xixj=xi*xj
      xi2=xi**2
      xj2=xj**2
      xk2=xk**2
      rat31_UT2L_QP=(-26E0_16+(59E0_16-52E0_16*xj)/xi
     -     +21E0_16/xj-32E0_16/(xixj)
     -     +(90E0_16+52E0_16*xj-40E0_16*xj2)/xi2
     -     +25E0_16/xk-32E0_16/(xi*xk)
     -     +(8E0_16*(-4E0_16+xi))/(xixj+4E0_16*xk))/3E0_16
      return
      end
