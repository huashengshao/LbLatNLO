      module oneloopints
      implicit none
      logical init_GPLs_1L
      contains
      function f1E2Euclid_w1(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E2Euclid_w1
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E2Euclid_w1=-G(1) + G(2) + G(3) - G(4)
      return
      end function f1E2Euclid_w1

      function f1E2Euclid_w2(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E2Euclid_w2
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E2Euclid_w2=        G(1)*G(3) + G(2)*G(3) - G(1)*G(4) - 
     -  G(2)*G(4) - TWO*G(3)*G(5) + 
     -  TWO*G(4)*G(5) - G(18) + G(19) - G(21) + 
     -  G(22) + G(24) - G(25) + G(26) - G(27) + 
     -  TWO*G(28) - TWO*G(29)
      return
      end function f1E2Euclid_w2

      function f1E3Euclid_w1(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E3Euclid_w1
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E3Euclid_w1=-G(7) + G(8)
      return
      end function f1E3Euclid_w1

      function f1E3Euclid_w2(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E3Euclid_w2
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E3Euclid_w2=        zeta2 - TWO*G(6)*G(8) + G(7)*G(8) + 
     -  TWO*(G(6)*G(7) - G(34)) - G(35) - G(36)
      return
      end function f1E3Euclid_w2

      function f1E4Euclid_w2(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E4Euclid_w2
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E4Euclid_w2=        -(G(1)*G(3)) + G(2)*G(3) + G(1)*G(4) - 
     -  G(2)*G(4) + G(18) - G(19) - G(21) + 
     -  G(22) + G(24) - G(25) - G(26) + G(27)
      return
      end function f1E4Euclid_w2

      function f1E4Euclid_w3(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E4Euclid_w3
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E4Euclid_w3=        -(G(3)*G(18)) + G(4)*G(18) - G(3)*G(19) + 
     -  G(4)*G(19) + TWO*G(3)*G(20) - 
     -  TWO*G(4)*G(20) + G(3)*G(21) - 
     -  G(4)*G(21) + G(3)*G(22) - G(4)*G(22) - 
     -  TWO*G(3)*G(23) + TWO*G(4)*G(23) - 
     -  G(1)*G(24) + G(2)*G(24) + G(1)*G(25) - 
     -  G(2)*G(25) - G(1)*G(26) + G(2)*G(26) + 
     -  G(1)*G(27) - G(2)*G(27) + G(65) - G(66) + 
     -  G(67) - G(68) - TWO*G(69) + TWO*G(70) - 
     -  G(71) + G(72) - G(73) + G(74) + 
     -  TWO*G(75) - TWO*G(76) + G(77) - G(78) + 
     -  G(79) - G(80) - G(81) + G(82) - G(83) + 
     -  G(84)
      return
      end function f1E4Euclid_w3

      function f1E5Euclid_w2(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E5Euclid_w2
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E5Euclid_w2=-(G(7)*G(8)) + G(35) + G(36)
      return
      end function f1E5Euclid_w2

      function f1E5Euclid_w3(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E5Euclid_w3
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E5Euclid_w3=        -(THREE*zeta3) - zeta2*G(7) + 
     -  zeta2*G(8) + TWO*G(8)*G(34) - 
     -  G(8)*G(35) + G(7)*G(36) - 
     -  TWO*(G(7)*G(34) - TWO*G(85)) + G(86) - 
     -  G(87)
      return
      end function f1E5Euclid_w3

      function f1E6Euclid_w2(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E6Euclid_w2
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E6Euclid_w2=        -(G(1)*G(7)) + G(2)*G(7) - G(3)*G(7) + 
     -  G(4)*G(7) + G(1)*G(8) - G(2)*G(8) + 
     -  G(3)*G(8) - G(4)*G(8) + TWO*G(30) - 
     -  TWO*G(31) - TWO*G(32) + TWO*G(33)
      return
      end function f1E6Euclid_w2

      function f1E6Euclid_w3(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=151)
      double complex f1E6Euclid_w3
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E6Euclid_w3=        TWO*G(9)*G(37) - TWO*G(9)*G(38) + 
     -  TWO*G(12)*G(39) - TWO*G(14)*G(39) - 
     -  G(12)*G(40) + G(14)*G(40) - G(12)*G(41) + 
     -  G(14)*G(41) + G(11)*G(42) - G(13)*G(42) + 
     -  G(16)*G(42) - G(17)*G(42) + 
     -  TWO*G(15)*G(43) - G(16)*G(43) - 
     -  G(17)*G(43) - TWO*G(15)*G(44) + 
     -  G(16)*G(44) + G(17)*G(44) - 
     -  TWO*G(11)*G(45) + TWO*G(13)*G(45) + 
     -  G(10)*G(46) - TWO*G(10)*G(47) - 
     -  TWO*G(15)*G(48) + G(16)*G(48) + 
     -  G(17)*G(48) - G(9)*G(49) + G(16)*G(49) - 
     -  G(17)*G(49) + G(9)*G(50) - G(16)*G(50) + 
     -  G(17)*G(50) - G(10)*G(51) + 
     -  TWO*G(10)*G(52) + TWO*G(15)*G(53) - 
     -  G(16)*G(53) - G(17)*G(53) - G(9)*G(54) - 
     -  G(16)*G(54) + G(17)*G(54) + G(9)*G(55) + 
     -  G(16)*G(55) - G(17)*G(55) + 
     -  TWO*G(12)*G(56) - TWO*G(14)*G(56) - 
     -  TWO*G(10)*G(57) + TWO*G(10)*G(58) - 
     -  G(10)*G(59) - G(12)*G(59) + G(14)*G(59) + 
     -  G(10)*G(60) - G(12)*G(60) + G(14)*G(60) - 
     -  G(10)*G(61) + G(12)*G(61) - G(14)*G(61) + 
     -  G(10)*G(62) - G(12)*G(62) + G(14)*G(62) + 
     -  G(10)*G(63) + G(12)*G(63) - G(14)*G(63) - 
     -  G(10)*G(64) - G(12)*G(64) + G(14)*G(64) - 
     -  TWO*G(88) + TWO*G(89) + G(90) - G(91) + 
     -  G(92) - G(93) + TWO*G(94) - TWO*G(95) - 
     -  G(96) + G(97) - G(98) + G(99) - G(100) + 
     -  TWO*G(101) + G(102) - TWO*G(103) + 
     -  G(104) - TWO*G(105) - G(106) + 
     -  TWO*G(107) - TWO*G(108) + TWO*G(109) + 
     -  TWO*G(110) - TWO*G(111) + G(112) + 
     -  G(113) - G(114) - G(115) + G(116) - 
     -  G(117) + G(118) - G(119) + G(120) - 
     -  G(121) + G(122) - G(123) - G(124) - 
     -  G(125) + G(126) + G(127) + G(128) - 
     -  G(129) + G(130) - G(131) + G(132) + 
     -  G(133) + G(134) - G(135) - G(136) - 
     -  G(137) + G(138) - G(139) - G(140) + 
     -  G(141) + G(142) - G(143) + G(144) - 
     -  G(145) + G(146) - G(147) + TWO*G(148) - 
     -  TWO*G(149) - TWO*G(150) + TWO*G(151)
      return
      end function f1E6Euclid_w3

      subroutine GPLbasis1L(w,z,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=151)
      ! itool: 1 (FastGPL, default), 2 (handyG)
      INTEGER,INTENT(IN),OPTIONAL::itool
      INTEGER::itool_used
      INTEGER::ix
      double complex cres(NDIM)
      double complex w,z,x
      double precision zz0
      parameter (zz0=0.5d0)
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double complex a(3)
      integer s(3)
      integer init,na
      DATA init/0/
      SAVE init
      IF(PRESENT(itool))THEN
         itool_used=itool
      ELSE
         itool_used=1
      ENDIF
      a(1)=-(1/w)
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      IF(init_GPLs_1L)THEN
        cres(1)=GPL(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs_1L=.FALSE.
      ELSE
        cres(1)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      ENDIF
      a(1)=1/w
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(2)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(3)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(4)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/(w*z)
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(5)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(z/w)
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(6)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      x=w
      ix=-1
      na=1
      cres(7)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      x=z
      ix=1
      na=1
      cres(8)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(9)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(10)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(w - zz0))
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(11)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(z - zz0))
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(12)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-w + zz0)
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(13)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-z + zz0)
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(14)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + z*zz0)/(w*z - z*zz0))
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(15)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(16)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(17)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(18)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(19)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/(w*z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(20)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(21)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(22)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(23)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(24)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(25)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(26)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(27)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/(w*z)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(28)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/(w*z)
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(29)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(30)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(31)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(32)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(33)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(z/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(34)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      x=w
      ix=-1
      na=2
      cres(35)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      x=z
      ix=1
      na=2
      cres(36)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1/zz0 - zz0)/(z - zz0)
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(37)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1/zz0 - zz0)/(z - zz0)
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(38)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=(1/z - zz0)/(w - zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(39)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(40)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(41)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(42)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(43)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(44)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((TWO*zz0)/(z - zz0))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(45)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(w - zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(46)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(w - zz0))
      s(1)=-1
      a(2)=-((z + zz0)/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(47)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(z - zz0))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(48)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(z - zz0))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(49)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(z - zz0))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(50)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-w + zz0)
      s(1)=1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(51)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-w + zz0)
      s(1)=1
      a(2)=-((z + zz0)/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(52)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-z + zz0)
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(53)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-z + zz0)
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(54)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-z + zz0)
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(55)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + z*zz0)/(w*z - z*zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(56)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + z*zz0)/(w*z - z*zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(57)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + z*zz0)/(w*z - z*zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(58)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(59)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(60)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(61)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(62)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(63)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(64)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(65)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(66)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(67)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(68)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(69)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(70)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(71)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(72)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(73)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(74)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(75)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(76)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(77)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(78)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(79)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(80)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(81)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(82)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(83)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(84)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(z/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(85)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      x=w
      ix=-1
      na=3
      cres(86)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      x=z
      ix=1
      na=3
      cres(87)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=(1/z - zz0)/(w - zz0)
      s(2)=1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(88)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=(1/z - zz0)/(w - zz0)
      s(2)=1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(89)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(90)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(91)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(92)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(w - zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(93)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=(1/zz0 - zz0)/(z - zz0)
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(94)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=(1/zz0 - zz0)/(z - zz0)
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(95)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(96)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(97)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(98)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(zz0/(z - zz0))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(99)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(w - zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(100)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(w - zz0))
      s(1)=-1
      a(2)=-((z + zz0)/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(101)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(z - zz0))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(102)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0)/(z - zz0))
      s(1)=1
      a(2)=-((TWO*zz0)/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(103)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-w + zz0)
      s(1)=1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(104)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-w + zz0)
      s(1)=1
      a(2)=-((z + zz0)/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(105)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-z + zz0)
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(106)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + zz0)/(-z + zz0)
      s(1)=1
      a(2)=-((TWO*zz0)/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(107)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + z*zz0)/(w*z - z*zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(108)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + z*zz0)/(w*z - z*zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(109)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + z*zz0)/(w*z - z*zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(110)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + z*zz0)/(w*z - z*zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(111)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(112)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(113)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(114)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(115)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(116)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(117)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(118)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(119)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + z - zz0 + z*zz0)/(w - w*z - zz0 + z*zz0)
      s(1)=1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(120)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(121)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(122)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=-(zz0/(w - zz0))
      s(2)=-1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(123)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(124)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(125)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=-((1 + zz0)/(w - zz0))
      s(2)=-1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(126)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=-(zz0/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(127)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=-((1 + zz0)/(w - zz0))
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(128)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + z - (1 + z)*zz0)/((1 + z)*(w - zz0))
      s(1)=-1
      a(2)=(-1 + zz0)/(-w + zz0)
      s(2)=1
      a(3)=(-1 + zz0)/(-w + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(129)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(130)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(131)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(132)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(133)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(134)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(135)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(136)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(137)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/((z - zz0)*(1 + zz0)))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(138)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(139)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(140)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(141)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(142)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(143)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(144)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(145)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(146)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + zz0**TWO)/((-1 + zz0)*(-z + zz0))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(147)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/(z*zz0 - zz0**TWO))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=-((1 + zz0)/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(148)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/(z*zz0 - zz0**TWO))
      s(1)=1
      a(2)=-(zz0/(z - zz0))
      s(2)=1
      a(3)=(-1 + zz0)/(-z + zz0)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(149)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/(z*zz0 - zz0**TWO))
      s(1)=1
      a(2)=-((1 + zz0)/(z - zz0))
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(150)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-((1 + zz0**TWO)/(z*zz0 - zz0**TWO))
      s(1)=1
      a(2)=(-1 + zz0)/(-z + zz0)
      s(2)=1
      a(3)=-(zz0/(z - zz0))
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(151)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end
      end module
