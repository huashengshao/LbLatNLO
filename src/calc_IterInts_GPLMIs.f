      module calc_IterInts_GPLMIs
      logical init_GPLs,init_GPLs_qp
      contains

      function f1E3_w2(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex f1E3_w2
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR,ZERO
      parameter (ZERO=0d0,FOUR=4d0)
      f1E3_w2=f1E3Euclid_w2(w,z,G)
      if(xtb.GT.ZERO.and.xtb.LT.FOUR)then
         f1E3_w2=-f1E3_w2
      endif
      return
      end

      function f1E5_w3(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex f1E5_w3
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      f1E5_w3=f1E5Euclid_w3(w,z,G)
      return
      end

      function fE13_w4(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE13_w4
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE13_w4=fE13Euclid_w4(w,z,G)
      if(xsb.GT.FOUR)then
          fE13_w4=-fE13_w4
      endif
      return
      end

      function fE14_w3(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE14_w3
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE14_w3=fE14Euclid_w3(w,z,G)
      return
      end

      function fE15_w2(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE15_w2
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE15_w2=fE15Euclid_w2(w,z,G)
      if(xtb.GT.0d0.AND.xtb.LT.FOUR)then
          fE15_w2=-fE15_w2
      endif
      return
      end

      function fE17_w4(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE17_w4
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE17_w4=fE17Euclid_w4(w,z,G)
      return
      end

      function fE19_w2(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE19_w2
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE19_w2=fE19Euclid_w2(w,z,G)
      if(xsb.GT.0d0.or.xtb.GT.0d0)then
         fE19_w2=-fE19_w2
      endif
      return
      end

      function fE19_w3(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE19_w3
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE19_w3=fE19Euclid_w3(w,z,G)
      if(xsb.GT.0d0.or.xtb.GT.0d0)then
          fE19_w3=-fE19_w3
      endif
      return
      end

      function fE20_w3(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE20_w3
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE20_w3=fE20Euclid_w3(w,z,G)
      return
      end

      function fE21_w4(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE21_w4
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE21_w4=fE21Euclid_w4(w,z,G)
      return
      end

      function fE22_w3(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE22_w3
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE22_w3=fE22Euclid_w3(w,z,G)
      if(xsb.GT.0d0.or.xtb.GT.0d0)then
          fE22_w3=-fE22_w3
      endif
      return
      end

      function fE27_w4(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE27_w4
      double complex G(NDIM)
      double precision xsb,xtb
      double complex w,z
      double precision FOUR
      parameter (FOUR=4d0)
      fE27_w4=fE27Euclid_w4(w,z,G)
      if(xsb.GT.0d0.or.xtb.GT.0d0)then
         fE27_w4=-fE27_w4
      endif
      if(xsb.GT.FOUR)then
         fE27_w4=-fE27_w4
      endif
      return
      end
      
      function f1E3Euclid_w2(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex f1E3Euclid_w2
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E3Euclid_w2=        zeta2 - TWO*G(9)*G(12) + G(11)*G(12) + 
     -  TWO*(G(9)*G(11) - G(57)) - G(59) - G(60)
      return
      end function f1E3Euclid_w2

      function f1E5Euclid_w3(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex f1E5Euclid_w3
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      f1E5Euclid_w3=        -(THREE*zeta3) - zeta2*G(11) + 
     -  zeta2*G(12) + TWO*G(12)*G(57) - 
     -  G(12)*G(59) + G(11)*G(60) - 
     -  TWO*(G(11)*G(57) - TWO*G(210)) + G(212) - 
     -  G(213)
      return
      end function f1E5Euclid_w3

      function fE13Euclid_w4(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE13Euclid_w4
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE13Euclid_w4=-8*((G(13) - G(14) + G(15) - G(16))*
     -      (G(17) - G(18) - G(24) + G(25)) + 
     -     (-G(3) + G(4))*
     -      (G(61) - G(62) + G(63) - G(64) - 
     -        G(67) + G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      (G(73) + G(74) - TWO*G(75) - G(77) - 
     -        G(78) + TWO*G(79) - G(96) - G(97) + 
     -        TWO*G(98) + G(100) + G(101) - 
     -        TWO*G(102)) + G(214) - G(215) + 
     -     G(216) - G(217) - G(220) + G(221) - 
     -     G(222) + G(223) - G(232) + G(233) - 
     -     G(234) + G(235) + G(238) - G(239) + 
     -     G(240) - G(241) - G(250) + G(251) - 
     -     G(252) + G(253) + TWO*G(254) - 
     -     TWO*G(255) + G(258) - G(259) + 
     -     G(260) - G(261) - TWO*G(262) + 
     -     TWO*G(263) + G(305) - G(306) + 
     -     G(307) - G(308) - TWO*G(309) + 
     -     TWO*G(310) - G(313) + G(314) - 
     -     G(315) + G(316) + TWO*G(317) - 
     -     TWO*G(318)) + 
     -  TWO*((G(17) - G(18) - G(24) + G(25))*
     -      (-G(13) + G(14) - G(15) + G(16) + 
     -        TWO*G(48) - TWO*G(49)) + 
     -     (-G(3) + G(4))*
     -      (-G(61) + G(62) - G(63) + G(64) + 
     -        TWO*G(65) - TWO*G(66) + G(67) - 
     -        G(68) + G(69) - G(70) - TWO*G(71) + 
     -        TWO*G(72)) + 
     -     (G(1) - G(2))*
     -      (-G(73) - G(74) + TWO*G(76) + G(77) + 
     -        G(78) - TWO*G(80) + G(96) + G(97) - 
     -        TWO*G(99) - G(100) - G(101) + 
     -        TWO*G(103)) - G(214) + G(215) - 
     -     G(216) + G(217) + TWO*G(218) - 
     -     TWO*G(219) + G(220) - G(221) + 
     -     G(222) - G(223) - TWO*G(224) + 
     -     TWO*G(225) + G(232) - G(233) + 
     -     G(234) - G(235) - TWO*G(236) + 
     -     TWO*G(237) - G(238) + G(239) - 
     -     G(240) + G(241) + TWO*G(242) - 
     -     TWO*G(243) + G(250) - G(251) + 
     -     G(252) - G(253) - TWO*G(256) + 
     -     TWO*G(257) - G(258) + G(259) - 
     -     G(260) + G(261) + TWO*G(264) - 
     -     TWO*G(265) - G(305) + G(306) - 
     -     G(307) + G(308) + TWO*G(311) - 
     -     TWO*G(312) + G(313) - G(314) + 
     -     G(315) - G(316) - TWO*G(319) + 
     -     TWO*G(320)) + 
     -  FOUR*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(17) - G(18) + TWO*G(19) + G(24) + 
     -        G(25) - TWO*G(26)) + 
     -     (-G(3) + G(4))*
     -      (G(61) - G(62) - G(63) + G(64) + 
     -        G(67) - G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      (G(73) - G(74) + G(77) - G(78) - 
     -        TWO*G(81) + TWO*G(82) - G(96) + 
     -        G(97) - G(100) + G(101) + 
     -        TWO*G(104) - TWO*G(105)) + G(214) - 
     -     G(215) - G(216) + G(217) + G(220) - 
     -     G(221) - G(222) + G(223) - G(232) + 
     -     G(233) + G(234) - G(235) - G(238) + 
     -     G(239) + G(240) - G(241) - G(250) + 
     -     G(251) + G(252) - G(253) - G(258) + 
     -     G(259) + G(260) - G(261) + 
     -     TWO*G(266) - TWO*G(267) - TWO*G(268) + 
     -     TWO*G(269) + G(305) - G(306) - 
     -     G(307) + G(308) + G(313) - G(314) - 
     -     G(315) + G(316) - TWO*G(321) + 
     -     TWO*G(322) + TWO*G(323) - TWO*G(324)) 
     -   - 7*((G(13) - G(14) - G(15) + G(16))*
     -      (G(17) + G(18) - TWO*G(21) - G(24) - 
     -        G(25) + TWO*G(28)) + 
     -     (G(1) - G(2))*
     -      (-G(73) + G(74) - G(77) + G(78) + 
     -        TWO*G(83) - TWO*G(84) + G(96) - 
     -        G(97) + G(100) - G(101) - 
     -        TWO*G(106) + TWO*G(107)) + 
     -     (-G(3) + G(4))*
     -      (-G(61) + G(62) + G(63) - G(64) - 
     -        G(67) + G(68) + G(69) - G(70) + 
     -        TWO*G(166) - TWO*G(167) - 
     -        TWO*G(168) + TWO*G(169)) - G(214) + 
     -     G(215) + G(216) - G(217) - G(220) + 
     -     G(221) + G(222) - G(223) + 
     -     TWO*G(226) - TWO*G(227) - TWO*G(228) + 
     -     TWO*G(229) + G(232) - G(233) - 
     -     G(234) + G(235) + G(238) - G(239) - 
     -     G(240) + G(241) - TWO*G(244) + 
     -     TWO*G(245) + TWO*G(246) - TWO*G(247) + 
     -     G(250) - G(251) - G(252) + G(253) + 
     -     G(258) - G(259) - G(260) + G(261) - 
     -     TWO*G(270) + TWO*G(271) + TWO*G(272) - 
     -     TWO*G(273) - G(305) + G(306) + 
     -     G(307) - G(308) - G(313) + G(314) + 
     -     G(315) - G(316) + TWO*G(325) - 
     -     TWO*G(326) - TWO*G(327) + TWO*G(328))
      return
      end function fE13Euclid_w4

      function fE14Euclid_w3(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE14Euclid_w3
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE14Euclid_w3=-6*zeta3 - TWO*zeta2*(-G(11) + G(12)) + 
     -  TWO*((TWO*G(7) - G(12))*G(59) - 
     -     G(11)*(TWO*(G(7)*G(12) - G(55)) - 
     -        G(60)) + 
     -     TWO*(HALF*G(7)*G(12)**TWO - 
     -        G(12)*G(55) + G(208)) + G(212) - 
     -     G(213)) + 
     -  TWO*(FOUR*G(12)*G(59) - 
     -     G(11)*(-6*G(54) - TWO*G(55) + 
     -        FOUR*G(60)) - 
     -     6*(G(12)*G(54) - TWO*G(207)) - 
     -     TWO*(G(12)*G(55) - TWO*G(208)) - 
     -     FOUR*G(212) + FOUR*G(213))
      return
      end function fE14Euclid_w3

      function fE15Euclid_w2(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE15Euclid_w2
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE15Euclid_w2=        -(TWO*zeta2) + 
     -  TWO*(-(G(11)*
     -        (-6*G(6) - TWO*G(7) + FOUR*G(12))) 
     -      - 6*(G(6)*G(12) - G(54)) - 
     -     TWO*(G(7)*G(12) - G(55)) + 
     -     FOUR*G(59) + FOUR*G(60))
      return
      end function fE15Euclid_w2

      function fE17Euclid_w4(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE17Euclid_w4
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE17Euclid_w4=        -6*zeta4 + FOUR*zeta3*
     -   (TWO*G(7) + G(11) - G(12)) + 
     -  TWO*(G(59)*(FOUR*G(35) - 
     -        TWO*(G(7)*G(12) - G(55)) - 
     -        TWO*G(55) + G(60)) + 
     -     (TWO*G(7) - G(12))*G(212) - 
     -     G(11)*(FOUR*
     -         (G(12)*G(35) - G(133) - G(190)) - 
     -        TWO*(HALF*G(7)*G(12)**TWO - 
     -           G(12)*G(55) + G(208)) - 
     -        TWO*(G(12)*G(55) - TWO*G(208)) + 
     -        G(213)) + 
     -     FOUR*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) - 
     -     TWO*((G(7)*G(12)**THREE)/6. - 
     -        HALF*G(12)**TWO*G(55) + 
     -        G(12)*G(208) - G(593)) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) + 
     -     G(596) + G(597))
      return
      end function fE17Euclid_w4

      function fE19Euclid_w2(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE19Euclid_w2
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE19Euclid_w2=        (8*(-((-(HALF*G(3)) + HALF*G(4))*G(11)) + 
     -       HALF*(G(1)*G(11) - G(48)) + 
     -       HALF*(-(G(2)*G(11)) + G(49)) + 
     -       HALF*(-(G(3)*G(12)) + G(50)) + 
     -       HALF*(G(4)*G(12) - G(51))) + 
     -    FOUR*(-((G(1) - G(2))*G(12)) - G(48) + 
     -       G(49) + G(50) - G(51)))/4.
      return
      end function fE19Euclid_w2

      function fE19Euclid_w3(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE19Euclid_w3
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE19Euclid_w3=        -(TWO*zeta2*(-(HALF*G(1)) + HALF*G(2) - 
     -       HALF*G(3) + HALF*G(4))) - 
     -  G(12)*(G(13) - G(14) + G(15) - G(16)) + 
     -  (-6*G(5) - TWO*G(7) + 
     -     ((TWO + TWO*w)*G(8))/(1 + w) + 
     -     ((-TWO + TWO*w)*G(10))/(-1 + w) + 
     -     TWO*G(12))*(-G(48) + G(49)) + 
     -  G(12)*(-G(13) + G(14) - G(15) + G(16) + 
     -     TWO*G(48) - TWO*G(49)) + 
     -  (G(1) - G(2))*
     -   (-G(50) - G(51) + TWO*G(53)) - 
     -  (G(1) - G(2))*
     -   (G(50) + G(51) - TWO*G(55)) + 
     -  (G(1) - G(2))*
     -   (6*(G(5)*G(12) - G(52)) + 
     -     TWO*(G(7)*G(12) - G(55)) + 
     -     ((-TWO - TWO*w)*(G(8)*G(12) - G(56)))/
     -      (1 + w) + 
     -     ((TWO - TWO*w)*(G(10)*G(12) - G(58)))/
     -      (-1 + w) - TWO*G(60)) - TWO*G(65) + 
     -  TWO*G(66) - TWO*G(71) + TWO*G(72) - 
     -  6*G(119) + 6*G(120) - TWO*G(131) + 
     -  TWO*G(132) + 
     -  ((TWO + TWO*w)*G(146))/(1 + w) + 
     -  ((-TWO - TWO*w)*G(147))/(1 + w) - 
     -  TWO*((G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      (G(13) - G(14) - G(15) + G(16)) + 
     -     (G(1) - G(2))*
     -      (((-1 - w)*G(38))/(1 + w) + G(39) + 
     -        G(43) + ((1 - w)*G(44))/(-1 + w)) - 
     -     G(61) + G(62) + G(63) - G(64) + 
     -     G(67) - G(68) - G(69) + G(70) + 
     -     G(136) + ((-1 - w)*G(137))/(1 + w) + 
     -     ((-1 - w)*G(138))/(1 + w) + G(139) + 
     -     ((1 - w)*G(151))/(-1 + w) + G(152) + 
     -     G(153) + ((1 - w)*G(154))/(-1 + w)) + 
     -  ((-TWO + TWO*w)*G(161))/(-1 + w) + 
     -  ((TWO - TWO*w)*G(162))/(-1 + w) - 
     -  TWO*G(166) + TWO*G(167) - TWO*G(168) + 
     -  TWO*G(169) + TWO*G(170) - TWO*G(171) + 
     -  TWO*G(177) - TWO*G(178) - TWO*G(186) + 
     -  TWO*G(187) - TWO*G(188) + TWO*G(189) + 
     -  TWO*G(203) - TWO*G(204) + TWO*G(205) - 
     -  TWO*G(206) + 
     -  TWO*(G(11)*G(13) - G(11)*G(14) + 
     -     G(11)*G(15) - G(11)*G(16) + 
     -     G(12)*G(33) - G(12)*G(34) + 
     -     G(12)*G(39) + G(12)*G(44) + 
     -     (-6*G(5) - TWO*G(7) + 
     -        ((TWO + TWO*w)*G(8))/(1 + w) + 
     -        ((-TWO + TWO*w)*G(10))/(-1 + w) + 
     -        TWO*G(12))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) - 
     -     G(12)*G(50) + G(12)*G(51) - 
     -     G(11)*(THREE*G(31) - THREE*G(32) + 
     -        G(33) - G(34) + 
     -        ((-1 - w)*G(38))/(1 + w) + G(39) + 
     -        ((1 - w)*G(43))/(-1 + w) + G(44) - 
     -        G(50) + G(51)) - G(65) + G(66) - 
     -     G(71) + G(72) - G(131) + G(132) - 
     -     G(147) - G(162) - G(166) + G(167) - 
     -     G(168) + G(169) + 
     -     THREE*(G(12)*G(31) - G(119) - 
     -        G(184)) - 
     -     THREE*(G(12)*G(32) - G(120) - 
     -        G(185)) - G(188) + G(189) + 
     -     ((-1 - w)*
     -        (G(12)*G(38) - G(146) - G(193)))/
     -      (1 + w) - G(194) + 
     -     ((1 - w)*(G(12)*G(43) - G(161) - 
     -          G(198)))/(-1 + w) - G(199) + 
     -     TWO*G(205) - TWO*G(206)) + 
     -  TWO*(G(12)*G(21) - G(12)*G(28) - 
     -     G(11)*(THREE*G(20) + G(21) - 
     -        THREE*G(27) - G(28) - 
     -        TWO*(G(3)*G(12) - G(50)) + 
     -        TWO*(G(4)*G(12) - G(51))) + 
     -     FOUR*(-(HALF*G(3)) + HALF*G(4))*
     -      G(59) - G(93) + G(116) + 
     -     THREE*(G(12)*G(20) - G(92) - G(173)) - 
     -     G(174) - THREE*
     -      (G(12)*G(27) - G(115) - G(180)) + 
     -     G(181) - TWO*
     -      (HALF*G(1)*G(11)**TWO - G(11)*G(48) + 
     -        G(203)) + 
     -     TWO*(HALF*G(2)*G(11)**TWO - 
     -        G(11)*G(49) + G(204)) - 
     -     TWO*(HALF*G(3)*G(12)**TWO - 
     -        G(12)*G(50) + G(205)) + 
     -     TWO*(HALF*G(4)*G(12)**TWO - 
     -        G(12)*G(51) + G(206))) - 
     -  TWO*(-(HALF*G(1)*G(11)**TWO) + 
     -     HALF*G(2)*G(11)**TWO + 
     -     HALF*G(8)*G(12)**TWO + G(11)*G(48) - 
     -     G(11)*G(49) - G(12)*G(56) - 
     -     G(11)*(G(8)*G(12) - G(56) + 
     -        ((1 - w)*(G(10)*G(12) - G(58)))/
     -         (-1 + w)) + 
     -     (G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      G(59) - G(203) + G(204) + G(209) + 
     -     ((1 - w)*(HALF*G(10)*G(12)**TWO - 
     -          G(12)*G(58) + G(211)))/(-1 + w))
      return
      end function fE19Euclid_w3

      function fE20Euclid_w3(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE20Euclid_w3
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE20Euclid_w3=        HALF*G(1)*G(11)**TWO + 
     -  HALF*G(2)*G(11)**TWO + 
     -  HALF*G(3)*G(12)**TWO + 
     -  HALF*G(4)*G(12)**TWO + 
     -  (TWO*G(7) - G(12))*
     -   (G(13) - G(14) - G(15) + G(16)) - 
     -  G(11)*G(48) - G(11)*G(49) + 
     -  (G(8) + ((1 - w)*G(10))/(-1 + w))*
     -   (-G(48) + G(49)) - G(12)*G(50) + 
     -  (G(1) - G(2))*
     -   (-(TWO*G(33)) + TWO*G(34) + G(50) - 
     -     G(51)) - G(12)*G(51) + 
     -  G(11)*(-(G(3)*G(12)) - G(4)*G(12) + 
     -     G(50) + G(51) + 
     -     TWO*(G(7)*G(12) - G(55))) + 
     -  (G(1) - G(2))*
     -   (G(10)*G(12) + 
     -     ((-1 - w)*(G(8)*G(12) - G(56)))/
     -      (1 + w) - G(58)) - 
     -  (-G(3) - G(4) + TWO*G(7))*G(59) + G(65) - 
     -  G(66) - G(71) + G(72) + 
     -  HALF*(-((G(1) - G(2))*
     -        (G(17) + G(18) - TWO*G(21) - 
     -          G(24) - G(25) + TWO*G(28))) - 
     -     (-G(3) + G(4))*
     -      (-G(13) + G(14) - G(15) + G(16) + 
     -        TWO*G(48) - TWO*G(49)) + G(61) - 
     -     G(62) + G(63) - G(64) - TWO*G(65) + 
     -     TWO*G(66) - G(67) + G(68) - G(69) + 
     -     G(70) + TWO*G(71) - TWO*G(72) + 
     -     G(73) - G(74) + G(77) - G(78) - 
     -     TWO*G(83) + TWO*G(84) - G(96) + 
     -     G(97) - G(100) + G(101) + TWO*G(106) - 
     -     TWO*G(107)) + TWO*G(121) - 
     -  TWO*G(122) - TWO*G(123) + TWO*G(124) + 
     -  G(146) + ((-1 - w)*G(147))/(1 + w) + 
     -  ((1 - w)*G(161))/(-1 + w) + G(162) + 
     -  G(166) - G(167) - G(168) + G(169) + 
     -  HALF*(-((-G(3) - G(4) + TWO*G(7))*
     -        (G(13) - G(14) - G(15) + G(16))) - 
     -     (G(1) - G(2))*
     -      (G(17) - G(18) + G(24) - G(25) - 
     -        TWO*G(33) + TWO*G(34)) + G(61) - 
     -     G(62) - G(63) + G(64) + G(67) - 
     -     G(68) - G(69) + G(70) + G(73) - 
     -     G(74) - G(77) + G(78) + G(96) - 
     -     G(97) - G(100) + G(101) - TWO*G(121) + 
     -     TWO*G(122) + TWO*G(123) - TWO*G(124) - 
     -     TWO*G(166) + TWO*G(167) + TWO*G(168) - 
     -     TWO*G(169)) - G(170) + G(171) + 
     -  G(177) - TWO*
     -   ((G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (G(13) - G(14) - G(15) + G(16)) + 
     -     (G(1) - G(2))*
     -      (-G(33) + G(34) + 
     -        ((-HALF - HALF*w)*G(38))/(1 + w) + 
     -        ((HALF + HALF*w)*G(39))/(1 + w) + 
     -        ((HALF - HALF*w)*G(43))/(-1 + w) + 
     -        ((-HALF + HALF*w)*G(44))/(-1 + w) + 
     -        G(50) - G(51)) + HALF*G(61) - 
     -     HALF*G(62) - HALF*G(63) + HALF*G(64) + 
     -     HALF*G(67) - HALF*G(68) - HALF*G(69) + 
     -     HALF*G(70) + G(121) - G(122) - 
     -     G(123) + G(124) + 
     -     ((HALF + HALF*w)*G(136))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(137))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(138))/(1 + w) + 
     -     ((HALF + HALF*w)*G(139))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(151))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(152))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(153))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(154))/(-1 + w) - 
     -     G(170) + G(171) + G(177) - G(178)) - 
     -  G(178) + TWO*
     -   (-(G(11)*(((-HALF - HALF*w)*G(38))/
     -           (1 + w) + 
     -          ((HALF + HALF*w)*G(39))/(1 + w) + 
     -          ((-HALF + HALF*w)*G(43))/
     -           (-1 + w) + 
     -          ((HALF - HALF*w)*G(44))/(-1 + w)))
     -       + (G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) + 
     -     HALF*(-(G(11)*G(13)) + G(65) + 
     -        G(166)) + 
     -     HALF*(G(11)*G(14) - G(66) - G(167)) + 
     -     HALF*(G(11)*G(15) - G(71) - G(168)) + 
     -     HALF*(-(G(11)*G(16)) + G(72) + 
     -        G(169)) + 
     -     ((-HALF - HALF*w)*
     -        (G(12)*G(38) - G(146) - G(193)))/
     -      (1 + w) + 
     -     ((HALF + HALF*w)*
     -        (G(12)*G(39) - G(147) - G(194)))/
     -      (1 + w) + 
     -     ((-HALF + HALF*w)*
     -        (G(12)*G(43) - G(161) - G(198)))/
     -      (-1 + w) + 
     -     ((HALF - HALF*w)*
     -        (G(12)*G(44) - G(162) - G(199)))/
     -      (-1 + w)) + G(203) + G(204) + 
     -  G(205) + G(206) - 
     -  TWO*(HALF*G(7)*G(12)**TWO - G(12)*G(55) + 
     -     G(208)) - TWO*G(212) - 
     -  TWO*(HALF*G(7)*G(12)**TWO - G(12)*G(55) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*G(59) - 
     -     G(11)*(G(7)*G(12) - G(55) + 
     -        ((HALF + HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((-HALF + HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         - G(60)) + 
     -     HALF*(HALF*G(1)*G(11)**TWO - 
     -        G(11)*G(48) + G(203)) + 
     -     HALF*(HALF*G(2)*G(11)**TWO - 
     -        G(11)*G(49) + G(204)) + G(208) + 
     -     ((HALF + HALF*w)*
     -        (HALF*G(8)*G(12)**TWO - 
     -          G(12)*G(56) + G(209)))/(1 + w) + 
     -     ((-HALF + HALF*w)*
     -        (HALF*G(10)*G(12)**TWO - 
     -          G(12)*G(58) + G(211)))/(-1 + w) - 
     -     G(213)) + 
     -  TWO*((TWO*G(7) - G(12))*G(59) - 
     -     G(11)*(TWO*(G(7)*G(12) - G(55)) - 
     -        G(60)) + 
     -     TWO*(HALF*G(7)*G(12)**TWO - 
     -        G(12)*G(55) + G(208)) + G(212) - 
     -     G(213))
      return
      end function fE20Euclid_w3

      function fE21Euclid_w4(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE21Euclid_w4
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE21Euclid_w4=        -6*zeta4 - FOUR*zeta3*
     -   (-G(1) - G(2) - G(3) - G(4) + TWO*G(7) + 
     -     TWO*G(11)) - 
     -  8*zeta3*(HALF*G(1) + HALF*G(2) + G(7) + 
     -     ((HALF + HALF*w)*G(8))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -     G(12)) + 12*zeta3*
     -   (TWO*G(7) + G(11) - G(12)) - 
     -  FOUR*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(33) - G(34) + TWO*G(35) + 
     -        ((-HALF - HALF*w)*G(38))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(39))/(1 + w) + 
     -        G(40) + 
     -        ((HALF - HALF*w)*G(43))/(-1 + w) + 
     -        ((HALF - HALF*w)*G(44))/(-1 + w) + 
     -        G(45) + G(50) + G(51) - TWO*G(55)) 
     -      + (G(7) + 
     -        ((HALF + HALF*w)*G(8))/(1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (-G(61) + G(62) + G(63) - G(64) - 
     -        G(67) + G(68) + G(69) - G(70) + 
     -        TWO*G(166) - TWO*G(167) - 
     -        TWO*G(168) + TWO*G(169)) + 
     -     (G(1) - G(2))*
     -      (G(121) - G(122) + G(123) - G(124) - 
     -        TWO*G(125) + TWO*G(126) + 
     -        ((HALF + HALF*w)*G(136))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(137))/(1 + w) + 
     -        ((HALF + HALF*w)*G(138))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(139))/(1 + w) + 
     -        ((-1 - w)*G(140))/(1 + w) + 
     -        G(141) + 
     -        ((-HALF + HALF*w)*G(151))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*G(152))/(-1 + w) + 
     -        ((-HALF + HALF*w)*G(153))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*G(154))/(-1 + w) + 
     -        ((1 - w)*G(155))/(-1 + w) + 
     -        G(156) - G(170) + G(171) - G(177) + 
     -        G(178) + TWO*G(188) - TWO*G(189)) - 
     -     HALF*G(214) + HALF*G(215) + 
     -     HALF*G(216) - HALF*G(217) - 
     -     HALF*G(220) + HALF*G(221) + 
     -     HALF*G(222) - HALF*G(223) + G(226) - 
     -     G(227) - G(228) + G(229) - 
     -     HALF*G(232) + HALF*G(233) + 
     -     HALF*G(234) - HALF*G(235) - 
     -     HALF*G(238) + HALF*G(239) + 
     -     HALF*G(240) - HALF*G(241) + G(244) - 
     -     G(245) - G(246) + G(247) - G(360) + 
     -     G(361) + G(362) - G(363) - G(364) + 
     -     G(365) + G(366) - G(367) + 
     -     TWO*G(368) - TWO*G(369) - TWO*G(370) + 
     -     TWO*G(371) + 
     -     ((-HALF - HALF*w)*G(397))/(1 + w) + 
     -     ((HALF + HALF*w)*G(398))/(1 + w) + 
     -     ((HALF + HALF*w)*G(399))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(400))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(401))/(1 + w) + 
     -     ((HALF + HALF*w)*G(402))/(1 + w) + 
     -     ((HALF + HALF*w)*G(403))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(404))/(1 + w) + 
     -     G(405) + ((-1 - w)*G(406))/(1 + w) + 
     -     ((-1 - w)*G(407))/(1 + w) + G(408) + 
     -     ((HALF - HALF*w)*G(434))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(435))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(436))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(437))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(438))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(439))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(440))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(441))/(-1 + w) + 
     -     G(442) + ((1 - w)*G(443))/(-1 + w) + 
     -     ((1 - w)*G(444))/(-1 + w) + G(445) + 
     -     G(483) - G(484) - G(485) + G(486) + 
     -     G(500) - G(501) - G(502) + G(503) - 
     -     TWO*G(521) + TWO*G(522) + TWO*G(523) - 
     -     TWO*G(524)) - 
     -  FOUR*((-G(48) + G(49))*
     -      (G(36) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(37) + 
     -        ((HALF + HALF*w)*G(41))/(1 + w) + 
     -        ((HALF/(-1 + w) - 
     -             (HALF*w**TWO)/(-1 + w))*G(42))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(46))/(-1 + w) + 
     -        ((-(HALF/(-1 + w)) + w/(-1 + w) - 
     -             (HALF*w**TWO)/(-1 + w))*G(47))/
     -         (-1 + w) - G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (G(65) - G(66) - G(71) + G(72)) + 
     -     (G(1) - G(2))*
     -      (G(12)*G(37) - G(12)*G(58) - G(135) + 
     -        (1/(-1 - w) - w/(1 + w))*
     -         (G(12)*G(36) - G(134) - G(191)) - 
     -        G(192) + 
     -        ((-(HALF/(1 + w)) - w/(1 + w) - 
     -             (HALF*w**TWO)/(1 + w))*
     -           (G(12)*G(41) - G(149) - G(196)))/
     -         (1 + w) + 
     -        ((HALF + HALF*w)*
     -           (G(12)*G(42) - G(150) - G(197)))/
     -         (1 + w) + 
     -        ((HALF/(1 + w) - 
     -             (HALF*w**TWO)/(1 + w))*
     -           (G(12)*G(46) - G(164) - G(201)))/
     -         (-1 + w) + 
     -        ((-HALF + HALF*w)*
     -           (G(12)*G(47) - G(165) - G(202)))/
     -         (-1 + w) + 
     -        (1/(1 + w) + w/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        TWO*G(211)) + HALF*G(218) - 
     -     HALF*G(219) - HALF*G(224) + 
     -     HALF*G(225) + HALF*G(236) - 
     -     HALF*G(237) - HALF*G(242) + 
     -     HALF*G(243) + G(376) + 
     -     (1/(-1 - w) - w/(1 + w))*G(377) + 
     -     (1/(-1 + w) - w/(-1 + w))*G(382) + 
     -     G(383) + ((HALF + HALF*w)*G(413))/
     -      (1 + w) + 
     -     ((-(HALF/(1 + w)) - w/(1 + w) - 
     -          (HALF*w**TWO)/(1 + w))*G(414))/
     -      (1 + w) + 
     -     ((HALF/(-1 + w) - 
     -          (HALF*w**TWO)/(-1 + w))*G(419))/
     -      (1 + w) + 
     -     ((HALF + HALF*w)*G(420))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(450))/(-1 + w) + 
     -     ((HALF/(1 + w) - 
     -          (HALF*w**TWO)/(1 + w))*G(451))/
     -      (-1 + w) + 
     -     ((-(HALF/(-1 + w)) + w/(-1 + w) - 
     -          (HALF*w**TWO)/(-1 + w))*G(456))/
     -      (-1 + w) + 
     -     ((-HALF + HALF*w)*G(457))/(-1 + w) - 
     -     G(542) + (1/(1 + w) + w/(1 + w))*
     -      G(543) + 
     -     (1/(1 - w) + w/(-1 + w))*G(555) - 
     -     G(556)) + 
     -  FOUR*((-G(48) + G(49))*
     -      (TWO*G(36) + 
     -        (TWO/(-1 + w) - (TWO*w)/(-1 + w))*
     -         G(37) - G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) + 
     -     (TWO*G(7) - G(12))*
     -      (G(65) - G(66) - G(71) + G(72)) + 
     -     (G(1) - G(2))*
     -      (-(G(12)*G(58)) + 
     -        (-(TWO/(1 + w)) - (TWO*w)/(1 + w))*
     -         (G(12)*G(36) - G(134) - G(191)) + 
     -        TWO*(G(12)*G(37) - G(135) - 
     -           G(192)) + 
     -        (1/(1 + w) + w/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        TWO*G(211)) + TWO*G(376) + 
     -     (-(TWO/(1 + w)) - (TWO*w)/(1 + w))*
     -      G(377) + 
     -     (TWO/(-1 + w) - (TWO*w)/(-1 + w))*
     -      G(382) + TWO*G(383) + G(475) - 
     -     G(476) - G(481) + G(482) - G(542) + 
     -     (1/(1 + w) + w/(1 + w))*G(543) + 
     -     (1/(1 - w) + w/(-1 + w))*G(555) - 
     -     G(556)) + 
     -  TWO*((G(13) - G(14) - G(15) + G(16))*
     -      (G(17) + G(18) - TWO*G(21) + G(24) + 
     -        G(25) - TWO*G(28) - TWO*G(33) - 
     -        TWO*G(34) + FOUR*G(35)) + 
     -     (G(1) - G(2))*
     -      (-G(73) + G(74) - G(77) + G(78) + 
     -        TWO*G(83) - TWO*G(84) - G(96) + 
     -        G(97) - G(100) + G(101) + 
     -        TWO*G(106) - TWO*G(107) + 
     -        TWO*G(121) - TWO*G(122) + 
     -        TWO*G(123) - TWO*G(124) - 
     -        FOUR*G(125) + FOUR*G(126)) + 
     -     (-G(3) - G(4) + TWO*G(7))*
     -      (-G(61) + G(62) + G(63) - G(64) - 
     -        G(67) + G(68) + G(69) - G(70) + 
     -        TWO*G(166) - TWO*G(167) - 
     -        TWO*G(168) + TWO*G(169)) + G(214) - 
     -     G(215) - G(216) + G(217) + G(220) - 
     -     G(221) - G(222) + G(223) - 
     -     TWO*G(226) + TWO*G(227) + TWO*G(228) - 
     -     TWO*G(229) + G(232) - G(233) - 
     -     G(234) + G(235) + G(238) - G(239) - 
     -     G(240) + G(241) - TWO*G(244) + 
     -     TWO*G(245) + TWO*G(246) - TWO*G(247) + 
     -     G(250) - G(251) - G(252) + G(253) + 
     -     G(258) - G(259) - G(260) + G(261) - 
     -     TWO*G(270) + TWO*G(271) + TWO*G(272) - 
     -     TWO*G(273) + G(305) - G(306) - 
     -     G(307) + G(308) + G(313) - G(314) - 
     -     G(315) + G(316) - TWO*G(325) + 
     -     TWO*G(326) + TWO*G(327) - TWO*G(328) - 
     -     TWO*G(360) + TWO*G(361) + TWO*G(362) - 
     -     TWO*G(363) - TWO*G(364) + TWO*G(365) + 
     -     TWO*G(366) - TWO*G(367) + 
     -     FOUR*G(368) - FOUR*G(369) - 
     -     FOUR*G(370) + FOUR*G(371) - 
     -     TWO*G(471) + TWO*G(472) + TWO*G(473) - 
     -     TWO*G(474) - TWO*G(477) + TWO*G(478) + 
     -     TWO*G(479) - TWO*G(480) + 
     -     FOUR*G(560) - FOUR*G(561) - 
     -     FOUR*G(562) + FOUR*G(563)) + 
     -  TWO*((G(13) - G(14) - G(15) + G(16))*
     -      (-(TWO*G(33)) - TWO*G(34) + 
     -        FOUR*G(35) + G(50) + G(51) - 
     -        TWO*G(55)) + 
     -     (TWO*G(7) - G(12))*
     -      (-G(61) + G(62) + G(63) - G(64) - 
     -        G(67) + G(68) + G(69) - G(70) + 
     -        TWO*G(166) - TWO*G(167) - 
     -        TWO*G(168) + TWO*G(169)) + 
     -     (G(1) - G(2))*
     -      (TWO*G(121) - TWO*G(122) + 
     -        TWO*G(123) - TWO*G(124) - 
     -        FOUR*G(125) + FOUR*G(126) - 
     -        G(170) + G(171) - G(177) + G(178) + 
     -        TWO*G(188) - TWO*G(189)) - 
     -     TWO*G(360) + TWO*G(361) + TWO*G(362) - 
     -     TWO*G(363) - TWO*G(364) + TWO*G(365) + 
     -     TWO*G(366) - TWO*G(367) + 
     -     FOUR*G(368) - FOUR*G(369) - 
     -     FOUR*G(370) + FOUR*G(371) - G(471) + 
     -     G(472) + G(473) - G(474) - G(477) + 
     -     G(478) + G(479) - G(480) + G(483) - 
     -     G(484) - G(485) + G(486) + G(500) - 
     -     G(501) - G(502) + G(503) - 
     -     TWO*G(521) + TWO*G(522) + TWO*G(523) - 
     -     TWO*G(524) + TWO*G(560) - TWO*G(561) - 
     -     TWO*G(562) + TWO*G(563)) - 
     -  FOUR*((G(13) - G(14) - G(15) + G(16))*
     -      (-(G(7)*G(12)) + TWO*G(35) + G(40) + 
     -        G(45) - G(55) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + G(60)) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (G(166) - G(167) - G(168) + G(169)) + 
     -     (G(1) - G(2))*
     -      (-(TWO*G(125)) + TWO*G(126) + 
     -        G(131) - G(132) + 
     -        ((-1 - w)*G(140))/(1 + w) + 
     -        G(141) + 
     -        ((HALF + HALF*w)*G(146))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(147))/(1 + w) + 
     -        ((1 - w)*G(155))/(-1 + w) + 
     -        G(156) + 
     -        ((-HALF + HALF*w)*G(161))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*G(162))/(-1 + w) + 
     -        TWO*G(188) - TWO*G(189) - G(205) + 
     -        G(206)) + HALF*G(226) - 
     -     HALF*G(227) - HALF*G(228) + 
     -     HALF*G(229) + HALF*G(244) - 
     -     HALF*G(245) - HALF*G(246) + 
     -     HALF*G(247) + TWO*G(368) - 
     -     TWO*G(369) - TWO*G(370) + TWO*G(371) - 
     -     G(384) + G(385) + G(386) - G(387) + 
     -     G(405) + ((-1 - w)*G(406))/(1 + w) + 
     -     ((-1 - w)*G(407))/(1 + w) + G(408) + 
     -     ((-HALF - HALF*w)*G(421))/(1 + w) + 
     -     ((HALF + HALF*w)*G(422))/(1 + w) + 
     -     ((HALF + HALF*w)*G(423))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(424))/(1 + w) + 
     -     G(442) + ((1 - w)*G(443))/(-1 + w) + 
     -     ((1 - w)*G(444))/(-1 + w) + G(445) + 
     -     ((HALF - HALF*w)*G(458))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(459))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(460))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(461))/(-1 + w) - 
     -     TWO*G(521) + TWO*G(522) + TWO*G(523) - 
     -     TWO*G(524) + G(564) - G(565) - 
     -     G(569) + G(570)) + 
     -  8*((G(13) - G(14) - G(15) + G(16))*
     -      (-(G(7)*G(12)) + G(35) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(36) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(37) + 
     -        ((HALF + HALF*w)*G(40))/(1 + w) + 
     -        ((1/(4.*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.*(1 + w)))*G(41))/
     -         (1 + w) + 
     -        ((-1/(4.*(-1 + w)) + 
     -             w**TWO/(4.*(-1 + w)))*G(42))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(45))/(-1 + w) + 
     -        ((-1/(4.*(1 + w)) + 
     -             w**TWO/(4.*(1 + w)))*G(46))/
     -         (-1 + w) + 
     -        ((1/(4.*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.*(-1 + w)))*G(47))/
     -         (-1 + w) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + (HALF/(-1 + w) - 
     -           (HALF*w)/(-1 + w))*G(58) + G(60))
     -       + (G(7) + 
     -        ((HALF + HALF*w)*G(8))/(1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (HALF*G(61) - HALF*G(62) - 
     -        HALF*G(63) + HALF*G(64) + 
     -        HALF*G(67) - HALF*G(68) - 
     -        HALF*G(69) + HALF*G(70)) + 
     -     (G(1) - G(2))*
     -      (-G(125) + G(126) + 
     -        (HALF/(-1 - w) - (HALF*w)/(1 + w))*
     -         G(127) + 
     -        (-(HALF/(-1 - w)) + 
     -           (HALF*w)/(1 + w))*G(128) + 
     -        (-(HALF/(1 - w)) - 
     -           (HALF*w)/(-1 + w))*G(129) + 
     -        (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -         G(130) + G(131) - G(132) + 
     -        ((-HALF - HALF*w)*G(140))/(1 + w) + 
     -        ((HALF + HALF*w)*G(141))/(1 + w) + 
     -        ((-1/(4.*(1 + w)) - 
     -             (HALF*w)/(1 + w) - 
     -             w**TWO/(4.*(1 + w)))*G(142))/
     -         (1 + w) + 
     -        ((1/(4.*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.*(1 + w)))*G(143))/
     -         (1 + w) + 
     -        ((1/(4.*(-1 + w)) - 
     -             w**TWO/(4.*(-1 + w)))*G(144))/
     -         (1 + w) + 
     -        ((-1/(4.*(-1 + w)) + 
     -             w**TWO/(4.*(-1 + w)))*G(145))/
     -         (1 + w) + 
     -        ((HALF + HALF*w)*G(146))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(147))/(1 + w) + 
     -        ((HALF - HALF*w)*G(155))/(-1 + w) + 
     -        ((-HALF + HALF*w)*G(156))/
     -         (-1 + w) + 
     -        ((1/(4.*(1 + w)) - 
     -             w**TWO/(4.*(1 + w)))*G(157))/
     -         (-1 + w) + 
     -        ((-1/(4.*(1 + w)) + 
     -             w**TWO/(4.*(1 + w)))*G(158))/
     -         (-1 + w) + 
     -        ((-1/(4.*(-1 + w)) + 
     -             (HALF*w)/(-1 + w) - 
     -             w**TWO/(4.*(-1 + w)))*G(159))/
     -         (-1 + w) + 
     -        ((1/(4.*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.*(-1 + w)))*G(160))/
     -         (-1 + w) + 
     -        ((-HALF + HALF*w)*G(161))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*G(162))/(-1 + w) + 
     -        G(188) - G(189) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(198) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(199) - G(205) + G(206)) + 
     -     G(214)/4. - G(215)/4. - G(216)/4. + 
     -     G(217)/4. + G(220)/4. - G(221)/4. - 
     -     G(222)/4. + G(223)/4. + G(232)/4. - 
     -     G(233)/4. - G(234)/4. + G(235)/4. + 
     -     G(238)/4. - G(239)/4. - G(240)/4. + 
     -     G(241)/4. + G(368) - G(369) - G(370) + 
     -     G(371) + (-(HALF/(-1 - w)) + 
     -        (HALF*w)/(1 + w))*G(372) + 
     -     (HALF/(-1 - w) - (HALF*w)/(1 + w))*
     -      G(373) + 
     -     (HALF/(-1 - w) - (HALF*w)/(1 + w))*
     -      G(374) + 
     -     (-(HALF/(-1 - w)) + (HALF*w)/(1 + w))*
     -      G(375) + 
     -     (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -      G(378) + 
     -     (-(HALF/(1 - w)) - (HALF*w)/(-1 + w))*
     -      G(379) + 
     -     (-(HALF/(1 - w)) - (HALF*w)/(-1 + w))*
     -      G(380) + 
     -     (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -      G(381) - G(384) + G(385) + G(386) - 
     -     G(387) + ((HALF + HALF*w)*G(405))/
     -      (1 + w) + 
     -     ((-HALF - HALF*w)*G(406))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(407))/(1 + w) + 
     -     ((HALF + HALF*w)*G(408))/(1 + w) + 
     -     ((1/(4.*(1 + w)) + (HALF*w)/(1 + w) + 
     -          w**TWO/(4.*(1 + w)))*G(409))/
     -      (1 + w) + 
     -     ((-1/(4.*(1 + w)) - (HALF*w)/(1 + w) - 
     -          w**TWO/(4.*(1 + w)))*G(410))/
     -      (1 + w) + 
     -     ((-1/(4.*(1 + w)) - (HALF*w)/(1 + w) - 
     -          w**TWO/(4.*(1 + w)))*G(411))/
     -      (1 + w) + 
     -     ((1/(4.*(1 + w)) + (HALF*w)/(1 + w) + 
     -          w**TWO/(4.*(1 + w)))*G(412))/
     -      (1 + w) + 
     -     ((-1/(4.*(-1 + w)) + 
     -          w**TWO/(4.*(-1 + w)))*G(415))/
     -      (1 + w) + 
     -     ((1/(4.*(-1 + w)) - 
     -          w**TWO/(4.*(-1 + w)))*G(416))/
     -      (1 + w) + 
     -     ((1/(4.*(-1 + w)) - 
     -          w**TWO/(4.*(-1 + w)))*G(417))/
     -      (1 + w) + 
     -     ((-1/(4.*(-1 + w)) + 
     -          w**TWO/(4.*(-1 + w)))*G(418))/
     -      (1 + w) + 
     -     ((-HALF - HALF*w)*G(421))/(1 + w) + 
     -     ((HALF + HALF*w)*G(422))/(1 + w) + 
     -     ((HALF + HALF*w)*G(423))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(424))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(442))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(443))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(444))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(445))/(-1 + w) + 
     -     ((-1/(4.*(1 + w)) + 
     -          w**TWO/(4.*(1 + w)))*G(446))/
     -      (-1 + w) + 
     -     ((1/(4.*(1 + w)) - 
     -          w**TWO/(4.*(1 + w)))*G(447))/
     -      (-1 + w) + 
     -     ((1/(4.*(1 + w)) - 
     -          w**TWO/(4.*(1 + w)))*G(448))/
     -      (-1 + w) + 
     -     ((-1/(4.*(1 + w)) + 
     -          w**TWO/(4.*(1 + w)))*G(449))/
     -      (-1 + w) + 
     -     ((1/(4.*(-1 + w)) - 
     -          (HALF*w)/(-1 + w) + 
     -          w**TWO/(4.*(-1 + w)))*G(452))/
     -      (-1 + w) + 
     -     ((-1/(4.*(-1 + w)) + 
     -          (HALF*w)/(-1 + w) - 
     -          w**TWO/(4.*(-1 + w)))*G(453))/
     -      (-1 + w) + 
     -     ((-1/(4.*(-1 + w)) + 
     -          (HALF*w)/(-1 + w) - 
     -          w**TWO/(4.*(-1 + w)))*G(454))/
     -      (-1 + w) + 
     -     ((1/(4.*(-1 + w)) - 
     -          (HALF*w)/(-1 + w) + 
     -          w**TWO/(4.*(-1 + w)))*G(455))/
     -      (-1 + w) + 
     -     ((HALF - HALF*w)*G(458))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(459))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(460))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(461))/(-1 + w) - 
     -     G(521) + G(522) + G(523) - G(524) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(534) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(535) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(536) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(537) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(547) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(548) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(549) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(550) + G(564) - G(565) - G(569) + 
     -     G(570)) - 
     -  8*((G(13) - G(14) - G(15) + G(16))*
     -      (TWO*G(35) + 
     -        (1/(1 + w) + w/(1 + w))*G(36) + 
     -        (1/(1 - w) + w/(-1 + w))*G(37) - 
     -        TWO*(G(7)*G(12) - G(55)) - G(55) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(58) + G(60)) + 
     -     (TWO*G(7) - G(12))*
     -      (HALF*G(61) - HALF*G(62) - 
     -        HALF*G(63) + HALF*G(64) + 
     -        HALF*G(67) - HALF*G(68) - 
     -        HALF*G(69) + HALF*G(70)) + 
     -     (G(1) - G(2))*
     -      (-(TWO*G(125)) + TWO*G(126) + 
     -        (1/(-1 - w) - w/(1 + w))*G(127) + 
     -        (-(1/(-1 - w)) + w/(1 + w))*
     -         G(128) + 
     -        (-(1/(1 - w)) - w/(-1 + w))*
     -         G(129) + 
     -        (1/(1 - w) + w/(-1 + w))*G(130) + 
     -        TWO*G(131) - TWO*G(132) + G(188) - 
     -        G(189) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(198) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(199) - G(205) + G(206)) + 
     -     TWO*G(368) - TWO*G(369) - TWO*G(370) + 
     -     TWO*G(371) + 
     -     (-(1/(-1 - w)) + w/(1 + w))*G(372) + 
     -     (1/(-1 - w) - w/(1 + w))*G(373) + 
     -     (1/(-1 - w) - w/(1 + w))*G(374) + 
     -     (-(1/(-1 - w)) + w/(1 + w))*G(375) + 
     -     (1/(1 - w) + w/(-1 + w))*G(378) + 
     -     (-(1/(1 - w)) - w/(-1 + w))*G(379) + 
     -     (-(1/(1 - w)) - w/(-1 + w))*G(380) + 
     -     (1/(1 - w) + w/(-1 + w))*G(381) - 
     -     TWO*G(384) + TWO*G(385) + TWO*G(386) - 
     -     TWO*G(387) + HALF*G(471) - 
     -     HALF*G(472) - HALF*G(473) + 
     -     HALF*G(474) + HALF*G(477) - 
     -     HALF*G(478) - HALF*G(479) + 
     -     HALF*G(480) - G(521) + G(522) + 
     -     G(523) - G(524) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(534) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(535) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(536) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(537) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(547) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(548) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(549) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(550) + G(564) - G(565) - G(569) + 
     -     G(570)) + 
     -  FOUR*((G(13) - G(14) - G(15) + G(16))*
     -      (FOUR*G(35) - 
     -        TWO*(G(7)*G(12) - G(55)) - 
     -        TWO*G(55) + G(60)) + 
     -     (TWO*G(7) - G(12))*
     -      (G(166) - G(167) - G(168) + G(169)) + 
     -     (G(1) - G(2))*
     -      (-(FOUR*G(125)) + FOUR*G(126) + 
     -        TWO*G(131) - TWO*G(132) + 
     -        TWO*G(188) - TWO*G(189) - G(205) + 
     -        G(206)) + FOUR*G(368) - 
     -     FOUR*G(369) - FOUR*G(370) + 
     -     FOUR*G(371) - TWO*G(384) + 
     -     TWO*G(385) + TWO*G(386) - TWO*G(387) - 
     -     TWO*G(521) + TWO*G(522) + TWO*G(523) - 
     -     TWO*G(524) + G(560) - G(561) - 
     -     G(562) + G(563) + G(564) - G(565) - 
     -     G(569) + G(570)) - 
     -  8*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (G(36) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(37) + 
     -        ((HALF + HALF*w)*G(41))/(1 + w) + 
     -        ((HALF/(-1 + w) - 
     -             (HALF*w**TWO)/(-1 + w))*G(42))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(46))/(-1 + w) + 
     -        ((-(HALF/(-1 + w)) + w/(-1 + w) - 
     -             (HALF*w**TWO)/(-1 + w))*G(47))/
     -         (-1 + w) - G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (HALF*(-(G(11)*G(13)) + G(65) + 
     -           G(166)) + 
     -        HALF*(G(11)*G(14) - G(66) - 
     -           G(167)) + 
     -        HALF*(G(11)*G(15) - G(71) - 
     -           G(168)) + 
     -        HALF*(-(G(11)*G(16)) + G(72) + 
     -           G(169))) - 
     -     G(11)*((HALF/(-1 - w) - 
     -           (HALF*w)/(1 + w))*G(127) + 
     -        (-(HALF/(-1 - w)) + 
     -           (HALF*w)/(1 + w))*G(128) + 
     -        (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -         G(129) + 
     -        (-(HALF/(1 - w)) - 
     -           (HALF*w)/(-1 + w))*G(130) + 
     -        ((-1/(4.*(1 + w)) - 
     -             (HALF*w)/(1 + w) - 
     -             w**TWO/(4.*(1 + w)))*G(142))/
     -         (1 + w) + 
     -        ((1/(4.*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.*(1 + w)))*G(143))/
     -         (1 + w) + 
     -        ((-1/(4.*(-1 + w)) + 
     -             w**TWO/(4.*(-1 + w)))*G(144))/
     -         (1 + w) + 
     -        ((1/(4.*(-1 + w)) - 
     -             w**TWO/(4.*(-1 + w)))*G(145))/
     -         (1 + w) + 
     -        ((1/(4.*(1 + w)) - 
     -             w**TWO/(4.*(1 + w)))*G(157))/
     -         (-1 + w) + 
     -        ((-1/(4.*(1 + w)) + 
     -             w**TWO/(4.*(1 + w)))*G(158))/
     -         (-1 + w) + 
     -        ((1/(4.*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.*(-1 + w)))*G(159))/
     -         (-1 + w) + 
     -        ((-1/(4.*(-1 + w)) + 
     -             (HALF*w)/(-1 + w) - 
     -             w**TWO/(4.*(-1 + w)))*G(160))/
     -         (-1 + w) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(198) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(199)) + 
     -     (-(G(11)*G(61)) + G(218) + G(226) + 
     -        G(471))/4. + 
     -     (G(11)*G(62) - G(219) - G(227) - 
     -        G(472))/4. + 
     -     (G(11)*G(63) - G(224) - G(228) - 
     -        G(473))/4. + 
     -     (-(G(11)*G(64)) + G(225) + G(229) + 
     -        G(474))/4. + 
     -     (-(G(11)*G(67)) + G(236) + G(244) + 
     -        G(477))/4. + 
     -     (G(11)*G(68) - G(237) - G(245) - 
     -        G(478))/4. + 
     -     (G(11)*G(69) - G(242) - G(246) - 
     -        G(479))/4. + 
     -     (-(G(11)*G(70)) + G(243) + G(247) + 
     -        G(480))/4. + 
     -     (HALF/(-1 - w) - (HALF*w)/(1 + w))*
     -      (G(12)*G(127) - G(376) - G(388) - 
     -        G(525)) + 
     -     (-(HALF/(-1 - w)) + (HALF*w)/(1 + w))*
     -      (G(12)*G(128) - G(377) - G(389) - 
     -        G(526)) + 
     -     (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -      (G(12)*G(129) - G(382) - G(390) - 
     -        G(527)) + 
     -     (-(HALF/(1 - w)) - (HALF*w)/(-1 + w))*
     -      (G(12)*G(130) - G(383) - G(391) - 
     -        G(528)) + 
     -     ((-1/(4.*(1 + w)) - (HALF*w)/(1 + w) - 
     -          w**TWO/(4.*(1 + w)))*
     -        (G(12)*G(142) - G(413) - G(425) - 
     -          G(538)))/(1 + w) + 
     -     ((1/(4.*(1 + w)) + (HALF*w)/(1 + w) + 
     -          w**TWO/(4.*(1 + w)))*
     -        (G(12)*G(143) - G(414) - G(426) - 
     -          G(539)))/(1 + w) + 
     -     ((-1/(4.*(-1 + w)) + 
     -          w**TWO/(4.*(-1 + w)))*
     -        (G(12)*G(144) - G(419) - G(427) - 
     -          G(540)))/(1 + w) + 
     -     ((1/(4.*(-1 + w)) - 
     -          w**TWO/(4.*(-1 + w)))*
     -        (G(12)*G(145) - G(420) - G(428) - 
     -          G(541)))/(1 + w) + 
     -     ((1/(4.*(1 + w)) - 
     -          w**TWO/(4.*(1 + w)))*
     -        (G(12)*G(157) - G(450) - G(462) - 
     -          G(551)))/(-1 + w) + 
     -     ((-1/(4.*(1 + w)) + 
     -          w**TWO/(4.*(1 + w)))*
     -        (G(12)*G(158) - G(451) - G(463) - 
     -          G(552)))/(-1 + w) + 
     -     ((1/(4.*(-1 + w)) - 
     -          (HALF*w)/(-1 + w) + 
     -          w**TWO/(4.*(-1 + w)))*
     -        (G(12)*G(159) - G(456) - G(464) - 
     -          G(553)))/(-1 + w) + 
     -     ((-1/(4.*(-1 + w)) + 
     -          (HALF*w)/(-1 + w) - 
     -          w**TWO/(4.*(-1 + w)))*
     -        (G(12)*G(160) - G(457) - G(465) - 
     -          G(554)))/(-1 + w) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      (G(12)*G(193) - G(542) - TWO*G(579)) 
     -      + (-(HALF/(1 + w)) - 
     -        (HALF*w)/(1 + w))*
     -      (G(12)*G(194) - G(543) - TWO*G(580)) 
     -      + (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (G(12)*G(198) - G(555) - TWO*G(584)) 
     -      + (-(HALF/(-1 + w)) + 
     -        (HALF*w)/(-1 + w))*
     -      (G(12)*G(199) - G(556) - TWO*G(585))) 
     -   + 8*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (TWO*G(36) + 
     -        (TWO/(-1 + w) - (TWO*w)/(-1 + w))*
     -         G(37) - G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) + 
     -     (TWO*G(7) - G(12))*
     -      (HALF*(-(G(11)*G(13)) + G(65) + 
     -           G(166)) + 
     -        HALF*(G(11)*G(14) - G(66) - 
     -           G(167)) + 
     -        HALF*(G(11)*G(15) - G(71) - 
     -           G(168)) + 
     -        HALF*(-(G(11)*G(16)) + G(72) + 
     -           G(169))) - 
     -     G(11)*((1/(-1 - w) - w/(1 + w))*
     -         G(127) + 
     -        (-(1/(-1 - w)) + w/(1 + w))*
     -         G(128) + 
     -        (1/(1 - w) + w/(-1 + w))*G(129) + 
     -        (-(1/(1 - w)) - w/(-1 + w))*
     -         G(130) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(198) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(199)) + 
     -     (1/(-1 - w) - w/(1 + w))*
     -      (G(12)*G(127) - G(376) - G(388) - 
     -        G(525)) + 
     -     (-(1/(-1 - w)) + w/(1 + w))*
     -      (G(12)*G(128) - G(377) - G(389) - 
     -        G(526)) + 
     -     (1/(1 - w) + w/(-1 + w))*
     -      (G(12)*G(129) - G(382) - G(390) - 
     -        G(527)) + 
     -     (-(1/(1 - w)) - w/(-1 + w))*
     -      (G(12)*G(130) - G(383) - G(391) - 
     -        G(528)) + 
     -     HALF*(-(G(11)*G(166)) + G(475) + 
     -        TWO*G(560)) + 
     -     HALF*(G(11)*G(167) - G(476) - 
     -        TWO*G(561)) + 
     -     HALF*(G(11)*G(168) - G(481) - 
     -        TWO*G(562)) + 
     -     HALF*(-(G(11)*G(169)) + G(482) + 
     -        TWO*G(563)) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      (G(12)*G(193) - G(542) - TWO*G(579)) 
     -      + (-(HALF/(1 + w)) - 
     -        (HALF*w)/(1 + w))*
     -      (G(12)*G(194) - G(543) - TWO*G(580)) 
     -      + (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (G(12)*G(198) - G(555) - TWO*G(584)) 
     -      + (-(HALF/(-1 + w)) + 
     -        (HALF*w)/(-1 + w))*
     -      (G(12)*G(199) - G(556) - TWO*G(585))) 
     -   + FOUR*((G(1)*G(11)**THREE)/6. + 
     -     (G(2)*G(11)**THREE)/6. - 
     -     HALF*G(12)**TWO*G(33) - 
     -     HALF*G(12)**TWO*G(34) + 
     -     HALF*G(12)**TWO*G(40) + 
     -     HALF*G(12)**TWO*G(45) - 
     -     HALF*G(11)**TWO*G(48) - 
     -     HALF*G(11)**TWO*G(49) + 
     -     HALF*G(12)**TWO*G(50) + 
     -     HALF*G(12)**TWO*G(51) + 
     -     (-G(33) - G(34) + TWO*G(35) + 
     -        ((-HALF - HALF*w)*G(38))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(39))/(1 + w) + 
     -        G(40) + 
     -        ((HALF - HALF*w)*G(43))/(-1 + w) + 
     -        ((HALF - HALF*w)*G(44))/(-1 + w) + 
     -        G(45) + G(50) + G(51) - TWO*G(55))*
     -      G(59) + G(12)*G(131) + G(12)*G(132) - 
     -     G(12)*G(148) - G(12)*G(163) + 
     -     G(12)*G(188) + G(12)*G(189) - 
     -     G(12)*G(195) - G(12)*G(200) + 
     -     G(11)*G(203) + G(11)*G(204) - 
     -     TWO*G(12)*G(205) - TWO*G(12)*G(206) - 
     -     G(11)*(-(G(12)*G(33)) - G(12)*G(34) + 
     -        G(12)*G(40) + G(12)*G(45) + 
     -        G(12)*G(50) + G(12)*G(51) + 
     -        G(131) + G(132) - G(148) - G(163) + 
     -        G(188) + G(189) + 
     -        TWO*(G(12)*G(35) - G(133) - 
     -           G(190)) + 
     -        ((-HALF - HALF*w)*
     -           (G(12)*G(38) - G(146) - G(193)))/
     -         (1 + w) + 
     -        ((-HALF - HALF*w)*
     -           (G(12)*G(39) - G(147) - G(194)))/
     -         (1 + w) - G(195) + 
     -        ((HALF - HALF*w)*
     -           (G(12)*G(43) - G(161) - G(198)))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*
     -           (G(12)*G(44) - G(162) - G(199)))/
     -         (-1 + w) - G(200) - TWO*G(205) - 
     -        TWO*G(206) - 
     -        TWO*(G(12)*G(55) - TWO*G(208))) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (-(HALF*G(1)*G(11)**TWO) - 
     -        HALF*G(2)*G(11)**TWO + 
     -        G(11)*G(48) + G(11)*G(49) - 
     -        G(203) - G(204) + TWO*G(212)) - 
     -     G(392) - G(393) + G(431) + G(468) - 
     -     G(529) - G(530) + G(544) + G(557) + 
     -     HALF*(-(HALF*G(11)**TWO*G(13)) + 
     -        G(11)*G(65) + G(11)*G(166) - 
     -        G(230) - G(475) - G(560)) + 
     -     HALF*(-(HALF*G(11)**TWO*G(14)) + 
     -        G(11)*G(66) + G(11)*G(167) - 
     -        G(231) - G(476) - G(561)) + 
     -     HALF*(-(HALF*G(11)**TWO*G(15)) + 
     -        G(11)*G(71) + G(11)*G(168) - 
     -        G(248) - G(481) - G(562)) + 
     -     HALF*(-(HALF*G(11)**TWO*G(16)) + 
     -        G(11)*G(72) + G(11)*G(169) - 
     -        G(249) - G(482) - G(563)) - 
     -     G(574) - G(575) + 
     -     TWO*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) + 
     -     ((-HALF - HALF*w)*
     -        (HALF*G(12)**TWO*G(38) - 
     -          G(12)*G(146) - G(12)*G(193) + 
     -          G(429) + G(542) + G(579)))/(1 + w)
     -       + ((-HALF - HALF*w)*
     -        (HALF*G(12)**TWO*G(39) - 
     -          G(12)*G(147) - G(12)*G(194) + 
     -          G(430) + G(543) + G(580)))/(1 + w)
     -       + G(581) + 
     -     ((HALF - HALF*w)*
     -        (HALF*G(12)**TWO*G(43) - 
     -          G(12)*G(161) - G(12)*G(198) + 
     -          G(466) + G(555) + G(584)))/
     -      (-1 + w) + 
     -     ((HALF - HALF*w)*
     -        (HALF*G(12)**TWO*G(44) - 
     -          G(12)*G(162) - G(12)*G(199) + 
     -          G(467) + G(556) + G(585)))/
     -      (-1 + w) + G(586) - G(589) - G(590) + 
     -     THREE*G(591) + THREE*G(592) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593))) - 
     -  TWO*(-(G(1)*G(11)**THREE)/6. - 
     -     (G(2)*G(11)**THREE)/6. + 
     -     (G(3)*G(12)**THREE)/6. + 
     -     (G(4)*G(12)**THREE)/6. + 
     -     HALF*G(11)**TWO*G(48) + 
     -     HALF*G(11)**TWO*G(49) - 
     -     HALF*G(12)**TWO*G(50) - 
     -     HALF*G(12)**TWO*G(51) + 
     -     (G(3)*G(12) + G(4)*G(12) - TWO*G(21) - 
     -        TWO*G(28) + FOUR*G(35) - G(50) - 
     -        G(51) - TWO*(G(7)*G(12) - G(55)))*
     -      G(59) - G(11)*G(203) - G(11)*G(204) + 
     -     G(12)*G(205) + G(12)*G(206) - 
     -     G(11)*(HALF*G(3)*G(12)**TWO + 
     -        HALF*G(4)*G(12)**TWO - 
     -        G(12)*G(50) - G(12)*G(51) - 
     -        TWO*(G(12)*G(21) - G(93) - 
     -           G(174)) - 
     -        TWO*(G(12)*G(28) - G(116) - 
     -           G(181)) + 
     -        FOUR*(G(12)*G(35) - G(133) - 
     -           G(190)) + G(205) + G(206) - 
     -        TWO*(HALF*G(7)*G(12)**TWO - 
     -           G(12)*G(55) + G(208))) + 
     -     (-G(3) - G(4) + TWO*G(7))*G(212) - 
     -     TWO*(HALF*G(12)**TWO*G(21) - 
     -        G(12)*G(93) - G(12)*G(174) + 
     -        G(302) + G(497) + G(566)) - 
     -     TWO*(HALF*G(12)**TWO*G(28) - 
     -        G(12)*G(116) - G(12)*G(181) + 
     -        G(357) + G(514) + G(571)) + 
     -     FOUR*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) + 
     -     G(589) + G(590) - G(591) - G(592) - 
     -     TWO*((G(7)*G(12)**THREE)/6. - 
     -        HALF*G(12)**TWO*G(55) + 
     -        G(12)*G(208) - G(593)) + TWO*G(596))
     -    - FOUR*(-(HALF*G(11)**TWO*G(48)) - 
     -     HALF*G(11)**TWO*G(49) + 
     -     HALF*G(12)**TWO*G(50) + 
     -     HALF*G(12)**TWO*G(51) + 
     -     (-(TWO*G(33)) - TWO*G(34) + 
     -        FOUR*G(35) + G(50) + G(51) - 
     -        TWO*G(55))*G(59) + 
     -     TWO*G(11)*G(203) + TWO*G(11)*G(204) - 
     -     TWO*G(12)*G(205) - TWO*G(12)*G(206) - 
     -     G(11)*(G(12)*G(50) + G(12)*G(51) - 
     -        TWO*(G(12)*G(33) - G(131) - 
     -           G(188)) - 
     -        TWO*(G(12)*G(34) - G(132) - 
     -           G(189)) + 
     -        FOUR*(G(12)*G(35) - G(133) - 
     -           G(190)) - TWO*G(205) - 
     -        TWO*G(206) - 
     -        TWO*(G(12)*G(55) - TWO*G(208))) + 
     -     (TWO*G(7) - G(12))*
     -      (-(HALF*G(1)*G(11)**TWO) - 
     -        HALF*G(2)*G(11)**TWO + 
     -        G(11)*G(48) + G(11)*G(49) - 
     -        G(203) - G(204) + TWO*G(212)) - 
     -     TWO*(HALF*G(12)**TWO*G(33) - 
     -        G(12)*G(131) - G(12)*G(188) + 
     -        G(392) + G(529) + G(574)) - 
     -     TWO*(HALF*G(12)**TWO*G(34) - 
     -        G(12)*G(132) - G(12)*G(189) + 
     -        G(393) + G(530) + G(575)) + 
     -     FOUR*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) - 
     -     THREE*G(589) - THREE*G(590) + 
     -     THREE*G(591) + THREE*G(592) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) + 
     -     TWO*G(596)) - 
     -  12*(-(G(7)*G(12)**THREE)/6. + 
     -     HALF*G(12)**TWO*G(40) + 
     -     HALF*G(12)**TWO*G(45) + 
     -     HALF*G(12)**TWO*G(55) + 
     -     G(59)*(-(G(7)*G(12)) + TWO*G(35) + 
     -        G(40) + G(45) - G(55) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + G(60)) - G(12)*G(148) - 
     -     G(12)*G(163) - G(12)*G(195) - 
     -     G(12)*G(200) - G(12)*G(208) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*G(212) - 
     -     G(11)*(-(HALF*G(7)*G(12)**TWO) + 
     -        G(12)*G(40) + G(12)*G(45) + 
     -        G(12)*G(55) - G(148) - G(163) + 
     -        TWO*(G(12)*G(35) - G(133) - 
     -           G(190)) - G(195) - G(200) - 
     -        G(208) - 
     -        TWO*(G(12)*G(55) - TWO*G(208)) + 
     -        ((-HALF - HALF*w)*
     -           (HALF*G(8)*G(12)**TWO - 
     -             G(12)*G(56) + G(209)))/(1 + w) 
     -         + ((HALF - HALF*w)*
     -           (HALF*G(10)*G(12)**TWO - 
     -             G(12)*G(58) + G(211)))/(-1 + w)
     -          + G(213)) + G(431) + G(468) + 
     -     G(544) + G(557) + 
     -     TWO*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) + 
     -     G(581) + G(586) + 
     -     HALF*((G(1)*G(11)**THREE)/6. - 
     -        HALF*G(11)**TWO*G(48) + 
     -        G(11)*G(203) - G(589)) + 
     -     HALF*((G(2)*G(11)**THREE)/6. - 
     -        HALF*G(11)**TWO*G(49) + 
     -        G(11)*G(204) - G(590)) + G(593) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) + 
     -     ((-HALF - HALF*w)*
     -        ((G(8)*G(12)**THREE)/6. - 
     -          HALF*G(12)**TWO*G(56) + 
     -          G(12)*G(209) - G(594)))/(1 + w) + 
     -     ((HALF - HALF*w)*
     -        ((G(10)*G(12)**THREE)/6. - 
     -          HALF*G(12)**TWO*G(58) + 
     -          G(12)*G(211) - G(595)))/(-1 + w) 
     -      + G(597)) - 
     -  8*(-(HALF*G(12)**TWO*G(55)) + 
     -     G(59)*(TWO*G(35) + 
     -        (1/(1 + w) + w/(1 + w))*G(36) + 
     -        (1/(1 - w) + w/(-1 + w))*G(37) - 
     -        TWO*(G(7)*G(12) - G(55)) - G(55) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(58) + G(60)) + 
     -     (TWO*G(7) - G(12))*
     -      (HALF*(HALF*G(1)*G(11)**TWO - 
     -           G(11)*G(48) + G(203)) + 
     -        HALF*(HALF*G(2)*G(11)**TWO - 
     -           G(11)*G(49) + G(204))) + 
     -     TWO*G(12)*G(208) - 
     -     G(11)*(-(G(12)*G(55)) + 
     -        TWO*(G(12)*G(35) - G(133) - 
     -           G(190)) + 
     -        (1/(1 + w) + w/(1 + w))*
     -         (G(12)*G(36) - G(134) - G(191)) + 
     -        (1/(1 - w) + w/(-1 + w))*
     -         (G(12)*G(37) - G(135) - G(192)) + 
     -        TWO*G(208) - 
     -        TWO*(HALF*G(7)*G(12)**TWO - 
     -           G(12)*G(55) + G(208)) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         (G(12)*G(58) - TWO*G(211)) + G(213)
     -        ) + TWO*
     -      (HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) + 
     -     (1/(1 + w) + w/(1 + w))*
     -      (HALF*G(12)**TWO*G(36) - 
     -        G(12)*G(134) - G(12)*G(191) + 
     -        G(395) + G(532) + G(577)) + 
     -     (1/(1 - w) + w/(-1 + w))*
     -      (HALF*G(12)**TWO*G(37) - 
     -        G(12)*G(135) - G(12)*G(192) + 
     -        G(396) + G(533) + G(578)) + 
     -     HALF*(HALF*G(11)**TWO*G(48) - 
     -        TWO*G(11)*G(203) + THREE*G(589)) + 
     -     HALF*(HALF*G(11)**TWO*G(49) - 
     -        TWO*G(11)*G(204) + THREE*G(590)) - 
     -     TWO*((G(7)*G(12)**THREE)/6. - 
     -        HALF*G(12)**TWO*G(55) + 
     -        G(12)*G(208) - G(593)) - 
     -     THREE*G(593) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      (HALF*G(12)**TWO*G(56) - 
     -        TWO*G(12)*G(209) + THREE*G(594)) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (HALF*G(12)**TWO*G(58) - 
     -        TWO*G(12)*G(211) + THREE*G(595)) + 
     -     G(597)) + 
     -  8*(-(G(7)*G(12)**THREE)/6. + 
     -     HALF*G(12)**TWO*G(35) + 
     -     G(59)*(-(G(7)*G(12)) + G(35) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(36) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(37) + 
     -        ((HALF + HALF*w)*G(40))/(1 + w) + 
     -        ((1/(4.*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.*(1 + w)))*G(41))/
     -         (1 + w) + 
     -        ((-1/(4.*(-1 + w)) + 
     -             w**TWO/(4.*(-1 + w)))*G(42))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(45))/(-1 + w) + 
     -        ((-1/(4.*(1 + w)) + 
     -             w**TWO/(4.*(1 + w)))*G(46))/
     -         (-1 + w) + 
     -        ((1/(4.*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.*(-1 + w)))*G(47))/
     -         (-1 + w) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + (HALF/(-1 + w) - 
     -           (HALF*w)/(-1 + w))*G(58) + G(60))
     -       - G(12)*G(133) - G(12)*G(190) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (HALF*(HALF*G(1)*G(11)**TWO - 
     -           G(11)*G(48) + G(203)) + 
     -        HALF*(HALF*G(2)*G(11)**TWO - 
     -           G(11)*G(49) + G(204))) + 
     -     G(12)*G(208) - 
     -     G(11)*(-(HALF*G(7)*G(12)**TWO) + 
     -        G(12)*G(35) - G(133) - G(190) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         (G(12)*G(36) - G(134) - G(191)) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*
     -         (G(12)*G(37) - G(135) - G(192)) + 
     -        ((HALF + HALF*w)*
     -           (G(12)*G(40) - G(148) - G(195)))/
     -         (1 + w) + 
     -        ((1/(4.*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.*(1 + w)))*
     -           (G(12)*G(41) - G(149) - G(196)))/
     -         (1 + w) + 
     -        ((-1/(4.*(-1 + w)) + 
     -             w**TWO/(4.*(-1 + w)))*
     -           (G(12)*G(42) - G(150) - G(197)))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*
     -           (G(12)*G(45) - G(163) - G(200)))/
     -         (-1 + w) + 
     -        ((-1/(4.*(1 + w)) + 
     -             w**TWO/(4.*(1 + w)))*
     -           (G(12)*G(46) - G(164) - G(201)))/
     -         (-1 + w) + 
     -        ((1/(4.*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.*(-1 + w)))*
     -           (G(12)*G(47) - G(165) - G(202)))/
     -         (-1 + w) + G(208) + 
     -        ((-HALF - HALF*w)*
     -           (HALF*G(8)*G(12)**TWO - 
     -             G(12)*G(56) + G(209)))/(1 + w) 
     -         + (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        ((HALF - HALF*w)*
     -           (HALF*G(10)*G(12)**TWO - 
     -             G(12)*G(58) + G(211)))/(-1 + w)
     -          + (HALF/(-1 + w) - 
     -           (HALF*w)/(-1 + w))*
     -         (G(12)*G(58) - TWO*G(211)) + G(213)
     -        ) + G(394) + G(531) + 
     -     (HALF*G(11)**TWO*G(13) - G(11)*G(65) - 
     -        G(11)*G(166) + G(230) + G(475) + 
     -        G(560))/4. + 
     -     (HALF*G(11)**TWO*G(14) - G(11)*G(66) - 
     -        G(11)*G(167) + G(231) + G(476) + 
     -        G(561))/4. + 
     -     (HALF*G(11)**TWO*G(15) - G(11)*G(71) - 
     -        G(11)*G(168) + G(248) + G(481) + 
     -        G(562))/4. + 
     -     (HALF*G(11)**TWO*G(16) - G(11)*G(72) - 
     -        G(11)*G(169) + G(249) + G(482) + 
     -        G(563))/4. + G(576) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      (HALF*G(12)**TWO*G(36) - 
     -        G(12)*G(134) - G(12)*G(191) + 
     -        G(395) + G(532) + G(577)) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      (HALF*G(12)**TWO*G(37) - 
     -        G(12)*G(135) - G(12)*G(192) + 
     -        G(396) + G(533) + G(578)) + 
     -     ((HALF + HALF*w)*
     -        (HALF*G(12)**TWO*G(40) - 
     -          G(12)*G(148) - G(12)*G(195) + 
     -          G(431) + G(544) + G(581)))/(1 + w)
     -       + ((1/(4.*(1 + w)) + 
     -          (HALF*w)/(1 + w) + 
     -          w**TWO/(4.*(1 + w)))*
     -        (HALF*G(12)**TWO*G(41) - 
     -          G(12)*G(149) - G(12)*G(196) + 
     -          G(432) + G(545) + G(582)))/(1 + w)
     -       + ((-1/(4.*(-1 + w)) + 
     -          w**TWO/(4.*(-1 + w)))*
     -        (HALF*G(12)**TWO*G(42) - 
     -          G(12)*G(150) - G(12)*G(197) + 
     -          G(433) + G(546) + G(583)))/(1 + w)
     -       + ((-HALF + HALF*w)*
     -        (HALF*G(12)**TWO*G(45) - 
     -          G(12)*G(163) - G(12)*G(200) + 
     -          G(468) + G(557) + G(586)))/
     -      (-1 + w) + 
     -     ((-1/(4.*(1 + w)) + 
     -          w**TWO/(4.*(1 + w)))*
     -        (HALF*G(12)**TWO*G(46) - 
     -          G(12)*G(164) - G(12)*G(201) + 
     -          G(469) + G(558) + G(587)))/
     -      (-1 + w) + 
     -     ((1/(4.*(-1 + w)) - 
     -          (HALF*w)/(-1 + w) + 
     -          w**TWO/(4.*(-1 + w)))*
     -        (HALF*G(12)**TWO*G(47) - 
     -          G(12)*G(165) - G(12)*G(202) + 
     -          G(470) + G(559) + G(588)))/
     -      (-1 + w) - TWO*G(593) + 
     -     ((-HALF - HALF*w)*
     -        ((G(8)*G(12)**THREE)/6. - 
     -          HALF*G(12)**TWO*G(56) + 
     -          G(12)*G(209) - G(594)))/(1 + w) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      (HALF*G(12)**TWO*G(56) - 
     -        TWO*G(12)*G(209) + THREE*G(594)) + 
     -     ((HALF - HALF*w)*
     -        ((G(10)*G(12)**THREE)/6. - 
     -          HALF*G(12)**TWO*G(58) + 
     -          G(12)*G(211) - G(595)))/(-1 + w) 
     -      + (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (HALF*G(12)**TWO*G(58) - 
     -        TWO*G(12)*G(211) + THREE*G(595)) + 
     -     G(597)) + 
     -  14*(G(59)*(FOUR*G(35) - 
     -        TWO*(G(7)*G(12) - G(55)) - 
     -        TWO*G(55) + G(60)) + 
     -     (TWO*G(7) - G(12))*G(212) - 
     -     G(11)*(FOUR*
     -         (G(12)*G(35) - G(133) - G(190)) - 
     -        TWO*(HALF*G(7)*G(12)**TWO - 
     -           G(12)*G(55) + G(208)) - 
     -        TWO*(G(12)*G(55) - TWO*G(208)) + 
     -        G(213)) + 
     -     FOUR*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) - 
     -     TWO*((G(7)*G(12)**THREE)/6. - 
     -        HALF*G(12)**TWO*G(55) + 
     -        G(12)*G(208) - G(593)) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) + 
     -     G(596) + G(597))
      return
      end function fE21Euclid_w4

      function fE22Euclid_w3(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE22Euclid_w3
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE22Euclid_w3=        -(FOUR*zeta2*
     -     (-(HALF*G(1)) + HALF*G(2) - 
     -       HALF*G(3) + HALF*G(4))) - 
     -  8*((G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      (G(13) - G(14) - G(15) + G(16)) + 
     -     (G(1) - G(2))*
     -      (((-1 - w)*G(38))/(1 + w) + G(39) + 
     -        G(43) + ((1 - w)*G(44))/(-1 + w)) - 
     -     G(61) + G(62) + G(63) - G(64) + 
     -     G(67) - G(68) - G(69) + G(70) + 
     -     G(136) + ((-1 - w)*G(137))/(1 + w) + 
     -     ((-1 - w)*G(138))/(1 + w) + G(139) + 
     -     ((1 - w)*G(151))/(-1 + w) + G(152) + 
     -     G(153) + ((1 - w)*G(154))/(-1 + w)) + 
     -  6*(-(G(12)*(G(13) - G(14) + G(15) - 
     -          G(16))) + 
     -     (G(1) - G(2))*
     -      (-G(50) - G(51) + TWO*G(53)) - 
     -     G(166) + G(167) - G(168) + G(169) + 
     -     G(170) - G(171) + G(177) - G(178) - 
     -     TWO*G(186) + TWO*G(187)) - 
     -  TWO*(-(G(12)*
     -        (-G(13) + G(14) - G(15) + G(16) + 
     -          TWO*G(48) - TWO*G(49))) + 
     -     (G(1) - G(2))*
     -      (G(50) + G(51) - TWO*G(55)) + 
     -     G(166) - G(167) + G(168) - G(169) - 
     -     G(170) + G(171) - G(177) + G(178) + 
     -     TWO*G(188) - TWO*G(189) - TWO*G(203) + 
     -     TWO*G(204)) + 
     -  8*((G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*(-G(48) + G(49)) + 
     -     (G(1) - G(2))*
     -      (-(G(7)*G(12)) + G(55) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + G(60)) - HALF*G(65) + 
     -     HALF*G(66) - HALF*G(71) + HALF*G(72) + 
     -     G(131) - G(132) + 
     -     ((HALF + HALF*w)*G(146))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(147))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(161))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(162))/(-1 + w) - 
     -     G(205) + G(206)) - 
     -  FOUR*((TWO*G(7) - G(12))*
     -      (-G(48) + G(49)) + 
     -     (G(1) - G(2))*
     -      (-(TWO*(G(7)*G(12) - G(55))) + G(60)) 
     -      + TWO*G(131) - TWO*G(132) - G(203) + 
     -     G(204) - G(205) + G(206)) + 
     -  TWO*((-6*G(5) - TWO*G(7) + 
     -        ((TWO + TWO*w)*G(8))/(1 + w) + 
     -        ((-TWO + TWO*w)*G(10))/(-1 + w) + 
     -        TWO*G(12))*(-G(48) + G(49)) + 
     -     (G(1) - G(2))*
     -      (6*(G(5)*G(12) - G(52)) + 
     -        TWO*(G(7)*G(12) - G(55)) + 
     -        ((-TWO - TWO*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((TWO - TWO*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         - TWO*G(60)) - TWO*G(65) + 
     -     TWO*G(66) - TWO*G(71) + TWO*G(72) - 
     -     6*G(119) + 6*G(120) - TWO*G(131) + 
     -     TWO*G(132) + 
     -     ((TWO + TWO*w)*G(146))/(1 + w) + 
     -     ((-TWO - TWO*w)*G(147))/(1 + w) + 
     -     ((-TWO + TWO*w)*G(161))/(-1 + w) + 
     -     ((TWO - TWO*w)*G(162))/(-1 + w) + 
     -     TWO*G(205) - TWO*G(206)) + 
     -  FOUR*(G(11)*G(13) - G(11)*G(14) + 
     -     G(11)*G(15) - G(11)*G(16) + 
     -     G(12)*G(33) - G(12)*G(34) + 
     -     G(12)*G(39) + G(12)*G(44) + 
     -     (-6*G(5) - TWO*G(7) + 
     -        ((TWO + TWO*w)*G(8))/(1 + w) + 
     -        ((-TWO + TWO*w)*G(10))/(-1 + w) + 
     -        TWO*G(12))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) - 
     -     G(12)*G(50) + G(12)*G(51) - 
     -     G(11)*(THREE*G(31) - THREE*G(32) + 
     -        G(33) - G(34) + 
     -        ((-1 - w)*G(38))/(1 + w) + G(39) + 
     -        ((1 - w)*G(43))/(-1 + w) + G(44) - 
     -        G(50) + G(51)) - G(65) + G(66) - 
     -     G(71) + G(72) - G(131) + G(132) - 
     -     G(147) - G(162) - G(166) + G(167) - 
     -     G(168) + G(169) + 
     -     THREE*(G(12)*G(31) - G(119) - 
     -        G(184)) - 
     -     THREE*(G(12)*G(32) - G(120) - 
     -        G(185)) - G(188) + G(189) + 
     -     ((-1 - w)*
     -        (G(12)*G(38) - G(146) - G(193)))/
     -      (1 + w) - G(194) + 
     -     ((1 - w)*(G(12)*G(43) - G(161) - 
     -          G(198)))/(-1 + w) - G(199) + 
     -     TWO*G(205) - TWO*G(206)) + 
     -  FOUR*(G(12)*G(21) - G(12)*G(28) - 
     -     G(11)*(THREE*G(20) + G(21) - 
     -        THREE*G(27) - G(28) - 
     -        TWO*(G(3)*G(12) - G(50)) + 
     -        TWO*(G(4)*G(12) - G(51))) + 
     -     FOUR*(-(HALF*G(3)) + HALF*G(4))*
     -      G(59) - G(93) + G(116) + 
     -     THREE*(G(12)*G(20) - G(92) - G(173)) - 
     -     G(174) - THREE*
     -      (G(12)*G(27) - G(115) - G(180)) + 
     -     G(181) - TWO*
     -      (HALF*G(1)*G(11)**TWO - G(11)*G(48) + 
     -        G(203)) + 
     -     TWO*(HALF*G(2)*G(11)**TWO - 
     -        G(11)*G(49) + G(204)) - 
     -     TWO*(HALF*G(3)*G(12)**TWO - 
     -        G(12)*G(50) + G(205)) + 
     -     TWO*(HALF*G(4)*G(12)**TWO - 
     -        G(12)*G(51) + G(206))) + 
     -  16*((G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) - 
     -     G(11)*(-(HALF*G(33)) + HALF*G(34) + 
     -        ((-0.25 - w/4.)*G(38))/(1 + w) + 
     -        ((0.25 + w/4.)*G(39))/(1 + w) + 
     -        ((0.25 - w/4.)*G(43))/(-1 + w) + 
     -        ((-0.25 + w/4.)*G(44))/(-1 + w) + 
     -        HALF*G(50) - HALF*G(51)) + 
     -     (G(11)*G(13) - G(65) - G(166))/4. + 
     -     (-(G(11)*G(14)) + G(66) + G(167))/4. + 
     -     (G(11)*G(15) - G(71) - G(168))/4. + 
     -     (-(G(11)*G(16)) + G(72) + G(169))/4. + 
     -     HALF*(-(G(12)*G(33)) + G(131) + 
     -        G(188)) + 
     -     HALF*(G(12)*G(34) - G(132) - G(189)) + 
     -     ((-0.25 - w/4.)*
     -        (G(12)*G(38) - G(146) - G(193)))/
     -      (1 + w) + 
     -     ((0.25 + w/4.)*
     -        (G(12)*G(39) - G(147) - G(194)))/
     -      (1 + w) + 
     -     ((0.25 - w/4.)*
     -        (G(12)*G(43) - G(161) - G(198)))/
     -      (-1 + w) + 
     -     ((-0.25 + w/4.)*
     -        (G(12)*G(44) - G(162) - G(199)))/
     -      (-1 + w) + 
     -     HALF*(G(12)*G(50) - TWO*G(205)) + 
     -     HALF*(-(G(12)*G(51)) + TWO*G(206))) - 
     -  8*(-(G(12)*G(33)) + G(12)*G(34) + 
     -     (TWO*G(7) - G(12))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) - 
     -     G(11)*(-G(33) + G(34) + HALF*G(50) - 
     -        HALF*G(51)) + G(131) - G(132) + 
     -     G(188) - G(189) + 
     -     HALF*(G(11)*G(48) - TWO*G(203)) + 
     -     HALF*(-(G(11)*G(49)) + TWO*G(204)) + 
     -     HALF*(G(12)*G(50) - TWO*G(205)) + 
     -     HALF*(-(G(12)*G(51)) + TWO*G(206))) - 
     -  8*(-(HALF*G(1)*G(11)**TWO) + 
     -     HALF*G(2)*G(11)**TWO + 
     -     HALF*G(8)*G(12)**TWO + G(11)*G(48) - 
     -     G(11)*G(49) - G(12)*G(56) - 
     -     G(11)*(G(8)*G(12) - G(56) + 
     -        ((1 - w)*(G(10)*G(12) - G(58)))/
     -         (-1 + w)) + 
     -     (G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      G(59) - G(203) + G(204) + G(209) + 
     -     ((1 - w)*(HALF*G(10)*G(12)**TWO - 
     -          G(12)*G(58) + G(211)))/(-1 + w))
      return
      end function fE22Euclid_w3

      function fE27Euclid_w4(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex fE27Euclid_w4
      double complex w,z
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      fE27Euclid_w4=        8*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(22) + 
     -        (1/(1 - w) + w/(-1 + w))*G(23) + 
     -        G(29) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(30)) + 
     -     (-G(3) + G(4))*
     -      (-G(61) + G(62) + G(63) - G(64) + 
     -        G(67) - G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      ((1/(1 + w) + w/(1 + w))*G(85) - 
     -        G(86) - G(87) + 
     -        (1/(1 - w) + w/(-1 + w))*G(88) + 
     -        (1/(-1 - w) - w/(1 + w))*G(108) + 
     -        G(109) + G(110) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(111)) - 
     -     G(214) + G(215) + G(216) - G(217) + 
     -     G(220) - G(221) - G(222) + G(223) + 
     -     G(232) - G(233) - G(234) + G(235) - 
     -     G(238) + G(239) + G(240) - G(241) - 
     -     G(276) + (1/(1 + w) + w/(1 + w))*
     -      G(277) + 
     -     (1/(1 + w) + w/(1 + w))*G(278) - 
     -     G(279) + (-(1/(-1 + w)) + w/(-1 + w))*
     -      G(282) - G(283) - G(284) + 
     -     (-(1/(-1 + w)) + w/(-1 + w))*G(285) + 
     -     G(331) + (-(1/(1 + w)) - w/(1 + w))*
     -      G(332) + 
     -     (-(1/(1 + w)) - w/(1 + w))*G(333) + 
     -     G(334) + (1/(-1 + w) - w/(-1 + w))*
     -      G(337) + G(338) + G(339) + 
     -     (1/(-1 + w) - w/(-1 + w))*G(340)) - 
     -  8*((G(13) - G(14) + G(15) - G(16))*
     -      (G(3)*G(12) - G(4)*G(12) - G(50) + 
     -        G(51)) + 
     -     (G(1) - G(2))*
     -      (G(89) + G(90) - TWO*G(91) - G(112) - 
     -        G(113) + TWO*G(114)) + 
     -     (-G(3) + G(4))*
     -      (-G(166) + G(167) - G(168) + G(169)) 
     -      - G(226) + G(227) - G(228) + G(229) + 
     -     G(244) - G(245) + G(246) - G(247) - 
     -     G(288) + G(289) - G(290) + G(291) + 
     -     TWO*G(292) - TWO*G(293) + G(343) - 
     -     G(344) + G(345) - G(346) - 
     -     TWO*G(347) + TWO*G(348)) + 
     -  8*((-G(48) + G(49))*
     -      (G(3)*G(12) - G(4)*G(12) - 
     -        TWO*G(21) + TWO*G(28) - G(50) + 
     -        G(51)) + 
     -     (-G(3) + G(4))*(-G(203) + G(204)) + 
     -     (G(1) - G(2))*
     -      (-(HALF*G(3)*G(12)**TWO) + 
     -        HALF*G(4)*G(12)**TWO + 
     -        G(12)*G(50) - G(12)*G(51) + 
     -        TWO*(G(12)*G(21) - G(93) - 
     -           G(174)) - 
     -        TWO*(G(12)*G(28) - G(116) - 
     -           G(181)) - G(205) + G(206)) - 
     -     G(230) + G(231) + G(248) - G(249) - 
     -     TWO*G(274) + TWO*G(275) + G(300) - 
     -     G(301) + TWO*G(329) - TWO*G(330) - 
     -     G(355) + G(356)) - 
     -  16*((-G(48) + G(49))*
     -      (G(3)*G(12) - G(4)*G(12) - G(21) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(22) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(23) + G(28) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(29) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(30) - 
     -        G(50) + G(51)) + 
     -     (-G(3) + G(4))*
     -      (-(HALF*G(65)) + HALF*G(66) - 
     -        HALF*G(71) + HALF*G(72)) + 
     -     (G(1) - G(2))*
     -      (-(HALF*G(3)*G(12)**TWO) + 
     -        HALF*G(4)*G(12)**TWO + 
     -        G(12)*G(21) - G(12)*G(28) + 
     -        G(12)*G(50) - G(12)*G(51) - G(93) + 
     -        G(116) - G(174) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         (G(12)*G(22) - G(94) - G(175)) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*
     -         (G(12)*G(23) - G(95) - G(176)) + 
     -        G(181) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*
     -         (G(12)*G(29) - G(117) - G(182)) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         (G(12)*G(30) - G(118) - G(183)) - 
     -        G(205) + G(206)) - HALF*G(218) + 
     -     HALF*G(219) - HALF*G(224) + 
     -     HALF*G(225) + HALF*G(236) - 
     -     HALF*G(237) + HALF*G(242) - 
     -     HALF*G(243) - G(274) + G(275) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(280) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(281) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(286) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(287) + G(300) - G(301) + G(329) - 
     -     G(330) + (HALF/(1 + w) + 
     -        (HALF*w)/(1 + w))*G(335) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(336) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(341) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(342) - G(355) + G(356)) - 
     -  8*((G(13) - G(14) + G(15) - G(16))*
     -      (G(50) - G(51)) - 
     -     G(12)*(G(61) - G(62) + G(63) - G(64) - 
     -        G(67) + G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      (G(170) + G(171) - TWO*G(172) - 
     -        G(177) - G(178) + TWO*G(179)) - 
     -     G(471) + G(472) - G(473) + G(474) + 
     -     G(477) - G(478) + G(479) - G(480) - 
     -     G(483) + G(484) - G(485) + G(486) + 
     -     TWO*G(487) - TWO*G(488) + G(500) - 
     -     G(501) + G(502) - G(503) - 
     -     TWO*G(504) + TWO*G(505)) + 
     -  16*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (G(3)*G(12) - G(4)*G(12) - 
     -        TWO*G(21) + TWO*G(28) - G(50) + 
     -        G(51)) + G(12)*G(83) - 
     -     G(12)*G(84) - G(12)*G(106) + 
     -     G(12)*G(107) - 
     -     G(11)*(G(83) - G(84) - HALF*G(89) + 
     -        HALF*G(90) - G(106) + G(107) + 
     -        HALF*G(112) - HALF*G(113)) + 
     -     (-G(3) + G(4))*
     -      (HALF*(G(11)*G(48) - TWO*G(203)) + 
     -        HALF*(-(G(11)*G(49)) + TWO*G(204))) 
     -      - G(274) + G(275) - G(294) + G(295) + 
     -     G(329) - G(330) + G(349) - G(350) + 
     -     HALF*(G(11)*G(65) - TWO*G(230) - 
     -        G(475)) + 
     -     HALF*(-(G(11)*G(66)) + TWO*G(231) + 
     -        G(476)) + 
     -     HALF*(-(G(11)*G(71)) + TWO*G(248) + 
     -        G(481)) + 
     -     HALF*(G(11)*G(72) - TWO*G(249) - 
     -        G(482)) - G(489) + G(490) + 
     -     HALF*(-(G(12)*G(89)) + TWO*G(300) + 
     -        G(495)) + 
     -     HALF*(G(12)*G(90) - TWO*G(301) - 
     -        G(496)) + G(506) - G(507) + 
     -     HALF*(G(12)*G(112) - TWO*G(355) - 
     -        G(512)) + 
     -     HALF*(-(G(12)*G(113)) + TWO*G(356) + 
     -        G(513))) - 
     -  32*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (G(3)*G(12) - G(4)*G(12) - G(21) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(22) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(23) + G(28) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(29) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(30) - 
     -        G(50) + G(51)) - 
     -     G(11)*(HALF*G(83) - HALF*G(84) + 
     -        (1/(4.*(1 + w)) + w/(4.*(1 + w)))*
     -         G(85) + 
     -        (-1/(4.*(1 + w)) - w/(4.*(1 + w)))*
     -         G(86) + 
     -        (-1/(4.*(-1 + w)) + 
     -           w/(4.*(-1 + w)))*G(87) + 
     -        (1/(4.*(-1 + w)) - w/(4.*(-1 + w)))*
     -         G(88) - HALF*G(89) + HALF*G(90) - 
     -        HALF*G(106) + HALF*G(107) + 
     -        (-1/(4.*(1 + w)) - w/(4.*(1 + w)))*
     -         G(108) + 
     -        (1/(4.*(1 + w)) + w/(4.*(1 + w)))*
     -         G(109) + 
     -        (1/(4.*(-1 + w)) - w/(4.*(-1 + w)))*
     -         G(110) + 
     -        (-1/(4.*(-1 + w)) + 
     -           w/(4.*(-1 + w)))*G(111) + 
     -        HALF*G(112) - HALF*G(113)) + 
     -     (-G(3) + G(4))*
     -      ((G(11)*G(13) - G(65) - G(166))/4. + 
     -        (-(G(11)*G(14)) + G(66) + G(167))/
     -         4. + (G(11)*G(15) - G(71) - 
     -           G(168))/4. + 
     -        (-(G(11)*G(16)) + G(72) + G(169))/4.
     -        ) + (G(11)*G(61) - G(218) - 
     -        G(226) - G(471))/4. + 
     -     (-(G(11)*G(62)) + G(219) + G(227) + 
     -        G(472))/4. + 
     -     (G(11)*G(63) - G(224) - G(228) - 
     -        G(473))/4. + 
     -     (-(G(11)*G(64)) + G(225) + G(229) + 
     -        G(474))/4. + 
     -     (-(G(11)*G(67)) + G(236) + G(244) + 
     -        G(477))/4. + 
     -     (G(11)*G(68) - G(237) - G(245) - 
     -        G(478))/4. + 
     -     (-(G(11)*G(69)) + G(242) + G(246) + 
     -        G(479))/4. + 
     -     (G(11)*G(70) - G(243) - G(247) - 
     -        G(480))/4. + 
     -     HALF*(G(12)*G(83) - G(274) - G(294) - 
     -        G(489)) + 
     -     HALF*(-(G(12)*G(84)) + G(275) + 
     -        G(295) + G(490)) + 
     -     (1/(4.*(1 + w)) + w/(4.*(1 + w)))*
     -      (G(12)*G(85) - G(280) - G(296) - 
     -        G(491)) + 
     -     (-1/(4.*(1 + w)) - w/(4.*(1 + w)))*
     -      (G(12)*G(86) - G(281) - G(297) - 
     -        G(492)) + 
     -     (-1/(4.*(-1 + w)) + w/(4.*(-1 + w)))*
     -      (G(12)*G(87) - G(286) - G(298) - 
     -        G(493)) + 
     -     (1/(4.*(-1 + w)) - w/(4.*(-1 + w)))*
     -      (G(12)*G(88) - G(287) - G(299) - 
     -        G(494)) + 
     -     HALF*(-(G(12)*G(89)) + TWO*G(300) + 
     -        G(495)) + 
     -     HALF*(G(12)*G(90) - TWO*G(301) - 
     -        G(496)) + 
     -     HALF*(-(G(12)*G(106)) + G(329) + 
     -        G(349) + G(506)) + 
     -     HALF*(G(12)*G(107) - G(330) - G(350) - 
     -        G(507)) + 
     -     (-1/(4.*(1 + w)) - w/(4.*(1 + w)))*
     -      (G(12)*G(108) - G(335) - G(351) - 
     -        G(508)) + 
     -     (1/(4.*(1 + w)) + w/(4.*(1 + w)))*
     -      (G(12)*G(109) - G(336) - G(352) - 
     -        G(509)) + 
     -     (1/(4.*(-1 + w)) - w/(4.*(-1 + w)))*
     -      (G(12)*G(110) - G(341) - G(353) - 
     -        G(510)) + 
     -     (-1/(4.*(-1 + w)) + w/(4.*(-1 + w)))*
     -      (G(12)*G(111) - G(342) - G(354) - 
     -        G(511)) + 
     -     HALF*(G(12)*G(112) - TWO*G(355) - 
     -        G(512)) + 
     -     HALF*(-(G(12)*G(113)) + TWO*G(356) + 
     -        G(513))) + 
     -  8*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(50) - G(51) + TWO*G(53)) - 
     -     G(12)*(G(61) - G(62) - G(63) + G(64) + 
     -        G(67) - G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      (G(170) - G(171) + G(177) - G(178) - 
     -        TWO*G(186) + TWO*G(187)) - G(471) + 
     -     G(472) + G(473) - G(474) - G(477) + 
     -     G(478) + G(479) - G(480) - G(483) + 
     -     G(484) + G(485) - G(486) - G(500) + 
     -     G(501) + G(502) - G(503) + 
     -     TWO*G(517) - TWO*G(518) - TWO*G(519) + 
     -     TWO*G(520)) + 
     -  8*((-G(48) + G(49))*
     -      (-G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) - 
     -     G(12)*(G(65) - G(66) - G(71) + 
     -        G(72)) + 
     -     (G(1) - G(2))*
     -      (-(G(12)*G(58)) + 
     -        (1/(1 + w) + w/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        TWO*G(211)) - G(475) + G(476) + 
     -     G(481) - G(482) - G(542) + 
     -     (1/(1 + w) + w/(1 + w))*G(543) + 
     -     (1/(1 - w) + w/(-1 + w))*G(555) - 
     -     G(556)) - 
     -  16*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(55) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(58) + G(60)) - 
     -     G(12)*(HALF*G(61) - HALF*G(62) - 
     -        HALF*G(63) + HALF*G(64) + 
     -        HALF*G(67) - HALF*G(68) - 
     -        HALF*G(69) + HALF*G(70)) + 
     -     (G(1) - G(2))*
     -      (G(188) - G(189) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(198) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(199) - G(205) + G(206)) - 
     -     HALF*G(471) + HALF*G(472) + 
     -     HALF*G(473) - HALF*G(474) - 
     -     HALF*G(477) + HALF*G(478) + 
     -     HALF*G(479) - HALF*G(480) - G(521) + 
     -     G(522) + G(523) - G(524) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(534) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(535) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(536) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(537) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(547) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(548) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(549) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(550) + G(564) - G(565) - G(569) + 
     -     G(570)) + 
     -  8*((G(13) - G(14) - G(15) + G(16))*
     -      (-(TWO*G(55)) + G(60)) - 
     -     G(12)*(G(166) - G(167) - G(168) + 
     -        G(169)) + 
     -     (G(1) - G(2))*
     -      (TWO*G(188) - TWO*G(189) - G(205) + 
     -        G(206)) - TWO*G(521) + TWO*G(522) + 
     -     TWO*G(523) - TWO*G(524) - G(560) + 
     -     G(561) + G(562) - G(563) + G(564) - 
     -     G(565) - G(569) + G(570)) + 
     -  8*(-(HALF*G(11)**TWO*G(13)) + 
     -     HALF*G(11)**TWO*G(14) + 
     -     HALF*G(11)**TWO*G(15) - 
     -     HALF*G(11)**TWO*G(16) - 
     -     HALF*G(12)**TWO*G(22) + 
     -     HALF*G(12)**TWO*G(29) + 
     -     (-G(22) + 
     -        (1/(1 - w) + w/(-1 + w))*G(23) + 
     -        G(29) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(30))*
     -      G(59) + G(11)*G(65) - G(11)*G(66) - 
     -     G(11)*G(71) + G(11)*G(72) + 
     -     G(12)*G(94) - G(12)*G(117) + 
     -     G(11)*G(166) - G(11)*G(167) - 
     -     G(11)*G(168) + G(11)*G(169) + 
     -     G(12)*G(175) - G(12)*G(182) - 
     -     G(11)*(-(G(12)*G(22)) + G(12)*G(29) + 
     -        G(94) - G(117) + G(175) + 
     -        (1/(1 - w) + w/(-1 + w))*
     -         (G(12)*G(23) - G(95) - G(176)) - 
     -        G(182) + 
     -        (1/(-1 + w) - w/(-1 + w))*
     -         (G(12)*G(30) - G(118) - G(183))) + 
     -     (-G(3) + G(4))*
     -      (-(HALF*G(1)*G(11)**TWO) + 
     -        HALF*G(2)*G(11)**TWO + 
     -        G(11)*G(48) - G(11)*G(49) - 
     -        G(203) + G(204)) - G(230) + 
     -     G(231) + G(248) - G(249) - G(303) + 
     -     G(358) - G(475) + G(476) + G(481) - 
     -     G(482) - G(498) + G(515) - G(560) + 
     -     G(561) + G(562) - G(563) - G(567) + 
     -     (1/(1 - w) + w/(-1 + w))*
     -      (HALF*G(12)**TWO*G(23) - 
     -        G(12)*G(95) - G(12)*G(176) + 
     -        G(304) + G(499) + G(568)) + 
     -     G(572) + (1/(-1 + w) - w/(-1 + w))*
     -      (HALF*G(12)**TWO*G(30) - 
     -        G(12)*G(118) - G(12)*G(183) + 
     -        G(359) + G(516) + G(573))) + 
     -  16*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (-G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) - 
     -     G(12)*(HALF*
     -         (-(G(11)*G(13)) + G(65) + G(166)) 
     -         + HALF*
     -         (G(11)*G(14) - G(66) - G(167)) + 
     -        HALF*(G(11)*G(15) - G(71) - 
     -           G(168)) + 
     -        HALF*(-(G(11)*G(16)) + G(72) + 
     -           G(169))) - 
     -     G(11)*((HALF/(1 + w) + 
     -           (HALF*w)/(1 + w))*G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(198) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(199)) + 
     -     HALF*(G(11)*G(166) - G(475) - 
     -        TWO*G(560)) + 
     -     HALF*(-(G(11)*G(167)) + G(476) + 
     -        TWO*G(561)) + 
     -     HALF*(-(G(11)*G(168)) + G(481) + 
     -        TWO*G(562)) + 
     -     HALF*(G(11)*G(169) - G(482) - 
     -        TWO*G(563)) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      (G(12)*G(193) - G(542) - TWO*G(579)) 
     -      + (-(HALF/(1 + w)) - 
     -        (HALF*w)/(1 + w))*
     -      (G(12)*G(194) - G(543) - TWO*G(580)) 
     -      + (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (G(12)*G(198) - G(555) - TWO*G(584)) 
     -      + (-(HALF/(-1 + w)) + 
     -        (HALF*w)/(-1 + w))*
     -      (G(12)*G(199) - G(556) - TWO*G(585))) 
     -   - 8*(HALF*G(11)**TWO*G(48) + 
     -     HALF*G(11)**TWO*G(49) + 
     -     HALF*G(12)**TWO*G(50) + 
     -     HALF*G(12)**TWO*G(51) + 
     -     (G(50) + G(51) - TWO*G(55))*G(59) - 
     -     TWO*G(11)*G(203) - TWO*G(11)*G(204) - 
     -     TWO*G(12)*G(205) - TWO*G(12)*G(206) - 
     -     G(11)*(G(12)*G(50) + G(12)*G(51) - 
     -        TWO*G(205) - TWO*G(206) - 
     -        TWO*(G(12)*G(55) - TWO*G(208))) - 
     -     G(12)*(-(HALF*G(1)*G(11)**TWO) - 
     -        HALF*G(2)*G(11)**TWO + 
     -        G(11)*G(48) + G(11)*G(49) - 
     -        G(203) - G(204) + TWO*G(212)) + 
     -     THREE*G(589) + THREE*G(590) + 
     -     THREE*G(591) + THREE*G(592) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) - 
     -     TWO*G(596)) - 
     -  16*(-(HALF*G(12)**TWO*G(55)) + 
     -     G(59)*(-G(55) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(58) + G(60)) - 
     -     G(12)*(HALF*
     -         (HALF*G(1)*G(11)**TWO - 
     -           G(11)*G(48) + G(203)) + 
     -        HALF*(HALF*G(2)*G(11)**TWO - 
     -           G(11)*G(49) + G(204))) + 
     -     TWO*G(12)*G(208) - 
     -     G(11)*(-(G(12)*G(55)) + TWO*G(208) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         (G(12)*G(58) - TWO*G(211)) + G(213)
     -        ) + HALF*
     -      (-(HALF*G(11)**TWO*G(48)) + 
     -        TWO*G(11)*G(203) - THREE*G(589)) + 
     -     HALF*(-(HALF*G(11)**TWO*G(49)) + 
     -        TWO*G(11)*G(204) - THREE*G(590)) - 
     -     THREE*G(593) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      (HALF*G(12)**TWO*G(56) - 
     -        TWO*G(12)*G(209) + THREE*G(594)) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (HALF*G(12)**TWO*G(58) - 
     -        TWO*G(12)*G(211) + THREE*G(595)) + 
     -     G(597)) + 
     -  16*(G(59)*(-(TWO*G(55)) + G(60)) - 
     -     G(12)*G(212) - 
     -     G(11)*(-(TWO*
     -           (G(12)*G(55) - TWO*G(208))) + 
     -        G(213)) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) - 
     -     G(596) + G(597))
      return
      end function fE27Euclid_w4

      function f1E3_w2_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 f1E3_w2_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR,ZERO
      parameter (ZERO=0E0_16,FOUR=4E0_16)
      f1E3_w2_qp=f1E3Euclid_w2_qp(w,z,G)
      if(xtb.GT.ZERO.and.xtb.LT.FOUR)then
         f1E3_w2_qp=-f1E3_w2_qp
      endif
      return
      end

      function f1E5_w3_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 f1E5_w3_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      f1E5_w3_qp=f1E5Euclid_w3_qp(w,z,G)
      return
      end

      function fE13_w4_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE13_w4_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE13_w4_qp=fE13Euclid_w4_qp(w,z,G)
      if(xsb.GT.FOUR)then
          fE13_w4_qp=-fE13_w4_qp
      endif
      return
      end

      function fE14_w3_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE14_w3_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE14_w3_qp=fE14Euclid_w3_qp(w,z,G)
      return
      end

      function fE15_w2_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE15_w2_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE15_w2_qp=fE15Euclid_w2_qp(w,z,G)
      if(xtb.GT.0E0_16.AND.xtb.LT.FOUR)then
          fE15_w2_qp=-fE15_w2_qp
      endif
      return
      end

      function fE17_w4_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE17_w4_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE17_w4_qp=fE17Euclid_w4_qp(w,z,G)
      return
      end

      function fE19_w2_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE19_w2_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE19_w2_qp=fE19Euclid_w2_qp(w,z,G)
      if(xsb.GT.0E0_16.or.xtb.GT.0E0_16)then
         fE19_w2_qp=-fE19_w2_qp
      endif
      return
      end

      function fE19_w3_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE19_w3_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE19_w3_qp=fE19Euclid_w3_qp(w,z,G)
      if(xsb.GT.0E0_16.or.xtb.GT.0E0_16)then
          fE19_w3_qp=-fE19_w3_qp
      endif
      return
      end

      function fE20_w3_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE20_w3_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE20_w3_qp=fE20Euclid_w3_qp(w,z,G)
      return
      end

      function fE21_w4_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE21_w4_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE21_w4_qp=fE21Euclid_w4_qp(w,z,G)
      return
      end

      function fE22_w3_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE22_w3_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE22_w3_qp=fE22Euclid_w3_qp(w,z,G)
      if(xsb.GT.0E0_16.or.xtb.GT.0E0_16)then
          fE22_w3_qp=-fE22_w3_qp
      endif
      return
      end

      function fE27_w4_qp(xsb,xtb,w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE27_w4_qp
      complex*32 G(NDIM)
      real*16 xsb,xtb
      complex*32 w,z
      real*16 FOUR
      parameter (FOUR=4E0_16)
      fE27_w4_qp=fE27Euclid_w4_qp(w,z,G)
      if(xsb.GT.0E0_16.or.xtb.GT.0E0_16)then
         fE27_w4_qp=-fE27_w4_qp
      endif
      if(xsb.GT.FOUR)then
         fE27_w4_qp=-fE27_w4_qp
      endif
      return
      end
      
      function f1E3Euclid_w2_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 f1E3Euclid_w2_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      f1E3Euclid_w2_qp=        zeta2 - TWO*G(9)*G(12) + G(11)*G(12) + 
     -  TWO*(G(9)*G(11) - G(57)) - G(59) - G(60)
      return
      end function f1E3Euclid_w2_qp

      function f1E5Euclid_w3_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 f1E5Euclid_w3_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      f1E5Euclid_w3_qp=        -(THREE*zeta3) - zeta2*G(11) + 
     -  zeta2*G(12) + TWO*G(12)*G(57) - 
     -  G(12)*G(59) + G(11)*G(60) - 
     -  TWO*(G(11)*G(57) - TWO*G(210)) + G(212) - 
     -  G(213)
      return
      end function f1E5Euclid_w3_qp

      function fE13Euclid_w4_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE13Euclid_w4_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE13Euclid_w4_qp=-8*((G(13) - G(14) + G(15) - G(16))*
     -      (G(17) - G(18) - G(24) + G(25)) + 
     -     (-G(3) + G(4))*
     -      (G(61) - G(62) + G(63) - G(64) - 
     -        G(67) + G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      (G(73) + G(74) - TWO*G(75) - G(77) - 
     -        G(78) + TWO*G(79) - G(96) - G(97) + 
     -        TWO*G(98) + G(100) + G(101) - 
     -        TWO*G(102)) + G(214) - G(215) + 
     -     G(216) - G(217) - G(220) + G(221) - 
     -     G(222) + G(223) - G(232) + G(233) - 
     -     G(234) + G(235) + G(238) - G(239) + 
     -     G(240) - G(241) - G(250) + G(251) - 
     -     G(252) + G(253) + TWO*G(254) - 
     -     TWO*G(255) + G(258) - G(259) + 
     -     G(260) - G(261) - TWO*G(262) + 
     -     TWO*G(263) + G(305) - G(306) + 
     -     G(307) - G(308) - TWO*G(309) + 
     -     TWO*G(310) - G(313) + G(314) - 
     -     G(315) + G(316) + TWO*G(317) - 
     -     TWO*G(318)) + 
     -  TWO*((G(17) - G(18) - G(24) + G(25))*
     -      (-G(13) + G(14) - G(15) + G(16) + 
     -        TWO*G(48) - TWO*G(49)) + 
     -     (-G(3) + G(4))*
     -      (-G(61) + G(62) - G(63) + G(64) + 
     -        TWO*G(65) - TWO*G(66) + G(67) - 
     -        G(68) + G(69) - G(70) - TWO*G(71) + 
     -        TWO*G(72)) + 
     -     (G(1) - G(2))*
     -      (-G(73) - G(74) + TWO*G(76) + G(77) + 
     -        G(78) - TWO*G(80) + G(96) + G(97) - 
     -        TWO*G(99) - G(100) - G(101) + 
     -        TWO*G(103)) - G(214) + G(215) - 
     -     G(216) + G(217) + TWO*G(218) - 
     -     TWO*G(219) + G(220) - G(221) + 
     -     G(222) - G(223) - TWO*G(224) + 
     -     TWO*G(225) + G(232) - G(233) + 
     -     G(234) - G(235) - TWO*G(236) + 
     -     TWO*G(237) - G(238) + G(239) - 
     -     G(240) + G(241) + TWO*G(242) - 
     -     TWO*G(243) + G(250) - G(251) + 
     -     G(252) - G(253) - TWO*G(256) + 
     -     TWO*G(257) - G(258) + G(259) - 
     -     G(260) + G(261) + TWO*G(264) - 
     -     TWO*G(265) - G(305) + G(306) - 
     -     G(307) + G(308) + TWO*G(311) - 
     -     TWO*G(312) + G(313) - G(314) + 
     -     G(315) - G(316) - TWO*G(319) + 
     -     TWO*G(320)) + 
     -  FOUR*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(17) - G(18) + TWO*G(19) + G(24) + 
     -        G(25) - TWO*G(26)) + 
     -     (-G(3) + G(4))*
     -      (G(61) - G(62) - G(63) + G(64) + 
     -        G(67) - G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      (G(73) - G(74) + G(77) - G(78) - 
     -        TWO*G(81) + TWO*G(82) - G(96) + 
     -        G(97) - G(100) + G(101) + 
     -        TWO*G(104) - TWO*G(105)) + G(214) - 
     -     G(215) - G(216) + G(217) + G(220) - 
     -     G(221) - G(222) + G(223) - G(232) + 
     -     G(233) + G(234) - G(235) - G(238) + 
     -     G(239) + G(240) - G(241) - G(250) + 
     -     G(251) + G(252) - G(253) - G(258) + 
     -     G(259) + G(260) - G(261) + 
     -     TWO*G(266) - TWO*G(267) - TWO*G(268) + 
     -     TWO*G(269) + G(305) - G(306) - 
     -     G(307) + G(308) + G(313) - G(314) - 
     -     G(315) + G(316) - TWO*G(321) + 
     -     TWO*G(322) + TWO*G(323) - TWO*G(324)) 
     -   - 7*((G(13) - G(14) - G(15) + G(16))*
     -      (G(17) + G(18) - TWO*G(21) - G(24) - 
     -        G(25) + TWO*G(28)) + 
     -     (G(1) - G(2))*
     -      (-G(73) + G(74) - G(77) + G(78) + 
     -        TWO*G(83) - TWO*G(84) + G(96) - 
     -        G(97) + G(100) - G(101) - 
     -        TWO*G(106) + TWO*G(107)) + 
     -     (-G(3) + G(4))*
     -      (-G(61) + G(62) + G(63) - G(64) - 
     -        G(67) + G(68) + G(69) - G(70) + 
     -        TWO*G(166) - TWO*G(167) - 
     -        TWO*G(168) + TWO*G(169)) - G(214) + 
     -     G(215) + G(216) - G(217) - G(220) + 
     -     G(221) + G(222) - G(223) + 
     -     TWO*G(226) - TWO*G(227) - TWO*G(228) + 
     -     TWO*G(229) + G(232) - G(233) - 
     -     G(234) + G(235) + G(238) - G(239) - 
     -     G(240) + G(241) - TWO*G(244) + 
     -     TWO*G(245) + TWO*G(246) - TWO*G(247) + 
     -     G(250) - G(251) - G(252) + G(253) + 
     -     G(258) - G(259) - G(260) + G(261) - 
     -     TWO*G(270) + TWO*G(271) + TWO*G(272) - 
     -     TWO*G(273) - G(305) + G(306) + 
     -     G(307) - G(308) - G(313) + G(314) + 
     -     G(315) - G(316) + TWO*G(325) - 
     -     TWO*G(326) - TWO*G(327) + TWO*G(328))
      return
      end function fE13Euclid_w4_qp

      function fE14Euclid_w3_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE14Euclid_w3_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE14Euclid_w3_qp=-6*zeta3 - TWO*zeta2*(-G(11) + G(12)) + 
     -  TWO*((TWO*G(7) - G(12))*G(59) - 
     -     G(11)*(TWO*(G(7)*G(12) - G(55)) - 
     -        G(60)) + 
     -     TWO*(HALF*G(7)*G(12)**TWO - 
     -        G(12)*G(55) + G(208)) + G(212) - 
     -     G(213)) + 
     -  TWO*(FOUR*G(12)*G(59) - 
     -     G(11)*(-6*G(54) - TWO*G(55) + 
     -        FOUR*G(60)) - 
     -     6*(G(12)*G(54) - TWO*G(207)) - 
     -     TWO*(G(12)*G(55) - TWO*G(208)) - 
     -     FOUR*G(212) + FOUR*G(213))
      return
      end function fE14Euclid_w3_qp

      function fE15Euclid_w2_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE15Euclid_w2_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE15Euclid_w2_qp=        -(TWO*zeta2) + 
     -  TWO*(-(G(11)*
     -        (-6*G(6) - TWO*G(7) + FOUR*G(12))) 
     -      - 6*(G(6)*G(12) - G(54)) - 
     -     TWO*(G(7)*G(12) - G(55)) + 
     -     FOUR*G(59) + FOUR*G(60))
      return
      end function fE15Euclid_w2_qp

      function fE17Euclid_w4_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE17Euclid_w4_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE17Euclid_w4_qp=        -6*zeta4 + FOUR*zeta3*
     -   (TWO*G(7) + G(11) - G(12)) + 
     -  TWO*(G(59)*(FOUR*G(35) - 
     -        TWO*(G(7)*G(12) - G(55)) - 
     -        TWO*G(55) + G(60)) + 
     -     (TWO*G(7) - G(12))*G(212) - 
     -     G(11)*(FOUR*
     -         (G(12)*G(35) - G(133) - G(190)) - 
     -        TWO*(HALF*G(7)*G(12)**TWO - 
     -           G(12)*G(55) + G(208)) - 
     -        TWO*(G(12)*G(55) - TWO*G(208)) + 
     -        G(213)) + 
     -     FOUR*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) - 
     -     TWO*((G(7)*G(12)**THREE)/6.0E0_16 - 
     -        HALF*G(12)**TWO*G(55) + 
     -        G(12)*G(208) - G(593)) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) + 
     -     G(596) + G(597))
      return
      end function fE17Euclid_w4_qp

      function fE19Euclid_w2_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE19Euclid_w2_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE19Euclid_w2_qp=        (8*(-((-(HALF*G(3)) + HALF*G(4))*G(11)) + 
     -       HALF*(G(1)*G(11) - G(48)) + 
     -       HALF*(-(G(2)*G(11)) + G(49)) + 
     -       HALF*(-(G(3)*G(12)) + G(50)) + 
     -       HALF*(G(4)*G(12) - G(51))) + 
     -    FOUR*(-((G(1) - G(2))*G(12)) - G(48) + 
     -       G(49) + G(50) - G(51)))/4.0E0_16
      return
      end function fE19Euclid_w2_qp

      function fE19Euclid_w3_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE19Euclid_w3_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE19Euclid_w3_qp=        -(TWO*zeta2*(-(HALF*G(1)) + HALF*G(2) - 
     -       HALF*G(3) + HALF*G(4))) - 
     -  G(12)*(G(13) - G(14) + G(15) - G(16)) + 
     -  (-6*G(5) - TWO*G(7) + 
     -     ((TWO + TWO*w)*G(8))/(1 + w) + 
     -     ((-TWO + TWO*w)*G(10))/(-1 + w) + 
     -     TWO*G(12))*(-G(48) + G(49)) + 
     -  G(12)*(-G(13) + G(14) - G(15) + G(16) + 
     -     TWO*G(48) - TWO*G(49)) + 
     -  (G(1) - G(2))*
     -   (-G(50) - G(51) + TWO*G(53)) - 
     -  (G(1) - G(2))*
     -   (G(50) + G(51) - TWO*G(55)) + 
     -  (G(1) - G(2))*
     -   (6*(G(5)*G(12) - G(52)) + 
     -     TWO*(G(7)*G(12) - G(55)) + 
     -     ((-TWO - TWO*w)*(G(8)*G(12) - G(56)))/
     -      (1 + w) + 
     -     ((TWO - TWO*w)*(G(10)*G(12) - G(58)))/
     -      (-1 + w) - TWO*G(60)) - TWO*G(65) + 
     -  TWO*G(66) - TWO*G(71) + TWO*G(72) - 
     -  6*G(119) + 6*G(120) - TWO*G(131) + 
     -  TWO*G(132) + 
     -  ((TWO + TWO*w)*G(146))/(1 + w) + 
     -  ((-TWO - TWO*w)*G(147))/(1 + w) - 
     -  TWO*((G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      (G(13) - G(14) - G(15) + G(16)) + 
     -     (G(1) - G(2))*
     -      (((-1 - w)*G(38))/(1 + w) + G(39) + 
     -        G(43) + ((1 - w)*G(44))/(-1 + w)) - 
     -     G(61) + G(62) + G(63) - G(64) + 
     -     G(67) - G(68) - G(69) + G(70) + 
     -     G(136) + ((-1 - w)*G(137))/(1 + w) + 
     -     ((-1 - w)*G(138))/(1 + w) + G(139) + 
     -     ((1 - w)*G(151))/(-1 + w) + G(152) + 
     -     G(153) + ((1 - w)*G(154))/(-1 + w)) + 
     -  ((-TWO + TWO*w)*G(161))/(-1 + w) + 
     -  ((TWO - TWO*w)*G(162))/(-1 + w) - 
     -  TWO*G(166) + TWO*G(167) - TWO*G(168) + 
     -  TWO*G(169) + TWO*G(170) - TWO*G(171) + 
     -  TWO*G(177) - TWO*G(178) - TWO*G(186) + 
     -  TWO*G(187) - TWO*G(188) + TWO*G(189) + 
     -  TWO*G(203) - TWO*G(204) + TWO*G(205) - 
     -  TWO*G(206) + 
     -  TWO*(G(11)*G(13) - G(11)*G(14) + 
     -     G(11)*G(15) - G(11)*G(16) + 
     -     G(12)*G(33) - G(12)*G(34) + 
     -     G(12)*G(39) + G(12)*G(44) + 
     -     (-6*G(5) - TWO*G(7) + 
     -        ((TWO + TWO*w)*G(8))/(1 + w) + 
     -        ((-TWO + TWO*w)*G(10))/(-1 + w) + 
     -        TWO*G(12))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) - 
     -     G(12)*G(50) + G(12)*G(51) - 
     -     G(11)*(THREE*G(31) - THREE*G(32) + 
     -        G(33) - G(34) + 
     -        ((-1 - w)*G(38))/(1 + w) + G(39) + 
     -        ((1 - w)*G(43))/(-1 + w) + G(44) - 
     -        G(50) + G(51)) - G(65) + G(66) - 
     -     G(71) + G(72) - G(131) + G(132) - 
     -     G(147) - G(162) - G(166) + G(167) - 
     -     G(168) + G(169) + 
     -     THREE*(G(12)*G(31) - G(119) - 
     -        G(184)) - 
     -     THREE*(G(12)*G(32) - G(120) - 
     -        G(185)) - G(188) + G(189) + 
     -     ((-1 - w)*
     -        (G(12)*G(38) - G(146) - G(193)))/
     -      (1 + w) - G(194) + 
     -     ((1 - w)*(G(12)*G(43) - G(161) - 
     -          G(198)))/(-1 + w) - G(199) + 
     -     TWO*G(205) - TWO*G(206)) + 
     -  TWO*(G(12)*G(21) - G(12)*G(28) - 
     -     G(11)*(THREE*G(20) + G(21) - 
     -        THREE*G(27) - G(28) - 
     -        TWO*(G(3)*G(12) - G(50)) + 
     -        TWO*(G(4)*G(12) - G(51))) + 
     -     FOUR*(-(HALF*G(3)) + HALF*G(4))*
     -      G(59) - G(93) + G(116) + 
     -     THREE*(G(12)*G(20) - G(92) - G(173)) - 
     -     G(174) - THREE*
     -      (G(12)*G(27) - G(115) - G(180)) + 
     -     G(181) - TWO*
     -      (HALF*G(1)*G(11)**TWO - G(11)*G(48) + 
     -        G(203)) + 
     -     TWO*(HALF*G(2)*G(11)**TWO - 
     -        G(11)*G(49) + G(204)) - 
     -     TWO*(HALF*G(3)*G(12)**TWO - 
     -        G(12)*G(50) + G(205)) + 
     -     TWO*(HALF*G(4)*G(12)**TWO - 
     -        G(12)*G(51) + G(206))) - 
     -  TWO*(-(HALF*G(1)*G(11)**TWO) + 
     -     HALF*G(2)*G(11)**TWO + 
     -     HALF*G(8)*G(12)**TWO + G(11)*G(48) - 
     -     G(11)*G(49) - G(12)*G(56) - 
     -     G(11)*(G(8)*G(12) - G(56) + 
     -        ((1 - w)*(G(10)*G(12) - G(58)))/
     -         (-1 + w)) + 
     -     (G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      G(59) - G(203) + G(204) + G(209) + 
     -     ((1 - w)*(HALF*G(10)*G(12)**TWO - 
     -          G(12)*G(58) + G(211)))/(-1 + w))
      return
      end function fE19Euclid_w3_qp

      function fE20Euclid_w3_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE20Euclid_w3_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE20Euclid_w3_qp=        HALF*G(1)*G(11)**TWO + 
     -  HALF*G(2)*G(11)**TWO + 
     -  HALF*G(3)*G(12)**TWO + 
     -  HALF*G(4)*G(12)**TWO + 
     -  (TWO*G(7) - G(12))*
     -   (G(13) - G(14) - G(15) + G(16)) - 
     -  G(11)*G(48) - G(11)*G(49) + 
     -  (G(8) + ((1 - w)*G(10))/(-1 + w))*
     -   (-G(48) + G(49)) - G(12)*G(50) + 
     -  (G(1) - G(2))*
     -   (-(TWO*G(33)) + TWO*G(34) + G(50) - 
     -     G(51)) - G(12)*G(51) + 
     -  G(11)*(-(G(3)*G(12)) - G(4)*G(12) + 
     -     G(50) + G(51) + 
     -     TWO*(G(7)*G(12) - G(55))) + 
     -  (G(1) - G(2))*
     -   (G(10)*G(12) + 
     -     ((-1 - w)*(G(8)*G(12) - G(56)))/
     -      (1 + w) - G(58)) - 
     -  (-G(3) - G(4) + TWO*G(7))*G(59) + G(65) - 
     -  G(66) - G(71) + G(72) + 
     -  HALF*(-((G(1) - G(2))*
     -        (G(17) + G(18) - TWO*G(21) - 
     -          G(24) - G(25) + TWO*G(28))) - 
     -     (-G(3) + G(4))*
     -      (-G(13) + G(14) - G(15) + G(16) + 
     -        TWO*G(48) - TWO*G(49)) + G(61) - 
     -     G(62) + G(63) - G(64) - TWO*G(65) + 
     -     TWO*G(66) - G(67) + G(68) - G(69) + 
     -     G(70) + TWO*G(71) - TWO*G(72) + 
     -     G(73) - G(74) + G(77) - G(78) - 
     -     TWO*G(83) + TWO*G(84) - G(96) + 
     -     G(97) - G(100) + G(101) + TWO*G(106) - 
     -     TWO*G(107)) + TWO*G(121) - 
     -  TWO*G(122) - TWO*G(123) + TWO*G(124) + 
     -  G(146) + ((-1 - w)*G(147))/(1 + w) + 
     -  ((1 - w)*G(161))/(-1 + w) + G(162) + 
     -  G(166) - G(167) - G(168) + G(169) + 
     -  HALF*(-((-G(3) - G(4) + TWO*G(7))*
     -        (G(13) - G(14) - G(15) + G(16))) - 
     -     (G(1) - G(2))*
     -      (G(17) - G(18) + G(24) - G(25) - 
     -        TWO*G(33) + TWO*G(34)) + G(61) - 
     -     G(62) - G(63) + G(64) + G(67) - 
     -     G(68) - G(69) + G(70) + G(73) - 
     -     G(74) - G(77) + G(78) + G(96) - 
     -     G(97) - G(100) + G(101) - TWO*G(121) + 
     -     TWO*G(122) + TWO*G(123) - TWO*G(124) - 
     -     TWO*G(166) + TWO*G(167) + TWO*G(168) - 
     -     TWO*G(169)) - G(170) + G(171) + 
     -  G(177) - TWO*
     -   ((G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (G(13) - G(14) - G(15) + G(16)) + 
     -     (G(1) - G(2))*
     -      (-G(33) + G(34) + 
     -        ((-HALF - HALF*w)*G(38))/(1 + w) + 
     -        ((HALF + HALF*w)*G(39))/(1 + w) + 
     -        ((HALF - HALF*w)*G(43))/(-1 + w) + 
     -        ((-HALF + HALF*w)*G(44))/(-1 + w) + 
     -        G(50) - G(51)) + HALF*G(61) - 
     -     HALF*G(62) - HALF*G(63) + HALF*G(64) + 
     -     HALF*G(67) - HALF*G(68) - HALF*G(69) + 
     -     HALF*G(70) + G(121) - G(122) - 
     -     G(123) + G(124) + 
     -     ((HALF + HALF*w)*G(136))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(137))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(138))/(1 + w) + 
     -     ((HALF + HALF*w)*G(139))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(151))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(152))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(153))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(154))/(-1 + w) - 
     -     G(170) + G(171) + G(177) - G(178)) - 
     -  G(178) + TWO*
     -   (-(G(11)*(((-HALF - HALF*w)*G(38))/
     -           (1 + w) + 
     -          ((HALF + HALF*w)*G(39))/(1 + w) + 
     -          ((-HALF + HALF*w)*G(43))/
     -           (-1 + w) + 
     -          ((HALF - HALF*w)*G(44))/(-1 + w)))
     -       + (G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) + 
     -     HALF*(-(G(11)*G(13)) + G(65) + 
     -        G(166)) + 
     -     HALF*(G(11)*G(14) - G(66) - G(167)) + 
     -     HALF*(G(11)*G(15) - G(71) - G(168)) + 
     -     HALF*(-(G(11)*G(16)) + G(72) + 
     -        G(169)) + 
     -     ((-HALF - HALF*w)*
     -        (G(12)*G(38) - G(146) - G(193)))/
     -      (1 + w) + 
     -     ((HALF + HALF*w)*
     -        (G(12)*G(39) - G(147) - G(194)))/
     -      (1 + w) + 
     -     ((-HALF + HALF*w)*
     -        (G(12)*G(43) - G(161) - G(198)))/
     -      (-1 + w) + 
     -     ((HALF - HALF*w)*
     -        (G(12)*G(44) - G(162) - G(199)))/
     -      (-1 + w)) + G(203) + G(204) + 
     -  G(205) + G(206) - 
     -  TWO*(HALF*G(7)*G(12)**TWO - G(12)*G(55) + 
     -     G(208)) - TWO*G(212) - 
     -  TWO*(HALF*G(7)*G(12)**TWO - G(12)*G(55) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*G(59) - 
     -     G(11)*(G(7)*G(12) - G(55) + 
     -        ((HALF + HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((-HALF + HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         - G(60)) + 
     -     HALF*(HALF*G(1)*G(11)**TWO - 
     -        G(11)*G(48) + G(203)) + 
     -     HALF*(HALF*G(2)*G(11)**TWO - 
     -        G(11)*G(49) + G(204)) + G(208) + 
     -     ((HALF + HALF*w)*
     -        (HALF*G(8)*G(12)**TWO - 
     -          G(12)*G(56) + G(209)))/(1 + w) + 
     -     ((-HALF + HALF*w)*
     -        (HALF*G(10)*G(12)**TWO - 
     -          G(12)*G(58) + G(211)))/(-1 + w) - 
     -     G(213)) + 
     -  TWO*((TWO*G(7) - G(12))*G(59) - 
     -     G(11)*(TWO*(G(7)*G(12) - G(55)) - 
     -        G(60)) + 
     -     TWO*(HALF*G(7)*G(12)**TWO - 
     -        G(12)*G(55) + G(208)) + G(212) - 
     -     G(213))
      return
      end function fE20Euclid_w3_qp

      function fE21Euclid_w4_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE21Euclid_w4_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE21Euclid_w4_qp=        -6*zeta4 - FOUR*zeta3*
     -   (-G(1) - G(2) - G(3) - G(4) + TWO*G(7) + 
     -     TWO*G(11)) - 
     -  8*zeta3*(HALF*G(1) + HALF*G(2) + G(7) + 
     -     ((HALF + HALF*w)*G(8))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -     G(12)) + 12*zeta3*
     -   (TWO*G(7) + G(11) - G(12)) - 
     -  FOUR*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(33) - G(34) + TWO*G(35) + 
     -        ((-HALF - HALF*w)*G(38))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(39))/(1 + w) + 
     -        G(40) + 
     -        ((HALF - HALF*w)*G(43))/(-1 + w) + 
     -        ((HALF - HALF*w)*G(44))/(-1 + w) + 
     -        G(45) + G(50) + G(51) - TWO*G(55)) 
     -      + (G(7) + 
     -        ((HALF + HALF*w)*G(8))/(1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (-G(61) + G(62) + G(63) - G(64) - 
     -        G(67) + G(68) + G(69) - G(70) + 
     -        TWO*G(166) - TWO*G(167) - 
     -        TWO*G(168) + TWO*G(169)) + 
     -     (G(1) - G(2))*
     -      (G(121) - G(122) + G(123) - G(124) - 
     -        TWO*G(125) + TWO*G(126) + 
     -        ((HALF + HALF*w)*G(136))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(137))/(1 + w) + 
     -        ((HALF + HALF*w)*G(138))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(139))/(1 + w) + 
     -        ((-1 - w)*G(140))/(1 + w) + 
     -        G(141) + 
     -        ((-HALF + HALF*w)*G(151))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*G(152))/(-1 + w) + 
     -        ((-HALF + HALF*w)*G(153))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*G(154))/(-1 + w) + 
     -        ((1 - w)*G(155))/(-1 + w) + 
     -        G(156) - G(170) + G(171) - G(177) + 
     -        G(178) + TWO*G(188) - TWO*G(189)) - 
     -     HALF*G(214) + HALF*G(215) + 
     -     HALF*G(216) - HALF*G(217) - 
     -     HALF*G(220) + HALF*G(221) + 
     -     HALF*G(222) - HALF*G(223) + G(226) - 
     -     G(227) - G(228) + G(229) - 
     -     HALF*G(232) + HALF*G(233) + 
     -     HALF*G(234) - HALF*G(235) - 
     -     HALF*G(238) + HALF*G(239) + 
     -     HALF*G(240) - HALF*G(241) + G(244) - 
     -     G(245) - G(246) + G(247) - G(360) + 
     -     G(361) + G(362) - G(363) - G(364) + 
     -     G(365) + G(366) - G(367) + 
     -     TWO*G(368) - TWO*G(369) - TWO*G(370) + 
     -     TWO*G(371) + 
     -     ((-HALF - HALF*w)*G(397))/(1 + w) + 
     -     ((HALF + HALF*w)*G(398))/(1 + w) + 
     -     ((HALF + HALF*w)*G(399))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(400))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(401))/(1 + w) + 
     -     ((HALF + HALF*w)*G(402))/(1 + w) + 
     -     ((HALF + HALF*w)*G(403))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(404))/(1 + w) + 
     -     G(405) + ((-1 - w)*G(406))/(1 + w) + 
     -     ((-1 - w)*G(407))/(1 + w) + G(408) + 
     -     ((HALF - HALF*w)*G(434))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(435))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(436))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(437))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(438))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(439))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(440))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(441))/(-1 + w) + 
     -     G(442) + ((1 - w)*G(443))/(-1 + w) + 
     -     ((1 - w)*G(444))/(-1 + w) + G(445) + 
     -     G(483) - G(484) - G(485) + G(486) + 
     -     G(500) - G(501) - G(502) + G(503) - 
     -     TWO*G(521) + TWO*G(522) + TWO*G(523) - 
     -     TWO*G(524)) - 
     -  FOUR*((-G(48) + G(49))*
     -      (G(36) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(37) + 
     -        ((HALF + HALF*w)*G(41))/(1 + w) + 
     -        ((HALF/(-1 + w) - 
     -             (HALF*w**TWO)/(-1 + w))*G(42))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(46))/(-1 + w) + 
     -        ((-(HALF/(-1 + w)) + w/(-1 + w) - 
     -             (HALF*w**TWO)/(-1 + w))*G(47))/
     -         (-1 + w) - G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (G(65) - G(66) - G(71) + G(72)) + 
     -     (G(1) - G(2))*
     -      (G(12)*G(37) - G(12)*G(58) - G(135) + 
     -        (1/(-1 - w) - w/(1 + w))*
     -         (G(12)*G(36) - G(134) - G(191)) - 
     -        G(192) + 
     -        ((-(HALF/(1 + w)) - w/(1 + w) - 
     -             (HALF*w**TWO)/(1 + w))*
     -           (G(12)*G(41) - G(149) - G(196)))/
     -         (1 + w) + 
     -        ((HALF + HALF*w)*
     -           (G(12)*G(42) - G(150) - G(197)))/
     -         (1 + w) + 
     -        ((HALF/(1 + w) - 
     -             (HALF*w**TWO)/(1 + w))*
     -           (G(12)*G(46) - G(164) - G(201)))/
     -         (-1 + w) + 
     -        ((-HALF + HALF*w)*
     -           (G(12)*G(47) - G(165) - G(202)))/
     -         (-1 + w) + 
     -        (1/(1 + w) + w/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        TWO*G(211)) + HALF*G(218) - 
     -     HALF*G(219) - HALF*G(224) + 
     -     HALF*G(225) + HALF*G(236) - 
     -     HALF*G(237) - HALF*G(242) + 
     -     HALF*G(243) + G(376) + 
     -     (1/(-1 - w) - w/(1 + w))*G(377) + 
     -     (1/(-1 + w) - w/(-1 + w))*G(382) + 
     -     G(383) + ((HALF + HALF*w)*G(413))/
     -      (1 + w) + 
     -     ((-(HALF/(1 + w)) - w/(1 + w) - 
     -          (HALF*w**TWO)/(1 + w))*G(414))/
     -      (1 + w) + 
     -     ((HALF/(-1 + w) - 
     -          (HALF*w**TWO)/(-1 + w))*G(419))/
     -      (1 + w) + 
     -     ((HALF + HALF*w)*G(420))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(450))/(-1 + w) + 
     -     ((HALF/(1 + w) - 
     -          (HALF*w**TWO)/(1 + w))*G(451))/
     -      (-1 + w) + 
     -     ((-(HALF/(-1 + w)) + w/(-1 + w) - 
     -          (HALF*w**TWO)/(-1 + w))*G(456))/
     -      (-1 + w) + 
     -     ((-HALF + HALF*w)*G(457))/(-1 + w) - 
     -     G(542) + (1/(1 + w) + w/(1 + w))*
     -      G(543) + 
     -     (1/(1 - w) + w/(-1 + w))*G(555) - 
     -     G(556)) + 
     -  FOUR*((-G(48) + G(49))*
     -      (TWO*G(36) + 
     -        (TWO/(-1 + w) - (TWO*w)/(-1 + w))*
     -         G(37) - G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) + 
     -     (TWO*G(7) - G(12))*
     -      (G(65) - G(66) - G(71) + G(72)) + 
     -     (G(1) - G(2))*
     -      (-(G(12)*G(58)) + 
     -        (-(TWO/(1 + w)) - (TWO*w)/(1 + w))*
     -         (G(12)*G(36) - G(134) - G(191)) + 
     -        TWO*(G(12)*G(37) - G(135) - 
     -           G(192)) + 
     -        (1/(1 + w) + w/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        TWO*G(211)) + TWO*G(376) + 
     -     (-(TWO/(1 + w)) - (TWO*w)/(1 + w))*
     -      G(377) + 
     -     (TWO/(-1 + w) - (TWO*w)/(-1 + w))*
     -      G(382) + TWO*G(383) + G(475) - 
     -     G(476) - G(481) + G(482) - G(542) + 
     -     (1/(1 + w) + w/(1 + w))*G(543) + 
     -     (1/(1 - w) + w/(-1 + w))*G(555) - 
     -     G(556)) + 
     -  TWO*((G(13) - G(14) - G(15) + G(16))*
     -      (G(17) + G(18) - TWO*G(21) + G(24) + 
     -        G(25) - TWO*G(28) - TWO*G(33) - 
     -        TWO*G(34) + FOUR*G(35)) + 
     -     (G(1) - G(2))*
     -      (-G(73) + G(74) - G(77) + G(78) + 
     -        TWO*G(83) - TWO*G(84) - G(96) + 
     -        G(97) - G(100) + G(101) + 
     -        TWO*G(106) - TWO*G(107) + 
     -        TWO*G(121) - TWO*G(122) + 
     -        TWO*G(123) - TWO*G(124) - 
     -        FOUR*G(125) + FOUR*G(126)) + 
     -     (-G(3) - G(4) + TWO*G(7))*
     -      (-G(61) + G(62) + G(63) - G(64) - 
     -        G(67) + G(68) + G(69) - G(70) + 
     -        TWO*G(166) - TWO*G(167) - 
     -        TWO*G(168) + TWO*G(169)) + G(214) - 
     -     G(215) - G(216) + G(217) + G(220) - 
     -     G(221) - G(222) + G(223) - 
     -     TWO*G(226) + TWO*G(227) + TWO*G(228) - 
     -     TWO*G(229) + G(232) - G(233) - 
     -     G(234) + G(235) + G(238) - G(239) - 
     -     G(240) + G(241) - TWO*G(244) + 
     -     TWO*G(245) + TWO*G(246) - TWO*G(247) + 
     -     G(250) - G(251) - G(252) + G(253) + 
     -     G(258) - G(259) - G(260) + G(261) - 
     -     TWO*G(270) + TWO*G(271) + TWO*G(272) - 
     -     TWO*G(273) + G(305) - G(306) - 
     -     G(307) + G(308) + G(313) - G(314) - 
     -     G(315) + G(316) - TWO*G(325) + 
     -     TWO*G(326) + TWO*G(327) - TWO*G(328) - 
     -     TWO*G(360) + TWO*G(361) + TWO*G(362) - 
     -     TWO*G(363) - TWO*G(364) + TWO*G(365) + 
     -     TWO*G(366) - TWO*G(367) + 
     -     FOUR*G(368) - FOUR*G(369) - 
     -     FOUR*G(370) + FOUR*G(371) - 
     -     TWO*G(471) + TWO*G(472) + TWO*G(473) - 
     -     TWO*G(474) - TWO*G(477) + TWO*G(478) + 
     -     TWO*G(479) - TWO*G(480) + 
     -     FOUR*G(560) - FOUR*G(561) - 
     -     FOUR*G(562) + FOUR*G(563)) + 
     -  TWO*((G(13) - G(14) - G(15) + G(16))*
     -      (-(TWO*G(33)) - TWO*G(34) + 
     -        FOUR*G(35) + G(50) + G(51) - 
     -        TWO*G(55)) + 
     -     (TWO*G(7) - G(12))*
     -      (-G(61) + G(62) + G(63) - G(64) - 
     -        G(67) + G(68) + G(69) - G(70) + 
     -        TWO*G(166) - TWO*G(167) - 
     -        TWO*G(168) + TWO*G(169)) + 
     -     (G(1) - G(2))*
     -      (TWO*G(121) - TWO*G(122) + 
     -        TWO*G(123) - TWO*G(124) - 
     -        FOUR*G(125) + FOUR*G(126) - 
     -        G(170) + G(171) - G(177) + G(178) + 
     -        TWO*G(188) - TWO*G(189)) - 
     -     TWO*G(360) + TWO*G(361) + TWO*G(362) - 
     -     TWO*G(363) - TWO*G(364) + TWO*G(365) + 
     -     TWO*G(366) - TWO*G(367) + 
     -     FOUR*G(368) - FOUR*G(369) - 
     -     FOUR*G(370) + FOUR*G(371) - G(471) + 
     -     G(472) + G(473) - G(474) - G(477) + 
     -     G(478) + G(479) - G(480) + G(483) - 
     -     G(484) - G(485) + G(486) + G(500) - 
     -     G(501) - G(502) + G(503) - 
     -     TWO*G(521) + TWO*G(522) + TWO*G(523) - 
     -     TWO*G(524) + TWO*G(560) - TWO*G(561) - 
     -     TWO*G(562) + TWO*G(563)) - 
     -  FOUR*((G(13) - G(14) - G(15) + G(16))*
     -      (-(G(7)*G(12)) + TWO*G(35) + G(40) + 
     -        G(45) - G(55) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + G(60)) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (G(166) - G(167) - G(168) + G(169)) + 
     -     (G(1) - G(2))*
     -      (-(TWO*G(125)) + TWO*G(126) + 
     -        G(131) - G(132) + 
     -        ((-1 - w)*G(140))/(1 + w) + 
     -        G(141) + 
     -        ((HALF + HALF*w)*G(146))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(147))/(1 + w) + 
     -        ((1 - w)*G(155))/(-1 + w) + 
     -        G(156) + 
     -        ((-HALF + HALF*w)*G(161))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*G(162))/(-1 + w) + 
     -        TWO*G(188) - TWO*G(189) - G(205) + 
     -        G(206)) + HALF*G(226) - 
     -     HALF*G(227) - HALF*G(228) + 
     -     HALF*G(229) + HALF*G(244) - 
     -     HALF*G(245) - HALF*G(246) + 
     -     HALF*G(247) + TWO*G(368) - 
     -     TWO*G(369) - TWO*G(370) + TWO*G(371) - 
     -     G(384) + G(385) + G(386) - G(387) + 
     -     G(405) + ((-1 - w)*G(406))/(1 + w) + 
     -     ((-1 - w)*G(407))/(1 + w) + G(408) + 
     -     ((-HALF - HALF*w)*G(421))/(1 + w) + 
     -     ((HALF + HALF*w)*G(422))/(1 + w) + 
     -     ((HALF + HALF*w)*G(423))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(424))/(1 + w) + 
     -     G(442) + ((1 - w)*G(443))/(-1 + w) + 
     -     ((1 - w)*G(444))/(-1 + w) + G(445) + 
     -     ((HALF - HALF*w)*G(458))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(459))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(460))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(461))/(-1 + w) - 
     -     TWO*G(521) + TWO*G(522) + TWO*G(523) - 
     -     TWO*G(524) + G(564) - G(565) - 
     -     G(569) + G(570)) + 
     -  8*((G(13) - G(14) - G(15) + G(16))*
     -      (-(G(7)*G(12)) + G(35) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(36) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(37) + 
     -        ((HALF + HALF*w)*G(40))/(1 + w) + 
     -        ((1/(4.0E0_16*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(41))/
     -         (1 + w) + 
     -        ((-1/(4.0E0_16*(-1 + w)) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(42))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(45))/(-1 + w) + 
     -        ((-1/(4.0E0_16*(1 + w)) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(46))/
     -         (-1 + w) + 
     -        ((1/(4.0E0_16*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(47))/
     -         (-1 + w) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + (HALF/(-1 + w) - 
     -           (HALF*w)/(-1 + w))*G(58) + G(60))
     -       + (G(7) + 
     -        ((HALF + HALF*w)*G(8))/(1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (HALF*G(61) - HALF*G(62) - 
     -        HALF*G(63) + HALF*G(64) + 
     -        HALF*G(67) - HALF*G(68) - 
     -        HALF*G(69) + HALF*G(70)) + 
     -     (G(1) - G(2))*
     -      (-G(125) + G(126) + 
     -        (HALF/(-1 - w) - (HALF*w)/(1 + w))*
     -         G(127) + 
     -        (-(HALF/(-1 - w)) + 
     -           (HALF*w)/(1 + w))*G(128) + 
     -        (-(HALF/(1 - w)) - 
     -           (HALF*w)/(-1 + w))*G(129) + 
     -        (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -         G(130) + G(131) - G(132) + 
     -        ((-HALF - HALF*w)*G(140))/(1 + w) + 
     -        ((HALF + HALF*w)*G(141))/(1 + w) + 
     -        ((-1/(4.0E0_16*(1 + w)) - 
     -             (HALF*w)/(1 + w) - 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(142))/
     -         (1 + w) + 
     -        ((1/(4.0E0_16*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(143))/
     -         (1 + w) + 
     -        ((1/(4.0E0_16*(-1 + w)) - 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(144))/
     -         (1 + w) + 
     -        ((-1/(4.0E0_16*(-1 + w)) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(145))/
     -         (1 + w) + 
     -        ((HALF + HALF*w)*G(146))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(147))/(1 + w) + 
     -        ((HALF - HALF*w)*G(155))/(-1 + w) + 
     -        ((-HALF + HALF*w)*G(156))/
     -         (-1 + w) + 
     -        ((1/(4.0E0_16*(1 + w)) - 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(157))/
     -         (-1 + w) + 
     -        ((-1/(4.0E0_16*(1 + w)) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(158))/
     -         (-1 + w) + 
     -        ((-1/(4.0E0_16*(-1 + w)) + 
     -             (HALF*w)/(-1 + w) - 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(159))/
     -         (-1 + w) + 
     -        ((1/(4.0E0_16*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(160))/
     -         (-1 + w) + 
     -        ((-HALF + HALF*w)*G(161))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*G(162))/(-1 + w) + 
     -        G(188) - G(189) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(198) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(199) - G(205) + G(206)) + 
     -     G(214)/4.0E0_16 - G(215)/4.0E0_16 - G(216)/4.0E0_16 + 
     -     G(217)/4.0E0_16 + G(220)/4.0E0_16 - G(221)/4.0E0_16 - 
     -     G(222)/4.0E0_16 + G(223)/4.0E0_16 + G(232)/4.0E0_16 - 
     -     G(233)/4.0E0_16 - G(234)/4.0E0_16 + G(235)/4.0E0_16 + 
     -     G(238)/4.0E0_16 - G(239)/4.0E0_16 - G(240)/4.0E0_16 + 
     -     G(241)/4.0E0_16 + G(368) - G(369) - G(370) + 
     -     G(371) + (-(HALF/(-1 - w)) + 
     -        (HALF*w)/(1 + w))*G(372) + 
     -     (HALF/(-1 - w) - (HALF*w)/(1 + w))*
     -      G(373) + 
     -     (HALF/(-1 - w) - (HALF*w)/(1 + w))*
     -      G(374) + 
     -     (-(HALF/(-1 - w)) + (HALF*w)/(1 + w))*
     -      G(375) + 
     -     (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -      G(378) + 
     -     (-(HALF/(1 - w)) - (HALF*w)/(-1 + w))*
     -      G(379) + 
     -     (-(HALF/(1 - w)) - (HALF*w)/(-1 + w))*
     -      G(380) + 
     -     (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -      G(381) - G(384) + G(385) + G(386) - 
     -     G(387) + ((HALF + HALF*w)*G(405))/
     -      (1 + w) + 
     -     ((-HALF - HALF*w)*G(406))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(407))/(1 + w) + 
     -     ((HALF + HALF*w)*G(408))/(1 + w) + 
     -     ((1/(4.0E0_16*(1 + w)) + (HALF*w)/(1 + w) + 
     -          w**TWO/(4.0E0_16*(1 + w)))*G(409))/
     -      (1 + w) + 
     -     ((-1/(4.0E0_16*(1 + w)) - (HALF*w)/(1 + w) - 
     -          w**TWO/(4.0E0_16*(1 + w)))*G(410))/
     -      (1 + w) + 
     -     ((-1/(4.0E0_16*(1 + w)) - (HALF*w)/(1 + w) - 
     -          w**TWO/(4.0E0_16*(1 + w)))*G(411))/
     -      (1 + w) + 
     -     ((1/(4.0E0_16*(1 + w)) + (HALF*w)/(1 + w) + 
     -          w**TWO/(4.0E0_16*(1 + w)))*G(412))/
     -      (1 + w) + 
     -     ((-1/(4.0E0_16*(-1 + w)) + 
     -          w**TWO/(4.0E0_16*(-1 + w)))*G(415))/
     -      (1 + w) + 
     -     ((1/(4.0E0_16*(-1 + w)) - 
     -          w**TWO/(4.0E0_16*(-1 + w)))*G(416))/
     -      (1 + w) + 
     -     ((1/(4.0E0_16*(-1 + w)) - 
     -          w**TWO/(4.0E0_16*(-1 + w)))*G(417))/
     -      (1 + w) + 
     -     ((-1/(4.0E0_16*(-1 + w)) + 
     -          w**TWO/(4.0E0_16*(-1 + w)))*G(418))/
     -      (1 + w) + 
     -     ((-HALF - HALF*w)*G(421))/(1 + w) + 
     -     ((HALF + HALF*w)*G(422))/(1 + w) + 
     -     ((HALF + HALF*w)*G(423))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(424))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(442))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(443))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(444))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(445))/(-1 + w) + 
     -     ((-1/(4.0E0_16*(1 + w)) + 
     -          w**TWO/(4.0E0_16*(1 + w)))*G(446))/
     -      (-1 + w) + 
     -     ((1/(4.0E0_16*(1 + w)) - 
     -          w**TWO/(4.0E0_16*(1 + w)))*G(447))/
     -      (-1 + w) + 
     -     ((1/(4.0E0_16*(1 + w)) - 
     -          w**TWO/(4.0E0_16*(1 + w)))*G(448))/
     -      (-1 + w) + 
     -     ((-1/(4.0E0_16*(1 + w)) + 
     -          w**TWO/(4.0E0_16*(1 + w)))*G(449))/
     -      (-1 + w) + 
     -     ((1/(4.0E0_16*(-1 + w)) - 
     -          (HALF*w)/(-1 + w) + 
     -          w**TWO/(4.0E0_16*(-1 + w)))*G(452))/
     -      (-1 + w) + 
     -     ((-1/(4.0E0_16*(-1 + w)) + 
     -          (HALF*w)/(-1 + w) - 
     -          w**TWO/(4.0E0_16*(-1 + w)))*G(453))/
     -      (-1 + w) + 
     -     ((-1/(4.0E0_16*(-1 + w)) + 
     -          (HALF*w)/(-1 + w) - 
     -          w**TWO/(4.0E0_16*(-1 + w)))*G(454))/
     -      (-1 + w) + 
     -     ((1/(4.0E0_16*(-1 + w)) - 
     -          (HALF*w)/(-1 + w) + 
     -          w**TWO/(4.0E0_16*(-1 + w)))*G(455))/
     -      (-1 + w) + 
     -     ((HALF - HALF*w)*G(458))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(459))/(-1 + w) + 
     -     ((-HALF + HALF*w)*G(460))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(461))/(-1 + w) - 
     -     G(521) + G(522) + G(523) - G(524) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(534) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(535) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(536) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(537) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(547) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(548) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(549) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(550) + G(564) - G(565) - G(569) + 
     -     G(570)) - 
     -  8*((G(13) - G(14) - G(15) + G(16))*
     -      (TWO*G(35) + 
     -        (1/(1 + w) + w/(1 + w))*G(36) + 
     -        (1/(1 - w) + w/(-1 + w))*G(37) - 
     -        TWO*(G(7)*G(12) - G(55)) - G(55) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(58) + G(60)) + 
     -     (TWO*G(7) - G(12))*
     -      (HALF*G(61) - HALF*G(62) - 
     -        HALF*G(63) + HALF*G(64) + 
     -        HALF*G(67) - HALF*G(68) - 
     -        HALF*G(69) + HALF*G(70)) + 
     -     (G(1) - G(2))*
     -      (-(TWO*G(125)) + TWO*G(126) + 
     -        (1/(-1 - w) - w/(1 + w))*G(127) + 
     -        (-(1/(-1 - w)) + w/(1 + w))*
     -         G(128) + 
     -        (-(1/(1 - w)) - w/(-1 + w))*
     -         G(129) + 
     -        (1/(1 - w) + w/(-1 + w))*G(130) + 
     -        TWO*G(131) - TWO*G(132) + G(188) - 
     -        G(189) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(198) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(199) - G(205) + G(206)) + 
     -     TWO*G(368) - TWO*G(369) - TWO*G(370) + 
     -     TWO*G(371) + 
     -     (-(1/(-1 - w)) + w/(1 + w))*G(372) + 
     -     (1/(-1 - w) - w/(1 + w))*G(373) + 
     -     (1/(-1 - w) - w/(1 + w))*G(374) + 
     -     (-(1/(-1 - w)) + w/(1 + w))*G(375) + 
     -     (1/(1 - w) + w/(-1 + w))*G(378) + 
     -     (-(1/(1 - w)) - w/(-1 + w))*G(379) + 
     -     (-(1/(1 - w)) - w/(-1 + w))*G(380) + 
     -     (1/(1 - w) + w/(-1 + w))*G(381) - 
     -     TWO*G(384) + TWO*G(385) + TWO*G(386) - 
     -     TWO*G(387) + HALF*G(471) - 
     -     HALF*G(472) - HALF*G(473) + 
     -     HALF*G(474) + HALF*G(477) - 
     -     HALF*G(478) - HALF*G(479) + 
     -     HALF*G(480) - G(521) + G(522) + 
     -     G(523) - G(524) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(534) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(535) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(536) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(537) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(547) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(548) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(549) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(550) + G(564) - G(565) - G(569) + 
     -     G(570)) + 
     -  FOUR*((G(13) - G(14) - G(15) + G(16))*
     -      (FOUR*G(35) - 
     -        TWO*(G(7)*G(12) - G(55)) - 
     -        TWO*G(55) + G(60)) + 
     -     (TWO*G(7) - G(12))*
     -      (G(166) - G(167) - G(168) + G(169)) + 
     -     (G(1) - G(2))*
     -      (-(FOUR*G(125)) + FOUR*G(126) + 
     -        TWO*G(131) - TWO*G(132) + 
     -        TWO*G(188) - TWO*G(189) - G(205) + 
     -        G(206)) + FOUR*G(368) - 
     -     FOUR*G(369) - FOUR*G(370) + 
     -     FOUR*G(371) - TWO*G(384) + 
     -     TWO*G(385) + TWO*G(386) - TWO*G(387) - 
     -     TWO*G(521) + TWO*G(522) + TWO*G(523) - 
     -     TWO*G(524) + G(560) - G(561) - 
     -     G(562) + G(563) + G(564) - G(565) - 
     -     G(569) + G(570)) - 
     -  8*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (G(36) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(37) + 
     -        ((HALF + HALF*w)*G(41))/(1 + w) + 
     -        ((HALF/(-1 + w) - 
     -             (HALF*w**TWO)/(-1 + w))*G(42))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(46))/(-1 + w) + 
     -        ((-(HALF/(-1 + w)) + w/(-1 + w) - 
     -             (HALF*w**TWO)/(-1 + w))*G(47))/
     -         (-1 + w) - G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (HALF*(-(G(11)*G(13)) + G(65) + 
     -           G(166)) + 
     -        HALF*(G(11)*G(14) - G(66) - 
     -           G(167)) + 
     -        HALF*(G(11)*G(15) - G(71) - 
     -           G(168)) + 
     -        HALF*(-(G(11)*G(16)) + G(72) + 
     -           G(169))) - 
     -     G(11)*((HALF/(-1 - w) - 
     -           (HALF*w)/(1 + w))*G(127) + 
     -        (-(HALF/(-1 - w)) + 
     -           (HALF*w)/(1 + w))*G(128) + 
     -        (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -         G(129) + 
     -        (-(HALF/(1 - w)) - 
     -           (HALF*w)/(-1 + w))*G(130) + 
     -        ((-1/(4.0E0_16*(1 + w)) - 
     -             (HALF*w)/(1 + w) - 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(142))/
     -         (1 + w) + 
     -        ((1/(4.0E0_16*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(143))/
     -         (1 + w) + 
     -        ((-1/(4.0E0_16*(-1 + w)) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(144))/
     -         (1 + w) + 
     -        ((1/(4.0E0_16*(-1 + w)) - 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(145))/
     -         (1 + w) + 
     -        ((1/(4.0E0_16*(1 + w)) - 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(157))/
     -         (-1 + w) + 
     -        ((-1/(4.0E0_16*(1 + w)) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(158))/
     -         (-1 + w) + 
     -        ((1/(4.0E0_16*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(159))/
     -         (-1 + w) + 
     -        ((-1/(4.0E0_16*(-1 + w)) + 
     -             (HALF*w)/(-1 + w) - 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(160))/
     -         (-1 + w) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(198) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(199)) + 
     -     (-(G(11)*G(61)) + G(218) + G(226) + 
     -        G(471))/4.0E0_16 + 
     -     (G(11)*G(62) - G(219) - G(227) - 
     -        G(472))/4.0E0_16 + 
     -     (G(11)*G(63) - G(224) - G(228) - 
     -        G(473))/4.0E0_16 + 
     -     (-(G(11)*G(64)) + G(225) + G(229) + 
     -        G(474))/4.0E0_16 + 
     -     (-(G(11)*G(67)) + G(236) + G(244) + 
     -        G(477))/4.0E0_16 + 
     -     (G(11)*G(68) - G(237) - G(245) - 
     -        G(478))/4.0E0_16 + 
     -     (G(11)*G(69) - G(242) - G(246) - 
     -        G(479))/4.0E0_16 + 
     -     (-(G(11)*G(70)) + G(243) + G(247) + 
     -        G(480))/4.0E0_16 + 
     -     (HALF/(-1 - w) - (HALF*w)/(1 + w))*
     -      (G(12)*G(127) - G(376) - G(388) - 
     -        G(525)) + 
     -     (-(HALF/(-1 - w)) + (HALF*w)/(1 + w))*
     -      (G(12)*G(128) - G(377) - G(389) - 
     -        G(526)) + 
     -     (HALF/(1 - w) + (HALF*w)/(-1 + w))*
     -      (G(12)*G(129) - G(382) - G(390) - 
     -        G(527)) + 
     -     (-(HALF/(1 - w)) - (HALF*w)/(-1 + w))*
     -      (G(12)*G(130) - G(383) - G(391) - 
     -        G(528)) + 
     -     ((-1/(4.0E0_16*(1 + w)) - (HALF*w)/(1 + w) - 
     -          w**TWO/(4.0E0_16*(1 + w)))*
     -        (G(12)*G(142) - G(413) - G(425) - 
     -          G(538)))/(1 + w) + 
     -     ((1/(4.0E0_16*(1 + w)) + (HALF*w)/(1 + w) + 
     -          w**TWO/(4.0E0_16*(1 + w)))*
     -        (G(12)*G(143) - G(414) - G(426) - 
     -          G(539)))/(1 + w) + 
     -     ((-1/(4.0E0_16*(-1 + w)) + 
     -          w**TWO/(4.0E0_16*(-1 + w)))*
     -        (G(12)*G(144) - G(419) - G(427) - 
     -          G(540)))/(1 + w) + 
     -     ((1/(4.0E0_16*(-1 + w)) - 
     -          w**TWO/(4.0E0_16*(-1 + w)))*
     -        (G(12)*G(145) - G(420) - G(428) - 
     -          G(541)))/(1 + w) + 
     -     ((1/(4.0E0_16*(1 + w)) - 
     -          w**TWO/(4.0E0_16*(1 + w)))*
     -        (G(12)*G(157) - G(450) - G(462) - 
     -          G(551)))/(-1 + w) + 
     -     ((-1/(4.0E0_16*(1 + w)) + 
     -          w**TWO/(4.0E0_16*(1 + w)))*
     -        (G(12)*G(158) - G(451) - G(463) - 
     -          G(552)))/(-1 + w) + 
     -     ((1/(4.0E0_16*(-1 + w)) - 
     -          (HALF*w)/(-1 + w) + 
     -          w**TWO/(4.0E0_16*(-1 + w)))*
     -        (G(12)*G(159) - G(456) - G(464) - 
     -          G(553)))/(-1 + w) + 
     -     ((-1/(4.0E0_16*(-1 + w)) + 
     -          (HALF*w)/(-1 + w) - 
     -          w**TWO/(4.0E0_16*(-1 + w)))*
     -        (G(12)*G(160) - G(457) - G(465) - 
     -          G(554)))/(-1 + w) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      (G(12)*G(193) - G(542) - TWO*G(579)) 
     -      + (-(HALF/(1 + w)) - 
     -        (HALF*w)/(1 + w))*
     -      (G(12)*G(194) - G(543) - TWO*G(580)) 
     -      + (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (G(12)*G(198) - G(555) - TWO*G(584)) 
     -      + (-(HALF/(-1 + w)) + 
     -        (HALF*w)/(-1 + w))*
     -      (G(12)*G(199) - G(556) - TWO*G(585))) 
     -   + 8*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (TWO*G(36) + 
     -        (TWO/(-1 + w) - (TWO*w)/(-1 + w))*
     -         G(37) - G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) + 
     -     (TWO*G(7) - G(12))*
     -      (HALF*(-(G(11)*G(13)) + G(65) + 
     -           G(166)) + 
     -        HALF*(G(11)*G(14) - G(66) - 
     -           G(167)) + 
     -        HALF*(G(11)*G(15) - G(71) - 
     -           G(168)) + 
     -        HALF*(-(G(11)*G(16)) + G(72) + 
     -           G(169))) - 
     -     G(11)*((1/(-1 - w) - w/(1 + w))*
     -         G(127) + 
     -        (-(1/(-1 - w)) + w/(1 + w))*
     -         G(128) + 
     -        (1/(1 - w) + w/(-1 + w))*G(129) + 
     -        (-(1/(1 - w)) - w/(-1 + w))*
     -         G(130) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(198) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(199)) + 
     -     (1/(-1 - w) - w/(1 + w))*
     -      (G(12)*G(127) - G(376) - G(388) - 
     -        G(525)) + 
     -     (-(1/(-1 - w)) + w/(1 + w))*
     -      (G(12)*G(128) - G(377) - G(389) - 
     -        G(526)) + 
     -     (1/(1 - w) + w/(-1 + w))*
     -      (G(12)*G(129) - G(382) - G(390) - 
     -        G(527)) + 
     -     (-(1/(1 - w)) - w/(-1 + w))*
     -      (G(12)*G(130) - G(383) - G(391) - 
     -        G(528)) + 
     -     HALF*(-(G(11)*G(166)) + G(475) + 
     -        TWO*G(560)) + 
     -     HALF*(G(11)*G(167) - G(476) - 
     -        TWO*G(561)) + 
     -     HALF*(G(11)*G(168) - G(481) - 
     -        TWO*G(562)) + 
     -     HALF*(-(G(11)*G(169)) + G(482) + 
     -        TWO*G(563)) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      (G(12)*G(193) - G(542) - TWO*G(579)) 
     -      + (-(HALF/(1 + w)) - 
     -        (HALF*w)/(1 + w))*
     -      (G(12)*G(194) - G(543) - TWO*G(580)) 
     -      + (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (G(12)*G(198) - G(555) - TWO*G(584)) 
     -      + (-(HALF/(-1 + w)) + 
     -        (HALF*w)/(-1 + w))*
     -      (G(12)*G(199) - G(556) - TWO*G(585))) 
     -   + FOUR*((G(1)*G(11)**THREE)/6.0E0_16 + 
     -     (G(2)*G(11)**THREE)/6.0E0_16 - 
     -     HALF*G(12)**TWO*G(33) - 
     -     HALF*G(12)**TWO*G(34) + 
     -     HALF*G(12)**TWO*G(40) + 
     -     HALF*G(12)**TWO*G(45) - 
     -     HALF*G(11)**TWO*G(48) - 
     -     HALF*G(11)**TWO*G(49) + 
     -     HALF*G(12)**TWO*G(50) + 
     -     HALF*G(12)**TWO*G(51) + 
     -     (-G(33) - G(34) + TWO*G(35) + 
     -        ((-HALF - HALF*w)*G(38))/(1 + w) + 
     -        ((-HALF - HALF*w)*G(39))/(1 + w) + 
     -        G(40) + 
     -        ((HALF - HALF*w)*G(43))/(-1 + w) + 
     -        ((HALF - HALF*w)*G(44))/(-1 + w) + 
     -        G(45) + G(50) + G(51) - TWO*G(55))*
     -      G(59) + G(12)*G(131) + G(12)*G(132) - 
     -     G(12)*G(148) - G(12)*G(163) + 
     -     G(12)*G(188) + G(12)*G(189) - 
     -     G(12)*G(195) - G(12)*G(200) + 
     -     G(11)*G(203) + G(11)*G(204) - 
     -     TWO*G(12)*G(205) - TWO*G(12)*G(206) - 
     -     G(11)*(-(G(12)*G(33)) - G(12)*G(34) + 
     -        G(12)*G(40) + G(12)*G(45) + 
     -        G(12)*G(50) + G(12)*G(51) + 
     -        G(131) + G(132) - G(148) - G(163) + 
     -        G(188) + G(189) + 
     -        TWO*(G(12)*G(35) - G(133) - 
     -           G(190)) + 
     -        ((-HALF - HALF*w)*
     -           (G(12)*G(38) - G(146) - G(193)))/
     -         (1 + w) + 
     -        ((-HALF - HALF*w)*
     -           (G(12)*G(39) - G(147) - G(194)))/
     -         (1 + w) - G(195) + 
     -        ((HALF - HALF*w)*
     -           (G(12)*G(43) - G(161) - G(198)))/
     -         (-1 + w) + 
     -        ((HALF - HALF*w)*
     -           (G(12)*G(44) - G(162) - G(199)))/
     -         (-1 + w) - G(200) - TWO*G(205) - 
     -        TWO*G(206) - 
     -        TWO*(G(12)*G(55) - TWO*G(208))) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (-(HALF*G(1)*G(11)**TWO) - 
     -        HALF*G(2)*G(11)**TWO + 
     -        G(11)*G(48) + G(11)*G(49) - 
     -        G(203) - G(204) + TWO*G(212)) - 
     -     G(392) - G(393) + G(431) + G(468) - 
     -     G(529) - G(530) + G(544) + G(557) + 
     -     HALF*(-(HALF*G(11)**TWO*G(13)) + 
     -        G(11)*G(65) + G(11)*G(166) - 
     -        G(230) - G(475) - G(560)) + 
     -     HALF*(-(HALF*G(11)**TWO*G(14)) + 
     -        G(11)*G(66) + G(11)*G(167) - 
     -        G(231) - G(476) - G(561)) + 
     -     HALF*(-(HALF*G(11)**TWO*G(15)) + 
     -        G(11)*G(71) + G(11)*G(168) - 
     -        G(248) - G(481) - G(562)) + 
     -     HALF*(-(HALF*G(11)**TWO*G(16)) + 
     -        G(11)*G(72) + G(11)*G(169) - 
     -        G(249) - G(482) - G(563)) - 
     -     G(574) - G(575) + 
     -     TWO*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) + 
     -     ((-HALF - HALF*w)*
     -        (HALF*G(12)**TWO*G(38) - 
     -          G(12)*G(146) - G(12)*G(193) + 
     -          G(429) + G(542) + G(579)))/(1 + w)
     -       + ((-HALF - HALF*w)*
     -        (HALF*G(12)**TWO*G(39) - 
     -          G(12)*G(147) - G(12)*G(194) + 
     -          G(430) + G(543) + G(580)))/(1 + w)
     -       + G(581) + 
     -     ((HALF - HALF*w)*
     -        (HALF*G(12)**TWO*G(43) - 
     -          G(12)*G(161) - G(12)*G(198) + 
     -          G(466) + G(555) + G(584)))/
     -      (-1 + w) + 
     -     ((HALF - HALF*w)*
     -        (HALF*G(12)**TWO*G(44) - 
     -          G(12)*G(162) - G(12)*G(199) + 
     -          G(467) + G(556) + G(585)))/
     -      (-1 + w) + G(586) - G(589) - G(590) + 
     -     THREE*G(591) + THREE*G(592) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593))) - 
     -  TWO*(-(G(1)*G(11)**THREE)/6.0E0_16 - 
     -     (G(2)*G(11)**THREE)/6.0E0_16 + 
     -     (G(3)*G(12)**THREE)/6.0E0_16 + 
     -     (G(4)*G(12)**THREE)/6.0E0_16 + 
     -     HALF*G(11)**TWO*G(48) + 
     -     HALF*G(11)**TWO*G(49) - 
     -     HALF*G(12)**TWO*G(50) - 
     -     HALF*G(12)**TWO*G(51) + 
     -     (G(3)*G(12) + G(4)*G(12) - TWO*G(21) - 
     -        TWO*G(28) + FOUR*G(35) - G(50) - 
     -        G(51) - TWO*(G(7)*G(12) - G(55)))*
     -      G(59) - G(11)*G(203) - G(11)*G(204) + 
     -     G(12)*G(205) + G(12)*G(206) - 
     -     G(11)*(HALF*G(3)*G(12)**TWO + 
     -        HALF*G(4)*G(12)**TWO - 
     -        G(12)*G(50) - G(12)*G(51) - 
     -        TWO*(G(12)*G(21) - G(93) - 
     -           G(174)) - 
     -        TWO*(G(12)*G(28) - G(116) - 
     -           G(181)) + 
     -        FOUR*(G(12)*G(35) - G(133) - 
     -           G(190)) + G(205) + G(206) - 
     -        TWO*(HALF*G(7)*G(12)**TWO - 
     -           G(12)*G(55) + G(208))) + 
     -     (-G(3) - G(4) + TWO*G(7))*G(212) - 
     -     TWO*(HALF*G(12)**TWO*G(21) - 
     -        G(12)*G(93) - G(12)*G(174) + 
     -        G(302) + G(497) + G(566)) - 
     -     TWO*(HALF*G(12)**TWO*G(28) - 
     -        G(12)*G(116) - G(12)*G(181) + 
     -        G(357) + G(514) + G(571)) + 
     -     FOUR*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) + 
     -     G(589) + G(590) - G(591) - G(592) - 
     -     TWO*((G(7)*G(12)**THREE)/6.0E0_16 - 
     -        HALF*G(12)**TWO*G(55) + 
     -        G(12)*G(208) - G(593)) + TWO*G(596))
     -    - FOUR*(-(HALF*G(11)**TWO*G(48)) - 
     -     HALF*G(11)**TWO*G(49) + 
     -     HALF*G(12)**TWO*G(50) + 
     -     HALF*G(12)**TWO*G(51) + 
     -     (-(TWO*G(33)) - TWO*G(34) + 
     -        FOUR*G(35) + G(50) + G(51) - 
     -        TWO*G(55))*G(59) + 
     -     TWO*G(11)*G(203) + TWO*G(11)*G(204) - 
     -     TWO*G(12)*G(205) - TWO*G(12)*G(206) - 
     -     G(11)*(G(12)*G(50) + G(12)*G(51) - 
     -        TWO*(G(12)*G(33) - G(131) - 
     -           G(188)) - 
     -        TWO*(G(12)*G(34) - G(132) - 
     -           G(189)) + 
     -        FOUR*(G(12)*G(35) - G(133) - 
     -           G(190)) - TWO*G(205) - 
     -        TWO*G(206) - 
     -        TWO*(G(12)*G(55) - TWO*G(208))) + 
     -     (TWO*G(7) - G(12))*
     -      (-(HALF*G(1)*G(11)**TWO) - 
     -        HALF*G(2)*G(11)**TWO + 
     -        G(11)*G(48) + G(11)*G(49) - 
     -        G(203) - G(204) + TWO*G(212)) - 
     -     TWO*(HALF*G(12)**TWO*G(33) - 
     -        G(12)*G(131) - G(12)*G(188) + 
     -        G(392) + G(529) + G(574)) - 
     -     TWO*(HALF*G(12)**TWO*G(34) - 
     -        G(12)*G(132) - G(12)*G(189) + 
     -        G(393) + G(530) + G(575)) + 
     -     FOUR*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) - 
     -     THREE*G(589) - THREE*G(590) + 
     -     THREE*G(591) + THREE*G(592) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) + 
     -     TWO*G(596)) - 
     -  12*(-(G(7)*G(12)**THREE)/6.0E0_16 + 
     -     HALF*G(12)**TWO*G(40) + 
     -     HALF*G(12)**TWO*G(45) + 
     -     HALF*G(12)**TWO*G(55) + 
     -     G(59)*(-(G(7)*G(12)) + TWO*G(35) + 
     -        G(40) + G(45) - G(55) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + G(60)) - G(12)*G(148) - 
     -     G(12)*G(163) - G(12)*G(195) - 
     -     G(12)*G(200) - G(12)*G(208) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*G(212) - 
     -     G(11)*(-(HALF*G(7)*G(12)**TWO) + 
     -        G(12)*G(40) + G(12)*G(45) + 
     -        G(12)*G(55) - G(148) - G(163) + 
     -        TWO*(G(12)*G(35) - G(133) - 
     -           G(190)) - G(195) - G(200) - 
     -        G(208) - 
     -        TWO*(G(12)*G(55) - TWO*G(208)) + 
     -        ((-HALF - HALF*w)*
     -           (HALF*G(8)*G(12)**TWO - 
     -             G(12)*G(56) + G(209)))/(1 + w) 
     -         + ((HALF - HALF*w)*
     -           (HALF*G(10)*G(12)**TWO - 
     -             G(12)*G(58) + G(211)))/(-1 + w)
     -          + G(213)) + G(431) + G(468) + 
     -     G(544) + G(557) + 
     -     TWO*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) + 
     -     G(581) + G(586) + 
     -     HALF*((G(1)*G(11)**THREE)/6.0E0_16 - 
     -        HALF*G(11)**TWO*G(48) + 
     -        G(11)*G(203) - G(589)) + 
     -     HALF*((G(2)*G(11)**THREE)/6.0E0_16 - 
     -        HALF*G(11)**TWO*G(49) + 
     -        G(11)*G(204) - G(590)) + G(593) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) + 
     -     ((-HALF - HALF*w)*
     -        ((G(8)*G(12)**THREE)/6.0E0_16 - 
     -          HALF*G(12)**TWO*G(56) + 
     -          G(12)*G(209) - G(594)))/(1 + w) + 
     -     ((HALF - HALF*w)*
     -        ((G(10)*G(12)**THREE)/6.0E0_16 - 
     -          HALF*G(12)**TWO*G(58) + 
     -          G(12)*G(211) - G(595)))/(-1 + w) 
     -      + G(597)) - 
     -  8*(-(HALF*G(12)**TWO*G(55)) + 
     -     G(59)*(TWO*G(35) + 
     -        (1/(1 + w) + w/(1 + w))*G(36) + 
     -        (1/(1 - w) + w/(-1 + w))*G(37) - 
     -        TWO*(G(7)*G(12) - G(55)) - G(55) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(58) + G(60)) + 
     -     (TWO*G(7) - G(12))*
     -      (HALF*(HALF*G(1)*G(11)**TWO - 
     -           G(11)*G(48) + G(203)) + 
     -        HALF*(HALF*G(2)*G(11)**TWO - 
     -           G(11)*G(49) + G(204))) + 
     -     TWO*G(12)*G(208) - 
     -     G(11)*(-(G(12)*G(55)) + 
     -        TWO*(G(12)*G(35) - G(133) - 
     -           G(190)) + 
     -        (1/(1 + w) + w/(1 + w))*
     -         (G(12)*G(36) - G(134) - G(191)) + 
     -        (1/(1 - w) + w/(-1 + w))*
     -         (G(12)*G(37) - G(135) - G(192)) + 
     -        TWO*G(208) - 
     -        TWO*(HALF*G(7)*G(12)**TWO - 
     -           G(12)*G(55) + G(208)) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         (G(12)*G(58) - TWO*G(211)) + G(213)
     -        ) + TWO*
     -      (HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) + 
     -     (1/(1 + w) + w/(1 + w))*
     -      (HALF*G(12)**TWO*G(36) - 
     -        G(12)*G(134) - G(12)*G(191) + 
     -        G(395) + G(532) + G(577)) + 
     -     (1/(1 - w) + w/(-1 + w))*
     -      (HALF*G(12)**TWO*G(37) - 
     -        G(12)*G(135) - G(12)*G(192) + 
     -        G(396) + G(533) + G(578)) + 
     -     HALF*(HALF*G(11)**TWO*G(48) - 
     -        TWO*G(11)*G(203) + THREE*G(589)) + 
     -     HALF*(HALF*G(11)**TWO*G(49) - 
     -        TWO*G(11)*G(204) + THREE*G(590)) - 
     -     TWO*((G(7)*G(12)**THREE)/6.0E0_16 - 
     -        HALF*G(12)**TWO*G(55) + 
     -        G(12)*G(208) - G(593)) - 
     -     THREE*G(593) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      (HALF*G(12)**TWO*G(56) - 
     -        TWO*G(12)*G(209) + THREE*G(594)) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (HALF*G(12)**TWO*G(58) - 
     -        TWO*G(12)*G(211) + THREE*G(595)) + 
     -     G(597)) + 
     -  8*(-(G(7)*G(12)**THREE)/6.0E0_16 + 
     -     HALF*G(12)**TWO*G(35) + 
     -     G(59)*(-(G(7)*G(12)) + G(35) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(36) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(37) + 
     -        ((HALF + HALF*w)*G(40))/(1 + w) + 
     -        ((1/(4.0E0_16*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(41))/
     -         (1 + w) + 
     -        ((-1/(4.0E0_16*(-1 + w)) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(42))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(45))/(-1 + w) + 
     -        ((-1/(4.0E0_16*(1 + w)) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*G(46))/
     -         (-1 + w) + 
     -        ((1/(4.0E0_16*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*G(47))/
     -         (-1 + w) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + (HALF/(-1 + w) - 
     -           (HALF*w)/(-1 + w))*G(58) + G(60))
     -       - G(12)*G(133) - G(12)*G(190) + 
     -     (G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (HALF*(HALF*G(1)*G(11)**TWO - 
     -           G(11)*G(48) + G(203)) + 
     -        HALF*(HALF*G(2)*G(11)**TWO - 
     -           G(11)*G(49) + G(204))) + 
     -     G(12)*G(208) - 
     -     G(11)*(-(HALF*G(7)*G(12)**TWO) + 
     -        G(12)*G(35) - G(133) - G(190) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         (G(12)*G(36) - G(134) - G(191)) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*
     -         (G(12)*G(37) - G(135) - G(192)) + 
     -        ((HALF + HALF*w)*
     -           (G(12)*G(40) - G(148) - G(195)))/
     -         (1 + w) + 
     -        ((1/(4.0E0_16*(1 + w)) + 
     -             (HALF*w)/(1 + w) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*
     -           (G(12)*G(41) - G(149) - G(196)))/
     -         (1 + w) + 
     -        ((-1/(4.0E0_16*(-1 + w)) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*
     -           (G(12)*G(42) - G(150) - G(197)))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*
     -           (G(12)*G(45) - G(163) - G(200)))/
     -         (-1 + w) + 
     -        ((-1/(4.0E0_16*(1 + w)) + 
     -             w**TWO/(4.0E0_16*(1 + w)))*
     -           (G(12)*G(46) - G(164) - G(201)))/
     -         (-1 + w) + 
     -        ((1/(4.0E0_16*(-1 + w)) - 
     -             (HALF*w)/(-1 + w) + 
     -             w**TWO/(4.0E0_16*(-1 + w)))*
     -           (G(12)*G(47) - G(165) - G(202)))/
     -         (-1 + w) + G(208) + 
     -        ((-HALF - HALF*w)*
     -           (HALF*G(8)*G(12)**TWO - 
     -             G(12)*G(56) + G(209)))/(1 + w) 
     -         + (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        ((HALF - HALF*w)*
     -           (HALF*G(10)*G(12)**TWO - 
     -             G(12)*G(58) + G(211)))/(-1 + w)
     -          + (HALF/(-1 + w) - 
     -           (HALF*w)/(-1 + w))*
     -         (G(12)*G(58) - TWO*G(211)) + G(213)
     -        ) + G(394) + G(531) + 
     -     (HALF*G(11)**TWO*G(13) - G(11)*G(65) - 
     -        G(11)*G(166) + G(230) + G(475) + 
     -        G(560))/4.0E0_16 + 
     -     (HALF*G(11)**TWO*G(14) - G(11)*G(66) - 
     -        G(11)*G(167) + G(231) + G(476) + 
     -        G(561))/4.0E0_16 + 
     -     (HALF*G(11)**TWO*G(15) - G(11)*G(71) - 
     -        G(11)*G(168) + G(248) + G(481) + 
     -        G(562))/4.0E0_16 + 
     -     (HALF*G(11)**TWO*G(16) - G(11)*G(72) - 
     -        G(11)*G(169) + G(249) + G(482) + 
     -        G(563))/4.0E0_16 + G(576) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      (HALF*G(12)**TWO*G(36) - 
     -        G(12)*G(134) - G(12)*G(191) + 
     -        G(395) + G(532) + G(577)) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      (HALF*G(12)**TWO*G(37) - 
     -        G(12)*G(135) - G(12)*G(192) + 
     -        G(396) + G(533) + G(578)) + 
     -     ((HALF + HALF*w)*
     -        (HALF*G(12)**TWO*G(40) - 
     -          G(12)*G(148) - G(12)*G(195) + 
     -          G(431) + G(544) + G(581)))/(1 + w)
     -       + ((1/(4.0E0_16*(1 + w)) + 
     -          (HALF*w)/(1 + w) + 
     -          w**TWO/(4.0E0_16*(1 + w)))*
     -        (HALF*G(12)**TWO*G(41) - 
     -          G(12)*G(149) - G(12)*G(196) + 
     -          G(432) + G(545) + G(582)))/(1 + w)
     -       + ((-1/(4.0E0_16*(-1 + w)) + 
     -          w**TWO/(4.0E0_16*(-1 + w)))*
     -        (HALF*G(12)**TWO*G(42) - 
     -          G(12)*G(150) - G(12)*G(197) + 
     -          G(433) + G(546) + G(583)))/(1 + w)
     -       + ((-HALF + HALF*w)*
     -        (HALF*G(12)**TWO*G(45) - 
     -          G(12)*G(163) - G(12)*G(200) + 
     -          G(468) + G(557) + G(586)))/
     -      (-1 + w) + 
     -     ((-1/(4.0E0_16*(1 + w)) + 
     -          w**TWO/(4.0E0_16*(1 + w)))*
     -        (HALF*G(12)**TWO*G(46) - 
     -          G(12)*G(164) - G(12)*G(201) + 
     -          G(469) + G(558) + G(587)))/
     -      (-1 + w) + 
     -     ((1/(4.0E0_16*(-1 + w)) - 
     -          (HALF*w)/(-1 + w) + 
     -          w**TWO/(4.0E0_16*(-1 + w)))*
     -        (HALF*G(12)**TWO*G(47) - 
     -          G(12)*G(165) - G(12)*G(202) + 
     -          G(470) + G(559) + G(588)))/
     -      (-1 + w) - TWO*G(593) + 
     -     ((-HALF - HALF*w)*
     -        ((G(8)*G(12)**THREE)/6.0E0_16 - 
     -          HALF*G(12)**TWO*G(56) + 
     -          G(12)*G(209) - G(594)))/(1 + w) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      (HALF*G(12)**TWO*G(56) - 
     -        TWO*G(12)*G(209) + THREE*G(594)) + 
     -     ((HALF - HALF*w)*
     -        ((G(10)*G(12)**THREE)/6.0E0_16 - 
     -          HALF*G(12)**TWO*G(58) + 
     -          G(12)*G(211) - G(595)))/(-1 + w) 
     -      + (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (HALF*G(12)**TWO*G(58) - 
     -        TWO*G(12)*G(211) + THREE*G(595)) + 
     -     G(597)) + 
     -  14*(G(59)*(FOUR*G(35) - 
     -        TWO*(G(7)*G(12) - G(55)) - 
     -        TWO*G(55) + G(60)) + 
     -     (TWO*G(7) - G(12))*G(212) - 
     -     G(11)*(FOUR*
     -         (G(12)*G(35) - G(133) - G(190)) - 
     -        TWO*(HALF*G(7)*G(12)**TWO - 
     -           G(12)*G(55) + G(208)) - 
     -        TWO*(G(12)*G(55) - TWO*G(208)) + 
     -        G(213)) + 
     -     FOUR*(HALF*G(12)**TWO*G(35) - 
     -        G(12)*G(133) - G(12)*G(190) + 
     -        G(394) + G(531) + G(576)) - 
     -     TWO*((G(7)*G(12)**THREE)/6.0E0_16 - 
     -        HALF*G(12)**TWO*G(55) + 
     -        G(12)*G(208) - G(593)) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) + 
     -     G(596) + G(597))
      return
      end function fE21Euclid_w4_qp

      function fE22Euclid_w3_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE22Euclid_w3_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE22Euclid_w3_qp=        -(FOUR*zeta2*
     -     (-(HALF*G(1)) + HALF*G(2) - 
     -       HALF*G(3) + HALF*G(4))) - 
     -  8*((G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      (G(13) - G(14) - G(15) + G(16)) + 
     -     (G(1) - G(2))*
     -      (((-1 - w)*G(38))/(1 + w) + G(39) + 
     -        G(43) + ((1 - w)*G(44))/(-1 + w)) - 
     -     G(61) + G(62) + G(63) - G(64) + 
     -     G(67) - G(68) - G(69) + G(70) + 
     -     G(136) + ((-1 - w)*G(137))/(1 + w) + 
     -     ((-1 - w)*G(138))/(1 + w) + G(139) + 
     -     ((1 - w)*G(151))/(-1 + w) + G(152) + 
     -     G(153) + ((1 - w)*G(154))/(-1 + w)) + 
     -  6*(-(G(12)*(G(13) - G(14) + G(15) - 
     -          G(16))) + 
     -     (G(1) - G(2))*
     -      (-G(50) - G(51) + TWO*G(53)) - 
     -     G(166) + G(167) - G(168) + G(169) + 
     -     G(170) - G(171) + G(177) - G(178) - 
     -     TWO*G(186) + TWO*G(187)) - 
     -  TWO*(-(G(12)*
     -        (-G(13) + G(14) - G(15) + G(16) + 
     -          TWO*G(48) - TWO*G(49))) + 
     -     (G(1) - G(2))*
     -      (G(50) + G(51) - TWO*G(55)) + 
     -     G(166) - G(167) + G(168) - G(169) - 
     -     G(170) + G(171) - G(177) + G(178) + 
     -     TWO*G(188) - TWO*G(189) - TWO*G(203) + 
     -     TWO*G(204)) + 
     -  8*((G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*(-G(48) + G(49)) + 
     -     (G(1) - G(2))*
     -      (-(G(7)*G(12)) + G(55) + 
     -        ((-HALF - HALF*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((HALF - HALF*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         + G(60)) - HALF*G(65) + 
     -     HALF*G(66) - HALF*G(71) + HALF*G(72) + 
     -     G(131) - G(132) + 
     -     ((HALF + HALF*w)*G(146))/(1 + w) + 
     -     ((-HALF - HALF*w)*G(147))/(1 + w) + 
     -     ((-HALF + HALF*w)*G(161))/(-1 + w) + 
     -     ((HALF - HALF*w)*G(162))/(-1 + w) - 
     -     G(205) + G(206)) - 
     -  FOUR*((TWO*G(7) - G(12))*
     -      (-G(48) + G(49)) + 
     -     (G(1) - G(2))*
     -      (-(TWO*(G(7)*G(12) - G(55))) + G(60)) 
     -      + TWO*G(131) - TWO*G(132) - G(203) + 
     -     G(204) - G(205) + G(206)) + 
     -  TWO*((-6*G(5) - TWO*G(7) + 
     -        ((TWO + TWO*w)*G(8))/(1 + w) + 
     -        ((-TWO + TWO*w)*G(10))/(-1 + w) + 
     -        TWO*G(12))*(-G(48) + G(49)) + 
     -     (G(1) - G(2))*
     -      (6*(G(5)*G(12) - G(52)) + 
     -        TWO*(G(7)*G(12) - G(55)) + 
     -        ((-TWO - TWO*w)*
     -           (G(8)*G(12) - G(56)))/(1 + w) + 
     -        ((TWO - TWO*w)*
     -           (G(10)*G(12) - G(58)))/(-1 + w) 
     -         - TWO*G(60)) - TWO*G(65) + 
     -     TWO*G(66) - TWO*G(71) + TWO*G(72) - 
     -     6*G(119) + 6*G(120) - TWO*G(131) + 
     -     TWO*G(132) + 
     -     ((TWO + TWO*w)*G(146))/(1 + w) + 
     -     ((-TWO - TWO*w)*G(147))/(1 + w) + 
     -     ((-TWO + TWO*w)*G(161))/(-1 + w) + 
     -     ((TWO - TWO*w)*G(162))/(-1 + w) + 
     -     TWO*G(205) - TWO*G(206)) + 
     -  FOUR*(G(11)*G(13) - G(11)*G(14) + 
     -     G(11)*G(15) - G(11)*G(16) + 
     -     G(12)*G(33) - G(12)*G(34) + 
     -     G(12)*G(39) + G(12)*G(44) + 
     -     (-6*G(5) - TWO*G(7) + 
     -        ((TWO + TWO*w)*G(8))/(1 + w) + 
     -        ((-TWO + TWO*w)*G(10))/(-1 + w) + 
     -        TWO*G(12))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) - 
     -     G(12)*G(50) + G(12)*G(51) - 
     -     G(11)*(THREE*G(31) - THREE*G(32) + 
     -        G(33) - G(34) + 
     -        ((-1 - w)*G(38))/(1 + w) + G(39) + 
     -        ((1 - w)*G(43))/(-1 + w) + G(44) - 
     -        G(50) + G(51)) - G(65) + G(66) - 
     -     G(71) + G(72) - G(131) + G(132) - 
     -     G(147) - G(162) - G(166) + G(167) - 
     -     G(168) + G(169) + 
     -     THREE*(G(12)*G(31) - G(119) - 
     -        G(184)) - 
     -     THREE*(G(12)*G(32) - G(120) - 
     -        G(185)) - G(188) + G(189) + 
     -     ((-1 - w)*
     -        (G(12)*G(38) - G(146) - G(193)))/
     -      (1 + w) - G(194) + 
     -     ((1 - w)*(G(12)*G(43) - G(161) - 
     -          G(198)))/(-1 + w) - G(199) + 
     -     TWO*G(205) - TWO*G(206)) + 
     -  FOUR*(G(12)*G(21) - G(12)*G(28) - 
     -     G(11)*(THREE*G(20) + G(21) - 
     -        THREE*G(27) - G(28) - 
     -        TWO*(G(3)*G(12) - G(50)) + 
     -        TWO*(G(4)*G(12) - G(51))) + 
     -     FOUR*(-(HALF*G(3)) + HALF*G(4))*
     -      G(59) - G(93) + G(116) + 
     -     THREE*(G(12)*G(20) - G(92) - G(173)) - 
     -     G(174) - THREE*
     -      (G(12)*G(27) - G(115) - G(180)) + 
     -     G(181) - TWO*
     -      (HALF*G(1)*G(11)**TWO - G(11)*G(48) + 
     -        G(203)) + 
     -     TWO*(HALF*G(2)*G(11)**TWO - 
     -        G(11)*G(49) + G(204)) - 
     -     TWO*(HALF*G(3)*G(12)**TWO - 
     -        G(12)*G(50) + G(205)) + 
     -     TWO*(HALF*G(4)*G(12)**TWO - 
     -        G(12)*G(51) + G(206))) + 
     -  16*((G(7) + ((HALF + HALF*w)*G(8))/
     -         (1 + w) + 
     -        ((-HALF + HALF*w)*G(10))/(-1 + w) - 
     -        G(12))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) - 
     -     G(11)*(-(HALF*G(33)) + HALF*G(34) + 
     -        ((-0.25E0_16 - w/4E0_16)*G(38))/(1 + w) + 
     -        ((0.25E0_16 + w/4E0_16)*G(39))/(1 + w) + 
     -        ((0.25E0_16 - w/4E0_16)*G(43))/(-1 + w) + 
     -        ((-0.25E0_16 + w/4.0E0_16)*G(44))/(-1 + w) + 
     -        HALF*G(50) - HALF*G(51)) + 
     -     (G(11)*G(13) - G(65) - G(166))/4.0E0_16 + 
     -     (-(G(11)*G(14)) + G(66) + G(167))/4.0E0_16 + 
     -     (G(11)*G(15) - G(71) - G(168))/4.0E0_16 + 
     -     (-(G(11)*G(16)) + G(72) + G(169))/4.0E0_16 + 
     -     HALF*(-(G(12)*G(33)) + G(131) + 
     -        G(188)) + 
     -     HALF*(G(12)*G(34) - G(132) - G(189)) + 
     -     ((-0.25E0_16 - w/4E0_16)*
     -        (G(12)*G(38) - G(146) - G(193)))/
     -      (1 + w) + 
     -     ((0.25E0_16 + w/4E0_16)*
     -        (G(12)*G(39) - G(147) - G(194)))/
     -      (1 + w) + 
     -     ((0.25E0_16 - w/4E0_16)*
     -        (G(12)*G(43) - G(161) - G(198)))/
     -      (-1 + w) + 
     -     ((-0.25E0_16 + w/4.0E0_16)*
     -        (G(12)*G(44) - G(162) - G(199)))/
     -      (-1 + w) + 
     -     HALF*(G(12)*G(50) - TWO*G(205)) + 
     -     HALF*(-(G(12)*G(51)) + TWO*G(206))) - 
     -  8*(-(G(12)*G(33)) + G(12)*G(34) + 
     -     (TWO*G(7) - G(12))*
     -      (HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49))) - 
     -     G(11)*(-G(33) + G(34) + HALF*G(50) - 
     -        HALF*G(51)) + G(131) - G(132) + 
     -     G(188) - G(189) + 
     -     HALF*(G(11)*G(48) - TWO*G(203)) + 
     -     HALF*(-(G(11)*G(49)) + TWO*G(204)) + 
     -     HALF*(G(12)*G(50) - TWO*G(205)) + 
     -     HALF*(-(G(12)*G(51)) + TWO*G(206))) - 
     -  8*(-(HALF*G(1)*G(11)**TWO) + 
     -     HALF*G(2)*G(11)**TWO + 
     -     HALF*G(8)*G(12)**TWO + G(11)*G(48) - 
     -     G(11)*G(49) - G(12)*G(56) - 
     -     G(11)*(G(8)*G(12) - G(56) + 
     -        ((1 - w)*(G(10)*G(12) - G(58)))/
     -         (-1 + w)) + 
     -     (G(8) + ((1 - w)*G(10))/(-1 + w))*
     -      G(59) - G(203) + G(204) + G(209) + 
     -     ((1 - w)*(HALF*G(10)*G(12)**TWO - 
     -          G(12)*G(58) + G(211)))/(-1 + w))
      return
      end function fE22Euclid_w3_qp

      function fE27Euclid_w4_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 fE27Euclid_w4_qp
      complex*32 w,z
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      complex*32 G(NDIM)
      fE27Euclid_w4_qp=        8*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(22) + 
     -        (1/(1 - w) + w/(-1 + w))*G(23) + 
     -        G(29) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(30)) + 
     -     (-G(3) + G(4))*
     -      (-G(61) + G(62) + G(63) - G(64) + 
     -        G(67) - G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      ((1/(1 + w) + w/(1 + w))*G(85) - 
     -        G(86) - G(87) + 
     -        (1/(1 - w) + w/(-1 + w))*G(88) + 
     -        (1/(-1 - w) - w/(1 + w))*G(108) + 
     -        G(109) + G(110) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(111)) - 
     -     G(214) + G(215) + G(216) - G(217) + 
     -     G(220) - G(221) - G(222) + G(223) + 
     -     G(232) - G(233) - G(234) + G(235) - 
     -     G(238) + G(239) + G(240) - G(241) - 
     -     G(276) + (1/(1 + w) + w/(1 + w))*
     -      G(277) + 
     -     (1/(1 + w) + w/(1 + w))*G(278) - 
     -     G(279) + (-(1/(-1 + w)) + w/(-1 + w))*
     -      G(282) - G(283) - G(284) + 
     -     (-(1/(-1 + w)) + w/(-1 + w))*G(285) + 
     -     G(331) + (-(1/(1 + w)) - w/(1 + w))*
     -      G(332) + 
     -     (-(1/(1 + w)) - w/(1 + w))*G(333) + 
     -     G(334) + (1/(-1 + w) - w/(-1 + w))*
     -      G(337) + G(338) + G(339) + 
     -     (1/(-1 + w) - w/(-1 + w))*G(340)) - 
     -  8*((G(13) - G(14) + G(15) - G(16))*
     -      (G(3)*G(12) - G(4)*G(12) - G(50) + 
     -        G(51)) + 
     -     (G(1) - G(2))*
     -      (G(89) + G(90) - TWO*G(91) - G(112) - 
     -        G(113) + TWO*G(114)) + 
     -     (-G(3) + G(4))*
     -      (-G(166) + G(167) - G(168) + G(169)) 
     -      - G(226) + G(227) - G(228) + G(229) + 
     -     G(244) - G(245) + G(246) - G(247) - 
     -     G(288) + G(289) - G(290) + G(291) + 
     -     TWO*G(292) - TWO*G(293) + G(343) - 
     -     G(344) + G(345) - G(346) - 
     -     TWO*G(347) + TWO*G(348)) + 
     -  8*((-G(48) + G(49))*
     -      (G(3)*G(12) - G(4)*G(12) - 
     -        TWO*G(21) + TWO*G(28) - G(50) + 
     -        G(51)) + 
     -     (-G(3) + G(4))*(-G(203) + G(204)) + 
     -     (G(1) - G(2))*
     -      (-(HALF*G(3)*G(12)**TWO) + 
     -        HALF*G(4)*G(12)**TWO + 
     -        G(12)*G(50) - G(12)*G(51) + 
     -        TWO*(G(12)*G(21) - G(93) - 
     -           G(174)) - 
     -        TWO*(G(12)*G(28) - G(116) - 
     -           G(181)) - G(205) + G(206)) - 
     -     G(230) + G(231) + G(248) - G(249) - 
     -     TWO*G(274) + TWO*G(275) + G(300) - 
     -     G(301) + TWO*G(329) - TWO*G(330) - 
     -     G(355) + G(356)) - 
     -  16*((-G(48) + G(49))*
     -      (G(3)*G(12) - G(4)*G(12) - G(21) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(22) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(23) + G(28) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(29) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(30) - 
     -        G(50) + G(51)) + 
     -     (-G(3) + G(4))*
     -      (-(HALF*G(65)) + HALF*G(66) - 
     -        HALF*G(71) + HALF*G(72)) + 
     -     (G(1) - G(2))*
     -      (-(HALF*G(3)*G(12)**TWO) + 
     -        HALF*G(4)*G(12)**TWO + 
     -        G(12)*G(21) - G(12)*G(28) + 
     -        G(12)*G(50) - G(12)*G(51) - G(93) + 
     -        G(116) - G(174) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         (G(12)*G(22) - G(94) - G(175)) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*
     -         (G(12)*G(23) - G(95) - G(176)) + 
     -        G(181) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*
     -         (G(12)*G(29) - G(117) - G(182)) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         (G(12)*G(30) - G(118) - G(183)) - 
     -        G(205) + G(206)) - HALF*G(218) + 
     -     HALF*G(219) - HALF*G(224) + 
     -     HALF*G(225) + HALF*G(236) - 
     -     HALF*G(237) + HALF*G(242) - 
     -     HALF*G(243) - G(274) + G(275) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(280) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(281) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(286) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(287) + G(300) - G(301) + G(329) - 
     -     G(330) + (HALF/(1 + w) + 
     -        (HALF*w)/(1 + w))*G(335) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(336) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(341) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(342) - G(355) + G(356)) - 
     -  8*((G(13) - G(14) + G(15) - G(16))*
     -      (G(50) - G(51)) - 
     -     G(12)*(G(61) - G(62) + G(63) - G(64) - 
     -        G(67) + G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      (G(170) + G(171) - TWO*G(172) - 
     -        G(177) - G(178) + TWO*G(179)) - 
     -     G(471) + G(472) - G(473) + G(474) + 
     -     G(477) - G(478) + G(479) - G(480) - 
     -     G(483) + G(484) - G(485) + G(486) + 
     -     TWO*G(487) - TWO*G(488) + G(500) - 
     -     G(501) + G(502) - G(503) - 
     -     TWO*G(504) + TWO*G(505)) + 
     -  16*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (G(3)*G(12) - G(4)*G(12) - 
     -        TWO*G(21) + TWO*G(28) - G(50) + 
     -        G(51)) + G(12)*G(83) - 
     -     G(12)*G(84) - G(12)*G(106) + 
     -     G(12)*G(107) - 
     -     G(11)*(G(83) - G(84) - HALF*G(89) + 
     -        HALF*G(90) - G(106) + G(107) + 
     -        HALF*G(112) - HALF*G(113)) + 
     -     (-G(3) + G(4))*
     -      (HALF*(G(11)*G(48) - TWO*G(203)) + 
     -        HALF*(-(G(11)*G(49)) + TWO*G(204))) 
     -      - G(274) + G(275) - G(294) + G(295) + 
     -     G(329) - G(330) + G(349) - G(350) + 
     -     HALF*(G(11)*G(65) - TWO*G(230) - 
     -        G(475)) + 
     -     HALF*(-(G(11)*G(66)) + TWO*G(231) + 
     -        G(476)) + 
     -     HALF*(-(G(11)*G(71)) + TWO*G(248) + 
     -        G(481)) + 
     -     HALF*(G(11)*G(72) - TWO*G(249) - 
     -        G(482)) - G(489) + G(490) + 
     -     HALF*(-(G(12)*G(89)) + TWO*G(300) + 
     -        G(495)) + 
     -     HALF*(G(12)*G(90) - TWO*G(301) - 
     -        G(496)) + G(506) - G(507) + 
     -     HALF*(G(12)*G(112) - TWO*G(355) - 
     -        G(512)) + 
     -     HALF*(-(G(12)*G(113)) + TWO*G(356) + 
     -        G(513))) - 
     -  32*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (G(3)*G(12) - G(4)*G(12) - G(21) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(22) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(23) + G(28) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(29) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(30) - 
     -        G(50) + G(51)) - 
     -     G(11)*(HALF*G(83) - HALF*G(84) + 
     -        (1/(4.0E0_16*(1 + w)) + w/(4.0E0_16*(1 + w)))*
     -         G(85) + 
     -        (-1/(4.0E0_16*(1 + w)) - w/(4.0E0_16*(1 + w)))*
     -         G(86) + 
     -        (-1/(4.0E0_16*(-1 + w)) + 
     -           w/(4.0E0_16*(-1 + w)))*G(87) + 
     -        (1/(4.0E0_16*(-1 + w)) - w/(4.0E0_16*(-1 + w)))*
     -         G(88) - HALF*G(89) + HALF*G(90) - 
     -        HALF*G(106) + HALF*G(107) + 
     -        (-1/(4.0E0_16*(1 + w)) - w/(4.0E0_16*(1 + w)))*
     -         G(108) + 
     -        (1/(4.0E0_16*(1 + w)) + w/(4.0E0_16*(1 + w)))*
     -         G(109) + 
     -        (1/(4.0E0_16*(-1 + w)) - w/(4.0E0_16*(-1 + w)))*
     -         G(110) + 
     -        (-1/(4.0E0_16*(-1 + w)) + 
     -           w/(4.0E0_16*(-1 + w)))*G(111) + 
     -        HALF*G(112) - HALF*G(113)) + 
     -     (-G(3) + G(4))*
     -      ((G(11)*G(13) - G(65) - G(166))/4.0E0_16 + 
     -        (-(G(11)*G(14)) + G(66) + G(167))/
     -         4. + (G(11)*G(15) - G(71) - 
     -           G(168))/4.0E0_16 + 
     -        (-(G(11)*G(16)) + G(72) + G(169))/4.0E0_16
     -        ) + (G(11)*G(61) - G(218) - 
     -        G(226) - G(471))/4.0E0_16 + 
     -     (-(G(11)*G(62)) + G(219) + G(227) + 
     -        G(472))/4.0E0_16 + 
     -     (G(11)*G(63) - G(224) - G(228) - 
     -        G(473))/4.0E0_16 + 
     -     (-(G(11)*G(64)) + G(225) + G(229) + 
     -        G(474))/4.0E0_16 + 
     -     (-(G(11)*G(67)) + G(236) + G(244) + 
     -        G(477))/4.0E0_16 + 
     -     (G(11)*G(68) - G(237) - G(245) - 
     -        G(478))/4.0E0_16 + 
     -     (-(G(11)*G(69)) + G(242) + G(246) + 
     -        G(479))/4.0E0_16 + 
     -     (G(11)*G(70) - G(243) - G(247) - 
     -        G(480))/4.0E0_16 + 
     -     HALF*(G(12)*G(83) - G(274) - G(294) - 
     -        G(489)) + 
     -     HALF*(-(G(12)*G(84)) + G(275) + 
     -        G(295) + G(490)) + 
     -     (1/(4.0E0_16*(1 + w)) + w/(4.0E0_16*(1 + w)))*
     -      (G(12)*G(85) - G(280) - G(296) - 
     -        G(491)) + 
     -     (-1/(4.0E0_16*(1 + w)) - w/(4.0E0_16*(1 + w)))*
     -      (G(12)*G(86) - G(281) - G(297) - 
     -        G(492)) + 
     -     (-1/(4.0E0_16*(-1 + w)) + w/(4.0E0_16*(-1 + w)))*
     -      (G(12)*G(87) - G(286) - G(298) - 
     -        G(493)) + 
     -     (1/(4.0E0_16*(-1 + w)) - w/(4.0E0_16*(-1 + w)))*
     -      (G(12)*G(88) - G(287) - G(299) - 
     -        G(494)) + 
     -     HALF*(-(G(12)*G(89)) + TWO*G(300) + 
     -        G(495)) + 
     -     HALF*(G(12)*G(90) - TWO*G(301) - 
     -        G(496)) + 
     -     HALF*(-(G(12)*G(106)) + G(329) + 
     -        G(349) + G(506)) + 
     -     HALF*(G(12)*G(107) - G(330) - G(350) - 
     -        G(507)) + 
     -     (-1/(4.0E0_16*(1 + w)) - w/(4.0E0_16*(1 + w)))*
     -      (G(12)*G(108) - G(335) - G(351) - 
     -        G(508)) + 
     -     (1/(4.0E0_16*(1 + w)) + w/(4.0E0_16*(1 + w)))*
     -      (G(12)*G(109) - G(336) - G(352) - 
     -        G(509)) + 
     -     (1/(4.0E0_16*(-1 + w)) - w/(4.0E0_16*(-1 + w)))*
     -      (G(12)*G(110) - G(341) - G(353) - 
     -        G(510)) + 
     -     (-1/(4.0E0_16*(-1 + w)) + w/(4.0E0_16*(-1 + w)))*
     -      (G(12)*G(111) - G(342) - G(354) - 
     -        G(511)) + 
     -     HALF*(G(12)*G(112) - TWO*G(355) - 
     -        G(512)) + 
     -     HALF*(-(G(12)*G(113)) + TWO*G(356) + 
     -        G(513))) + 
     -  8*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(50) - G(51) + TWO*G(53)) - 
     -     G(12)*(G(61) - G(62) - G(63) + G(64) + 
     -        G(67) - G(68) - G(69) + G(70)) + 
     -     (G(1) - G(2))*
     -      (G(170) - G(171) + G(177) - G(178) - 
     -        TWO*G(186) + TWO*G(187)) - G(471) + 
     -     G(472) + G(473) - G(474) - G(477) + 
     -     G(478) + G(479) - G(480) - G(483) + 
     -     G(484) + G(485) - G(486) - G(500) + 
     -     G(501) + G(502) - G(503) + 
     -     TWO*G(517) - TWO*G(518) - TWO*G(519) + 
     -     TWO*G(520)) + 
     -  8*((-G(48) + G(49))*
     -      (-G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) - 
     -     G(12)*(G(65) - G(66) - G(71) + 
     -        G(72)) + 
     -     (G(1) - G(2))*
     -      (-(G(12)*G(58)) + 
     -        (1/(1 + w) + w/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        TWO*G(211)) - G(475) + G(476) + 
     -     G(481) - G(482) - G(542) + 
     -     (1/(1 + w) + w/(1 + w))*G(543) + 
     -     (1/(1 - w) + w/(-1 + w))*G(555) - 
     -     G(556)) - 
     -  16*((G(13) - G(14) - G(15) + G(16))*
     -      (-G(55) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(58) + G(60)) - 
     -     G(12)*(HALF*G(61) - HALF*G(62) - 
     -        HALF*G(63) + HALF*G(64) + 
     -        HALF*G(67) - HALF*G(68) - 
     -        HALF*G(69) + HALF*G(70)) + 
     -     (G(1) - G(2))*
     -      (G(188) - G(189) + 
     -        (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -         G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(198) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(199) - G(205) + G(206)) - 
     -     HALF*G(471) + HALF*G(472) + 
     -     HALF*G(473) - HALF*G(474) - 
     -     HALF*G(477) + HALF*G(478) + 
     -     HALF*G(479) - HALF*G(480) - G(521) + 
     -     G(522) + G(523) - G(524) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(534) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(535) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      G(536) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      G(537) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(547) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(548) + 
     -     (-(HALF/(-1 + w)) + (HALF*w)/(-1 + w))*
     -      G(549) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      G(550) + G(564) - G(565) - G(569) + 
     -     G(570)) + 
     -  8*((G(13) - G(14) - G(15) + G(16))*
     -      (-(TWO*G(55)) + G(60)) - 
     -     G(12)*(G(166) - G(167) - G(168) + 
     -        G(169)) + 
     -     (G(1) - G(2))*
     -      (TWO*G(188) - TWO*G(189) - G(205) + 
     -        G(206)) - TWO*G(521) + TWO*G(522) + 
     -     TWO*G(523) - TWO*G(524) - G(560) + 
     -     G(561) + G(562) - G(563) + G(564) - 
     -     G(565) - G(569) + G(570)) + 
     -  8*(-(HALF*G(11)**TWO*G(13)) + 
     -     HALF*G(11)**TWO*G(14) + 
     -     HALF*G(11)**TWO*G(15) - 
     -     HALF*G(11)**TWO*G(16) - 
     -     HALF*G(12)**TWO*G(22) + 
     -     HALF*G(12)**TWO*G(29) + 
     -     (-G(22) + 
     -        (1/(1 - w) + w/(-1 + w))*G(23) + 
     -        G(29) + 
     -        (1/(-1 + w) - w/(-1 + w))*G(30))*
     -      G(59) + G(11)*G(65) - G(11)*G(66) - 
     -     G(11)*G(71) + G(11)*G(72) + 
     -     G(12)*G(94) - G(12)*G(117) + 
     -     G(11)*G(166) - G(11)*G(167) - 
     -     G(11)*G(168) + G(11)*G(169) + 
     -     G(12)*G(175) - G(12)*G(182) - 
     -     G(11)*(-(G(12)*G(22)) + G(12)*G(29) + 
     -        G(94) - G(117) + G(175) + 
     -        (1/(1 - w) + w/(-1 + w))*
     -         (G(12)*G(23) - G(95) - G(176)) - 
     -        G(182) + 
     -        (1/(-1 + w) - w/(-1 + w))*
     -         (G(12)*G(30) - G(118) - G(183))) + 
     -     (-G(3) + G(4))*
     -      (-(HALF*G(1)*G(11)**TWO) + 
     -        HALF*G(2)*G(11)**TWO + 
     -        G(11)*G(48) - G(11)*G(49) - 
     -        G(203) + G(204)) - G(230) + 
     -     G(231) + G(248) - G(249) - G(303) + 
     -     G(358) - G(475) + G(476) + G(481) - 
     -     G(482) - G(498) + G(515) - G(560) + 
     -     G(561) + G(562) - G(563) - G(567) + 
     -     (1/(1 - w) + w/(-1 + w))*
     -      (HALF*G(12)**TWO*G(23) - 
     -        G(12)*G(95) - G(12)*G(176) + 
     -        G(304) + G(499) + G(568)) + 
     -     G(572) + (1/(-1 + w) - w/(-1 + w))*
     -      (HALF*G(12)**TWO*G(30) - 
     -        G(12)*G(118) - G(12)*G(183) + 
     -        G(359) + G(516) + G(573))) + 
     -  16*((HALF*(G(1)*G(11) - G(48)) + 
     -        HALF*(-(G(2)*G(11)) + G(49)))*
     -      (-G(56) + 
     -        (1/(1 - w) + w/(-1 + w))*G(58)) - 
     -     G(12)*(HALF*
     -         (-(G(11)*G(13)) + G(65) + G(166)) 
     -         + HALF*
     -         (G(11)*G(14) - G(66) - G(167)) + 
     -        HALF*(G(11)*G(15) - G(71) - 
     -           G(168)) + 
     -        HALF*(-(G(11)*G(16)) + G(72) + 
     -           G(169))) - 
     -     G(11)*((HALF/(1 + w) + 
     -           (HALF*w)/(1 + w))*G(193) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(194) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(198) + 
     -        (-(HALF/(-1 + w)) + 
     -           (HALF*w)/(-1 + w))*G(199)) + 
     -     HALF*(G(11)*G(166) - G(475) - 
     -        TWO*G(560)) + 
     -     HALF*(-(G(11)*G(167)) + G(476) + 
     -        TWO*G(561)) + 
     -     HALF*(-(G(11)*G(168)) + G(481) + 
     -        TWO*G(562)) + 
     -     HALF*(G(11)*G(169) - G(482) - 
     -        TWO*G(563)) + 
     -     (HALF/(1 + w) + (HALF*w)/(1 + w))*
     -      (G(12)*G(193) - G(542) - TWO*G(579)) 
     -      + (-(HALF/(1 + w)) - 
     -        (HALF*w)/(1 + w))*
     -      (G(12)*G(194) - G(543) - TWO*G(580)) 
     -      + (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (G(12)*G(198) - G(555) - TWO*G(584)) 
     -      + (-(HALF/(-1 + w)) + 
     -        (HALF*w)/(-1 + w))*
     -      (G(12)*G(199) - G(556) - TWO*G(585))) 
     -   - 8*(HALF*G(11)**TWO*G(48) + 
     -     HALF*G(11)**TWO*G(49) + 
     -     HALF*G(12)**TWO*G(50) + 
     -     HALF*G(12)**TWO*G(51) + 
     -     (G(50) + G(51) - TWO*G(55))*G(59) - 
     -     TWO*G(11)*G(203) - TWO*G(11)*G(204) - 
     -     TWO*G(12)*G(205) - TWO*G(12)*G(206) - 
     -     G(11)*(G(12)*G(50) + G(12)*G(51) - 
     -        TWO*G(205) - TWO*G(206) - 
     -        TWO*(G(12)*G(55) - TWO*G(208))) - 
     -     G(12)*(-(HALF*G(1)*G(11)**TWO) - 
     -        HALF*G(2)*G(11)**TWO + 
     -        G(11)*G(48) + G(11)*G(49) - 
     -        G(203) - G(204) + TWO*G(212)) + 
     -     THREE*G(589) + THREE*G(590) + 
     -     THREE*G(591) + THREE*G(592) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) - 
     -     TWO*G(596)) - 
     -  16*(-(HALF*G(12)**TWO*G(55)) + 
     -     G(59)*(-G(55) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*G(56) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         G(58) + G(60)) - 
     -     G(12)*(HALF*
     -         (HALF*G(1)*G(11)**TWO - 
     -           G(11)*G(48) + G(203)) + 
     -        HALF*(HALF*G(2)*G(11)**TWO - 
     -           G(11)*G(49) + G(204))) + 
     -     TWO*G(12)*G(208) - 
     -     G(11)*(-(G(12)*G(55)) + TWO*G(208) + 
     -        (-(HALF/(1 + w)) - 
     -           (HALF*w)/(1 + w))*
     -         (G(12)*G(56) - TWO*G(209)) + 
     -        (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -         (G(12)*G(58) - TWO*G(211)) + G(213)
     -        ) + HALF*
     -      (-(HALF*G(11)**TWO*G(48)) + 
     -        TWO*G(11)*G(203) - THREE*G(589)) + 
     -     HALF*(-(HALF*G(11)**TWO*G(49)) + 
     -        TWO*G(11)*G(204) - THREE*G(590)) - 
     -     THREE*G(593) + 
     -     (-(HALF/(1 + w)) - (HALF*w)/(1 + w))*
     -      (HALF*G(12)**TWO*G(56) - 
     -        TWO*G(12)*G(209) + THREE*G(594)) + 
     -     (HALF/(-1 + w) - (HALF*w)/(-1 + w))*
     -      (HALF*G(12)**TWO*G(58) - 
     -        TWO*G(12)*G(211) + THREE*G(595)) + 
     -     G(597)) + 
     -  16*(G(59)*(-(TWO*G(55)) + G(60)) - 
     -     G(12)*G(212) - 
     -     G(11)*(-(TWO*
     -           (G(12)*G(55) - TWO*G(208))) + 
     -        G(213)) - 
     -     TWO*(HALF*G(12)**TWO*G(55) - 
     -        TWO*G(12)*G(208) + THREE*G(593)) - 
     -     G(596) + G(597))
      return
      end function fE27Euclid_w4_qp

      subroutine GPLbasis(w,z,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=597)
      ! itool: 1 (FastGPL, default), 2 (handyG)
      INTEGER,INTENT(IN),OPTIONAL::itool
      INTEGER::itool_used
      INTEGER::ix
      double complex cres(NDIM)
      double complex w,z,x
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double complex a(4)
      integer s(4)
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
      IF(init_GPLs)THEN
        cres(1)=GPL(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs=.FALSE.
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
      a(1)=-(1/(w*z))
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(5)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(w/z)
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(6)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(7)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(8)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(z/w)
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(9)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(10)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      x=w
      ix=-1
      na=1
      cres(11)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      x=z
      ix=1
      na=1
      cres(12)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(13)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(14)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(15)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(16)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(17)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(18)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(19)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(w/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(20)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(21)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(22)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(23)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(24)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(25)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(26)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(w/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(27)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(28)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(29)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(30)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(31)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(32)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(33)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(34)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(35)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(36)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(37)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(38)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(39)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(40)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(41)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(42)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(43)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(44)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(45)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(46)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(47)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(48)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(49)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(50)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(51)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/(w*z))
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(52)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(53)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(w/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(54)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(55)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(56)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(z/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(57)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(58)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      x=w
      ix=-1
      na=2
      cres(59)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      x=z
      ix=1
      na=2
      cres(60)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(61)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(62)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(63)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(64)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(65)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(66)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(67)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(68)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(69)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(70)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(71)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(72)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(73)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(74)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(75)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(76)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(77)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
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
      a(3)=1/(w*z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(79)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(80)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(81)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(82)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(83)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(84)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(85)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(86)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(87)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(88)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(89)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(90)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(91)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(92)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(93)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(94)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(95)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(96)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(97)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(98)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(99)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(100)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(101)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(102)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(103)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(104)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(105)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(106)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(107)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(108)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(109)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(110)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(111)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(112)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(113)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(114)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(115)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(116)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(117)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(118)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(119)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(120)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(121)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(122)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(123)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(124)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(125)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(126)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(127)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(128)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(129)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(130)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(131)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(132)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(133)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(134)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(135)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(136)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(137)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(138)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(139)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(140)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(141)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(142)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(143)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(144)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(145)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(146)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(147)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(148)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(149)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(150)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(151)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(152)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(153)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(154)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(155)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(156)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(157)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(158)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(159)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(160)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(161)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(162)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(163)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(164)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(165)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(166)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(167)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(168)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(169)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(170)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(171)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(172)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(173)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(174)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(175)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(176)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(177)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(178)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(179)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(180)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(181)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(182)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(183)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/(w*z))
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(184)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/(w*z))
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(185)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(186)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(187)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(188)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(189)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(190)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(191)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(192)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(193)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(194)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(195)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(196)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(197)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(198)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(199)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(200)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(201)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(202)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(203)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(204)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(205)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(206)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(207)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(208)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(209)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(z/w)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(210)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(211)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      x=w
      ix=-1
      na=3
      cres(212)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      x=z
      ix=1
      na=3
      cres(213)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(214)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(215)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(216)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(217)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(218)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(219)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(220)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(221)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(222)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(223)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(224)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(225)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(226)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(227)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(228)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(229)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(230)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(231)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(232)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(233)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(234)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(235)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(236)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(237)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(238)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(239)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(240)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(241)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(242)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(243)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(244)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(245)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(246)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(247)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(248)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(249)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(250)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(251)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(252)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(253)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(254)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(255)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(256)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(257)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(258)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(259)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(260)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(261)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(262)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(263)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(264)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(265)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(266)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(267)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(268)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(269)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(270)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(271)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(272)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(273)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(274)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(275)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(276)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(277)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(278)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(279)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(280)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(281)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(282)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(283)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(284)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(285)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(286)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(287)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(288)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(289)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(290)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(291)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(292)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(293)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(294)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(295)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(296)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(297)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(298)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(299)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(300)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(301)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(302)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(303)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(304)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(305)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(306)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(307)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(308)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(309)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(310)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(311)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(312)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(313)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(314)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(315)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(316)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(317)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(318)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(319)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(320)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(321)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(322)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(323)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(324)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(325)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(326)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(327)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(328)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(329)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(330)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(331)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(332)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(333)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(334)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(335)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(336)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(337)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(338)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(339)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(340)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(341)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(342)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(343)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(344)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(345)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(346)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(347)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(348)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(349)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(350)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(351)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(352)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(353)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(354)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(355)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(356)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(357)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(358)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(359)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(360)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(361)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(362)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(363)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(364)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(365)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(366)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(367)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(368)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(369)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(370)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(371)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(372)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(373)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(374)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(375)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(376)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(377)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(378)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(379)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(380)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(381)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(382)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(383)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(384)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(385)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(386)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(387)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(388)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(389)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(390)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(391)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(392)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(393)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(394)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(395)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(396)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(397)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(398)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(399)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(400)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(401)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(402)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(403)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(404)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(405)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(406)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(407)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(408)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(409)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(410)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(411)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(412)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(413)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(414)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(415)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(416)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(417)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(418)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(419)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(420)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(421)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(422)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(423)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(424)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(425)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(426)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(427)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(428)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(429)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(430)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(431)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(432)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(433)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(434)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(435)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(436)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(437)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(438)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(439)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(440)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(441)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(442)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(443)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(444)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(445)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(446)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(447)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(448)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(449)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(450)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(451)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(452)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(453)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(454)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(455)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(456)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(457)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(458)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(459)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(460)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(461)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(462)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(463)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(464)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(465)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(466)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(467)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(468)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(469)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(470)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(471)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(472)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(473)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(474)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(475)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(476)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(477)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(478)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(479)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(480)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(481)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(482)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(483)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(484)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(485)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(486)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(487)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(488)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(489)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(490)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(491)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(492)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(493)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(494)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(495)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(496)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(497)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(498)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(499)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(500)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(501)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(502)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(503)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(504)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(505)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(506)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(507)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(508)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(509)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(510)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(511)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(512)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(513)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(514)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(515)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(516)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(517)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(518)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(519)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(520)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(521)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(522)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(523)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(524)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(525)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(526)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(527)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(528)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(529)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(530)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(531)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(532)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(533)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(534)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(535)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(536)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(537)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(538)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(539)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(540)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(541)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(542)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(543)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(544)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(545)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(546)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(547)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(548)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(549)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(550)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(551)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(552)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(553)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(554)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(555)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(556)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(557)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(558)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(559)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(560)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(561)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(562)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(563)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(564)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(565)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(566)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(567)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(568)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(569)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(570)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(571)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(572)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(573)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(574)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(575)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(576)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(577)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(578)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(579)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(580)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(581)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(582)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(583)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(584)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(585)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(586)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(587)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(588)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(589)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(590)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(591)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(592)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(593)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(594)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(595)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=dcmplx(ZERO,ZERO)
      s(4)=1
      x=w
      ix=-1
      na=4
      cres(596)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=dcmplx(ZERO,ZERO)
      s(2)=1
      a(3)=dcmplx(ZERO,ZERO)
      s(3)=1
      a(4)=dcmplx(ZERO,ZERO)
      s(4)=1
      x=z
      ix=1
      na=4
      cres(597)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end

      subroutine GPLbasis_QP(w,z,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=597)
      ! itool: 1 (FastGPL, default), 2 (handyG)
      INTEGER,INTENT(IN),OPTIONAL::itool
      INTEGER::itool_used
      INTEGER::ix
      complex*32 cres(NDIM)
      complex*32 w,z,x
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      complex*32 a(4)
      integer s(4)
      integer init,na
      DATA init/0/
      SAVE init
      IF(PRESENT(itool))THEN
         itool_used=itool
      ELSE
         itool_used=2
      ENDIF
      a(1)=-(1/w)
      s(1)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      IF(init_GPLs_qp)THEN
        cres(1)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs_qp=.FALSE.
      ELSE
        cres(1)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      ENDIF
      a(1)=1/w
      s(1)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(2)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(3)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(4)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(5)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(w/z)
      s(1)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(6)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(7)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(8)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(z/w)
      s(1)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(9)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(10)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      x=w
      ix=-1
      na=1
      cres(11)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      x=z
      ix=1
      na=1
      cres(12)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(13)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(14)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(15)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(16)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(17)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(18)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(19)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(w/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(20)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(21)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(22)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(23)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(24)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(25)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(26)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(w/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(27)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(28)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(29)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(30)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(31)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(32)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(33)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(34)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(35)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(36)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(37)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(38)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(39)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(40)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(41)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(42)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(43)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(44)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(45)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(46)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(47)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(48)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(49)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(50)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(51)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/(w*z))
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(52)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(53)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(w/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(54)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(55)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(56)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(z/w)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(57)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(58)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      x=w
      ix=-1
      na=2
      cres(59)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      x=z
      ix=1
      na=2
      cres(60)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(61)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(62)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(63)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(64)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(65)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(66)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(67)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(68)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(69)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(70)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(71)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(72)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(73)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(74)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(75)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(76)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(77)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(78)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(79)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(80)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(81)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(82)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(83)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(84)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(85)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(86)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(87)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(88)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(89)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(90)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(91)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(92)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(93)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(94)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(95)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(96)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(97)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(98)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(99)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(100)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(101)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(102)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(103)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(104)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(105)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(106)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(107)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(108)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(109)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(110)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(111)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(112)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(113)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(114)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(115)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(116)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(117)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(118)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(119)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/(w*z))
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(120)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(121)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(122)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(123)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(124)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(125)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(126)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(127)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(128)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(129)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(130)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(131)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(132)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(133)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(134)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(135)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(136)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(137)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(138)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(139)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(140)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(141)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(142)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(143)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(144)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(145)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(146)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(147)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(148)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(149)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(150)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(151)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(152)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(153)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(154)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(155)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(156)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(157)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(158)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(159)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(160)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(161)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(162)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(163)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(164)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(165)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(166)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(167)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(168)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(169)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(170)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(171)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(172)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(173)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(174)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(175)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(176)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(177)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(178)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(179)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(180)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(181)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(182)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(183)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/(w*z))
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(184)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/(w*z))
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(185)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(186)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(187)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(188)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(189)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(190)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(191)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(192)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(193)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(194)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(195)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(196)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(197)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(198)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(199)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(200)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(201)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(202)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(203)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(204)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(205)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(206)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(w/z)
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(207)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(208)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(209)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(z/w)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(210)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(211)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      x=w
      ix=-1
      na=3
      cres(212)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      x=z
      ix=1
      na=3
      cres(213)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(214)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(215)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(216)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(217)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(218)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(219)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(220)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(221)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(222)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(223)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(224)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(225)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(226)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(227)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(228)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(229)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(230)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(231)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(232)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(233)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(234)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(235)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(236)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(237)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(238)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(239)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(240)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(241)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(242)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(243)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(244)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(245)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(246)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(247)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(248)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(249)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(250)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(251)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(252)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(253)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(254)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(255)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(256)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(257)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(258)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(259)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(260)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(261)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(262)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(263)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(264)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(265)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(266)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(267)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(268)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(269)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(270)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(271)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(272)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(273)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(274)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(275)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(276)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(277)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(278)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(279)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(280)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(281)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(282)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(283)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(284)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(285)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(286)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(287)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(288)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(289)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(290)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(291)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(292)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(293)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(294)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(295)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(296)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(297)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(298)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(299)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(300)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(301)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(302)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(303)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(304)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(305)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(306)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(307)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(308)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(309)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(310)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(311)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(312)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(313)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(314)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(315)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(316)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(317)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(318)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(319)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(320)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(321)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(322)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(323)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(324)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(325)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(326)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(327)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(328)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(329)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(330)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(331)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(332)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(333)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(334)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(335)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(336)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(337)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(338)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(339)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(340)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(341)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(342)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(343)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(344)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(345)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(346)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(347)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(348)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(349)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(350)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(351)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(352)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(353)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(354)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(355)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(356)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(357)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(358)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(359)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(360)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(361)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(362)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(363)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(364)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(365)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(366)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(367)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(368)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(369)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(370)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(371)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(372)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(373)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(374)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(375)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(376)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(377)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(378)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(379)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(380)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(381)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(382)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(383)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(384)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(385)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(386)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(387)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(388)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(389)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(390)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(391)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(392)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(393)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(394)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(395)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=w/z
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(396)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(397)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(398)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(399)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(400)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(401)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(402)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(403)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(404)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(405)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(406)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(407)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(408)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(409)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(410)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(411)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(412)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(413)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(414)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(415)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(416)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(417)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(418)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(419)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(420)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(421)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(422)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(423)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(424)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(425)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(426)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(427)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(428)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(429)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(430)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(431)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(432)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(-1 + w)/((1 + w)*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(433)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(434)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(435)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(436)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(437)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(438)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(439)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(440)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(441)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(442)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(443)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(444)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(445)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(446)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(447)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(448)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(449)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(450)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(451)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(452)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(453)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(454)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(455)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(456)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(457)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(458)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(459)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(460)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(461)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(462)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(463)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(464)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(465)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(466)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(467)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(468)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(469)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=(1 + w)/(z - w*z)
      s(1)=-1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(470)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(471)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(472)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(473)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(474)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(475)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(476)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(477)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(478)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(479)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(480)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(481)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(482)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(483)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(484)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(485)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(486)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(487)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(488)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(489)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(490)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(491)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(492)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(493)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(494)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(495)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(496)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(497)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(498)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(499)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(500)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(501)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(502)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(503)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(504)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=1/(w*z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(505)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(506)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(507)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(508)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(509)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(510)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(511)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(512)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(513)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(514)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(515)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(516)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(517)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(518)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(519)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/(w*z)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(520)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(521)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(522)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(523)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(524)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(525)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(526)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(527)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(528)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(529)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(530)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(531)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(532)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=w/z
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(533)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(534)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(535)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(536)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(537)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(538)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(539)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(540)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(541)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(542)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(543)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(544)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(545)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(-1 + w)/((1 + w)*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(546)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(547)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(548)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(549)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(550)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(551)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(552)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(553)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(554)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(555)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(556)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(557)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(558)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=(1 + w)/(z - w*z)
      s(2)=-1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(559)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(560)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/w)
      s(3)=-1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(561)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(562)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/w
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(563)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(564)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(565)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(566)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(567)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1/z)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(568)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(569)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(570)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(571)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(572)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1/z
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(573)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(574)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(575)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(576)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(577)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=w/z
      s(3)=-1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(578)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(579)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(580)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(581)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(582)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(-1 + w)/((1 + w)*z)
      s(3)=-1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(583)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(584)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(585)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(586)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(587)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=(1 + w)/(z - w*z)
      s(3)=-1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(588)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/w)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(589)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/w
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(590)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1/z)
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(591)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1/z
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(592)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=w/z
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(593)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(-1 + w)/((1 + w)*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(594)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=(1 + w)/(z - w*z)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(595)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=cmplx(ZERO,ZERO,kind=16)
      s(4)=1
      x=w
      ix=-1
      na=4
      cres(596)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=cmplx(ZERO,ZERO,kind=16)
      s(4)=1
      x=z
      ix=1
      na=4
      cres(597)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end

      end module
