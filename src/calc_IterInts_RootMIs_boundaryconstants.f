      module calc_IterInts_RootMIs_boundaryconstants
      logical init_GPLs_BCs,init_GPLs_BCs_qp
      contains
      subroutine get_boundaryconstants_Phys12(xst,cres)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex G(NDIM)
      double complex cres(19)
      double precision xst
      double complex yst
      if(xst.LE.0d0)then
        write(*,*)"ERROR: xst <= 0 !"
        stop
      endif
      init_GPLs_BCs=.TRUE.
      yst=1d0-xst/2d0-sqrt(dcmplx(xst*(xst-4d0),0d0))/2d0
      call GPLbasis_BC_Phys12(yst,G,GPLs_tool)
      cres(1)=BPhys12_2A1(G)
      cres(2)=BPhys12_2A2(G)
      cres(3)=BPhys12_4A3(G)
      cres(4)=BPhys12_10A3(G)
      cres(5)=BPhys12_11A2(G)
      cres(6)=BPhys12_18A3(G)
      cres(7)=BPhys12_20A3(G)
      cres(8)=BPhys12_23A3(G)
      cres(9)=BPhys12_26A4(G)
      cres(10)=BPhys12_28A4(G)
      cres(11)=BPhys12_29A4(G)
      ! B3A2
      cres(12)=-cres(1)**2
      ! B3A3
      cres(13)=-2*cres(1)*cres(2)
      ! B4A2
      cres(14)=-cres(1)**2/2d0
      ! B5A3
      cres(15)=cres(1)**3/2d0
      ! B6A3
      cres(16)=-6*cres(3) + 3*cres(4) - 2*cres(1)*cres(5)
      ! B7A2
      cres(17)=-6*cres(2) - 4*cres(5)
      ! B8A3
      cres(18)=(6*cres(3) - 5*cres(4) + 6*cres(1)*cres(5))/4d0
      ! B9A3
      cres(19)=(2*cres(3) + 3*cres(4) - 2*cres(1)*cres(5))/4d0
      return
      end subroutine get_boundaryconstants_Phys12

      subroutine get_boundaryconstants_Phys34(xst,cres)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=16)
      double complex G(NDIM)
      double complex cres(6)
      double precision xst
      double complex yst
      if(xst.LE.0d0)then
        write(*,*)"ERROR: xst <= 0 !"
        stop
      endif
      init_GPLs_BCs=.TRUE.
      yst=1d0-xst/2d0-sqrt(dcmplx(xst*(xst-4d0),0d0))/2d0
      call GPLbasis_BC_Phys34(yst,G,GPLs_tool)
      cres(1)=BPhys34_14A3(G)
      cres(2)=BPhys34_16A2(G)
      cres(3)=BPhys34_16A3(G)
      cres(4)=BPhys34_23A3(G)
      cres(5)=BPhys34_26A4(G)
      ! B14A2
      cres(6)=-4*cres(2)
      return
      end subroutine get_boundaryconstants_Phys34

      function BPhys12_2A1(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_2A1
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_2A1=G(1)
      return
      end function BPhys12_2A1

      function BPhys12_2A2(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_2A2
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_2A2=-zeta2 - TWO*G(1)*G(2) + G(4) + TWO*G(5)
      return
      end function BPhys12_2A2

      function BPhys12_4A3(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_4A3
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_4A3=        THREE*zeta3 + G(1)*(zeta2 + TWO*G(5)) - 
     -  G(8) - FOUR*G(9)
      return
      end function BPhys12_4A3

      function BPhys12_10A3(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_10A3
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_10A3=        TWO*G(1)*(zeta2 - G(6)) + G(8) + 
     -  FOUR*(zeta3 + G(10))
      return
      end function BPhys12_10A3

      function BPhys12_11A2(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_11A2
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_11A2=zeta2 - G(1)*G(3) + HALF*G(4) + G(6)
      return
      end function BPhys12_11A2

      function BPhys12_18A3(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_18A3
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_18A3=        -(G(3)*G(4)) - G(1)*(zeta2 - TWO*G(6)) - 
     -  THREE*(zeta3 + G(10))
      return
      end function BPhys12_18A3

      function BPhys12_20A3(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_20A3
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_20A3=zeta3 + zeta2*G(1) - G(3)*G(4) + G(8) + G(10)
      return
      end function BPhys12_20A3

      function BPhys12_23A3(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_23A3
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_23A3=        -(TWO*G(3)*G(4)) + TWO*G(1)*G(6) + G(8) - 
     -  TWO*(zeta3 + G(10))
      return
      end function BPhys12_23A3

      function BPhys12_26A4(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_26A4
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_26A4=        HALF*(5*zeta4 + FOUR*zeta2*G(6) + 
     -    TWO*G(4)*(zeta2 + G(6) + FOUR*G(7)) - 
     -    6*G(3)*G(8) - 
     -    FOUR*G(1)*(zeta2*G(3) + TWO*G(11) + 
     -       G(12)) + 8*G(16) + FOUR*G(17))
      return
      end function BPhys12_26A4

      function BPhys12_28A4(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_28A4
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_28A4=        -9*zeta4 + G(4)*
     -   (-(FOUR*zeta2) + 8*G(5) + 14*G(6)) - 
     -  8*G(1)*(zeta3 + FOUR*G(9) + FOUR*G(10)) - 
     -  G(13) + 72*G(14) + 54*G(15)
      return
      end function BPhys12_28A4

      function BPhys12_29A4(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      double complex BPhys12_29A4
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys12_29A4=        -5*zeta4 - FOUR*zeta2*G(6) - 
     -  TWO*G(4)*(zeta2 + G(6) + FOUR*G(7)) + 
     -  6*G(3)*G(8) + 
     -  FOUR*G(1)*(zeta2*G(3) + TWO*G(11) + 
     -     G(12)) - 8*G(16) - FOUR*G(17)
      return
      end function BPhys12_29A4

      function BPhys34_14A3(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      double complex BPhys34_14A3
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys34_14A3=        FOUR*G(2)*G(3) - 
     -  TWO*G(1)*(zeta2 + 6*G(4) + FOUR*G(5)) + 
     -  6*(-zeta3 + G(7) + FOUR*G(8) + TWO*G(9))
      return
      end function BPhys34_14A3

      function BPhys34_16A2(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      double complex BPhys34_16A2
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys34_16A2=-(HALF*G(3))
      return
      end function BPhys34_16A2

      function BPhys34_16A3(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      double complex BPhys34_16A3
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys34_16A3=        HALF*(-zeta3 - 6*G(2)*G(3) + 
     -    G(1)*(zeta2 + 6*G(4) + 8*G(5)) - G(7) - 
     -    12*G(8) - 10*G(9))
      return
      end function BPhys34_16A3

      function BPhys34_23A3(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      double complex BPhys34_23A3
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys34_23A3=        -(TWO*G(2)*G(3)) + TWO*G(1)*G(5) + G(7) - 
     -  TWO*(zeta3 + G(9))
      return
      end function BPhys34_23A3

      function BPhys34_26A4(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      double complex BPhys34_26A4
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex G(NDIM)
      BPhys34_26A4=        -6*zeta4 + 8*G(3)*G(6) + 
     -  G(2)*(8*zeta3 - FOUR*G(7)) - 
     -  FOUR*G(1)*(zeta3 - G(9) + TWO*G(10) + 
     -     TWO*G(11)) + TWO*G(12) - 8*G(13) + 
     -  8*G(14) + 8*G(15) + 8*G(16)
      return
      end function BPhys34_26A4

      subroutine GPLbasis_BC_Phys12(yst,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=17)
      ! itool: 1 (FastGPL, default), 2 (handyG)
      INTEGER,INTENT(IN),OPTIONAL::itool
      INTEGER::itool_used
      INTEGER::ix
      double complex cres(NDIM)
      double complex yst,x
      double complex a(4)
      integer s(4)
      double precision ZERO
      parameter (ZERO=0d0)
      integer init,na
      DATA init/0/
      SAVE init
      IF(PRESENT(itool))THEN
         itool_used=itool
      ELSE
         itool_used=1
      ENDIF
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      x=yst
      ix=-1
      na=1
      IF(init_GPLs_BCs)THEN
        cres(1)=GPL(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs_BCs=.FALSE.
      ELSE
        cres(1)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      ENDIF
      a(1)=-(1/yst)
      s(1)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(2)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/yst
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(3)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      x=yst
      ix=-1
      na=2
      cres(4)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=-(1/yst)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(5)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=1/yst
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(6)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/yst
      s(1)=1
      a(2)=1/yst
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(7)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      x=yst
      ix=-1
      na=3
      cres(8)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=-(1/yst)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(9)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=1/yst
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(10)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=1/yst
      s(2)=1
      a(3)=1/yst
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(11)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/yst
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=1/yst
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(12)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      a(4)=dcmplx(0,ZERO)
      s(4)=1
      x=yst
      ix=-1
      na=4
      cres(13)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      a(4)=-(1/yst)
      s(4)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(14)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      a(4)=1/yst
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(15)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=1/yst
      s(3)=1
      a(4)=1/yst
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(16)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=1/yst
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      a(4)=1/yst
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(17)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end

      subroutine GPLbasis_BC_Phys34(yst,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=16)
      ! itool: 1 (FastGPL, default), 2 (handyG)
      INTEGER,INTENT(IN),OPTIONAL::itool
      INTEGER::itool_used
      INTEGER::ix
      double complex cres(NDIM)
      double complex yst,x
      double complex a(4)
      integer s(4)
      double precision ZERO
      parameter (ZERO=0d0)
      integer init,na
      DATA init/0/
      SAVE init
      IF(PRESENT(itool))THEN
         itool_used=itool
      ELSE
         itool_used=1
      ENDIF
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      x=yst
      ix=-1
      na=1
      IF(init_GPLs_BCs)THEN
        cres(1)=GPL(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs_BCs=.FALSE.
      ELSE
        cres(1)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      ENDIF
      a(1)=1/yst
      s(1)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=1
      cres(2)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      x=yst
      ix=-1
      na=2
      cres(3)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=-(1/yst)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(4)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=1/yst
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(5)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/yst
      s(1)=1
      a(2)=1/yst
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(6)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      x=yst
      ix=-1
      na=3
      cres(7)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=-(1/yst)
      s(3)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(8)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=1/yst
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(9)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=1/yst
      s(2)=1
      a(3)=1/yst
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(10)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/yst
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=1/yst
      s(3)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=3
      cres(11)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      a(4)=dcmplx(0,ZERO)
      s(4)=1
      x=yst
      ix=-1
      na=4
      cres(12)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      a(4)=1/yst
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(13)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=1/yst
      s(3)=1
      a(4)=1/yst
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(14)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(0,ZERO)
      s(1)=1
      a(2)=1/yst
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      a(4)=1/yst
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(15)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/yst
      s(1)=1
      a(2)=dcmplx(0,ZERO)
      s(2)=1
      a(3)=dcmplx(0,ZERO)
      s(3)=1
      a(4)=1/yst
      s(4)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=4
      cres(16)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end

      subroutine get_boundaryconstants_Phys12_QP(xst,cres)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 G(NDIM)
      complex*32 cres(19)
      real*16 xst
      complex*32 yst
      if(xst.LE.0E0_16)then
        write(*,*)"ERROR: xst <= 0 !"
        stop
      endif
      init_GPLs_BCs_qp=.TRUE.
      yst=1E0_16-xst/2E0_16-sqrt(cmplx(xst*(xst-4E0_16),0E0_16,kind=16))
     $     /2E0_16
      call GPLbasis_BC_Phys12_QP(yst,G,GPLs_qp_tool)
      cres(1)=BPhys12_2A1_QP(G)
      cres(2)=BPhys12_2A2_QP(G)
      cres(3)=BPhys12_4A3_QP(G)
      cres(4)=BPhys12_10A3_QP(G)
      cres(5)=BPhys12_11A2_QP(G)
      cres(6)=BPhys12_18A3_QP(G)
      cres(7)=BPhys12_20A3_QP(G)
      cres(8)=BPhys12_23A3_QP(G)
      cres(9)=BPhys12_26A4_QP(G)
      cres(10)=BPhys12_28A4_QP(G)
      cres(11)=BPhys12_29A4_QP(G)
      ! B3A2
      cres(12)=-cres(1)**2
      ! B3A3
      cres(13)=-2E0_16*cres(1)*cres(2)
      ! B4A2
      cres(14)=-cres(1)**2/2E0_16
      ! B5A3
      cres(15)=cres(1)**3/2E0_16
      ! B6A3
      cres(16)=-6E0_16*cres(3) + 3E0_16*cres(4) - 2E0_16*cres(1)*cres(5)
      ! B7A2
      cres(17)=-6E0_16*cres(2) - 4E0_16*cres(5)
      ! B8A3
      cres(18)=(6E0_16*cres(3) - 5E0_16*cres(4) + 
     $     6E0_16*cres(1)*cres(5))/4E0_16
      ! B9A3
      cres(19)=(2E0_16*cres(3) + 3E0_16*cres(4) - 
     $     2E0_16*cres(1)*cres(5))/4E0_16
      return
      end subroutine get_boundaryconstants_Phys12_QP

      subroutine get_boundaryconstants_Phys34_QP(xst,cres)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=16)
      complex*32 G(NDIM)
      complex*32 cres(6)
      real*16 xst
      complex*32 yst
      if(xst.LE.0E0_16)then
        write(*,*)"ERROR: xst <= 0 !"
        stop
      endif
      init_GPLs_BCs_qp=.TRUE.
      yst=1E0_16-xst/2E0_16-sqrt(cmplx(xst*(xst-4E0_16),0E0_16,kind=16))
     $     /2E0_16
      call GPLbasis_BC_Phys34_QP(yst,G,GPLs_qp_tool)
      cres(1)=BPhys34_14A3_QP(G)
      cres(2)=BPhys34_16A2_QP(G)
      cres(3)=BPhys34_16A3_QP(G)
      cres(4)=BPhys34_23A3_QP(G)
      cres(5)=BPhys34_26A4_QP(G)
      ! B14A2
      cres(6)=-4E0_16*cres(2)
      return
      end subroutine get_boundaryconstants_Phys34_QP

      function BPhys12_2A1_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_2A1_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_2A1_QP=G(1)
      return
      end function BPhys12_2A1_QP

      function BPhys12_2A2_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_2A2_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_2A2_QP=-zeta2 - TWO*G(1)*G(2) + G(4) + TWO*G(5)
      return
      end function BPhys12_2A2_QP

      function BPhys12_4A3_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_4A3_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_4A3_QP=        THREE*zeta3 + G(1)*(zeta2 + TWO*G(5)) - 
     -  G(8) - FOUR*G(9)
      return
      end function BPhys12_4A3_QP

      function BPhys12_10A3_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_10A3_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_10A3_QP=TWO*G(1)*(zeta2 - G(6)) + G(8) + 
     -  FOUR*(zeta3 + G(10))
      return
      end function BPhys12_10A3_QP

      function BPhys12_11A2_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_11A2_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_11A2_QP=zeta2 - G(1)*G(3) + HALF*G(4) + G(6)
      return
      end function BPhys12_11A2_QP

      function BPhys12_18A3_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_18A3_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_18A3_QP=-(G(3)*G(4)) - G(1)*(zeta2 - TWO*G(6)) - 
     -  THREE*(zeta3 + G(10))
      return
      end function BPhys12_18A3_QP

      function BPhys12_20A3_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_20A3_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_20A3_QP=zeta3 + zeta2*G(1) - G(3)*G(4) + G(8) + G(10)
      return
      end function BPhys12_20A3_QP

      function BPhys12_23A3_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_23A3_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_23A3_QP=-(TWO*G(3)*G(4)) + TWO*G(1)*G(6) + G(8) - 
     -  TWO*(zeta3 + G(10))
      return
      end function BPhys12_23A3_QP

      function BPhys12_26A4_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_26A4_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_26A4_QP=        HALF*(5E0_16*zeta4 + FOUR*zeta2*G(6) + 
     -    TWO*G(4)*(zeta2 + G(6) + FOUR*G(7)) - 
     -    6E0_16*G(3)*G(8) - 
     -    FOUR*G(1)*(zeta2*G(3) + TWO*G(11) + 
     -       G(12)) + 8E0_16*G(16) + FOUR*G(17))
      return
      end function BPhys12_26A4_QP

      function BPhys12_28A4_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_28A4_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_28A4_QP=        -9E0_16*zeta4 + G(4)*
     -   (-(FOUR*zeta2) + 8E0_16*G(5) + 14E0_16*G(6)) - 
     -  8E0_16*G(1)*(zeta3 + FOUR*G(9) + FOUR*G(10)) - 
     -  G(13) + 72E0_16*G(14) + 54E0_16*G(15)
      return
      end function BPhys12_28A4_QP

      function BPhys12_29A4_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=17)
      complex*32 BPhys12_29A4_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys12_29A4_QP=        -5E0_16*zeta4 - FOUR*zeta2*G(6) - 
     -  TWO*G(4)*(zeta2 + G(6) + FOUR*G(7)) + 
     -  6E0_16*G(3)*G(8) + 
     -  FOUR*G(1)*(zeta2*G(3) + TWO*G(11) + 
     -     G(12)) - 8E0_16*G(16) - FOUR*G(17)
      return
      end function BPhys12_29A4_QP

      function BPhys34_14A3_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      complex*32 BPhys34_14A3_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys34_14A3_QP=        FOUR*G(2)*G(3) - 
     -  TWO*G(1)*(zeta2 + 6E0_16*G(4) + FOUR*G(5)) + 
     -  6E0_16*(-zeta3 + G(7) + FOUR*G(8) + TWO*G(9))
      return
      end function BPhys34_14A3_QP

      function BPhys34_16A2_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      complex*32 BPhys34_16A2_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys34_16A2_QP=-(HALF*G(3))
      return
      end function BPhys34_16A2_QP

      function BPhys34_16A3_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      complex*32 BPhys34_16A3_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys34_16A3_QP=        HALF*(-zeta3 - 6E0_16*G(2)*G(3) + 
     -    G(1)*(zeta2 + 6E0_16*G(4) + 8E0_16*G(5)) - G(7) - 
     -    12E0_16*G(8) - 10E0_16*G(9))
      return
      end function BPhys34_16A3_QP

      function BPhys34_23A3_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      complex*32 BPhys34_23A3_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys34_23A3_QP=        -(TWO*G(2)*G(3)) + TWO*G(1)*G(5) + G(7) - 
     -  TWO*(zeta3 + G(9))
      return
      end function BPhys34_23A3_QP

      function BPhys34_26A4_QP(G)
      implicit none
      integer NDIM
      parameter (NDIM=16)
      complex*32 BPhys34_26A4_QP
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zeta3=
     $     1.2020569031595942853997381615114499907649862923405E0_16)
      parameter (zeta4=
     $     1.0823232337111381915160036965411679027747509519187E0_16)
      complex*32 G(NDIM)
      BPhys34_26A4_QP=-6E0_16*zeta4 + 8E0_16*G(3)*G(6) + 
     -  G(2)*(8E0_16*zeta3 - FOUR*G(7)) - 
     -  FOUR*G(1)*(zeta3 - G(9) + TWO*G(10) + 
     -     TWO*G(11)) + TWO*G(12) - 8E0_16*G(13) + 
     -  8E0_16*G(14) + 8E0_16*G(15) + 8E0_16*G(16)
      return
      end function BPhys34_26A4_QP

      subroutine GPLbasis_BC_Phys12_QP(yst,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=17)
      ! itool: 1 (FastGPL, default), 2 (handyG)
      INTEGER,INTENT(IN),OPTIONAL::itool
      INTEGER::itool_used
      INTEGER::ix
      complex*32 cres(NDIM)
      complex*32 yst,x
      complex*32 a(4)
      integer s(4)
      real*16 ZERO
      parameter (ZERO=0E0_16)
      integer init,na
      DATA init/0/
      SAVE init
      IF(PRESENT(itool))THEN
         itool_used=itool
      ELSE
         itool_used=2
      ENDIF
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      x=yst
      ix=-1
      na=1
      IF(init_GPLs_BCs_qp)THEN
        cres(1)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs_BCs_qp=.FALSE.
      ELSE
        cres(1)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      ENDIF
      a(1)=-(1E0_16/yst)
      s(1)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(2)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1E0_16/yst
      s(1)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(3)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      x=yst
      ix=-1
      na=2
      cres(4)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1E0_16/yst)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(5)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1E0_16/yst
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(6)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1E0_16/yst
      s(1)=1
      a(2)=1E0_16/yst
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(7)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      x=yst
      ix=-1
      na=3
      cres(8)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1E0_16/yst)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(9)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1E0_16/yst
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(10)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1E0_16/yst
      s(2)=1
      a(3)=1E0_16/yst
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(11)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1E0_16/yst
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1E0_16/yst
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(12)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=cmplx(ZERO,ZERO,kind=16)
      s(4)=1
      x=yst
      ix=-1
      na=4
      cres(13)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=-(1E0_16/yst)
      s(4)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(14)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1E0_16/yst
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(15)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1E0_16/yst
      s(3)=1
      a(4)=1E0_16/yst
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(16)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1E0_16/yst
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1E0_16/yst
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(17)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end

      subroutine GPLbasis_BC_Phys34_QP(yst,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=16)
      ! itool: 1 (FastGPL, default), 2 (handyG)
      INTEGER,INTENT(IN),OPTIONAL::itool
      INTEGER::itool_used
      INTEGER::ix
      complex*32 cres(NDIM)
      complex*32 yst,x
      complex*32 a(4)
      integer s(4)
      real*16 ZERO
      parameter (ZERO=0E0_16)
      integer init,na
      DATA init/0/
      SAVE init
      IF(PRESENT(itool))THEN
         itool_used=itool
      ELSE
         itool_used=2
      ENDIF
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      x=yst
      ix=-1
      na=1
      IF(init_GPLs_BCs_qp)THEN
        cres(1)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs_BCs_qp=.FALSE.
      ELSE
        cres(1)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      ENDIF
      a(1)=1E0_16/yst
      s(1)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=1
      cres(2)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      x=yst
      ix=-1
      na=2
      cres(3)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1E0_16/yst)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(4)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1E0_16/yst
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(5)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1E0_16/yst
      s(1)=1
      a(2)=1E0_16/yst
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(6)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      x=yst
      ix=-1
      na=3
      cres(7)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=-(1E0_16/yst)
      s(3)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(8)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1E0_16/yst
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(9)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1E0_16/yst
      s(2)=1
      a(3)=1E0_16/yst
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(10)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1E0_16/yst
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1E0_16/yst
      s(3)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=3
      cres(11)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=cmplx(ZERO,ZERO,kind=16)
      s(4)=1
      x=yst
      ix=-1
      na=4
      cres(12)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1E0_16/yst
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(13)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=1E0_16/yst
      s(3)=1
      a(4)=1E0_16/yst
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(14)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1E0_16/yst
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1E0_16/yst
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(15)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1E0_16/yst
      s(1)=1
      a(2)=cmplx(ZERO,ZERO,kind=16)
      s(2)=1
      a(3)=cmplx(ZERO,ZERO,kind=16)
      s(3)=1
      a(4)=1E0_16/yst
      s(4)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=4
      cres(16)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end

      end module
