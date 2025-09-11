      module UToneloopbasis
      implicit none
      logical init_GPLs_1L_LO,init_GPLs_1L_LO_qp
      contains

      ! let us multiply back the square roots
      subroutine UToneloopbasis_nosqrt(xs,xt,loopba,cres)
      use kinetics
      implicit none
      integer NDIM
      parameter (NDIM=9)
      ! loopba is the integrals divided by square roots
      ! cres is the integrals without square roots
      double complex cres(NDIM),loopba(NDIM)
      double precision xs,xt,xu
      xu=-xs-xt
      cres(1)=loopba(1)*sqrt1(xs)
      cres(2)=loopba(2)*sqrt1(xt)
      cres(3)=loopba(3)*sqrt1(xu)
      cres(4)=loopba(4)
      cres(5)=loopba(5)
      cres(6)=loopba(6)
      cres(7)=loopba(7)*sqrt3(xs,xt)
      cres(8)=loopba(8)*sqrt3(xt,xu)
      cres(9)=loopba(9)*sqrt3(xu,xs)
      return
      end

      ! this we have divided by square roots
      subroutine UToneloopbasis_scalar(xs,xt,logm2omu2,loopba,cres)
      implicit none
      integer NDIM
      parameter (NDIM=9)
      double complex cres(NDIM),loopba(NDIM)
      double precision xs,xt,xu,logm2omu2
      xu=-xs-xt
      cres(1)=(-2d0+loopba(1)+logm2omu2)/(xs-4d0)
      cres(2)=(-2d0+loopba(2)+logm2omu2)/(xt-4d0)
      cres(3)=(-2d0+loopba(3)+logm2omu2)/(xu-4d0)
      cres(4)=xs*loopba(4)
      cres(5)=xt*loopba(5)
      cres(6)=xu*loopba(6)
      cres(7)=0.5d0*loopba(7)
      cres(8)=0.5d0*loopba(9)
      cres(9)=0.5d0*loopba(8)
      return
      end

      ! this we have divided by square roots
      subroutine UToneloopbasis_G(xs,xt,cres)
      use GPL_wrapper
      use express_wz
      use kinetics
      implicit none
      integer GNDIM
      parameter (GNDIM=18)
      double complex G(GNDIM)
      integer NDIM
      parameter (NDIM=9)
      double complex cres(NDIM)
      double precision xs,xt,xu
      double complex w,z,fE
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      integer init
      DATA init/0/
      SAVE init
      xu=-xs-xt
      !GPLs_tool=1
      init_GPLs_1L_LO=.TRUE.
      call evaluate_wz(xs,xt,w,z)
      CALL GPLbasis1L_LO(w,z,G,GPLs_tool)
      fE=f1E2Euclid_w1_LO(w,z,G)
      cres(1)=fE/sqrt1(xs)
      if(xs.gt.FOUR)then
         cres(1)=-cres(1)
      endif
      fE=f1E4Euclid_w2_LO(w,z,G)
      cres(4)=fE
      fE=f1E6Euclid_w2_LO(w,z,G)
      cres(7)=-fE/sqrt3(xs,xt)
      init_GPLs_1L_LO=.TRUE.
      call evaluate_wz(xt,xu,w,z)
      CALL GPLbasis1L_LO(w,z,G,GPLs_tool)
      fE=f1E2Euclid_w1_LO(w,z,G)
      cres(2)=fE/sqrt1(xt)
      fE=f1E4Euclid_w2_LO(w,z,G)
      cres(5)=fE
      fE=f1E6Euclid_w2_LO(w,z,G)
      cres(8)=fE/sqrt3(xt,xu)
      init_GPLs_1L_LO=.TRUE.
      call evaluate_wz(xu,xs,w,z)
      CALL GPLbasis1L_LO(w,z,G,GPLs_tool)
      fE=f1E2Euclid_w1_LO(w,z,G)
      cres(3)=fE/sqrt1(xu)
      fE=f1E4Euclid_w2_LO(w,z,G)
      cres(6)=fE
      fE=f1E6Euclid_w2_LO(w,z,G)
      cres(9)=-fE/sqrt3(xu,xs)
      return
      end

      function f1E2Euclid_w1_LO(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=18)
      double complex f1E2Euclid_w1_LO
      double complex w,z
      double complex G(NDIM)
      f1E2Euclid_w1_LO=-G(1) + G(2) + G(3) - G(4)
      return
      end function f1E2Euclid_w1_LO

      function f1E4Euclid_w2_LO(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=18)
      double complex f1E4Euclid_w2_LO
      double complex w,z
      double complex G(NDIM)
      f1E4Euclid_w2_LO= -(G(1)*G(3)) + G(2)*G(3) + G(1)*G(4) - 
     -  G(2)*G(4) + G(7) - G(8) - G(9) + G(10) + 
     -  G(11) - G(12) - G(13) + G(14)
      return
      end function f1E4Euclid_w2_LO

      function f1E6Euclid_w2_LO(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=18)
      double complex f1E6Euclid_w2_LO
      double complex w,z
      double complex G(NDIM)
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      f1E6Euclid_w2_LO=-(G(1)*G(5)) + G(2)*G(5) - G(3)*G(5) + 
     -  G(4)*G(5) + G(1)*G(6) - G(2)*G(6) + 
     -  G(3)*G(6) - G(4)*G(6) + TWO*G(15) - 
     -  TWO*G(16) - TWO*G(17) + TWO*G(18)
      return
      end function f1E6Euclid_w2_LO

      subroutine GPLbasis1L_LO(w,z,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=18)
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
      double complex a(2)
      integer s(2)
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
      IF(init_GPLs_1L_LO)THEN
        cres(1)=GPL(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs_1L_LO=.FALSE.
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
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      x=w
      ix=-1
      na=1
      cres(5)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      x=z
      ix=1
      na=1
      cres(6)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(7)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(8)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(9)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(10)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(11)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(12)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(13)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(14)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(15)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(16)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(17)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=dcmplx(ZERO,ZERO)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=dcmplx(1,ZERO)
      ix=1
      na=2
      cres(18)=GPL(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end

      ! let us multiply back the square roots
      subroutine UToneloopbasis_nosqrt_qp(xs,xt,loopba,cres)
      use kinetics
      implicit none
      integer NDIM
      parameter (NDIM=9)
      ! loopba is the integrals divided by square roots
      ! cres is the integrals without square roots
      complex*32 cres(NDIM),loopba(NDIM)
      real*16 xs,xt,xu
      xu=-xs-xt
      cres(1)=loopba(1)*sqrt1_QP(xs)
      cres(2)=loopba(2)*sqrt1_QP(xt)
      cres(3)=loopba(3)*sqrt1_QP(xu)
      cres(4)=loopba(4)
      cres(5)=loopba(5)
      cres(6)=loopba(6)
      cres(7)=loopba(7)*sqrt3_QP(xs,xt)
      cres(8)=loopba(8)*sqrt3_QP(xt,xu)
      cres(9)=loopba(9)*sqrt3_QP(xu,xs)
      return
      end

      ! this we have divided by square roots
      subroutine UToneloopbasis_scalar_qp(xs,xt,logm2omu2,loopba,cres)
      implicit none
      integer NDIM
      parameter (NDIM=9)
      complex*32 cres(NDIM),loopba(NDIM)
      real*16 xs,xt,xu,logm2omu2
      xu=-xs-xt
      cres(1)=(-2E0_16+loopba(1)+logm2omu2)/(xs-4E0_16)
      cres(2)=(-2E0_16+loopba(2)+logm2omu2)/(xt-4E0_16)
      cres(3)=(-2E0_16+loopba(3)+logm2omu2)/(xu-4E0_16)
      cres(4)=xs*loopba(4)
      cres(5)=xt*loopba(5)
      cres(6)=xu*loopba(6)
      cres(7)=0.5E0_16*loopba(7)
      cres(8)=0.5E0_16*loopba(9)
      cres(9)=0.5E0_16*loopba(8)
      return
      end

      ! this we have divided by square roots
      subroutine UToneloopbasis_G_qp(xs,xt,cres)
      use GPL_wrapper
      use express_wz
      use kinetics
      implicit none
      integer GNDIM
      parameter (GNDIM=18)
      complex*32 G(GNDIM)
      integer NDIM
      parameter (NDIM=9)
      complex*32 cres(NDIM)
      real*16 xs,xt,xu
      complex*32 w,z,fE
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      integer init
      DATA init/0/
      SAVE init
      xu=-xs-xt
      !GPLs_tool=1
      init_GPLs_1L_LO_qp=.TRUE.
      call evaluate_wz_qp(xs,xt,w,z)
      CALL GPLbasis1L_LO_qp(w,z,G,GPLs_tool)
      fE=f1E2Euclid_w1_LO_qp(w,z,G)
      cres(1)=fE/sqrt1_QP(xs)
      if(xs.gt.FOUR)then
         cres(1)=-cres(1)
      endif
      fE=f1E4Euclid_w2_LO_qp(w,z,G)
      cres(4)=fE
      fE=f1E6Euclid_w2_LO_qp(w,z,G)
      cres(7)=-fE/sqrt3_QP(xs,xt)
      init_GPLs_1L_LO_qp=.TRUE.
      call evaluate_wz_qp(xt,xu,w,z)
      CALL GPLbasis1L_LO_qp(w,z,G,GPLs_tool)
      fE=f1E2Euclid_w1_LO_qp(w,z,G)
      cres(2)=fE/sqrt1_QP(xt)
      fE=f1E4Euclid_w2_LO_qp(w,z,G)
      cres(5)=fE
      fE=f1E6Euclid_w2_LO_qp(w,z,G)
      cres(8)=fE/sqrt3_QP(xt,xu)
      init_GPLs_1L_LO_qp=.TRUE.
      call evaluate_wz_qp(xu,xs,w,z)
      CALL GPLbasis1L_LO_qp(w,z,G,GPLs_tool)
      fE=f1E2Euclid_w1_LO_qp(w,z,G)
      cres(3)=fE/sqrt1_QP(xu)
      fE=f1E4Euclid_w2_LO_qp(w,z,G)
      cres(6)=fE
      fE=f1E6Euclid_w2_LO_qp(w,z,G)
      cres(9)=-fE/sqrt3_QP(xu,xs)
      return
      end

      function f1E2Euclid_w1_LO_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=18)
      complex*32 f1E2Euclid_w1_LO_qp
      complex*32 w,z
      complex*32 G(NDIM)
      f1E2Euclid_w1_LO_qp=-G(1) + G(2) + G(3) - G(4)
      return
      end function f1E2Euclid_w1_LO_qp

      function f1E4Euclid_w2_LO_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=18)
      complex*32 f1E4Euclid_w2_LO_qp
      complex*32 w,z
      complex*32 G(NDIM)
      f1E4Euclid_w2_LO_qp= -(G(1)*G(3)) + G(2)*G(3) + G(1)*G(4) - 
     -  G(2)*G(4) + G(7) - G(8) - G(9) + G(10) + 
     -  G(11) - G(12) - G(13) + G(14)
      return
      end function f1E4Euclid_w2_LO_qp

      function f1E6Euclid_w2_LO_qp(w,z,G)
      implicit none
      integer NDIM
      parameter (NDIM=18)
      complex*32 f1E6Euclid_w2_LO_qp
      complex*32 w,z
      complex*32 G(NDIM)
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      f1E6Euclid_w2_LO_qp=-(G(1)*G(5)) + G(2)*G(5) - G(3)*G(5) + 
     -  G(4)*G(5) + G(1)*G(6) - G(2)*G(6) + 
     -  G(3)*G(6) - G(4)*G(6) + TWO*G(15) - 
     -  TWO*G(16) - TWO*G(17) + TWO*G(18)
      return
      end function f1E6Euclid_w2_LO_qp

      subroutine GPLbasis1L_LO_qp(w,z,cres,itool)
      use GPL_wrapper
      implicit none
      integer NDIM
      parameter (NDIM=18)
      ! itool: 1 (FastGPL, default), 2 (handyG)
      INTEGER,INTENT(IN),OPTIONAL::itool
      INTEGER::itool_used
      INTEGER::ix
      complex*32 cres(NDIM)
      complex*32 w,z,x
      real*16 zz0
      parameter (zz0=0.5E0_16)
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      complex*32 a(2)
      integer s(2)
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
      IF(init_GPLs_1L_LO_qp)THEN
        cres(1)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.TRUE.)
        init_GPLs_1L_LO_qp=.FALSE.
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
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      x=w
      ix=-1
      na=1
      cres(5)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      x=z
      ix=1
      na=1
      cres(6)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=-(1/w)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(7)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/w)
      s(1)=-1
      a(2)=1/w
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(8)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(9)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/w
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(10)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(11)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=-(1/z)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(12)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(13)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=1/z
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(14)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/w)
      s(2)=-1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(15)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/w
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(16)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=-(1/z)
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(17)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      a(1)=cmplx(ZERO,ZERO,kind=16)
      s(1)=1
      a(2)=1/z
      s(2)=1
      x=cmplx(1E0_16,ZERO,kind=16)
      ix=1
      na=2
      cres(18)=GPL_QP(a(1:na),s(1:na),x,ix,itool_used,.FALSE.)
      return
      end

      end module
