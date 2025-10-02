      ! the two-loop master integrals
      ! that have NOT yet been divided by the corresponding sqrt
      ! if exists
      subroutine UTtwoloopbasis(xs,xt,loopin,cres)
      use LbL_Global
      use GPL_wrapper
      use express_wz
      use kinetics
      use calc_IterInts_GPLMIs
      use calc_IterInts_RootMIs
      implicit none
      integer NDIM
      parameter (NDIM=597)
      double complex G(NDIM)
      integer i,ioff
      ! input integrals without dividing by square roots
      double complex loopin(9)
      double complex cres(84)
      double precision xs,xt,xu
      double complex w,z
      xu=-xs-xt
      do i=1,9
        cres(i)=loopin(i)
      enddo
      ioff=9
      init_GPLs=.TRUE.
      call evaluate_wz(xs,xt,w,z)
      call GPLbasis(w,z,G,GPLs_tool)
      cres(ioff+1)=f1E3_w2(xs,xt,w,z,G)
      cres(ioff+2)=f1E5_w3(xs,xt,w,z,G)
      cres(ioff+3)=fE13_w4(xs,xt,w,z,G)
      cres(ioff+4)=fE14_w3(xs,xt,w,z,G)
      cres(ioff+5)=fE15_w2(xs,xt,w,z,G)
      cres(ioff+6)=fE17_w4(xs,xt,w,z,G)
      cres(ioff+7)=fE19_w2(xs,xt,w,z,G)
      cres(ioff+8)=fE19_w3(xs,xt,w,z,G)
      cres(ioff+9)=fE20_w3(xs,xt,w,z,G)
      cres(ioff+10)=fE21_w4(xs,xt,w,z,G)
      cres(ioff+11)=fE22_w3(xs,xt,w,z,G)
      cres(ioff+12)=fE27_w4(xs,xt,w,z,G)
      init_GPLs=.TRUE.
      call evaluate_wz(xs,xu,w,z)
      call GPLbasis(w,z,G,GPLs_tool)
      cres(ioff+13)=f1E3_w2(xs,xu,w,z,G)
      cres(ioff+14)=f1E5_w3(xs,xu,w,z,G)
      cres(ioff+15)=fE14_w3(xs,xu,w,z,G)
      cres(ioff+16)=fE15_w2(xs,xu,w,z,G)
      cres(ioff+17)=fE17_w4(xs,xu,w,z,G)
      cres(ioff+18)=fE19_w2(xs,xu,w,z,G)
      cres(ioff+19)=fE19_w3(xs,xu,w,z,G)
      cres(ioff+20)=fE20_w3(xs,xu,w,z,G)
      cres(ioff+21)=fE21_w4(xs,xu,w,z,G)
      cres(ioff+22)=fE22_w3(xs,xu,w,z,G)
      cres(ioff+23)=fE27_w4(xs,xu,w,z,G)
      init_GPLs=.TRUE.
      call evaluate_wz(xt,xs,w,z)
      call GPLbasis(w,z,G,GPLs_tool)
      cres(ioff+24)=f1E3_w2(xt,xs,w,z,G)
      cres(ioff+25)=f1E5_w3(xt,xs,w,z,G)
      cres(ioff+26)=fE13_w4(xt,xs,w,z,G)
      cres(ioff+27)=fE14_w3(xt,xs,w,z,G)
      cres(ioff+28)=fE15_w2(xt,xs,w,z,G)
      cres(ioff+29)=fE17_w4(xt,xs,w,z,G)
      cres(ioff+30)=fE19_w3(xt,xs,w,z,G)
      cres(ioff+31)=fE20_w3(xt,xs,w,z,G)
      cres(ioff+32)=fE27_w4(xt,xs,w,z,G)
      init_GPLs=.TRUE.
      call evaluate_wz(xt,xu,w,z)
      call GPLbasis(w,z,G,GPLs_tool)
      cres(ioff+33)=fE19_w2(xt,xu,w,z,G)
      cres(ioff+34)=fE19_w3(xt,xu,w,z,G)
      cres(ioff+35)=fE20_w3(xt,xu,w,z,G)
      cres(ioff+36)=fE21_w4(xt,xu,w,z,G)
      cres(ioff+37)=fE22_w3(xt,xu,w,z,G)
      cres(ioff+38)=fE27_w4(xt,xu,w,z,G)
      init_GPLs=.TRUE.
      call evaluate_wz(xu,xs,w,z)
      call GPLbasis(w,z,G,GPLs_tool)
      cres(ioff+39)=fE13_w4(xu,xs,w,z,G)
      cres(ioff+40)=fE19_w3(xu,xs,w,z,G)
      cres(ioff+41)=fE20_w3(xu,xs,w,z,G)
      cres(ioff+42)=fE27_w4(xu,xs,w,z,G)
      init_GPLs=.TRUE.
      call evaluate_wz(xu,xt,w,z)
      call GPLbasis(w,z,G,GPLs_tool)
      cres(ioff+43)=fE19_w3(xu,xt,w,z,G)
      cres(ioff+44)=fE20_w3(xu,xt,w,z,G)
      cres(ioff+45)=fE27_w4(xu,xt,w,z,G)
      ! 1: use trapezoid
      ! 2: use double exponential quadrature
      integration_method=grid_integration_method
      ! use the dynamical boundary points (not for Euclidean region)
      boundary_point_scheme=2
      npoints=1000 ! we can lower the number of points for integrals (matters only for trapezoid)
      ! xt,xu,xs
      cres(ioff+49)=fE18_w3(xt,xu)
      cres(ioff+55)=fE25_w4(xt,xu)
      cres(ioff+61)=fE26_w4(xt,xu)
      cres(ioff+67)=fE28_w4(xt,xu)
      cres(ioff+73)=fE29_w4(xt,xu)
      ! xu,xt,xs
      cres(ioff+51)=fE18_w3(xu,xt)
      cres(ioff+57)=fE25_w4(xu,xt)
      cres(ioff+63)=fE26_w4(xu,xt)
      cres(ioff+69)=fE28_w4(xu,xt)
      cres(ioff+75)=fE29_w4(xu,xt)
      ! xs,xt,xu
      cres(ioff+46)=fE18_w3(xs,xt)
      cres(ioff+52)=fE25_w4(xs,xt)
      cres(ioff+58)=fE26_w4(xs,xt)
      cres(ioff+64)=fE28_w4(xs,xt)
      cres(ioff+70)=fE29_w4(xs,xt)
      ! xs,xu,xt
      cres(ioff+47)=fE18_w3(xs,xu)
      cres(ioff+53)=fE25_w4(xs,xu)
      cres(ioff+59)=fE26_w4(xs,xu)
      cres(ioff+65)=fE28_w4(xs,xu)
      cres(ioff+71)=fE29_w4(xs,xu)
      ! xt,xs,xu
      cres(ioff+48)=fE18_w3(xt,xs)
      cres(ioff+54)=fE25_w4(xt,xs)
      cres(ioff+60)=fE26_w4(xt,xs)
      cres(ioff+66)=fE28_w4(xt,xs)
      cres(ioff+72)=fE29_w4(xt,xs)
      ! xu,xs,xt
      cres(ioff+50)=fE18_w3(xu,xs)
      cres(ioff+56)=fE25_w4(xu,xs)
      cres(ioff+62)=fE26_w4(xu,xs)
      cres(ioff+68)=fE28_w4(xu,xs)
      cres(ioff+74)=fE29_w4(xu,xs)
      return
      end subroutine UTtwoloopbasis

      ! the two-loop master integrals
      ! that have NOT yet been divided by the corresponding sqrt
      ! if exists
      subroutine UTtwoloopbasis_QP(xs,xt,loopin,cres)
      use GPL_wrapper
      use express_wz
      use kinetics
      use calc_IterInts_GPLMIs
      use calc_IterInts_RootMIs
      implicit none
      integer NDIM
      parameter (NDIM=597)
      complex*32 G(NDIM)
      integer i,ioff
      ! input integrals without dividing by square roots
      complex*32 loopin(9)
      complex*32 cres(84)
      real*16 xs,xt,xu
      complex*32 w,z
      xu=-xs-xt
      do i=1,9
        cres(i)=loopin(i)
      enddo
      ioff=9
      init_GPLs_qp=.TRUE.
      call evaluate_wz_qp(xs,xt,w,z)
      call GPLbasis_QP(w,z,G,GPLs_qp_tool)
      cres(ioff+1)=f1E3_w2_qp(xs,xt,w,z,G)
      cres(ioff+2)=f1E5_w3_qp(xs,xt,w,z,G)
      cres(ioff+3)=fE13_w4_qp(xs,xt,w,z,G)
      cres(ioff+4)=fE14_w3_qp(xs,xt,w,z,G)
      cres(ioff+5)=fE15_w2_qp(xs,xt,w,z,G)
      cres(ioff+6)=fE17_w4_qp(xs,xt,w,z,G)
      cres(ioff+7)=fE19_w2_qp(xs,xt,w,z,G)
      cres(ioff+8)=fE19_w3_qp(xs,xt,w,z,G)
      cres(ioff+9)=fE20_w3_qp(xs,xt,w,z,G)
      cres(ioff+10)=fE21_w4_qp(xs,xt,w,z,G)
      cres(ioff+11)=fE22_w3_qp(xs,xt,w,z,G)
      cres(ioff+12)=fE27_w4_qp(xs,xt,w,z,G)
      init_GPLs_qp=.TRUE.
      call evaluate_wz_qp(xs,xu,w,z)
      call GPLbasis_QP(w,z,G,GPLs_qp_tool)
      cres(ioff+13)=f1E3_w2_qp(xs,xu,w,z,G)
      cres(ioff+14)=f1E5_w3_qp(xs,xu,w,z,G)
      cres(ioff+15)=fE14_w3_qp(xs,xu,w,z,G)
      cres(ioff+16)=fE15_w2_qp(xs,xu,w,z,G)
      cres(ioff+17)=fE17_w4_qp(xs,xu,w,z,G)
      cres(ioff+18)=fE19_w2_qp(xs,xu,w,z,G)
      cres(ioff+19)=fE19_w3_qp(xs,xu,w,z,G)
      cres(ioff+20)=fE20_w3_qp(xs,xu,w,z,G)
      cres(ioff+21)=fE21_w4_qp(xs,xu,w,z,G)
      cres(ioff+22)=fE22_w3_qp(xs,xu,w,z,G)
      cres(ioff+23)=fE27_w4_qp(xs,xu,w,z,G)
      init_GPLs_qp=.TRUE.
      call evaluate_wz_qp(xt,xs,w,z)
      call GPLbasis_QP(w,z,G,GPLs_qp_tool)
      cres(ioff+24)=f1E3_w2_qp(xt,xs,w,z,G)
      cres(ioff+25)=f1E5_w3_qp(xt,xs,w,z,G)
      cres(ioff+26)=fE13_w4_qp(xt,xs,w,z,G)
      cres(ioff+27)=fE14_w3_qp(xt,xs,w,z,G)
      cres(ioff+28)=fE15_w2_qp(xt,xs,w,z,G)
      cres(ioff+29)=fE17_w4_qp(xt,xs,w,z,G)
      cres(ioff+30)=fE19_w3_qp(xt,xs,w,z,G)
      cres(ioff+31)=fE20_w3_qp(xt,xs,w,z,G)
      cres(ioff+32)=fE27_w4_qp(xt,xs,w,z,G)
      init_GPLs_qp=.TRUE.
      call evaluate_wz_qp(xt,xu,w,z)
      call GPLbasis_QP(w,z,G,GPLs_qp_tool)
      cres(ioff+33)=fE19_w2_qp(xt,xu,w,z,G)
      cres(ioff+34)=fE19_w3_qp(xt,xu,w,z,G)
      cres(ioff+35)=fE20_w3_qp(xt,xu,w,z,G)
      cres(ioff+36)=fE21_w4_qp(xt,xu,w,z,G)
      cres(ioff+37)=fE22_w3_qp(xt,xu,w,z,G)
      cres(ioff+38)=fE27_w4_qp(xt,xu,w,z,G)
      init_GPLs_qp=.TRUE.
      call evaluate_wz_qp(xu,xs,w,z)
      call GPLbasis_QP(w,z,G,GPLs_qp_tool)
      cres(ioff+39)=fE13_w4_qp(xu,xs,w,z,G)
      cres(ioff+40)=fE19_w3_qp(xu,xs,w,z,G)
      cres(ioff+41)=fE20_w3_qp(xu,xs,w,z,G)
      cres(ioff+42)=fE27_w4_qp(xu,xs,w,z,G)
      init_GPLs_qp=.TRUE.
      call evaluate_wz_qp(xu,xt,w,z)
      call GPLbasis_QP(w,z,G,GPLs_qp_tool)
      cres(ioff+43)=fE19_w3_qp(xu,xt,w,z,G)
      cres(ioff+44)=fE20_w3_qp(xu,xt,w,z,G)
      cres(ioff+45)=fE27_w4_qp(xu,xt,w,z,G)
      ! use the dynamical boundary points (not for Euclidean region)
      boundary_point_scheme=2
      npoints=1000 ! we can lower the number of points for integrals
      ! xt,xu,xs
      cres(ioff+49)=fE18_w3_qp(xt,xu)
      cres(ioff+55)=fE25_w4_qp(xt,xu)
      cres(ioff+61)=fE26_w4_qp(xt,xu)
      cres(ioff+67)=fE28_w4_qp(xt,xu)
      cres(ioff+73)=fE29_w4_qp(xt,xu)
      ! xu,xt,xs
      cres(ioff+51)=fE18_w3_qp(xu,xt)
      cres(ioff+57)=fE25_w4_qp(xu,xt)
      cres(ioff+63)=fE26_w4_qp(xu,xt)
      cres(ioff+69)=fE28_w4_qp(xu,xt)
      cres(ioff+75)=fE29_w4_qp(xu,xt)
      ! xs,xt,xu
      cres(ioff+46)=fE18_w3_qp(xs,xt)
      cres(ioff+52)=fE25_w4_qp(xs,xt)
      cres(ioff+58)=fE26_w4_qp(xs,xt)
      cres(ioff+64)=fE28_w4_qp(xs,xt)
      cres(ioff+70)=fE29_w4_qp(xs,xt)
      ! xs,xu,xt
      cres(ioff+47)=fE18_w3_qp(xs,xu)
      cres(ioff+53)=fE25_w4_qp(xs,xu)
      cres(ioff+59)=fE26_w4_qp(xs,xu)
      cres(ioff+65)=fE28_w4_qp(xs,xu)
      cres(ioff+71)=fE29_w4_qp(xs,xu)
      ! xt,xs,xu
      cres(ioff+48)=fE18_w3_qp(xt,xs)
      cres(ioff+54)=fE25_w4_qp(xt,xs)
      cres(ioff+60)=fE26_w4_qp(xt,xs)
      cres(ioff+66)=fE28_w4_qp(xt,xs)
      cres(ioff+72)=fE29_w4_qp(xt,xs)
      ! xu,xs,xt
      cres(ioff+50)=fE18_w3_qp(xu,xs)
      cres(ioff+56)=fE25_w4_qp(xu,xs)
      cres(ioff+62)=fE26_w4_qp(xu,xs)
      cres(ioff+68)=fE28_w4_qp(xu,xs)
      cres(ioff+74)=fE29_w4_qp(xu,xs)
      return
      end subroutine UTtwoloopbasis_QP
