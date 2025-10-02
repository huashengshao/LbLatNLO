      module calc_IterInts_RootMIs
      ! integration method:
      ! 1: trapezoid
      ! 2: double exponential quadrature
      integer integration_method
      data integration_method/1/
      ! 1: use the default fixed boundary points
      ! 2: use the dynamical boundary points (not for Euclidean region)
      integer boundary_point_scheme
      data boundary_point_scheme /1/
      ! number of points for numerical integrations (trapezoid)
      integer npoints
      data npoints/5000/ ! can lower npoints to 500 when using boundary_point_scheme=2
      ! relative error for numerical integrations (double exponential quadrature)
      ! double precision
      integer DEQ_init
      data DEQ_init/0/
      save DEQ_init
      double precision DEQ_eps
      data DEQ_eps/1d-15/
      integer lenaw
      parameter (lenaw=8000)
      double precision tiny
      parameter (tiny=1.0d-307)
      double precision aw(0:lenaw-1)
      save aw
      ! quadruple precision
      integer DEQ_init_qp
      data DEQ_init_qp/0/
      save DEQ_init_qp
      real*16 DEQ_eps_qp
      data DEQ_eps_qp/1E-31_16/
      integer lenaw_qp
      parameter (lenaw_qp=16000)
      real*16 tiny_qp
      parameter (tiny_qp=1.0E-4931_16)
      real*16 aw_qp(0:lenaw_qp-1)
      save aw_qp
      ! boundary points
      ! Euclid: xsb<0 xtb<0
      ! Phys1: xsb>4 xtb<0
      ! Phys2: 0<xsb<4 xtb<0
      ! Phys3: xsb<0 xtb>4
      ! Phys4: xsb<0 0<xtb<4
      double precision xsb0_Euclid,xtb0_Euclid
      data xsb0_Euclid/0d0/
      data xtb0_Euclid/0d0/
      double precision xsb0_Phys1,xtb0_Phys1
      data xsb0_Phys1/4.1d0/
      data xtb0_Phys1/0d0/
      double precision xsb0_Phys2,xtb0_Phys2
      data xsb0_Phys2/2.1d0/
      data xtb0_Phys2/0d0/
      double precision xsb0_Phys3,xtb0_Phys3
      data xsb0_Phys3/0d0/
      data xtb0_Phys3/4.1d0/
      double precision xsb0_Phys4,xtb0_Phys4
      data xsb0_Phys4/0d0/
      data xtb0_Phys4/2.1d0/
      ! quadruple precision
      real*16 xsb0_Euclid_qp,xtb0_Euclid_qp
      data xsb0_Euclid_qp/0E0_16/
      data xtb0_Euclid_qp/0E0_16/
      real*16 xsb0_Phys1_qp,xtb0_Phys1_qp
      data xsb0_Phys1_qp/4.1E0_16/
      data xtb0_Phys1_qp/0E0_16/
      real*16 xsb0_Phys2_qp,xtb0_Phys2_qp
      data xsb0_Phys2_qp/2.1E0_16/
      data xtb0_Phys2_qp/0E0_16/
      real*16 xsb0_Phys3_qp,xtb0_Phys3_qp
      data xsb0_Phys3_qp/0E0_16/
      data xtb0_Phys3_qp/4.1E0_16/
      real*16 xsb0_Phys4_qp,xtb0_Phys4_qp
      data xsb0_Phys4_qp/0E0_16/
      data xtb0_Phys4_qp/2.1E0_16/
      ! boundary constants
      double complex B2A2_Phys1, B3A2_Phys1, B3A3_Phys1
      double complex B4A2_Phys1, B4A3_Phys1, B5A3_Phys1
      double complex B6A3_Phys1, B7A2_Phys1, B8A3_Phys1
      double complex B9A3_Phys1, B10A3_Phys1, B11A2_Phys1
      double complex B18A3_Phys1, B20A3_Phys1, B23A3_Phys1
      double complex B26A4_Phys1, B28A4_Phys1, B29A4_Phys1
      data B2A2_Phys1 /(9.2380214124483022874d0,
     -     -7.2337844124154648125d0)/
      data B3A2_Phys1 /(9.7704267987673657951d0,
     -     1.9787306035604043949d0)/
      data B3A3_Phys1 /(39.632644645760733688d0,
     -     62.600395996914881446d0)/
      data B4A2_Phys1 /(4.8852133993836828976d0,
     -     0.9893653017802021975d0)/
      data B4A3_Phys1 /(5.4055296842814968642d0,
     -     4.26227427265167735d0)/
      data B5A3_Phys1 /(-4.646657404548094611d0,
     -     15.035774899866776076d0)/
      data B6A3_Phys1 /(-21.115460572443021688d0,
     -     -22.846775675910579278d0)/
      data B7A2_Phys1 /(-55.417768578725626448d0,
     -     34.537213852654680469d0)/
      data B8A3_Phys1 /(8.0339962048207877497d0,
     -     5.7279672055041674672d0)/
      data B9A3_Phys1 /(5.5321942254522383065d0,
     -     2.8128546263257098805d0)/
      data B10A3_Phys1 /(8.4139898283330120766d0,
     -     1.3797082665262650588d0)/
      data B11A2_Phys1 /(-0.0025899739910468191d0,
     -     2.2163731554595271015d0)/
      data B18A3_Phys1 /(-2.8294293833114898744d0,
     -     -0.6817174899998712055d0)/
      data B20A3_Phys1 /(5.5845604450215222022d0,
     -     0.6979907765263938532d0)/
      data B23A3_Phys1 /(2.7551310617100323278d0,
     -     0.0162732865265226477d0)/
      data B26A4_Phys1 /(-10.6339198205641588649d0,
     -     -2.4281110122251515238d0)/
      data B28A4_Phys1 /(3.5385228974299711735d0,
     -     12.0883070153949520565d0)/
      data B29A4_Phys1 /(21.26783964112831773d0,
     -     4.856222024450303048d0)/
      double complex B2A2_Phys2, B3A2_Phys2, B3A3_Phys2
      double complex B4A2_Phys2, B4A3_Phys2, B5A3_Phys2
      double complex B6A3_Phys2, B7A2_Phys2, B8A3_Phys2
      double complex B9A3_Phys2, B10A3_Phys2, B11A2_Phys2
      double complex B18A3_Phys2, B20A3_Phys2, B23A3_Phys2
      double complex B26A4_Phys2, B28A4_Phys2, B29A4_Phys2
      data B2A2_Phys2 /(0d0,
     -     -0.82500302749446986929d0)/
      data B3A2_Phys2 /(2.6270483426551970923d0,
     -     0d0)/
      data B3A3_Phys2 /(2.6743581669712199862d0,
     -     0d0)/
      data B4A2_Phys2 /(1.3135241713275985462d0,
     -     0d0)/
      data B4A3_Phys2 /(0.31700070280911543096d0,
     -     0d0)/
      data B5A3_Phys2 /(0d0,
     -     2.1289825479625977883d0)/
      data B6A3_Phys2 /(0.82623119449144494083d0,
     -     0d0)/
      data B7A2_Phys2 /(0d0,
     -     -1.0471292962363372515d0)/
      data B8A3_Phys2 /(0.95878290910364721611d0,
     -     0d0)/
      data B9A3_Phys2 /(0.84055920424109209713d0,
     -     0d0)/
      data B10A3_Phys2 /(2.5294584133995772146d0,
     -     0d0)/
      data B11A2_Phys2 /(0d0,
     -     1.4992868653007891168d0)/
      data B18A3_Phys2 /(-0.68205885283653438165d0,
     -     0d0)/
      data B20A3_Phys2 /(1.847399560563042833d0,
     -     0d0)/
      data B23A3_Phys2 /(1.1653407077265084513d0,
     -     0d0)/
      data B26A4_Phys2 /(-2.6791975416289301837d0,
     -     0d0)/
      data B28A4_Phys2 /(0d0,
     -     2.0284487302266188385d0)/
      data B29A4_Phys2 /(5.3583950832578603674d0,
     -     0d0)/
      double complex B14A2_Phys3, B14A3_Phys3, B16A2_Phys3
      double complex B16A3_Phys3, B23A3_Phys3, B26A4_Phys3
      data B14A2_Phys3 /(-9.7704267987673657951d0,
     -     -1.9787306035604043949d0)/
      data B14A3_Phys3 /(-21.115460572443021688d0,
     -     -22.846775675910579278d0)/
      data B16A2_Phys3 /(2.4426066996918414488d0,
     -     0.4946826508901010987d0)/
      data B16A3_Phys3 /(8.0339962048207877497d0,
     -     5.7279672055041674672d0)/
      data B23A3_Phys3 /(2.7551310617100323278d0,
     -     0.0162732865265226477d0)/
      data B26A4_Phys3 /(-4.6070666045155698136d0,
     -     -0.0003213506334210942d0)/
      double complex B14A2_Phys4, B14A3_Phys4, B16A2_Phys4
      double complex B16A3_Phys4, B23A3_Phys4, B26A4_Phys4
      data B14A2_Phys4 /(-2.6270483426551970923d0,
     -     0d0)/
      data B14A3_Phys4 /(0.82623119449144494083d0,
     -     0d0)/
      data B16A2_Phys4 /(0.65676208566379927308d0,
     -     0d0)/
      data B16A3_Phys4 /(0.95878290910364721611d0,
     -     0d0)/
      data B23A3_Phys4 /(1.1653407077265084513d0,
     -     0d0)/
      data B26A4_Phys4 /(-2.2062067042935499415d0,
     -     0d0)/
      ! quadruple precision
      complex*32 B2A2_Phys1_qp, B3A2_Phys1_qp, B3A3_Phys1_qp
      complex*32 B4A2_Phys1_qp, B4A3_Phys1_qp, B5A3_Phys1_qp
      complex*32 B6A3_Phys1_qp, B7A2_Phys1_qp, B8A3_Phys1_qp
      complex*32 B9A3_Phys1_qp, B10A3_Phys1_qp, B11A2_Phys1_qp
      complex*32 B18A3_Phys1_qp, B20A3_Phys1_qp, B23A3_Phys1_qp
      complex*32 B26A4_Phys1_qp, B28A4_Phys1_qp, B29A4_Phys1_qp
      data B2A2_Phys1_qp /(9.2380214124483022874E0_16,
     -     -7.2337844124154648125E0_16)/
      data B3A2_Phys1_qp /(9.7704267987673657951E0_16,
     -     1.9787306035604043949E0_16)/
      data B3A3_Phys1_qp /(39.632644645760733688E0_16,
     -     62.600395996914881446E0_16)/
      data B4A2_Phys1_qp /(4.8852133993836828976E0_16,
     -     0.9893653017802021975E0_16)/
      data B4A3_Phys1_qp /(5.4055296842814968642E0_16,
     -     4.26227427265167735E0_16)/
      data B5A3_Phys1_qp /(-4.646657404548094611E0_16,
     -     15.035774899866776076E0_16)/
      data B6A3_Phys1_qp /(-21.115460572443021688E0_16,
     -     -22.846775675910579278E0_16)/
      data B7A2_Phys1_qp /(-55.417768578725626448E0_16,
     -     34.537213852654680469E0_16)/
      data B8A3_Phys1_qp /(8.0339962048207877497E0_16,
     -     5.7279672055041674672E0_16)/
      data B9A3_Phys1_qp /(5.5321942254522383065E0_16,
     -     2.8128546263257098805E0_16)/
      data B10A3_Phys1_qp /(8.4139898283330120766E0_16,
     -     1.3797082665262650588E0_16)/
      data B11A2_Phys1_qp /(-0.0025899739910468191E0_16,
     -     2.2163731554595271015E0_16)/
      data B18A3_Phys1_qp /(-2.8294293833114898744E0_16,
     -     -0.6817174899998712055E0_16)/
      data B20A3_Phys1_qp /(5.5845604450215222022E0_16,
     -     0.6979907765263938532E0_16)/
      data B23A3_Phys1_qp /(2.7551310617100323278E0_16,
     -     0.0162732865265226477E0_16)/
      data B26A4_Phys1_qp /(-10.6339198205641588649E0_16,
     -     -2.4281110122251515238E0_16)/
      data B28A4_Phys1_qp /(3.5385228974299711735E0_16,
     -     12.0883070153949520565E0_16)/
      data B29A4_Phys1_qp /(21.26783964112831773E0_16,
     -     4.856222024450303048E0_16)/
      complex*32 B2A2_Phys2_qp, B3A2_Phys2_qp, B3A3_Phys2_qp
      complex*32 B4A2_Phys2_qp, B4A3_Phys2_qp, B5A3_Phys2_qp
      complex*32 B6A3_Phys2_qp, B7A2_Phys2_qp, B8A3_Phys2_qp
      complex*32 B9A3_Phys2_qp, B10A3_Phys2_qp, B11A2_Phys2_qp
      complex*32 B18A3_Phys2_qp, B20A3_Phys2_qp, B23A3_Phys2_qp
      complex*32 B26A4_Phys2_qp, B28A4_Phys2_qp, B29A4_Phys2_qp
      data B2A2_Phys2_qp /(0E0_16,
     -     -0.82500302749446986929E0_16)/
      data B3A2_Phys2_qp /(2.6270483426551970923E0_16,
     -     0E0_16)/
      data B3A3_Phys2_qp /(2.6743581669712199862E0_16,
     -     0E0_16)/
      data B4A2_Phys2_qp /(1.3135241713275985462E0_16,
     -     0E0_16)/
      data B4A3_Phys2_qp /(0.31700070280911543096E0_16,
     -     0E0_16)/
      data B5A3_Phys2_qp /(0E0_16,
     -     2.1289825479625977883E0_16)/
      data B6A3_Phys2_qp /(0.82623119449144494083E0_16,
     -     0E0_16)/
      data B7A2_Phys2_qp /(0E0_16,
     -     -1.0471292962363372515E0_16)/
      data B8A3_Phys2_qp /(0.95878290910364721611E0_16,
     -     0E0_16)/
      data B9A3_Phys2_qp /(0.84055920424109209713E0_16,
     -     0E0_16)/
      data B10A3_Phys2_qp /(2.5294584133995772146E0_16,
     -     0E0_16)/
      data B11A2_Phys2_qp /(0E0_16,
     -     1.4992868653007891168E0_16)/
      data B18A3_Phys2_qp /(-0.68205885283653438165E0_16,
     -     0E0_16)/
      data B20A3_Phys2_qp /(1.847399560563042833E0_16,
     -     0E0_16)/
      data B23A3_Phys2_qp /(1.1653407077265084513E0_16,
     -     0E0_16)/
      data B26A4_Phys2_qp /(-2.6791975416289301837E0_16,
     -     0E0_16)/
      data B28A4_Phys2_qp /(0E0_16,
     -     2.0284487302266188385E0_16)/
      data B29A4_Phys2_qp /(5.3583950832578603674E0_16,
     -     0E0_16)/
      complex*32 B14A2_Phys3_qp, B14A3_Phys3_qp, B16A2_Phys3_qp
      complex*32 B16A3_Phys3_qp, B23A3_Phys3_qp, B26A4_Phys3_qp
      data B14A2_Phys3_qp /(-9.7704267987673657951E0_16,
     -     -1.9787306035604043949E0_16)/
      data B14A3_Phys3_qp /(-21.115460572443021688E0_16,
     -     -22.846775675910579278E0_16)/
      data B16A2_Phys3_qp /(2.4426066996918414488E0_16,
     -     0.4946826508901010987E0_16)/
      data B16A3_Phys3_qp /(8.0339962048207877497E0_16,
     -     5.7279672055041674672E0_16)/
      data B23A3_Phys3_qp /(2.7551310617100323278E0_16,
     -     0.0162732865265226477E0_16)/
      data B26A4_Phys3_qp /(-4.6070666045155698136E0_16,
     -     -0.0003213506334210942E0_16)/
      complex*32 B14A2_Phys4_qp, B14A3_Phys4_qp, B16A2_Phys4_qp
      complex*32 B16A3_Phys4_qp, B23A3_Phys4_qp, B26A4_Phys4_qp
      data B14A2_Phys4_qp /(-2.6270483426551970923E0_16,
     -     0E0_16)/
      data B14A3_Phys4_qp /(0.82623119449144494083E0_16,
     -     0E0_16)/
      data B16A2_Phys4_qp /(0.65676208566379927308E0_16,
     -     0E0_16)/
      data B16A3_Phys4_qp /(0.95878290910364721611E0_16,
     -     0E0_16)/
      data B23A3_Phys4_qp /(1.1653407077265084513E0_16,
     -     0E0_16)/
      data B26A4_Phys4_qp /(-2.2062067042935499415E0_16,
     -     0E0_16)/
      double precision zero_thr
      parameter (zero_thr=1d-13) ! may increase it to 1d-13 when using npoints=500000
      real*16 zero_thr_qp
      parameter (zero_thr_qp=1E-28_16) ! may increase it to 1E-28_16 when using npoints=500000
      contains
      function fE18_w3(xsb,xtb)
      implicit none
      double complex fE18_w3
      double precision xsb,xtb
      if(xsb.LT.0d0.and.xtb.LT.0d0)then
         ! Euclid
         fE18_w3=fE18Euclid_w3(xsb,xtb)
      elseif(xsb.GT.4d0.and.xtb.LT.0d0.and.xsb+xtb.GT.0d0)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE18_w3=fE18Phys1_w3(xsb,xtb)
      elseif(xsb.GT.0d0.and.xsb.LT.4d0.and.xtb.LT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE18_w3=fE18Phys2_w3(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.GT.4d0.and.xsb+xtb.GT.0d0)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE18_w3=fE18Phys3_w3(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.LT.4d0.and.xtb.GT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE18_w3=fE18Phys4_w3(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE25_w4(xsb,xtb)
      implicit none
      double complex fE25_w4
      double precision xsb,xtb
      if(xsb.LT.0d0.and.xtb.LT.0d0)then
         ! Euclid
         fE25_w4=fE25Euclid_w4(xsb,xtb)
      elseif(xsb.GT.4d0.and.xtb.LT.0d0.and.xsb+xtb.GT.0d0)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE25_w4=fE25Phys1_w4(xsb,xtb)
      elseif(xsb.GT.0d0.and.xsb.LT.4d0.and.xtb.LT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE25_w4=fE25Phys2_w4(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.GT.4d0.and.xsb+xtb.GT.0d0)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE25_w4=fE25Phys3_w4(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.LT.4d0.and.xtb.GT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE25_w4=fE25Phys4_w4(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE26_w4(xsb,xtb)
      implicit none
      double complex fE26_w4
      double precision xsb,xtb
      if(xsb.LT.0d0.and.xtb.LT.0d0)then
         ! Euclid
         fE26_w4=fE26Euclid_w4(xsb,xtb)
      elseif(xsb.GT.4d0.and.xtb.LT.0d0.and.xsb+xtb.GT.0d0)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE26_w4=fE26Phys1_w4(xsb,xtb)
      elseif(xsb.GT.0d0.and.xsb.LT.4d0.and.xtb.LT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE26_w4=fE26Phys2_w4(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.GT.4d0.and.xsb+xtb.GT.0d0)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE26_w4=fE26Phys3_w4(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.LT.4d0.and.xtb.GT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE26_w4=fE26Phys4_w4(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE28_w4(xsb,xtb)
      implicit none
      double complex fE28_w4
      double precision xsb,xtb
      if(xsb.LT.0d0.and.xtb.LT.0d0)then
         ! Euclid
         fE28_w4=fE28Euclid_w4(xsb,xtb)
      elseif(xsb.GT.4d0.and.xtb.LT.0d0.and.xsb+xtb.GT.0d0)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE28_w4=fE28Phys1_w4(xsb,xtb)
      elseif(xsb.GT.0d0.and.xsb.LT.4d0.and.xtb.LT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE28_w4=fE28Phys2_w4(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.GT.4d0.and.xsb+xtb.GT.0d0)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE28_w4=fE28Phys3_w4(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.LT.4d0.and.xtb.GT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE28_w4=fE28Phys4_w4(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE29_w4(xsb,xtb)
      implicit none
      double complex fE29_w4
      double precision xsb,xtb
      if(xsb.LT.0d0.and.xtb.LT.0d0)then
         ! Euclid
         fE29_w4=fE29Euclid_w4(xsb,xtb)
      elseif(xsb.GT.4d0.and.xtb.LT.0d0.and.xsb+xtb.GT.0d0)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE29_w4=fE29Phys1_w4(xsb,xtb)
      elseif(xsb.GT.0d0.and.xsb.LT.4d0.and.xtb.LT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12(xsb,0d0)
         endif
         fE29_w4=fE29Phys2_w4(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.GT.4d0.and.xsb+xtb.GT.0d0)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE29_w4=fE29Phys3_w4(xsb,xtb)
      elseif(xsb.LT.0d0.and.xtb.LT.4d0.and.xtb.GT.0d0
     $        .and.xsb+xtb.GT.0d0)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34(0d0,xtb)
         endif
         fE29_w4=fE29Phys4_w4(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE18Euclid_w3(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE18Euclid_w3
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      CALL SetPath_st(xsb,xsb0_Euclid,xtb,xtb0_Euclid)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE18Euclid_w3_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE18Euclid_w3_fxn,
     $        one,integral)
      endif
      fE18Euclid_w3=integral
      return
      end function fE18Euclid_w3

      function fE18Euclid_w3_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE18Euclid_w3_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Euclid_w3_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb).lt.zero_thr)then
         fE18Euclid_w3_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(HALF*(logs(2)**TWO + 8*logs(8)**TWO)))
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-logs(2)**TWO + 6*logs(8)**TWO)/4D0)
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -li2s(2) - li2s(3) - li2s(4) - li2s(5) + 
     -  li2s(6) + li2s(7) + li2s(8) + li2s(9) + 
     -  logs(1)*logs(5) + logs(3)*logs(6) - 
     -  logs(4)*logs(7))
      fE18Euclid_w3_fxn=res
      return
      end function fE18Euclid_w3_fxn

      function fE25Euclid_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE25Euclid_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(3)
      common /fE25Euclid_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Euclid,xtb,xtb0_Euclid)
      fteq1(1)=dlog_f11(xsb,xtb)
      fteq1(2)=dlog_f12(xsb,xtb)
      fteq1(3)=dlog_f13(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE25Euclid_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE25Euclid_w4_fxn,
     $        one,integral)
      endif
      fE25Euclid_w4=integral
      return
      end function fE25Euclid_w4

      function fE25Euclid_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE25Euclid_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(3)
      common /fE25Euclid_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Euclid_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb).lt.zero_thr)then
         fE25Euclid_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((logs(2)**TWO + 8*logs(8)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((logs(2)**TWO - 6*logs(8)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(2)
      f1=dlog_f12(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(5*logs(2)**TWO - 6*logs(8)**TWO))/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((9*logs(2)**TWO + 22*logs(8)**TWO)/4D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-9*(logs(2)**TWO + logs(8)**TWO))/2D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2D0)
      fE25Euclid_w4_fxn=res
      return
      end function fE25Euclid_w4_fxn

      function fE26Euclid_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE26Euclid_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(5)
      common /fE26Euclid_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Euclid,xtb,xtb0_Euclid)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f7(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE26Euclid_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE26Euclid_w4_fxn,
     $        one,integral)
      endif
      fE26Euclid_w4=integral
      return
      end function fE26Euclid_w4

      function fE26Euclid_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE26Euclid_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE26Euclid_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Euclid_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb).lt.zero_thr)then
         fE26Euclid_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(logs(2)**TWO - TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*logs(2)**TWO + 5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(FOUR*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-logs(2)**TWO + 6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(2) - li2s(3) - li2s(4) - li2s(5) + 
     -  li2s(6) + li2s(7) + li2s(8) + li2s(9) + 
     -  logs(1)*logs(5) + logs(3)*logs(6) - 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-7*logs(2)**TWO + 10*logs(8)**TWO)/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-5*(logs(2)**TWO + TWO*logs(8)**TWO))/2D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(5*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Euclid_w4_fxn=res
      return
      end function fE26Euclid_w4_fxn

      function fE28Euclid_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE28Euclid_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(2)
      common /fE28Euclid_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Euclid,xtb,xtb0_Euclid)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f9(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE28Euclid_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE28Euclid_w4_fxn,
     $        one,integral)
      endif
      fE28Euclid_w4=integral
      return
      end function fE28Euclid_w4

      function fE28Euclid_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE28Euclid_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(2)
      common /fE28Euclid_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Euclid_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb).lt.zero_thr)then
         fE28Euclid_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((TWO*RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(-((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb(xsb,xtb)
      dfdxtb=df2dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(5*logs(3)**TWO - TWO*logs(9)**TWO))/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -    li2s(6) + li2s(7) + li2s(8) + li2s(9) + 
     -    li2s(10) + logs(1)*logs(6) + 
     -    logs(4)*logs(7) - logs(5)*logs(8)))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(logs(3)**TWO + 8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((logs(3)**TWO - 6*logs(9)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(3) + li2s(4) + li2s(5) + li2s(6) - 
     -  li2s(7) - li2s(8) - li2s(9) - li2s(10) - 
     -  logs(1)*logs(6) - logs(4)*logs(7) + 
     -  logs(5)*logs(8))
      fE28Euclid_w4_fxn=res
      return
      end function fE28Euclid_w4_fxn

      function fE29Euclid_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE29Euclid_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(5)
      common /fE29Euclid_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Euclid,xtb,xtb0_Euclid)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f8(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE29Euclid_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE29Euclid_w4_fxn,
     $        one,integral)
      endif
      fE29Euclid_w4=integral
      return
      end function fE29Euclid_w4

      function fE29Euclid_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE29Euclid_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE29Euclid_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Euclid_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb).lt.zero_thr)then
         fE29Euclid_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(logs(2)**TWO + TWO*logs(8)**TWO))/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(logs(2)**TWO - TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-logs(2)**TWO - 6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-logs(2)**TWO + 6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(2) - li2s(3) - li2s(4) - li2s(5) + 
     -  li2s(6) + li2s(7) + li2s(8) + li2s(9) + 
     -  logs(1)*logs(5) + logs(3)*logs(6) - 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(logs(2)**TWO + TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-logs(2)**TWO + TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      fE29Euclid_w4_fxn=res
      return
      end function fE29Euclid_w4_fxn

      function fE18Phys1_w3(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE18Phys1_w3
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B18A3
      B18A3=B18A3_Phys1
      CALL SetPath_st(xsb,xsb0_Phys1,xtb,xtb0_Phys1)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE18Phys1_w3_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE18Phys1_w3_fxn,
     $        one,integral)
      endif
      fE18Phys1_w3=integral
      fE18Phys1_w3=fE18Phys1_w3+
     - +B18A3
      return
      end function fE18Phys1_w3

      function fE18Phys1_w3_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE18Phys1_w3_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B3A2
      double complex B4A2
      double complex B18A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Phys1_w3_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B3A2=B3A2_Phys1
      B4A2=B4A2_Phys1
      B18A3=B18A3_Phys1
      CALL GetPath_Phys1_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE18Phys1_w3_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      fE18Phys1_w3_fxn=res
      return
      end function fE18Phys1_w3_fxn

      function fE25Phys1_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE25Phys1_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(3)
      common /fE25Phys1_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Phys1,xtb,xtb0_Phys1)
      fteq1(1)=dlog_f11(xsb,xtb)
      fteq1(2)=dlog_f12(xsb,xtb)
      fteq1(3)=dlog_f13(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE25Phys1_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE25Phys1_w4_fxn,
     $        one,integral)
      endif
      fE25Phys1_w4=integral
      return
      end function fE25Phys1_w4

      function fE25Phys1_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE25Phys1_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B3A2
      double complex B4A2
      double complex B5A3
      double complex B10A3
      double complex B18A3
      double complex B20A3
      double complex B23A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(3)
      common /fE25Phys1_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Phys1_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B3A2=B3A2_Phys1
      B4A2=B4A2_Phys1
      B5A3=B5A3_Phys1
      B10A3=B10A3_Phys1
      B18A3=B18A3_Phys1
      B20A3=B20A3_Phys1
      B23A3=B23A3_Phys1
      CALL GetPath_Phys1_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE25Phys1_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B18A3*HALF))
      dfdxsb=df12dxsb(xsb,xtb)
      dfdxtb=df12dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B5A3)
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B10A3 + B20A3 - 8*B23A3)*HALF)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO + logs(2)**TWO + 
     -    8*logs(8)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + logs(2)**TWO - 
     -    6*logs(8)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(2)
      f1=dlog_f12(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(B3A2 - B4A2*TWO + THREE*logs(2)**TWO))
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO + 15*logs(2)**TWO - 
     -    18*logs(8)**TWO)/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + 9*logs(2)**TWO + 
     -    22*logs(8)**TWO)/4D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-9*(logs(2)**TWO + logs(8)**TWO))/2D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2D0)
      fE25Phys1_w4_fxn=res
      return
      end function fE25Phys1_w4_fxn

      function fE26Phys1_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE26Phys1_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B26A4
      double complex fteq1(5)
      common /fE26Phys1_w4_ft/ fteq1
      B26A4=B26A4_Phys1
      CALL SetPath_st(xsb,xsb0_Phys1,xtb,xtb0_Phys1)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f7(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE26Phys1_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE26Phys1_w4_fxn,
     $        one,integral)
      endif
      fE26Phys1_w4=integral
      fE26Phys1_w4=fE26Phys1_w4+
     - +B26A4
      return
      end function fE26Phys1_w4

      function fE26Phys1_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE26Phys1_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B3A2
      double complex B4A2
      double complex B5A3
      double complex B6A3
      double complex B8A3
      double complex B10A3
      double complex B18A3
      double complex B20A3
      double complex B23A3
      double complex B26A4
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE26Phys1_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Phys1_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B3A2=B3A2_Phys1
      B4A2=B4A2_Phys1
      B5A3=B5A3_Phys1
      B6A3=B6A3_Phys1
      B8A3=B8A3_Phys1
      B10A3=B10A3_Phys1
      B18A3=B18A3_Phys1
      B20A3=B20A3_Phys1
      B23A3=B23A3_Phys1
      B26A4=B26A4_Phys1
      CALL GetPath_Phys1_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE26Phys1_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B5A3*HALF))
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B6A3 - B8A3*FOUR - B10A3*TWO)/4D0)
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(HALF*(-8*B23A3 + B6A3 + B8A3*FOUR + B10A3*TWO))
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B18A3)
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B10A3 - B20A3 + B23A3*FOUR)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-B3A2 + B4A2*TWO - THREE*logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(logs(2)**TWO - TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*logs(2)**TWO + 5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(FOUR*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO - 7*logs(2)**TWO + 
     -    10*logs(8)**TWO)/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-B3A2 + B4A2*TWO + 
     -      5*(logs(2)**TWO + 2*logs(8)**TWO))))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(5*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Phys1_w4_fxn=res
      return
      end function fE26Phys1_w4_fxn

      function fE28Phys1_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE28Phys1_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B28A4
      double complex fteq1(2)
      common /fE28Phys1_w4_ft/ fteq1
      B28A4=B28A4_Phys1
      CALL SetPath_st(xsb,xsb0_Phys1,xtb,xtb0_Phys1)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f9(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE28Phys1_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE28Phys1_w4_fxn,
     $        one,integral)
      endif
      fE28Phys1_w4=integral
      fE28Phys1_w4=fE28Phys1_w4+
     - +B28A4
      return
      end function fE28Phys1_w4

      function fE28Phys1_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE28Phys1_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B2A2
      double complex B3A2
      double complex B3A3
      double complex B4A2
      double complex B4A3
      double complex B6A3
      double complex B7A2
      double complex B8A3
      double complex B9A3
      double complex B10A3
      double complex B11A2
      double complex B18A3
      double complex B20A3
      double complex B23A3
      double complex B28A4
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(2)
      common /fE28Phys1_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Phys1_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B2A2=B2A2_Phys1
      B3A2=B3A2_Phys1
      B3A3=B3A3_Phys1
      B4A2=B4A2_Phys1
      B4A3=B4A3_Phys1
      B6A3=B6A3_Phys1
      B7A2=B7A2_Phys1
      B8A3=B8A3_Phys1
      B9A3=B9A3_Phys1
      B10A3=B10A3_Phys1
      B11A2=B11A2_Phys1
      B18A3=B18A3_Phys1
      B20A3=B20A3_Phys1
      B23A3=B23A3_Phys1
      B28A4=B28A4_Phys1
      CALL GetPath_Phys1_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE28Phys1_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((TWO*RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(-((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        HALF*(-8*B23A3 + 5*B6A3 - B10A3*FOUR + 
     -    B8A3*FOUR + B9A3*FOUR - B20A3*TWO + 
     -    B3A3*TWO + B4A3*TWO))
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B18A3)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-32*B11A2 - 48*B2A2 - 8*B7A2 + 
     -    32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb(xsb,xtb)
      dfdxtb=df2dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(-B3A2 + B4A2*TWO + 5*logs(3)**TWO - 
     -      TWO*logs(9)**TWO))/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + 
     -    THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -      li2s(6) + li2s(7) + li2s(8) + 
     -      li2s(9) + li2s(10) + 
     -      logs(1)*logs(6) + logs(4)*logs(7) - 
     -      logs(5)*logs(8))))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + logs(3)**TWO + 
     -    8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + logs(3)**TWO - 
     -    6*logs(9)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(3) - li2s(4) - li2s(5) - li2s(6) + 
     -  li2s(7) + li2s(8) + li2s(9) + li2s(10) + 
     -  logs(1)*logs(6) + logs(4)*logs(7) - 
     -  logs(5)*logs(8))
      fE28Phys1_w4_fxn=res
      return
      end function fE28Phys1_w4_fxn

      function fE29Phys1_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE29Phys1_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B29A4
      double complex fteq1(5)
      common /fE29Phys1_w4_ft/ fteq1
      B29A4=B29A4_Phys1
      CALL SetPath_st(xsb,xsb0_Phys1,xtb,xtb0_Phys1)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f8(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE29Phys1_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE29Phys1_w4_fxn,
     $        one,integral)
      endif
      fE29Phys1_w4=integral
      fE29Phys1_w4=fE29Phys1_w4+
     - +B29A4
      return
      end function fE29Phys1_w4

      function fE29Phys1_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE29Phys1_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B3A2
      double complex B4A2
      double complex B5A3
      double complex B6A3
      double complex B8A3
      double complex B10A3
      double complex B18A3
      double complex B20A3
      double complex B23A3
      double complex B29A4
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE29Phys1_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Phys1_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B3A2=B3A2_Phys1
      B4A2=B4A2_Phys1
      B5A3=B5A3_Phys1
      B6A3=B6A3_Phys1
      B8A3=B8A3_Phys1
      B10A3=B10A3_Phys1
      B18A3=B18A3_Phys1
      B20A3=B20A3_Phys1
      B23A3=B23A3_Phys1
      B29A4=B29A4_Phys1
      CALL GetPath_Phys1_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE29Phys1_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B5A3)
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        HALF*(-8*B23A3 + 12*B8A3 + B10A3*FOUR + 
     -    B6A3*THREE - B20A3*TWO))
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B6A3 + B23A3*FOUR - B8A3*FOUR + B20A3*TWO)
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B18A3)
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B10A3 - B20A3)*TWO)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(B3A2 - B4A2*TWO + THREE*logs(2)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO - THREE*logs(2)**TWO - 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + 
     -    THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(li2s(2) + li2s(3) + li2s(4) + 
     -    li2s(5) - li2s(6) - li2s(7) - li2s(8) - 
     -    li2s(9) - logs(1)*logs(5) - 
     -    logs(3)*logs(6) + logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-B3A2 + B4A2*TWO + logs(2)**TWO - 
     -      TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-B3A2 + B4A2*TWO - logs(2)**TWO - 6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-B3A2 + B4A2*TWO + logs(2)**TWO + 
     -    TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(B3A2 - B4A2*TWO - logs(2)**TWO + TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE29Phys1_w4_fxn=res
      return
      end function fE29Phys1_w4_fxn

      function fE18Phys2_w3(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE18Phys2_w3
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B18A3
      B18A3=B18A3_Phys2
      CALL SetPath_st(xsb,xsb0_Phys2,xtb,xtb0_Phys2)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE18Phys2_w3_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE18Phys2_w3_fxn,
     $        one,integral)
      endif
      fE18Phys2_w3=integral
      fE18Phys2_w3=fE18Phys2_w3+
     - +B18A3
      return
      end function fE18Phys2_w3

      function fE18Phys2_w3_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE18Phys2_w3_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B3A2
      double complex B4A2
      double complex B18A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Phys2_w3_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B3A2=B3A2_Phys2
      B4A2=B4A2_Phys2
      B18A3=B18A3_Phys2
      CALL GetPath_Phys2_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE18Phys2_w3_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      fE18Phys2_w3_fxn=res
      return
      end function fE18Phys2_w3_fxn

      function fE25Phys2_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE25Phys2_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(3)
      common /fE25Phys2_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Phys2,xtb,xtb0_Phys2)
      fteq1(1)=dlog_f11(xsb,xtb)
      fteq1(2)=dlog_f12(xsb,xtb)
      fteq1(3)=dlog_f13(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE25Phys2_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE25Phys2_w4_fxn,
     $        one,integral)
      endif
      fE25Phys2_w4=integral
      return
      end function fE25Phys2_w4

      function fE25Phys2_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE25Phys2_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B3A2
      double complex B4A2
      double complex B5A3
      double complex B10A3
      double complex B18A3
      double complex B20A3
      double complex B23A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(3)
      common /fE25Phys2_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Phys2_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B3A2=B3A2_Phys2
      B4A2=B4A2_Phys2
      B5A3=B5A3_Phys2
      B10A3=B10A3_Phys2
      B18A3=B18A3_Phys2
      B20A3=B20A3_Phys2
      B23A3=B23A3_Phys2
      CALL GetPath_Phys2_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE25Phys2_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B18A3*HALF))
      dfdxsb=df12dxsb(xsb,xtb)
      dfdxtb=df12dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B5A3)
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B10A3 + B20A3 - 8*B23A3)*HALF)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO + logs(2)**TWO + 
     -    8*logs(8)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + logs(2)**TWO - 
     -    6*logs(8)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(2)
      f1=dlog_f12(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(B3A2 - B4A2*TWO + THREE*logs(2)**TWO))
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO + 15*logs(2)**TWO - 
     -    18*logs(8)**TWO)/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + 9*logs(2)**TWO + 
     -    22*logs(8)**TWO)/4D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-9*(logs(2)**TWO + logs(8)**TWO))/2D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2D0)
      fE25Phys2_w4_fxn=res
      return
      end function fE25Phys2_w4_fxn

      function fE26Phys2_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE26Phys2_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B26A4
      double complex fteq1(5)
      common /fE26Phys2_w4_ft/ fteq1
      B26A4=B26A4_Phys2
      CALL SetPath_st(xsb,xsb0_Phys2,xtb,xtb0_Phys2)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f7(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE26Phys2_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE26Phys2_w4_fxn,
     $        one,integral)
      endif
      fE26Phys2_w4=integral
      fE26Phys2_w4=fE26Phys2_w4+
     - +B26A4
      return
      end function fE26Phys2_w4

      function fE26Phys2_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE26Phys2_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B3A2
      double complex B4A2
      double complex B5A3
      double complex B6A3
      double complex B8A3
      double complex B10A3
      double complex B18A3
      double complex B20A3
      double complex B23A3
      double complex B26A4
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE26Phys2_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Phys2_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B3A2=B3A2_Phys2
      B4A2=B4A2_Phys2
      B5A3=B5A3_Phys2
      B6A3=B6A3_Phys2
      B8A3=B8A3_Phys2
      B10A3=B10A3_Phys2
      B18A3=B18A3_Phys2
      B20A3=B20A3_Phys2
      B23A3=B23A3_Phys2
      B26A4=B26A4_Phys2
      CALL GetPath_Phys2_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE26Phys2_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B5A3*HALF))
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B6A3 - B8A3*FOUR - B10A3*TWO)/4D0)
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(HALF*(-8*B23A3 + B6A3 + B8A3*FOUR + B10A3*TWO))
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B18A3)
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B10A3 - B20A3 + B23A3*FOUR)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-B3A2 + B4A2*TWO - THREE*logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(logs(2)**TWO - TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*logs(2)**TWO + 5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(FOUR*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO - 7*logs(2)**TWO + 
     -    10*logs(8)**TWO)/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-B3A2 + B4A2*TWO + 
     -      5*(logs(2)**TWO + 2*logs(8)**TWO))))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(5*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Phys2_w4_fxn=res
      return
      end function fE26Phys2_w4_fxn

      function fE28Phys2_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE28Phys2_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B28A4
      double complex fteq1(2)
      common /fE28Phys2_w4_ft/ fteq1
      B28A4=B28A4_Phys2
      CALL SetPath_st(xsb,xsb0_Phys2,xtb,xtb0_Phys2)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f9(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE28Phys2_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE28Phys2_w4_fxn,
     $        one,integral)
      endif
      fE28Phys2_w4=integral
      fE28Phys2_w4=fE28Phys2_w4+
     - +B28A4
      return
      end function fE28Phys2_w4

      function fE28Phys2_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE28Phys2_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B2A2
      double complex B3A2
      double complex B3A3
      double complex B4A2
      double complex B4A3
      double complex B6A3
      double complex B7A2
      double complex B8A3
      double complex B9A3
      double complex B10A3
      double complex B11A2
      double complex B18A3
      double complex B20A3
      double complex B23A3
      double complex B28A4
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(2)
      common /fE28Phys2_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Phys2_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B2A2=B2A2_Phys2
      B3A2=B3A2_Phys2
      B3A3=B3A3_Phys2
      B4A2=B4A2_Phys2
      B4A3=B4A3_Phys2
      B6A3=B6A3_Phys2
      B7A2=B7A2_Phys2
      B8A3=B8A3_Phys2
      B9A3=B9A3_Phys2
      B10A3=B10A3_Phys2
      B11A2=B11A2_Phys2
      B18A3=B18A3_Phys2
      B20A3=B20A3_Phys2
      B23A3=B23A3_Phys2
      B28A4=B28A4_Phys2
      CALL GetPath_Phys2_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE28Phys2_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((TWO*RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(-((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        HALF*(-8*B23A3 + 5*B6A3 - B10A3*FOUR + 
     -    B8A3*FOUR + B9A3*FOUR - B20A3*TWO + 
     -    B3A3*TWO + B4A3*TWO))
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B18A3)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-32*B11A2 - 48*B2A2 - 8*B7A2 + 
     -    32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb(xsb,xtb)
      dfdxtb=df2dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(-B3A2 + B4A2*TWO + 5*logs(3)**TWO - 
     -      TWO*logs(9)**TWO))/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + 
     -    THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -      li2s(6) + li2s(7) + li2s(8) + 
     -      li2s(9) + li2s(10) + 
     -      logs(1)*logs(6) + logs(4)*logs(7) - 
     -      logs(5)*logs(8))))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + logs(3)**TWO + 
     -    8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + logs(3)**TWO - 
     -    6*logs(9)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(3) - li2s(4) - li2s(5) - li2s(6) + 
     -  li2s(7) + li2s(8) + li2s(9) + li2s(10) + 
     -  logs(1)*logs(6) + logs(4)*logs(7) - 
     -  logs(5)*logs(8))
      fE28Phys2_w4_fxn=res
      return
      end function fE28Phys2_w4_fxn

      function fE29Phys2_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE29Phys2_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B29A4
      double complex fteq1(5)
      common /fE29Phys2_w4_ft/ fteq1
      B29A4=B29A4_Phys2
      CALL SetPath_st(xsb,xsb0_Phys2,xtb,xtb0_Phys2)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f8(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE29Phys2_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE29Phys2_w4_fxn,
     $        one,integral)
      endif
      fE29Phys2_w4=integral
      fE29Phys2_w4=fE29Phys2_w4+
     - +B29A4
      return
      end function fE29Phys2_w4

      function fE29Phys2_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE29Phys2_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B3A2
      double complex B4A2
      double complex B5A3
      double complex B6A3
      double complex B8A3
      double complex B10A3
      double complex B18A3
      double complex B20A3
      double complex B23A3
      double complex B29A4
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE29Phys2_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Phys2_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B3A2=B3A2_Phys2
      B4A2=B4A2_Phys2
      B5A3=B5A3_Phys2
      B6A3=B6A3_Phys2
      B8A3=B8A3_Phys2
      B10A3=B10A3_Phys2
      B18A3=B18A3_Phys2
      B20A3=B20A3_Phys2
      B23A3=B23A3_Phys2
      B29A4=B29A4_Phys2
      CALL GetPath_Phys2_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xtb).lt.zero_thr.or.abs(xsb-4d0).lt.zero_thr)then
         fE29Phys2_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B5A3)
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        HALF*(-8*B23A3 + 12*B8A3 + B10A3*FOUR + 
     -    B6A3*THREE - B20A3*TWO))
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B6A3 + B23A3*FOUR - B8A3*FOUR + B20A3*TWO)
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B18A3)
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B10A3 - B20A3)*TWO)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(B3A2 - B4A2*TWO + THREE*logs(2)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO - THREE*logs(2)**TWO - 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + 
     -    THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(li2s(2) + li2s(3) + li2s(4) + 
     -    li2s(5) - li2s(6) - li2s(7) - li2s(8) - 
     -    li2s(9) - logs(1)*logs(5) - 
     -    logs(3)*logs(6) + logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-B3A2 + B4A2*TWO + logs(2)**TWO - 
     -      TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-B3A2 + B4A2*TWO - logs(2)**TWO - 6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-B3A2 + B4A2*TWO + logs(2)**TWO + 
     -    TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(B3A2 - B4A2*TWO - logs(2)**TWO + TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE29Phys2_w4_fxn=res
      return
      end function fE29Phys2_w4_fxn

      function fE18Phys3_w3(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE18Phys3_w3
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      CALL SetPath_st(xsb,xsb0_Phys3,xtb,xtb0_Phys3)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE18Phys3_w3_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE18Phys3_w3_fxn,
     $        one,integral)
      endif
      fE18Phys3_w3=integral
      return
      end function fE18Phys3_w3

      function fE18Phys3_w3_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE18Phys3_w3_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B16A2
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Phys3_w3_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3
      B16A2=B16A2_Phys3
      CALL GetPath_Phys3_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE18Phys3_w3_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      fE18Phys3_w3_fxn=res
      return
      end function fE18Phys3_w3_fxn

      function fE25Phys3_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE25Phys3_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(3)
      common /fE25Phys3_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Phys3,xtb,xtb0_Phys3)
      fteq1(1)=dlog_f11(xsb,xtb)
      fteq1(2)=dlog_f12(xsb,xtb)
      fteq1(3)=dlog_f13(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE25Phys3_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE25Phys3_w4_fxn,
     $        one,integral)
      endif
      fE25Phys3_w4=integral
      return
      end function fE25Phys3_w4

      function fE25Phys3_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE25Phys3_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B14A3
      double complex B16A2
      double complex B16A3
      double complex B23A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(3)
      common /fE25Phys3_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Phys3_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3
      B14A3=B14A3_Phys3
      B16A2=B16A2_Phys3
      B16A3=B16A3_Phys3
      B23A3=B23A3_Phys3
      CALL GetPath_Phys3_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE25Phys3_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B14A3 - 8*B23A3 + B16A3*FOUR)*HALF)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-8*B14A2 - 32*B16A2 + logs(2)**TWO + 
     -    8*logs(8)**TWO)/4d0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (6*B14A2 + 24*B16A2 + logs(2)**TWO - 
     -    6*logs(8)**TWO)/8d0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(2)
      f1=dlog_f12(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(6*B14A2 + 24*B16A2 + 5*logs(2)**TWO - 
     -      6*logs(8)**TWO))/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-22*B14A2 - 88*B16A2 + 9*logs(2)**TWO + 
     -    22*logs(8)**TWO)/4D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -      logs(8)**TWO))/2D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2D0)
      fE25Phys3_w4_fxn=res
      return
      end function fE25Phys3_w4_fxn

      function fE26Phys3_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE26Phys3_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B26A4
      double complex fteq1(5)
      common /fE26Phys3_w4_ft/ fteq1
      B26A4=B26A4_Phys3
      CALL SetPath_st(xsb,xsb0_Phys3,xtb,xtb0_Phys3)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f7(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE26Phys3_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE26Phys3_w4_fxn,
     $        one,integral)
      endif
      fE26Phys3_w4=integral
      fE26Phys3_w4=fE26Phys3_w4+
     - +B26A4
      return
      end function fE26Phys3_w4

      function fE26Phys3_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE26Phys3_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B14A3
      double complex B16A2
      double complex B16A3
      double complex B23A3
      double complex B26A4
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE26Phys3_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Phys3_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3
      B14A3=B14A3_Phys3
      B16A2=B16A2_Phys3
      B16A3=B16A3_Phys3
      B23A3=B23A3_Phys3
      B26A4=B26A4_Phys3
      CALL GetPath_Phys3_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE26Phys3_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B14A3 - 8*B23A3 + B16A3*FOUR)*HALF)
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B14A3 - (B16A3 - B23A3)*FOUR)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        8*B16A2 + B14A2*TWO + logs(2)**TWO - 
     -  TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -5*B14A2 - 20*B16A2 + TWO*logs(2)**TWO + 
     -  5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        FOUR*(B14A2 + B16A2*FOUR - logs(2)**TWO - 
     -    logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-10*B14A2 - 40*B16A2 - 7*logs(2)**TWO + 
     -    10*logs(8)**TWO)/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-5*(-8*B16A2 - B14A2*TWO + 
     -      logs(2)**TWO + TWO*logs(8)**TWO))/2D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Phys3_w4_fxn=res
      return
      end function fE26Phys3_w4_fxn

      function fE28Phys3_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE28Phys3_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(2)
      common /fE28Phys3_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Phys3,xtb,xtb0_Phys3)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f9(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE28Phys3_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE28Phys3_w4_fxn,
     $        one,integral)
      endif
      fE28Phys3_w4=integral
      return
      end function fE28Phys3_w4

      function fE28Phys3_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE28Phys3_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B16A2
      double complex B23A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(2)
      common /fE28Phys3_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Phys3_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3
      B16A2=B16A2_Phys3
      B23A3=B23A3_Phys3
      CALL GetPath_Phys3_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE28Phys3_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((TWO*RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(-((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B23A3*FOUR))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb(xsb,xtb)
      dfdxtb=df2dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(8*B16A2 + B14A2*TWO + 
     -      5*logs(3)**TWO - TWO*logs(9)**TWO))/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-10*B14A2 - 40*B16A2 + 
     -    THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-B14A2 - B16A2*FOUR + 
     -      logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -      li2s(6) + li2s(7) + li2s(8) + 
     -      li2s(9) + li2s(10) + 
     -      logs(1)*logs(6) + logs(4)*logs(7) - 
     -      logs(5)*logs(8))))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-8*B14A2 - 32*B16A2 + 
     -    logs(3)**TWO + 8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (6*B14A2 + 24*B16A2 + logs(3)**TWO - 
     -    6*logs(9)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(3) - li2s(4) - li2s(5) - li2s(6) + 
     -  li2s(7) + li2s(8) + li2s(9) + li2s(10) + 
     -  logs(1)*logs(6) + logs(4)*logs(7) - 
     -  logs(5)*logs(8))
      fE28Phys3_w4_fxn=res
      return
      end function fE28Phys3_w4_fxn

      function fE29Phys3_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE29Phys3_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(5)
      common /fE29Phys3_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Phys3,xtb,xtb0_Phys3)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f8(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE29Phys3_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE29Phys3_w4_fxn,
     $        one,integral)
      endif
      fE29Phys3_w4=integral
      return
      end function fE29Phys3_w4

      function fE29Phys3_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE29Phys3_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B14A3
      double complex B16A2
      double complex B16A3
      double complex B23A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE29Phys3_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Phys3_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3
      B14A3=B14A3_Phys3
      B16A2=B16A2_Phys3
      B16A3=B16A3_Phys3
      B23A3=B23A3_Phys3
      CALL GetPath_Phys3_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE29Phys3_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B23A3*FOUR))
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B14A3 + (B16A3 + B23A3)*FOUR)
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B14A3 - B16A3*FOUR)*TWO)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-3*(-8*B16A2 - B14A2*TWO + 
     -      logs(2)**TWO + TWO*logs(8)**TWO))/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-10*B14A2 - 40*B16A2 + 
     -    THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-B14A2 - B16A2*FOUR + 
     -      logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(li2s(2) + li2s(3) + li2s(4) + 
     -    li2s(5) - li2s(6) - li2s(7) - li2s(8) - 
     -    li2s(9) - logs(1)*logs(5) - 
     -    logs(3)*logs(6) + logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(8*B16A2 + B14A2*TWO + 
     -      logs(2)**TWO - TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        6*B14A2 + 24*B16A2 - logs(2)**TWO - 
     -  6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-8*B16A2 - B14A2*TWO + 
     -    logs(2)**TWO + TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -8*B16A2 - B14A2*TWO - logs(2)**TWO + 
     -  TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE29Phys3_w4_fxn=res
      return
      end function fE29Phys3_w4_fxn

      function fE18Phys4_w3(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE18Phys4_w3
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      CALL SetPath_st(xsb,xsb0_Phys4,xtb,xtb0_Phys4)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE18Phys4_w3_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE18Phys4_w3_fxn,
     $        one,integral)
      endif
      fE18Phys4_w3=integral
      return
      end function fE18Phys4_w3

      function fE18Phys4_w3_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE18Phys4_w3_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B16A2
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Phys4_w3_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4
      B16A2=B16A2_Phys4
      CALL GetPath_Phys4_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE18Phys4_w3_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      fE18Phys4_w3_fxn=res
      return
      end function fE18Phys4_w3_fxn

      function fE25Phys4_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE25Phys4_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(3)
      common /fE25Phys4_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Phys4,xtb,xtb0_Phys4)
      fteq1(1)=dlog_f11(xsb,xtb)
      fteq1(2)=dlog_f12(xsb,xtb)
      fteq1(3)=dlog_f13(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE25Phys4_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE25Phys4_w4_fxn,
     $        one,integral)
      endif
      fE25Phys4_w4=integral
      return
      end function fE25Phys4_w4

      function fE25Phys4_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE25Phys4_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B14A3
      double complex B16A2
      double complex B16A3
      double complex B23A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(3)
      common /fE25Phys4_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Phys4_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4
      B14A3=B14A3_Phys4
      B16A2=B16A2_Phys4
      B16A3=B16A3_Phys4
      B23A3=B23A3_Phys4
      CALL GetPath_Phys4_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE25Phys4_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B14A3 - 8*B23A3 + B16A3*FOUR)*HALF)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-8*B14A2 - 32*B16A2 + logs(2)**TWO + 
     -    8*logs(8)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (6*B14A2 + 24*B16A2 + logs(2)**TWO - 
     -    6*logs(8)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8D0)
      ft1=fteq1(1)
      f1=dlog_f11(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(2)
      f1=dlog_f12(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(6*B14A2 + 24*B16A2 + 5*logs(2)**TWO - 
     -      6*logs(8)**TWO))/8D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-22*B14A2 - 88*B16A2 + 9*logs(2)**TWO + 
     -    22*logs(8)**TWO)/4D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -      logs(8)**TWO))/2D0)
      ft1=fteq1(3)
      f1=dlog_f13(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2D0)
      fE25Phys4_w4_fxn=res
      return
      end function fE25Phys4_w4_fxn

      function fE26Phys4_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE26Phys4_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex B26A4
      double complex fteq1(5)
      common /fE26Phys4_w4_ft/ fteq1
      B26A4=B26A4_Phys4
      CALL SetPath_st(xsb,xsb0_Phys4,xtb,xtb0_Phys4)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f7(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE26Phys4_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE26Phys4_w4_fxn,
     $        one,integral)
      endif
      fE26Phys4_w4=integral
      fE26Phys4_w4=fE26Phys4_w4+
     - +B26A4
      return
      end function fE26Phys4_w4

      function fE26Phys4_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE26Phys4_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B14A3
      double complex B16A2
      double complex B16A3
      double complex B23A3
      double complex B26A4
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE26Phys4_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Phys4_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4
      B14A3=B14A3_Phys4
      B16A2=B16A2_Phys4
      B16A3=B16A3_Phys4
      B23A3=B23A3_Phys4
      B26A4=B26A4_Phys4
      CALL GetPath_Phys4_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE26Phys4_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B14A3 - 8*B23A3 + B16A3*FOUR)*HALF)
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B14A3 - (B16A3 - B23A3)*FOUR)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        8*B16A2 + B14A2*TWO + logs(2)**TWO - 
     -  TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -5*B14A2 - 20*B16A2 + TWO*logs(2)**TWO + 
     -  5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        FOUR*(B14A2 + B16A2*FOUR - logs(2)**TWO - 
     -    logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f7(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-10*B14A2 - 40*B16A2 - 7*logs(2)**TWO + 
     -    10*logs(8)**TWO)/4D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-5*(-8*B16A2 - B14A2*TWO + 
     -      logs(2)**TWO + TWO*logs(8)**TWO))/2D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Phys4_w4_fxn=res
      return
      end function fE26Phys4_w4_fxn

      function fE28Phys4_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE28Phys4_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(2)
      common /fE28Phys4_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Phys4,xtb,xtb0_Phys4)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f9(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE28Phys4_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE28Phys4_w4_fxn,
     $        one,integral)
      endif
      fE28Phys4_w4=integral
      return
      end function fE28Phys4_w4

      function fE28Phys4_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE28Phys4_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B16A2
      double complex B23A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(2)
      common /fE28Phys4_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Phys4_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4
      B16A2=B16A2_Phys4
      B23A3=B23A3_Phys4
      CALL GetPath_Phys4_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE28Phys4_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((TWO*RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(-((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B23A3*FOUR))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb(xsb,xtb)
      dfdxtb=df2dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(8*B16A2 + B14A2*TWO + 
     -      5*logs(3)**TWO - TWO*logs(9)**TWO))/4D0)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-10*B14A2 - 40*B16A2 + 
     -    THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-B14A2 - B16A2*FOUR + 
     -      logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -      li2s(6) + li2s(7) + li2s(8) + 
     -      li2s(9) + li2s(10) + 
     -      logs(1)*logs(6) + logs(4)*logs(7) - 
     -      logs(5)*logs(8))))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-8*B14A2 - 32*B16A2 + 
     -    logs(3)**TWO + 8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (6*B14A2 + 24*B16A2 + logs(3)**TWO - 
     -    6*logs(9)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4D0)
      ft1=fteq1(2)
      f1=dlog_f9(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(3) - li2s(4) - li2s(5) - li2s(6) + 
     -  li2s(7) + li2s(8) + li2s(9) + li2s(10) + 
     -  logs(1)*logs(6) + logs(4)*logs(7) - 
     -  logs(5)*logs(8))
      fE28Phys4_w4_fxn=res
      return
      end function fE28Phys4_w4_fxn

      function fE29Phys4_w4(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      double complex fE29Phys4_w4
      double precision xsb,xtb
      double complex integral
      double precision error
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      double complex fteq1(5)
      common /fE29Phys4_w4_ft/ fteq1
      CALL SetPath_st(xsb,xsb0_Phys4,xtb,xtb0_Phys4)
      fteq1(1)=dlog_f1(xsb,xtb)
      fteq1(2)=dlog_f3(xsb,xtb)
      fteq1(3)=dlog_f4(xsb,xtb)
      fteq1(4)=dlog_f8(xsb,xtb)
      fteq1(5)=dlog_f10(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init.eq.0)then
            CALL DEQuadrature_integrator_ini(lenaw,tiny,DEQ_eps,aw)
            DEQ_init=1
         endif
         CALL DEQuadrature_Cintegrator(fE29Phys4_w4_fxn,zero,one,aw,
     $        integral,error)
      else
         CALL trapezoid_Cintegrator(npoints,fE29Phys4_w4_fxn,
     $        one,integral)
      endif
      fE29Phys4_w4=integral
      return
      end function fE29Phys4_w4

      function fE29Phys4_w4_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      double complex fE29Phys4_w4_fxn
      double precision t
      double precision xsb,xtb,dxsbdt,dxtbdt
      double complex dfdxsb,dfdxtb,dfdt
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double precision zeta2,zeta3,zeta4,Pi
      parameter (zeta2=1.64493406684822643647241516664602518921894990d0)
      parameter (zeta3=1.20205690315959428539973816151144999076532573d0)
      parameter (zeta4=1.08232323371113819151600369654116790277475095d0)
      parameter (Pi=3.141592653589793238462643383279502884197169399d0)
      double complex B14A2
      double complex B14A3
      double complex B16A2
      double complex B16A3
      double complex B23A3
      double complex res,f1,ft1
      integer init
      save init
      data init/0/
      double complex fteq1(5)
      common /fE29Phys4_w4_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      double complex logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      double complex li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Phys4_w4_fxn=dcmplx(ZERO,ZERO)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4
      B14A3=B14A3_Phys4
      B16A2=B16A2_Phys4
      B16A3=B16A3_Phys4
      B23A3=B23A3_Phys4
      CALL GetPath_Phys4_st(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0d0.and.abs(dxtbdt).eq.0d0)
     $     .or.abs(xsb).lt.zero_thr.or.abs(xtb-4d0).lt.zero_thr)then
         fE29Phys4_w4_fxn=dcmplx(ZERO,ZERO)
         RETURN
      endif
      logs(1)=        LogP2((-1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2((-xsb + RSQRTP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2((-1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2((1 + RSQRTP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2((RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xsb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2((RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP(1 - FOUR/xtb) + 
     -     RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2((-xtb + RSQRTP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M(1 - (-xsb + 
     -      RSQRTP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 - RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M((1 - RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M((1 + RSQRTP(1 - FOUR/xsb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M((1 - RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M((1 + RSQRTP(1 - FOUR/xtb))/
     -   (1 + RSQRTP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=dcmplx(0d0,0d0)
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B23A3*FOUR))
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B14A3 + (B16A3 + B23A3)*FOUR)
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B14A3 - B16A3*FOUR)*TWO)
      ft1=fteq1(1)
      f1=dlog_f1(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-3*(-8*B16A2 - B14A2*TWO + 
     -      logs(2)**TWO + TWO*logs(8)**TWO))/4D0)
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-10*B14A2 - 40*B16A2 + 
     -    THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-B14A2 - B16A2*FOUR + 
     -      logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(li2s(2) + li2s(3) + li2s(4) + 
     -    li2s(5) - li2s(6) - li2s(7) - li2s(8) - 
     -    li2s(9) - logs(1)*logs(5) - 
     -    logs(3)*logs(6) + logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(8*B16A2 + B14A2*TWO + 
     -      logs(2)**TWO - TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        6*B14A2 + 24*B16A2 - logs(2)**TWO - 
     -  6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb(xsb,xtb)
      dfdxtb=df7dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb(xsb,xtb)
      dfdxtb=df8dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb(xsb,xtb)
      dfdxtb=df9dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4D0)
      ft1=fteq1(4)
      f1=dlog_f8(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb(xsb,xtb)
      dfdxtb=df11dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb(xsb,xtb)
      dfdxtb=df1dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2D0)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb(xsb,xtb)
      dfdxtb=df3dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-8*B16A2 - B14A2*TWO + 
     -    logs(2)**TWO + TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb(xsb,xtb)
      dfdxtb=df4dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -8*B16A2 - B14A2*TWO - logs(2)**TWO + 
     -  TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb(xsb,xtb)
      dfdxtb=df10dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb(xsb,xtb)
      dfdxtb=df13dxtb(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE29Phys4_w4_fxn=res
      return
      end function fE29Phys4_w4_fxn
      
      ! non-rationalised dlog forms
      function dlog_f1(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f1
      double precision xsb,xtb
      dlog_f1=        -LogP2((-xsb + RSQRTP((-4 + xsb)*xsb))/
     -    (xsb + RSQRTP((-4 + xsb)*xsb)),
     -   (-4 + xsb)*xsb)
      if(isnan(dreal(dlog_f1))
     $     .or.dreal(dlog_f1)-1d0.eq.dreal(dlog_f1))then
          dlog_f1=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f1

      function df1dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df1dxsb
      double precision xsb,xtb
      df1dxsb=1d0/RSQRTP((-4d0+xsb)*xsb)
      return
      end function df1dxsb

      function df1dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df1dxtb
      double precision xsb,xtb
      df1dxtb=0d0
      return
      end function df1dxtb

      function dlog_f2(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f2
      double precision xsb,xtb
      dlog_f2=-RLogP(-4 + xsb)
      if(isnan(dreal(dlog_f2))
     $     .or.dreal(dlog_f2)-1d0.eq.dreal(dlog_f2))then
          dlog_f2=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f2

      function df2dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df2dxsb
      double precision xsb,xtb
      df2dxsb=1d0/(4d0-xsb)
      return
      end function df2dxsb

      function df2dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df2dxtb
      double precision xsb,xtb
      df2dxtb=0d0
      return
      end function df2dxtb

      function dlog_f3(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f3
      double precision xsb,xtb
      dlog_f3=RLogP(xsb)
      if(isnan(dreal(dlog_f3))
     $     .or.dreal(dlog_f3)-1d0.eq.dreal(dlog_f3))then
         ! nan or infinite
          dlog_f3=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f3

      function df3dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df3dxsb
      double precision xsb,xtb
      df3dxsb=1d0/xsb
      return
      end function df3dxsb

      function df3dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df3dxtb
      double precision xsb,xtb
      df3dxtb=0d0
      return
      end function df3dxtb

      function dlog_f4(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f4
      double precision xsb,xtb
      dlog_f4=RLogP(xtb)
      if(isnan(dreal(dlog_f4))
     $     .or.dreal(dlog_f4)-1d0.eq.dreal(dlog_f4))then
          dlog_f4=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f4

      function df4dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df4dxsb
      double precision xsb,xtb
      df4dxsb=0d0
      return
      end function df4dxsb

      function df4dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df4dxtb
      double precision xsb,xtb
      df4dxtb=1d0/xtb
      return
      end function df4dxtb

      function dlog_f7(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f7
      double precision xsb,xtb
      dlog_f7=        LogP2((xsb*(1 + xtb) - 
     -      RSQRTM(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2)))/
     -    (xsb*(1 + xtb) + 
     -      RSQRTM(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))),
     -   xsb*(xsb*(-1 + xtb)**2 - 4*xtb**2))/2D0
      if(isnan(dreal(dlog_f7))
     $     .or.dreal(dlog_f7)-1d0.eq.dreal(dlog_f7))then
         ! nan or infinite
          dlog_f7=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f7

      function df7dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df7dxsb
      double precision xsb,xtb
      df7dxsb=        -(xtb*(1 + xtb))/
     -  (2D0*(xsb + xtb)*
     -    RSQRTM(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df7dxsb

      function df7dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df7dxtb
      double precision xsb,xtb
      df7dxtb=        (xsb*(xsb + 2*xtb - xsb*xtb))/
     -  (2D0*xtb*(xsb + xtb)*
     -    RSQRTM(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df7dxtb

      function dlog_f8(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f8
      double precision xsb,xtb
      double complex den1
      if(xsb.GT.0d0.and.-xtb.LT.1D-6.and.-xtb.GT.0d0)then
         den1=dcmplx(-2d0*xtb**2,0d0)
      else
         den1=(xsb*(-1d0 + xtb) +
     -      RSQRTM(xsb*
     -        (xsb*(-1d0 + xtb)**2 - 4d0*xtb**2)))
      endif
      dlog_f8=        LogP2((xsb*(-1d0 + xtb) -
     -      RSQRTM(xsb*
     -        (xsb*(-1d0 + xtb)**2 - 4d0*xtb**2)))/
     -  den1, xsb*(xsb*(-1d0 + xtb)**2 - 4d0*xtb**2))+
     -  LogP2((xsb*(1d0 + xtb) -
     -      RSQRTM(xsb*
     -        (xsb*(-1d0 + xtb)**2 - 4d0*xtb**2)))/
     -    (xsb*(1d0 + xtb) +
     -      RSQRTM(xsb*
     -        (xsb*(-1d0 + xtb)**2 - 4d0*xtb**2))),
     -     xsb*(xsb*(-1d0 + xtb)**2 - 4d0*xtb**2))
c      dlog_f8=        LogP2((xsb*(-1 + xtb) - 
c     -      RSQRTM(xsb*
c     -        (xsb*(-1 + xtb)**2 - 4*xtb**2)))/
c     -    (xsb*(-1 + xtb) + 
c     -      RSQRTM(xsb*
c     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))),
c     -   xsb*(xsb*(-1 + xtb)**2 - 4*xtb**2)) + 
c     -  LogP2((xsb*(1 + xtb) - 
c     -      RSQRTM(xsb*
c     -        (xsb*(-1 + xtb)**2 - 4*xtb**2)))/
c     -    (xsb*(1 + xtb) + 
c     -      RSQRTM(xsb*
c     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))),
c     -   xsb*(xsb*(-1 + xtb)**2 - 4*xtb**2))
      if(isnan(dreal(dlog_f8))
     $     .or.dreal(dlog_f8)-1d0.eq.dreal(dlog_f8))then
         ! nan or infinite
          dlog_f8=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f8

      function df8dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df8dxsb
      double precision xsb,xtb
      df8dxsb=        (xsb - xsb*xtb - 2*xtb**2)/
     -  ((xsb + xtb)*
     -    RSQRTM(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df8dxsb

      function df8dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df8dxtb
      double precision xsb,xtb
      df8dxtb=        -((xsb**2*(1 + xtb))/
     -    (xtb*(xsb + xtb)*
     -      RSQRTM(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))))
      return
      end function df8dxtb

      function dlog_f9(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f9
      double precision xsb,xtb
      double complex twopii
      parameter (twopii=(0d0,6.28318530717958647692528676656d0))
      dlog_f9=        -LogP2((xsb*(-xsb - 4*xtb + xsb*xtb) - 
     -      RSQRTM(xsb*
     -         (xsb*(-1 + xtb)**2 - 4*xtb**2))*
     -       RSQRTP((-4 + xsb)*xsb))/
     -    (xsb*(-xsb - 4*xtb + xsb*xtb) + 
     -      RSQRTM(xsb*
     -         (xsb*(-1 + xtb)**2 - 4*xtb**2))*
     -       RSQRTP((-4 + xsb)*xsb)),
     -   (-4 + xsb)*xsb**2*
     -    (xsb*(-1 + xtb)**2 - 4*xtb**2))
      if(xsb.GT.0d0.and.xsb.LT.4d0.and.xtb.LT.0d0
     - .and.-xsb-4*xtb+xsb*xtb.ge.0d0)then
         dlog_f9=dlog_f9-twopii
      endif
      if(isnan(dreal(dlog_f9)))then
          dlog_f9=dcmplx(0d0,0d0)
      endif
      if(dreal(dlog_f9)-1d0.eq.dreal(dlog_f9))then
         ! we encounter the infinity
         dlog_f9=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f9

      function df9dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df9dxsb
      double precision xsb,xtb
      df9dxsb=        (xsb*(-1 + xtb) + 4*xtb)/
     -  (RSQRTM(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2))*
     -    RSQRTP((-4 + xsb)*xsb))
      return
      end function df9dxsb

      function df9dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df9dxtb
      double precision xsb,xtb
      df9dxtb=        (2*RSQRTP((-4 + xsb)*xsb))/
     -  RSQRTM(xsb*(xsb*(-1 + xtb)**2 - 4*xtb**2))
      return
      end function df9dxtb

      function dlog_f10(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f10
      double precision xsb,xtb
      dlog_f10= -RLogP(xsb)/2D0 + RLogP(xtb)/2D0 + 
     -  RLogP(xsb + xtb)/2D0
      if(isnan(dreal(dlog_f10))
     $     .or.dreal(dlog_f10)-1d0.eq.dreal(dlog_f10))then
         ! nan or infinite
          dlog_f10=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f10

      function df10dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df10dxsb
      double precision xsb,xtb
      df10dxsb=-xtb/(2D0*xsb*(xsb + xtb))
      return
      end function df10dxsb

      function df10dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df10dxtb
      double precision xsb,xtb
      df10dxtb=(1D0/xtb+1D0/(xsb+xtb))/2D0
      return
      end function df10dxtb

      function dlog_f11(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f11
      double precision xsb,xtb
      if(abs(xsb).le.zero_thr)then
         dlog_f11=-RLogM(xsb/(16d0*xtb**2))
      else
         dlog_f11=-LogM2((xsb*xtb*
     -        (-3*xsb - 4*xtb + xsb*xtb) - 
     -        RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)*RSQRTM(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2)))/
     -        (xsb*xtb*(-3*xsb - 4*xtb + xsb*xtb) + 
     -        RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)*RSQRTM(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))),
     -        xsb**2*(xsb*(-4 + xtb) - 4*xtb)*xtb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))
      endif
      if(isnan(dreal(dlog_f11))
     $     .or.dreal(dlog_f11)-1d0.eq.dreal(dlog_f11))then
         ! nan or infinite
          dlog_f11=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f11

      function df11dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df11dxsb
      double precision xsb,xtb
      df11dxsb=        (xtb**2*(4*xtb + xsb*(5 + xtb)))/
     -  ((xsb + xtb)*
     -    RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -      xtb)*RSQRTM(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df11dxsb

      function df11dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df11dxtb
      double precision xsb,xtb
      df11dxtb=        (xsb*(3*xsb**2*(-1 + xtb) + 
     -      2*xsb*(-6 + xtb)*xtb - 8*xtb**2))/
     -  ((xsb + xtb)*
     -    RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -      xtb)*RSQRTM(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df11dxtb

      function dlog_f12(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f12
      double precision xsb,xtb
      dlog_f12=        LogP2(((-4*xsb + xsb**2)*xtb - 
     -     RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)*RSQRTP((-4 + xsb)*xsb))/
     -   ((-4*xsb + xsb**2)*xtb + 
     -     RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)*RSQRTP((-4 + xsb)*xsb)),
     -  (-4 + xsb)*xsb**2*
     -   (xsb*(-4 + xtb) - 4*xtb)*xtb)
      if(isnan(dreal(dlog_f12)))then
          dlog_f12=dcmplx(0d0,0d0)
      endif
      if(dreal(dlog_f12)-1d0.eq.dreal(dlog_f12))then
          ! we encounter the infinity
          dlog_f12=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f12

      function df12dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df12dxsb
      double precision xsb,xtb
      df12dxsb=        (-4*xtb)/
     -  (RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*xtb)*
     -    RSQRTP((-4 + xsb)*xsb))
      return
      end function df12dxsb

      function df12dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df12dxtb
      double precision xsb,xtb
      df12dxtb=        -(RSQRTP((-4 + xsb)*xsb)/
     -    RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*xtb)
     -    )
      return
      end function df12dxtb

      function dlog_f13(xsb,xtb)
      use func_psi
      implicit none
      double complex dlog_f13
      double precision xsb,xtb
      dlog_f13=        -LogP2((xsb*xtb - 
     -      RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb))/
     -    (xsb*xtb + 
     -      RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)),xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -    xtb)
      if(isnan(dreal(dlog_f13))
     $     .or.dreal(dlog_f13)-1d0.eq.dreal(dlog_f13))then
         ! nan or infinite
          dlog_f13=dcmplx(0d0,0d0)
      endif
      return
      end function dlog_f13

      function df13dxsb(xsb,xtb)
      use func_psi
      implicit none
      double complex df13dxsb
      double precision xsb,xtb
      df13dxsb=        xtb**2/
     -  ((xsb + xtb)*
     -    RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*xtb)
     -    )
      return
      end function df13dxsb

      function df13dxtb(xsb,xtb)
      use func_psi
      implicit none
      double complex df13dxtb
      double precision xsb,xtb
      df13dxtb=        xsb**2/
     -  ((xsb + xtb)*
     -    RSQRTM(xsb*(xsb*(-4 + xtb) - 4*xtb)*xtb)
     -    )
      return
      end function df13dxtb

      subroutine evaluate_boundaryconstants_Phys12(xsb0,xtb0)
      use calc_IterInts_RootMIs_boundaryconstants
      implicit none
      double precision xsb0,xtb0
      double complex cres(19)
      save cres
      double precision xsb0_save, xtb0_save
      data xsb0_save /0d0/
      data xtb0_save /0d0/
      save xsb0_save, xtb0_save
      if(xtb0.ne.0d0.or.xsb0.le.0d0)then
         write(*,*)"Error: xtb0 =!= 0 or xsb0 <= 0 in Phys 1 or 2"
         stop
      endif
      if(.not.(xsb0.eq.xsb0_save.and.xtb0.eq.xtb0_save))then
         ! evaluate boundary constants
         call get_boundaryconstants_Phys12(xsb0,cres)
      endif
      xsb0_save=xsb0
      xtb0_save=xtb0
      if(xsb0.GE.4d0)then
         ! Phys 1
         xsb0_Phys1=xsb0
         xtb0_Phys1=xtb0
         B2A2_Phys1=cres(2)
         B4A3_Phys1=cres(3)
         B10A3_Phys1=cres(4)
         B11A2_Phys1=cres(5)
         B18A3_Phys1=cres(6)
         B20A3_Phys1=cres(7)
         B23A3_Phys1=cres(8)
         B26A4_Phys1=cres(9)
         B28A4_Phys1=cres(10)
         B29A4_Phys1=cres(11)
         B3A2_Phys1=cres(12)
         B3A3_Phys1=cres(13)
         B4A2_Phys1=cres(14)
         B5A3_Phys1=cres(15)
         B6A3_Phys1=cres(16)
         B7A2_Phys1=cres(17)
         B8A3_Phys1=cres(18)
         B9A3_Phys1=cres(19)
      else
         ! Phys 2
         xsb0_Phys2=xsb0
         xtb0_Phys2=xtb0
         B2A2_Phys2=cres(2)
         B4A3_Phys2=cres(3)
         B10A3_Phys2=cres(4)
         B11A2_Phys2=cres(5)
         B18A3_Phys2=cres(6)
         B20A3_Phys2=cres(7)
         B23A3_Phys2=cres(8)
         B26A4_Phys2=cres(9)
         B28A4_Phys2=cres(10)
         B29A4_Phys2=cres(11)
         B3A2_Phys2=cres(12)
         B3A3_Phys2=cres(13)
         B4A2_Phys2=cres(14)
         B5A3_Phys2=cres(15)
         B6A3_Phys2=cres(16)
         B7A2_Phys2=cres(17)
         B8A3_Phys2=cres(18)
         B9A3_Phys2=cres(19)
      endif
      return
      end subroutine

      subroutine evaluate_boundaryconstants_Phys34(xsb0,xtb0)
      use calc_IterInts_RootMIs_boundaryconstants
      implicit none
      double precision xsb0,xtb0
      double complex cres(6)
      save cres
      double precision xsb0_save, xtb0_save
      data xsb0_save /0d0/
      data xtb0_save /0d0/
      save xsb0_save, xtb0_save
      if(xsb0.ne.0d0.or.xtb0.le.0d0)then
         write(*,*)"Error: xsb0 =!= 0 or xtb0 <= 0 in Phys 3, 4"
         stop
      endif
      if(.not.(xsb0.eq.xsb0_save.and.xtb0.eq.xtb0_save))then
         ! evaluate boundary constants
         call get_boundaryconstants_Phys34(xtb0,cres)
      endif
      xsb0_save=xsb0
      xtb0_save=xtb0
      if(xtb0.GE.4d0)then
         ! Phys 3
         xsb0_Phys3=xsb0
         xtb0_Phys3=xtb0
         B14A3_Phys3=cres(1)
         B16A2_Phys3=cres(2)
         B16A3_Phys3=cres(3)
         B23A3_Phys3=cres(4)
         B26A4_Phys3=cres(5)
         B14A2_Phys3=cres(6)
      else
         ! Phys 4
         xsb0_Phys4=xsb0
         xtb0_Phys4=xtb0
         B14A3_Phys4=cres(1)
         B16A2_Phys4=cres(2)
         B16A3_Phys4=cres(3)
         B23A3_Phys4=cres(4)
         B26A4_Phys4=cres(5)
         B14A2_Phys4=cres(6)
      endif
      return
      end subroutine

      function fE18_w3_qp(xsb,xtb)
      implicit none
      complex*32 fE18_w3_qp
      real*16 xsb,xtb
      if(xsb.LT.0E0_16.and.xtb.LT.0E0_16)then
         ! Euclid
         fE18_w3_qp=fE18Euclid_w3_qp(xsb,xtb)
      elseif(xsb.GT.4E0_16.and.xtb.LT.0E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE18_w3_qp=fE18Phys1_w3_qp(xsb,xtb)
      elseif(xsb.GT.0E0_16.and.xsb.LT.4E0_16.and.xtb.LT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE18_w3_qp=fE18Phys2_w3_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.GT.4E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE18_w3_qp=fE18Phys3_w3_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.LT.4E0_16.and.xtb.GT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE18_w3_qp=fE18Phys4_w3_qp(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE25_w4_qp(xsb,xtb)
      implicit none
      complex*32 fE25_w4_qp
      real*16 xsb,xtb
      if(xsb.LT.0E0_16.and.xtb.LT.0E0_16)then
         ! Euclid
         fE25_w4_qp=fE25Euclid_w4_qp(xsb,xtb)
      elseif(xsb.GT.4E0_16.and.xtb.LT.0E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE25_w4_qp=fE25Phys1_w4_qp(xsb,xtb)
      elseif(xsb.GT.0E0_16.and.xsb.LT.4E0_16.and.xtb.LT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE25_w4_qp=fE25Phys2_w4_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.GT.4E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE25_w4_qp=fE25Phys3_w4_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.LT.4E0_16.and.xtb.GT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE25_w4_qp=fE25Phys4_w4_qp(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE26_w4_qp(xsb,xtb)
      implicit none
      complex*32 fE26_w4_qp
      real*16 xsb,xtb
      if(xsb.LT.0E0_16.and.xtb.LT.0E0_16)then
         ! Euclid
         fE26_w4_qp=fE26Euclid_w4_qp(xsb,xtb)
      elseif(xsb.GT.4E0_16.and.xtb.LT.0E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE26_w4_qp=fE26Phys1_w4_qp(xsb,xtb)
      elseif(xsb.GT.0E0_16.and.xsb.LT.4E0_16.and.xtb.LT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE26_w4_qp=fE26Phys2_w4_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.GT.4E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE26_w4_qp=fE26Phys3_w4_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.LT.4E0_16.and.xtb.GT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE26_w4_qp=fE26Phys4_w4_qp(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE28_w4_qp(xsb,xtb)
      implicit none
      complex*32 fE28_w4_qp
      real*16 xsb,xtb
      if(xsb.LT.0E0_16.and.xtb.LT.0E0_16)then
         ! Euclid
         fE28_w4_qp=fE28Euclid_w4_qp(xsb,xtb)
      elseif(xsb.GT.4E0_16.and.xtb.LT.0E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE28_w4_qp=fE28Phys1_w4_qp(xsb,xtb)
      elseif(xsb.GT.0E0_16.and.xsb.LT.4E0_16.and.xtb.LT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE28_w4_qp=fE28Phys2_w4_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.GT.4E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE28_w4_qp=fE28Phys3_w4_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.LT.4E0_16.and.xtb.GT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE28_w4_qp=fE28Phys4_w4_qp(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      function fE29_w4_qp(xsb,xtb)
      implicit none
      complex*32 fE29_w4_qp
      real*16 xsb,xtb
      if(xsb.LT.0E0_16.and.xtb.LT.0E0_16)then
         ! Euclid
         fE29_w4_qp=fE29Euclid_w4_qp(xsb,xtb)
      elseif(xsb.GT.4E0_16.and.xtb.LT.0E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys1
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE29_w4_qp=fE29Phys1_w4_qp(xsb,xtb)
      elseif(xsb.GT.0E0_16.and.xsb.LT.4E0_16.and.xtb.LT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys2
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys12_qp(xsb,0E0_16)
         endif
         fE29_w4_qp=fE29Phys2_w4_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.GT.4E0_16.and.xsb+xtb.GT.0E0_16)then
         ! Phys3
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE29_w4_qp=fE29Phys3_w4_qp(xsb,xtb)
      elseif(xsb.LT.0E0_16.and.xtb.LT.4E0_16.and.xtb.GT.0E0_16
     $        .and.xsb+xtb.GT.0E0_16)then
         ! Phys4
         if(boundary_point_scheme.eq.2)then
            ! use the dynamical one
            call evaluate_boundaryconstants_Phys34_qp(0E0_16,xtb)
         endif
         fE29_w4_qp=fE29Phys4_w4_qp(xsb,xtb)
      else
         write(*,*)"ERROR: do not cover (xsb,xtb)=",xsb,xtb
         stop
      endif
      return
      end

      ! quadruple precision
      function fE18Euclid_w3_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE18Euclid_w3_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      CALL SetPath_st_qp(xsb,xsb0_Euclid_qp,xtb,xtb0_Euclid_qp)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE18Euclid_w3_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE18Euclid_w3_qp_fxn,
     $        one,integral)
      endif
      fE18Euclid_w3_qp=integral
      return
      end function fE18Euclid_w3_qp

      function fE18Euclid_w3_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE18Euclid_w3_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Euclid_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp.or.abs(xsb).lt.zero_thr_qp)then
         fE18Euclid_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(HALF*(logs(2)**TWO + 8*logs(8)**TWO)))
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-logs(2)**TWO + 6*logs(8)**TWO)/4E0_16)
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -li2s(2) - li2s(3) - li2s(4) - li2s(5) + 
     -  li2s(6) + li2s(7) + li2s(8) + li2s(9) + 
     -  logs(1)*logs(5) + logs(3)*logs(6) - 
     -  logs(4)*logs(7))
      fE18Euclid_w3_qp_fxn=res
      return
      end function fE18Euclid_w3_qp_fxn

      function fE25Euclid_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE25Euclid_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(3)
      common /fE25Euclid_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Euclid_qp,xtb,xtb0_Euclid_qp)
      fteq1(1)=dlog_f11_qp(xsb,xtb)
      fteq1(2)=dlog_f12_qp(xsb,xtb)
      fteq1(3)=dlog_f13_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE25Euclid_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE25Euclid_w4_qp_fxn,
     $        one,integral)
      endif
      fE25Euclid_w4_qp=integral
      return
      end function fE25Euclid_w4_qp

      function fE25Euclid_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE25Euclid_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(3)
      common /fE25Euclid_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Euclid_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp.or.abs(xsb).lt.zero_thr_qp)then
         fE25Euclid_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((logs(2)**TWO + 8*logs(8)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((logs(2)**TWO - 6*logs(8)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(2)
      f1=dlog_f12_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(5*logs(2)**TWO - 6*logs(8)**TWO))/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((9*logs(2)**TWO + 22*logs(8)**TWO)/4E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-9*(logs(2)**TWO + logs(8)**TWO))/2E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2E0_16)
      fE25Euclid_w4_qp_fxn=res
      return
      end function fE25Euclid_w4_qp_fxn

      function fE26Euclid_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE26Euclid_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(5)
      common /fE26Euclid_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Euclid_qp,xtb,xtb0_Euclid_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f7_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE26Euclid_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE26Euclid_w4_qp_fxn,
     $        one,integral)
      endif
      fE26Euclid_w4_qp=integral
      return
      end function fE26Euclid_w4_qp

      function fE26Euclid_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE26Euclid_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE26Euclid_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Euclid_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp.or.abs(xsb).lt.zero_thr_qp)then
         fE26Euclid_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(logs(2)**TWO - TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*logs(2)**TWO + 5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(FOUR*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-logs(2)**TWO + 6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(2) - li2s(3) - li2s(4) - li2s(5) + 
     -  li2s(6) + li2s(7) + li2s(8) + li2s(9) + 
     -  logs(1)*logs(5) + logs(3)*logs(6) - 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-7*logs(2)**TWO + 10*logs(8)**TWO)/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-5*(logs(2)**TWO + TWO*logs(8)**TWO))/2E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(5*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Euclid_w4_qp_fxn=res
      return
      end function fE26Euclid_w4_qp_fxn

      function fE28Euclid_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE28Euclid_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(2)
      common /fE28Euclid_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Euclid_qp,xtb,xtb0_Euclid_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f9_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE28Euclid_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE28Euclid_w4_qp_fxn,
     $        one,integral)
      endif
      fE28Euclid_w4_qp=integral
      return
      end function fE28Euclid_w4_qp

      function fE28Euclid_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE28Euclid_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(2)
      common /fE28Euclid_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Euclid_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp.or.abs(xsb).lt.zero_thr_qp)then
         fE28Euclid_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((TWO*RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(-((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb_qp(xsb,xtb)
      dfdxtb=df2dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(5*logs(3)**TWO - TWO*logs(9)**TWO))/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -    li2s(6) + li2s(7) + li2s(8) + li2s(9) + 
     -    li2s(10) + logs(1)*logs(6) + 
     -    logs(4)*logs(7) - logs(5)*logs(8)))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(logs(3)**TWO + 8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((logs(3)**TWO - 6*logs(9)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(3) + li2s(4) + li2s(5) + li2s(6) - 
     -  li2s(7) - li2s(8) - li2s(9) - li2s(10) - 
     -  logs(1)*logs(6) - logs(4)*logs(7) + 
     -  logs(5)*logs(8))
      fE28Euclid_w4_qp_fxn=res
      return
      end function fE28Euclid_w4_qp_fxn

      function fE29Euclid_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE29Euclid_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(5)
      common /fE29Euclid_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Euclid_qp,xtb,xtb0_Euclid_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f8_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE29Euclid_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE29Euclid_w4_qp_fxn,
     $        one,integral)
      endif
      fE29Euclid_w4_qp=integral
      return
      end function fE29Euclid_w4_qp

      function fE29Euclid_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE29Euclid_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE29Euclid_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Euclid_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      CALL GetPath_Euclid_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp.or.abs(xsb).lt.zero_thr_qp)then
         fE29Euclid_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(logs(2)**TWO + TWO*logs(8)**TWO))/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(logs(2)**TWO - TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-logs(2)**TWO - 6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-logs(2)**TWO + 6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(2) - li2s(3) - li2s(4) - li2s(5) + 
     -  li2s(6) + li2s(7) + li2s(8) + li2s(9) + 
     -  logs(1)*logs(5) + logs(3)*logs(6) - 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(logs(2)**TWO + TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-logs(2)**TWO + TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      fE29Euclid_w4_qp_fxn=res
      return
      end function fE29Euclid_w4_qp_fxn

      function fE18Phys1_w3_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE18Phys1_w3_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B18A3
      B18A3=B18A3_Phys1_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys1_qp,xtb,xtb0_Phys1_qp)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE18Phys1_w3_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE18Phys1_w3_qp_fxn,
     $        one,integral)
      endif
      fE18Phys1_w3_qp=integral
      fE18Phys1_w3_qp=fE18Phys1_w3_qp+
     - +B18A3
      return
      end function fE18Phys1_w3_qp

      function fE18Phys1_w3_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE18Phys1_w3_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B3A2
      complex*32 B4A2
      complex*32 B18A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Phys1_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B3A2=B3A2_Phys1_qp
      B4A2=B4A2_Phys1_qp
      B18A3=B18A3_Phys1_qp
      CALL GetPath_Phys1_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE18Phys1_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      fE18Phys1_w3_qp_fxn=res
      return
      end function fE18Phys1_w3_qp_fxn

      function fE25Phys1_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE25Phys1_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(3)
      common /fE25Phys1_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Phys1_qp,xtb,xtb0_Phys1_qp)
      fteq1(1)=dlog_f11_qp(xsb,xtb)
      fteq1(2)=dlog_f12_qp(xsb,xtb)
      fteq1(3)=dlog_f13_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE25Phys1_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE25Phys1_w4_qp_fxn,
     $        one,integral)
      endif
      fE25Phys1_w4_qp=integral
      return
      end function fE25Phys1_w4_qp

      function fE25Phys1_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE25Phys1_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B3A2
      complex*32 B4A2
      complex*32 B5A3
      complex*32 B10A3
      complex*32 B18A3
      complex*32 B20A3
      complex*32 B23A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(3)
      common /fE25Phys1_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Phys1_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B3A2=B3A2_Phys1_qp
      B4A2=B4A2_Phys1_qp
      B5A3=B5A3_Phys1_qp
      B10A3=B10A3_Phys1_qp
      B18A3=B18A3_Phys1_qp
      B20A3=B20A3_Phys1_qp
      B23A3=B23A3_Phys1_qp
      CALL GetPath_Phys1_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE25Phys1_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B18A3*HALF))
      dfdxsb=df12dxsb_qp(xsb,xtb)
      dfdxtb=df12dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B5A3)
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B10A3 + B20A3 - 8*B23A3)*HALF)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO + logs(2)**TWO + 
     -    8*logs(8)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + logs(2)**TWO - 
     -    6*logs(8)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(2)
      f1=dlog_f12_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(B3A2 - B4A2*TWO + THREE*logs(2)**TWO))
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO + 15*logs(2)**TWO - 
     -    18*logs(8)**TWO)/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + 9*logs(2)**TWO + 
     -    22*logs(8)**TWO)/4E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-9*(logs(2)**TWO + logs(8)**TWO))/2E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2E0_16)
      fE25Phys1_w4_qp_fxn=res
      return
      end function fE25Phys1_w4_qp_fxn

      function fE26Phys1_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE26Phys1_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B26A4
      complex*32 fteq1(5)
      common /fE26Phys1_w4_qp_ft/ fteq1
      B26A4=B26A4_Phys1_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys1_qp,xtb,xtb0_Phys1_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f7_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE26Phys1_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE26Phys1_w4_qp_fxn,
     $        one,integral)
      endif
      fE26Phys1_w4_qp=integral
      fE26Phys1_w4_qp=fE26Phys1_w4_qp+
     - +B26A4
      return
      end function fE26Phys1_w4_qp

      function fE26Phys1_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE26Phys1_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B3A2
      complex*32 B4A2
      complex*32 B5A3
      complex*32 B6A3
      complex*32 B8A3
      complex*32 B10A3
      complex*32 B18A3
      complex*32 B20A3
      complex*32 B23A3
      complex*32 B26A4
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE26Phys1_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Phys1_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B3A2=B3A2_Phys1_qp
      B4A2=B4A2_Phys1_qp
      B5A3=B5A3_Phys1_qp
      B6A3=B6A3_Phys1_qp
      B8A3=B8A3_Phys1_qp
      B10A3=B10A3_Phys1_qp
      B18A3=B18A3_Phys1_qp
      B20A3=B20A3_Phys1_qp
      B23A3=B23A3_Phys1_qp
      B26A4=B26A4_Phys1_qp
      CALL GetPath_Phys1_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE26Phys1_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B5A3*HALF))
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B6A3 - B8A3*FOUR - B10A3*TWO)/4E0_16)
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(HALF*(-8*B23A3 + B6A3 + B8A3*FOUR + B10A3*TWO))
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B18A3)
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B10A3 - B20A3 + B23A3*FOUR)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-B3A2 + B4A2*TWO - THREE*logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(logs(2)**TWO - TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*logs(2)**TWO + 5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(FOUR*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO - 7*logs(2)**TWO + 
     -    10*logs(8)**TWO)/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-B3A2 + B4A2*TWO + 
     -      5*(logs(2)**TWO + 2*logs(8)**TWO))))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(5*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Phys1_w4_qp_fxn=res
      return
      end function fE26Phys1_w4_qp_fxn

      function fE28Phys1_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE28Phys1_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B28A4
      complex*32 fteq1(2)
      common /fE28Phys1_w4_qp_ft/ fteq1
      B28A4=B28A4_Phys1_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys1_qp,xtb,xtb0_Phys1_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f9_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE28Phys1_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE28Phys1_w4_qp_fxn,
     $        one,integral)
      endif
      fE28Phys1_w4_qp=integral
      fE28Phys1_w4_qp=fE28Phys1_w4_qp+
     - +B28A4
      return
      end function fE28Phys1_w4_qp

      function fE28Phys1_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE28Phys1_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B2A2
      complex*32 B3A2
      complex*32 B3A3
      complex*32 B4A2
      complex*32 B4A3
      complex*32 B6A3
      complex*32 B7A2
      complex*32 B8A3
      complex*32 B9A3
      complex*32 B10A3
      complex*32 B11A2
      complex*32 B18A3
      complex*32 B20A3
      complex*32 B23A3
      complex*32 B28A4
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(2)
      common /fE28Phys1_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Phys1_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B2A2=B2A2_Phys1_qp
      B3A2=B3A2_Phys1_qp
      B3A3=B3A3_Phys1_qp
      B4A2=B4A2_Phys1_qp
      B4A3=B4A3_Phys1_qp
      B6A3=B6A3_Phys1_qp
      B7A2=B7A2_Phys1_qp
      B8A3=B8A3_Phys1_qp
      B9A3=B9A3_Phys1_qp
      B10A3=B10A3_Phys1_qp
      B11A2=B11A2_Phys1_qp
      B18A3=B18A3_Phys1_qp
      B20A3=B20A3_Phys1_qp
      B23A3=B23A3_Phys1_qp
      B28A4=B28A4_Phys1_qp
      CALL GetPath_Phys1_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE28Phys1_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((TWO*RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(-((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        HALF*(-8*B23A3 + 5*B6A3 - B10A3*FOUR + 
     -    B8A3*FOUR + B9A3*FOUR - B20A3*TWO + 
     -    B3A3*TWO + B4A3*TWO))
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B18A3)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-32*B11A2 - 48*B2A2 - 8*B7A2 + 
     -    32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb_qp(xsb,xtb)
      dfdxtb=df2dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(-B3A2 + B4A2*TWO + 5*logs(3)**TWO - 
     -      TWO*logs(9)**TWO))/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + 
     -    THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -      li2s(6) + li2s(7) + li2s(8) + 
     -      li2s(9) + li2s(10) + 
     -      logs(1)*logs(6) + logs(4)*logs(7) - 
     -      logs(5)*logs(8))))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + logs(3)**TWO + 
     -    8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + logs(3)**TWO - 
     -    6*logs(9)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(3) - li2s(4) - li2s(5) - li2s(6) + 
     -  li2s(7) + li2s(8) + li2s(9) + li2s(10) + 
     -  logs(1)*logs(6) + logs(4)*logs(7) - 
     -  logs(5)*logs(8))
      fE28Phys1_w4_qp_fxn=res
      return
      end function fE28Phys1_w4_qp_fxn

      function fE29Phys1_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE29Phys1_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B29A4
      complex*32 fteq1(5)
      common /fE29Phys1_w4_qp_ft/ fteq1
      B29A4=B29A4_Phys1_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys1_qp,xtb,xtb0_Phys1_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f8_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE29Phys1_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE29Phys1_w4_qp_fxn,
     $        one,integral)
      endif
      fE29Phys1_w4_qp=integral
      fE29Phys1_w4_qp=fE29Phys1_w4_qp+
     - +B29A4
      return
      end function fE29Phys1_w4_qp

      function fE29Phys1_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE29Phys1_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B3A2
      complex*32 B4A2
      complex*32 B5A3
      complex*32 B6A3
      complex*32 B8A3
      complex*32 B10A3
      complex*32 B18A3
      complex*32 B20A3
      complex*32 B23A3
      complex*32 B29A4
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE29Phys1_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Phys1_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B3A2=B3A2_Phys1_qp
      B4A2=B4A2_Phys1_qp
      B5A3=B5A3_Phys1_qp
      B6A3=B6A3_Phys1_qp
      B8A3=B8A3_Phys1_qp
      B10A3=B10A3_Phys1_qp
      B18A3=B18A3_Phys1_qp
      B20A3=B20A3_Phys1_qp
      B23A3=B23A3_Phys1_qp
      B29A4=B29A4_Phys1_qp
      CALL GetPath_Phys1_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE29Phys1_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B5A3)
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        HALF*(-8*B23A3 + 12*B8A3 + B10A3*FOUR + 
     -    B6A3*THREE - B20A3*TWO))
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B6A3 + B23A3*FOUR - B8A3*FOUR + B20A3*TWO)
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B18A3)
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B10A3 - B20A3)*TWO)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(B3A2 - B4A2*TWO + THREE*logs(2)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO - THREE*logs(2)**TWO - 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + 
     -    THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(li2s(2) + li2s(3) + li2s(4) + 
     -    li2s(5) - li2s(6) - li2s(7) - li2s(8) - 
     -    li2s(9) - logs(1)*logs(5) - 
     -    logs(3)*logs(6) + logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-B3A2 + B4A2*TWO + logs(2)**TWO - 
     -      TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-B3A2 + B4A2*TWO - logs(2)**TWO - 6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-B3A2 + B4A2*TWO + logs(2)**TWO + 
     -    TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(B3A2 - B4A2*TWO - logs(2)**TWO + TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE29Phys1_w4_qp_fxn=res
      return
      end function fE29Phys1_w4_qp_fxn

      function fE18Phys2_w3_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE18Phys2_w3_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B18A3
      B18A3=B18A3_Phys2_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys2_qp,xtb,xtb0_Phys2_qp)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE18Phys2_w3_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE18Phys2_w3_qp_fxn,
     $        one,integral)
      endif
      fE18Phys2_w3_qp=integral
      fE18Phys2_w3_qp=fE18Phys2_w3_qp+
     - +B18A3
      return
      end function fE18Phys2_w3_qp

      function fE18Phys2_w3_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE18Phys2_w3_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B3A2
      complex*32 B4A2
      complex*32 B18A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Phys2_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B3A2=B3A2_Phys2_qp
      B4A2=B4A2_Phys2_qp
      B18A3=B18A3_Phys2_qp
      CALL GetPath_Phys2_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE18Phys2_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      fE18Phys2_w3_qp_fxn=res
      return
      end function fE18Phys2_w3_qp_fxn

      function fE25Phys2_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE25Phys2_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(3)
      common /fE25Phys2_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Phys2_qp,xtb,xtb0_Phys2_qp)
      fteq1(1)=dlog_f11_qp(xsb,xtb)
      fteq1(2)=dlog_f12_qp(xsb,xtb)
      fteq1(3)=dlog_f13_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE25Phys2_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE25Phys2_w4_qp_fxn,
     $        one,integral)
      endif
      fE25Phys2_w4_qp=integral
      return
      end function fE25Phys2_w4_qp

      function fE25Phys2_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE25Phys2_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B3A2
      complex*32 B4A2
      complex*32 B5A3
      complex*32 B10A3
      complex*32 B18A3
      complex*32 B20A3
      complex*32 B23A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(3)
      common /fE25Phys2_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Phys2_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B3A2=B3A2_Phys2_qp
      B4A2=B4A2_Phys2_qp
      B5A3=B5A3_Phys2_qp
      B10A3=B10A3_Phys2_qp
      B18A3=B18A3_Phys2_qp
      B20A3=B20A3_Phys2_qp
      B23A3=B23A3_Phys2_qp
      CALL GetPath_Phys2_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE25Phys2_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B18A3*HALF))
      dfdxsb=df12dxsb_qp(xsb,xtb)
      dfdxtb=df12dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B5A3)
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B10A3 + B20A3 - 8*B23A3)*HALF)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO + logs(2)**TWO + 
     -    8*logs(8)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + logs(2)**TWO - 
     -    6*logs(8)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(2)
      f1=dlog_f12_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(B3A2 - B4A2*TWO + THREE*logs(2)**TWO))
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO + 15*logs(2)**TWO - 
     -    18*logs(8)**TWO)/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + 9*logs(2)**TWO + 
     -    22*logs(8)**TWO)/4E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-9*(logs(2)**TWO + logs(8)**TWO))/2E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2E0_16)
      fE25Phys2_w4_qp_fxn=res
      return
      end function fE25Phys2_w4_qp_fxn

      function fE26Phys2_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE26Phys2_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B26A4
      complex*32 fteq1(5)
      common /fE26Phys2_w4_qp_ft/ fteq1
      B26A4=B26A4_Phys2_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys2_qp,xtb,xtb0_Phys2_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f7_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE26Phys2_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE26Phys2_w4_qp_fxn,
     $        one,integral)
      endif
      fE26Phys2_w4_qp=integral
      fE26Phys2_w4_qp=fE26Phys2_w4_qp+
     - +B26A4
      return
      end function fE26Phys2_w4_qp

      function fE26Phys2_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE26Phys2_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B3A2
      complex*32 B4A2
      complex*32 B5A3
      complex*32 B6A3
      complex*32 B8A3
      complex*32 B10A3
      complex*32 B18A3
      complex*32 B20A3
      complex*32 B23A3
      complex*32 B26A4
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE26Phys2_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Phys2_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B3A2=B3A2_Phys2_qp
      B4A2=B4A2_Phys2_qp
      B5A3=B5A3_Phys2_qp
      B6A3=B6A3_Phys2_qp
      B8A3=B8A3_Phys2_qp
      B10A3=B10A3_Phys2_qp
      B18A3=B18A3_Phys2_qp
      B20A3=B20A3_Phys2_qp
      B23A3=B23A3_Phys2_qp
      B26A4=B26A4_Phys2_qp
      CALL GetPath_Phys2_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE26Phys2_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B5A3*HALF))
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B6A3 - B8A3*FOUR - B10A3*TWO)/4E0_16)
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(HALF*(-8*B23A3 + B6A3 + B8A3*FOUR + B10A3*TWO))
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B18A3)
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B10A3 - B20A3 + B23A3*FOUR)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-B3A2 + B4A2*TWO - THREE*logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(logs(2)**TWO - TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*logs(2)**TWO + 5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(FOUR*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO - 7*logs(2)**TWO + 
     -    10*logs(8)**TWO)/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-B3A2 + B4A2*TWO + 
     -      5*(logs(2)**TWO + 2*logs(8)**TWO))))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(5*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Phys2_w4_qp_fxn=res
      return
      end function fE26Phys2_w4_qp_fxn

      function fE28Phys2_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE28Phys2_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B28A4
      complex*32 fteq1(2)
      common /fE28Phys2_w4_qp_ft/ fteq1
      B28A4=B28A4_Phys2_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys2_qp,xtb,xtb0_Phys2_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f9_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE28Phys2_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE28Phys2_w4_qp_fxn,
     $        one,integral)
      endif
      fE28Phys2_w4_qp=integral
      fE28Phys2_w4_qp=fE28Phys2_w4_qp+
     - +B28A4
      return
      end function fE28Phys2_w4_qp

      function fE28Phys2_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE28Phys2_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B2A2
      complex*32 B3A2
      complex*32 B3A3
      complex*32 B4A2
      complex*32 B4A3
      complex*32 B6A3
      complex*32 B7A2
      complex*32 B8A3
      complex*32 B9A3
      complex*32 B10A3
      complex*32 B11A2
      complex*32 B18A3
      complex*32 B20A3
      complex*32 B23A3
      complex*32 B28A4
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(2)
      common /fE28Phys2_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Phys2_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B2A2=B2A2_Phys2_qp
      B3A2=B3A2_Phys2_qp
      B3A3=B3A3_Phys2_qp
      B4A2=B4A2_Phys2_qp
      B4A3=B4A3_Phys2_qp
      B6A3=B6A3_Phys2_qp
      B7A2=B7A2_Phys2_qp
      B8A3=B8A3_Phys2_qp
      B9A3=B9A3_Phys2_qp
      B10A3=B10A3_Phys2_qp
      B11A2=B11A2_Phys2_qp
      B18A3=B18A3_Phys2_qp
      B20A3=B20A3_Phys2_qp
      B23A3=B23A3_Phys2_qp
      B28A4=B28A4_Phys2_qp
      CALL GetPath_Phys2_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE28Phys2_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((TWO*RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(-((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        HALF*(-8*B23A3 + 5*B6A3 - B10A3*FOUR + 
     -    B8A3*FOUR + B9A3*FOUR - B20A3*TWO + 
     -    B3A3*TWO + B4A3*TWO))
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B18A3)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-32*B11A2 - 48*B2A2 - 8*B7A2 + 
     -    32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb_qp(xsb,xtb)
      dfdxtb=df2dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(-B3A2 + B4A2*TWO + 5*logs(3)**TWO - 
     -      TWO*logs(9)**TWO))/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + 
     -    THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -      li2s(6) + li2s(7) + li2s(8) + 
     -      li2s(9) + li2s(10) + 
     -      logs(1)*logs(6) + logs(4)*logs(7) - 
     -      logs(5)*logs(8))))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + logs(3)**TWO + 
     -    8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO + logs(3)**TWO - 
     -    6*logs(9)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(3) - li2s(4) - li2s(5) - li2s(6) + 
     -  li2s(7) + li2s(8) + li2s(9) + li2s(10) + 
     -  logs(1)*logs(6) + logs(4)*logs(7) - 
     -  logs(5)*logs(8))
      fE28Phys2_w4_qp_fxn=res
      return
      end function fE28Phys2_w4_qp_fxn

      function fE29Phys2_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE29Phys2_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B29A4
      complex*32 fteq1(5)
      common /fE29Phys2_w4_qp_ft/ fteq1
      B29A4=B29A4_Phys2_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys2_qp,xtb,xtb0_Phys2_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f8_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE29Phys2_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE29Phys2_w4_qp_fxn,
     $        one,integral)
      endif
      fE29Phys2_w4_qp=integral
      fE29Phys2_w4_qp=fE29Phys2_w4_qp+
     - +B29A4
      return
      end function fE29Phys2_w4_qp

      function fE29Phys2_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE29Phys2_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B3A2
      complex*32 B4A2
      complex*32 B5A3
      complex*32 B6A3
      complex*32 B8A3
      complex*32 B10A3
      complex*32 B18A3
      complex*32 B20A3
      complex*32 B23A3
      complex*32 B29A4
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE29Phys2_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Phys2_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B3A2=B3A2_Phys2_qp
      B4A2=B4A2_Phys2_qp
      B5A3=B5A3_Phys2_qp
      B6A3=B6A3_Phys2_qp
      B8A3=B8A3_Phys2_qp
      B10A3=B10A3_Phys2_qp
      B18A3=B18A3_Phys2_qp
      B20A3=B20A3_Phys2_qp
      B23A3=B23A3_Phys2_qp
      B29A4=B29A4_Phys2_qp
      CALL GetPath_Phys2_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xtb).lt.zero_thr_qp
     $     .or.abs(xsb-4E0_16).lt.zero_thr_qp)then
         fE29Phys2_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B5A3)
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        HALF*(-8*B23A3 + 12*B8A3 + B10A3*FOUR + 
     -    B6A3*THREE - B20A3*TWO))
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B6A3 + B23A3*FOUR - B8A3*FOUR + B20A3*TWO)
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B18A3)
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B10A3 - B20A3)*TWO)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*(B3A2 - B4A2*TWO + THREE*logs(2)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-B3A2 + B4A2*TWO - THREE*logs(2)**TWO - 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(B3A2 - B4A2*TWO + 
     -    THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(THREE*(logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(li2s(2) + li2s(3) + li2s(4) + 
     -    li2s(5) - li2s(6) - li2s(7) - li2s(8) - 
     -    li2s(9) - logs(1)*logs(5) - 
     -    logs(3)*logs(6) + logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-B3A2 + B4A2*TWO + logs(2)**TWO - 
     -      TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-B3A2 + B4A2*TWO - logs(2)**TWO - 6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(B3A2 - B4A2*TWO + logs(2)**TWO + 
     -      8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (B3A2 - B4A2*TWO - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-B3A2 + B4A2*TWO + logs(2)**TWO + 
     -    TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(B3A2 - B4A2*TWO - logs(2)**TWO + TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(TWO*(logs(2)**TWO + logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE29Phys2_w4_qp_fxn=res
      return
      end function fE29Phys2_w4_qp_fxn

      function fE18Phys3_w3_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE18Phys3_w3_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      CALL SetPath_st_qp(xsb,xsb0_Phys3_qp,xtb,xtb0_Phys3_qp)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE18Phys3_w3_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE18Phys3_w3_qp_fxn,
     $        one,integral)
      endif
      fE18Phys3_w3_qp=integral
      return
      end function fE18Phys3_w3_qp

      function fE18Phys3_w3_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE18Phys3_w3_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B16A2
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Phys3_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3_qp
      B16A2=B16A2_Phys3_qp
      CALL GetPath_Phys3_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE18Phys3_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      fE18Phys3_w3_qp_fxn=res
      return
      end function fE18Phys3_w3_qp_fxn

      function fE25Phys3_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE25Phys3_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(3)
      common /fE25Phys3_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Phys3_qp,xtb,xtb0_Phys3_qp)
      fteq1(1)=dlog_f11_qp(xsb,xtb)
      fteq1(2)=dlog_f12_qp(xsb,xtb)
      fteq1(3)=dlog_f13_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE25Phys3_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE25Phys3_w4_qp_fxn,
     $        one,integral)
      endif
      fE25Phys3_w4_qp=integral
      return
      end function fE25Phys3_w4_qp

      function fE25Phys3_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE25Phys3_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B14A3
      complex*32 B16A2
      complex*32 B16A3
      complex*32 B23A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(3)
      common /fE25Phys3_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Phys3_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3_qp
      B14A3=B14A3_Phys3_qp
      B16A2=B16A2_Phys3_qp
      B16A3=B16A3_Phys3_qp
      B23A3=B23A3_Phys3_qp
      CALL GetPath_Phys3_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE25Phys3_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B14A3 - 8*B23A3 + B16A3*FOUR)*HALF)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-8*B14A2 - 32*B16A2 + logs(2)**TWO + 
     -    8*logs(8)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (6*B14A2 + 24*B16A2 + logs(2)**TWO - 
     -    6*logs(8)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(2)
      f1=dlog_f12_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(6*B14A2 + 24*B16A2 + 5*logs(2)**TWO - 
     -      6*logs(8)**TWO))/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-22*B14A2 - 88*B16A2 + 9*logs(2)**TWO + 
     -    22*logs(8)**TWO)/4E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -      logs(8)**TWO))/2E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2E0_16)
      fE25Phys3_w4_qp_fxn=res
      return
      end function fE25Phys3_w4_qp_fxn

      function fE26Phys3_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE26Phys3_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B26A4
      complex*32 fteq1(5)
      common /fE26Phys3_w4_qp_ft/ fteq1
      B26A4=B26A4_Phys3_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys3_qp,xtb,xtb0_Phys3_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f7_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE26Phys3_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE26Phys3_w4_qp_fxn,
     $        one,integral)
      endif
      fE26Phys3_w4_qp=integral
      fE26Phys3_w4_qp=fE26Phys3_w4_qp+
     - +B26A4
      return
      end function fE26Phys3_w4_qp

      function fE26Phys3_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE26Phys3_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B14A3
      complex*32 B16A2
      complex*32 B16A3
      complex*32 B23A3
      complex*32 B26A4
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE26Phys3_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Phys3_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3_qp
      B14A3=B14A3_Phys3_qp
      B16A2=B16A2_Phys3_qp
      B16A3=B16A3_Phys3_qp
      B23A3=B23A3_Phys3_qp
      B26A4=B26A4_Phys3_qp
      CALL GetPath_Phys3_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE26Phys3_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B14A3 - 8*B23A3 + B16A3*FOUR)*HALF)
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B14A3 - (B16A3 - B23A3)*FOUR)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        8*B16A2 + B14A2*TWO + logs(2)**TWO - 
     -  TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -5*B14A2 - 20*B16A2 + TWO*logs(2)**TWO + 
     -  5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        FOUR*(B14A2 + B16A2*FOUR - logs(2)**TWO - 
     -    logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-10*B14A2 - 40*B16A2 - 7*logs(2)**TWO + 
     -    10*logs(8)**TWO)/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-5*(-8*B16A2 - B14A2*TWO + 
     -      logs(2)**TWO + TWO*logs(8)**TWO))/2E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Phys3_w4_qp_fxn=res
      return
      end function fE26Phys3_w4_qp_fxn

      function fE28Phys3_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE28Phys3_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(2)
      common /fE28Phys3_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Phys3_qp,xtb,xtb0_Phys3_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f9_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE28Phys3_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE28Phys3_w4_qp_fxn,
     $        one,integral)
      endif
      fE28Phys3_w4_qp=integral
      return
      end function fE28Phys3_w4_qp

      function fE28Phys3_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE28Phys3_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B16A2
      complex*32 B23A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(2)
      common /fE28Phys3_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Phys3_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3_qp
      B16A2=B16A2_Phys3_qp
      B23A3=B23A3_Phys3_qp
      CALL GetPath_Phys3_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE28Phys3_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((TWO*RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(-((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B23A3*FOUR))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb_qp(xsb,xtb)
      dfdxtb=df2dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(8*B16A2 + B14A2*TWO + 
     -      5*logs(3)**TWO - TWO*logs(9)**TWO))/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-10*B14A2 - 40*B16A2 + 
     -    THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-B14A2 - B16A2*FOUR + 
     -      logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -      li2s(6) + li2s(7) + li2s(8) + 
     -      li2s(9) + li2s(10) + 
     -      logs(1)*logs(6) + logs(4)*logs(7) - 
     -      logs(5)*logs(8))))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-8*B14A2 - 32*B16A2 + 
     -    logs(3)**TWO + 8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (6*B14A2 + 24*B16A2 + logs(3)**TWO - 
     -    6*logs(9)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(3) - li2s(4) - li2s(5) - li2s(6) + 
     -  li2s(7) + li2s(8) + li2s(9) + li2s(10) + 
     -  logs(1)*logs(6) + logs(4)*logs(7) - 
     -  logs(5)*logs(8))
      fE28Phys3_w4_qp_fxn=res
      return
      end function fE28Phys3_w4_qp_fxn

      function fE29Phys3_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE29Phys3_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(5)
      common /fE29Phys3_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Phys3_qp,xtb,xtb0_Phys3_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f8_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE29Phys3_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE29Phys3_w4_qp_fxn,
     $        one,integral)
      endif
      fE29Phys3_w4_qp=integral
      return
      end function fE29Phys3_w4_qp

      function fE29Phys3_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE29Phys3_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B14A3
      complex*32 B16A2
      complex*32 B16A3
      complex*32 B23A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE29Phys3_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Phys3_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys3_qp
      B14A3=B14A3_Phys3_qp
      B16A2=B16A2_Phys3_qp
      B16A3=B16A3_Phys3_qp
      B23A3=B23A3_Phys3_qp
      CALL GetPath_Phys3_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp.or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE29Phys3_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B23A3*FOUR))
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B14A3 + (B16A3 + B23A3)*FOUR)
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B14A3 - B16A3*FOUR)*TWO)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-3*(-8*B16A2 - B14A2*TWO + 
     -      logs(2)**TWO + TWO*logs(8)**TWO))/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-10*B14A2 - 40*B16A2 + 
     -    THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-B14A2 - B16A2*FOUR + 
     -      logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(li2s(2) + li2s(3) + li2s(4) + 
     -    li2s(5) - li2s(6) - li2s(7) - li2s(8) - 
     -    li2s(9) - logs(1)*logs(5) - 
     -    logs(3)*logs(6) + logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(8*B16A2 + B14A2*TWO + 
     -      logs(2)**TWO - TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        6*B14A2 + 24*B16A2 - logs(2)**TWO - 
     -  6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-8*B16A2 - B14A2*TWO + 
     -    logs(2)**TWO + TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -8*B16A2 - B14A2*TWO - logs(2)**TWO + 
     -  TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE29Phys3_w4_qp_fxn=res
      return
      end function fE29Phys3_w4_qp_fxn

      function fE18Phys4_w3_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE18Phys4_w3_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      CALL SetPath_st_qp(xsb,xsb0_Phys4_qp,xtb,xtb0_Phys4_qp)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE18Phys4_w3_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE18Phys4_w3_qp_fxn,
     $        one,integral)
      endif
      fE18Phys4_w3_qp=integral
      return
      end function fE18Phys4_w3_qp

      function fE18Phys4_w3_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE18Phys4_w3_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B16A2
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE18Phys4_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4_qp
      B16A2=B16A2_Phys4_qp
      CALL GetPath_Phys4_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE18Phys4_w3_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      fE18Phys4_w3_qp_fxn=res
      return
      end function fE18Phys4_w3_qp_fxn

      function fE25Phys4_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE25Phys4_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(3)
      common /fE25Phys4_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Phys4_qp,xtb,xtb0_Phys4_qp)
      fteq1(1)=dlog_f11_qp(xsb,xtb)
      fteq1(2)=dlog_f12_qp(xsb,xtb)
      fteq1(3)=dlog_f13_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE25Phys4_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE25Phys4_w4_qp_fxn,
     $        one,integral)
      endif
      fE25Phys4_w4_qp=integral
      return
      end function fE25Phys4_w4_qp

      function fE25Phys4_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE25Phys4_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B14A3
      complex*32 B16A2
      complex*32 B16A3
      complex*32 B23A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(3)
      common /fE25Phys4_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE25Phys4_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4_qp
      B14A3=B14A3_Phys4_qp
      B16A2=B16A2_Phys4_qp
      B16A3=B16A3_Phys4_qp
      B23A3=B23A3_Phys4_qp
      CALL GetPath_Phys4_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE25Phys4_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B14A3 - 8*B23A3 + B16A3*FOUR)*HALF)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-8*B14A2 - 32*B16A2 + logs(2)**TWO + 
     -    8*logs(8)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (6*B14A2 + 24*B16A2 + logs(2)**TWO - 
     -    6*logs(8)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/8E0_16)
      ft1=fteq1(1)
      f1=dlog_f11_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(2)
      f1=dlog_f12_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(6*B14A2 + 24*B16A2 + 5*logs(2)**TWO - 
     -      6*logs(8)**TWO))/8E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-22*B14A2 - 88*B16A2 + 9*logs(2)**TWO + 
     -    22*logs(8)**TWO)/4E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -      logs(8)**TWO))/2E0_16)
      ft1=fteq1(3)
      f1=dlog_f13_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-9*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7)))/2E0_16)
      fE25Phys4_w4_qp_fxn=res
      return
      end function fE25Phys4_w4_qp_fxn

      function fE26Phys4_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE26Phys4_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 B26A4
      complex*32 fteq1(5)
      common /fE26Phys4_w4_qp_ft/ fteq1
      B26A4=B26A4_Phys4_qp
      CALL SetPath_st_qp(xsb,xsb0_Phys4_qp,xtb,xtb0_Phys4_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f7_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE26Phys4_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE26Phys4_w4_qp_fxn,
     $        one,integral)
      endif
      fE26Phys4_w4_qp=integral
      fE26Phys4_w4_qp=fE26Phys4_w4_qp+
     - +B26A4
      return
      end function fE26Phys4_w4_qp

      function fE26Phys4_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE26Phys4_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B14A3
      complex*32 B16A2
      complex*32 B16A3
      complex*32 B23A3
      complex*32 B26A4
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE26Phys4_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE26Phys4_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4_qp
      B14A3=B14A3_Phys4_qp
      B16A2=B16A2_Phys4_qp
      B16A3=B16A3_Phys4_qp
      B23A3=B23A3_Phys4_qp
      B26A4=B26A4_Phys4_qp
      CALL GetPath_Phys4_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE26Phys4_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((B14A3 - 8*B23A3 + B16A3*FOUR)*HALF)
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-B14A3 - (B16A3 - B23A3)*FOUR)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(1) + logs(2)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(HALF*logs(2)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        8*B16A2 + B14A2*TWO + logs(2)**TWO - 
     -  TWO*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -5*B14A2 - 20*B16A2 + TWO*logs(2)**TWO + 
     -  5*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        FOUR*(B14A2 + B16A2*FOUR - logs(2)**TWO - 
     -    logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(FOUR*(-li2s(2) - li2s(3) - li2s(4) - 
     -      li2s(5) + li2s(6) + li2s(7) + 
     -      li2s(8) + li2s(9) + logs(1)*logs(5) + 
     -      logs(3)*logs(6) - logs(4)*logs(7))))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f7_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-10*B14A2 - 40*B16A2 - 7*logs(2)**TWO + 
     -    10*logs(8)**TWO)/4E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-5*(-8*B16A2 - B14A2*TWO + 
     -      logs(2)**TWO + TWO*logs(8)**TWO))/2E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        5*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE26Phys4_w4_qp_fxn=res
      return
      end function fE26Phys4_w4_qp_fxn

      function fE28Phys4_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE28Phys4_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(2)
      common /fE28Phys4_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Phys4_qp,xtb,xtb0_Phys4_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f9_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE28Phys4_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE28Phys4_w4_qp_fxn,
     $        one,integral)
      endif
      fE28Phys4_w4_qp=integral
      return
      end function fE28Phys4_w4_qp

      function fE28Phys4_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE28Phys4_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B16A2
      complex*32 B23A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(2)
      common /fE28Phys4_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=9)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=10)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE28Phys4_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4_qp
      B16A2=B16A2_Phys4_qp
      B23A3=B23A3_Phys4_qp
      CALL GetPath_Phys4_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE28Phys4_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((TWO*RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(4)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(5)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(8)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(9)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(-((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -     (xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(3)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(10)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B23A3*FOUR))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (32*zeta2 + 64*li2s(1) - 20*li2s(2) + 
     -    64*logs(2)*logs(3) - 21*logs(3)**TWO)/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df2dxsb_qp(xsb,xtb)
      dfdxtb=df2dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(TWO*logs(3)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (3*(8*B16A2 + B14A2*TWO + 
     -      5*logs(3)**TWO - TWO*logs(9)**TWO))/4E0_16)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-10*B14A2 - 40*B16A2 + 
     -    THREE*logs(3)**TWO + 10*logs(9)**TWO))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-B14A2 - B16A2*FOUR + 
     -      logs(3)**TWO + logs(9)**TWO)))
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-li2s(3) - li2s(4) - li2s(5) - 
     -      li2s(6) + li2s(7) + li2s(8) + 
     -      li2s(9) + li2s(10) + 
     -      logs(1)*logs(6) + logs(4)*logs(7) - 
     -      logs(5)*logs(8))))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-8*B14A2 - 32*B16A2 + 
     -    logs(3)**TWO + 8*logs(9)**TWO))
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (6*B14A2 + 24*B16A2 + logs(3)**TWO - 
     -    6*logs(9)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((FOUR*li2s(2) + logs(3)**TWO)/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f9_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -li2s(3) - li2s(4) - li2s(5) - li2s(6) + 
     -  li2s(7) + li2s(8) + li2s(9) + li2s(10) + 
     -  logs(1)*logs(6) + logs(4)*logs(7) - 
     -  logs(5)*logs(8))
      fE28Phys4_w4_qp_fxn=res
      return
      end function fE28Phys4_w4_qp_fxn

      function fE29Phys4_w4_qp(xsb,xtb)
      use Integration_Paths
      use simple_integrators
      implicit none
      complex*32 fE29Phys4_w4_qp
      real*16 xsb,xtb
      complex*32 integral
      real*16 error
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
      complex*32 fteq1(5)
      common /fE29Phys4_w4_qp_ft/ fteq1
      CALL SetPath_st_qp(xsb,xsb0_Phys4_qp,xtb,xtb0_Phys4_qp)
      fteq1(1)=dlog_f1_qp(xsb,xtb)
      fteq1(2)=dlog_f3_qp(xsb,xtb)
      fteq1(3)=dlog_f4_qp(xsb,xtb)
      fteq1(4)=dlog_f8_qp(xsb,xtb)
      fteq1(5)=dlog_f10_qp(xsb,xtb)
      if(integration_method.eq.2)then
         ! use double exponential quadrature
         if(DEQ_init_qp.eq.0)then
            CALL DEQuadrature_integrator_ini_qp(lenaw_qp,tiny_qp,
     $           DEQ_eps_qp,aw_qp)
            DEQ_init_qp=1
         endif
         CALL DEQuadrature_Cintegrator_qp(fE29Phys4_w4_qp_fxn,zero,
     $        one,aw_qp,integral,error)
      else
         CALL trapezoid_Cintegrator_QP(npoints,fE29Phys4_w4_qp_fxn,
     $        one,integral)
      endif
      fE29Phys4_w4_qp=integral
      return
      end function fE29Phys4_w4_qp

      function fE29Phys4_w4_qp_fxn(t)
      use Integration_Paths
      use func_psi
      implicit none
      complex*32 fE29Phys4_w4_qp_fxn
      real*16 t
      real*16 xsb,xtb,dxsbdt,dxtbdt
      complex*32 dfdxsb,dfdxtb,dfdt
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      real*16 zeta2,zeta3,zeta4,Pi
      parameter (zeta2=
     $     1.64493406684822643647241516664602518921894990E0_16)
      parameter (zeta3=
     $     1.20205690315959428539973816151144999076532573E0_16)
      parameter (zeta4=
     $     1.08232323371113819151600369654116790277475095E0_16)
      parameter (Pi=
     $     3.141592653589793238462643383279502884197169399E0_16)
      complex*32 B14A2
      complex*32 B14A3
      complex*32 B16A2
      complex*32 B16A3
      complex*32 B23A3
      complex*32 res,f1,ft1
      integer init
      save init
      data init/0/
      complex*32 fteq1(5)
      common /fE29Phys4_w4_qp_ft/ fteq1
      integer NLOGS
      parameter (NLOGS=8)
      complex*32 logs(NLOGS)
      integer NLI2S
      parameter (NLI2S=9)
      complex*32 li2s(NLI2S)
      IF(t.EQ.ZERO)THEN
      fE29Phys4_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
      RETURN
      ENDIF
      B14A2=B14A2_Phys4_qp
      B14A3=B14A3_Phys4_qp
      B16A2=B16A2_Phys4_qp
      B16A3=B16A3_Phys4_qp
      B23A3=B23A3_Phys4_qp
      CALL GetPath_Phys4_st_qp(t,xsb,xtb,dxsbdt,dxtbdt)
      if((abs(dxsbdt).eq.0E0_16.and.abs(dxtbdt).eq.0E0_16)
     $     .or.abs(xsb).lt.zero_thr_qp
     $     .or.abs(xtb-4E0_16).lt.zero_thr_qp)then
         fE29Phys4_w4_qp_fxn=cmplx(ZERO,ZERO,kind=16)
         RETURN
      endif
      logs(1)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb)),1 - FOUR/xsb)
      logs(2)=        LogP2_QP((-xsb + RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -   (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      logs(3)=        LogP2_QP((-1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xtb)),1 - FOUR/xtb)
      logs(4)=        LogM2_QP((1 + RSQRTP_QP(1 - FOUR/xsb - 
     -       FOUR/xtb))/
     -   (-1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  1 - FOUR/xsb - FOUR/xtb)
      logs(5)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xsb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      logs(6)=        LogM2_QP((RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb))/
     -   (-RSQRTP_QP(1 - FOUR/xtb) + 
     -     RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      logs(7)=RLogP_QP(16/(xsb*(-(FOUR/xsb) - FOUR/xtb)**TWO*xtb))
      logs(8)=        LogP2_QP((-xtb + RSQRTP_QP(xtb*(-FOUR + xtb)))/
     -   (xtb + RSQRTP_QP(xtb*(-FOUR + xtb))),
     -  xtb*(-FOUR + xtb))
      li2s(1)=        Li2M_QP(1 - (-xsb + 
     -      RSQRTP_QP(xsb*(-FOUR + xsb)))/
     -    (xsb + RSQRTP_QP(xsb*(-FOUR + xsb))),
     -  xsb*(-FOUR + xsb))
      li2s(2)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(3)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(4)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(5)=        Li2P_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 - RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(6)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(7)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xsb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xsb,1 - FOUR/xsb - FOUR/xtb))
      li2s(8)=        Li2M_QP((1 - RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      li2s(9)=        Li2M_QP((1 + RSQRTP_QP(1 - FOUR/xtb))/
     -   (1 + RSQRTP_QP(1 - FOUR/xsb - FOUR/xtb)),
     -  Min(1 - FOUR/xtb,1 - FOUR/xsb - FOUR/xtb))
      res=cmplx(0E0_16,0E0_16,kind=16)
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(-(B23A3*FOUR))
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*(B14A3 + (B16A3 + B23A3)*FOUR)
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*((-B14A3 - B16A3*FOUR)*TWO)
      ft1=fteq1(1)
      f1=dlog_f1_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*logs(2)**TWO)/2E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-3*(FOUR*li2s(1) + logs(2)**TWO))/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-3*(-8*B16A2 - B14A2*TWO + 
     -      logs(2)**TWO + TWO*logs(8)**TWO))/4E0_16)
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-10*B14A2 - 40*B16A2 + 
     -    THREE*logs(2)**TWO + 10*logs(8)**TWO))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(THREE*(-B14A2 - B16A2*FOUR + 
     -      logs(2)**TWO + logs(8)**TWO)))
      ft1=fteq1(2)
      f1=dlog_f3_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        THREE*(li2s(2) + li2s(3) + li2s(4) + 
     -    li2s(5) - li2s(6) - li2s(7) - li2s(8) - 
     -    li2s(9) - logs(1)*logs(5) - 
     -    logs(3)*logs(6) + logs(4)*logs(7)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(-(HALF*(FOUR*li2s(1) + logs(2)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(8*B16A2 + B14A2*TWO + 
     -      logs(2)**TWO - TWO*logs(8)**TWO)))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        6*B14A2 + 24*B16A2 - logs(2)**TWO - 
     -  6*logs(8)**TWO)
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(3)
      f1=dlog_f4_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df7dxsb_qp(xsb,xtb)
      dfdxtb=df7dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -(HALF*(-8*B14A2 - 32*B16A2 + 
     -      logs(2)**TWO + 8*logs(8)**TWO)))
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df8dxsb_qp(xsb,xtb)
      dfdxtb=df8dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        (-6*B14A2 - 24*B16A2 - logs(2)**TWO + 
     -    6*logs(8)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df9dxsb_qp(xsb,xtb)
      dfdxtb=df9dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((-(FOUR*li2s(1)) - logs(2)**TWO)/4E0_16)
      ft1=fteq1(4)
      f1=dlog_f8_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df11dxsb_qp(xsb,xtb)
      dfdxtb=df11dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        li2s(2) + li2s(3) + li2s(4) + li2s(5) - 
     -  li2s(6) - li2s(7) - li2s(8) - li2s(9) - 
     -  logs(1)*logs(5) - logs(3)*logs(6) + 
     -  logs(4)*logs(7))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df1dxsb_qp(xsb,xtb)
      dfdxtb=df1dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*((3*(FOUR*li2s(1) + logs(2)**TWO))/2E0_16)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df3dxsb_qp(xsb,xtb)
      dfdxtb=df3dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        HALF*(-8*B16A2 - B14A2*TWO + 
     -    logs(2)**TWO + TWO*logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df4dxsb_qp(xsb,xtb)
      dfdxtb=df4dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        -8*B16A2 - B14A2*TWO - logs(2)**TWO + 
     -  TWO*logs(8)**TWO)
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df10dxsb_qp(xsb,xtb)
      dfdxtb=df10dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-B14A2 - B16A2*FOUR + logs(2)**TWO + 
     -    logs(8)**TWO))
      ft1=fteq1(5)
      f1=dlog_f10_qp(xsb,xtb)
      f1=ft1-f1
      dfdxsb=df13dxsb_qp(xsb,xtb)
      dfdxtb=df13dxtb_qp(xsb,xtb)
      dfdt=dfdxsb*dxsbdt+dfdxtb*dxtbdt
      res=res+dfdt*f1*(        TWO*(-li2s(2) - li2s(3) - li2s(4) - 
     -    li2s(5) + li2s(6) + li2s(7) + li2s(8) + 
     -    li2s(9) + logs(1)*logs(5) + 
     -    logs(3)*logs(6) - logs(4)*logs(7)))
      fE29Phys4_w4_qp_fxn=res
      return
      end function fE29Phys4_w4_qp_fxn
      
      ! non-rationalised dlog forms
      function dlog_f1_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f1_qp
      real*16 xsb,xtb
      dlog_f1_qp=        -LogP2_QP((-xsb + RSQRTP_QP((-4 + xsb)*xsb))/
     -    (xsb + RSQRTP_QP((-4 + xsb)*xsb)),
     -   (-4 + xsb)*xsb)
      if(isnan(real(dlog_f1_qp,kind=16))
     $     .or.real(dlog_f1_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f1_qp,kind=16))then
          dlog_f1_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f1_qp

      function df1dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df1dxsb_qp
      real*16 xsb,xtb
      df1dxsb_qp=1E0_16/RSQRTP_QP((-4E0_16+xsb)*xsb)
      return
      end function df1dxsb_qp

      function df1dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df1dxtb_qp
      real*16 xsb,xtb
      df1dxtb_qp=0E0_16
      return
      end function df1dxtb_qp

      function dlog_f2_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f2_qp
      real*16 xsb,xtb
      dlog_f2_qp=-RLogP_QP(-4 + xsb)
      if(isnan(real(dlog_f2_qp,kind=16))
     $     .or.real(dlog_f2_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f2_qp,kind=16))then
          dlog_f2_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f2_qp

      function df2dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df2dxsb_qp
      real*16 xsb,xtb
      df2dxsb_qp=1E0_16/(4E0_16-xsb)
      return
      end function df2dxsb_qp

      function df2dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df2dxtb_qp
      real*16 xsb,xtb
      df2dxtb_qp=0E0_16
      return
      end function df2dxtb_qp

      function dlog_f3_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f3_qp
      real*16 xsb,xtb
      dlog_f3_qp=RLogP_QP(xsb)
      if(isnan(real(dlog_f3_qp,kind=16))
     $     .or.real(dlog_f3_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f3_qp,kind=16))then
         ! nan or infinite
          dlog_f3_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f3_qp

      function df3dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df3dxsb_qp
      real*16 xsb,xtb
      df3dxsb_qp=1E0_16/xsb
      return
      end function df3dxsb_qp

      function df3dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df3dxtb_qp
      real*16 xsb,xtb
      df3dxtb_qp=0E0_16
      return
      end function df3dxtb_qp

      function dlog_f4_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f4_qp
      real*16 xsb,xtb
      dlog_f4_qp=RLogP_QP(xtb)
      if(isnan(real(dlog_f4_qp,kind=16))
     $     .or.real(dlog_f4_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f4_qp,kind=16))then
          dlog_f4_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f4_qp

      function df4dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df4dxsb_qp
      real*16 xsb,xtb
      df4dxsb_qp=0E0_16
      return
      end function df4dxsb_qp

      function df4dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df4dxtb_qp
      real*16 xsb,xtb
      df4dxtb_qp=1E0_16/xtb
      return
      end function df4dxtb_qp

      function dlog_f7_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f7_qp
      real*16 xsb,xtb
      dlog_f7_qp=        LogP2_QP((xsb*(1 + xtb) - 
     -      RSQRTM_QP(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2)))/
     -    (xsb*(1 + xtb) + 
     -      RSQRTM_QP(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))),
     -   xsb*(xsb*(-1 + xtb)**2 - 4*xtb**2))/2E0_16
      if(isnan(real(dlog_f7_qp,kind=16))
     $     .or.real(dlog_f7_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f7_qp,kind=16))then
         ! nan or infinite
          dlog_f7_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f7_qp

      function df7dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df7dxsb_qp
      real*16 xsb,xtb
      df7dxsb_qp=        -(xtb*(1 + xtb))/
     -  (2E0_16*(xsb + xtb)*
     -    RSQRTM_QP(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df7dxsb_qp

      function df7dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df7dxtb_qp
      real*16 xsb,xtb
      df7dxtb_qp=        (xsb*(xsb + 2*xtb - xsb*xtb))/
     -  (2E0_16*xtb*(xsb + xtb)*
     -    RSQRTM_QP(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df7dxtb_qp

      function dlog_f8_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f8_qp
      real*16 xsb,xtb
      complex*32 den1
      if(xsb.GT.0E0_16.and.-xtb.LT.1E-23_16.and.-xtb.GT.0E0_16)then
         den1=cmplx(-2E0_16*xtb**2,0E0_16,kind=16)
      else
         den1=(xsb*(-1E0_16 + xtb) +
     -      RSQRTM_QP(xsb*
     -        (xsb*(-1E0_16 + xtb)**2 - 4E0_16*xtb**2)))
      endif
      dlog_f8_qp=        LogP2_QP((xsb*(-1E0_16 + xtb) -
     -      RSQRTM_QP(xsb*
     -        (xsb*(-1E0_16 + xtb)**2 - 4E0_16*xtb**2)))/
     -  den1, xsb*(xsb*(-1E0_16 + xtb)**2 - 4E0_16*xtb**2))+
     -  LogP2_QP((xsb*(1E0_16 + xtb) -
     -      RSQRTM_QP(xsb*
     -        (xsb*(-1E0_16 + xtb)**2 - 4E0_16*xtb**2)))/
     -    (xsb*(1E0_16 + xtb) +
     -      RSQRTM_QP(xsb*
     -        (xsb*(-1E0_16 + xtb)**2 - 4E0_16*xtb**2))),
     -     xsb*(xsb*(-1E0_16 + xtb)**2 - 4E0_16*xtb**2))
      if(isnan(real(dlog_f8_qp,kind=16))
     $     .or.real(dlog_f8_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f8_qp,kind=16))then
         ! nan or infinite
          dlog_f8_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f8_qp

      function df8dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df8dxsb_qp
      real*16 xsb,xtb
      df8dxsb_qp=        (xsb - xsb*xtb - 2*xtb**2)/
     -  ((xsb + xtb)*
     -    RSQRTM_QP(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df8dxsb_qp

      function df8dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df8dxtb_qp
      real*16 xsb,xtb
      df8dxtb_qp=        -((xsb**2*(1 + xtb))/
     -    (xtb*(xsb + xtb)*
     -      RSQRTM_QP(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))))
      return
      end function df8dxtb_qp

      function dlog_f9_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f9_qp
      real*16 xsb,xtb
      complex*32 twopii
      parameter (twopii=(0E0_16,
     $     6.2831853071795864769252867665590057683943387987502E0_16))
      dlog_f9_qp=        -LogP2_QP((xsb*(-xsb - 4*xtb + xsb*xtb) - 
     -      RSQRTM_QP(xsb*
     -         (xsb*(-1 + xtb)**2 - 4*xtb**2))*
     -       RSQRTP_QP((-4 + xsb)*xsb))/
     -    (xsb*(-xsb - 4*xtb + xsb*xtb) + 
     -      RSQRTM_QP(xsb*
     -         (xsb*(-1 + xtb)**2 - 4*xtb**2))*
     -       RSQRTP_QP((-4 + xsb)*xsb)),
     -   (-4 + xsb)*xsb**2*
     -    (xsb*(-1 + xtb)**2 - 4*xtb**2))
      if(xsb.GT.0E0_16.and.xsb.LT.4E0_16.and.xtb.LT.0E0_16
     - .and.-xsb-4E0_16*xtb+xsb*xtb.ge.0E0_16)then
         dlog_f9_qp=dlog_f9_qp-twopii
      endif
      if(isnan(real(dlog_f9_qp,kind=16)))then
          dlog_f9_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      if(real(dlog_f9_qp,kind=16)-1E0_16.eq.real(dlog_f9_qp,kind=16))then
         ! we encounter the infinity
         dlog_f9_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f9_qp

      function df9dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df9dxsb_qp
      real*16 xsb,xtb
      df9dxsb_qp=        (xsb*(-1 + xtb) + 4*xtb)/
     -  (RSQRTM_QP(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2))*
     -    RSQRTP_QP((-4 + xsb)*xsb))
      return
      end function df9dxsb_qp

      function df9dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df9dxtb_qp
      real*16 xsb,xtb
      df9dxtb_qp=        (2*RSQRTP_QP((-4 + xsb)*xsb))/
     -  RSQRTM_QP(xsb*(xsb*(-1 + xtb)**2 - 4*xtb**2))
      return
      end function df9dxtb_qp

      function dlog_f10_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f10_qp
      real*16 xsb,xtb
      dlog_f10_qp= -RLogP_QP(xsb)/2E0_16 + RLogP_QP(xtb)/2E0_16 + 
     -  RLogP_QP(xsb + xtb)/2E0_16
      if(isnan(real(dlog_f10_qp,kind=16))
     $     .or.real(dlog_f10_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f10_qp,kind=16))then
         ! nan or infinite
          dlog_f10_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f10_qp

      function df10dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df10dxsb_qp
      real*16 xsb,xtb
      df10dxsb_qp=-xtb/(2E0_16*xsb*(xsb + xtb))
      return
      end function df10dxsb_qp

      function df10dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df10dxtb_qp
      real*16 xsb,xtb
      df10dxtb_qp=(1E0_16/xtb+1E0_16/(xsb+xtb))/2E0_16
      return
      end function df10dxtb_qp

      function dlog_f11_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f11_qp
      real*16 xsb,xtb
      if(abs(xsb).le.zero_thr_qp)then
         dlog_f11_qp=-RLogM_QP(xsb/(16E0_16*xtb**2))
      else
         dlog_f11_qp=-LogM2_QP((xsb*xtb*
     -        (-3*xsb - 4*xtb + xsb*xtb) - 
     -        RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)*RSQRTM_QP(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2)))/
     -        (xsb*xtb*(-3*xsb - 4*xtb + xsb*xtb) + 
     -        RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)*RSQRTM_QP(xsb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))),
     -        xsb**2*(xsb*(-4 + xtb) - 4*xtb)*xtb*
     -        (xsb*(-1 + xtb)**2 - 4*xtb**2))
      endif
      if(isnan(real(dlog_f11_qp,kind=16))
     $     .or.real(dlog_f11_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f11_qp,kind=16))then
         ! nan or infinite
          dlog_f11_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f11_qp

      function df11dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df11dxsb_qp
      real*16 xsb,xtb
      df11dxsb_qp=        (xtb**2*(4*xtb + xsb*(5 + xtb)))/
     -  ((xsb + xtb)*
     -    RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -      xtb)*RSQRTM_QP(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df11dxsb_qp

      function df11dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df11dxtb_qp
      real*16 xsb,xtb
      df11dxtb_qp=        (xsb*(3*xsb**2*(-1 + xtb) + 
     -      2*xsb*(-6 + xtb)*xtb - 8*xtb**2))/
     -  ((xsb + xtb)*
     -    RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -      xtb)*RSQRTM_QP(xsb*
     -      (xsb*(-1 + xtb)**2 - 4*xtb**2)))
      return
      end function df11dxtb_qp

      function dlog_f12_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f12_qp
      real*16 xsb,xtb
      dlog_f12_qp=        LogP2_QP(((-4*xsb + xsb**2)*xtb - 
     -     RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)*RSQRTP_QP((-4 + xsb)*xsb))/
     -   ((-4*xsb + xsb**2)*xtb + 
     -     RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)*RSQRTP_QP((-4 + xsb)*xsb)),
     -  (-4 + xsb)*xsb**2*
     -   (xsb*(-4 + xtb) - 4*xtb)*xtb)
      if(isnan(real(dlog_f12_qp,kind=16)))then
          dlog_f12_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      if(real(dlog_f12_qp,kind=16)-1E0_16
     $     .eq.real(dlog_f12_qp,kind=16))then
          ! we encounter the infinity
          dlog_f12_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f12_qp

      function df12dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df12dxsb_qp
      real*16 xsb,xtb
      df12dxsb_qp=        (-4*xtb)/
     -  (RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*xtb)*
     -    RSQRTP_QP((-4 + xsb)*xsb))
      return
      end function df12dxsb_qp

      function df12dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df12dxtb_qp
      real*16 xsb,xtb
      df12dxtb_qp=        -(RSQRTP_QP((-4 + xsb)*xsb)/
     -    RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*xtb)
     -    )
      return
      end function df12dxtb_qp

      function dlog_f13_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 dlog_f13_qp
      real*16 xsb,xtb
      dlog_f13_qp=        -LogP2_QP((xsb*xtb - 
     -      RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb))/
     -    (xsb*xtb + 
     -      RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -        xtb)),xsb*(xsb*(-4 + xtb) - 4*xtb)*
     -    xtb)
      if(isnan(real(dlog_f13_qp,kind=16))
     $     .or.real(dlog_f13_qp,kind=16)-1E0_16.eq.
     $     real(dlog_f13_qp,kind=16))then
         ! nan or infinite
          dlog_f13_qp=cmplx(0E0_16,0E0_16,kind=16)
      endif
      return
      end function dlog_f13_qp

      function df13dxsb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df13dxsb_qp
      real*16 xsb,xtb
      df13dxsb_qp=        xtb**2/
     -  ((xsb + xtb)*
     -    RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*xtb)
     -    )
      return
      end function df13dxsb_qp

      function df13dxtb_qp(xsb,xtb)
      use func_psi
      implicit none
      complex*32 df13dxtb_qp
      real*16 xsb,xtb
      df13dxtb_qp=        xsb**2/
     -  ((xsb + xtb)*
     -    RSQRTM_QP(xsb*(xsb*(-4 + xtb) - 4*xtb)*xtb)
     -    )
      return
      end function df13dxtb_qp

      subroutine evaluate_boundaryconstants_Phys12_qp(xsb0,xtb0)
      use calc_IterInts_RootMIs_boundaryconstants
      implicit none
      real*16 xsb0,xtb0
      complex*32 cres(19)
      save cres
      real*16 xsb0_save, xtb0_save
      data xsb0_save /0E0_16/
      data xtb0_save /0E0_16/
      save xsb0_save, xtb0_save
      if(xtb0.ne.0E0_16.or.xsb0.le.0E0_16)then
         write(*,*)"Error: xtb0 =!= 0 or xsb0 <= 0 in Phys 1 or 2"
         stop
      endif
      if(.not.(xsb0.eq.xsb0_save.and.xtb0.eq.xtb0_save))then
         ! evaluate boundary constants
         call get_boundaryconstants_Phys12_QP(xsb0,cres)
      endif
      xsb0_save=xsb0
      xtb0_save=xtb0
      if(xsb0.GE.4E0_16)then
         ! Phys 1
         xsb0_Phys1_qp=xsb0
         xtb0_Phys1_qp=xtb0
         B2A2_Phys1_qp=cres(2)
         B4A3_Phys1_qp=cres(3)
         B10A3_Phys1_qp=cres(4)
         B11A2_Phys1_qp=cres(5)
         B18A3_Phys1_qp=cres(6)
         B20A3_Phys1_qp=cres(7)
         B23A3_Phys1_qp=cres(8)
         B26A4_Phys1_qp=cres(9)
         B28A4_Phys1_qp=cres(10)
         B29A4_Phys1_qp=cres(11)
         B3A2_Phys1_qp=cres(12)
         B3A3_Phys1_qp=cres(13)
         B4A2_Phys1_qp=cres(14)
         B5A3_Phys1_qp=cres(15)
         B6A3_Phys1_qp=cres(16)
         B7A2_Phys1_qp=cres(17)
         B8A3_Phys1_qp=cres(18)
         B9A3_Phys1_qp=cres(19)
      else
         ! Phys 2
         xsb0_Phys2_qp=xsb0
         xtb0_Phys2_qp=xtb0
         B2A2_Phys2_qp=cres(2)
         B4A3_Phys2_qp=cres(3)
         B10A3_Phys2_qp=cres(4)
         B11A2_Phys2_qp=cres(5)
         B18A3_Phys2_qp=cres(6)
         B20A3_Phys2_qp=cres(7)
         B23A3_Phys2_qp=cres(8)
         B26A4_Phys2_qp=cres(9)
         B28A4_Phys2_qp=cres(10)
         B29A4_Phys2_qp=cres(11)
         B3A2_Phys2_qp=cres(12)
         B3A3_Phys2_qp=cres(13)
         B4A2_Phys2_qp=cres(14)
         B5A3_Phys2_qp=cres(15)
         B6A3_Phys2_qp=cres(16)
         B7A2_Phys2_qp=cres(17)
         B8A3_Phys2_qp=cres(18)
         B9A3_Phys2_qp=cres(19)
      endif
      return
      end subroutine

      subroutine evaluate_boundaryconstants_Phys34_qp(xsb0,xtb0)
      use calc_IterInts_RootMIs_boundaryconstants
      implicit none
      real*16 xsb0,xtb0
      complex*32 cres(6)
      save cres
      real*16 xsb0_save, xtb0_save
      data xsb0_save /0E0_16/
      data xtb0_save /0E0_16/
      save xsb0_save, xtb0_save
      if(xsb0.ne.0E0_16.or.xtb0.le.0E0_16)then
         write(*,*)"Error: xsb0 =!= 0 or xtb0 <= 0 in Phys 3, 4"
         stop
      endif
      if(.not.(xsb0.eq.xsb0_save.and.xtb0.eq.xtb0_save))then
         ! evaluate boundary constants
         call get_boundaryconstants_Phys34_QP(xtb0,cres)
      endif
      xsb0_save=xsb0
      xtb0_save=xtb0
      if(xtb0.GE.4E0_16)then
         ! Phys 3
         xsb0_Phys3_qp=xsb0
         xtb0_Phys3_qp=xtb0
         B14A3_Phys3_qp=cres(1)
         B16A2_Phys3_qp=cres(2)
         B16A3_Phys3_qp=cres(3)
         B23A3_Phys3_qp=cres(4)
         B26A4_Phys3_qp=cres(5)
         B14A2_Phys3_qp=cres(6)
      else
         ! Phys 4
         xsb0_Phys4_qp=xsb0
         xtb0_Phys4_qp=xtb0
         B14A3_Phys4_qp=cres(1)
         B16A2_Phys4_qp=cres(2)
         B16A3_Phys4_qp=cres(3)
         B23A3_Phys4_qp=cres(4)
         B26A4_Phys4_qp=cres(5)
         B14A2_Phys4_qp=cres(6)
      endif
      return
      end subroutine

      end module
