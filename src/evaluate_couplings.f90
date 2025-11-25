MODULE evaluate_couplings
  IMPLICIT NONE
CONTAINS
    FUNCTION QMASS_RG_RUNNING(nloop,as0,asmu,mass0) RESULT(res)
    ! the RG running of MSbar quark mass
    ! it has the relation wrt the MSbar Yukawa coupling via 
    ! y(mu)=sqrt(2)*m(mu)/vev, vev=246.2 GeV at scale mu
    ! It will not vary the number of quark flavours in RG running
    USE qcd_constants
    USE global_constants
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nloop ! from 0 to 3
    REAL(KIND(1d0)),INTENT(IN)::as0,asmu
    REAL(KIND(1d0)),INTENT(IN)::mass0 ! scale invariant mass, i.e. 
                                      ! m^{MSar}(m^{SI})=m^{SI} can be read from PDG
    REAL(KIND(1d0))::res
    REAL(KIND(1d0))::c0,c1,c2,c3
    SAVE c0,c1,c2,c3
    INTEGER::init=0
    SAVE init
    REAL(KIND(1d0))::casmu,cas0
    REAL(KIND(1d0))::asmuo2pi,as0o2pi,fac
    IF(nloop.LT.0)THEN
       res=mass0
       RETURN
    ENDIF
    IF(init.EQ.0)THEN
       IF(beta0.EQ.0d0.OR.gammam0.EQ.0d0)THEN
          WRITE(*,*)"Error: Please update beta or gamma_m functions first #0"
          STOP
       ENDIF
       IF(nloop.GE.1.AND.(beta1.EQ.0.OR.gammam1.EQ.0d0))THEN
          WRITE(*,*)"Error: Please update beta or gamma_m functions first #1"
          STOP
       ENDIF
       IF(nloop.GE.2.AND.(beta2.EQ.0.OR.gammam2.EQ.0d0))THEN
          WRITE(*,*)"Error: Please update beta or gamma_m functions first #2"
          STOP
       ENDIF
       IF(nloop.GE.3.AND.(beta3.EQ.0.OR.gammam3.EQ.0d0))THEN
          WRITE(*,*)"Error: Please update beta or gamma_m functions first #3"
          STOP
       ENDIF
       c0=gammam0/beta0
       c1=-beta1*gammam0/beta0**2+gammam1/beta0
       c2=gammam0/beta0**2*(beta1**2/beta0-beta2)-beta1*gammam1/beta0**2+gammam2/beta0
       c3=gammam0/beta0**2*(TWO*beta1*beta2/beta0-beta1**3/beta0**2-beta3)&
            +gammam1/beta0**2*(beta1**2/beta0-beta2)-beta1*gammam2/beta0**2+gammam3/beta0
       init=1
    ENDIF
    as0o2pi=as0/TWOPI
    asmuo2pi=asmu/TWOPI
    cas0=one
    casmu=one
    IF(nloop.LE.0)THEN
       cas0=cas0*as0o2pi**c0
       casmu=casmu*asmuo2pi**c0
       res=casmu/cas0*mass0
       RETURN
    ENDIF
    cas0=cas0+c1*as0o2pi
    casmu=casmu+c1*asmuo2pi
    IF(nloop.LE.1)THEN
       cas0=cas0*as0o2pi**c0
       casmu=casmu*asmuo2pi**c0
       res=casmu/cas0*mass0
       RETURN
    ENDIF
    fac=HALF*(c1**2+c2)
    cas0=cas0+fac*as0o2pi**2
    casmu=casmu+fac*asmuo2pi**2
    IF(nloop.LE.2)THEN
       cas0=cas0*as0o2pi**c0
       casmu=casmu*asmuo2pi**c0
       res=casmu/cas0*mass0
       RETURN
    ENDIF
    fac=THIRD*(HALF*c1**3+THREE*HALF*c1*c2+c3)
    cas0=cas0+fac*as0o2pi**3
    casmu=casmu+fac*asmuo2pi**3
    IF(nloop.LE.3)THEN
       cas0=cas0*as0o2pi**c0
       casmu=casmu*asmuo2pi**c0
       res=casmu/cas0*mass0
       RETURN
    ENDIF
    WRITE(*,*)"ERROR: cannot run quark mass with nloop = ", nloop
    STOP
  END FUNCTION QMASS_RG_RUNNING

  FUNCTION ALPHAS(Q)
    USE qcd_coupling
    USE coupling_global
    IMPLICIT NONE
    REAL(KIND(1d0))::ALPHAS
    REAL(KIND(1d0)),INTENT(IN)::Q
    ALPHAS=as_value(as_box,Q)
  END FUNCTION ALPHAS

  FUNCTION ALPHAEW(Q)
    USE LbL_Global
    IMPLICIT NONE
    REAL(KIND(1d0))::ALPHAEW
    REAL(KIND(1d0)),INTENT(IN)::Q
    REAL(KIND(1d0)),SAVE::ALPHAEW_SAVE
    REAL(KIND(1d0)),SAVE::DalphalepMZ=0d0,DalphahadMZ=0d0,&
         DalphatopMZ=0d0 ! ,DalphaWMZ=0d0
    REAL(KIND(1d0))::DalphalepQ,DDalphahad,DalphatopQ !,DalphaWQ
    INTEGER,SAVE::init=0
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    IF(init.EQ.0)THEN
       IF(alpha_scheme.EQ.0)THEN
          ! alpha(0) scheme
          ALPHAEW_SAVE=1d0/alphaemm1
       ELSEIF(alpha_scheme.EQ.1)THEN
          ! Gmu scheme
          ALPHAEW_SAVE=DSQRT(2d0)*Gfermi*wmass_PDG**2&
               *(zmass_PDG**2-wmass_PDG**2)/zmass_PDG**2/pipi
       ELSE
          ALPHAEW_SAVE=0d0
          ! these values are tunned to make sure
          ! when Q -> 0 we have alpha -> 1/137.036
          IF(alpha_nloop.EQ.1)THEN
             umass_PDG=43d-3
             dmass_PDG=43d-3
             ! the following tunnes with W contribution
             !umass_PDG=53.56d-3
             !dmass_PDG=53.56d-3
          ELSEIF(alpha_nloop.EQ.2)THEN
             umass_PDG=83d-3
             dmass_PDG=83d-3
             ! the following tunnes with W contribution
             !umass_PDG=101.7d-3
             !dmass_PDG=101.7d-3
          ELSE
             WRITE(*,*)"ERROR: unknown alpha_nloop in ALPHAEW: ", alpha_nloop
             STOP
          ENDIF
          DalphalepMZ=DeltaAlphaLep(zmass_PDG)
          DalphahadMZ=DeltaAlphaHad(zmass_PDG)
          DalphatopMZ=DeltaAlphaTop(zmass_PDG)
          !DalphaWMZ=DeltaAlphaW(zmass_PDG)
       ENDIF
       init=1
    ENDIF
    IF(alpha_scheme.LE.1)THEN
       ! alpha(0) or Gmu scheme
       ! we use the saved value
       ALPHAEW=ALPHAEW_SAVE
    ELSE
       ! alpha(Q) scheme
       ! alpha(Q)=alpha(0)/(1-Delta alpha(Q))
       ! Delta alpha(Q)=Delta alpha_{had}(Q)+Delta alpha_{lep}(Q)
       ! Delta alpha(Q)=Sigma_{AA}(0)-Re(Sigma_{AA}(Q))
       ! alpha(Q)=alpha(MZ)*(1-Delta alpha_{had}(MZ)-Delta alpha_{lep}(MZ))/
       !    (1-Delta alpha_{had}(MZ)-Delta alpha_{lep}(Q)+(Delta alpha_{had}(MZ)-Delta alpha_{had}(Q))),
       ! where Delta alpha_{had}(MZ)-Delta alpha_{had}(Q) we use the perturbative one-loop results [insensitive to light quark mass when Q > 1 GeV]
       DalphalepQ=DeltaAlphaLep(Q)
       DDalphahad=DalphahadMZ-DeltaAlphaHad(Q)
       DalphaTopQ=DeltaAlphaTop(Q)
       !DalphaWQ=DeltaAlphaW(Q)
       !ALPHAEW=1d0/alphaMZm1*(1d0-DalphahadMZ_PDG-DalphaTopMZ-DalphalepMZ)&
       !     /(1d0-DalphahadMZ_PDG-DalphalepQ-DalphaTopQ+DDalphahad)
       ! same as above
       ALPHAEW=1d0/alphaemm1/(1d0-DalphahadMZ_PDG-DalphalepQ-DalphaTopQ+DDalphahad)
    ENDIF
  END FUNCTION ALPHAEW

  FUNCTION ReSIGMAAA(Q,M)
    ! one-loop perturbation result of
    ! ReSigma_AA(Q)-Sigma_AA(0) for a given fermion
    ! It has been dropped alpha/(3*Pi)*Ncf*Qf**2
    IMPLICIT NONE
    REAL(KIND(1d0))::ReSIGMAAA
    REAL(KIND(1d0)),INTENT(IN)::Q,M
    COMPLEX(KIND(1d0))::SIGMAAA
    REAL(KIND(1d0))::MQ2Q2,LOGMQ2Q2
    COMPLEX(KIND(1d0))::sqrtt
    MQ2Q2=M**2/Q**2
    IF(4d0*MQ2Q2.EQ.1d0)THEN
       ReSIGMAAA=8d0/3d0
       RETURN
    ENDIF
    IF(MQ2Q2.GT.1d-8.AND.MQ2Q2.LT.1d5)THEN
       sqrtt=SQRT(DCMPLX(1d0-4d0*MQ2Q2,0d0))
       SIGMAAA=5d0/3d0+4d0*MQ2Q2+sqrtt*(1d0+2d0*MQ2Q2)*LOG((sqrtt-1d0)/(sqrtt+1d0))
    ELSEIF(MQ2Q2.LE.1d-8)THEN
       ! use the HE expansion when MQ2Q2 -> 0
       LOGMQ2Q2=LOG(MQ2Q2)
       SIGMAAA=LOGMQ2Q2+5d0/3d0+6d0*MQ2Q2+MQ2Q2**2*(3d0-6d0*LOGMQ2Q2)&
            +MQ2Q2**3*(-16d0/3d0-8d0*LOGMQ2Q2)&
            +MQ2Q2**4*(-33d0/2d0-18d0*LOGMQ2Q2)
    ELSEIF(MQ2Q2.GE.1d5)THEN
       ! use the LE expansion when MQ2Q2 -> infinity
       SIGMAAA=1d0/(5d0*MQ2Q2)+3d0/140d0/MQ2Q2**2+1d0/315d0/MQ2Q2**3&
            +1d0/1848d0/MQ2Q2**4+1d0/10010d0/MQ2Q2**5+1d0/51480d0/MQ2Q2**6&
            +1d0/255255d0/MQ2Q2**7
    ELSE
       WRITE(*,*)"ERROR: cannot reach here ReSIGMAAA"
       STOP
    ENDIF
    ReSIGMAAA=DREAL(SIGMAAA)
    RETURN
  END FUNCTION ReSIGMAAA

  FUNCTION ReSIGMAAA2L(Q,M)
    ! two-loop perturbation result of
    ! ReSigma_AA(Q)-Sigma_AA(0) for a given fermion
    ! It has been dropped alpha**2/(Pi**2)*Ncf*Qf**4 for NLO QED
    !                     alpha*aS/(Pi**2)*Ncf*Qf**2*CFf for NLO QCD
    IMPLICIT NONE
    REAL(KIND(1d0))::ReSIGMAAA2L
    REAL(KIND(1d0)),INTENT(IN)::Q,M
    COMPLEX(KIND(1d0))::SIGMAAA2L
    REAL(KIND(1d0))::MQ2Q2,LOGMQ2Q2
    COMPLEX(KIND(1d0))::vq,vqponeovervqmone,phi1,phi2,phi3,LL
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    REAL(KIND(1d0)),PARAMETER::zeta3=1.20205690315959428539973816151d0
    MQ2Q2=M**2/Q**2
    IF(4d0*MQ2Q2.EQ.1d0)THEN
       ! hit the threshold in vacuum polarization function
       ! return zero
       ReSIGMAAA2L=0d0
       RETURN
    ENDIF
    IF(MQ2Q2.GT.1d-8.AND.MQ2Q2.LT.1d3)THEN
       vq=SQRT(DCMPLX(1d0-4d0*MQ2Q2,0d0))
       vqponeovervqmone=(vq-1d0)/(vq+1d0)
       phi1=phin(1,vqponeovervqmone)
       phi2=phin(2,vqponeovervqmone)
       phi3=phin(3,vqponeovervqmone)
       LL=LOG(vqponeovervqmone)
       SIGMAAA2L=(vq**4-2d0*vq**2-3d0)/12d0*(phi1*LL**2-4d0*phi2*LL+6d0*phi3+3d0*zeta3)&
            +(3d0*vq-vq**3)/12d0*(4d0*phi1*LL-4d0*phi2+3d0*LL**2)&
            +(5d0*vq-3d0*vq**3)/8d0*LL+(7d0*vq**4-22d0*vq**2-33d0)/96d0*LL**2&
            -13d0*(vq**2-1d0)/24d0+5d0/24d0
    ELSEIF(MQ2Q2.LE.1d-8)THEN
       ! use the HE expansion when MQ2Q2 -> 0
       LOGMQ2Q2=LOG(MQ2Q2)
       SIGMAAA2L=5d0/24d0+0.25d0*LOGMQ2Q2-zeta3+3d0*MQ2Q2*LOGMQ2Q2&
            +MQ2Q2**2*(1d0/6d0+18d0*zeta2+4d0*zeta3+2.5d0*LOGMQ2Q2&
            -3d0*LOGMQ2Q2**2)+MQ2Q2**3*(248d0/27d0+116d0/3d0*zeta2&
            -188d0/27d0*LOGMQ2Q2-58d0/9d0*LOGMQ2Q2**2)
    ELSEIF(MQ2Q2.GE.1d3)THEN
       ! use the LE expansion when MQ2Q2 -> infinity
       SIGMAAA2L=41d0/162d0/MQ2Q2+449d0/10800d0/MQ2Q2**2&
            +62479d0/7938000d0/MQ2Q2**3+25993d0/16329600d0/MQ2Q2**4&
            +6756019d0/20170458000d0/MQ2Q2**5&
            +338452951d0/4674935865600d0/MQ2Q2**6
    ELSE
       WRITE(*,*)"ERROR: cannot reach here ReSIGMAAA2L"
       STOP
    ENDIF
    ReSIGMAAA2L=DREAL(SIGMAAA2L)
    RETURN
  END FUNCTION ReSIGMAAA2L

  ! Note W contribution is gauge dependent
  ! so that we should exclud it
  FUNCTION ReSIGMAAA1LW(Q,M)
    ! one-loop perturbation result of
    ! ReSigma_AA(Q)-Sigma_AA(0) for the W boson
    ! It has been dropped alpha/Pi
    IMPLICIT NONE
    REAL(KIND(1d0))::ReSIGMAAA1LW
    REAL(KIND(1d0)),INTENT(IN)::Q,M
    COMPLEX(KIND(1d0))::SIGMAAA
    REAL(KIND(1d0))::MQ2Q2,LOGMQ2Q2
    COMPLEX(KIND(1d0))::sqrtt
    MQ2Q2=M**2/Q**2
    IF(4d0*MQ2Q2.EQ.1d0)THEN
       ReSIGMAAA1LW=-11d0/6d0
       RETURN
    ENDIF
    IF(MQ2Q2.GT.1d-8.AND.MQ2Q2.LT.1d5)THEN
       sqrtt=SQRT(DCMPLX(1d0-4d0*MQ2Q2,0d0))
       SIGMAAA=-4d0/3d0-2d0*MQ2Q2+sqrtt*(-3d0/4d0-MQ2Q2)*LOG((sqrtt-1d0)/(sqrtt+1d0))
    ELSEIF(MQ2Q2.LE.1d-8)THEN
       ! use the HE expansion when MQ2Q2 -> 0
       LOGMQ2Q2=LOG(MQ2Q2)
       SIGMAAA=-3d0/4d0*LOGMQ2Q2-4d0/3d0&
            +MQ2Q2*(-7d0/2d0+0.5d0*LOGMQ2Q2)&
            +MQ2Q2**2*(-5d0/4d0+3.5d0*LOGMQ2Q2)&
            +MQ2Q2**3*(3.5d0+5d0*LOGMQ2Q2)&
            +MQ2Q2**4*(257d0/24d0+23d0/2d0*LOGMQ2Q2)
    ELSEIF(MQ2Q2.GE.1d5)THEN
       ! use the LE expansion when MQ2Q2 -> infinity
       SIGMAAA=-17d0/(120d0*MQ2Q2)-5d0/336d0/MQ2Q2**2&
            -11d0/5040d0/MQ2Q2**3-41d0/110880d0/MQ2Q2**4&
            -7d0/102960d0/MQ2Q2**5-19d0/1441440d0/MQ2Q2**6&
            -1d0/376992d0/MQ2Q2**7
    ELSE
       WRITE(*,*)"ERROR: cannot reach here ReSIGMAAA1LW"
       STOP
    ENDIF
    ReSIGMAAA1LW=DREAL(SIGMAAA)
    RETURN
  END FUNCTION ReSIGMAAA1LW

  FUNCTION DeltaAlphaLep(Q)
    USE LbL_Global
    ! one-loop, two loop QED
    ! Delta alpha_{lep}(Q)
    IMPLICIT NONE
    REAL(KIND(1d0))::DeltaAlphaLep
    REAL(KIND(1d0))::DeltaAlphaLep1L,DeltaAlphaLep2L
    REAL(KIND(1d0)),INTENT(IN)::Q
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    DeltaAlphaLep1L=ReSIGMAAA(Q,emass_PDG)
    DeltaAlphaLep1L=DeltaAlphaLep1L+ReSIGMAAA(Q,mumass_PDG)
    DeltaAlphaLep1L=DeltaAlphaLep1L+ReSIGMAAA(Q,taumass_PDG)
    DeltaAlphaLep1L=DeltaAlphaLep1L*(-1d0/(3d0*pipi*alphaemm1))
    IF(alpha_nloop.EQ.1)THEN
       DeltaAlphaLep=DeltaAlphaLep1L
       RETURN
    ENDIF
    DeltaAlphaLep2L=ReSIGMAAA2L(Q,emass_PDG)
    DeltaAlphaLep2L=DeltaAlphaLep2L+ReSIGMAAA2L(Q,mumass_PDG)
    DeltaAlphaLep2L=DeltaAlphaLep2L+ReSIGMAAA2L(Q,taumass_PDG)
    DeltaAlphaLep2L=DeltaAlphaLep2L*(-1d0/(pipi*alphaemm1)**2)
    DeltaAlphaLep=DeltaAlphaLep1L+DeltaAlphaLep2L
    RETURN
  END FUNCTION DeltaAlphaLep

  FUNCTION DeltaAlphaHad(Q)
    use LbL_Global
    ! one-loop, two loop QCD and QED
    ! Delta alpha_{had}(Q) [only 5 quark flavours]
    IMPLICIT NONE
    REAL(KIND(1d0))::DeltaAlphaHad
    REAL(KIND(1d0))::DeltaAlphaHad1L,DeltaAlphaHad2L
    REAL(KIND(1d0))::DeltaAlphaHad2LQCD,DeltaAlphaHad2LQED
    REAL(KIND(1d0)),INTENT(IN)::Q
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::CF=4d0/3d0
    REAL(KIND(1d0))::aSatQ
    DeltaAlphaHad1L=ReSIGMAAA(Q,umass_PDG)*(2d0/3d0)**2
    DeltaAlphaHad1L=DeltaAlphaHad1L+ReSIGMAAA(Q,dmass_PDG)*(-1d0/3d0)**2
    DeltaAlphaHad1L=DeltaAlphaHad1L+ReSIGMAAA(Q,smass_PDG)*(-1d0/3d0)**2
    DeltaAlphaHad1L=DeltaAlphaHad1L+ReSIGMAAA(Q,cmass_PDG)*(2d0/3d0)**2
    DeltaAlphaHad1L=DeltaAlphaHad1L+ReSIGMAAA(Q,bmass_PDG)*(-1d0/3d0)**2
    DeltaAlphaHad1L=DeltaAlphaHad1L*(-1d0/(pipi*alphaemm1))
    IF(alpha_nloop.EQ.1)THEN
       DeltaAlphaHad=DeltaAlphaHad1L
       RETURN
    ENDIF
    IF(Q.GE.1d0)THEN
       aSatQ=ALPHAS(Q)
    ELSE
       ! turn off two-loop QCD below 1 GeV
       aSatQ=0d0
    ENDIF
    DeltaAlphaHad2L=ReSIGMAAA2L(Q,umass_PDG)+ReSIGMAAA2L(Q,cmass_PDG)
    DeltaAlphaHad2LQED=DeltaAlphaHad2L*(2d0/3d0)**4*(-3d0)/(pipi*alphaemm1)**2
    DeltaAlphaHad2LQCD=DeltaAlphaHad2L*(2d0/3d0)**2*(-3d0)*CF*aSatQ/(pipi**2*alphaemm1)
    DeltaAlphaHad2L=ReSIGMAAA2L(Q,dmass_PDG)+ReSIGMAAA2L(Q,smass_PDG)&
         +ReSIGMAAA2L(Q,bmass_PDG)
    DeltaAlphaHad2LQED=DeltaAlphaHad2LQED+&
         DeltaAlphaHad2L*(-1d0/3d0)**4*(-3d0)/(pipi*alphaemm1)**2
    DeltaAlphaHad2LQCD=DeltaAlphaHad2LQCD+&
         DeltaAlphaHad2L*(-1d0/3d0)**2*(-3d0)*CF*aSatQ/(pipi**2*alphaemm1)
    DeltaAlphaHad=DeltaAlphaHad1L+DeltaAlphaHad2LQED+DeltaAlphaHad2LQCD
    RETURN
  END FUNCTION DeltaAlphaHad

  FUNCTION DeltaAlphaTop(Q)
    use LbL_Global
    ! one-loop, two-loop QED and QCD
    ! Delta alpha_{t}(Q)
    IMPLICIT NONE
    REAL(KIND(1d0))::DeltaAlphaTop
    REAL(KIND(1d0))::DeltaAlphaTop1L,DeltaAlphaTop2L
    REAL(KIND(1d0))::DeltaAlphaTop2LQCD,DeltaAlphaTop2LQED
    REAL(KIND(1d0)),INTENT(IN)::Q
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::CF=4d0/3d0
    REAL(KIND(1d0))::aSatQ
    DeltaAlphaTop1L=ReSIGMAAA(Q,tmass_PDG)*(2d0/3d0)**2
    DeltaAlphaTop1L=DeltaAlphaTop1L*(-1d0/(pipi*alphaemm1))
    IF(alpha_nloop.EQ.1)THEN
       DeltaAlphaTop=DeltaAlphaTop1L
       RETURN
    ENDIF
    IF(Q.GE.1d0)THEN
       aSatQ=ALPHAS(Q)
    ELSE
       ! turn off two-loop QCD below 1 GeV
       aSatQ=0d0
    ENDIF
    DeltaAlphaTop2L=ReSIGMAAA2L(Q,tmass_PDG)
    DeltaAlphaTop2LQED=DeltaAlphaTop2L*(2d0/3d0)**4*(-3d0)/(pipi*alphaemm1)**2
    DeltaAlphaTop2LQCD=DeltaAlphaTop2L*(2d0/3d0)**2*(-3d0)*CF*aSatQ/(pipi**2*alphaemm1)
    DeltaAlphaTop=DeltaAlphaTop1L+DeltaAlphaTop2LQED+DeltaAlphaTop2LQCD
    RETURN
  END FUNCTION DeltaAlphaTop

  ! Note W contribution is gauge dependent
  ! so that we should exclud it
  FUNCTION DeltaAlphaW(Q)
    use LbL_Global
    ! one-loop
    ! Delta alpha_{W}(Q)
    IMPLICIT NONE
    REAL(KIND(1d0))::DeltaAlphaW
    REAL(KIND(1d0))::DeltaAlphaW1L
    REAL(KIND(1d0)),INTENT(IN)::Q
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    DeltaAlphaW1L=ReSIGMAAA1LW(Q,wmass_PDG)
    DeltaAlphaW1L=DeltaAlphaW1L*(-1d0/(pipi*alphaemm1))
    DeltaAlphaW=DeltaAlphaW1L
    RETURN
  END FUNCTION DeltaAlphaW

  SUBROUTINE Evaluate_DalphahadMZ(alphaemm1,alphaMZm1,MZ,DalphahadMZ)
    ! evaluate Delta alpha_{had}^{(5)}(mZ**2) from input
    ! alpha(0)=1/alphaemm1, alphaMZ=1/alphaMZm1
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::alphaemm1,alphaMZm1,MZ
    REAL(KIND(1d0)),INTENT(OUT)::DalphahadMZ
    REAL(KIND(1d0))::DeltaAlphaLepMZ,DeltaAlphaTopMZ !,DeltaAlphaWMZ
    DeltaAlphaLepMZ=DeltaAlphaLep(MZ)
    DeltaAlphaTopMZ=DeltaAlphaTop(MZ)
    !DeltaAlphaWMZ=DeltaAlphaW(MZ)
    DalphahadMZ=1d0-DeltaAlphaLepMZ-DeltaAlphaTopMZ-alphaMZm1/alphaemm1
    RETURN
  END SUBROUTINE Evaluate_DalphahadMZ

  FUNCTION phin(n,x)
    ! phin(n,x)=Lin(x)+2Lin(-x)
    USE Func_PSI
    USE nielsen_generalized_polylog_wrapper
    IMPLICIT NONE
    INTEGER,INTENT(IN)::n
    COMPLEX(KIND(1d0)),INTENT(IN)::x
    COMPLEX(KIND(1d0))::phin
    IF(n.LE.0.OR.n.GE.7)THEN
       WRITE(*,*)"ERROR: n<=0 or n>=7"
       STOP
    ENDIF
    IF(n.EQ.1)THEN
       ! Li1(x)=-log(1-x)
       phin=-log(1d0-x)-2d0*log(1d0+x)
    ELSEIF(n.EQ.2)THEN
       phin=li2(x)+2d0*li2(-x)
    ELSEIF(n.EQ.3)THEN
       phin=cdli3(x)+2d0*cdli3(-x)
    ELSEIF(n.EQ.4)THEN
       phin=cdli4(x)+2d0*cdli4(-x)
    ELSEIF(n.EQ.5)THEN
       phin=cdli5(x)+2d0*cdli5(-x)
    ELSEIF(n.EQ.6)THEN
       phin=cdli6(x)+2d0*cdli6(-x)
    ELSE
       WRITE(*,*)"ERROR: cannot reach here"
       STOP
    ENDIF
    RETURN
  END FUNCTION phin
END MODULE evaluate_couplings
