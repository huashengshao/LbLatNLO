MODULE NLOME
  IMPLICIT NONE
CONTAINS
  subroutine stwoloopmatrix_LbL(pborn,virt_wgts)
    use kinetics
    use UToneloopbasis
    use LbL_Global
    use qcd_constants
    use evaluate_couplings
    use LbL_setscale
    ! helicity summed amplitude square
    ! from helicity amplitudes
    ! this is applicable for fermions and W boson
    implicit none
    integer NDIM_1L,NDIM_2L
    parameter (NDIM_1L=9,NDIM_2L=84)
    double precision virt_wgts,virt_wgts_up,virt_wgts_down
    double precision pborn(4,0:3)
    double precision shat,that,uhat
    double precision xs,xt,xu,MQ,MQ2,MQ4,MUR
    double precision logm2omu2
    double complex loopba1L(NDIM_1L),loopba1L2(NDIM_1L)
    double complex loopba2L(NDIM_2L)
    double complex amp1L(5),camp1L(5),amptmp,amptmpp,amptmpp2
    double complex amp2L(5),camp2L(5),ampstmp(5)
    double complex amp2L_up(5),camp2L_up(5)
    double complex amp2L_down(5),camp2L_down(5)
    integer i,j,kk1
    ! averaged over initial state and final state symmetry
    integer IDEN
    parameter (IDEN=8)
    ! massless fermions
    double complex OneLoop_HelAmp_Massless
    external OneLoop_HelAmp_Massless
    double complex TwoLoop_HelAmp_Massless
    external TwoLoop_HelAmp_Massless
    ! massive fermions
    double complex OneLoop_HelAmp_Massive
    external OneLoop_HelAmp_Massive
    ! massive W boson
    double complex OneLoop_HelAmp_MassiveW
    external OneLoop_HelAmp_MassiveW
    ! low-energy limit for fermions
    !double complex OneLoop_HelAmp_LowEnergyLimit
    !external  OneLoop_HelAmp_LowEnergyLimit
    !double complex TwoLoop_HelAmp_LowEnergyLimit
    !external  TwoLoop_HelAmp_LowEnergyLimit
    ! low-energy expansion for fermions
    double complex OneLoop_HelAmp_LE
    external OneLoop_HelAmp_LE
    double complex TwoLoop_HelAmp_LE
    external TwoLoop_HelAmp_LE
    ! low-energy limit for W boson
    !double complex OneLoop_HelAmp_LowEnergyLimitW
    !external  OneLoop_HelAmp_LowEnergyLimitW
    ! low-energy expansion for W boson
    double complex OneLoopW_HelAmp_LE
    external OneLoopW_HelAmp_LE
    ! check whether we use the low-energy limit
    !integer massive_1LAmp_xs_LElimit
    !external massive_1LAmp_xs_LElimit
    !integer LElimit
    double precision log10xs,y
    double complex prefactor,prefac01L,prefac02LQCD,prefac02LQED
    integer i_mass,i_massless
    double complex prefac1L(10),prefac2LQCD(10),prefac2LQED(10)
    double precision mass(10)
    integer init,init_grid
    data init/0/
    data init_grid/0/
    save init,init_grid,i_mass,i_massless,prefac01L,prefac1L,mass
    save prefac02LQCD,prefac02LQED,prefac2LQCD,prefac2LQED
    double precision pipi
    parameter (pipi=3.14159265358979323846264338328d0)
    double precision alpham1,as_cen,as_up,as_down
    double precision aew_cen,aew_up,aew_down
    shat=sumdot2(pborn(1,0:3),pborn(2,0:3),1d0)
    that=sumdot2(pborn(2,0:3),pborn(3,0:3),-1d0)
    uhat=sumdot2(pborn(1,0:3),pborn(3,0:3),-1d0)
    ! scale (should be independent since it is finite)
    if(init.eq.0)then
       ! initialisation
       prefactor=dcmplx(0d0,8d0*alphaemm1**(-2))
       AQEDUP=alphaemm1**(-1)
       i_mass=0
       i_massless=0
       prefac01L=dcmplx(0d0,0d0)
       prefac02LQED=dcmplx(0d0,0d0)
       if(alpha_scheme.LE.1)then
          alpham1=ALPHAEW(1d0)
          AQEDUP=alpham1
          alpham1=1d0/alpham1
       endif
       IF(umass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(2d0/3d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             ! do not include aEWM1 since it will be calcualted dynamically
             prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(umass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=umass
          prefac1L(i_mass)=3d0*(2d0/3d0)**4*prefactor
          ! do not include aS
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**4&
               *CF/pipi
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
                  /pipi
          endif
       ENDIF
       IF(dmass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(-1d0/3d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(dmass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=dmass
          prefac1L(i_mass)=3d0*(-1d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**4&
               *CF/pipi
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
                  /pipi
          endif
       ENDIF
       IF(smass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(-1d0/3d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(smass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=smass
          prefac1L(i_mass)=3d0*(-1d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**4&
               *CF/pipi
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
                  /pipi
          endif
       ENDIF
       IF(cmass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(2d0/3d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(cmass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=cmass
          prefac1L(i_mass)=3d0*(2d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**4&
               *CF/pipi
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
                  /pipi
          endif
       ENDIF
       IF(bmass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(-1d0/3d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(bmass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=bmass
          prefac1L(i_mass)=3d0*(-1d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**4&
               *CF/pipi
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
                  /pipi
          endif
       ENDIF
       IF(tmass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(2d0/3d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(tmass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=tmass
          prefac1L(i_mass)=3d0*(2d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**4&
               *CF/pipi
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
                  /pipi
          endif
       ENDIF
       prefac02LQCD=prefac01L*CF/pipi
       IF(emass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+(-1d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(emass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=emass
          prefac1L(i_mass)=(-1d0)**4*prefactor
          prefac2LQCD(i_mass)=0d0
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
                  /pipi
          endif
       ENDIF
       IF(mumass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+(-1d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(mumass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=mumass
          prefac1L(i_mass)=(-1d0)**4*prefactor
          prefac2LQCD(i_mass)=0d0
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
                  /pipi
          endif
       ENDIF
       IF(taumass.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+(-1d0)**4*prefactor
          if(alpha_scheme.LE.1)then
             prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
                  *alpham1**(-1)/pipi
          else
             prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
                  /pipi
          endif
       ELSEIF(taumass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=taumass
          prefac1L(i_mass)=(-1d0)**4*prefactor
          prefac2LQCD(i_mass)=0d0
          if(alpha_scheme.LE.1)then
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
                  *alpham1**(-1)/pipi
          else
             prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
                  /pipi
          endif
       ENDIF
       IF(wmass.EQ.0d0)THEN
          WRITE(*,*)"ERROR: wmass=0 !"
          STOP
       ELSEIF(wmass.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=wmass
          prefac1L(i_mass)=-1.5d0*prefactor
          prefac2LQCD(i_mass)=0d0
          prefac2LQED(i_mass)=0d0
       ENDIF
       init=1
    endif

    if(abs(order).GT.3)then
       write(*,*)"ERROR: do not know order=",order
       stop
    endif

    amp1L(1:5)=dcmplx(0d0,0d0)
    amp2L(1:5)=dcmplx(0d0,0d0)
    amp2L_up(1:5)=amp2L(1:5)
    amp2L_down(1:5)=amp2L(1:5)
    wgtxsecmu(1:3)=1d0
    CALL LbL_scale(MUR)
    if(abs(order).ge.2)then
       ! we must have QCD corrections
       as_cen=ALPHAS(MUR)
       AQCDUP=as_cen
       if(reweight_scale)then
          as_up=ALPHAS(MUR*rw_RScale_up)
          as_down=ALPHAS(MUR*rw_RScale_down)
       else
          as_up=as_cen
          as_down=as_cen
       endif
    endif
    if((abs(order).eq.1.or.abs(order).eq.3).and.alpha_scheme.eq.2)then
       ! we must have QED corrections
       ! and we use alpha(mu) scheme
       aew_cen=ALPHAEW(MUR)
       AQEDUP=aew_cen
       if(reweight_scale)then
          aew_up=ALPHAEW(MUR*rw_RScale_up)
          aew_down=ALPHAEW(MUR*rw_RScale_down)
       else
          aew_up=aew_cen
          aew_down=aew_cen
       endif
    endif

    ! massless fermion loops
    IF(i_massless.GT.0)then
       DO i=1,5
          amp1L(i)=OneLoop_HelAmp_Massless(i,shat,that,uhat)
          amp1L(i)=amp1L(i)*prefac01L
       ENDDO
       if(abs(order).GT.0)then
          DO i=1,5
             amp2L(i)=TwoLoop_HelAmp_Massless(i,shat,that,uhat)
             if(abs(order).eq.1)then
                ! only QED
                amp2L(i)=amp2L(i)*prefac02LQED
                if(alpha_scheme.eq.2)then
                   amp2L_up(i)=amp2L(i)*aew_up
                   amp2L_down(i)=amp2L(i)*aew_down
                   amp2L(i)=amp2L(i)*aew_cen
                else
                   amp2L_up(i)=amp2L(i)
                   amp2L_down(i)=amp2L(i)
                endif
             elseif(abs(order).eq.2)then
                ! only QCD
                amp2L(i)=amp2L(i)*prefac02LQCD
                amp2L_up(i)=amp2L(i)*as_up
                amp2L_down(i)=amp2L(i)*as_down
                amp2L(i)=amp2L(i)*as_cen
             else
                ! QED+QCD
                if(alpha_scheme.eq.2)then
                   amp2L_up(i)=amp2L(i)*(prefac02LQCD*as_up&
                        +prefac02LQED*aew_up)
                   amp2L_down(i)=amp2L(i)*(prefac02LQCD*as_down&
                        +prefac02LQED*aew_down)
                   amp2L(i)=amp2L(i)*(prefac02LQCD*as_cen&
                        +prefac02LQED*aew_cen)
                else
                   amp2L_up(i)=amp2L(i)*(prefac02LQCD*as_up&
                        +prefac02LQED)
                   amp2L_down(i)=amp2L(i)*(prefac02LQCD*as_down&
                        +prefac02LQED)
                   amp2L(i)=amp2L(i)*(prefac02LQCD*as_cen&
                        +prefac02LQED)
                endif
             endif
          ENDDO
       endif
    ENDIF

    ! massive fermion and massive W boson loops
    DO i=1,i_mass
       MQ=mass(i)
       MQ2=MQ**2
       xs=shat/MQ2
       xt=that/MQ2
       xu=uhat/MQ2
       log10xs=DLOG10(xs)
       y=-xt/(xs/2d0)
       ! use OneLOop (faster)
       call loopbasis(MQ,shat,that,MUR,loopba1L)
       logm2omu2=DLOG(MQ2/MUR**2)
       call UToneloopbasis_scalar(xs,xt,logm2omu2,loopba1L,loopba1L2)
       if(i.eq.i_mass.and.wmass.GT.0d0)then
          ! it must be the W boson
          DO j=1,5
             if(improve_w_LE.NE.1)then
                amptmp=OneLoop_HelAmp_MassiveW(j,xs,xt,xu,loopba1L2)
                amptmpp=amptmp
             endif
             if(improve_w_LE.EQ.-1)then
                ! improved with low-energy expansion
                if(xs.LE.1d0)then
                   !LElimit=massive_1LAmp_xs_LElimit(log10xs,y,j)
                   !amptmp=OneLoop_HelAmp_LowEnergyLimitW(j,xs,xt,xu)
                   amptmp=OneLoopW_HelAmp_LE(j,-1,xs,xt,xu)
                   !amptmpp2=amptmp
                   !if(LElimit.eq.0)then
                   !  amptmp=amptmpp2
                   !else
                   !   amptmp=amptmpp
                   !endif
                endif
             elseif(improve_w_LE.EQ.1)then
                ! simply use the low-energy expansion
                !amptmp=OneLoop_HelAmp_LowEnergyLimitW(j,xs,xt,xu)
                amptmp=OneLoopW_HelAmp_LE(j,-1,xs,xt,xu)
             endif
             amp1L(j)=amp1L(j)+amptmp*prefac1L(i)
          ENDDO
       else
          ! it is the massive fermion
          DO j=1,5
             if(improve_w_LE.NE.1)then
                amptmp=OneLoop_HelAmp_Massive(j,xs,xt,xu,loopba1L2)
                amptmpp=amptmp
             endif
             if(improve_w_LE.EQ.-1)then
                ! improved with low-energy expansion
                if(xs.LE.1d0)then
                   !LElimit=massive_1LAmp_xs_LElimit(log10xs,y,j)
                   !amptmp=OneLoop_HelAmp_LowEnergyLimit(j,xs,xt,xu)
                   amptmp=OneLoop_HelAmp_LE(j,-1,xs,xt,xu)
                   amptmp=amptmp/8d0 ! dividing by 1/8 because of the prefactor in LE
                   !amptmpp2=amptmp
                   !if(LElimit.eq.0)then
                   !   amptmp=amptmpp2
                   !else
                   !   amptmp=amptmpp
                   !endif
                endif
             elseif(improve_w_LE.EQ.1)then
                ! simply use the low-energy expansion
                !amptmp=OneLoop_HelAmp_LowEnergyLimit(j,xs,xt,xu)
                amptmp=OneLoop_HelAmp_LE(j,-1,xs,xt,xu)
                amptmp=amptmp/8d0 ! dividing by 1/8 because of the prefactor in LE
             endif
             amp1L(j)=amp1L(j)+amptmp*prefac1L(i)
          ENDDO
       endif
       if(abs(order).GT.0)then
          if((i.lt.i_mass.and.wmass.GT.0d0).or.wmass.LT.0d0)then
             ! we do not have NLO for W boson
             if(improve_w_LE.NE.1)then
                if(grid_gen.eq.1.and.init_grid.eq.1)then
                   call eval_twoloop_massive_amplitudes(xs,xt,ampstmp,0)
                else
                   OPEN(UNIT=3033,FILE="../check_interp/amp2L_in_xs_interp_y0.06.dat")
                   DO kk1=0,63
                      xs=1.2d0**kk1
                      !DO kk2=1,30
                      !   xt=-xs/2d0*DBLE(kk2)/30d0
                      xt=-xs/2d0*0.06d0                
                   call eval_twoloop_massive_amplitudes(xs,xt,ampstmp,grid_gen)
                      WRITE(3033,*)xs,dreal(ampstmp(1)),dimag(ampstmp(1)),&
                           dreal(ampstmp(2)),dimag(ampstmp(2)),&
                           dreal(ampstmp(3)),dimag(ampstmp(3)),&
                           dreal(ampstmp(4)),dimag(ampstmp(4)),&
                           dreal(ampstmp(5)),dimag(ampstmp(5))
                      !ENDDO
                   ENDDO
                   CLOSE(UNIT=3033)
                   stop
                   if(grid_gen.eq.1.and.init_grid.eq.0)init_grid=1
                endif
             endif

             if(improve_w_LE.EQ.-1)then
                ! improve with the low-energy expansion results
                if(xs.LE.1d0)then
                   DO j=1,5
                      !LElimit=massive_2LAmp_xs_LElimit(log10xs,y,j)
                      !if(LElimit.EQ.0)then
                      !   amptmp=TwoLoop_HelAmp_LowEnergyLimit(j,xs,xt,xu)
                      !   ampstmp(j)=amptmp
                      !endif
                      amptmp=TwoLoop_HelAmp_LE(j,-1,xs,xt,xu)
                      ampstmp(j)=amptmp
                   ENDDO
                endif
             elseif(improve_w_LE.EQ.1)then
                ! simply use the low-energy expansion
                DO j=1,5
                   !amptmp=TwoLoop_HelAmp_LowEnergyLimit(j,xs,xt,xu)
                   amptmp=TwoLoop_HelAmp_LE(j,-1,xs,xt,xu)
                   ampstmp(j)=amptmp
                ENDDO
             endif
             ! ! first multiply sqrts in the one-loop integrals that appeared
             ! ! at one-loop
             ! CALL UToneloopbasis_nosqrt(xs,xt,loopba1L2,loopba1L)
             ! ! get two-loop integrals
             ! CALL UTtwoloopbasis(xs,xt,loopba1L,loopba2L)
             ! ! get the two-loop helicity amplitudes
             ! CALL Get_TwoLoop_HelAmp_Massive(xs,xt,xu,loopba2L&
             !     ,ampstmp)
             if(abs(order).eq.2.and.prefac2LQCD(i).NE.0d0)then
                ! only QCD
                DO j=1,5
                   amptmp=ampstmp(j)
                   amp2L_up(j)=amp2L_up(j)+amptmp*prefac2LQCD(i)*as_up
                   amp2L_down(j)=amp2L_down(j)+amptmp*prefac2LQCD(i)*as_down
                   amp2L(j)=amp2L(j)+amptmp*prefac2LQCD(i)*as_cen
                ENDDO
             elseif(abs(order).eq.1)then
                ! only QED
                if(alpha_scheme.eq.2)then
                   DO j=1,5
                      amptmp=ampstmp(j)
                      amp2L(j)=amp2L(j)+amptmp*prefac2LQED(i)*aew_cen
                      amp2L_up(j)=amp2L_up(j)+amptmp*prefac2LQED(i)*aew_up
                      amp2L_down(j)=amp2L_down(j)+amptmp*prefac2LQED(i)*aew_down
                   ENDDO
                else
                   DO j=1,5
                      amptmp=ampstmp(j)
                      amp2L(j)=amp2L(j)+amptmp*prefac2LQED(i)
                      amp2L_up(j)=amp2L_up(j)+amptmp*prefac2LQED(i)
                      amp2L_down(j)=amp2L_down(j)+amptmp*prefac2LQED(i)
                   ENDDO
                endif
             else
                ! QED+QCD
                if(alpha_scheme.eq.2)then
                   DO j=1,5
                      amptmp=ampstmp(j)
                      amp2L(j)=amp2L(j)+amptmp*(prefac2LQCD(i)*as_cen&
                           +prefac2LQED(i)*aew_cen)
                      amp2L_up(j)=amp2L_up(j)+amptmp*(prefac2LQCD(i)*as_up&
                           +prefac2LQED(i)*aew_up)
                      amp2L_down(j)=amp2L_down(j)+amptmp*(prefac2LQCD(i)*as_down&
                           +prefac2LQED(i)*aew_down)
                   ENDDO
                else
                   DO j=1,5
                      amptmp=ampstmp(j)
                      amp2L(j)=amp2L(j)+amptmp*(prefac2LQCD(i)*as_cen&
                           +prefac2LQED(i))
                      amp2L_up(j)=amp2L_up(j)+amptmp*(prefac2LQCD(i)*as_up&
                           +prefac2LQED(i))
                      amp2L_down(j)=amp2L_down(j)+amptmp*(prefac2LQCD(i)*as_down&
                           +prefac2LQED(i))
                   ENDDO
                endif
             endif
          endif
       endif
    ENDDO

    ! helicity-summed amplitude square
    DO i=1,5
       camp1L(i)=DCONJG(amp1L(i))
       camp2L(i)=DCONJG(amp2L(i))
       camp2L_up(i)=DCONJG(amp2L_up(i))
       camp2L_down(i)=DCONJG(amp2L_down(i))
    enddo
    if(order.GE.0)then
       virt_wgts=amp1L(1)*camp1L(1)*2d0+amp1L(2)*camp1L(2)*8d0&
            +amp1L(3)*camp1L(3)*2d0+amp1L(4)*camp1L(4)*2d0&
            +amp1L(5)*camp1L(5)*2d0
    endif
    if(order.GT.0)then
       ! NLO in amplitude square
       if(reweight_scale)then
          virt_wgts_up=virt_wgts+2d0*DREAL(amp2L_up(1)*camp1L(1)*2d0&
               +amp2L_up(2)*camp1L(2)*8d0+amp2L_up(3)*camp1L(3)*2d0&
               +amp2L_up(4)*camp1L(4)*2d0+amp2L_up(5)*camp1L(5)*2d0)
          virt_wgts_down=virt_wgts+2d0*DREAL(amp2L_down(1)*camp1L(1)*2d0&
               +amp2L_down(2)*camp1L(2)*8d0+amp2L_down(3)*camp1L(3)*2d0&
               +amp2L_down(4)*camp1L(4)*2d0+amp2L_down(5)*camp1L(5)*2d0)
       endif
       virt_wgts=virt_wgts+2d0*DREAL(amp2L(1)*camp1L(1)*2d0&
            +amp2L(2)*camp1L(2)*8d0+amp2L(3)*camp1L(3)*2d0&
            +amp2L(4)*camp1L(4)*2d0+amp2L(5)*camp1L(5)*2d0)
       if(reweight_scale.and.virt_wgts.ne.0d0)then
          wgtxsecmu(2)=virt_wgts_up/virt_wgts
          wgtxsecmu(3)=virt_wgts_down/virt_wgts
       endif
    elseif(order.LT.0)then
       ! NLO in amplitude
       DO i=1,5
          amp2L(i)=amp1L(i)+amp2L(i)
          amp2L_up(i)=amp1L(i)+amp2L_up(i)
          amp2L_down(i)=amp1L(i)+amp2L_down(i)
          camp2L(i)=camp1L(i)+camp2L(i)
          camp2L_up(i)=camp1L(i)+camp2L_up(i)
          camp2L_down(i)=camp1L(i)+camp2L_down(i)
       ENDDO
       if(reweight_scale)then
          virt_wgts_up=amp2L_up(1)*camp2L_up(1)*2d0+amp2L_up(2)*camp2L_up(2)*8d0&
            +amp2L_up(3)*camp2L_up(3)*2d0+amp2L_up(4)*camp2L_up(4)*2d0&
            +amp2L_up(5)*camp2L_up(5)*2d0
          virt_wgts_down=amp2L_down(1)*camp2L_down(1)*2d0+amp2L_down(2)*camp2L_down(2)*8d0&
            +amp2L_down(3)*camp2L_down(3)*2d0+amp2L_down(4)*camp2L_down(4)*2d0&
            +amp2L_down(5)*camp2L_down(5)*2d0
       endif
       virt_wgts=amp2L(1)*camp2L(1)*2d0+amp2L(2)*camp2L(2)*8d0&
            +amp2L(3)*camp2L(3)*2d0+amp2L(4)*camp2L(4)*2d0&
            +amp2L(5)*camp2L(5)*2d0
       if(reweight_scale.and.virt_wgts.ne.0d0)then
          wgtxsecmu(2)=virt_wgts_up/virt_wgts
          wgtxsecmu(3)=virt_wgts_down/virt_wgts
       endif
    endif
    virt_wgts=virt_wgts/DBLE(IDEN)
    return
  end subroutine stwoloopmatrix_LbL

  subroutine stwoloopmatrix(pborn,virt_wgts)
    use kinetics
    use UToneloopbasis
    ! helicity summed amplitude square
    ! from helicity amplitudes
    ! this is applicable for fermions and W boson
    implicit none
    integer iorder
    ! A(0,0) one-loop amplitude
    ! A(1,0) two-loop QCD amplitude
    ! A(0,1) two-loop QED amplitude
    !  0: LO |A(0,0)|**2
    !  1: NLO QED |A(0,0)|**2+2*Re(DCONJG(A(0,0))*A(0,1))
    !  2: NLO QCD |A(0,0)|**2+2*Re(DCONJG(A(0,0))*A(1,0))
    !  3: NLO QED+QCD |A(0,0)|**2+2*Re(DCONJG(A(0,0))*(A(1,0)+A(0,1)))
    ! -1: NLO QED |A(0,0)+A(0,1)|**2
    ! -2: NLO QCD |A(0,0)+A(1,0)|**2
    ! -3: NLO QED+QCD |A(0,0)+A(0,1)+A(1,0)|**2
    common /LbL_order/ iorder
    integer NDIM_1L,NDIM_2L
    parameter (NDIM_1L=9,NDIM_2L=84)
    ! if mass is negative, it means we do not want to include its contribution
    double precision aEWM1,MU,MD,MS,MC,MB,MT
    double precision ME,MM,MTA,MW
    common /Params/ aEWM1,MU,MD,MS,MC,MB,MT,ME,MM,MTA,MW
    ! the couplings for the quantum corrections
    double precision aS,aEWM1atmu
    common /Params_NLO/ aS,aEWM1atmu
    integer iloopbasis
    common /LbL_loop_1L/ iloopbasis ! 0: UT basis with oneloop for massive fermions and W bosons
    ! 1: UT basis with G's for massive fermions and W bosons
    double precision virt_wgts
    double precision pborn(4,0:3)
    double precision shat,that,uhat
    double precision xs,xt,xu,MQ,MQ2,MQ4,MUR
    double precision logm2omu2
    double complex loopba1L(NDIM_1L),loopba1L2(NDIM_1L)
    double complex loopba2L(NDIM_2L)
    double complex amp1L(5),camp1L(5),amptmp
    double complex amp2L(5),camp2L(5),ampstmp(5)
    integer i,j
    ! averaged over initial state and final state symmetry
    integer IDEN
    parameter (IDEN=8)
    ! massless fermions
    double complex OneLoop_HelAmp_Massless
    external OneLoop_HelAmp_Massless
    double complex TwoLoop_HelAmp_Massless
    external TwoLoop_HelAmp_Massless
    ! massive fermions
    double complex OneLoop_HelAmp_Massive
    external OneLoop_HelAmp_Massive
    ! massive W boson
    double complex OneLoop_HelAmp_MassiveW
    external OneLoop_HelAmp_MassiveW
    double complex prefactor,prefac01L,prefac02LQCD,prefac02LQED
    integer i_mass,i_massless
    double complex prefac1L(10),prefac2LQCD(10),prefac2LQED(10)
    double precision mass(10)
    integer init
    data init/0/
    save init,i_mass,i_massless,prefac01L,prefac1L,mass
    save prefac02LQCD,prefac02LQED,prefac2LQCD,prefac2LQED
    double precision pipi,CF
    parameter (pipi=3.14159265358979323846264338328d0)
    parameter (CF=1.33333333333333333333333333333d0)
    shat=sumdot2(pborn(1,0:3),pborn(2,0:3),1d0)
    that=sumdot2(pborn(2,0:3),pborn(3,0:3),-1d0)
    uhat=sumdot2(pborn(1,0:3),pborn(3,0:3),-1d0)
    ! scale (should be independent since it is finite)
    MUR=10d0
    if(init.eq.0)then
       ! initialisation
       prefactor=dcmplx(0d0,8d0*aEWM1**(-2))
       i_mass=0
       i_massless=0
       prefac01L=dcmplx(0d0,0d0)
       prefac02LQED=dcmplx(0d0,0d0)
       IF(MU.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(2d0/3d0)**4*prefactor
          prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(MU.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MU
          prefac1L(i_mass)=3d0*(2d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**4&
               *CF*aS/pipi
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       IF(MD.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(-1d0/3d0)**4*prefactor
          prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(MD.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MD
          prefac1L(i_mass)=3d0*(-1d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**4&
               *CF*aS/pipi
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       IF(MS.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(-1d0/3d0)**4*prefactor
          prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(MS.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MS
          prefac1L(i_mass)=3d0*(-1d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**4&
               *CF*aS/pipi
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       IF(MC.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(2d0/3d0)**4*prefactor
          prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(MC.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MC
          prefac1L(i_mass)=3d0*(2d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**4&
               *CF*aS/pipi
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       IF(MB.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(-1d0/3d0)**4*prefactor
          prefac02LQED=prefac02LQED+3d0*(-1d0/3d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(MB.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MB
          prefac1L(i_mass)=3d0*(-1d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**4&
               *CF*aS/pipi
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(-1d0/3d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       IF(MT.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+3d0*(2d0/3d0)**4*prefactor
          prefac02LQED=prefac02LQED+3d0*(2d0/3d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(MT.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MT
          prefac1L(i_mass)=3d0*(2d0/3d0)**4*prefactor
          prefac2LQCD(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**4&
               *CF*aS/pipi
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*3d0*(2d0/3d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       prefac02LQCD=prefac01L*CF*aS/pipi
       IF(ME.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+(-1d0)**4*prefactor
          prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(ME.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=ME
          prefac1L(i_mass)=(-1d0)**4*prefactor
          prefac2LQCD(i_mass)=0d0
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       IF(MM.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+(-1d0)**4*prefactor
          prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(MM.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MM
          prefac1L(i_mass)=(-1d0)**4*prefactor
          prefac2LQCD(i_mass)=0d0
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       IF(MTA.EQ.0d0)THEN
          i_massless=i_massless+1
          prefac01L=prefac01L+(-1d0)**4*prefactor
          prefac02LQED=prefac02LQED+(-1d0)**6*prefactor&
               *aEWM1atmu**(-1)/pipi
       ELSEIF(MTA.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MTA
          prefac1L(i_mass)=(-1d0)**4*prefactor
          prefac2LQCD(i_mass)=0d0
          prefac2LQED(i_mass)=-1d0/8d0*prefactor*(-1d0)**6&
               *aEWM1atmu**(-1)/pipi
       ENDIF
       IF(MW.EQ.0d0)THEN
          WRITE(*,*)"ERROR: MW=0 !"
          STOP
       ELSEIF(MW.GT.0d0)THEN
          i_mass=i_mass+1
          mass(i_mass)=MW
          prefac1L(i_mass)=-1.5d0*prefactor
          prefac2LQCD(i_mass)=0d0
          prefac2LQED(i_mass)=0d0
       ENDIF
       init=1
    endif

    if(abs(iorder).GT.3)then
       write(*,*)"ERROR: do not know iorder=",iorder
       stop
    endif

    amp1L(1:5)=dcmplx(0d0,0d0)
    amp2L(1:5)=dcmplx(0d0,0d0)

    ! massless fermion loops
    IF(i_massless.GT.0)then
       DO i=1,5
          amp1L(i)=OneLoop_HelAmp_Massless(i,shat,that,uhat)
          amp1L(i)=amp1L(i)*prefac01L
       ENDDO
       if(abs(iorder).GT.0)then
          DO i=1,5
             amp2L(i)=TwoLoop_HelAmp_Massless(i,shat,that,uhat)
             if(abs(iorder).eq.1)then
                ! only QED
                amp2L(i)=amp2L(i)*prefac02LQED
             elseif(abs(iorder).eq.2)then
                ! only QCD
                amp2L(i)=amp2L(i)*prefac02LQCD
             else
                ! QED+QCD
                amp2L(i)=amp2L(i)*(prefac02LQCD+prefac02LQED)
             endif
          ENDDO
       endif
    ENDIF

    ! massive fermion and massive W boson loops
    DO i=1,i_mass
       MQ=mass(i)
       MQ2=MQ**2
       xs=shat/MQ2
       xt=that/MQ2
       xu=uhat/MQ2
       if(iloopbasis.eq.0)then
          ! use OneLOop (faster)
          call loopbasis(MQ,shat,that,MUR,loopba1L)
          logm2omu2=DLOG(MQ2/MUR**2)
          call UToneloopbasis_scalar(xs,xt,logm2omu2,loopba1L,loopba1L2)
       else
          ! use G's
          call UToneloopbasis_G(xs,xt,loopba1L2)
       endif
       if(i.eq.i_mass.and.MW.GT.0d0)then
          ! it must be the W boson
          DO j=1,5
             amptmp=OneLoop_HelAmp_MassiveW(j,xs,xt,xu,loopba1L2)
             amp1L(j)=amp1L(j)+amptmp*prefac1L(i)
          ENDDO
       else
          ! it is the massive fermion
          DO j=1,5
             amptmp=OneLoop_HelAmp_Massive(j,xs,xt,xu,loopba1L2)
             amp1L(j)=amp1L(j)+amptmp*prefac1L(i)
          ENDDO
       endif
       if(abs(iorder).GT.0)then
          if((i.lt.i_mass.and.MW.GT.0d0).or.MW.LT.0d0)then
             ! we do not have NLO for W boson
             ! first multiply sqrts in the one-loop integrals that appeared
             ! at one-loop
             CALL UToneloopbasis_nosqrt(xs,xt,loopba1L2,loopba1L)
             ! get two-loop integrals
             CALL UTtwoloopbasis(xs,xt,loopba1L,loopba2L)
             ! get the two-loop helicity amplitudes
             CALL Get_TwoLoop_HelAmp_Massive(xs,xt,xu,loopba2L&
                  ,ampstmp)
             if(abs(iorder).eq.2.and.prefac2LQCD(i).NE.0d0)then
                ! only QCD
                DO j=1,5
                   amptmp=ampstmp(j)
                   amp2L(j)=amp2L(j)+amptmp*prefac2LQCD(i)
                ENDDO
             elseif(abs(iorder).eq.1)then
                ! only QED
                DO j=1,5
                   amptmp=ampstmp(j)
                   amp2L(j)=amp2L(j)+amptmp*prefac2LQED(i)
                ENDDO
             else
                ! QED+QCD
                DO j=1,5
                   amptmp=ampstmp(j)
                   amp2L(j)=amp2L(j)+amptmp*(prefac2LQCD(i)&
                        +prefac2LQED(i))
                ENDDO
             endif
          endif
       endif
    ENDDO

    ! helicity-summed amplitude square
    DO i=1,5
       camp1L(i)=DCONJG(amp1L(i))
       camp2L(i)=DCONJG(amp2L(i))
    enddo
    if(iorder.GE.0)then
       virt_wgts=amp1L(1)*camp1L(1)*2d0+amp1L(2)*camp1L(2)*8d0&
            +amp1L(3)*camp1L(3)*2d0+amp1L(4)*camp1L(4)*2d0&
            +amp1L(5)*camp1L(5)*2d0
    endif
    if(iorder.GT.0)then
       ! NLO in amplitude square
       virt_wgts=virt_wgts+2d0*DREAL(amp2L(1)*camp1L(1)*2d0&
            +amp2L(2)*camp1L(2)*8d0+amp2L(3)*camp1L(3)*2d0&
            +amp2L(4)*camp1L(4)*2d0+amp2L(5)*camp1L(5)*2d0)
    elseif(iorder.LT.0)then
       ! NLO in amplitude
       DO i=1,5
          amp2L(i)=amp1L(i)+amp2L(i)
          camp2L(i)=camp1L(i)+camp2L(i)
       ENDDO
       virt_wgts=amp2L(1)*camp2L(1)*2d0+amp2L(2)*camp2L(2)*8d0&
            +amp2L(3)*camp2L(3)*2d0+amp2L(4)*camp2L(4)*2d0&
            +amp2L(5)*camp2L(5)*2d0
    endif
    virt_wgts=virt_wgts/DBLE(IDEN)
    return
  end subroutine stwoloopmatrix

  subroutine eval_twoloop_massive_amplitudes(xs,xt,amps,force)
    use LbL_Global
    use UToneloopbasis
    use interpolation
    implicit none
    real(kind(1d0)),intent(in)::xs,xt
    real(kind(1d0))::xu
    complex(kind(1d0)),dimension(5),intent(out)::amps
    complex(kind(1d0))::amptmp
    real(kind(1d0)),dimension(10)::ramps
    ! 0: check whether there is a grid. If yes, just use the grid. Otherwise, generate a grid first and write to the disk and then use it.
    ! 1: force to generate a grid and write to the disk and then use it
    ! 2: just evaluate it directly without using the grid
    integer,optional,intent(in)::force
    integer,save::init=0
    integer::force_used
    INTEGER::NXA,NYA
    SAVE NXA,NYA
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::XA,YA
    REAL(KIND(1d0)),DIMENSION(:,:,:),ALLOCATABLE::ZA
    SAVE XA,YA,ZA
    ! NXSEG1 for xs from 1 to 10**(2) [200 total]
    INTEGER,PARAMETER::NXSEG1=100
    ! NXSEG2 for xs from 10**(n) to 10**(n+1) except n=0,1
    INTEGER,PARAMETER::NXSEG2=20
    INTEGER::NXMIN=-2,NXMAX=8
    SAVE NXMIN,NXMAX
    ! for xt, we take xt=-xs/2*y with 0<y<=1
    ! we take 40 points from 10**(-n-1) to 10**(-n) with n>=1
    INTEGER,PARAMETER::NYSEG1=40
    ! and take 50 points from 0.1 to 1
    INTEGER,PARAMETER::NYSEG2=50
    INTEGER::NYMIN=-4
    SAVE NYMIN
    INTEGER,PARAMETER::n_interp=6
    REAL(KIND(1d0)),DIMENSION(1)::XI,YI,ZI
    REAL(KIND(1d0)),DIMENSION(n_interp)::XA2,YA2
    REAL(KIND(1d0)),DIMENSION(n_interp,n_interp)::ZA2
    LOGICAL::lexist
    INTEGER,PARAMETER::iunit=30333
    INTEGER::i,j,k,l,nn
    INTEGER::ilog10xs,ilog10y
    INTEGER::iprint=0
    SAVE iprint
    INTEGER,PARAMETER::NDIM_1L=9,NDIM_2L=84
    REAL(KIND(1d0))::logm2omu2
    REAL(KIND(1E0_16))::logm2omu2_QP
    COMPLEX(KIND(1d0)),DIMENSION(NDIM_1L)::loopba1L,loopba1L2
    COMPLEX(KIND(1d0)),DIMENSION(NDIM_2L)::loopba2L
    COMPLEX(KIND(1E0_16)),DIMENSION(NDIM_1L)::loopba1L_qp,loopba1L2_qp
    COMPLEX(KIND(1E0_16)),DIMENSION(NDIM_2L)::loopba2L_qp
    REAL(KIND(1d0))::xxs,xxt,xxu,yy,shat,that,uhat,MQ2
    REAL(KIND(1E0_16))::xxs_qp,xxt_qp,xxu_qp,yy_qp,shat_qp,that_qp,uhat_qp,MQ2_qp
    REAL(KIND(1d0)),PARAMETER::MQ=1d0,MUR=1d0
    REAL(KIND(1E0_16)),PARAMETER::MQ_qp=1E0_16,MUR_qp=1E0_16
    COMPLEX(KIND(1d0)),DIMENSION(5)::ampstmp1,ampstmp2
    COMPLEX(KIND(1E0_16)),DIMENSION(5)::ampstmp1_qp,ampstmp2_qp
    COMPLEX(KIND(1d0)),DIMENSION(5)::LOampstmp1,LOampstmp2
    ! two-loop high energy expansion for fermions
    COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmp_HE
    ! two-loop low energy expansion for fermions
    COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmp_LE
    !COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmp_Massless
    !COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmp_Massive
    !COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmp_Massless
    ! whether to rescale the two-loop massless limit with the one-loop amplitudes
    ! if yes, it will rescale with one-loop(massive)/one-loop(massless)
    !LOGICAL,PARAMETER::rescale_massless=.TRUE.
    integer,dimension(5)::iamp
    real(kind(1d0)),DIMENSION(:),ALLOCATABLE::Hel5Amp_Last_xs
    complex(kind(1d0)),DIMENSION(:),ALLOCATABLE::Hel5Amp_Last
    double precision logxsHE5,xxs_HE,xxt_HE,xxu_HE,xx1,xx2
    common /logxs_HE/ logxsHE5
    complex(kind(1d0))::yy1,yy2
    if(present(force))then
       force_used=force
    else
       force_used=0
    endif
    if(force_used.LT.0.or.force_used.GT.2)then
       write(*,*)"ERROR: do not know force = ", force_used
       stop
    endif
    xu=-xs-xt
    if(xs.LE.0d0)then
       write(*,*)"ERROR: xs <= 0"
       stop
    endif
    if(xt.GE.0d0.or.xu.GE.0d0)then
       write(*,*)"ERROR: xt >= 0 or xu >= 0"
       stop
    endif
    if(force_used.eq.0.and.init.eq.0)then
       ! let us first check whether the grid is already present
       INQUIRE(FILE=TRIM(grid_dir)//"Amp2L.grid",EXIST=lexist)
       IF(.NOT.lexist)THEN
          WRITE(*,*)"INFO: the file Amp2L.grid does not exist !"
          WRITE(*,*)"INFO: will force to regenerate the grid first"
          force_used=1
       ENDIF
    endif
    if(force_used.eq.1)init=0
    if(force_used.lt.2.and.init.eq.0)then
       ! we probably need to update the grid
       if(force_used.eq.1)then
          NXA=(NXMAX-NXMIN-2)*NXSEG2+2*NXSEG1+1
          NYA=(-1-NYMIN)*NYSEG1+NYSEG2+1
          IF(ALLOCATED(XA))THEN
             IF(SIZE(XA).NE.NXA)THEN
                DEALLOCATE(XA)
                ALLOCATE(XA(NXA))
             ENDIF
          ELSE
             ALLOCATE(XA(NXA))
          ENDIF
          ! log10(xs)
          DO i=1,(-NXMIN)*NXSEG2
             XA(i)=(i-1)/DBLE(NXSEG2)+NXMIN
          ENDDO
          DO i=(-NXMIN)*NXSEG2+1,(-NXMIN)*NXSEG2+2*NXSEG1
             XA(i)=(i-(-NXMIN)*NXSEG2-1)/DBLE(NXSEG1)
          ENDDO
          DO i=(-NXMIN)*NXSEG2+2*NXSEG1+1,NXA
             XA(i)=(i-((-NXMIN)*NXSEG2+2*NXSEG1+1))/DBLE(NXSEG2)+2d0
          ENDDO
          IF(ALLOCATED(YA))THEN
             IF(SIZE(YA).NE.NYA)THEN
                DEALLOCATE(YA)
                ALLOCATE(YA(NYA))
             ENDIF
          ELSE
             ALLOCATE(YA(NYA))
          ENDIF
          IF(ALLOCATED(Hel5Amp_Last))THEN
             IF(SIZE(Hel5Amp_Last).NE.NYA)THEN
                DEALLOCATE(Hel5Amp_Last)
                ALLOCATE(Hel5Amp_Last(NYA))
             ENDIF
          ELSE
             ALLOCATE(Hel5Amp_Last(NYA))
          ENDIF
          IF(ALLOCATED(Hel5Amp_Last_xs))THEN
             IF(SIZE(Hel5Amp_Last_xs).NE.NYA)THEN
                DEALLOCATE(Hel5Amp_Last_xs)
                ALLOCATE(Hel5Amp_Last_xs(NYA))
             ENDIF
          ELSE
             ALLOCATE(Hel5Amp_Last_xs(NYA))
          ENDIF
          ! log10(y)
          DO i=1,(-1-NYMIN)*NYSEG1
             YA(i)=(i-1)/DBLE(NYSEG1)+NYMIN
          ENDDO
          DO i=(-1-NYMIN)*NYSEG1+1,NYA
             YA(i)=(i-(-1-NYMIN)*NYSEG1-1)/DBLE(NYSEG2)-1d0
          ENDDO
          IF(ALLOCATED(ZA))THEN
             IF(SIZE(ZA).NE.NXA.OR.SIZE(ZA,2).NE.NYA)THEN
                DEALLOCATE(ZA)
                ALLOCATE(ZA(NXA,NYA,10))
             ENDIF
          ELSE
             ALLOCATE(ZA(NXA,NYA,10))
          ENDIF
          MQ2=MQ**2
          MQ2_qp=MQ_qp**2
          logm2omu2=DLOG(MQ2/MUR**2)
          logm2omu2_QP=LOG(MQ2_qp/MUR_qp**2)
          DO i=1,NXA
             IF(i.eq.1)then
                CALL progress_LbL(INT(I*50d0/NXA),50,.TRUE.)
             else
                CALL progress_LbL(INT(I*50d0/NXA),50)
             endif
             xxs=10d0**(XA(i))
             xxs_qp=10E0_16**(XA(i))
             shat=xxs*MQ2
             shat_qp=xxs_qp*MQ2_qp
             DO j=1,NYA
                yy=10d0**(YA(j))
                xxt=-xxs/2d0*yy
                xxu=-xxs-xxt
                that=xxt*MQ2
                uhat=xxu*MQ2
                yy_qp=10E0_16**(YA(j))
                xxt_qp=-xxs_qp/2E0_16*yy_qp
                xxu_qp=-xxs_qp-xxt_qp
                that_qp=xxt_qp*MQ2_qp
                uhat_qp=xxu_qp*MQ2_qp
                ! decide when we use the HE expansion
                ! if iamp(k)=-1, it will use the LE expansion
                ! if iamp(k)=0, it will use the HE expansion
                ! if iamp(k)=1, it simply use the massive amplitude (double precision)
                ! if iamp(k)=2, it will use the massive amplitude (quadruple precision)
                ! if iamp(k)=3, it will use the linear interpolation in log10xs
                !                                       (assuming only k=5 is possible)
                do k=1,5
                   iamp(k)=massive_2LAmp_xs_HE(DLOG(xxs),DLOG(yy),k)
                enddo
                if(xxs.LE.4d0.and.xxs.GT.2d0)THEN
                   if(use_2L_DP)then
                      iamp(1:5)=1
                   elseif(use_2L_QP)then
                      iamp(1:5)=2
                   else
                      iamp(1:5)=-1
                   endif
                elseif(xxs.LE.2d0.and.xxs.GT.1d0)THEN
                   if(use_2L_QP)then
                      iamp(1:5)=2
                   elseif(use_2L_DP)then
                      iamp(1:5)=1
                   else
                      iamp(1:5)=-1
                   endif
                elseif(xxs.LE.1d0)then
                   iamp(1:5)=-1
                endif
                ! check whether only k=5 can have iamp(k)=3
                do k=1,4
                   if(iamp(k).eq.3)then
                      write(*,*)"ERROR: only iamp(5) is allowed to be 3"
                      stop
                   endif
                enddo
                if(iamp(1).eq.2.or.iamp(2).eq.2.or.iamp(3).eq.2.or.&
                     iamp(4).eq.2.or.iamp(5).eq.2)then
                   ! use OneLOop (faster)
                   call loopbasis_qp(MQ_qp,shat_qp,that_qp,MUR_qp,loopba1L_qp)
                   call UToneloopbasis_scalar_qp(xxs_qp,xxt_qp,logm2omu2_qp,loopba1L_qp,loopba1L2_qp)
                   ! first multiply sqrts in the one-loop integrals that appeared
                   ! at one-loop
                   CALL UToneloopbasis_nosqrt_qp(xxs_qp,xxt_qp,loopba1L2_qp,loopba1L_qp)
                   ! get two-loop integrals
                   CALL UTtwoloopbasis_qp(xxs_qp,xxt_qp,loopba1L_qp,loopba2L_qp)
                   ! get the two-loop helicity amplitudes
                   CALL Get_TwoLoop_HelAmp_Massive_qp(xxs_qp,xxt_qp,xxu_qp,loopba2L_qp&
                        ,ampstmp1_qp)
                   ampstmp1(1:5)=ampstmp1_qp(1:5)
                elseif(iamp(1).eq.1.or.iamp(2).eq.1.or.iamp(3).eq.1.or.&
                     iamp(4).eq.1.or.iamp(5).eq.1)then
                   ! use OneLOop (faster)
                   call loopbasis(MQ,shat,that,MUR,loopba1L)
                   call UToneloopbasis_scalar(xxs,xxt,logm2omu2,loopba1L,loopba1L2)
                   ! first multiply sqrts in the one-loop integrals that appeared
                   ! at one-loop
                   CALL UToneloopbasis_nosqrt(xxs,xxt,loopba1L2,loopba1L)
                   ! get two-loop integrals
                   CALL UTtwoloopbasis(xxs,xxt,loopba1L,loopba2L)
                   ! get the two-loop helicity amplitudes
                   CALL Get_TwoLoop_HelAmp_Massive(xxs,xxt,xxu,loopba2L&
                        ,ampstmp1)
                else
                   ampstmp1(1:5)=dcmplx(0d0,0d0)
                endif
                do k=1,5
                   if(iamp(k).eq.0)then
                      ampstmp2(k)=TwoLoop_HelAmp_HE(k,-1,xxs,xxt,xxu)
                      !LOampstmp1(k)=OneLoop_HelAmp_Massive(k,xxs,xxt,xxu,loopba1L2)
                      !LOampstmp2(k)=OneLoop_HelAmp_Massless(k,xxs,xxt,xxu)
                      !ampstmp2(k)=TwoLoop_HelAmp_Massless(k,xxs,xxt,xxu)
                      !ampstmp2(k)=-8d0*ampstmp2(k)
                      !IF(rescale_massless.and.abs(LOampstmp2(k)).NE.0d0.and.&
                      !     .not.(ISNAN(dreal(LOampstmp2(k))).or.&
                      !     ISNAN(dimag(LOampstmp2(k))).or.&
                      !     ISNAN(dreal(LOampstmp1(k))).or.&
                      !     ISNAN(dimag(LOampstmp1(k)))))then
                      !   ampstmp2(k)=ampstmp2(k)*LOampstmp1(k)/LOampstmp2(k)
                      !endif
                   elseif(iamp(k).eq.-1)then
                      ampstmp2(k)=TwoLoop_HelAmp_LE(k,-1,xxs,xxt,xxu)
                   elseif(iamp(k).eq.3)then
                      if(k.ne.5)then
                         write(*,*)"ERROR: cannot reach here"
                         stop
                      endif
                      ! use the linear interpolation
                      xxs_HE=DEXP(logxsHE5)
                      xxt_HE=-xxs_HE/2d0*yy
                      xxu_HE=-xxs_HE-xxt_HE
                      yy2=TwoLoop_HelAmp_HE(k,-1,xxs_HE,xxt_HE,xxu_HE)
                      xx1=DLOG10(Hel5Amp_Last_xs(j))
                      xx2=DLOG10(xxs_HE)
                      yy1=Hel5Amp_Last(j)
                      ampstmp2(k)=(yy2-yy1)/(xx2-xx1)*XA(i)+&
                           (yy1*xx2-xx1*yy2)/(xx2-xx1)
                   else
                      ampstmp2(k)=dcmplx(0d0,0d0)
                   endif
                enddo
                DO k=1,5
                   if(iamp(k).eq.1.or.iamp(k).eq.2)then
                      ZA(i,j,2*k-1)=DREAL(ampstmp1(k))
                      if(xxs.GT.4d0)then
                         ZA(i,j,2*k)=DIMAG(ampstmp1(k))
                      else
                         ZA(i,j,2*k)=0d0
                      endif
                   else
                      ZA(i,j,2*k-1)=DREAL(ampstmp2(k))
                      if(xxs.GT.4d0)then
                         ZA(i,j,2*k)=DIMAG(ampstmp2(k))
                      else
                         ZA(i,j,2*k)=0d0
                      endif
                   endif
                ENDDO
                ! this is for the linear interpolation when
                ! iamp(5)=3
                if(iamp(5).eq.1.or.iamp(5).eq.2)then
                   Hel5Amp_Last_xs(j)=xxs
                   Hel5Amp_Last(j)=dcmplx(ZA(i,j,9),ZA(i,j,10))
                endif
             ENDDO
          ENDDO
          ! we decide to update the grid
          INQUIRE(FILE=TRIM(grid_dir)//"Amp2L.grid",EXIST=lexist)
          IF(.NOT.lexist)THEN
             OPEN(UNIT=iunit,FILE=TRIM(grid_dir)//"Amp2L.grid",STATUS="NEW")
          ELSE
             OPEN(UNIT=iunit,FILE=TRIM(grid_dir)//"Amp2L.grid",STATUS="REPLACE")
          ENDIF
          WRITE(iunit,*)NXA,NYA
          DO i=1,NXA
             WRITE(iunit,*)XA(i)
          ENDDO
          DO i=1,NYA
             WRITE(iunit,*)YA(i)
          ENDDO
          DO i=1,NXA
             DO j=1,NYA
                WRITE(iunit,*)ZA(i,j,1:10)
             ENDDO
          ENDDO
          CLOSE(UNIT=iunit)
       endif
       ! now, we make sure Amp2L.grid has been generated
       ! just read the grid
       OPEN(UNIT=iunit,FILE=TRIM(grid_dir)//"Amp2L.grid")
       READ(iunit,*)NXA,NYA
       IF(ALLOCATED(XA))THEN
          IF(SIZE(XA).NE.NXA)THEN
             DEALLOCATE(XA)
             ALLOCATE(XA(NXA))
          ENDIF
       ELSE
          ALLOCATE(XA(NXA))
       ENDIF
       ! log10(xs)
       DO i=1,NXA
          READ(iunit,*)XA(i)
       ENDDO
       IF(ALLOCATED(YA))THEN
          IF(SIZE(YA).NE.NYA)THEN
             DEALLOCATE(YA)
             ALLOCATE(YA(NYA))
          ENDIF
       ELSE
          ALLOCATE(YA(NYA))
       ENDIF
       ! log10(y)
       DO i=1,NYA
          READ(iunit,*)YA(i)
       ENDDO
       IF(ALLOCATED(ZA))THEN
          IF(SIZE(ZA).NE.NXA.OR.SIZE(ZA,2).NE.NYA)THEN
             DEALLOCATE(ZA)
             ALLOCATE(ZA(NXA,NYA,10))
          ENDIF
       ELSE
          ALLOCATE(ZA(NXA,NYA,10))
       ENDIF
       DO i=1,NXA
          DO j=1,NYA
             READ(iunit,*)ZA(i,j,1:10)
          ENDDO
       ENDDO
       CLOSE(UNIT=iunit)
       init=1
    endif
    if(force_used.eq.2)then
       MQ2=MQ**2
       logm2omu2=DLOG(MQ2/MUR**2)
       shat=xs*MQ2
       that=xt*MQ2
       uhat=xu*MQ2
       ! directly compute the massive amplitude
       call loopbasis(MQ,shat,that,MUR,loopba1L)
       call UToneloopbasis_scalar(xs,xt,logm2omu2,loopba1L,loopba1L2)
       ! first multiply sqrts in the one-loop integrals that appeared
       ! at one-loop
       CALL UToneloopbasis_nosqrt(xs,xt,loopba1L2,loopba1L)
       ! get two-loop integrals
       CALL UTtwoloopbasis(xs,xt,loopba1L,loopba2L)
       ! get the two-loop helicity amplitudes
       CALL Get_TwoLoop_HelAmp_Massive(xs,xt,xu,loopba2L&
            ,amps)
    else
       if(xt.LT.xu)then
          xxt=xu
          xxu=xt
       else
          xxt=xt
          xxu=xu
       endif
       ! check the grid range
       if(xs.LT.10d0**(NXMIN).or.xs.GT.10d0**(NXMAX)&
            .or.(-2d0*xxt/xs).LT.10d0**(NYMIN))then
          iprint=iprint+1
          if(iprint.LE.5)then
             write(*,*)"WARNING: outside the range of the grid (xs,xt)=",xs,xt
             write(*,*)"WARNING: set the 2-loop amplitudes to zero"
             if(iprint.eq.5)then
                write(*,*)"INFO: will suppress the further warning"
             endif
          endif
          amps(1:5)=dcmplx(0d0,0d0)
          !do k=1,5
          !   amps(k)=TwoLoop_HelAmp_Massless(k,xs,xt,xu)
          !   amps(k)=-8d0*amps(k)
          !enddo
          return
       endif
       ! we do the interpolation from the grid
       XI(1)=DLOG10(xs)
       YI(1)=DLOG10(-2d0*xxt/xs)

       ilog10xs=FLOOR(XI(1)) ! ilog10xs has been make sure
                             ! NXMIN<=ilog10xs<=NXMAX
       IF(ilog10xs.LT.0)THEN
          k=(ilog10xs-NXMIN)*NXSEG2+1
          do i=k,k+NXSEG2
             if(XA(i).GE.XI(1))EXIT
          enddo
       ELSEIF(ilog10xs.EQ.0)THEN
          k=(-NXMIN)*NXSEG2+1
          do i=k,MIN(k+NXSEG1,NXA)
             if(XA(i).GE.XI(1))EXIT
          ENDDO
       ELSEIF(ilog10xs.EQ.1)THEN
          k=(-NXMIN)*NXSEG2+NXSEG1+1
          do i=k,MIN(k+NXSEG1,NXA)
             if(XA(i).GE.XI(1))EXIT
          ENDDO
       ELSE
          k=(ilog10xs-NXMIN-2)*NXSEG2+2*NXSEG1+1
          do i=k,MIN(k+NXSEG2,NXA)
             if(XA(i).GE.XI(1))EXIT
          ENDDO
       ENDIF
       IF(i-n_interp/2.GE.1.AND.i-n_interp/2-1+n_interp.LE.NXA)THEN
          k=i-n_interp/2-1
       ELSEIF(i-n_interp/2.LT.1)THEN
          k=0
       ELSEIF(i-n_interp/2-1+n_interp.GT.NXA)THEN
          k=NXA-n_interp
       ELSE
          WRITE(*,*)"Error: you cannot reach here #1 !"
          STOP
       ENDIF

       ilog10y=FLOOR(YI(1)) ! NYMIN<=ilog10y<=0
       if(ilog10y.LE.-1)then
          l=(ilog10y-NYMIN)*NYSEG1+1
       else
          l=NYA
       endif
       if(ilog10y.LT.-1)then
          do i=l,l+NYSEG1
             if(YA(i).GE.YI(1))EXIT
          enddo
       else
          do i=l,MIN(l+NYSEG2,NYA)
             if(YA(i).GE.YI(1))EXIT
          enddo
       endif
       IF(i-n_interp/2.GE.1.AND.i-n_interp/2-1+n_interp.LE.NYA)THEN
          l=i-n_interp/2-1
       ELSEIF(i-n_interp/2.LT.1)THEN
          l=0
       ELSEIF(i-n_interp/2-1+n_interp.GT.NYA)THEN
          l=NYA-n_interp
       ELSE
          WRITE(*,*)"Error: you cannot reach here #2 !"
          STOP
       ENDIF

       DO i=1,n_interp
          XA2(i)=XA(k+i)
          YA2(i)=YA(l+i)
       ENDDO

       DO nn=1,10
          DO i=1,n_interp
             DO j=1,n_interp
                ZA2(i,j)=ZA(k+i,l+j,nn)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d(n_interp-1,n_interp-1,XA2,YA2,ZA2,1,XI,YI,ZI)
          ramps(nn)=ZI(1)
       ENDDO
       DO nn=1,5
          amps(nn)=dcmplx(ramps(2*nn-1),ramps(2*nn))
       ENDDO
       if(xt.LT.xu)then
          ! swap the 4th and 5th amplitudes
          amptmp=amps(4)
          amps(4)=amps(5)
          amps(5)=amptmp
       endif
       return
    endif
    return
  end subroutine eval_twoloop_massive_amplitudes

  function massive_2LAmp_xs_HE(logxs,logy0,ihel)
    USE LbL_Global
    implicit none
    integer::massive_2LAmp_xs_HE
    real(kind(1d0)),intent(in)::logxs,logy0
    real(kind(1d0))::logy
    integer,intent(in)::ihel
    real(kind(1d0))::logxsHE,logxsDP,logxsQP
    double precision logxsHE5
    common /logxs_HE/ logxsHE5
    logy=logy0
    IF(logy.LE.-9.210340371976184d0)logy=-9.210340371976184d0
    select case(ihel)
    case(1)
       ! ++++
       logxsHE=3.8091168568054297d0-0.1815478927827205d0*logy+&
            0.18344056325979144d0*logy**2+0.013505207915210513d0*logy**3
       if(logy.GE.-4.961845129926823d0)then
          logxsDP=logxsHE
       else
          logxsDP=11.166864817597137d0+1.4967909500407761d0*logy+&
               0.04797951260207679d0*logy**2
       endif
       logxsQP=logxsHE
    CASE(2)
       ! -+++
       logxsHE=3.5685799292818454d0-0.5147127975001642d0*logy+&
            0.132904714406617d0*logy**2+0.010688230351607484d0*logy**3
       if(logy.GE.-2.175071772695086d0)then
          logxsDP=logxsHE
       else
          logxsDP=27.915888866385085d0+28.024426456486754d0*logy+& 
               12.911891925845469d0*logy**2+2.825200757717727d0*logy**3+&
               0.2875553031803774d0*logy**4+0.010980793086902637d0*logy**5
       endif
       logxsQP=logxsHE
    CASE(3)
       ! --++
       logxsHE=3.50655300057434d0-0.6961702708838362d0*logy+&
            0.05683163805811889d0*logy**2+0.004531550079671995d0*logy**3
       if(logy.GE.-2.5902671654458267d0)then
          logxsDP=logxsHE
       else
          logxsDP=15.219240765711538d0+16.564149068772423d0*logy+&
               9.462307925180454d0*logy**2+2.3332643650514235d0*logy**3+&
               0.2539240184668183d0*logy**4+0.010111527892924256d0*logy**5
       endif
       logxsQP=logxsHE
    CASE(4)
       ! +-+-
       logxsHE=3.6390120255034684d0-0.4678048115018555d0*logy+& 
            0.11539017438154622d0*logy**2+0.008511864820080747d0*logy**3
       if(logy.GE.-2.5257286443082556d0)then
          logxsDP=logxsHE
       else
          logxsDP=-23.52030780274741d0-22.66377415412352d0*logy-&
               6.0919720338628025d0*logy**2-0.6713025046842788d0*logy**3-&
               0.026747134119486496d0*logy**4
       endif
       logxsQP=logxsHE
    CASE(5)
       ! +--+
       logxsHE=3.3492335122469856d0-0.43973890555225204d0*logy+&
            0.11343768830122365d0*logy**2+0.007413431851109939d0*logy**3
       if(logy.GE.-1.2039728043259361d0)then
          logxsDP=logxsHE
       else
          logxsDP=4.272942407116875d0+0.4514903614762066d0*logy+&
               0.04000828109192846d0*logy**2+0.005112322539972597d0*logy**3
       endif
       if(logy.GE.-2.5902671654458267d0)then
          logxsQP=logxsHE
       else
          logxsQP=17.29265639754289d0+7.405565256934902d0*logy+&
               1.219494244596741d0*logy**2+0.06907286764828978d0*logy**3
       endif
    case default
       write(*,*)"ERROR: do not know the helicity configuration = ",ihel
       stop
    END select
    IF(logxs.GE.logxsHE)THEN
       ! just use the high-energy exp two-loop amps
       massive_2LAmp_xs_HE=0
    elseif(use_2L_DP.and.logxs.LE.logxsDP)THEN
       ! use the massive two-loop amps (double precision)
       massive_2LAmp_xs_HE=1
    elseif(use_2L_QP.and.logxs.LE.logxsQP)THEN
       ! use the massive two-loop amps (quadruple precision)
       massive_2LAmp_xs_HE=2
    else
       ! use the linear interpolation
       massive_2LAmp_xs_HE=3
       logxsHE5=logxsHE
    ENDIF
    return
  end function massive_2LAmp_xs_HE

  function massive_2LAmp_xs_limit(log10xs,y,ihel)
    USE LbL_Global
    implicit none
    integer::massive_2LAmp_xs_limit
    real(kind(1d0)),intent(in)::log10xs,y
    integer,intent(in)::ihel
    real(kind(1d0))::log10xsmax,y0
    select case(ihel)
    case(1)
       log10xsmax=5d0
    case(2)
       if(y.gt.0.13d0)then
          log10xsmax=5d0
       else
          if(y.lt.5d-3)then
             y0=5d-3
          else
             y0=y
          endif
          log10xsmax=1.7787354164416862d0+0.6466982079159993d0*DLog(1d0/y0)&
               -0.12957462263084676d0*DLog(1d0/y0)**2
       endif
    case(3)
       if(y.lt.1d-3)then
          log10xsmax=1.96d0
       else
          if(y.gt.0.08d0)then
             log10xsmax=4.5d0
          else
             if(y.lt.5d-3)then
                y0=5d-3
             else
                y0=y
             endif
             log10xsmax=2.1768230517805724D0+0.2601015056723903D0*DLog(1D0/y0)&
                  -0.039789361035437294D0*DLog(1D0/y0)**2
          endif
       endif
    case(4)
       if(y.lt.1d-3)then
          log10xsmax=1.96d0
       else
          if(y.gt.0.07d0)then
             log10xsmax=4.4d0
          else
             if(y.lt.5d-3)then
                y0=5d-3
             else
                y0=y
             endif
             log10xsmax=2.1768230517805724d0+0.2601015056723903d0*DLog(1d0/y0)&
                  -0.039789361035437294d0*DLog(1d0/y0)**2
          endif
       endif
    case(5)
       if(y.lt.1d-3)then
          y0=1d-3
       else
          y0=y
       endif
       !log10xsmax=3.0063483105705364d0-5.446494968533712d0*y0&
       !     +16.525911973917086d0*y0**2-25.190296330086866d0*y0**3&
       !     +14.103931460161265d0*y0**4+0.45998052242753623d0*DLog(y0)&
       !     +0.011961644553083368d0*DLog(y0)**2
       log10xsmax=2.7911842384268715d0+4.207693185201302d0*y0&
            -9.76591637974952d0*y0**2+12.554910264370395d0*y0**3&
            -5.809492514996749d0*y0**4+0.6245937869762597d0*DLog(y0)&
            +0.0531023802880679d0*DLog(y0)**2
    case default
       write(*,*)"ERROR: do not know the helicity configuration = ",ihel
       stop
    end select
    !log10xsmax=5d0
    if(log10xs.LE.log10xsmax)then
       if(use_2L_DP)then
          ! use the massive two-loop amps (double precision)
          massive_2LAmp_xs_limit=1
       elseif(use_2L_QP)then
          ! use the massive two-loop amps (quadruple precision)
          massive_2LAmp_xs_limit=2
       else
          ! just use the massless two-loop amps
          massive_2LAmp_xs_limit=0
       endif
    else
       ! just use the massless two-loop amps
       massive_2LAmp_xs_limit=0
    endif
    return
  end function massive_2LAmp_xs_limit

  function massive_2LAmp_xs_LElimit(log10xs,y0,ihel)
    implicit none
    integer::massive_2LAmp_xs_LElimit
    real(kind(1d0)),intent(in)::log10xs,y0
    integer,intent(in)::ihel
    real(kind(1d0))::log10xsmax,y
    if(y0.GT.1d0)then
       y=2d0-y0
    else
       y=y0
    endif
    select case(ihel)
    case(1)
       if(y.LT.8d-3)then
          log10xsmax=-0.2d0
       elseif(y.LT.5d-2)then
          log10xsmax=-1.5d0
       elseif(y.LT.2d-1)then
          log10xsmax=-1.8d0
       else
          log10xsmax=-2d0
       endif
    case(2)
       if(y.LT.8d-3)then
          log10xsmax=-0.1d0
       elseif(y.LT.3.35d-2)then
          log10xsmax=-0.8d0
       elseif(y.LT.5d-2)then
          log10xsmax=-1d0
       else
          log10xsmax=-1.3d0
       endif
    case(3)
       if(y.LT.8d-3)then
          log10xsmax=-0.2d0
       elseif(y.LT.2d-1)then
          log10xsmax=-1.2d0
       else
          log10xsmax=-1.3d0
       endif
    case(4)
       if(y0.GT.1d0)then
          if(y.LT.8d-3)then
             log10xsmax=-0.1d0
          elseif(y.LT.1.1d-2)then
             log10xsmax=-0.7d0
          elseif(y.LT.3.35d-2)then
             log10xsmax=-1.2d0
          elseif(y.LT.0.3d0)then
             log10xsmax=-1.3d0
          else
             log10xsmax=-1.4d0
          endif
       else
          if(y.LT.8d-3)then
             log10xsmax=-0.3d0
          elseif(y.LT.1.136d-1)then
             log10xsmax=-1.3d0
          elseif(y.LT.0.3d0)then
             log10xsmax=-1.5d0
          elseif(y.LT.0.556d0)then
             log10xsmax=-1.6d0
          else
             log10xsmax=-1.9d0
          endif
       endif
    case(5)
       if(y0.GT.1d0)then
          if(y.LT.8d-3)then
             log10xsmax=-0.3d0
          elseif(y.LT.1.136d-1)then
             log10xsmax=-1.3d0
          elseif(y.LT.0.3d0)then
             log10xsmax=-1.5d0
          elseif(y.LT.0.556d0)then
             log10xsmax=-1.6d0
          else
             log10xsmax=-1.9d0
          endif
       else
          if(y.LT.8d-3)then
             log10xsmax=-0.1d0
          elseif(y.LT.1.1d-2)then
             log10xsmax=-0.7d0
          elseif(y.LT.3.35d-2)then
             log10xsmax=-1.2d0
          elseif(y.LT.0.3d0)then
             log10xsmax=-1.3d0
          else
             log10xsmax=-1.4d0
          endif
       endif
    case default
       write(*,*)"ERROR: do not know the helicity configuration = ",ihel
       stop
    end select
    if(log10xs.LE.log10xsmax)then
       ! use the low-energy limit                                                                               
       massive_2LAmp_xs_LElimit=0
    else
       ! just use the massive two-loop amps
       massive_2LAmp_xs_LElimit=0
    endif
    return
  end function massive_2LAmp_xs_LElimit

  subroutine progress_LbL(j,nmax,forceinit)
    implicit none
    integer,intent(in)::j,nmax
    logical,intent(in),optional::forceinit
    integer::k
    character(:), allocatable :: bar, bar0
    character(5)::nmax_str
    !character(len=)::bar="???% |
    integer::init=0
    save init,bar,bar0,nmax_str
    IF(present(forceinit))THEN
       IF(forceinit)THEN
          init=0
          IF(ALLOCATED(bar))THEN
             DEALLOCATE(bar)
          ENDIF
          IF(ALLOCATED(bar0))THEN
             DEALLOCATE(bar0)
          ENDIF
       ENDIF
    ENDIF
    IF(init.EQ.0)THEN
       allocate(character(nmax+7) :: bar)
       allocate(character(nmax+7) :: bar0)
       bar(1:6)="???% |"
       do k=1,nmax
          bar(6+k:6+k)=" "
       enddo
       bar(nmax+7:nmax+7)="|"
       bar0=bar
       !bar="???% |"//repeat(' ',nmax)//"|"
       write(unit=nmax_str,fmt="(i5)") nmax+7
       nmax_str=adjustl(nmax_str)
       init=1
    ENDIF
    bar=bar0
    write(unit=bar(1:3),fmt="(i3)") INT(100*DBLE(j)/DBLE(nmax))
    do k=1, j
       bar(6+k:6+k)="*"
    enddo
    ! print the progress bar.
    write(unit=6,fmt="(a1,a"//trim(nmax_str)//")",advance="no") char(13), bar
    if (j.NE.nmax) then
       flush(unit=6)
    else
       write(unit=6,fmt=*)
    endif
    return
  end subroutine progress_LbL
end module NLOME
