MODULE phase_space_integrators
  USE LbL_Global
  USE plot_LbL
  USE Constants
  USE LbL_setscale
  IMPLICIT NONE
  INTEGER::nprint
  INTEGER::varnum,nunwei
  LOGICAL::lunwei2
  INTEGER::NPRUP=0,lwmax
  INTEGER,PARAMETER::maxprint=8
  REAL(KIND(1d0))::weight_Romberg
  SAVE
CONTAINS
  SUBROUTINE calc_LbL
    USE qcd_constants
    USE qcd_setup
    USE qcd_coupling
    USE coupling_global
    USE GPL_wrapper
    USE ElasticPhotonPhotonFlux
    IMPLICIT NONE
    INTEGER::i,i1,j1,k1,s1x
    INTEGER::nh,innum,outnum
    INTEGER::ioerror,icase=0
    LOGICAL::lunwei=.FALSE.,lhewgt=.FALSE.
    REAL(KIND(1d0)),DIMENSION(3)::rslt
    INTEGER::itmxn,ncalln
    LOGICAL::lexist
    INTEGER::lfound
    SAVE lunwei,lhewgt,icase
    REAL(KIND(1d0))::temp
    CHARACTER(len=7)::A1name,A2name
    REAL(KIND(1d0))::Aval,Zval

    include 'banner.inc'
    CALL ReadElem_integer('colpar',colpar)
    if(colpar.LT.1.OR.colpar.GT.4)then
       WRITE(*,*)"ERROR: colpar = ",colpar
       STOP
    ENDIF
    IF(colpar.EQ.1)THEN
       ! heavy ion is only possible when colpar=1
       CALL ReadElem_integer('UPC_photon_flux',UPC_photon_flux)
       IF(UPC_photon_flux.LE.0.OR.UPC_photon_flux.GT.3)THEN
          WRITE(*,*)"ERROR: UPC_Photon_flux =",UPC_photon_flux
          STOP
       ENDIF
       CALL ReadElem_logic('intrinsic_kt',intrinsic_kt)
       IF(intrinsic_kt)then
          WRITE(*,*)"INFO: Will include intrinsic kT due to the virtuality of photon"
       ENDIF

       CALL ReadElem_integer('nuclearA_beam1',nuclearA1)
       CALL ReadElem_integer('nuclearA_beam2',nuclearA2)
       CALL ReadElem_integer('nuclearZ_beam1',nuclearZ1)
       CALL ReadElem_integer('nuclearZ_beam2',nuclearZ2)
       if(nuclearA1.EQ.1.and.nuclearZ1.EQ.1)THEN
          ! for proton
          nuclearA1=0
          nuclearZ1=0
       endif
       if(nuclearA2.EQ.1.and.nuclearZ2.EQ.1)THEN
          ! for proton
          nuclearA2=0
          nuclearZ2=0
       endif

       IF(UPC_photon_flux.EQ.3.AND.(nuclearA1.GT.1.OR.&
            nuclearA2.GT.1))THEN
          WRITE(*,*)"ERROR: UPC_photon_flux=3 (iww) only applies to proton-proton case"
          STOP
       ENDIF
       IF(.NOT.(nuclearA1.EQ.0.and.nuclearZ1.EQ.0))THEN
          A1name=GetASymbol(nuclearA1,nuclearZ1)
          CALL GetNuclearInfo(A1name,Aval,Zval,LbL_RA(1),LbL_aA(1),LbL_wA(1))
       ENDIF
       IF(.NOT.(nuclearA2.EQ.0.and.nuclearZ2.EQ.0))THEN
          A2name=GetASymbol(nuclearA2,nuclearZ2)
          CALL GetNuclearInfo(A2name,Aval,Zval,LbL_RA(2),LbL_aA(2),LbL_wA(2))
       ENDIF
    ENDIF

    CALL ReadElem_real('energy_beam1',energy_beam1)
    CALL ReadElem_real('energy_beam2',energy_beam2)
    IF(energy_beam1.LE.0d0.OR.energy_beam2.LE.0d0)THEN
       WRITE(*,*)"ERROR: the energies of the beams cannot be non positive"
       STOP
    ENDIF

    CALL ReadElem_integer('order',order)
    IF(order.LT.-3.OR.order.GT.3)then
       WRITE(*,*)"ERROR: order must be between -3 to 3. order=",order
       STOP
    ENDIF

    lunwei=.FALSE.
    lhewgt=.FALSE.
    CALL ReadElem_logic('topdrawer_output',topdrawer_output)
    CALL ReadElem_logic('gnuplot_output',gnuplot_output)
    CALL ReadElem_logic('root_output',root_output)
    CALL ReadElem_logic('hwu_output',hwu_output)
    plot_output=topdrawer_output.OR.gnuplot_output.OR.root_output.OR.hwu_output

    CALL ReadElem_integer('gener',gener)
    IF(gener.NE.0.AND.gener.NE.1)THEN
       WRITE(*,*)"ERROR:unknown the integrator gener=",gener
       STOP
    ENDIF
    CALL ReadElem_logic('unwgt',unwgt)
    lunwei=unwgt
    lunwei2=lunwei
    if(lunwei.and.gener.ne.0)then
       WRITE(*,*)"ERROR: only gener=0 (VEGAS) is allowed for generating the LHE file"
       STOP
    endif
    CALL ReadElem_logic('lhewgtup',lhewgtup)
    lhewgt=lhewgtup
    IF(lunwei.AND.lhewgt)icase=1

    CALL ReadConst
    IF(alphasMZ.LT.0d0)THEN
       alphasMZ=0.118d0
    ENDIF
    IF(abs(order).GE.2)THEN
       ! Need QCD corrections
       ! set QCD colour factors
       CALL SET_QCD_GROUP(QCD_CA,QCD_CF,QCD_TF)
       ! initialisation for alpha_s running
       CALL ReadElem_integer('alphas_nloop',alphas_nloop)
       IF(alphas_nloop.LT.1.OR.alphas_nloop.GT.5)THEN
          WRITE(*,*)"ERROR: the alphas RG running can only be from 1 to 5"
          STOP
       ENDIF
       ! Q_THR_default is the OS mass (2)
       ! the threshold is 1d0*quark_mass (1d0)
       CALL as_run_init(as_box,alphasMZ,zmass_PDG,alphas_nloop,&
            no_fix_nf,Q_THR_default(4:6),2,1D0)
    ENDIF
    IF(abs(order).EQ.1.OR.abs(order).EQ.3)THEN
       ! Need QED corrections
       CALL ReadElem_integer('alpha_scheme',alpha_scheme)
       IF(alpha_scheme.LT.0.OR.alpha_scheme.GT.2)THEN
          WRITE(*,*)"ERROR: alpha_scheme can only be from 0 to 2"
          STOP
       ENDIF
    ENDIF

    IF(nunit1.NE.6)THEN
       OPEN(UNIT=nunit1,FILE=TRIM(output_dir)//"RESULT_LbL.out")
    ENDIF
    nunit2=32
    CLOSE(nunit2)
    OPEN(nunit2,FILE=TRIM(output_dir)//'kine_LbL.out')
    nunit3=30
    CLOSE(nunit3)
    OPEN(nunit3,FILE=TRIM(tmp_dir)//'even_LbL.out',FORM='unformatted')
    nunit30=300
    CLOSE(nunit30)
    OPEN(nunit30,FILE=TRIM(tmp_dir)//'even_rwgt_LbL.out',FORM='unformatted')
    ! for LHA
    CLOSE(200)
    OPEN(200,FILE=TRIM(tmp_dir)//'sample_LbL.init')

    ! Scale scheme
    CALL ReadElem_integer('Scale',scale)
    CALL ReadElem_real('FScaleValue',fscalevalue)
    CALL ReadElem_real('muR_over_ref',muR_over_ref)

    ! reweighting stuff
    CALL ReadElem_logic('reweight_Scale',reweight_scale)
    IF(order.EQ.0.OR.(abs(order).EQ.1.AND.alpha_scheme.LE.1))THEN
       ! no need to reweight the scale for LO
       ! and NLO QED with alpha(0) or Gmu scheme
       reweight_scale=.FALSE.
    ENDIF
    IF(reweight_scale)THEN
       CALL ReadElem_real('rw_RScale_down',rw_Rscale_down)
       CALL ReadElem_real('rw_RScale_up',rw_Rscale_up)
       ho_nscale=2
       IF(rw_Rscale_down.LE.0d0.OR.rw_Rscale_up.LE.0d0)THEN
          reweight_scale=.FALSE.
          ho_nscale=0
          WRITE(*,*)"WARNING:Scale reweighting is off."
          WRITE(*,*)"WARNING:Please make sure rw_Rscale_up, rw_Rscale_down are larger than 0."
       ENDIF
       IF(reweight_scale.AND.rw_Rscale_down.GT.rw_Rscale_up)THEN
          temp=rw_Rscale_down
          rw_Rscale_down=rw_Rscale_up
          rw_Rscale_up=temp
       ENDIF
       rw_Rscales(1)=1d0
       rw_Rscales(2)=rw_Rscale_up
       rw_Rscales(3)=rw_Rscale_down
    ENDIF
    IF(reweight_scale)THEN
       tot_nrwgt=tot_nrwgt+ho_nscale+1
    ENDIF
    ! rwgt_values are used for LHE event file
    IF(reweight_scale)THEN
       IF(ALLOCATED(rwgt_values))THEN
          DEALLOCATE(rwgt_values)
       ENDIF
       ALLOCATE(rwgt_values(tot_nrwgt))
    ENDIF

    ! grid_gen
    CALL ReadElem_integer("grid_gen",grid_gen)
    if(grid_gen.LT.0.or.grid_gen.GT.2)then
       write(*,*)"ERROR: grid_gen can only be 0,1,2"
       stop
    endif
    if(grid_gen.eq.0)then
       INQUIRE(FILE=TRIM(grid_dir)//"Amp2L.grid",EXIST=lexist)
    else
       lexist=.FALSE.
    endif
    ! improve_w_LE
    CALL ReadElem_integer("improve_w_LE",improve_w_LE)
    if(improve_w_LE.LT.-1.or.improve_w_LE.GT.1)then
       write(*,*)"ERROR: improve_w_LE can only be -1,0,1"
       stop
    endif
    
    ! GPL stuff
    !CALL ReadElem_integer("GPLs_tool",GPLs_tool)
    GPLs_tool=1    ! (only FastGPL for double precision)
    GPLs_qp_tool=2 ! (only handyG for quadruple precision)
    !if(GPLs_tool.ne.1.and.GPLs_tool.ne.2)then
    !   WRITE(*,*)"ERROR: unknown GPLs_tool ! Only 1 (FastGPL) or 2 (handyG) is possible"
    !   STOP
    if(order.NE.0.and..NOT.(grid_gen.eq.0.and.lexist))then
       WRITE(*,*)"INFO: Will generate two-loop amp grid first ..."
       if(FastGPL_tool)then
          WRITE(*,*)"INFO: you will use FastGPL (2112.04122)"
          use_2L_DP=.TRUE.
       else
          WRITE(*,*)"INFO: you have disabled FastGPL (double precision)"
          use_2L_DP=.FALSE.
       endif
       if(HandyG_tool)then
          WRITE(*,*)"INFO: you will use handyG (1909.01656)"
          use_2L_QP=.TRUE.
       else
          WRITE(*,*)"INFO: you have disabled handyG (quadruple precision)"
          use_2L_QP=.FALSE.
       endif
       if(.not.FastGPL_tool.and..not.HandyG_tool)then
          WRITE(*,*)"ERROR: you have disabled both FastGPL and handyG"
          WRITE(*,*)"INFO: please set grid_gen = 0 or enable at least one of them"
          STOP
       elseif(FastGPL_tool.and..not.HandyG_tool)then
          WRITE(*,*)"INFO: Will use double precision only to generate two-loop amp grid"
       elseif(.not.FastGPL_tool.and.HandyG_tool)then
          WRITE(*,*)"INFO: Will use quadruple precision only to generate two-loop amp grid"
       endif
    endif

    CALL ReadElem_integer("nmc",nmc)

    IF(gener.EQ.0)THEN
       CALL LbL_VEGAS(rslt,nmc)
    ELSE
       CALL LbL_Romberg(rslt,nmc)
    ENDIF
    WRITE(nunit1,*)"sigma (pb)                   sd (pb)"
    WRITE(nunit1,*)rslt(1),rslt(2)
    CLOSE(nunit2)
    CLOSE(nunit3)
    CLOSE(21)
    CLOSE(200)
    IF(lunwei)THEN
       IF(Nevents_posw.EQ.Nevents_negw.AND.Nevents_posw.GT.0)THEN
          WRITE(*,*)"ERROR: Failed to generate the LHE file since there is same number of positive and negative events"
          STOP
       ENDIF
       CALL Generate_lhe_LbL(Nevents_posw,Nevents_negw,icase)
    ENDIF
  END SUBROUTINE calc_LbL

  SUBROUTINE LbL_Romberg(rslt,ncalln)
    USE NINTLIB
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(3),INTENT(OUT)::rslt
    INTEGER,INTENT(IN),OPTIONAL::ncalln
    INTEGER::ITMX
    REAL(KIND(1d0))::vfes,sd,chi2a
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::XL,XU
    INTEGER,DIMENSION(:),ALLOCATABLE::sub_num
    INTEGER::ncalm,nc,ii
    CHARACTER(len=4),DIMENSION(20)::chchar
    INTEGER::IDBMUP1,IDBMUP2,IDWTUP
    REAL(KIND(1d0))::TOL
    INTEGER::ind_err,eval_num
    if(colpar.eq.4)then
       ! without PDF
       varnum=1
    else
       ! with PDF
       varnum=3
    endif

    IF(ALLOCATED(XL))THEN
       DEALLOCATE(XL)
    ENDIF
    ALLOCATE(XL(varnum))
    IF(ALLOCATED(XU))THEN
       DEALLOCATE(XU)
    ENDIF
    ALLOCATE(XU(varnum))
    IF(ALLOCATED(sub_num))THEN
       DEALLOCATE(sub_num)
    ENDIF
    ALLOCATE(sub_num(varnum))
    DO ii=1,varnum
       XL(ii)=0.0d0
       XU(ii)=1.0d0
    ENDDO
    ITMX=1
    IF(PRESENT(ncalln))THEN
       ncalm=ncalln
    ELSE
       ncalm=5000
    ENDIF
    if(varnum.eq.1)then
       sub_num(1)=ncalm
    else
       sub_num(1)=MAX(INT(DBLE(ncalm)**(1d0/3d0))+1,12)
       sub_num(2)=sub_num(1)
       sub_num(3)=sub_num(2)-2
    endif
    nprint=ncalm/10
    TOL=MAX(1d0/DSQRT(DBLE(ncalm)),1d-5)
    weight_Romberg=PRODUCT((XU(1:varnum)-XL(1:varnum))&
         /DBLE(sub_num(1:varnum)))
    IF(plot_output)CALL initplot_LbL
    CALL Romberg_nd(LbL_fxn4Romberg,XL,XU,varnum,sub_num,ITMX,tol,&
         rslt(1),ind_err,eval_num)
    if(ind_err.EQ.-1)then
       WRITE(*,*)"WARNING: the precision in Romberg integration has not bee reached"
    endif
    rslt(2)=abs(tol*rslt(1))
    rslt(3)=0d0
    WRITE(*,*)rslt(1),"+\-",rslt(2)
    WRITE(*,*)"precision:",rslt(2)/MAX(ABS(rslt(1)),1D-99)
    IF(plot_output)CALL plotout_LbL
    RETURN
  END SUBROUTINE LbL_Romberg

  FUNCTION LbL_fxn4Romberg(dim_num,x)
    USE LbL_cuts
    USE NLOME
    USE ElasticPhotonPhotonFlux
    IMPLICIT NONE
    INTEGER,INTENT(IN)::dim_num
    REAL(KIND(1d0)),DIMENSION(dim_num),INTENT(IN)::x
    REAL(KIND(1d0))::LbL_fxn4Romberg
    REAL(KIND(1d0))::wgt,sinth,costh,phi
    INTEGER::init=0,nwarn,nwmax,nwri,nnn=0,nnntot=0
    INTEGER::i,j,ii,kk
    LOGICAL::icuts,lflag
    SAVE init,nwarn,nwmax,nwri,nnn,nnntot
    REAL(KIND(1d0))::sqs,sq,ycollcm,maa,jac,ycms
    SAVE sqs,sq,ycollcm
    REAL(KIND(1d0))::taumin,taumax,ymax0,ymin0,jac0,tau10,tau11,tau
    SAVE taumin,taumax,jac0,tau10,tau11
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),DIMENSION(0:3)::PBOO
    REAL(KIND(1d0))::wtotal,wme,wsf,wthis,w1
    REAL(KIND(1d0))::recmax=0,recmin=0
    REAL(KIND(1d0))::Q1,Q2,Q12,Q22
    LOGICAL::reshuffled
    IF(init.EQ.0)THEN
       nwmax=0
       nwarn=0
       nwri=0
       ebeam0(1)=ABS(energy_beam1)
       ebeam0(2)=ABS(energy_beam2)
       IF(ABS(ebeam0(1)-ebeam0(2))/MAX(ebeam0(1)+ebeam0(2),1d-17).LT.1d-8)THEN
          labeqcoll=.TRUE.
       ELSE
          labeqcoll=.FALSE.
       ENDIF
       sqs=2d0*DSQRT(ebeam0(1)*ebeam0(2)) ! we always neglect the mass of initial states
       EBMUP1=ebeam0(1)
       EBMUP2=ebeam0(2)
       ycollcm=DLOG(ABS(ebeam0(1))/ABS(ebeam0(2)))/2d0

       if(colpar.eq.1)then
          iPDFSUP1=750050+UPC_photon_flux
          iPDFSUP2=750050+UPC_photon_flux
          iPDFGUP1=0
          iPDFGUP2=0
       elseif(colpar.eq.2)then
          iPDFSUP1=750054
          iPDFSUP2=750053
          iPDFGUP1=0
          iPDFGUP2=0
       elseif(colpar.eq.3)then
          iPDFSUP1=750054
          iPDFSUP2=750054
          iPDFGUP1=0
          iPDFGUP2=0
       elseif(colpar.eq.4)then
          iPDFSUP1=-1
          iPDFSUP2=-1
          iPDFGUP1=-1
          iPDFGUP2=-1
       else
          WRITE(*,*)"ERROR: Do not understand colpar=",colpar
          STOP
       endif

       xp1=1d0
       xp2=1d0
       CALL readcuts_LbL
       sq=sqs*sqs
       if(maxyrapp.GE.0d0.and.minyrapp.LE.0d0)then
          ymin0=0d0
       else
          ymin0=MIN(abs(maxyrapp),abs(minyrapp))
       endif
       ymax0=MAX(abs(maxyrapp),abs(minyrapp))
       if(colpar.eq.4)then
          if(minmpp.GT.sqs)then
             write(*,*)"ERROR: No event survives in gamma-gamma collisions #1"
             stop
          endif
          if(maxmpp.GT.0d0.AND.maxmpp.LT.sqs)then
             write(*,*)"ERROR: No event survives in gamma-gamma collisions #2"
             stop
          endif
          !if(minptp*2d0*DCOSH(ymin0).GT.sqs)then
          !   write(*,*)"ERROR: No event survives in gamma-gamma collisions #3"
          !   stop
          !endif
          !if(maxptp.GT.0d0.AND.maxptp*2d0*DCOSH(ymax0).LT.sqs)then
          !   write(*,*)"ERROR: No event survives in gamma-gamma collisions #4"
          !   stop
          !endif
          taumin=1d0
          taumax=1d0
          jac0=1d0/(16d0*pipi*sq)
       else
          !taumin=MAX(minmpp,minptp*2d0*DCOSH(ymin0))**2/sq
          taumin=minmpp**2/sq
          taumin=MAX(taumin,0d0)
          taumax=1d0
          if(maxmpp.GT.0d0)then
             taumax=MIN(taumax,maxmpp**2/sq)
          endif
          CALL GET_ntau(taumin,ntau)
          !if(maxptp.GT.0d0)then
          !   taumax=MIN(taumax,(maxptp*2d0*DCOSH(ymax0))**2/sq)
          !endif
          if(ntau.LE.1d0)then
             write(*,*)"ERROR: ntau <= 1"
             stop
          endif
          tau10=taumin**(1d0-ntau)
          tau11=tau10-taumax**(1d0-ntau)
          jac0=tau11/(ntau-1d0)
          jac0=jac0/(16d0*pipi*sq)
       endif

       init=1
    ENDIF
    nnntot=nnntot+1 
    wgt=weight_Romberg
    wtotal=0d0
    LbL_fxn4Romberg=0d0
    costh=2d0*x(1)-1d0 ! theta is defined in the partonic rest frame
    ! jacobia 2 has been absorbed into jac0
    if(abs(costh).GT.1d0)costh=1d0
    sinth=DSQRT(1d0-costh**2)
    jac=jac0
    if(colpar.ne.4)then
       ! with PDF
       tau=(tau10-tau11*x(2))**(1d0/(1d0-ntau))
       if(tau.GT.1d0)tau=1d0
       if(tau.LE.0d0)then
          write(*,*)"Error: tau <= 0"
          stop
       endif
       ycms=(1d0-2d0*x(3))/2d0*DLOG(tau) ! ycms is defined in the center-of-mass frame of the two intial beams
       xp1=DSQRT(tau)*DEXP(ycms)
       xp2=DSQRT(tau)*DEXP(-ycms)
       jac=jac*tau**(ntau-1d0)*(-DLOG(tau))
    endif
    ! initial state
    ! first in the center-of-mass frame of the two initial beams
    LbL_pmom_CF(1,0)=sqs/2d0*xp1
    LbL_pmom_CF(1,1:2)=0d0
    LbL_pmom_CF(1,3)=sqs/2d0*xp1
    LbL_pmom_CF(2,0)=sqs/2d0*xp2
    LbL_pmom_CF(2,1:2)=0d0
    LbL_pmom_CF(2,3)=-sqs/2d0*xp2
    ! final state
    ! in the partonic rest frame
    maa=DSQRT(xp1*xp2*sq)
    phi=2d0*pipi*rand() ! ME is independent of phi anyway
    LbL_pmom_CF(3,0)=maa/2d0
    LbL_pmom_CF(3,1)=-maa/2d0*sinth*DCOS(phi)
    LbL_pmom_CF(3,2)=-maa/2d0*sinth*DSIN(phi)
    LbL_pmom_CF(3,3)=-maa/2d0*costh
    LbL_pmom_CF(4,0)=maa/2d0
    LbL_pmom_CF(4,1)=maa/2d0*sinth*DCOS(phi)
    LbL_pmom_CF(4,2)=maa/2d0*sinth*DSIN(phi)
    LbL_pmom_CF(4,3)=maa/2d0*costh
    ! boost back to the collision frame (from cms)
    if(xp1.NE.xp2)then
       PBOO(1:2)=0d0
       PBOO(3)=(xp1-xp2)*sqs/2d0
       PBOO(0)=(xp1+xp2)*sqs/2d0
       DO i=3,4
          CALL Boostl(maa,PBOO,LbL_pmom_CF(i,0:3))
       ENDDO
    endif
    IF(colpar.EQ.1.and.intrinsic_kt)then
       Q1=0d0
       Q2=0d0
       reshuffled=.FALSE.
       DO WHILE(.NOT.reshuffled)
          ! generate Q1 and Q2
          if(nuclearA1.EQ.0.and.nuclearZ1.EQ.0)then
             call generate_Q2_epa_proton_iWW(xp1,Q2max,Q12)
          else
             call generate_Q2_epa_ion_ChFF(ion_Form,1,xp1,Q2max,&
                  LbL_RA(1),LbL_aA(1),LbL_wA(1),Q12)
          endif
          if(nuclearA2.EQ.0.and.nuclearZ2.EQ.0)then
             call generate_Q2_epa_proton_iWW(xp2,Q2max,Q22)
          else
             if(nuclearA1.EQ.nuclearA2.and.nuclearZ1.eq.nuclearZ2)then
                call generate_Q2_epa_ion_ChFF(ion_Form,1,xp2,Q2max,&
                     LbL_RA(1),LbL_aA(1),LbL_wA(1),Q22)
             else
                call generate_Q2_epa_ion_ChFF(ion_Form,2,xp2,Q2max,&
                     LbL_RA(2),LbL_aA(2),LbL_wA(2),Q22)
             endif
          endif
          Q1=DSQRT(Q12)
          Q2=DSQRT(Q22)
          ! perform the initial momentum reshuffling
          CALL InitialMomentumReshuffle_PhotonPhoton(4,LbL_pmom_CF(1:4,0:3),&
               sqs,xp1,xp2,Q1,Q2,LbL_pmom(1:4,0:3),reshuffled)
       ENDDO
    ELSE
       DO i=1,4
          LbL_pmom(i,0:3)=LbL_pmom_CF(i,0:3)
       ENDDO
    endif
    ! boost back to the lab frame
    IF(.NOT.labeqcoll)THEN
       PBOO(1:2)=0d0
       PBOO(3)=ebeam0(1)-ebeam0(2)
       PBOO(0)=ebeam0(1)+ebeam0(2)
       DO i=1,4
          CALL Boostl(sqs,PBOO,LbL_pmom(i,0:3))
          CALL Boostl(sqs,PBOO,LbL_pmom_CF(i,0:3))
       ENDDO
    ENDIF

    CALL Cuts_LbL(icuts)

    IF(.NOT.icuts.or.jac.LE.0d0)THEN
       LbL_fxn4Romberg=0d0
       RETURN
    ENDIF

    ! ME can only be evaluated in collinear factorisation
    CALL stwoloopmatrix_LbL(LbL_pmom_CF,wme)
    if(isnan(wme))then
       WRITE(*,*)"ERROR: amplitude square becomes NaN in the following PS:"
       do i=1,4
          WRITE(*,*)LbL_pmom_CF(i,0:3)
       enddo
       stop
    endif
    if(wme.eq.0d0)then
       LbL_fxn4Romberg=0d0
       RETURN
    endif
    CALL strf_pdf_LbL(wsf)
    if(wsf.le.0d0)then
       LbL_fxn4Romberg=0d0
       RETURN
    endif
    wtotal=wme*wsf*jac*3.8937966d8 ! in unit of pb

    if(plot_output.and.wtotal.ne.0d0)then
       w1=wtotal*wgt
       call outfun_LbL(w1)
    endif

    LbL_fxn4Romberg=wtotal
    nnn=nnn+1
    IF(recmax.LT.LbL_fxn4Romberg)recmax=LbL_fxn4Romberg
    IF(recmin.GT.LbL_fxn4Romberg)recmin=LbL_fxn4Romberg
    IF(MOD(nnn,nprint).EQ.0)THEN
       PRINT *,"max=",recmax
       PRINT *,"min=",recmin
       PRINT *, "      n_pass,     n_total"
       PRINT *,nnn,nnntot
    ENDIF
    RETURN
  END FUNCTION LbL_fxn4Romberg

  SUBROUTINE LbL_VEGAS(rslt,ncalln)
    USE MC_VEGAS
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(3),INTENT(OUT)::rslt
    INTEGER,INTENT(IN),OPTIONAL::ncalln
    REAL(KIND(1d0))::vfes,sd,chi2a
    INTEGER::ncalm,nc,ii
    CHARACTER(len=4),DIMENSION(20)::chchar
    INTEGER::IDBMUP1,IDBMUP2,IDWTUP
    lwmax=0
    NPRN=-1
    if(colpar.EQ.4)then
       ! without PDF
       varnum=2
    ELSE
       ! with PDF
       varnum=4
    ENDIF
    DO ii=1,varnum
       XL(ii)=0.0d0
       XU(ii)=1.0d0
    ENDDO
    ITMX=1
    IF(PRESENT(ncalln))THEN
       ncalm=ncalln
    ELSE
       ncalm=5000
    ENDIF
    chchar(1)="40K"
    chchar(2)="80K"
    chchar(3)="160K"
    chchar(4)="320K"
    chchar(5)="640K"
    chchar(6)="1M"
    chchar(7)="2M"
    chchar(8)="4M"
    chchar(9)="8M"
    chchar(10)="16M"
    chchar(11)='32M'
    chchar(12)='64M'
    chchar(13)='120M'
    chchar(14)='240M'
    chchar(15)='480M'
    chchar(16)='960M'
    chchar(17)='2G'
    chchar(18)='4G'
    chchar(19)='8G'
    chchar(20)='16G'
    nprint=10000
    NCALL=20000
    IF(plot_output)CALL initplot_LbL
    CALL VEGAS(varnum,LbL_fxn,vfes,sd,chi2a)
    IF(plot_output)CALL plotout_LbL
    WRITE(*,*)' '
    WRITE(*,*)"====================NCALL=20K==========================="
    WRITE(*,*)" "
    ii=1
    WRITE(*,*)"ITERATION ",ii,":"
    WRITE(*,*)vfes,"+\-",sd
    WRITE(*,*)"precision:",sd/vfes
    DO ii=2,10
       IF(plot_output)CALL initplot_LbL
       CALL VEGAS(varnum,LbL_fxn,vfes,sd,chi2a,1)
       IF(plot_output)CALL plotout_LbL
       WRITE(*,*)"ITERATION ",ii,":"
       WRITE(*,*)vfes,"+\-",sd
       WRITE(*,*)"precision:",sd/vfes
    ENDDO
    ii=1
    DO
       nc=2*NCALL
       IF(nc.GT.ncalm)EXIT
       IF(2*nc.GT.ncalm.AND.lunwei2.AND.lwmax.EQ.0)THEN
          lwmax=1
       ENDIF
       NCALL=nc
       IF(NCALL/maxprint.GT.nprint)nprint=NCALL/maxprint
       WRITE(*,*)"====================NCALL="//chchar(ii)//"==========================="
       WRITE(*,*)" "
       IF(plot_output)CALL initplot_LbL
       CALL VEGAS(varnum,LbL_fxn,vfes,sd,chi2a,1)
       IF(plot_output)CALL plotout_LbL
       WRITE(*,*)vfes,"+\-",sd
       WRITE(*,*)"precision:",sd/vfes
       ii=ii+1
    ENDDO
    IF(lunwei2.AND.lwmax.EQ.1)THEN
       lwmax=2
       WRITE(*,*)" "
       WRITE(*,*)"START UNWEIGHTING"
       NCALL=nc/2
       WRITE(*,*)"====================NCALL="//chchar(ii-1)//"==========================="
       WRITE(*,*)" "
       IF(plot_output)CALL initplot_LbL
       CALL VEGAS(varnum,LbL_fxn,vfes,sd,chi2a,1)
       IF(plot_output)CALL plotout_LbL
       WRITE(*,*)vfes,"+\-",sd
       WRITE(*,*)"precision:",sd/vfes
    ENDIF
    SELECT CASE(colpar)
    CASE(1)
       IDBMUP1=2212
       IDBMUP2=2212
    CASE(2)
       IDBMUP1=2212
       IDBMUP2=11
    CASE(3)
       IDBMUP1=11
       IDBMUP2=-11
    CASE DEFAULT
       IDBMUP1=22
       IDBMUP2=22
    END SELECT
    IF(lunwei2)THEN
       IDWTUP=3
    ELSE
       IDWTUP=1
    ENDIF
    NPRUP=NPRUP+1
    WRITE(200,5100) IDBMUP1,IDBMUP2,EBMUP1,EBMUP2,&
         iPDFGUP1,iPDFGUP2,iPDFSUP1,iPDFSUP2,IDWTUP,NPRUP
    WRITE(200,5200) vfes,sd,1d0, 92
    rslt(1)=vfes
    rslt(2)=sd
    rslt(3)=chi2a
    IF(lunwei2)PRINT *,"number of events, w>0, w<0:",Nevents,Nevents_posw,Nevents_negw
    RETURN
5100 FORMAT(1P,2I8,2E14.6,6I8)
5200 FORMAT(1P,3E20.10,I6)
  END SUBROUTINE LbL_VEGAS

  FUNCTION LbL_fxn(x,wgt)
    USE LbL_cuts
    USE NLOME
    USE ElasticPhotonPhotonFlux
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(varnum),INTENT(IN)::x
    REAL(KIND(1d0)),INTENT(IN)::wgt
    REAL(KIND(1d0))::LbL_fxn
    REAL(KIND(1d0))::sinth,costh,phi
    INTEGER::init=0,nwarn,nwmax,nwri,nnn=0,nnntot=0
    INTEGER::i,j,ii,kk
    LOGICAL::icuts,lflag
    SAVE init,nwarn,nwmax,nwri,nnn,nnntot
    REAL(KIND(1d0))::sqs,sq,ycollcm,maa,jac,ycms
    SAVE sqs,sq,ycollcm
    REAL(KIND(1d0))::taumin,taumax,ymax0,ymin0,jac0,tau10,tau11,tau
    SAVE taumin,taumax,jac0,tau10,tau11
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),DIMENSION(0:3)::PBOO
    REAL(KIND(1d0))::wtotal,wme,wsf,wthis,w1
    REAL(KIND(1d0))::recmax=0,recmin=0
    REAL(KIND(1d0))::Q1,Q2,Q12,Q22
    LOGICAL::reshuffled
    IF(init.EQ.0)THEN
       nwmax=0
       nwarn=0
       nwri=0
       ebeam0(1)=ABS(energy_beam1)
       ebeam0(2)=ABS(energy_beam2)
       IF(ABS(ebeam0(1)-ebeam0(2))/MAX(ebeam0(1)+ebeam0(2),1d-17).LT.1d-8)THEN
          labeqcoll=.TRUE.
       ELSE
          labeqcoll=.FALSE.
       ENDIF
       sqs=2d0*DSQRT(ebeam0(1)*ebeam0(2)) ! we always neglect the mass of initial states
       EBMUP1=ebeam0(1)
       EBMUP2=ebeam0(2)
       ycollcm=DLOG(ABS(ebeam0(1))/ABS(ebeam0(2)))/2d0

       if(colpar.eq.1)then
          iPDFSUP1=750050+UPC_photon_flux
          iPDFSUP2=750050+UPC_photon_flux
          iPDFGUP1=0
          iPDFGUP2=0
       elseif(colpar.eq.2)then
          iPDFSUP1=750054
          iPDFSUP2=750053
          iPDFGUP1=0
          iPDFGUP2=0
       elseif(colpar.eq.3)then
          iPDFSUP1=750054
          iPDFSUP2=750054
          iPDFGUP1=0
          iPDFGUP2=0
       elseif(colpar.eq.4)then
          iPDFSUP1=-1
          iPDFSUP2=-1
          iPDFGUP1=-1
          iPDFGUP2=-1
       else
          WRITE(*,*)"ERROR: Do not understand colpar=",colpar
          STOP
       endif

       xp1=1d0
       xp2=1d0
       CALL readcuts_LbL
       sq=sqs*sqs
       if(maxyrapp.GE.0d0.and.minyrapp.LE.0d0)then
          ymin0=0d0
       else
          ymin0=MIN(abs(maxyrapp),abs(minyrapp))
       endif
       ymax0=MAX(abs(maxyrapp),abs(minyrapp))
       if(colpar.eq.4)then
          if(minmpp.GT.sqs)then
             write(*,*)"ERROR: No event survives in gamma-gamma collisions #1"
             stop
          endif
          if(maxmpp.GT.0d0.AND.maxmpp.LT.sqs)then
             write(*,*)"ERROR: No event survives in gamma-gamma collisions #2"
             stop
          endif
          taumin=1d0
          taumax=1d0
          jac0=1d0/(16d0*pipi*sq)
       else
          taumin=minmpp**2/sq
          taumin=MAX(taumin,0d0)
          taumax=1d0
          if(maxmpp.GT.0d0)then
             taumax=MIN(taumax,maxmpp**2/sq)
          endif
          CALL GET_ntau(taumin,ntau)
          if(ntau.LE.1d0)then
             write(*,*)"ERROR: ntau <= 1"
             stop
          endif
          tau10=taumin**(1d0-ntau)
          tau11=tau10-taumax**(1d0-ntau)
          jac0=tau11/(ntau-1d0)
          jac0=jac0/(16d0*pipi*sq)
       endif

       init=1
    ENDIF
    nnntot=nnntot+1
    wtotal=0d0
    LbL_fxn=0d0

    costh=2d0*x(1)-1d0 ! theta is defined in the partonic rest frame
    ! jacobia 2 has been absorbed into jac0
    if(abs(costh).GT.1d0)costh=1d0
    sinth=DSQRT(1d0-costh**2)
    phi=2d0*pipi*x(2) ! ME is independent of phi anyway
    jac=jac0
    if(colpar.ne.4)then
       ! with PDF
       tau=(tau10-tau11*x(3))**(1d0/(1d0-ntau))
       if(tau.GT.1d0)tau=1d0
       if(tau.LE.0d0)then
          write(*,*)"Error: tau <= 0"
          stop
       endif
       ycms=(1d0-2d0*x(4))/2d0*DLOG(tau) ! ycms is defined in the center-of-mass frame of the two intial beams
       xp1=DSQRT(tau)*DEXP(ycms)
       xp2=DSQRT(tau)*DEXP(-ycms)
       jac=jac*tau**(ntau-1d0)*(-DLOG(tau))
    endif
    ! initial state
    ! first in the center-of-mass frame of the two initial beams
    LbL_pmom_CF(1,0)=sqs/2d0*xp1
    LbL_pmom_CF(1,1:2)=0d0
    LbL_pmom_CF(1,3)=sqs/2d0*xp1
    LbL_pmom_CF(2,0)=sqs/2d0*xp2
    LbL_pmom_CF(2,1:2)=0d0
    LbL_pmom_CF(2,3)=-sqs/2d0*xp2
    ! final state
    ! in the partonic rest frame
    maa=DSQRT(xp1*xp2*sq)
    LbL_pmom_CF(3,0)=maa/2d0
    LbL_pmom_CF(3,1)=-maa/2d0*sinth*DCOS(phi)
    LbL_pmom_CF(3,2)=-maa/2d0*sinth*DSIN(phi)
    LbL_pmom_CF(3,3)=-maa/2d0*costh
    LbL_pmom_CF(4,0)=maa/2d0
    LbL_pmom_CF(4,1)=maa/2d0*sinth*DCOS(phi)
    LbL_pmom_CF(4,2)=maa/2d0*sinth*DSIN(phi)
    LbL_pmom_CF(4,3)=maa/2d0*costh
    ! boost back to the collision frame (from cms)
    if(xp1.NE.xp2)then
       PBOO(1:2)=0d0
       PBOO(3)=(xp1-xp2)*sqs/2d0
       PBOO(0)=(xp1+xp2)*sqs/2d0
       DO i=3,4
          CALL Boostl(maa,PBOO,LbL_pmom_CF(i,0:3))
       ENDDO
    endif
    IF(colpar.EQ.1.and.intrinsic_kt)then
       Q1=0d0
       Q2=0d0
       reshuffled=.FALSE.
       DO WHILE(.NOT.reshuffled)
          ! generate Q1 and Q2
          if(nuclearA1.EQ.0.and.nuclearZ1.EQ.0)then
             call generate_Q2_epa_proton_iWW(xp1,Q2max,Q12)
          else
             call generate_Q2_epa_ion_ChFF(ion_Form,1,xp1,Q2max,&
                  LbL_RA(1),LbL_aA(1),LbL_wA(1),Q12)
          endif
          if(nuclearA2.EQ.0.and.nuclearZ2.EQ.0)then
             call generate_Q2_epa_proton_iWW(xp2,Q2max,Q22)
          else
             if(nuclearA1.EQ.nuclearA2.and.nuclearZ1.eq.nuclearZ2)then
                call generate_Q2_epa_ion_ChFF(ion_Form,1,xp2,Q2max,&
                     LbL_RA(1),LbL_aA(1),LbL_wA(1),Q22)
             else
                call generate_Q2_epa_ion_ChFF(ion_Form,2,xp2,Q2max,&
                     LbL_RA(2),LbL_aA(2),LbL_wA(2),Q22)
             endif
          endif
          Q1=DSQRT(Q12)
          Q2=DSQRT(Q22)
          ! perform the initial momentum reshuffling
          CALL InitialMomentumReshuffle_PhotonPhoton(4,LbL_pmom_CF(1:4,0:3),&
               sqs,xp1,xp2,Q1,Q2,LbL_pmom(1:4,0:3),reshuffled)
       ENDDO
    ELSE
       DO i=1,4
          LbL_pmom(i,0:3)=LbL_pmom_CF(i,0:3)
       ENDDO
    ENDIF
    ! boost back to the lab frame
    IF(.NOT.labeqcoll)THEN
       PBOO(1:2)=0d0
       PBOO(3)=ebeam0(1)-ebeam0(2)
       PBOO(0)=ebeam0(1)+ebeam0(2)
       DO i=1,4
          CALL Boostl(sqs,PBOO,LbL_pmom(i,0:3))
          CALL Boostl(sqs,PBOO,LbL_pmom_CF(i,0:3))
       ENDDO
    ENDIF

    CALL Cuts_LbL(icuts)

    IF(.NOT.icuts.or.jac.LE.0d0)THEN
       LbL_fxn=0d0
       RETURN
    ENDIF

    ! ME can only be evaluated in collinear factorisation
    CALL stwoloopmatrix_LbL(LbL_pmom_CF,wme)
    if(isnan(wme))then
       WRITE(*,*)"ERROR: amplitude square becomes NaN in the following PS:"
       do i=1,4
          WRITE(*,*)LbL_pmom_CF(i,0:3)
       enddo
       stop
    endif
    if(wme.eq.0d0)then
       LbL_fxn=0d0
       RETURN
    endif
    CALL strf_pdf_LbL(wsf)
    if(wsf.le.0d0)then
       LbL_fxn=0d0
       RETURN
    endif
    wtotal=wme*wsf*jac*3.8937966d8 ! in unit of pb

    IF(lunwei2.AND.lwmax.GT.0.AND.wtotal.ne.0d0)THEN
       w1=wtotal*wgt ! multiply VEGAS weight
       CALL unwei_procedure_LbL(w1,nwri,nwmax,nwarn)
    ENDIF
    if(plot_output.and.wtotal.ne.0d0)then
       w1=wtotal*wgt
       call outfun_LbL(w1)
    endif

    LbL_fxn=wtotal
    nnn=nnn+1
    IF(recmax.LT.LbL_fxn)recmax=LbL_fxn
    IF(recmin.GT.LbL_fxn)recmin=LbL_fxn
    IF(MOD(nnn,nprint).EQ.0)THEN
       PRINT *,"max=",recmax
       PRINT *,"min=",recmin
       PRINT *, "      n_pass,     n_total"
       PRINT *,nnn,nnntot
    ENDIF
    RETURN
  END FUNCTION LbL_fxn

  SUBROUTINE strf_pdf_LbL(wsf)
    use Photon_PDFs
    USE ElasticPhotonPhotonFlux
    IMPLICIT NONE
    include '../vendor/gammaUPC/run90.inc'
    REAL(KIND(1d0)),INTENT(OUT)::wsf
    INTEGER::init=0
    SAVE init
    if(init.eq.0)then
       nuclearA_beam1=nuclearA1
       nuclearZ_beam1=nuclearZ1
       nuclearA_beam2=nuclearA2
       nuclearZ_beam2=nuclearZ2
       ebeam(1)=ebeam0(1)
       ebeam(2)=ebeam0(2)
       alphaem_elasticphoton=1d0/alphaemm1
       ! whether or not to use MC-Glauber TAA for the survival probability
       ! when colpar=1 and UPC_photon_flux=1 or 2.
       CALL ReadElem_logic('use_MC_Glauber',use_MC_Glauber)
       init=1
    endif

    if(colpar.eq.4)then
       ! gamma-gamma
       wsf=1d0
    elseif(colpar.eq.3)then
       ! e-e+
       wsf=epa_electron(xp1,q2max)
       wsf=wsf*epa_electron(xp2,q2max)
    elseif(colpar.eq.2)then
       ! e-p
       wsf=epa_proton(xp1,q2max)
       wsf=wsf*epa_electron(xp2,q2max)
    else
       ! hadron-hadron UPC
       if(UPC_photon_flux.EQ.3)then
          ! iww (no survival probability) for pp
          wsf=epa_proton(xp1,q2max)
          wsf=wsf*epa_proton(xp2,q2max)
       else
          if(UPC_photon_flux.eq.1)then
             ! ChFF (see gamma-UPC 2207.03012)
             USE_CHARGEFORMFACTOR4PHOTON=.TRUE.
          else
             ! EDFF (see gamma-UPC 2207.03012)
             USE_CHARGEFORMFACTOR4PHOTON=.FALSE.
          endif
          IF(nuclearA_beam1.EQ.0.AND.nuclearA_beam2.EQ.0)THEN
             ! pp
             wsf=PhotonPhotonFlux_pp(xp1,xp2)
          ELSEIF((nuclearA_beam1.NE.0.AND.nuclearA_beam2.EQ.0).OR.&
               (nuclearA_beam1.EQ.0.AND.nuclearA_beam2.NE.0))THEN
             ! pA
             wsf=PhotonPhotonFlux_pA_WoodsSaxon(xp1,xp2)
          ELSE
             ! AB
             wsf=PhotonPhotonFlux_AB_WoodsSaxon(xp1,xp2)
          ENDIF
       endif
    endif
    
    IF(init.EQ.0)init=1
  END SUBROUTINE strf_pdf_LbL

  SUBROUTINE readcuts_LbL
    IMPLICIT NONE
    CALL ReadElem_real('minptp',minptp)
    CALL ReadElem_real('maxptp',maxptp)
    CALL ReadElem_real('maxptpp',maxptpp)
    CALL ReadElem_real('maxaco',maxaco)
    CALL ReadElem_real('maxyrapp',maxyrapp)
    CALL ReadElem_real('minyrapp',minyrapp)
    CALL ReadElem_real('minmpp',minmpp)
    CALL ReadElem_real('maxmpp',maxmpp)
    WRITE(*,*)'---------------------------------------------------'
    WRITE(*,*)'        the cuts in the lab frame                  '
    WRITE(*,*)'min transverse momentum of photon ',minptp
    IF(maxptp.GT.0d0)THEN
       WRITE(*,*)'max transverse momentum of photon ',maxptp
       IF(minptp.GE.maxptp)THEN
          WRITE(*,*)'ERROR: No event survives with pT cut'
          STOP
       ENDIF
    ENDIF
    WRITE(*,*)'max (pseudo-)rapidity of photon ',maxyrapp
    WRITE(*,*)'min (pseudo-)rapidity of photon ',minyrapp
    IF(minyrapp.GE.maxyrapp)THEN
       WRITE(*,*)'ERROR: No event survives with (pseudo-)rapidity cut'
       STOP
    ENDIF
    WRITE(*,*)'min invariant mass of di-photon ',minmpp
    IF(maxmpp.GT.0d0)THEN
       WRITE(*,*)'max invariant mass of di-photon ',maxmpp
       IF(minmpp.GE.maxmpp)THEN
          WRITE(*,*)'ERROR: No event survives with invariant mass cut'
          STOP
       ENDIF
    ENDIF
    IF(maxptpp.GE.0d0)THEN
       WRITE(*,*)'max transverse mom. of di-photon ',maxptpp
       IF(maxptpp.EQ.0d0)THEN
          WRITE(*,*)'ERROR: no event survives with the transverse mom cut'
          STOP
       ENDIF
    ENDIF
    IF(maxaco.GE.0d0)THEN
       WRITE(*,*)'max acoplanarity of di-photon ',maxaco
       IF(maxaco.EQ.0d0)THEN
          WRITE(*,*)'ERROR: no event survives with the acoplanarity cut'
          STOP
       ENDIF
    ENDIF
    WRITE(*,*)'---------------------------------------------------'
    RETURN
  END SUBROUTINE readcuts_LbL

  SUBROUTINE unwei_procedure_LbL(w1,nwri,nwmax,nwarn)
    USE MC_VEGAS
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::w1
    INTEGER,INTENT(INOUT)::nwri,nwmax,nwarn
    REAL(KIND(1d0)),DIMENSION(1)::ranr
    REAL(KIND(1d0))::vtime,vspin,scale1,scalup,xwgtup,&
         px,py,pz,p0,pmass,umax,umax1
    INTEGER::init=0,i,iij,i2,j2
    INTEGER::idup,idprup,istup,imothup1,imothup2,icol1,icol2
    LOGICAL::llwri
    SAVE init,umax,umax1
    IF(init.EQ.0)THEN
       umax=0d0
       umax1=0d0
       init=1
    ENDIF
    IF(lwmax.EQ.1)THEN
       IF(umax.LT.DABS(w1))THEN
          umax=DABS(w1)
       ENDIF
       nwmax=nwmax+1
       umax1=umax
    ENDIF
    IF(lwmax.EQ.2)THEN
       IF(DABS(w1).GT.umax1)THEN
          umax1=DABS(w1)
          WRITE(*,*)'WARNING:umax1,umax',umax1,umax
       ENDIF
       llwri=.FALSE.
       CALL RANDA(1,ranr)
       IF(umax*ranr(1).LT.DABS(w1))llwri=.TRUE.
       IF(umax.LT.DABS(w1))nwarn=nwarn+1
       IF(llwri)THEN
          nwri=nwri+1
          idprup=92 ! id for the process
          if(w1.GT.0d0)THEN
             xwgtup=1d0
             Nevents_posw=Nevents_posw+1
          ELSE
             xwgtup=-1d0
             Nevents_negw=Nevents_negw+1
          ENDIF
          call LbL_scale(scalup)
          WRITE(nunit3)4,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP
          DO i=1,4
             idup=22
             IF(i.LE.2)THEN
                istup=-1
             ELSE
                istup=1
             ENDIF
             imothup1=0
             imothup2=0
             IF(i.EQ.3.OR.i.EQ.4)THEN
                imothup1=1
                imothup2=2
             ENDIF
             icol1=0
             icol2=0
             px=LbL_pmom(i,1)
             py=LbL_pmom(i,2)
             pz=LbL_pmom(i,3)
             p0=LbL_pmom(i,0)
             if(colpar.EQ.1.and.intrinsic_kt.and.i.le.2)then
                pmass=p0**2-px**2-py**2-pz**2
                if(pmass.LT.0d0)then
                   ! negative means it is spacelike
                   pmass=-dsqrt(-pmass)
                else
                   pmass=dsqrt(pmass)
                endif
             else
                pmass=0d0
             endif
             vtime=0
             vspin=9
             WRITE(nunit3)idup,istup,imothup1,imothup2,icol1,icol2&
                  ,px,py,pz,p0,pmass,vtime,vspin
          ENDDO
          IF(reweight_scale)THEN
             iij=0
             DO i=1,3
                iij=iij+1
                rwgt_values(iij)=wgtxsecmu(i)*XWGTUP
             ENDDO
             IF(iij.NE.tot_nrwgt)THEN
                WRITE(*,*)"ERROR:inconsistence for the reweigts #0 !"
                WRITE(*,*)"tot_nrwgt=",tot_nrwgt
                WRITE(*,*)"couting nrwgt=",iij
                STOP
             ENDIF
             WRITE(nunit30)(rwgt_values(i),i=1,tot_nrwgt)
          ENDIF
       ENDIF
    ENDIF
    Nevents=nwri
  END SUBROUTINE unwei_procedure_LbL

  SUBROUTINE Generate_lhe_LbL(nevent_posw,nevent_negw,icase)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nevent_posw,nevent_negw,icase
    INTEGER::nevent
    CHARACTER(len=24),PARAMETER::tmp_dir="./tmp/",output_dir="./output/"
    INTEGER::i,nunit4,nunit5
    INTEGER::irwgt,irwgt2
    INTEGER::istop,k,i2,jj2
    INTEGER,DIMENSION(:),ALLOCATABLE::rwgt_ids
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::rwgt_vals
    REAL(KIND(1d0)),DIMENSION(3)::Rscaleupdown
    REAL(KIND(1d0))::p0,px,py,pz,SPINUP,EBMUP1,EBMUP2,XSECUP1,XSECUP2,XERRUP1,XMAXUP1,&
         XWGTUP,VTIMUP,SCALUP,PM0,AQEDUP,AQCDUP,XWGTUP2
    INTEGER::IDBMUP1,IDBMUP2,IDWTUP,NPRUP,IDPRUP,NUP,IDUP,ISTUP,IMOTHUP1,IMOTHUP2,&
         ICOLUP1,ICOLUP2,iPDFGUP1,iPDFGUP2,iPDFSUP1,iPDFSUP2,LPRUP1
    REAL(KIND(1d0))::xxwgt_old
    nunit3=30
    CLOSE(nunit3)
    OPEN(nunit3,FILE=TRIM(tmp_dir)//'even_LbL.out',FORM='unformatted')
    nunit4=31
    CLOSE(nunit4)
    OPEN(nunit4,FILE=TRIM(tmp_dir)//'sample_LbL.init')
    nunit5=32
    CLOSE(nunit5)
    OPEN(nunit5,FILE=TRIM(output_dir)//'sample_LbLatNLO.lhe')
    IF(reweight_scale)THEN
       nunit30=300
       CLOSE(nunit30)
       OPEN(nunit30,FILE=TRIM(tmp_dir)//'even_rwgt_LbL.out',FORM='unformatted')
    ENDIF
    WRITE(nunit5,'(A)') '<LesHouchesEvents version="3.0">'
    WRITE(nunit5,'(A)') '<!--'
    WRITE(nunit5,'(A)') 'File generated with LbLatNLO (2312.16956,2312.16966)'
    WRITE(nunit5,'(A)') '-->'
    IF(reweight_scale)THEN
       irwgt=0
       irwgt=irwgt+3
       IF(ALLOCATED(rwgt_ids))THEN
          DEALLOCATE(rwgt_ids)
       ENDIF
       ALLOCATE(rwgt_ids(irwgt))
       IF(ALLOCATED(rwgt_vals))THEN
          DEALLOCATE(rwgt_vals)
       ENDIF
       ALLOCATE(rwgt_vals(irwgt))

       WRITE(nunit5,'(A)') '  <header>'
       WRITE(nunit5,'(A)') '  <initrwgt>'
       irwgt2=0
       WRITE(nunit5,'(A)') "    <weightgroup type='scale_variation' combine='envelope'>"
       Rscaleupdown(1)=1d0
       Rscaleupdown(2)=rw_RScale_up
       Rscaleupdown(3)=rw_Rscale_down
       DO i=1,3
          k=k+1
          irwgt2=irwgt2+1
          rwgt_ids(irwgt2)=k
          WRITE(nunit5,6100)k,Rscaleupdown(i)
       ENDDO
       WRITE(nunit5,'(A)') "    </weightgroup>"
       WRITE(nunit5,'(A)') '  </initrwgt>'
       WRITE(nunit5,'(A)') '  </header>'
       IF(irwgt2.NE.irwgt)THEN
          WRITE(*,*)"ERROR:inconsistece of the number of reweights"
          WRITE(*,*)"irwgt=",irwgt
          WRITE(*,*)"irwgt2=",irwgt2
          STOP
       ENDIF
    ENDIF
    WRITE(nunit5,'(A)') '<init>'
    READ(nunit4,*)IDBMUP1,IDBMUP2,EBMUP1,EBMUP2,iPDFGUP1,iPDFGUP2,iPDFSUP1,iPDFSUP2,IDWTUP,NPRUP
    READ(nunit4,*)XSECUP1,XERRUP1,XMAXUP1,LPRUP1
    WRITE(nunit5,5000)IDBMUP1,IDBMUP2,EBMUP1,EBMUP2,iPDFGUP1,iPDFGUP2,iPDFSUP1,iPDFSUP2,IDWTUP,NPRUP
    WRITE(nunit5,5100)XSECUP1,XERRUP1,XMAXUP1,LPRUP1
    WRITE(nunit5,'(A)') '</init>'
    istop=1
    k=0
    XWGTUP2=XSECUP1/(nevent_posw-nevent_negw)
    DO WHILE(istop.EQ.1)
       k=k+1
       READ(nunit3,END=100) NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP
       IF(icase.EQ.1)THEN
          XWGTUP=SIGN(1d0,XWGTUP)*XWGTUP2
       ENDIF
       WRITE(nunit5,'(A)') '<event>'
       WRITE(nunit5,5200) NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP
       DO i=1,NUP
          READ(nunit3)IDUP,ISTUP,iMOTHUP1,iMOTHUP2,ICOLUP1,ICOLUP2,px,py,pz,p0,pm0,VTIMUP,SPINUP
          WRITE(nunit5,5300)IDUP,ISTUP,iMOTHUP1,iMOTHUP2,ICOLUP1,ICOLUP2,px,py,pz,p0,pm0,VTIMUP,SPINUP
       ENDDO
       IF(irwgt.GT.0.AND.reweight_scale)THEN
          ! filling the rwgt
          WRITE(nunit5,'(A)') '  <rwgt>'
          READ(nunit30)(rwgt_vals(i),i=1,irwgt)
          IF(icase.EQ.1)THEN
             xxwgt_old=rwgt_vals(1)
             IF(xxwgt_old.EQ.0d0)THEN
                xxwgt_old=1d0
             ENDIF
             DO i=1,irwgt
                rwgt_vals(i)=rwgt_vals(i)*XWGTUP2*SIGN(1d0,xxwgt_old)/xxwgt_old
             ENDDO
          ENDIF
          DO i=1,irwgt
             WRITE(nunit5,6300)rwgt_ids(i),rwgt_vals(i)
          ENDDO
          WRITE(nunit5,'(A)') '  </rwgt>'
       ENDIF
       WRITE(nunit5,'(A)') '</event>'
    ENDDO
100 CONTINUE
    nevent=nevent_posw+nevent_negw
    IF(icase.EQ.1.AND.k.NE.nevent+1)THEN
       WRITE(*,*)"WARNING:mismatching of the unweighted lhe events number ",k-1,nevent
    ENDIF
    WRITE(nunit5,'(A)') '</LesHouchesEvents>'
    CLOSE(nunit3,STATUS='delete')
    CLOSE(nunit4,STATUS='delete')
    IF(reweight_scale)THEN
       CLOSE(nunit30,STATUS='delete')
    ENDIF
5200 FORMAT(1P,2I6,4E14.6)
5300 FORMAT(1P,I8,5I5,5E18.10,E14.6,E12.4)
5000 FORMAT(1P,2I8,2E14.6,6I8)
5100 FORMAT(1P,3E20.10,I6)
    CLOSE(nunit5,STATUS='keep')
6100 FORMAT(6x,"<weight id='",I4,"'>",1x,&
          "muR=",E11.5,1x,"</weight>")
6300 FORMAT(4x,"<wgt id='",I4,"'>",1x,&
          E11.5,1x,"</wgt>")
  END SUBROUTINE Generate_lhe_LbL

  SUBROUTINE GET_ntau(tau,ntau)
    ! get ntau for a given tau
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::tau
    REAL(KIND(1d0)),INTENT(OUT)::ntau
    REAL(KIND(1d0))::x
    x=DLOG10(tau)
    if(x.GT.0d0)then
       write(*,*)"ERROR: tau > 1"
       stop
    endif
    IF(colpar.eq.1)then
       if(nuclearZ1.EQ.82.and.nuclearZ2.EQ.82)then
          ! Pb+Pb
          if(x.LE.-7.3d0)then
             ntau=-1.67d0
          else
             ntau=-94.3051532633065d0-73.44040922736761d0*x-24.20040079794743d0*x**2-&
                  4.078561946253477d0*x**3-0.3478111311655984d0*x**4-0.011926801483889686d0*x**5
          endif
          ntau=-ntau
       elseif(nuclearZ1.GE.54.and.nuclearZ2.GE.54)then
          ! Xe+Xe or other two heavier ions
          if(x.LE.-7.3d0)then
             ntau=-1.65d0
          else
             ntau=-80.40283638414304d0-61.87697086384057d0*x-20.190330720309685d0*x**2-&
                  3.3649524772624226d0*x**3-0.2833770322998644d0*x**4-0.009585167381630894d0*x**5
          endif
          ntau=-ntau
       elseif(nuclearZ1.GE.36.and.nuclearZ2.GE.36)then
          ! Kr+Kr or other two heavier ions
          if(x.LE.-7.6d0)then
             ntau=-1.56747d0
          else
             ntau=-62.53889073061811d0-47.055403812306324d0*x-15.143875760073954d0*x**2-&
                  2.493885793678072d0*x**3-0.2077103390435214d0*x**4-0.006952069793456002d0*x**5
          endif
          ntau=-ntau
       elseif(nuclearZ1.EQ.20.and.nuclearZ2.EQ.20)then
          ! Ca+Ca
          if(x.LE.-7.6d0)then
             ntau=-1.5586d0
          else
             ntau=-54.557802450791044d0-41.00008257855877d0*x-13.247561234064998d0*x**2-&
                  2.1906729816860953d0*x**3-0.18310288951474804d0*x**4-0.006144522758036254d0*x**5
          endif
          ntau=-ntau
       elseif(nuclearZ1.EQ.18.and.nuclearZ2.EQ.18)then
          ! Ar+Ar
          if(x.LE.-7.6d0)then
             ntau=-1.54506d0
          else
             ntau=-56.73289386251643d0-43.29067661237275d0*x-14.192594861897161d0*x**2-&
                  2.3816880463718246d0*x**3-0.20203012870909018d0*x**4-0.0068804496003979365d0*x**5
          endif
          ntau=-ntau
       elseif(nuclearZ1.GE.8.and.nuclearZ2.GE.8)then
          ! O+O or other two heavier ions
          if(x.LE.-7.6d0)then
             ntau=-1.53397d0
          else
             ntau=-39.30707034802285d0-28.904825168893716d0*x-9.308769766515642d0*x**2-&
                  1.5405970204691584d0*x**3-0.12911565048937673d0*x**4-0.004347965155734601d0*x**5
          endif
          ntau=-ntau
       elseif((nuclearZ1.EQ.0.and.nuclearZ2.GE.54)&
            .or.(nuclearZ2.EQ.0.and.nuclearZ1.GE.54))then
          ! p+Pb or Pb+p or a heavy ion+p
          if(x.LE.-7.8d0)then
             ntau=-1.49d0
          else
             ntau=-20.85512449173276d0-13.434567465290675d0*x-4.010519989747026d0*x**2-&
                  0.6244230265646026d0*x**3-0.04965283409587662d0*x**4-0.0015947318297498464d0*x**5
          endif
          ntau=-ntau
       elseif(nuclearA1.EQ.0.and.nuclearA2.EQ.0.and.&
            nuclearZ1.EQ.0.and.nuclearZ2.EQ.0)then
          ! pp
          if(x.LE.-8.2d0)then
             ntau=-1.39d0
          else
             ntau=-8.050931677173754d0-3.963308342683446d0*x-1.0494503104856845d0*x**2-&
                  0.1481666911671101d0*x**3-0.010796574134484576d0*x**4-0.0003194433638012126d0*x**5
          endif
          ntau=-ntau
       else
          WRITE(*,*)"WARNING: do not implement ntau for (Z1,Z2)=",nuclearZ1,nuclearZ2
          WRITE(*,*)"WARNING: will simply use ntau=2"
          ntau=2d0
       endif
       return
    elseif(colpar.eq.2)then
       ! e+p collisions
       ntau=2d0
       if(x.LE.-5.6d0)then
          ntau=-1.45662d0
       else
          ntau=-32.264482248394486d0-102.18795453870347d0*x-174.36487147984568d0*x**2-&
               180.61250551307256d0*x**3-121.34418842063245d0*x**4-54.551887771742415d0*x**5-&
               16.542376180975708d0*x**6-3.336791516816648d0*x**7-0.4285875710923549d0*x**8-&
               0.03168449402584781d0*x**9-0.001025143571009115d0*x**10
       endif
       ntau=-ntau
       ntau=MAX(ntau,1.01d0)
    elseif(colpar.eq.3)then
       ! e+e-
       if(x.LE.-3.9d0)then
          ntau=-1.46482d0
       else
          ntau=-24930.214875089572d0-28641.899212638815d0*x-5274.155833095974d0*x**2-&
               2283.3990257961455d0*x**3-998.9639559499716d0*x**4-351.0699145262098d0*x**5-&
               89.33462851169318d0*x**6-15.136722123477405d0*x**7-1.5133829561232466d0*x**8-&
               0.06718609263939923d0*x**9-22260.339669715704d0*DLog(-x)-&
               9161.961548713729d0*DLog(-x)**2-2202.9674489393824d0*DLog(-x)**3-&
               321.82005540583435d0*DLog(-x)**4-26.551577150222602d0*DLog(-x)**5-&
               0.9468751293556315d0*DLog(-x)**6
       endif
       ntau=-ntau
    elseif(colpar.eq.4)then
       ntau=2d0
    else
       WRITE(*,*)"WARNING: do not implement ntau for colpar=",colpar
       WRITE(*,*)"WARNING: will simply use ntau=2"
       ntau=2d0
    endif
    return
  END SUBROUTINE GET_ntau

END MODULE phase_space_integrators
