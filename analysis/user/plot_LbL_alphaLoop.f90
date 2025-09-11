MODULE plot_LbL
  USE LbL_Global
  USE kinetics
  IMPLICIT NONE
  INTEGER::max_weight=1
  INTEGER::nwgt_analysis
  INTEGER::num_plots=16 ! num_plots=inum_plots*2
  INTEGER::inum_plots=8
  INTEGER,PARAMETER::n_width=1,n_height=1
CONTAINS
  SUBROUTINE initplot_LbL
    IMPLICIT NONE
    INTEGER::nwgt
    CHARACTER(len=15),DIMENSION(:),ALLOCATABLE::weights_info
    INTEGER::i
    max_weight=1+ho_nscale
    IF(ALLOCATED(weights_info))THEN
       DEALLOCATE(weights_info)
    ENDIF
    ALLOCATE(weights_info(max_weight))
    nwgt=1
    weights_info(nwgt)="central value  "
    IF(reweight_scale)THEN
       nwgt=nwgt+ho_nscale
       IF(ho_nscale.NE.2)THEN
          WRITE(*,*) 'ERROR #1 in initplot_LbL:',ho_nscale
          STOP
       ENDIF
       WRITE(weights_info(nwgt-1),'(a4,f3.1,a2)')&
            "muR=",rw_Rscale_up
       WRITE(weights_info(nwgt  ),'(a4,f3.1,a2)')&
            "muR=",rw_Rscale_down
    END IF
    ! output plot files
    CALL plot_begin_LbL(nwgt,max_weight,weights_info)
    RETURN
  END SUBROUTINE initplot_LbL

  SUBROUTINE plotout_LbL
    USE MC_VEGAS
    IMPLICIT NONE
    LOGICAL::usexinteg=.FALSE.,mint=.FALSE. ! I have not implemented MINT 
    LOGICAL::useitmax=.TRUE.
    REAL(KIND(1d0))::xnorm
    xnorm=1.d0
    xnorm=xnorm/float(itmx) ! use vegas with itmax
    ! output plot files
    CALL plot_end_LbL(xnorm)
    RETURN
  END SUBROUTINE plotout_LbL

  SUBROUTINE outfun_LbL(www)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::www
    INTEGER::i,j
    INTEGER::nwgt
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::wgts !,wgtden
    INTEGER::init=0
    SAVE init,wgts
    IF(www.EQ.0d0)RETURN
    IF(init.EQ.0)THEN
       max_weight=1+ho_nscale
       IF(ALLOCATED(wgts))THEN
          DEALLOCATE(wgts)
       ENDIF
       ALLOCATE(wgts(max_weight))
       init=1
    ENDIF
    nwgt=1
    wgts(1)=www
    IF(reweight_scale)THEN
       DO i=2,3
          nwgt=nwgt+1
          wgts(nwgt)=www*wgtxsecmu(i)
       ENDDO
    ENDIF
    ! output plot file 
    CALL plot_fill_LbL(wgts)
    RETURN
  END SUBROUTINE outfun_LbL

  SUBROUTINE plot_begin_LbL(nwgt,max_weight0,weights_info)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nwgt,max_weight0
    CHARACTER(len=*),DIMENSION(max_weight0),INTENT(IN)::weights_info
    INTEGER::i,j,kk,l
    REAL(KIND(1d0)),PARAMETER::PI=3.14159265358979312D0
    ! ATLAS means the cut in 2008.05355
    character(len=5)::cc(2)=(/'     ','ATLAS'/)
    INCLUDE '../hbook/dbookf90.inc'
    INCLUDE '../hbook/histogram_common90.inc'
    top_yes=topdrawer_output
    gnu_yes=gnuplot_output
    root_yes=root_output
    hwu_yes=hwu_output
    CALL HISTOGRAM_INIT(nwgt,weights_info)
    nwgt_analysis=nwgt
    IF(nwgt_analysis*16.GT.nplots/4)THEN
       WRITE(*,*) 'error in plot_begin_LbL: ',&
            nwgt_analysis*num_plots*4 ! 4 is internal in dbook
       STOP
    ENDIF
    DO i=1,2
       l=(i-1)*inum_plots
       CALL histogram_book(l+ 1,'total rate '//cc(i),&
            2,0d0,4d0,.FALSE.)
       CALL histogram_book(l+ 2,'total rate, 3.45<m(aa)<3.65 '//cc(i),&
            2,0d0,4d0,.TRUE.)
       CALL histogram_book(l+ 3,'m(aa) '//cc(i),&
            100,0d0,50d0,.TRUE.)
       CALL histogram_book(l+ 4,'m(aa) zoom in '//cc(i),&
            100,2.8d0,3.8d0,.TRUE.)
       CALL histogram_book(l+ 5,'m(aa) zoom in 2 '//cc(i),&
            40,2.8d0,4d0,.TRUE.)
       CALL histogram_book(l+ 6,'Abs(y(aa)) '//cc(i),&
            100,0d0,5d0,.TRUE.)
       CALL histogram_book(l+ 7,'pT(a) avg '//cc(i),&
            100,5d0,500d0,.TRUE.)
       CALL histogram_book(l+ 8,'cos(theta) '//cc(i),&
            100,-1d0,1d0,.TRUE.)
    ENDDO

    CALL HISTOGRAM_BOOK_FINISH
    RETURN
  END SUBROUTINE plot_begin_LbL

  SUBROUTINE plot_end_LbL(xnorm)
    IMPLICIT NONE
    CHARACTER(len=14)::ytit
    REAL(KIND(1d0)),INTENT(IN)::xnorm
    INTEGER::i
    INTEGER::kk,l
    INCLUDE '../hbook/dbookf90.inc'
    CHARACTER(len=400)::psfile,rootfile
    IF(topdrawer_output.OR.gnuplot_output.OR.root_output)THEN
       CALL mclear
       CALL mclear_3d
       DO i=1,NPLOTS
          CALL mopera(i,'+',i,i,xnorm,0.d0)
          CALL mfinal(i)
          CALL mopera_3d(i,'+',i,i,xnorm,0.d0)
          CALL mfinal_3d(i)
       ENDDO
    ENDIF
    ytit='sigma per bin '
    IF(topdrawer_output)THEN
       CALL open_topdrawer_file_LbL
       CALL HISTOGRAM_END(1,ytit,' ',' ')
       CALL close_topdrawer_file_LbL
    ENDIF
    IF(gnuplot_output)THEN
       CALL open_gnuplot_file_LbL
       psfile="LbLatNLO.ps"
       CALL HISTOGRAM_END(2,ytit,psfile,' ')
       CALL close_gnuplot_file_LbL
    ENDIF
    IF(root_output)THEN
       CALL open_root_file_LbL
       rootfile="LbLatNLO.root"
       CALL HISTOGRAM_END(3,ytit,' ',rootfile)
       ! wirte the end of the .C file for root
       WRITE (96, 100)
100    FORMAT(/1X,&
            ' hohisto -> cd();',/1X,&
            ' if (histos -> GetEntries() > 0 ) then {',/1X,&
            '  histos->Write();',/1X,&
            '  hohisto -> Close();',/1X,&
            ' }',/1X,'}')
       CALL close_root_file_LbL
    ENDIF
    IF(hwu_output)THEN
       CALL HISTOGRAM_END(4,ytit,' ',' ')
       CALL HwU_write_file_LbL
    ENDIF
    RETURN
  END SUBROUTINE plot_end_LbL

  SUBROUTINE plot_fill_LbL(wgts)
    IMPLICIT NONE
    REAL(KIND(1d0)),DIMENSION(:),INTENT(IN)::wgts
    REAL(KIND(1d0))::var,Q,www
    REAL(KIND(1d0))::exp1,exp2,maa,yaa,etaa1,etaa2,pta1,pta2,pTavg
    REAL(KIND(1d0))::ya1,ya2,costh
    INTEGER::i,m,l,kk,numFa,numIa
    REAL(KIND(1d0)),DIMENSION(2,0:3)::xpI_a,xpF_a
    REAL(KIND(1d0)),DIMENSION(0:3)::xpF_aa
    REAL(KIND(1d0)),PARAMETER::PI=3.14159265358979312D0
    LOGICAL,DIMENSION(2)::pass_cuts
    IF(wgts(1).EQ.0d0)RETURN
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! DEFINE OBSERVABLES
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    var=1.0d0 ! total cross section
    xpF_a(1,0:3)=LbL_pmom(3,0:3)
    xpF_a(2,0:3)=LbL_pmom(4,0:3)
    xpF_aa(0:3)=LbL_pmom(3,0:3)+LbL_pmom(4,0:3)
    maa=DSQRT(xpF_aa(0)**2-xpF_aa(1)**2-xpF_aa(2)**2-xpF_aa(3)**2)
    yaa=rapidity(xpF_aa)
    yaa=DABS(yaa)
    pass_cuts(1)=.TRUE.
    pta1=DSQRT(xpF_a(1,1)**2+xpF_a(1,2)**2)
    pta2=DSQRT(xpF_a(2,1)**2+xpF_a(2,2)**2)
    pTavg=(pta1+pta2)/2d0
    etaa1=prapidity(xpF_a(1,0:3))
    etaa2=prapidity(xpF_a(2,0:3))
    ya1=rapidity(xpF_a(1,0:3))
    ya2=rapidity(xpF_a(2,0:3))
    ! In fact, |cos(theta*)| = |tanh((y1-y2)/2)|
    ! theta* is the scattering angle in the rest frame of the initial partonic scattering
    costh=DTANH((ya1-ya2)/2d0)
    !costh=DABS(costh)
    ! ATLAS cuts (arXiv:2008.05355)
    ! for this 2 -> 2 process
    ! I do not impose pT(aa) cut and (1-|D phi(aa)|/pi)<0.01 cut
    pass_cuts(2)=pta1.GT.2.5d0.AND.pta2.GT.2.5d0.AND.DABS(etaa1).LT.2.37d0&
         .AND.DABS(etaa2).LT.2.37d0.AND.maa.GT.5d0
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! START TO FILL HISTOGRAM
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    DO i=1,2
       IF(.NOT.pass_cuts(i))CYCLE
       l=(i-1)*inum_plots
       CALL HISTOGRAM_fill(l+1,var,wgts)
       IF(maa.GE.3.45d0.AND.maa.LE.3.65d0)THEN
          CALL HISTOGRAM_fill(l+2,var,wgts)
       ENDIF
       CALL HISTOGRAM_fill(l+3,maa,wgts)
       CALL HISTOGRAM_fill(l+4,maa,wgts)
       CALL HISTOGRAM_fill(l+5,maa,wgts)
       CALL HISTOGRAM_fill(l+6,yaa,wgts)
       CALL HISTOGRAM_fill(l+7,pTavg,wgts)
       CALL HISTOGRAM_fill(l+8,costh,wgts)
    ENDDO
    IF(hwu_output)THEN
       call HwU_add_points
    ENDIF
    ! boost back to c.m. frame
    RETURN
  END SUBROUTINE plot_fill_LbL

  SUBROUTINE GetPTOrdered(npt,pt_ordered)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::npt
    REAL(KIND(1d0)),DIMENSION(npt),INTENT(INOUT)::pt_ordered
    INTEGER::i,j
    REAL(KIND(1d0)),DIMENSION(npt)::pt_orig
    LOGICAL,DIMENSION(npt)::used
    REAL(KIND(1d0))::ptmax
    INTEGER::indexmax
    pt_orig(1:npt)=pt_ordered(1:npt)
    used(1:npt)=.FALSE.
    DO i=1,npt
       ptmax=-1d0
       indexmax=0
       DO j=1,npt
          IF(used(j))CYCLE
          IF(pt_orig(j).GT.ptmax)THEN
             ptmax=pt_orig(j)
             indexmax=j
          ENDIF
       ENDDO
       pt_ordered(i)=ptmax
       used(indexmax)=.TRUE.
    ENDDO
    RETURN
  END SUBROUTINE GetPTOrdered

  FUNCTION DeltaPh4(pmom1,pmom2)
    IMPLICIT NONE
    REAL(KIND(1d0))::DeltaPh4
    REAL(KIND(1d0)),DIMENSION(4),INTENT(IN)::pmom1,pmom2 ! px,py,pz,E
    REAL(KIND(1d0)),PARAMETER::pi=3.1415926535897932384626433832795d0
    DeltaPh4=ph4(pmom1(1),pmom1(2),pmom1(3)) ! ph4 is defined in Kinematic_Func.f90
    DeltaPh4=DeltaPh4-ph4(pmom2(1),pmom2(2),pmom2(3))
    IF(DeltaPh4.GT.pi)DeltaPh4=2d0*pi-DeltaPh4
    RETURN
  END FUNCTION DeltaPh4

  SUBROUTINE HwU_write_file_LbL
    IMPLICIT NONE
    REAL(KIND(1d0))::xnorm
    OPEN(unit=599,file=TRIM(output_dir)//'LbLatNLO.HwU',&
         status='unknown')
    xnorm=1d0
    CALL HwU_output(599,xnorm)
    CLOSE(599)
    RETURN
  END SUBROUTINE HwU_write_file_LbL
  
  SUBROUTINE open_topdrawer_file_LbL
    IMPLICIT NONE
    LOGICAL::useitmax
    OPEN(unit=99,FILE=TRIM(output_dir)//'LbLatNLO.top',&
         status='unknown')
    useitmax=.FALSE.
  END SUBROUTINE open_topdrawer_file_LbL

  SUBROUTINE close_topdrawer_file_LbL
    IMPLICIT NONE
    CLOSE(99)
    RETURN
  END SUBROUTINE close_topdrawer_file_LbL

  SUBROUTINE open_gnuplot_file_LbL
    IMPLICIT NONE
    LOGICAL::useitmax
    OPEN(unit=97,FILE=TRIM(output_dir)//'LbLatNLO.gnu',&
         status='unknown')
    useitmax=.FALSE.
  END SUBROUTINE open_gnuplot_file_LbL

  SUBROUTINE close_gnuplot_file_LbL
    IMPLICIT NONE
    CLOSE(97)
    RETURN
  END SUBROUTINE close_gnuplot_file_LbL

  SUBROUTINE open_root_file_LbL
    USE LbL_Global
    IMPLICIT NONE
    LOGICAL::useitmax
    OPEN(unit=96,FILE=TRIM(output_dir)//'LbLatNLO.C',&
         status='unknown')
    useitmax=.FALSE.
  END SUBROUTINE open_root_file_LbL

  SUBROUTINE close_root_file_LbL
    IMPLICIT NONE
    CLOSE(96)
    RETURN
  END SUBROUTINE close_root_file_LbL
END MODULE plot_LbL
