MODULE LbL_Global
  IMPLICIT NONE
  ! beam configurations
  ! colliding particles
  INTEGER::colpar=1
  REAL(KIND(1d0))::energy_beam1=7000d0
  REAL(KIND(1d0))::energy_beam2=7000d0
  REAL(KIND(1d0)),DIMENSION(2)::ebeam0
  INTEGER::nuclearA1=0
  INTEGER::nuclearA2=0
  INTEGER::nuclearZ1=0
  INTEGER::nuclearZ2=0
  INTEGER::UPC_photon_flux=1 ! only colpar=1
  REAL(KIND(1d0)),DIMENSION(2)::LbL_RA,LbL_aA,LbL_wA
  ! order
  INTEGER::order=-3
  ! masses (negative means we will not include such a contribution)
  REAL(KIND(1d0))::emass=0d0
  REAL(KIND(1d0))::mumass=0d0
  REAL(KIND(1d0))::taumass=1.77686d0
  REAL(KIND(1d0))::umass=0d0
  REAL(KIND(1d0))::dmass=0d0
  REAL(KIND(1d0))::smass=0d0
  REAL(KIND(1d0))::cmass=1.5d0
  REAL(KIND(1d0))::bmass=4.75d0
  REAL(KIND(1d0))::tmass=172.69d0
  REAL(KIND(1d0))::wmass=80.377d0
  REAL(KIND(1d0)),PARAMETER::emass_PDG=0.51099895d-3
  REAL(KIND(1d0)),PARAMETER::mumass_PDG=0.1056583755d0
  REAL(KIND(1d0)),PARAMETER::taumass_PDG=1.77686d0
  REAL(KIND(1d0)),PARAMETER::umass_PDG=41d-3
  REAL(KIND(1d0)),PARAMETER::dmass_PDG=41d-3
  REAL(KIND(1d0)),PARAMETER::smass_PDG=0.150d0
  REAL(KIND(1d0)),PARAMETER::cmass_PDG=1.5d0
  REAL(KIND(1d0)),PARAMETER::bmass_PDG=4.5d0
  REAL(KIND(1d0)),PARAMETER::tmass_PDG=172.69d0
  REAL(KIND(1d0)),PARAMETER::wmass_PDG=80.377d0
  REAL(KIND(1d0)),PARAMETER::zmass_PDG=91.1876d0
  ! couplings
  REAL(KIND(1d0))::alphaemm1=137.036d0
  REAL(KIND(1d0))::alphasMZ=0.118d0
  INTEGER::alphas_nloop=2
  REAL(KIND(1d0))::Gfermi=1.1663787d-5
  REAL(KIND(1d0))::alphaMZm1=127.955d0
  REAL(KIND(1d0))::DalphahadMZ_PDG=0.02766d0 ! 5 flavour number scheme
                                             ! uncertainty is +-0.00007 
                                             ! (page 4 in https://pdg.lbl.gov/2020/reviews/rpp2020-rev-standard-model.pdf)
  INTEGER::alpha_scheme=0
  INTEGER::scale=0
  REAL(KIND(1d0))::fscalevalue=10d0
  REAL(KIND(1d0))::muR_over_ref=1d0
  REAL(KIND(1d0))::rw_RScale_down=0.5d0
  REAL(KIND(1d0))::rw_RScale_up=2.0d0
  LOGICAL::reweight_Scale=.FALSE.
  ! kinematic cuts in the lab frame
  REAL(KIND(1d0))::minptp=0d0
  REAL(KIND(1d0))::maxptp=-1d0
  REAL(KIND(1d0))::maxyrapp=2.4d0
  REAL(KIND(1d0))::minyrapp=-2.4d0
  REAL(KIND(1d0))::minmpp=4d0
  REAL(KIND(1d0))::maxmpp=-1d0
  REAL(KIND(1d0))::maxptpp=-1d0
  REAL(KIND(1d0))::maxaco=-1d0
  ! event generation
  INTEGER::gener=0
  LOGICAL::unwgt=.FALSE.
  LOGICAL::lhewgtup=.TRUE.
  INTEGER::nmc=100000
  ! number of lhe events
  INTEGER::Nevents=0,Nevents_posw=0,Nevents_negw=0
  REAL(KIND(1d0))::aqedup=-1d0,aqcdup=-1d0
  ! histogram output
  LOGICAL::topdrawer_output=.FALSE.
  LOGICAL::gnuplot_output=.TRUE.
  LOGICAL::root_output=.FALSE.
  LOGICAL::hwu_output=.TRUE.
  LOGICAL::plot_output=.FALSE.
  ! four momenta of external legs (after kt smearing)
  ! this is 2 > 2 scattering
  REAL(KIND(1d0)),DIMENSION(4,0:3)::LbL_pmom
  ! four momenta of external legs in collinear factorisation
  REAL(KIND(1d0)),DIMENSION(4,0:3)::LbL_pmom_CF
  LOGICAL::intrinsic_kt=.FALSE.
  INTEGER::ion_Form=1 ! Form1: Q**2=kT**2+(mn*x)**2, Qmin**2=(mn*x)**2;
                      ! Form2: Q**2=kT**2/(1-x)+(mn*x)**2/(1-x), Qmin**2=(mn*x)**2/(1-x) [more exact]
  INTEGER::ho_nscale=0 ! number of scales
  INTEGER::tot_nrwgt=0
  ! scale reweight stuff
  REAL(KIND(1d0)),DIMENSION(3)::rw_Rscales
  REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::rwgt_values
  REAL(KIND(1d0)),DIMENSION(3)::wgtxsecmu
  ! input files
  CHARACTER(len=20),PARAMETER::Input_File="run.inp"
  CHARACTER(len=24),PARAMETER::input_dir="./input/"
  CHARACTER(len=24),PARAMETER::tmp_dir="./tmp/"
  CHARACTER(len=24),PARAMETER::output_dir="./output/"
  CHARACTER(len=24),PARAMETER::grid_dir="./grid/"
  LOGICAL::labeqcoll
  ! For phase space parametrisation
  ! assuming the flux is tau**(-ntau+1)
  ! or dL/dW is tau**(-ntau)
  ! where tau=x1*x2
  REAL(KIND(1d0))::xp1,xp2
  REAL(KIND(1d0))::ntau=2d0
  REAL(KIND(1d0))::EBMUP1,EBMUP2
  INTEGER::iPDFSUP1,iPDFSUP2,iPDFGUP1,iPDFGUP2
  ! q2max is for iww
  REAL(KIND(1d0))::q2max=1d0
  INTEGER::nunit1=6,nunit2,nunit3,nunit30
  ! grid_gen
  ! 0: check whether there is a grid. If yes, just use the grid.
  !    Otherwise, generate a grid first and write to the disk and then use it.
  ! 1: force to generate a grid and write to the disk and then use it
  ! 2: just evaluate it directly without using the grid
  INTEGER::grid_gen=0
  ! improve_w_LE
  ! -1: We need to use the low-energy expansion results to improve the massive amplitudes
  !  0: We do not use the low-energy expansion results to improve the massive amplitudes
  !  1: We simply use the low-energy expansion results for the massive amplitudes
  INTEGER::improve_w_LE=-1
  ! gamma-UPC options
  ! whether or not to use MC-Glauber TAA for the survival probability
  ! when colpar=1 and UPC_photon_flux=1 or 2.
  !LOGICAL::Survival_MC_Glauber=.FALSE.
  ! use double or quadruple precision for two-loop amp
  LOGICAL::use_2L_DP=.FALSE.
  LOGICAL::use_2L_QP=.FALSE.
  ! 1: use trapezoid
  ! 2: use double exponential quadrature
  INTEGER::grid_integration_method=2
END MODULE LbL_Global
