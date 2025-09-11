FC=
objdir=$(PWD)/obj/
moddir=$(PWD)/mod/
srcdir=$(PWD)/src/
libdir=$(PWD)/lib/
plotdir=$(PWD)/analysis/user/
gammaupcdir=$(PWD)/vendor/gammaUPC/
handyGdummyobj=
handyGdummymod=
# uncomment the following line to use gdb to debug
# FFLAGS=-ggdb -g -fbounds-check -ffpe-trap=invalid,zero,overflow,underflow,denormal -Wall
objs=$(objdir)global_constants.o $(objdir)qcd_constants.o $(objdir)qcd_setup.o $(objdir)qcd_coupling.o $(objdir)assertions.o $(objdir)runge_kutta.o $(objdir)newton_method.o $(objdir)coupling_global.o $(objdir)LbL_Global.o $(objdir)evaluate_couplings.o $(objdir)LbL_setscale.o $(objdir)LbL_cuts.o $(objdir)MC_VEGAS.o $(objdir)Constants.o $(objdir)Integration_Paths.o $(objdir)use_func.o $(objdir)Func_PSI.o $(objdir)interpolation.o $(objdir)nielsen_generalized_polylog.o $(objdir)nielsen_generalized_polylog_wrapper.o $(objdir)nintlib.o $(objdir)simple_integrators.o $(objdir)UToneloopbasis.o $(objdir)oneloopints.o $(objdir)express_wz.o $(handyGdummyobj) $(objdir)GPL_wrapper.o $(objdir)twoloopbasis.o $(objdir)kinetics.o $(objdir)loopbasis.o $(objdir)LOME.o $(objdir)calc_IterInts_GPLMIs.o $(objdir)calc_IterInts_RootMIs_boundaryconstants.o $(objdir)calc_IterInts_RootMIs.o $(objdir)OneLoopLowEnergyME.o $(objdir)TwoLoopLowEnergyME.o $(objdir)OneLoopHighEnergyME.o $(objdir)TwoLoopHighEnergyME.o $(objdir)TwoLoopMasslessME.o $(objdir)TwoLoopMassiveME.o $(objdir)NLOME.o $(objdir)plot_LbL.o $(objdir)photon_PDFs.o $(objdir)phase_space_integrators.o $(objdir)driver.o
mod = $(moddir)/*.mod
exe = $(PWD)/bin/LbLatNLO
linkexe = $(PWD)/LbLatNLO
EXLIBS =
INCLUDE =
FFLAGS+=$(INCLUDE) -I$(gammaupcdir) -fPIC -w -fno-automatic -ffixed-line-length-132 -std=legacy

all:    clean $(exe)
	ln -s $(exe) $(linkexe)

$(exe): makefile_hbook makefile_gammaUPC makefile_fastgpl $(objs)
	@cp *.o $(objdir)
	@cp *.mod $(moddir)
	@rm *.o *.mod
	$(FC) -o $(exe) $(objs) -L$(libdir) -lhbook -lgammaUPC $(EXLIBS) -lfastgplfortran

$(objdir)phase_space_integrators.o $(moddir)phase_space_integrators.mod: $(srcdir)phase_space_integrators.f90 $(moddir)LbL_Global.mod $(moddir)plot_LbL.mod $(moddir)Constants.mod $(moddir)LbL_setscale.mod $(moddir)qcd_constants.mod $(moddir)qcd_setup.mod $(moddir)qcd_coupling.mod $(moddir)coupling_global.mod $(moddir)nintlib.mod $(moddir)LbL_cuts.mod $(moddir)NLOME.mod $(moddir)MC_VEGAS.mod $(moddir)photon_PDFs.mod $(moddir)GPL_wrapper.mod $(gammaupcdir)elasticphotonphotonFlux.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)plot_LbL.o $(moddir)plot_LbL.mod: $(plotdir)plot_LbL.f90 $(moddir)LbL_Global.mod $(moddir)kinetics.mod $(moddir)MC_VEGAS.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)photon_PDFs.o $(moddir)photon_PDFs.mod: $(srcdir)photon_PDFs.f90 $(moddir)LbL_Global.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)LOME.o: $(srcdir)LOME.f $(moddir)kinetics.mod $(moddir)UToneloopbasis.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)NLOME.o $(moddir)NLOME.mod: $(srcdir)NLOME.f90 $(moddir)kinetics.mod $(moddir)UToneloopbasis.mod $(moddir)qcd_constants.mod $(moddir)evaluate_couplings.mod $(moddir)LbL_setscale.mod $(moddir)interpolation.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)calc_IterInts_GPLMIs.o $(moddir)calc_IterInts_GPLMIs.mod: $(srcdir)calc_IterInts_GPLMIs.f $(moddir)GPL_wrapper.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)calc_IterInts_RootMIs.o $(moddir)calc_IterInts_RootMIs.mod: $(srcdir)calc_IterInts_RootMIs.f $(moddir)calc_IterInts_RootMIs_boundaryconstants.mod $(moddir)simple_integrators.mod $(moddir)Func_PSI.mod $(moddir)Integration_Paths.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)calc_IterInts_RootMIs_boundaryconstants.o $(moddir)calc_IterInts_RootMIs_boundaryconstants.mod: $(srcdir)calc_IterInts_RootMIs_boundaryconstants.f $(moddir)GPL_wrapper.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)twoloopbasis.o: $(srcdir)twoloopbasis.f $(moddir)express_wz.mod $(moddir)kinetics.mod $(moddir)calc_IterInts_GPLMIs.mod $(moddir)calc_IterInts_RootMIs.mod $(moddir)GPL_wrapper.mod $(moddir)Func_PSI.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)Integration_Paths.o $(moddir)Integration_Paths.mod: $(srcdir)Integration_Paths.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)use_func.o $(moddir)use_func.mod: $(srcdir)use_func.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)Func_PSI.o $(moddir)Func_PSI.mod: $(srcdir)Func_PSI.f90 $(moddir)use_func.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)TwoLoopMassiveME.o: $(srcdir)TwoLoopMassiveME.f $(moddir)kinetics.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)TwoLoopMasslessME.o: $(srcdir)TwoLoopMasslessME.f $(moddir)nielsen_generalized_polylog_wrapper.mod $(moddir)kinetics.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)TwoLoopLowEnergyME.o: $(srcdir)TwoLoopLowEnergyME.f
	$(FC) $(FFLAGS) -c $<

$(objdir)OneLoopLowEnergyME.o: $(srcdir)OneLoopLowEnergyME.f
	$(FC) $(FFLAGS) -c $<

$(objdir)TwoLoopHighEnergyME.o: $(srcdir)TwoLoopHighEnergyME.f $(moddir)nielsen_generalized_polylog_wrapper.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)OneLoopHighEnergyME.o: $(srcdir)OneLoopHighEnergyME.f
	$(FC) $(FFLAGS) -c $<

$(objdir)loopbasis.o: $(srcdir)loopbasis.f
	$(FC) $(FFLAGS) -c $<

$(objdir)oneloopints.o $(moddir)oneloopints.mod: $(srcdir)oneloopints.f $(moddir)GPL_wrapper.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)UToneloopbasis.o $(moddir)UToneloopbasis.mod: $(srcdir)UToneloopbasis.f $(moddir)GPL_wrapper.mod $(moddir)express_wz.mod $(moddir)kinetics.mod
	$(FC) $(FFLAGS) -c $< 

$(objdir)kinetics.o $(moddir)kinetics.mod: $(srcdir)kinetics.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)UVloopbasis.o: $(srcdir)UVloopbasis.f $(moddir)oneloopints.mod $(moddir)express_wz.mod $(moddir)GPL_wrapper.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)simple_integrators.o $(moddir)simple_integrators.mod: $(srcdir)simple_integrators.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)nintlib.o $(moddir)nintlib.mod: $(gammaupcdir)nintlib.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)GPL_wrapper.o $(moddir)GPL_wrapper.mod: $(srcdir)GPL_wrapper.f90 $(handyGdummymod)
	$(FC) $(FFLAGS) -c $<

$(objdir)handyg_dummy.o $(moddir)handyg.mod: $(srcdir)handyg_dummy.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)interpolation.o $(moddir)interpolation.mod: $(gammaupcdir)interpolation.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)express_wz.o $(moddir)express_wz.mod: $(srcdir)express_wz.f90 $(moddir)interpolation.mod $(moddir)LbL_Global.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)nielsen_generalized_polylog.o $(moddir)nielsen_generalized_polylog.mod: $(gammaupcdir)nielsen_generalized_polylog.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)nielsen_generalized_polylog_wrapper.o $(moddir)nielsen_generalized_polylog_wrapper.mod: $(srcdir)nielsen_generalized_polylog_wrapper.f90 $(moddir)Func_PSI.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)Constants.o $(moddir)Constants.mod: $(srcdir)Constants.f90 $(moddir)LbL_Global.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)LbL_setscale.o $(moddir)LbL_setscale.mod: $(srcdir)LbL_setscale.f90 $(moddir)LbL_Global.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)LbL_cuts.o $(moddir)LbL_cuts.mod: $(srcdir)LbL_cuts.f90 $(moddir)LbL_Global.mod $(moddir)kinetics.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)LbL_Global.o $(moddir)LbL_Global.mod: $(srcdir)LbL_Global.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)coupling_global.o $(moddir)coupling_global.mod: $(srcdir)coupling_global.f90 $(moddir)qcd_coupling.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)evaluate_couplings.o $(moddir)evaluate_couplings.mod: $(srcdir)evaluate_couplings.f90 $(moddir)LbL_Global.mod $(moddir)global_constants.mod $(moddir)qcd_constants.mod $(moddir)coupling_global.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)global_constants.o $(moddir)global_constants.mod: $(srcdir)global_constants.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)qcd_constants.o $(moddir)qcd_constants.mod: $(srcdir)qcd_constants.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)qcd_setup.o $(moddir)qcd_setup.mod: $(srcdir)qcd_setup.f90 $(moddir)qcd_constants.mod $(moddir)global_constants.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)qcd_coupling.o $(moddir)qcd_coupling.mod: $(srcdir)qcd_coupling.f90 $(moddir)assertions.mod $(moddir)global_constants.mod $(moddir)qcd_constants.mod $(moddir)qcd_setup.mod $(moddir)runge_kutta.mod $(moddir)newton_method.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)assertions.o $(moddir)assertions.mod: $(srcdir)assertions.f90 $(moddir)global_constants.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)runge_kutta.o $(moddir)runge_kutta.mod: $(srcdir)runge_kutta.f90 $(moddir)global_constants.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)newton_method.o $(moddir)newton_method.mod: $(srcdir)newton_method.f90 $(moddir)global_constants.mod
	$(FC) $(FFLAGS) -c $<

$(objdir)MC_VEGAS.o $(moddir)MC_VEGAS.mod: $(srcdir)MC_VEGAS.f90
	$(FC) $(FFLAGS) -c $<

$(objdir)driver.o: $(srcdir)driver.f90 $(moddir)phase_space_integrators.mod 
	$(FC) $(FFLAGS) -c $<

makefile_hbook :
	(cd ./analysis/hbook; make clean -f makefile_hbook ; make -f makefile_hbook; rm *.o; cd ../../)
makefile_gammaUPC :
	(cd ./vendor/gammaUPC; make clean -f makefile_gammaUPC ; make -f makefile_gammaUPC; rm *.o; cd ../../)
makefile_fastgpl :
	(cd ./gpl/fastgpl/ ; make clean -f makefile_fastgplfortran ; make -f makefile_fastgplfortran ; cd ../../)

clean:
	@(cd ./analysis/hbook; make clean -f makefile_hbook; cd ../../)
	@(cd ./vendor/gammaUPC; make clean -f makefile_gammaUPC; cd ../../)
	@(cd ./gpl/fastgpl; make clean -f makefile_fastgplfortran; cd ../../)
	rm -rf $(objs) $(mod) $(exe) $(linkexe) $(PWD)/tmp/* *~ $(PWD)/input/*~ $(PWD)/src/*~ $(PWD)/output/* fort.* ./bin/*
end
