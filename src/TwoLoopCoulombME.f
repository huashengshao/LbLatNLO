      ! We need to multiply -I*Nc*Qf^4*a^2*a/Pi*Qf^2 for QED
      !                     -I*Nc*Qf^4*a^2*as/Pi*CF for QCD

      ! LP Coulomb approximation
      function TwoLoop_HelAmp_LPCoulombApprox(amp1L,
     $     mu2oM2,xs)
      implicit none
      double complex TwoLoop_HelAmp_LPCoulombApprox(5)
      ! amp1L(5) is the one-loop massive helicity amplitudes
      ! by calling OneLoop_HelAmp_Massive
      ! The prefactor 8*I*Nc*Qf^4*a^2 has been dropped
      double complex amp1L(5)
      ! mu2oM2=muR**2/mf**2
      ! xs=s/mf**2
      double precision mu2oM2,xs
      double precision EEoM
      double complex sqrtv,logv,pref
      double precision pipi
      parameter(pipi=3.14159265358979323846264338328d0)
      integer ihel
      if(xs.LE.0d0.OR.mu2oM2.LE.0d0)THEN
         WRITE(*,*)"ERROR: xs < 0 or mu2oM2 < 0"
         STOP
      ENDIF
      ! EEoM=EE/mf, EE=Sqrt(s)-2*mf
      ! EEoM=Sqrt(xs)-2
      EEoM=DSQRT(xs)-2d0
      IF(EEoM.EQ.0d0)THEN
         TwoLoop_HelAmp_LPCoulombApprox(1:5)=dcmplx(0d0,0d0)
         return
      ENDIF
      ! EEoM -> EEoM+i0^+
      IF(EEoM.GT.0d0)THEN
         ! sqrt(-EEoM)=-I*sqrt(EEoM)
         sqrtv=dcmplx(0d0,-dsqrt(EEoM))
         ! log(-4d0*EEoM/mu2oM2)=log(4d0*EEoM/mu2oM2)-I*Pi
         logv=dcmplx(dlog(4d0*EEoM/mu2oM2),-pipi)
      ELSE
         sqrtv=dcmplx(dsqrt(-EEoM),0d0)
         logv=dcmplx(dlog(-4d0*EEoM/mu2oM2),0d0)
      ENDIF
      ! GLP(aS)/GLP(aS**0)/(-aS*CF/(8*Pi))
      pref=4d0*pipi*(1d0-logv)/sqrtv
      do ihel=1,5
         TwoLoop_HelAmp_LPCoulombApprox(ihel)=pref*amp1L(ihel)
      enddo
      return
      end
