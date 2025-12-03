      ! We need to multiply -I*Nc*Qf^4*a^2*a/Pi*Qf^2 for QED
      !                     -I*Nc*Qf^4*a^2*as/Pi*CF for QCD

      ! LP Coulomb approximation without A term
      subroutine Get_TwoLoop_HelAmp_LPCoulombApproxNOA(mu2oM2,xs,
     $     amp2L)
      implicit none
      double complex amp2L(5)
      ! mu2oM2=muR**2/mf**2
      ! xs=s/mf**2
      double precision mu2oM2,xs
      double precision EEoM
      double complex sqrtv,logv,prefv0,prefv1
      double precision pipi
      parameter(pipi=3.14159265358979323846264338328d0)
      if(xs.LE.0d0.OR.mu2oM2.LE.0d0)THEN
         WRITE(*,*)"ERROR: xs < 0 or mu2oM2 < 0"
         STOP
      ENDIF
      ! EEoM=EE/mf, EE=Sqrt(s)-2*mf
      ! EEoM=Sqrt(xs)-2
      EEoM=DSQRT(xs)-2d0
      IF(EEoM.EQ.0d0)THEN
         amp2L(1:5)=dcmplx(0d0,0d0)
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
      ! O(v**0)
      prefv0=4d0*pipi**2*(1d0-logv)
      ! O(v**1)
      prefv1=-8d0*pipi*(pipi**2/4d0-5d0)*sqrtv
      amp2L(1)=prefv0+prefv1
      amp2L(3)=-amp2L(1)
      amp2L(2)=dcmplx(0d0,0d0)
      amp2L(4)=dcmplx(0d0,0d0)
      amp2L(5)=dcmplx(0d0,0d0)
      return
      end

      ! LP Coulomb approximation with A term
      subroutine Get_TwoLoop_HelAmp_LPCoulombApprox(xs,xt,
     $     amp2L)
      implicit none
      double complex amp2L(5)
      ! xs=s/mf**2
      double complex amp2LA(5),amp2LnoA(5)
      double precision xs,xt
      double precision xxs,xxt
      parameter(xxs=3.99999999999d0)
      integer i
      if(xs.LE.0d0.OR.xt.GE.0d0)THEN
         WRITE(*,*)"ERROR: xs < 0 or xt > 0"
         STOP
      ENDIF
      ! this is done with mu2oM2=1
      xxt=xt/xs*xxs
      CALL Get_TwoLoop_HelAmp_CoulombA(xxt,amp2LA)
      ! mu2oM2=1
      CALL Get_TwoLoop_HelAmp_LPCoulombApproxNOA(1d0,xs,
     $     amp2LnoA)
      IF(DREAL(amp2LA(1)).NE.DREAL(amp2LA(1)))THEN
         PRINT *, "ERROR 1", xs,xt
         STOP
      ENDIF
      IF(DREAL(amp2LnoA(1)).NE.DREAL(amp2LnoA(1)))THEN
          PRINT *, "ERROR 2", xs,xt
          STOP
      ENDIF
      DO i=1,5
         amp2L(i)=amp2LA(i)+amp2LnoA(i)
      ENDDO
      return
      end

      ! A(1) term in two-loop ME expansion
      subroutine Get_TwoLoop_HelAmp_CoulombA(xt,amps)
      use UToneloopbasis
      use interpolation
      implicit none
      double precision xt
      ! only ihel=1 ++++
      !      ihel=3 --++ are needed
      ! other ihel is set to zero
      double complex amps(5)
      double complex amp2LCoul(5),ampstmp1(5)
      double precision ampAhel1,ampAhel3
      INTEGER::NYA
      SAVE NYA
      REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::YA
      REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::ZA
      SAVE YA,ZA
      ! for xt, we take xt=-xs/2*y with 0<y<=1
      ! we take 40 points from 10**(-n-1) to 10**(-n) with n>=1
      INTEGER,PARAMETER::NYSEG1=40
      ! and take 50 points from 0.1 to 1
      INTEGER,PARAMETER::NYSEG2=50
      INTEGER::NYMIN=-4
      SAVE NYMIN
      integer,save::init=0
      REAL(KIND(1d0))::xu
      REAL(KIND(1d0)),parameter::xs=3.99999999999d0
      integer::i
      REAL(KIND(1d0)),PARAMETER::MQ=1d0,MUR=1d0
      REAL(KIND(1d0))::xxs,xxt,xxu,yy,shat,that,uhat,MQ2
      REAL(KIND(1d0))::logm2omu2,mu2om2
      integer NDIM_1L,NDIM_2L
      parameter (NDIM_1L=9,NDIM_2L=84)
      double complex loopba1L(NDIM_1L),loopba1L2(NDIM_1L)
      double complex loopba2L(NDIM_2L)
      INTEGER,PARAMETER::iunit=30336
      logical,parameter::generate_grid=.FALSE.
      if(init.eq.0)then
         IF(generate_grid)THEN
            NYA=(-1-NYMIN)*NYSEG1+NYSEG2+1
            IF(ALLOCATED(YA))THEN
               IF(SIZE(YA).NE.NYA)THEN
                  DEALLOCATE(YA)
                  ALLOCATE(YA(NYA))
               ENDIF
            ELSE
               ALLOCATE(YA(NYA))
            ENDIF
            ! log10(y)
            DO i=1,(-1-NYMIN)*NYSEG1
               YA(i)=(i-1)/DBLE(NYSEG1)+NYMIN
            ENDDO
            DO i=(-1-NYMIN)*NYSEG1+1,NYA
               YA(i)=(i-(-1-NYMIN)*NYSEG1-1)/DBLE(NYSEG2)-1d0
            ENDDO
            IF(ALLOCATED(ZA))THEN
               IF(SIZE(ZA).NE.NYA)THEN
                  DEALLOCATE(ZA)
                  ALLOCATE(ZA(NYA,2))
               ENDIF
            ELSE
               ALLOCATE(ZA(NYA,4))
            ENDIF
            MQ2=MQ**2
            mu2om2=MUR**2/MQ2
            logm2omu2=DLOG(MQ2/MUR**2)
            xxs=xs
            shat=xxs*MQ2
            CALL Get_TwoLoop_HelAmp_LPCoulombApproxNOA(mu2om2,xxs,
     $           amp2LCoul)
            DO i=1,NYA
               yy=10d0**(YA(i))
               xxt=-xxs/2d0*yy
               xxu=-xxs-xxt
               that=xxt*MQ2
               uhat=xxu*MQ2
               call loopbasis(MQ,shat,that,MUR,loopba1L)
               call UToneloopbasis_avh(xxs,xxt,logm2omu2,
     $              loopba1L,loopba1L2)
               CALL UToneloopbasis_nosqrt(xxs,xxt,loopba1L2,loopba1L)
               CALL UTtwoloopbasis(xxs,xxt,loopba1L,loopba2L)
               CALL Get_TwoLoop_HelAmp_Massive(xxs,xxt,xxu,loopba2L,
     $              ampstmp1)
               ZA(i,1)=DREAL(ampstmp1(1)-amp2LCoul(1))
               ZA(i,2)=DREAL(ampstmp1(3)-amp2LCoul(3))
            ENDDO
            OPEN(UNIT=iunit,FILE="Amp2LCoulA.grid",STATUS="NEW")
            WRITE(iunit,*)NYA
            DO i=1,NYA
               WRITE(iunit,*)YA(i)
            ENDDO
            DO i=1,NYA
               WRITE(iunit,*)ZA(i,1:2)
            ENDDO
            CLOSE(UNIT=iunit)
         ENDIF
         ! now, we make sure Amp2LCoulA.grid has been generated
         ! just read the grid
         OPEN(UNIT=iunit,FILE="Amp2LCoulA.grid")
         READ(iunit,*)NYA
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
            IF(SIZE(ZA).NE.NYA)THEN
               DEALLOCATE(ZA)
               ALLOCATE(ZA(NYA,2))
            ENDIF
         ELSE
            ALLOCATE(ZA(NYA,2))
         ENDIF
         DO i=1,NYA
            READ(iunit,*)ZA(i,1:2)
         ENDDO
         CLOSE(UNIT=iunit)
         init=1
      endif
      xu=-xs-xt
      if(xt.LT.xu)then
         xxt=xu
         xxu=xt
      else
         xxt=xt
         xxu=xu
      endif
      ! check the grid range
      yy=DLOG10(-2d0*xxt/xs)
      IF(yy.LT.YA(1))THEN
         amps(1:5)=dcmplx(0d0,0d0)
         return
      ENDIF
      amps(2)=dcmplx(0d0,0d0)
      amps(4:5)=dcmplx(0d0,0d0)
      CALL SPLINE_INTERPOLATE(YA,ZA(1:NYA,1),NYA,yy,ampAhel1)
      amps(1)=dcmplx(ampAhel1,0d0)
      CALL SPLINE_INTERPOLATE(YA,ZA(1:NYA,2),NYA,yy,ampAhel3)
      amps(3)=dcmplx(ampAhel3,0d0)
      return
      end
