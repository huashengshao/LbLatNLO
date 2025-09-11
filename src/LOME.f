      subroutine sloopmatrix(pborn,virt_wgts)
      use kinetics
      use UToneloopbasis
      ! helicity summed amplitude square
      ! from helicity amplitudes
      ! this is applicable for fermions and W boson
      implicit none
      integer NDIM
      parameter (NDIM=9)
      ! if mass is negative, it means we do not want to include its contribution
      double precision aEWM1,MU,MD,MS,MC,MB,MT
      double precision ME,MM,MTA,MW
      common /Params/ aEWM1,MU,MD,MS,MC,MB,MT,ME,MM,MTA,MW
      integer iloopbasis
      common /LbL_loop_1L/ iloopbasis ! 0: UT basis with oneloop for massive fermions and W bosons
                                      ! 1: UT basis with G's for massive fermions and W bosons
      double precision virt_wgts
      double precision pborn(4,0:3)
      double precision shat,that,uhat
      double precision xs,xt,xu,MQ,MQ2,MUR
      double precision logm2omu2
      double complex loopba(NDIM),loopba2(NDIM)
      double complex amp(5),camp(5),amptmp
      integer i,j
      ! massless fermions
      double complex OneLoop_HelAmp_Massless
      external OneLoop_HelAmp_Massless
      ! massive fermions
      double complex OneLoop_HelAmp_Massive
      external OneLoop_HelAmp_Massive
      ! massive W boson
      double complex OneLoop_HelAmp_MassiveW
      external OneLoop_HelAmp_MassiveW
      double complex prefactor,prefac0
      integer i_mass,i_massless
      double complex prefac(10)
      double precision mass(10)
      ! averaged over initial state and final state symmetry
      integer IDEN
      parameter (IDEN=8)
      integer init
      data init/0/
      save init,i_mass,i_massless,prefac0,prefac,mass
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
         prefac0=dcmplx(0d0,0d0)
         IF(MU.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+3d0*(2d0/3d0)**4*prefactor
         ELSEIF(MU.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MU
            prefac(i_mass)=3d0*(2d0/3d0)**4*prefactor
         ENDIF
         IF(MD.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+3d0*(-1d0/3d0)**4*prefactor
         ELSEIF(MD.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MD
            prefac(i_mass)=3d0*(-1d0/3d0)**4*prefactor
         ENDIF
         IF(MS.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+3d0*(-1d0/3d0)**4*prefactor
         ELSEIF(MS.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MS
            prefac(i_mass)=3d0*(-1d0/3d0)**4*prefactor
         ENDIF
         IF(MC.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+3d0*(2d0/3d0)**4*prefactor
         ELSEIF(MC.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MC
            prefac(i_mass)=3d0*(2d0/3d0)**4*prefactor
         ENDIF
         IF(MB.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+3d0*(-1d0/3d0)**4*prefactor
         ELSEIF(MB.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MB
            prefac(i_mass)=3d0*(-1d0/3d0)**4*prefactor
         ENDIF
         IF(MT.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+3d0*(2d0/3d0)**4*prefactor
         ELSEIF(MT.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MT
            prefac(i_mass)=3d0*(2d0/3d0)**4*prefactor
         ENDIF
         IF(ME.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+(-1d0)**4*prefactor
         ELSEIF(ME.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=ME
            prefac(i_mass)=(-1d0)**4*prefactor
         ENDIF
         IF(MM.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+(-1d0)**4*prefactor
         ELSEIF(MM.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MM
            prefac(i_mass)=(-1d0)**4*prefactor
         ENDIF
         IF(MTA.EQ.0d0)THEN
            i_massless=i_massless+1
            prefac0=prefac0+(-1d0)**4*prefactor
         ELSEIF(MTA.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MTA
            prefac(i_mass)=(-1d0)**4*prefactor
         ENDIF
         IF(MW.EQ.0d0)THEN
            WRITE(*,*)"ERROR: MW=0 !"
            STOP
         ELSEIF(MW.GT.0d0)THEN
            i_mass=i_mass+1
            mass(i_mass)=MW
            prefac(i_mass)=-1.5d0*prefactor
         ENDIF
         init=1
      endif

      amp(1:5)=dcmplx(0d0,0d0)

      ! massless fermion loops
      IF(i_massless.GT.0)THEN
         DO i=1,5
            amp(i)=OneLoop_HelAmp_Massless(i,shat,that,uhat)
            amp(i)=amp(i)*prefac0
         ENDDO
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
            call loopbasis(MQ,shat,that,MUR,loopba)
            logm2omu2=DLOG(MQ2/MUR**2)
            call UToneloopbasis_scalar(xs,xt,logm2omu2,loopba,loopba2)
         else
            ! use G's
            call UToneloopbasis_G(xs,xt,loopba2)
         endif
         if(i.eq.i_mass.and.MW.GT.0d0)then
            ! it must be the W boson
            DO j=1,5
               amptmp=OneLoop_HelAmp_MassiveW(j,xs,xt,xu,loopba2)
               amp(j)=amp(j)+amptmp*prefac(i)
            ENDDO
         else
            ! it is the massive fermion
            DO j=1,5
               amptmp=OneLoop_HelAmp_Massive(j,xs,xt,xu,loopba2)
               amp(j)=amp(j)+amptmp*prefac(i)
            ENDDO
         endif
      ENDDO

      ! helicity-summed amplitude square
      DO i=1,5
         camp(i)=DCONJG(amp(i))
      enddo
      virt_wgts=amp(1)*camp(1)*2d0+amp(2)*camp(2)*8d0
     $     +amp(3)*camp(3)*2d0+amp(4)*camp(4)*2d0
     $     +amp(5)*camp(5)*2d0
      virt_wgts=virt_wgts/DBLE(IDEN)
      return
      end

      subroutine sloopmatrix_fromFFs_fermions(pborn,virt_wgts)
      use kinetics
      use UToneloopbasis
      ! helicity summed amplitude square
      ! from form factors (FFs)
      ! this is applicable only for fermions
      implicit none
      integer NDIM
      parameter (NDIM=9)
      double precision aEWM1,MU,MD,MS,MC,MB,MT
      double precision ME,MM,MTA,MW
      common /Params/ aEWM1,MU,MD,MS,MC,MB,MT,ME,MM,MTA,MW
      integer iformfactor
      common /amplitude_1L/ iformfactor ! 0: use 7 FFs; 1: use 5 FFs (not UT basis); 
                                        ! 2: use 5 FFs (UT basis with oneloop for massive fermions); 
                                        ! 3: use 5 FFs (UT basis with G's for massive fermions)
      double precision virt_wgts
      double complex ans
      double precision pborn(4,0:3)
      double precision shat,that,uhat
      double precision xs,xt,xu,MQ,MQ2,MQ4,MUR
      double precision logm2omu2
      double complex loopba(NDIM),loopba2(NDIM)
      double complex amp(5)
      integer i,j
      double complex A1stu_tmp,A1tsu_tmp,A1ust_tmp
      double complex ASstu_tmp
      double complex DB111stu_tmp,DB111tsu_tmp,DB111ust_tmp
      double complex DBhat111stu_tmp,DBhat111tsu_tmp,DBhat111ust_tmp
      double complex DC2111stu_tmp
      double complex DChat2111stu_tmp
      double complex A1stu,A1tsu,A1ust
      double complex ASstu
      double complex DB111stu,DB111tsu,DB111ust
      double complex DBhat111stu,DBhat111tsu,DBhat111ust
      double complex DC2111stu
      double complex DChat2111stu
      double complex CA1stu,CA1tsu,CA1ust
      double complex CDB111stu,CDB111tsu,CDB111ust
      double complex CDC2111stu
      double complex CASstu
      double complex CDBhat111stu,CDBhat111tsu,CDBhat111ust
      double complex CDChat2111stu
      double complex A1stu_FormFactor_Massive
      external A1stu_FormFactor_Massive
      double complex A1tsu_FormFactor_Massive
      external A1tsu_FormFactor_Massive
      double complex A1ust_FormFactor_Massive
      external A1ust_FormFactor_Massive
      double complex ASstu_FormFactor_Massive
      external ASstu_FormFactor_Massive
      double complex ASstu_FormFactor_Massive_UT
      external ASstu_FormFactor_Massive_UT
      double complex DB111stu_FormFactor_Massive
      external DB111stu_FormFactor_Massive
      double complex DB111tsu_FormFactor_Massive
      external DB111tsu_FormFactor_Massive
      double complex DB111ust_FormFactor_Massive
      external DB111ust_FormFactor_Massive
      double complex DBhat111stu_FormFactor_Massive
      external DBhat111stu_FormFactor_Massive
      double complex DBhat111stu_FormFactor_Massive_UT
      external DBhat111stu_FormFactor_Massive_UT
      double complex DBhat111tsu_FormFactor_Massive
      external DBhat111tsu_FormFactor_Massive
      double complex DBhat111tsu_FormFactor_Massive_UT
      external DBhat111tsu_FormFactor_Massive_UT
      double complex DBhat111ust_FormFactor_Massive
      external DBhat111ust_FormFactor_Massive
      double complex DBhat111ust_FormFactor_Massive_UT
      external DBhat111ust_FormFactor_Massive_UT
      double complex DC2111stu_FormFactor_Massive
      external DC2111stu_FormFactor_Massive
      double complex DC2111stu_FormFactor_Massive_UT
      external DC2111stu_FormFactor_Massive_UT
      double complex A1stu_FormFactor_Massless
      external A1stu_FormFactor_Massless
      double complex A1tsu_FormFactor_Massless
      external A1tsu_FormFactor_Massless
      double complex A1ust_FormFactor_Massless
      external A1ust_FormFactor_Massless
      double complex ASstu_FormFactor_Massless
      external ASstu_FormFactor_Massless
      double complex DB111stu_FormFactor_Massless
      external DB111stu_FormFactor_Massless
      double complex DB111tsu_FormFactor_Massless
      external DB111tsu_FormFactor_Massless
      double complex DB111ust_FormFactor_Massless
      external DB111ust_FormFactor_Massless
      double complex DBhat111stu_FormFactor_Massless
      external DBhat111stu_FormFactor_Massless
      double complex DBhat111tsu_FormFactor_Massless
      external DBhat111tsu_FormFactor_Massless
      double complex DBhat111ust_FormFactor_Massless
      external DBhat111ust_FormFactor_Massless
      double complex DC2111stu_FormFactor_Massless
      external DC2111stu_FormFactor_Massless
c      double complex OneLoop_HelAmp_Massless
c      external OneLoop_HelAmp_Massless
c      double complex OneLoop_HelAmp_Massive
c      external OneLoop_HelAmp_Massive
c      double complex OneLoop_HelAmp_MassiveW
c      external OneLoop_HelAmp_MassiveW
      double complex prefactor,prefac0
      integer i_mass
      double complex prefac(9)
      double precision mass(9)
      ! averaged over initial state and final state symmetry
      integer IDEN
      parameter (IDEN=8)
      integer init
      data init/0/
      save init,i_mass,prefac0,prefac,mass
      shat=sumdot2(pborn(1,0:3),pborn(2,0:3),1d0)
      that=sumdot2(pborn(2,0:3),pborn(3,0:3),-1d0)
      uhat=sumdot2(pborn(1,0:3),pborn(3,0:3),-1d0)
      ! scale (should be independent since it is finite)
      MUR=10d0
      if(init.eq.0)then
         ! initialisation
         prefactor=dcmplx(0d0,32d0*aEWM1**(-2))
         i_mass=0
         prefac0=dcmplx(0d0,0d0)
         IF(MU.EQ.0d0)THEN
            prefac0=prefac0+3d0*(2d0/3d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=MU
            prefac(i_mass)=3d0*(2d0/3d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         IF(MD.EQ.0d0)THEN
            prefac0=prefac0+3d0*(-1d0/3d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=MD
            prefac(i_mass)=3d0*(-1d0/3d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         IF(MS.EQ.0d0)THEN
            prefac0=prefac0+3d0*(-1d0/3d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=MS
            prefac(i_mass)=3d0*(-1d0/3d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         IF(MC.EQ.0d0)THEN
            prefac0=prefac0+3d0*(2d0/3d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=MC
            prefac(i_mass)=3d0*(2d0/3d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         IF(MB.EQ.0d0)THEN
            prefac0=prefac0+3d0*(-1d0/3d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=MB
            prefac(i_mass)=3d0*(-1d0/3d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         IF(MT.EQ.0d0)THEN
            prefac0=prefac0+3d0*(2d0/3d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=MT
            prefac(i_mass)=3d0*(2d0/3d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         IF(ME.EQ.0d0)THEN
            prefac0=prefac0+(-1d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=ME
            prefac(i_mass)=(-1d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         IF(MM.EQ.0d0)THEN
            prefac0=prefac0+(-1d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=MM
            prefac(i_mass)=(-1d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         IF(MTA.EQ.0d0)THEN
            prefac0=prefac0+(-1d0)**4
         ELSE
            i_mass=i_mass+1
            mass(i_mass)=MTA
            prefac(i_mass)=(-1d0)**4/mass(i_mass)**4*prefactor
         ENDIF
         prefac0=prefac0*prefactor
         init=1
      endif

      A1stu=dcmplx(0d0,0d0)
      A1tsu=dcmplx(0d0,0d0)
      A1ust=dcmplx(0d0,0d0)
      ASstu=dcmplx(0d0,0d0)
      DB111stu=dcmplx(0d0,0d0)
      DB111tsu=dcmplx(0d0,0d0)
      DB111ust=dcmplx(0d0,0d0)
      DBhat111stu=dcmplx(0d0,0d0)
      DBhat111tsu=dcmplx(0d0,0d0)
      DBhat111ust=dcmplx(0d0,0d0)
      DC2111stu=dcmplx(0d0,0d0)
      DChat2111stu=dcmplx(0d0,0d0)
      ! massless fermion loops
      IF(i_mass.LT.9)THEN
         MQ=0d0
         call loopbasis(MQ,shat,that,MUR,loopba)
         DC2111stu_tmp=DC2111stu_FormFactor_Massless(shat,that
     $        ,uhat,loopba)
         if(iformfactor.eq.0)then
            A1stu_tmp=A1stu_FormFactor_Massless(shat,that,uhat,loopba)
            A1tsu_tmp=A1tsu_FormFactor_Massless(shat,that,uhat,loopba)
            A1ust_tmp=A1ust_FormFactor_Massless(shat,that,uhat,loopba)
            A1stu_tmp=1d0/96d0*A1stu_tmp
            A1tsu_tmp=1d0/96d0*A1tsu_tmp
            A1ust_tmp=1d0/96d0*A1ust_tmp
            DB111stu_tmp=DB111stu_FormFactor_Massless(shat,that
     $           ,uhat,loopba)
            DB111tsu_tmp=DB111tsu_FormFactor_Massless(shat,that
     $           ,uhat,loopba)
            DB111ust_tmp=DB111ust_FormFactor_Massless(shat,that
     $           ,uhat,loopba)
            DB111stu_tmp=1d0/48d0*DB111stu_tmp
            DB111tsu_tmp=1d0/48d0*DB111tsu_tmp
            DB111ust_tmp=1d0/48d0*DB111ust_tmp
            A1stu=A1stu+A1stu_tmp*prefac0
            A1tsu=A1tsu+A1tsu_tmp*prefac0
            A1ust=A1ust+A1ust_tmp*prefac0
            DB111stu=DB111stu+DB111stu_tmp*prefac0
            DB111tsu=DB111tsu+DB111tsu_tmp*prefac0
            DB111ust=DB111ust+DB111ust_tmp*prefac0
            DC2111stu=DC2111stu+DC2111stu_tmp*prefac0
         else
            ASstu_tmp=ASstu_FormFactor_Massless(shat,that,uhat,loopba)
            ASstu_tmp=1d0/96d0*ASstu_tmp
            DBhat111stu_tmp=DBhat111stu_FormFactor_Massless(shat,that
     $           ,uhat,loopba)
            DBhat111tsu_tmp=DBhat111tsu_FormFactor_Massless(shat,that
     $           ,uhat,loopba)
            DBhat111ust_tmp=DBhat111ust_FormFactor_Massless(shat,that
     $           ,uhat,loopba)
            DBhat111stu_tmp=1d0/48d0*DBhat111stu_tmp
            DBhat111tsu_tmp=1d0/48d0*DBhat111tsu_tmp
            DBhat111ust_tmp=1d0/48d0*DBhat111ust_tmp
            DChat2111stu_tmp=DC2111stu_tmp-ASstu_tmp/(shat*uhat)
            ASstu=ASstu+ASstu_tmp*prefac0
            DBhat111stu=DBhat111stu+DBhat111stu_tmp*prefac0
            DBhat111tsu=DBhat111tsu+DBhat111tsu_tmp*prefac0
            DBhat111ust=DBhat111ust+DBhat111ust_tmp*prefac0
            DChat2111stu=DChat2111stu+DChat2111stu_tmp*prefac0
         endif
      ENDIF
      
      
      ! massive fermion loops
      DO i=1,i_mass
         MQ=mass(i)
         MQ2=MQ**2
         MQ4=MQ2**2
         xs=shat/MQ2
         xt=that/MQ2
         xu=uhat/MQ2
         if(iformfactor.le.2)then
            call loopbasis(MQ,shat,that,MUR,loopba)
            if(iformfactor.eq.2)then
               logm2omu2=DLOG(MQ2/MUR**2)
               call UToneloopbasis_scalar(xs,xt,logm2omu2,loopba,loopba2)
            endif
         else
            call UToneloopbasis_G(xs,xt,loopba2)
         endif
         if(iformfactor.eq.0)then
            DC2111stu_tmp=DC2111stu_FormFactor_Massive(xs,xt,xu,loopba)
            A1stu_tmp=A1stu_FormFactor_Massive(xs,xt,xu,loopba)
            A1tsu_tmp=A1tsu_FormFactor_Massive(xs,xt,xu,loopba)
            A1ust_tmp=A1ust_FormFactor_Massive(xs,xt,xu,loopba)
            A1stu_tmp=MQ4/96d0*A1stu_tmp
            A1tsu_tmp=MQ4/96d0*A1tsu_tmp
            A1ust_tmp=MQ4/96d0*A1ust_tmp
            DB111stu_tmp=DB111stu_FormFactor_Massive(xs,xt,xu,loopba)
            DB111tsu_tmp=DB111tsu_FormFactor_Massive(xs,xt,xu,loopba)
            DB111ust_tmp=DB111ust_FormFactor_Massive(xs,xt,xu,loopba)
            DB111stu_tmp=MQ2/48d0*DB111stu_tmp
            DB111tsu_tmp=MQ2/48d0*DB111tsu_tmp
            DB111ust_tmp=MQ2/48d0*DB111ust_tmp
            A1stu=A1stu+A1stu_tmp*prefac(i)
            A1tsu=A1tsu+A1tsu_tmp*prefac(i)
            A1ust=A1ust+A1ust_tmp*prefac(i)
            DB111stu=DB111stu+DB111stu_tmp*prefac(i)
            DB111tsu=DB111tsu+DB111tsu_tmp*prefac(i)
            DB111ust=DB111ust+DB111ust_tmp*prefac(i)
            DC2111stu=DC2111stu+DC2111stu_tmp*prefac(i)
         elseif(iformfactor.eq.1)then
            DC2111stu_tmp=DC2111stu_FormFactor_Massive(xs,xt,xu,loopba)
            ASstu_tmp=ASstu_FormFactor_Massive(xs,xt,xu,loopba)
            ASstu_tmp=MQ4/96d0*ASstu_tmp
            DBhat111stu_tmp=DBhat111stu_FormFactor_Massive(xs,xt,xu
     $           ,loopba)
            DBhat111tsu_tmp=DBhat111tsu_FormFactor_Massive(xs,xt,xu
     $           ,loopba)
            DBhat111ust_tmp=DBhat111ust_FormFactor_Massive(xs,xt,xu
     $           ,loopba)
            DBhat111stu_tmp=MQ2/48d0*DBhat111stu_tmp
            DBhat111tsu_tmp=MQ2/48d0*DBhat111tsu_tmp
            DBhat111ust_tmp=MQ2/48d0*DBhat111ust_tmp
            DChat2111stu_tmp=DC2111stu_tmp-ASstu_tmp/(shat*uhat)
            ASstu=ASstu+ASstu_tmp*prefac(i)
            DBhat111stu=DBhat111stu+DBhat111stu_tmp*prefac(i)
            DBhat111tsu=DBhat111tsu+DBhat111tsu_tmp*prefac(i)
            DBhat111ust=DBhat111ust+DBhat111ust_tmp*prefac(i)
            DChat2111stu=DChat2111stu+DChat2111stu_tmp*prefac(i)
         else
            ASstu_tmp=ASstu_FormFactor_Massive_UT(xs,xt,xu,loopba2)
            ASstu_tmp=MQ4/8d0*ASstu_tmp
            DBhat111stu_tmp=DBhat111stu_FormFactor_Massive_UT(xs,xt,xu
     $           ,loopba2)
            DBhat111tsu_tmp=DBhat111tsu_FormFactor_Massive_UT(xs,xt,xu
     $           ,loopba2)
            DBhat111ust_tmp=DBhat111ust_FormFactor_Massive_UT(xs,xt,xu
     $           ,loopba2)
            DBhat111stu_tmp=MQ2/8d0*DBhat111stu_tmp
            DBhat111tsu_tmp=MQ2/8d0*DBhat111tsu_tmp
            DBhat111ust_tmp=MQ2/8d0*DBhat111ust_tmp
            DC2111stu_tmp=DC2111stu_FormFactor_Massive_UT(xs,xt,xu
     $           ,loopba2)
            DChat2111stu_tmp=DC2111stu_tmp-ASstu_tmp/(shat*uhat)
            ASstu=ASstu+ASstu_tmp*prefac(i)
            DBhat111stu=DBhat111stu+DBhat111stu_tmp*prefac(i)
            DBhat111tsu=DBhat111tsu+DBhat111tsu_tmp*prefac(i)
            DBhat111ust=DBhat111ust+DBhat111ust_tmp*prefac(i)
            DChat2111stu=DChat2111stu+DChat2111stu_tmp*prefac(i)
         endif
      ENDDO
      ! helicity-summed amplitude sqaure
      if(iformfactor.eq.0)then
         CA1stu=DCONJG(A1stu)
         CA1tsu=DCONJG(A1tsu)
         CA1ust=DCONJG(A1ust)
         CDB111stu=DCONJG(DB111stu)
         CDB111tsu=DCONJG(DB111tsu)
         CDB111ust=DCONJG(DB111ust)
         CDC2111stu=DCONJG(DC2111stu)
         ans=(2*A1UST*CA1STU + 2*A1UST*CA1TSU + 4*A1UST*CA1UST
     $        + 2*A1UST*CDB111ust*that + 2*CA1UST*DB111ust*that
     $        + 2*CDB111ust*DB111ust*that**2 +
     $        2*CA1STU*DB111stu*uhat + 2*CA1TSU*DB111tsu*uhat -
     $        A1UST*CDC2111stu*shat*uhat + 2*CDB111stu*DB111stu*uhat**2
     $        +2*CDB111tsu*DB111tsu*uhat**2 -
     $        DC2111stu*shat*uhat*(CA1STU + CA1TSU + CA1UST -
     $        CDC2111stu*shat*uhat) +
     $        A1STU*(4*CA1STU - CDC2111stu*shat*uhat +
     $        2*(CA1TSU + CA1UST + CDB111stu*uhat)) +
     $        A1TSU*(-(CDC2111stu*shat*uhat) +
     $        2*(CA1STU + 2*CA1TSU + CA1UST + CDB111tsu*uhat)))
         virt_wgts=dreal(ans)
      else
         CASstu=DCONJG(ASstu)
         CDBhat111stu=DCONJG(DBhat111stu)
         CDBhat111tsu=DCONJG(DBhat111tsu)
         CDBhat111ust=DCONJG(DBhat111ust)
         CDChat2111stu=DCONJG(DChat2111stu)
c         virt_wgts=abs(ASstu)**2+2d0*uhat**2*abs(DBhat111stu)**2
c     $        +2d0*uhat**2*abs(DBhat111tsu)**2
c     $        +2d0*that**2*abs(DBhat111ust)**2
c     $        +shat**2*uhat**2*abs(DChat2111stu)**2
         virt_wgts=ASstu*CASstu+2d0*uhat**2*(DBhat111stu*CDBhat111stu
     $        +DBhat111tsu*CDBhat111tsu)
     $        +2d0*that**2*DBhat111ust*CDBhat111ust
     $        +shat**2*uhat**2*CDChat2111stu*DChat2111stu
      endif
      virt_wgts=virt_wgts/DBLE(IDEN)
      return
c      W loop check
c      MQ=MW
c      MQ2=MQ**2
c      MQ4=MQ2**2
c      xs=shat/MQ2
c      xt=that/MQ2
c      xu=uhat/MQ2
c      call loopbasis(MQ,shat,that,MUR,loopba)
c      logm2omu2=DLOG(MQ2/MUR**2)
c      call UToneloopbasis_scalar(xs,xt,logm2omu2,loopba,loopba2)
c      do i=1,5
c         amp(i)=OneLoop_HelAmp_MassiveW(i,xs,xt,xu,loopba2)
c      enddo
c      virt_wgts=amp(1)*DCONJG(amp(1))*2d0+amp(2)*DCONJG(amp(2))*8d0
c     $     +amp(3)*DCONJG(amp(3))*2d0+amp(4)*DCONJG(amp(4))*2d0
c     $     +amp(5)*DCONJG(amp(5))*2d0
c      virt_wgts=virt_wgts*(-12d0*aEWM1**(-2))**2
c      virt_wgts=virt_wgts/DBLE(IDEN)
c     end of W loop check
c      massless fermions      
c      virt_wgts=0d0
c      ASstu=OneLoop_HelAmp_Massless(1,shat,that,uhat)
c      DBhat111stu=OneLoop_HelAmp_Massless(2,shat,that,uhat)
c      DBhat111tsu=OneLoop_HelAmp_Massless(3,shat,that,uhat)
c      DBhat111ust=OneLoop_HelAmp_Massless(4,shat,that,uhat)
c      DChat2111stu=OneLoop_HelAmp_Massless(5,shat,that,uhat)
c      ! 44/9 is the sum of N_{c,f}*Q_f**4 for
c      ! 6 quarks + 3 charged leptons
c      ASstu=ASstu*8d0*aEWM1**(-2)*44d0/9d0
c      DBhat111stu=DBhat111stu*8d0*aEWM1**(-2)*44d0/9d0
c      DBhat111tsu=DBhat111tsu*8d0*aEWM1**(-2)*44d0/9d0
c      DBhat111ust=DBhat111ust*8d0*aEWM1**(-2)*44d0/9d0
c      DChat2111stu=DChat2111stu*8d0*aEWM1**(-2)*44d0/9d0
c      CASstu=DCONJG(ASstu)
c      CDBhat111stu=DCONJG(DBhat111stu)
c      CDBhat111tsu=DCONJG(DBhat111tsu)
c      CDBhat111ust=DCONJG(DBhat111ust)
c      CDChat2111stu=DCONJG(DChat2111stu)
c      virt_wgts=ASstu*CASstu*2d0
c     $     +DBhat111stu*CDBhat111stu*8d0
c     $     +DBhat111tsu*CDBhat111tsu*2d0
c     $     +DBhat111ust*CDBhat111ust*2d0
c     $     +DChat2111stu*CDChat2111stu*2d0
c      virt_wgts=virt_wgts/DBLE(IDEN)
      return
      end


      ! need to multiply MQ**4/96
      double complex function A1stu_FormFactor_Massive(xs,xt,xu,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      A1stu_FormFactor_Massive= 8-(8*(xs**2 - 3*xt*xu)*loopba(1))/
     -   (xt*xu) - (8*xt**2*loopba(2))/(xs*xu) - 
     -  (4*(3*xs**2 - 2*xt*xu + xs*(3*xt + xu))*
     -     loopba(3))/(xs*xt) - 
     -  (2*(4*xs**3*xu**2 + 
     -       3*xt**2*xu*
     -        (xt**2 + 3*xt*xu + 2*(-8 + xu)*xu) 
     -        - 4*xs**2*xt*
     -        (xt**2 - 4*xu**2 - 3*xt*(2 + xu)) + 
     -       3*xs*xt*
     -        (-8*xu**2 + 6*xt*xu**2 + 
     -          xt**2*(8 + xu)))*loopba(4))/
     -   (xt**2*xu**2) - 
     -  (2*xt*(4*xs*(-6 + xt)*xu**2 + 
     -       4*xt**2*xu**2 + 
     -       xs**3*(24 - 4*xt + 9*xu) + 
     -       xs**2*(-3*xt*(-8 + xu) + 9*xu**2))*
     -     loopba(5))/(xs**2*xu**2) + 
     -  ((6*xs**5 + xs**3*xt*
     -        (-12 + 3*xt - 26*xu) + 
     -       2*xs**4*(6*xt - xu) - 8*xt**4*xu + 
     -       xs*xt**2*
     -        (3*xt**2 + 48*xu - 5*xt*xu) - 
     -       3*xs**2*xt*(-12*xu + xt*(4 + 9*xu)))*
     -     loopba(6))/(xs**2*xt**2) - 
     -  (2*(32*xt**2 + 
     -       2*xs**2*(-4 + xt)*(-4 + xt - 3*xu) + 
     -       xs*xt*(64 - 16*xt + 24*xu - 3*xu**2))
     -      *loopba(7))/xu**2 - 
     -  (2*(2*xs**4 + 10*xs**3*xt + 32*xt**2 + 
     -       xs*xt**2*(-40 + 3*xt) + 
     -       xs**2*xt*(-16 + 11*xt))*loopba(8))/
     -   xt**2 - (2*(3*xs**3*(-8 + xt) + 
     -       4*xs*(-4 + xt)*xt**2 + 2*xt**4 + 
     -       xs**2*(32 - 16*xt + 5*xt**2))*
     -     loopba(9))/xs**2
      return
      end


      ! need to multiply MQ**4/96
      double complex function A1tsu_FormFactor_Massive(xs,xt,xu,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      A1tsu_FormFactor_Massive=8-(8*xs**2*loopba(1))/(xt*xu) - 
     -  (4*(6*xs**2*xu - xt*xu*(5*xt + 3*xu) + 
     -       xs*(-2*xt**2 - 3*xt*xu + 6*xu**2))*
     -     loopba(2))/(xs*xt*xu) - 
     -  (4*(xs*(3*xt - 2*xu) + xt*(3*xt + xu))*
     -     loopba(3))/(xs*xt) - 
     -  (2*xs*(4*xs**2*xu**2 + 
     -       xs*xt*(-4*xt**2 - 3*xt*(-8 + xu) + 
     -          4*xu**2) + 
     -       3*xt*(-8*xu**2 + 3*xt*xu**2 + 
     -          xt**2*(8 + 3*xu)))*loopba(4))/
     -   (xt**2*xu**2) - 
     -  (2*(3*xs**4*xu + 4*xt**3*xu**2 + 
     -       8*xs*xt*(-3 + 2*xt)*xu**2 + 
     -       6*xs**2*
     -        (3*xt*xu**2 + (-8 + xu)*xu**2 + 
     -          2*xt**2*(2 + xu)) + 
     -       xs**3*(-4*xt**2 + 9*xu**2 + 
     -          3*xt*(8 + xu)))*loopba(5))/
     -   (xs**2*xu**2) + 
     -  ((xs**4*(3*xt - 8*xu) + 
     -       2*xt**4*(3*xt - xu) - 
     -       5*xs**3*xt*xu + 
     -       3*xs**2*xt*
     -        (xt**2 + 16*xu - xt*(4 + 9*xu)) + 
     -       2*xs*xt**2*
     -        (6*xt**2 + 18*xu - xt*(6 + 13*xu)))*
     -     loopba(6))/(xs**2*xt**2) - 
     -  (2*(2*xs**2*(-4 + xt)**2 + 
     -       8*xt**2*(4 + 3*xu) + 
     -       xs*xt*(64 + 24*xu - 3*xu**2 - 
     -          2*xt*(8 + 3*xu)))*loopba(7))/xu**2
     -    - (2*(2*xs**4 + 4*xs**3*xt + 
     -       8*(4 - 3*xt)*xt**2 + 
     -       xs*xt**2*(-16 + 3*xt) + 
     -       xs**2*xt*(-16 + 5*xt))*loopba(8))/
     -   xt**2 - (2*(3*xs**3*xt + 2*xt**4 + 
     -       2*xs*xt**2*(-8 + 5*xt) + 
     -       xs**2*(32 - 40*xt + 11*xt**2))*
     -     loopba(9))/xs**2
      return
      end


      ! need to multiply MQ**4/96
      double complex function A1ust_FormFactor_Massive(xs,xt,xu,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      A1ust_FormFactor_Massive=8-(8*xs**2*loopba(1))/(xt*xu) - 
     -  (4*(xs*(-2*xt + 3*xu) + xu*(xt + 3*xu))*
     -     loopba(2))/(xs*xu) - 
     -  (4*(6*xs**2*xt - xt*xu*(3*xt + 5*xu) + 
     -       xs*(6*xt**2 - 3*xt*xu - 2*xu**2))*
     -     loopba(3))/(xs*xt*xu) - 
     -  (2*xs*(4*xs**2*xt**2 + 
     -       xs*xu*(4*xt**2 - 3*xt*xu - 
     -          4*(-6 + xu)*xu) + 
     -       3*xu*(8*xu**2 + 3*xt*xu**2 + 
     -          xt**2*(-8 + 3*xu)))*loopba(4))/
     -   (xt**2*xu**2) + 
     -  ((-5*xs**3*xt*xu - 2*(xt - 3*xu)*xu**4 + 
     -       xs**4*(-8*xt + 3*xu) + 
     -       3*xs**2*xu*
     -        (xt*(16 - 9*xu) + (-4 + xu)*xu) + 
     -       2*xs*xu**2*
     -        (xt*(18 - 13*xu) + 6*(-1 + xu)*xu))*
     -     loopba(5))/(xs**2*xu**2) - 
     -  (2*(3*xs**4*xt + 4*xt**2*xu**3 + 
     -       8*xs*xt**2*xu*(-3 + 2*xu) + 
     -       xs**3*(9*xt**2 + 3*xt*xu - 
     -          4*(-6 + xu)*xu) + 
     -       6*xs**2*
     -        (xt**3 + 4*xu**2 + 2*xt*xu**2 + 
     -          xt**2*(-8 + 3*xu)))*loopba(6))/
     -   (xs**2*xt**2) - 
     -  (2*(2*xs**4 + 4*xs**3*xu + 
     -       8*(4 - 3*xu)*xu**2 + 
     -       xs*xu**2*(-16 + 3*xu) + 
     -       xs**2*xu*(-16 + 5*xu))*loopba(7))/
     -   xu**2 - (2*(2*xs**2*(-4 + xu)**2 + 
     -       xs*(-3*xt**2 - 16*(-4 + xu) - 
     -          6*xt*(-4 + xu))*xu + 
     -       8*(4 + 3*xt)*xu**2)*loopba(8))/xt**2 
     -   - (2*(3*xs**3*xu + 2*xu**4 + 
     -       2*xs*xu**2*(-8 + 5*xu) + 
     -       xs**2*(32 - 40*xu + 11*xu**2))*
     -     loopba(9))/xs**2
      return
      end


      ! need to multiply MQ**4/96
      double complex function ASstu_FormFactor_Massive(xs,xt,xu,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      ASstu_FormFactor_Massive=24 + (24*(-xs**2 + xt*xu)*loopba(1))/
     -   (xt*xu) - (8*
     -     (-2*xs*xt**2 + xt**3 + 3*xs**2*xu - 
     -       2*xt**2*xu + 3*xs*xu**2)*loopba(2))/
     -   (xs*xt*xu) + 
     -  (24*(xt*xu + xs*(xt + xu))*loopba(3))/
     -   (xs*xt) - (2*
     -     (4*xs*xt**2*
     -        (xs*(12 + xs) - 2*(-6 + xs)*xt) + 
     -       xt**2*(13*xs**2 + 12*xs*(-2 + xt) + 
     -          3*xt**2)*xu + 
     -       (8*xs**2*(3 + xs) + 
     -          xs*(-48 + 17*xs)*xt + 
     -          12*(-4 + 3*xs)*xt**2 + 9*xt**3)*
     -        xu**2 + 
     -       (-4*(-6 + xs)*xs + 9*xs*xt + 
     -          6*xt**2)*xu**3)*loopba(4))/
     -   (xt**2*xu**2) - 
     -  ((8*xs**2*xt*
     -        (xs*(12 + xs) - 2*(-6 + xs)*xt) + 
     -       xs**2*(-48*xt + 
     -          (3*xs + 2*xt)*(xs + 9*xt))*xu + 
     -       (18*xs**3 + 16*xt**3 + 
     -          4*xs*xt*(-33 + 10*xt) + 
     -          xs**2*(-84 + 81*xt))*xu**2 + 
     -       xs*(12 + 9*xs + 26*xt)*xu**3 + 
     -       2*(-6*xs + xt)*xu**4 - 6*xu**5)*
     -     loopba(5))/(xs**2*xu**2) + 
     -  ((6*xs**5 + 6*xt**5 + 
     -       xs**4*(9*xt - 10*xu) - 10*xt**4*xu - 
     -       8*xt**2*xu**3 - 
     -       3*xs**2*
     -        (3*(-8 + xt)*xt**2 + 
     -          2*xt*(-14 + 15*xt)*xu + 
     -          8*(2 + xt)*xu**2) + 
     -       xs*xt**2*
     -        (15*xt**2 + 4*(33 - 8*xu)*xu - 
     -          xt*(12 + 31*xu)) - 
     -       xs**3*(15*xt**2 - 8*(-6 + xu)*xu + 
     -          xt*(12 + 37*xu)))*loopba(6))/
     -   (xs**2*xt**2) - 
     -  (2*(2*xs**4 + 4*xs**2*(-4 + xt)**2 - 
     -       32*xs*(-4 + xt)*xt + 64*xt**2 + 
     -       2*(2*xs**2*(2 + xs) - 
     -          3*(-8 + xs)*xs*xt - 
     -          3*(-4 + xs)*xt**2)*xu + 
     -       (32 + xs*(-16 + 5*xs - 6*xt))*
     -        xu**2 + 3*(-8 + xs)*xu**3)*loopba(7)
     -     )/xu**2 - 
     -  (2*(4*xs**2*(8 + xs**2) + 
     -       2*xs**2*(-16 + 7*xs)*xt + 
     -       8*(8 + xs*(-7 + 2*xs))*xt**2 + 
     -       6*(-4 + xs)*xt**3 + 
     -       xs*(64 - 16*xs - 3*(-8 + xt)*xt)*
     -        xu + 2*(-4 + xs)*(-4 + xs - 3*xt)*
     -        xu**2)*loopba(8))/xt**2 - 
     -  (2*(3*xs**3*(-8 + 2*xt + xu) + 
     -       2*(2*xt**4 + xu**4) + 
     -       2*xs*(xt**2*(-16 + 7*xt) + 
     -          xu**2*(-8 + 5*xu)) + 
     -       xs**2*(96 + 8*xt*(-7 + 2*xt) + 
     -          xu*(-40 + 11*xu)))*loopba(9))/
     -   xs**2
      return
      end

      ! need to multiply MQ**2/48
      double complex function DB111stu_FormFactor_Massive(xs,xt,xu,
     $     loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      DB111stu_FormFactor_Massive=8/xu+(4*(4*xs**3*xu + 
     -       xs**2*
     -        (4*xt**2 + 5*xt*xu + 4*xu**2) + 
     -       xt*(-8*xu**2 + xt*xu*(-16 + 3*xu) + 
     -          xt**2*(-8 + 3*xu)) + 
     -       xs*xt*(8*xt**2 + xu*(-8 + 5*xu) + 
     -          2*xt*(-4 + 5*xu)))*loopba(1))/
     -   (xs*xt**2*xu**2) - 
     -  (4*(xs**2*(8 + 4*xt - 3*xu) + 
     -       2*xu*(xt**2 + 4*xu + 
     -          2*xt*(2 + xu)) + 
     -       xs*(8*xt**2 + (16 - 3*xu)*xu + 
     -          xt*(8 + 5*xu)))*loopba(2))/
     -   (xs*xt*xu**2) + 
     -  (4*(4*xs**3 + xs*xt*(-8 + 12*xt + xu) + 
     -       xs**2*(9*xt + 4*xu) + 
     -       xt*(7*xt**2 - 8*xu + xt*(-8 + 9*xu)))
     -      *loopba(3))/(xs*xt**2*xu) - 
     -  (2*(8*xs**4*xu**2 + 
     -       2*xs**3*xu**2*(19*xt + 8*xu) + 
     -       6*xt*(4*xt**4 + 
     -          xt**2*(-4 + xu)*xu**2 - 
     -          12*xt*xu**3 - 4*xu**4 + 
     -          xt**3*xu*(4 + xu)) + 
     -       xs**2*(-4*xt**4 + 6*xt**3*xu + 
     -          33*xt**2*xu**2 + 8*xu**4 + 
     -          12*xt*xu**2*(-2 + 5*xu)) + 
     -       xs*xt*(24*xt**3 - 12*xt**4 + 
     -          3*xt**2*xu*(8 + 5*xu) + 
     -          2*xu**3*(-24 + 7*xu) + 
     -          xt*xu**2*(-48 + 23*xu)))*loopba(4)
     -     )/(xt**3*xu**3) + 
     -  (2*(6*xs**4*(4 + xu) - 
     -       3*xs*xt*xu**2*(-8 + 5*xt + 3*xu) + 
     -       3*xs**2*xt*
     -        (4*xt**2 - (-8 + xu)*xu + 
     -          2*xt*(8 + xu)) + 
     -       xs**3*(4*xt**2 + 6*xu*(4 + xu) + 
     -          6*xt*(12 + xu)) - 
     -       2*xt*xu**2*
     -        (6*xt**2 + 3*(-4 + xu)*xu + 
     -          xt*(-12 + 7*xu)))*loopba(5))/
     -   (xs**2*xu**3) - 
     -  ((24*xs**5 + xs**4*(70*xt + 32*xu) + 
     -       xs**2*xt*
     -        (-10*xt**2 + xt*(-72 + xu) + 
     -          2*xu*(-48 + 7*xu)) + 
     -       4*xt**3*
     -        (6*xt**2 + 3*(-4 + xu)*xu + 
     -          xt*(-12 + 7*xu)) + 
     -       xs**3*(21*xt**2 + 8*xu**2 + 
     -          12*xt*(-6 + 7*xu)) + 
     -       3*xs*xt*
     -        (13*xt**3 - 24*xt*xu - 8*xu**2 + 
     -          xt**2*(-32 + 9*xu)))*loopba(6))/
     -   (xs**2*xt**3) + 
     -  (2*(xs**2*(16 - 2*xt**2 - 6*xu + 
     -          xt*(4 + 3*xu)) + 
     -       xt*(24*xt**2 + 3*(-4 + xu)*xu**2 + 
     -          xt*(16 + 12*xu + 3*xu**2)) + 
     -       xs*(28*xt**2 - 6*xt**3 - 12*xu**2 + 
     -          xt*(32 + 6*xu + 6*xu**2)))*
     -     loopba(7))/xu**3 + 
     -  ((-8*xs**5 - 2*xs**4*(11*xt + 8*xu) + 
     -       2*xt**2*xu*
     -        (-16 - 46*xt + 3*xt**2 - 46*xu + 
     -          3*xt*xu) - 
     -       2*xs**3*
     -        (7*xt**2 + 4*xu**2 + 
     -          4*xt*(-5 + 2*xu)) + 
     -       xs**2*xt*
     -        (2*xu*(40 + 7*xu) + xt*(52 + 37*xu))
     -         + xs*xt*
     -        (37*xt**2*xu + 40*xu**2 + 
     -          xt*(-32 + 37*xu**2)))*loopba(8))/
     -   (xt**3*xu) - 
     -  ((4*xt**2*(2*xt**2 + xt*(-10 + xu) - 
     -          10*xu) + 
     -       3*xs**2*
     -        (2*xt**2 + xt*(-4 + xu) - 4*xu) + 
     -       xs*(14*xt**3 + 5*xt**2*(-8 + xu) + 
     -          32*xu + xt*(32 + 8*xu + 3*xu**2)))
     -      *loopba(9))/(xs**2*xu)
      return
      end


      ! need to multiply MQ**2/48
      double complex function DB111tsu_FormFactor_Massive(xs,xt,xu,
     $     loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      DB111tsu_FormFactor_Massive=8/xu+(4*(4*xs**3*xu + 
     -       xs**2*(-8*xt**2 + 2*xt*xu + 
     -          4*xu**2) + 
     -       xt*(-8*xu**2 + xt*xu*(-16 + 3*xu) + 
     -          xt**2*(-8 + 3*xu)) - 
     -       xs*xt*(4*xt**2 + 4*xu*(2 + xu) + 
     -          xt*(8 + 5*xu)))*loopba(1))/
     -   (xs*xt**2*xu**2) + 
     -  (4*(-4*xt**2*xu + 
     -       xs**2*(-8 + 8*xt + 3*xu) + 
     -       xs*(4*xt**2 + xu*(-8 + 3*xu) + 
     -          xt*(-8 + 5*xu)))*loopba(2))/
     -   (xs*xt*xu**2) + 
     -  (4*(7*xs**2 + 5*xt**2 + xt*(-8 + xu) - 
     -       8*xu + xs*(-8 + 12*xt + 9*xu))*
     -     loopba(3))/(xs*xt*xu) + 
     -  (2*xs*(-8*xs**3*xu**2 + 
     -       4*xs**2*
     -        (3*xt**3 - 5*xt*xu**2 - 2*xu**3) + 
     -       3*xt*(16*xt**3 - 
     -          xt**2*(-8 + xu)*xu + 
     -          xt*(8 - 3*xu)*xu**2 - 
     -          2*(-4 + xu)*xu**3) + 
     -       xs*xt*(4*xt**3 - 15*xt*xu**2 + 
     -          2*(12 - 7*xu)*xu**2 + 
     -          6*xt**2*(8 + xu)))*loopba(4))/
     -   (xt**3*xu**3) + 
     -  (2*(12*xs**4*(-2 + xt) + 
     -       xs*xu**2*
     -        (-41*xt**2 + xt*(48 - 23*xu) + 
     -          72*xu) + 
     -       xs**3*(-24*xt + 4*xt**2 - 
     -          6*xu*(4 + xu)) + 
     -       2*xu**2*
     -        (-19*xt**3 + xt*(24 - 7*xu)*xu + 
     -          12*xu**2 - 6*xt**2*(-2 + 5*xu)) - 
     -       3*xs**2*xu*
     -        (2*xt**2 + 2*(-4 + xu)*xu + 
     -          xt*(8 + 5*xu)))*loopba(5))/
     -   (xs**2*xu**3) - 
     -  ((16*xs**6 + 
     -       xs**2*xt**2*
     -        (21*xt**2 + xt*(-72 + xu) - 72*xu) 
     -        + 8*xs**5*(5*xt + 2*xu) + 
     -       xs**4*xt*(-48 + 39*xt + 28*xu) + 
     -       8*xt**4*
     -        (3*xt**2 + 4*xt*xu + xu**2) + 
     -       2*xs*xt**2*
     -        (35*xt**3 - 12*xu**2 + 
     -          xt*xu*(-48 + 7*xu) + 
     -          6*xt**2*(-6 + 7*xu)) + 
     -       xs**3*xt*
     -        (-10*xt**2 + 12*(-4 + xu)*xu + 
     -          3*xt*(-32 + 9*xu)))*loopba(6))/
     -   (xs**3*xt**3) + 
     -  (2*(-6*xs**3*(-4 + xt) + 
     -       xs**2*(16 + 28*xt - 2*xt**2 + 
     -          12*xu + 3*xu**2) - 
     -       2*xt*(6*xu**2 + xt*(-8 + 3*xu)) + 
     -       xs*(3*(-4 + xu)*xu**2 + 
     -          xt**2*(4 + 3*xu) + 
     -          xt*(32 + 6*xu + 6*xu**2)))*
     -     loopba(7))/xu**3 - 
     -  ((8*xs**5 + xs**2*xt*
     -        (6*xt**2 + 5*xt*(-8 + xu) - 40*xu) 
     -        + 4*(8 - 3*xt)*xt**2*xu + 
     -       8*xs**4*(2*xt + xu) + 
     -       2*xs**3*xt*(-20 + 7*xt + 2*xu) + 
     -       xs*xt**2*
     -        (32 + 3*xt*(-4 + xu) + 8*xu + 
     -          3*xu**2))*loopba(8))/(xt**3*xu) + 
     -  ((6*xs**3*xt*xu + 
     -       xs**2*xu*
     -        (-32 + 37*xt**2 + xt*(-92 + 6*xu)) 
     -        + 2*xt**2*
     -        (-11*xt**3 + xt**2*(20 - 8*xu) + 
     -          20*xu**2 + xt*xu*(40 + 7*xu)) - 
     -       xs*(22*xt**4 + 32*xu**2 + 
     -          4*xt*xu*(16 + 23*xu) - 
     -          xt**3*(52 + 37*xu) + 
     -          xt**2*(32 - 37*xu**2)))*loopba(9))
     -    /(xs**2*xt*xu)
      return
      end


      ! need to multiply MQ**2/48
      double complex function DB111ust_FormFactor_Massive(xs,xt,xu,
     $     loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      DB111ust_FormFactor_Massive=8/xt+(4*(4*xs**3*xt + 
     -       2*xs**2*
     -        (2*xt**2 + xt*xu - 4*xu**2) + 
     -       xu*(-8*xu**2 + xt*xu*(-16 + 3*xu) + 
     -          xt**2*(-8 + 3*xu)) - 
     -       xs*xu*(4*xt**2 + 4*xu*(2 + xu) + 
     -          xt*(8 + 5*xu)))*loopba(1))/
     -   (xs*xt**2*xu**2) + 
     -  (4*(7*xs**2 + xt*(-8 + xu) + 
     -       xu*(-8 + 5*xu) + 
     -       xs*(-8 + 9*xt + 12*xu))*loopba(2))/
     -   (xs*xt*xu) + 
     -  (4*(-4*xt*xu**2 + 
     -       xs**2*(-8 + 3*xt + 8*xu) + 
     -       xs*(3*xt**2 + 4*(-2 + xu)*xu + 
     -          xt*(-8 + 5*xu)))*loopba(3))/
     -   (xs*xt**2*xu) - 
     -  (2*xs*(8*xs**3*xt**2 + 
     -       4*xs**2*
     -        (2*xt**3 + 5*xt**2*xu - 3*xu**3) + 
     -       3*xu*(2*xt**4 + xt**2*(-8 + xu)*xu - 
     -          8*xt*xu**2 - 16*xu**3 + 
     -          xt**3*(-8 + 3*xu)) - 
     -       xs*xu*(-14*xt**3 + 6*xt*xu**2 + 
     -          4*xu**2*(12 + xu) - 
     -          3*xt**2*(-8 + 5*xu)))*loopba(4))/
     -   (xt**3*xu**3) - 
     -  ((16*xs**6 + 8*xs**5*(2*xt + 5*xu) + 
     -       xs**4*xu*(-48 + 28*xt + 39*xu) + 
     -       8*xu**4*
     -        (xt**2 + 4*xt*xu + 3*xu**2) + 
     -       xs**2*xu**2*
     -        (xt*(-72 + xu) + 3*xu*(-24 + 7*xu)) 
     -        + xs**3*xu*
     -        (12*xt**2 - 2*xu*(48 + 5*xu) + 
     -          3*xt*(-16 + 9*xu)) + 
     -       2*xs*xu**2*
     -        (xt**2*(-12 + 7*xu) + 
     -          6*xt*xu*(-8 + 7*xu) + 
     -          xu**2*(-36 + 35*xu)))*loopba(5))/
     -   (xs**3*xu**3) + 
     -  (2*(12*xs**4*(-2 + xu) + 
     -       xs*xt**2*
     -        (xt*(72 - 23*xu) + (48 - 41*xu)*xu) 
     -        - 2*xs**3*
     -        (12*xt + 3*xt**2 - 2*(-6 + xu)*xu) 
     -        - 3*xs**2*xt*
     -        (2*xt**2 + 2*xu*(4 + xu) + 
     -          xt*(-8 + 5*xu)) + 
     -       2*xt**2*
     -        (xt**2*(12 - 7*xu) + 
     -          (12 - 19*xu)*xu**2 - 
     -          6*xt*xu*(-4 + 5*xu)))*loopba(6))/
     -   (xs**2*xt**3) - 
     -  ((8*xs**5 + 4*xt*(8 - 3*xu)*xu**2 + 
     -       8*xs**4*(xt + 2*xu) + 
     -       2*xs**3*xu*(-20 + 2*xt + 7*xu) + 
     -       xs**2*xu*
     -        (5*xt*(-8 + xu) + 2*xu*(-20 + 3*xu))
     -         + xs*xu**2*
     -        (32 + 3*xt**2 - 12*xu + 
     -          xt*(8 + 3*xu)))*loopba(7))/
     -   (xt*xu**3) + 
     -  (2*(-6*xs**3*(-4 + xu) - 
     -       2*xu*(6*xt**2 - 8*xu + 3*xt*xu) + 
     -       xs**2*(16 + 12*xt + 3*xt**2 + 
     -          28*xu - 2*xu**2) + 
     -       xs*(3*xt**3 + 6*xt**2*(-2 + xu) + 
     -          3*xt*xu*(2 + xu) + 4*xu*(8 + xu)))
     -      *loopba(8))/xt**3 + 
     -  ((6*xs**3*xt*xu + 
     -       xs**2*xt*
     -        (-32 + (-92 + 6*xt)*xu + 37*xu**2) 
     -        + 2*xu**2*
     -        (-8*xt*(-5 + xu)*xu + 
     -          (20 - 11*xu)*xu**2 + 
     -          xt**2*(20 + 7*xu)) + 
     -       xs*(-2*xu**2*
     -           (16 - 26*xu + 11*xu**2) + 
     -          xt*xu*(-64 + 37*xu**2) + 
     -          xt**2*(-32 - 92*xu + 37*xu**2)))*
     -     loopba(9))/(xs**2*xt*xu)
      return
      end

      ! need to multiply MQ**2/48
      double complex function DBhat111stu_FormFactor_Massive(xs,xt,xu,
     $     loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DBhat111stu_FormFactor_Massive=12/xu - (4*(-8*xs*xt + xs**2*xt - 
     -       8*xt**2 + 4*xs*xt**2 - 4*xs**2*xu - 
     -       8*xt*xu + 3*xt**2*xu - 4*xs*xu**2)*
     -     loopba(1))/(xt**2*xu**2) - 
     -  (4*(8*xs**2 + 8*xs*xt + 4*xs**2*xt + 
     -       8*xs*xt**2 + xt**3 + 16*xs*xu - 
     -       3*xs**2*xu + 8*xt*xu + 5*xs*xt*xu + 
     -       2*xt**2*xu + 8*xu**2 - 3*xs*xu**2 + 
     -       4*xt*xu**2)*loopba(2))/(xs*xt*xu**2) 
     -   + (2*(8*xs**3 - 16*xs*xt + 15*xs**2*xt - 
     -       16*xt**2 + 21*xs*xt**2 + 14*xt**3 + 
     -       8*xs**2*xu - 16*xt*xu + xs*xt*xu + 
     -       20*xt**2*xu)*loopba(3))/(xs*xt**2*xu)
     -    - ((24*xs**2*xt**3 + 72*xs*xt**4 - 
     -       12*xs**2*xt**4 + 48*xt**5 - 
     -       24*xs*xt**5 + 48*xs*xt**3*xu + 
     -       24*xs**2*xt**3*xu + 48*xt**4*xu + 
     -       3*xs*xt**4*xu + 3*xt**5*xu + 
     -       16*xs**4*xu**2 - 48*xs**2*xt*xu**2 + 
     -       80*xs**3*xt*xu**2 - 
     -       120*xs*xt**2*xu**2 + 
     -       82*xs**2*xt**2*xu**2 - 
     -       96*xt**3*xu**2 + 48*xs*xt**3*xu**2 + 
     -       21*xt**4*xu**2 + 32*xs**3*xu**3 - 
     -       96*xs*xt*xu**3 + 
     -       120*xs**2*xt*xu**3 - 
     -       144*xt**2*xu**3 + 
     -       46*xs*xt**2*xu**3 + 18*xt**3*xu**3 + 
     -       16*xs**2*xu**4 - 48*xt*xu**4 + 
     -       28*xs*xt*xu**4)*loopba(4))/
     -   (xt**3*xu**3) + 
     -  ((48*xs**4 + 120*xs**3*xt + 
     -       72*xs**2*xt**2 + 12*xs**3*xt**2 + 
     -       24*xs**2*xt**3 + 48*xs**3*xu + 
     -       12*xs**4*xu + 48*xs**2*xt*xu + 
     -       3*xs**3*xt*xu + 15*xs**2*xt**2*xu + 
     -       12*xs**3*xu**2 + 72*xs*xt*xu**2 - 
     -       15*xs**2*xt*xu**2 + 48*xt**2*xu**2 - 
     -       34*xs*xt**2*xu**2 - 28*xt**3*xu**2 + 
     -       48*xt*xu**3 - 18*xs*xt*xu**3 - 
     -       28*xt**2*xu**3 - 12*xt*xu**4)*
     -     loopba(5))/(xs**2*xu**3) + 
     -  ((6*xs**5*xt - 12*xs**3*xt**2 + 
     -       12*xs**4*xt**2 - 12*xs**2*xt**3 + 
     -       3*xs**3*xt**3 + 3*xs*xt**5 - 
     -       48*xs**5*xu + 144*xs**3*xt*xu - 
     -       142*xs**4*xt*xu + 
     -       180*xs**2*xt**2*xu - 
     -       68*xs**3*xt**2*xu + 
     -       240*xs*xt**3*xu - 7*xs**2*xt**3*xu + 
     -       96*xt**4*xu - 83*xs*xt**4*xu - 
     -       56*xt**5*xu - 64*xs**4*xu**2 + 
     -       192*xs**2*xt*xu**2 - 
     -       168*xs**3*xt*xu**2 + 
     -       144*xs*xt**2*xu**2 - 
     -       2*xs**2*xt**2*xu**2 + 
     -       96*xt**3*xu**2 - 54*xs*xt**3*xu**2 - 
     -       56*xt**4*xu**2 - 16*xs**3*xu**3 + 
     -       48*xs*xt*xu**3 - 28*xs**2*xt*xu**3 - 
     -       24*xt**3*xu**3)*loopba(6))/
     -   (2.*xs**2*xt**3*xu) + 
     -  (3*xs*(8*xs*xt + 8*xt**2 - 2*xs*xt**2 - 
     -       4*xt**3 - 12*xs*xu + 4*xt*xu + 
     -       4*xs*xt*xu - 8*xu**2 + 3*xt*xu**2)*
     -     loopba(7))/xu**3 - 
     -  (xs*(8*xs**4 - 40*xs**2*xt + 
     -       24*xs**3*xt - 68*xs*xt**2 + 
     -       24*xs**2*xt**2 - 40*xt**3 + 
     -       11*xs*xt**3 + 3*xt**4 + 
     -       16*xs**3*xu - 80*xs*xt*xu + 
     -       16*xs**2*xt*xu - 92*xt**2*xu - 
     -       37*xs*xt**2*xu - 31*xt**3*xu + 
     -       8*xs**2*xu**2 - 40*xt*xu**2 - 
     -       14*xs*xt*xu**2 - 37*xt**2*xu**2)*
     -     loopba(8))/(xt**3*xu) - 
     -  ((32*xs**2 - 24*xs**3 + 32*xs*xt - 
     -       28*xs**2*xt + 3*xs**3*xt - 
     -       56*xs*xt**2 + 11*xs**2*xt**2 - 
     -       40*xt**3 + 18*xs*xt**3 + 10*xt**4 + 
     -       32*xs*xu - 12*xs**2*xu + 
     -       8*xs*xt*xu + 3*xs**2*xt*xu - 
     -       40*xt**2*xu + 5*xs*xt**2*xu + 
     -       4*xt**3*xu + 3*xs*xt*xu**2)*loopba(9)
     -     )/(xs**2*xu)
      return
      end


      ! need to multiply MQ**2/48
      double complex function DBhat111tsu_FormFactor_Massive(xs,xt,xu,
     $     loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DBhat111tsu_FormFactor_Massive=12/xu - (4*(xs**2*xt + 8*xs*xt**2 + 
     -       4*xt**3 - 4*xs**2*xu - 2*xs*xt*xu + 
     -       8*xt**2*xu - 4*xs*xu**2 + 4*xt*xu**2)
     -      *loopba(1))/(xt**2*xu**2) + 
     -  (2*(-16*xs**2 - 16*xs*xt + 16*xs**2*xt + 
     -       10*xs*xt**2 - 16*xs*xu + 
     -       13*xs*xt*xu - 3*xt**2*xu + 3*xt*xu**2
     -       )*loopba(2))/(xs*xt*xu**2) + 
     -  (2*(14*xs**2 + 21*xs*xt + 7*xt**2 + 
     -       20*xs*xu + xt*xu)*loopba(3))/
     -   (xs*xt*xu) - 
     -  (xs*(-72*xs*xt**3 - 24*xs**2*xt**3 - 
     -       72*xt**4 - 12*xs*xt**4 - 
     -       48*xt**3*xu - 15*xs*xt**3*xu + 
     -       9*xt**4*xu + 16*xs**3*xu**2 - 
     -       48*xs*xt*xu**2 + 44*xs**2*xt*xu**2 - 
     -       72*xt**2*xu**2 + 34*xs*xt**2*xu**2 + 
     -       15*xt**3*xu**2 + 16*xs**2*xu**3 - 
     -       48*xt*xu**3 + 28*xs*xt*xu**3 + 
     -       18*xt**2*xu**3 + 12*xt*xu**4)*
     -     loopba(4))/(xt**3*xu**3) - 
     -  ((48*xs**4 + 72*xs**3*xt - 24*xs**4*xt + 
     -       24*xs**2*xt**2 - 12*xs**3*xt**2 + 
     -       48*xs**3*xu + 3*xs**4*xu + 
     -       48*xs**2*xt*xu + 3*xs**3*xt*xu + 
     -       24*xs**2*xt**2*xu - 96*xs**2*xu**2 + 
     -       21*xs**3*xu**2 - 120*xs*xt*xu**2 + 
     -       48*xs**2*xt*xu**2 - 48*xt**2*xu**2 + 
     -       98*xs*xt**2*xu**2 + 80*xt**3*xu**2 - 
     -       144*xs*xu**3 + 18*xs**2*xu**3 - 
     -       96*xt*xu**3 + 46*xs*xt*xu**3 + 
     -       120*xt**2*xu**3 - 48*xu**4 + 
     -       28*xt*xu**4)*loopba(5))/(xs**2*xu**3)
     -    - ((-3*xs**4*xt**2 + 12*xs**2*xt**3 + 
     -       12*xs*xt**4 - 3*xs**2*xt**4 - 
     -       12*xs*xt**5 - 6*xt**6 + 
     -       32*xs**5*xu - 96*xs**3*xt*xu + 
     -       88*xs**4*xt*xu - 
     -       240*xs**2*xt**2*xu + 
     -       83*xs**3*xt**2*xu - 
     -       180*xs*xt**3*xu + 7*xs**2*xt**3*xu - 
     -       144*xt**4*xu + 68*xs*xt**4*xu + 
     -       94*xt**5*xu + 32*xs**4*xu**2 - 
     -       96*xs**2*xt*xu**2 + 
     -       56*xs**3*xt*xu**2 - 
     -       144*xs*xt**2*xu**2 + 
     -       54*xs**2*xt**2*xu**2 - 
     -       192*xt**3*xu**2 + 2*xs*xt**3*xu**2 + 
     -       152*xt**4*xu**2 + 
     -       24*xs**2*xt*xu**3 - 48*xt**2*xu**3 + 
     -       28*xt**3*xu**3)*loopba(6))/
     -   (2.*xs**2*xt**3*xu) + 
     -  (3*(16*xs**3 + 24*xs**2*xt - 4*xs**3*xt + 
     -       8*xs*xt**2 - 2*xs**2*xt**2 + 
     -       8*xs**2*xu - 4*xs*xt*xu - 
     -       12*xt**2*xu + 4*xs*xt**2*xu - 
     -       8*xs*xu**2 + 2*xs**2*xu**2 - 
     -       8*xt*xu**2 + 5*xs*xt*xu**2 + 
     -       2*xs*xu**3)*loopba(7))/xu**3 - 
     -  ((8*xs**5 - 40*xs**3*xt + 18*xs**4*xt + 
     -       32*xs*xt**2 - 56*xs**2*xt**2 + 
     -       18*xs**3*xt**2 + 32*xt**3 - 
     -       28*xs*xt**3 + 11*xs**2*xt**3 - 
     -       24*xt**4 + 3*xs*xt**4 + 8*xs**4*xu - 
     -       40*xs**2*xt*xu + 4*xs**3*xt*xu + 
     -       32*xt**2*xu + 8*xs*xt**2*xu + 
     -       5*xs**2*xt**2*xu - 12*xt**3*xu + 
     -       3*xs*xt**3*xu + 3*xs*xt**2*xu**2)*
     -     loopba(8))/(xt**3*xu) - 
     -  ((32*xs**2*xt + 32*xs*xt**2 - 
     -       40*xs**2*xt**2 + 3*xs**3*xt**2 - 
     -       68*xs*xt**3 + 11*xs**2*xt**3 - 
     -       40*xt**4 + 32*xs*xt**4 + 24*xt**5 + 
     -       32*xs**2*xu + 64*xs*xt*xu + 
     -       92*xs**2*xt*xu - 6*xs**3*xt*xu - 
     -       37*xs**2*xt**2*xu - 80*xt**3*xu - 
     -       37*xs*xt**3*xu + 16*xt**4*xu + 
     -       32*xs*xu**2 + 92*xs*xt*xu**2 - 
     -       6*xs**2*xt*xu**2 - 40*xt**2*xu**2 - 
     -       37*xs*xt**2*xu**2 - 14*xt**3*xu**2)*
     -     loopba(9))/(xs**2*xt*xu)
      return
      end


      ! need to multiply MQ**2/48
      double complex function DBhat111ust_FormFactor_Massive(xs,xt,xu,
     $     loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DBhat111ust_FormFactor_Massive=12/xt-(4*(-4*xs**2*xt-4*xs*xt**2+ 
     -       xs**2*xu - 2*xs*xt*xu + 4*xt**2*xu + 
     -       8*xs*xu**2 + 8*xt*xu**2 + 4*xu**3)*
     -     loopba(1))/(xt**2*xu**2) + 
     -  (2*(14*xs**2 + 20*xs*xt + 21*xs*xu + 
     -       xt*xu + 7*xu**2)*loopba(2))/
     -   (xs*xt*xu) + 
     -  (2*(-16*xs**2 - 16*xs*xt - 16*xs*xu + 
     -       16*xs**2*xu + 13*xs*xt*xu + 
     -       3*xt**2*xu + 10*xs*xu**2 - 3*xt*xu**2
     -       )*loopba(3))/(xs*xt**2*xu) - 
     -  (xs*(-48*xs*xt**2 + 28*xs**2*xt**2 - 
     -       48*xt**3 + 28*xs*xt**3 + 12*xt**4 - 
     -       72*xt**2*xu + 34*xs*xt**2*xu + 
     -       18*xt**3*xu - 72*xs*xu**2 - 
     -       24*xs**2*xu**2 - 48*xt*xu**2 - 
     -       15*xs*xt*xu**2 + 15*xt**2*xu**2 - 
     -       72*xu**3 - 12*xs*xu**3 + 9*xt*xu**3)*
     -     loopba(4))/(xt**3*xu**2) - 
     -  ((32*xs**5*xt + 32*xs**4*xt**2 - 
     -       96*xs**3*xt*xu + 88*xs**4*xt*xu - 
     -       96*xs**2*xt**2*xu + 
     -       56*xs**3*xt**2*xu + 
     -       24*xs**2*xt**3*xu - 3*xs**4*xu**2 - 
     -       240*xs**2*xt*xu**2 + 
     -       83*xs**3*xt*xu**2 - 
     -       144*xs*xt**2*xu**2 + 
     -       54*xs**2*xt**2*xu**2 - 
     -       48*xt**3*xu**2 + 12*xs**2*xu**3 - 
     -       180*xs*xt*xu**3 + 7*xs**2*xt*xu**3 - 
     -       192*xt**2*xu**3 + 2*xs*xt**2*xu**3 + 
     -       28*xt**3*xu**3 + 12*xs*xu**4 - 
     -       3*xs**2*xu**4 - 144*xt*xu**4 + 
     -       68*xs*xt*xu**4 + 152*xt**2*xu**4 - 
     -       12*xs*xu**5 + 94*xt*xu**5 - 6*xu**6)*
     -     loopba(5))/(2.*xs**2*xt*xu**3) - 
     -  ((48*xs**4 + 48*xs**3*xt + 3*xs**4*xt - 
     -       96*xs**2*xt**2 + 21*xs**3*xt**2 - 
     -       144*xs*xt**3 + 18*xs**2*xt**3 - 
     -       48*xt**4 + 72*xs**3*xu - 
     -       24*xs**4*xu + 48*xs**2*xt*xu + 
     -       3*xs**3*xt*xu - 120*xs*xt**2*xu + 
     -       48*xs**2*xt**2*xu - 96*xt**3*xu + 
     -       46*xs*xt**3*xu + 28*xt**4*xu + 
     -       24*xs**2*xu**2 - 12*xs**3*xu**2 + 
     -       24*xs**2*xt*xu**2 - 48*xt**2*xu**2 + 
     -       98*xs*xt**2*xu**2 + 
     -       120*xt**3*xu**2 + 80*xt**2*xu**3)*
     -     loopba(6))/(xs**2*xt**3) - 
     -  ((-40*xs**3 + 10*xs**4 - 40*xs**2*xt + 
     -       4*xs**3*xt + 32*xs*xu - 
     -       56*xs**2*xu + 18*xs**3*xu + 
     -       32*xt*xu + 8*xs*xt*xu + 
     -       5*xs**2*xt*xu + 3*xs*xt**2*xu + 
     -       32*xu**2 - 28*xs*xu**2 + 
     -       11*xs**2*xu**2 - 12*xt*xu**2 + 
     -       3*xs*xt*xu**2 - 24*xu**3 + 3*xs*xu**3
     -       )*loopba(7))/(xt*xu**2) + 
     -  (3*xu*(8*xs**2 - 4*xs**3 + 4*xs*xt - 
     -       8*xt**2 + 3*xs*xt**2 + 8*xs*xu - 
     -       2*xs**2*xu - 12*xt*xu + 4*xs*xt*xu)*
     -     loopba(8))/xt**3 + 
     -  ((-32*xs**2 - 32*xs*xt - 92*xs**2*xt + 
     -       6*xs**3*xt - 92*xs*xt**2 + 
     -       6*xs**2*xt**2 - 32*xs*xu + 
     -       40*xs**2*xu - 3*xs**3*xu + 
     -       37*xs**2*xt*xu + 40*xt**2*xu + 
     -       37*xs*xt**2*xu + 68*xs*xu**2 - 
     -       11*xs**2*xu**2 + 80*xt*xu**2 + 
     -       37*xs*xt*xu**2 + 14*xt**2*xu**2 + 
     -       40*xu**3 - 32*xs*xu**3 - 
     -       16*xt*xu**3 - 24*xu**4)*loopba(9))/
     -   (xs**2*xt)
      return
      end


      double complex function DC2111stu_FormFactor_Massive(xs,xt,xu,
     $     loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      DC2111stu_FormFactor_Massive=-((xs*xu**2*(xs**3*(-2 + 3*xt) + 
     -          2*xt*
     -           (3*xt**2 + 4*xt*xu + xu**2) + 
     -          xs*(-3*xt**3 + xt**2*(8 - 9*xu) - 
     -             3*xt*(-2 + xu)*xu + 2*xu**2)))/
     -      xt**2 + (6*xs**2*
     -        (2*xs**5*xu**3 + 
     -          3*xs**4*xu**3*(2*xt + xu) + 
     -          2*xs*xt*
     -           (xt**5 + xt**4*xu - xt*xu**4 - 
     -             xu**5) + 
     -          xt**2*
     -           (xt**5 + xt**4*xu + 
     -             xt**2*xu**3 + 3*xt*xu**4 + 
     -             xu**5) + 
     -          xs**2*xt*
     -           (xt**4 + xt**3*xu + 
     -             2*xt**2*xu**3 + 
     -             (-6 + xu)*xu**4 + 
     -             xt*xu**3*(-5 + 3*xu)) + 
     -          xs**3*xu**3*
     -           (6*xt**2 + xu**2 + 
     -             xt*(-4 + 6*xu)))*loopba(4))/
     -      xt**4 + (6*
     -        (2*xs**6 - xs*xt**2*xu**3 + 
     -          2*xs**4*xt*(4*xt + xu) + 
     -          xs**5*(7*xt + xu) + 
     -          xs**3*(3*xt**3 + xt**2*xu + xu**3)
     -          )*loopba(5))/xt + 
     -     (3*xs*xu**3*
     -        (-(xs**5*(xt - 5*xu)) + 
     -          xt**2*xu**2*(3*xt + xu) - 
     -          3*xs**4*
     -           (xt**2 - 4*xt*xu - 2*xu**2) - 
     -          xs*xt*
     -           (xt**3 + 2*xt*xu**2 + 2*xu**3) + 
     -          xs**2*xt*
     -           (-xt**3 + (-12 + xu)*xu**2 + 
     -             2*xt**2*(1 + xu) + 
     -             2*xt*xu*(-5 + 2*xu)) + 
     -          xs**3*
     -           (-3*xt**3 + 10*xt*(-1 + xu)*xu + 
     -             xu**3 + xt**2*(1 + 9*xu)))*
     -        loopba(6))/xt**4 + 
     -     3*xs**2*(xs*(-2 + xt) - 2*xt)*xu**2*
     -      loopba(7) + 
     -     (3*xs**2*xu**3*
     -        (2*xs**6 + 
     -          xs**3*xt*
     -           (2*xt**2 + xt*(-15 + xu) - 12*xu)
     -            + 3*xs**5*(2*xt + xu) - 
     -          2*xt**2*xu*(xt**2 - xu + xt*xu) + 
     -          xs*xt**2*
     -           (2*xt*(1 + xu) + 3*xu*(2 + xu)) 
     -           + xs**4*
     -           (6*xt**2 + xu**2 + 
     -             xt*(-8 + 5*xu)) - 
     -          xs**2*xt*
     -           (4*xu**2 + xt**2*(7 + xu) + 
     -             xt*(-4 + 8*xu + xu**2)))*
     -        loopba(8))/xt**4 + 
     -     3*xs*(xt*(-2 + xu) - 2*xu)*xu**3*
     -      loopba(9))/(3.*xs**3*xu**4)
      return
      end

      ! need to multiply 1/96
      double complex function A1stu_FormFactor_Massless(shat,that
     $     ,uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      A1stu_FormFactor_Massless=8-(8*(shat**2-3*that*uhat)*loopba(1))/
     -   (that*uhat) - 
     -  (8*that**2*loopba(2))/(shat*uhat) - 
     -  (4*(3*shat**2 - 2*that*uhat + 
     -       shat*(3*that + uhat))*loopba(3))/
     -   (shat*that) - 
     -  (2*(3*shat*that**2*uhat*(that + 6*uhat) + 
     -       3*that**2*uhat*
     -        (that**2 + 3*that*uhat + 2*uhat**2) 
     -        - 4*shat**2*
     -        (that**3 - 3*that**2*uhat - 
     -          3*that*uhat**2 + uhat**3))*
     -     loopba(4))/(that**2*uhat**2) + 
     -  (2*that*(shat**3*(4*that - 9*uhat) + 
     -       3*shat**2*(that - 3*uhat)*uhat + 
     -       4*that*uhat**3)*loopba(5))/
     -   (shat**2*uhat**2) - 
     -  ((-8*that**3*uhat**2 + 
     -       3*shat**2*that**2*(that + 9*uhat) + 
     -       shat**3*
     -        (3*that**2 + 24*that*uhat - 
     -          8*uhat**2))*loopba(6))/
     -   (shat**2*that**2) - 
     -  (2*shat*that*
     -     (2*shat*(that - 3*uhat) - 3*uhat**2)*
     -     loopba(7))/uhat**2 - 
     -  (2*shat*(3*that**3 + 
     -       shat*(3*that**2 - 6*that*uhat + 
     -          2*uhat**2))*loopba(8))/that**2 + 
     -  (2*that*(3*shat**3 - 2*that*uhat**2 + 
     -       3*shat**2*(that + 2*uhat))*loopba(9))
     -    /shat**2
      return
      end

      ! need to multiply 1/96
      double complex function A1tsu_FormFactor_Massless(shat,that,
     $     uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      A1tsu_FormFactor_Massless=8-(8*shat**2*loopba(1))/(that*uhat)- 
     -  (8*(that**2 - 3*shat*uhat)*loopba(2))/
     -   (shat*uhat) - 
     -  (4*(shat*(3*that - 2*uhat) + 
     -       that*(3*that + uhat))*loopba(3))/
     -   (shat*that) + 
     -  (8*shat**2*(that**3 + 3*that**2*uhat + 
     -       uhat**3)*loopba(4))/(that**2*uhat**2)
     -    - (2*(3*shat**4*uhat + 
     -       12*shat*that**2*uhat**2 - 
     -       4*that**2*uhat**3 + 
     -       6*shat**2*uhat*
     -        (2*that**2 + 3*that*uhat + uhat**2) 
     -        + shat**3*
     -        (-4*that**2 + 3*that*uhat + 
     -          9*uhat**2))*loopba(5))/
     -   (shat**2*uhat**2) - 
     -  ((24*shat*that**3*uhat - 
     -       8*that**3*uhat**2 + 
     -       3*shat**2*that**2*(that + 9*uhat) + 
     -       shat**3*(3*that**2 - 8*uhat**2))*
     -     loopba(6))/(shat**2*that**2) - 
     -  (2*shat*that*
     -     (2*shat*that - 3*uhat*(2*that + uhat))*
     -     loopba(7))/uhat**2 + 
     -  (2*shat*(3*that**2*(that + 2*uhat) + 
     -       shat*(3*that**2 - 2*uhat**2))*
     -     loopba(8))/that**2 - 
     -  (2*that*(3*shat**3 + 3*shat**2*that - 
     -       6*shat*that*uhat + 2*that*uhat**2)*
     -     loopba(9))/shat**2
      return
      end

      ! need to multiply 1/96
      double complex function A1ust_FormFactor_Massless(shat,that
     $     ,uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      A1ust_FormFactor_Massless=8-(8*shat**2*loopba(1))/(that*uhat)- 
     -  (4*(shat*(-2*that + 3*uhat) + 
     -       uhat*(that + 3*uhat))*loopba(2))/
     -   (shat*uhat) + 
     -  (8*(3*shat*that - uhat**2)*loopba(3))/
     -   (shat*that) + 
     -  (8*shat**2*(that**3 + 3*that*uhat**2 + 
     -       uhat**3)*loopba(4))/(that**2*uhat**2)
     -    + ((-24*shat*that*uhat**3 + 
     -       8*that**2*uhat**3 - 
     -       3*shat**2*uhat**2*(9*that + uhat) + 
     -       shat**3*(8*that**2 - 3*uhat**2))*
     -     loopba(5))/(shat**2*uhat**2) - 
     -  (2*(3*shat**4*that + 
     -       12*shat*that**2*uhat**2 - 
     -       4*that**3*uhat**2 + 
     -       shat**3*
     -        (9*that**2 + 3*that*uhat - 
     -          4*uhat**2) + 
     -       6*shat**2*that*
     -        (that**2 + 3*that*uhat + 2*uhat**2))
     -      *loopba(6))/(shat**2*that**2) + 
     -  (2*shat*(3*uhat**2*(2*that + uhat) + 
     -       shat*(-2*that**2 + 3*uhat**2))*
     -     loopba(7))/uhat**2 - 
     -  (2*shat*uhat*
     -     (-3*that**2 + 2*shat*uhat - 
     -       6*that*uhat)*loopba(8))/that**2 - 
     -  (2*uhat*(3*shat**3 + 3*shat**2*uhat - 
     -       6*shat*that*uhat + 2*that**2*uhat)*
     -     loopba(9))/shat**2
      return
      end

      ! need to multiply 1/96
      double complex function ASstu_FormFactor_Massless(shat,that,uhat,
     $     loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      ASstu_FormFactor_Massless=24 + (24*(-shat**2 + that*uhat)*
     -     loopba(1))/(that*uhat) + 
     -  (4*(2*shat*that - 4*that**2 + 
     -       3*shat*uhat - that*uhat - 3*uhat**2)*
     -     loopba(2))/(shat*uhat) - 
     -  (12*(shat**2 + that**2 + uhat**2)*
     -     loopba(3))/(shat*that) + 
     -  (24*shat*(-(that**2*uhat**2) + 
     -       shat*(that**3 + uhat**3))*loopba(4))/
     -   (that**2*uhat**2) - 
     -  (3*(-8*that**2*uhat**3 + 
     -       8*shat*that*uhat**2*(that + uhat) + 
     -       shat**2*uhat**2*(21*that + 5*uhat) + 
     -       shat**3*(-8*that**2 + 5*uhat**2))*
     -     loopba(5))/(shat**2*uhat**2) - 
     -  (24*uhat*(shat**3*that + 
     -       that**2*uhat**2 + 
     -       shat**2*(3*that**2 + uhat**2) + 
     -       shat*(that**3 - 2*that*uhat**2))*
     -     loopba(6))/(shat**2*that**2) + 
     -  (6*shat*(uhat**2*(2*that + uhat) + 
     -       shat*(-2*that**2 + uhat**2))*
     -     loopba(7))/uhat**2 + 
     -  (6*shat*uhat*
     -     (2*shat*(that - uhat) + 
     -       that*(3*that + 2*uhat))*loopba(8))/
     -   that**2 - (6*uhat*
     -     (shat**3 + shat**2*uhat + 
     -       2*that**2*uhat)*loopba(9))/shat**2
      return
      end


      ! need to multiply 1/48
      double complex function DB111stu_FormFactor_Massless(shat,that
     $     ,uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DB111stu_FormFactor_Massless=8/uhat+(4*(8*that**3+9*that**2*uhat+ 
     -       11*that*uhat**2 + 4*uhat**3 + 
     -       shat*(4*that**2 + 3*that*uhat + 
     -          4*uhat**2))*loopba(1))/
     -   (that**2*uhat**2) - 
     -  (4*(shat**2*(4*that - 3*uhat) + 
     -       2*that*uhat**2 + 
     -       shat*(8*that**2 + 3*that*uhat - 
     -          3*uhat**2))*loopba(2))/
     -   (shat*that*uhat**2) + 
     -  (4*(shat**2*(3*that - 4*uhat) + 
     -       that**2*(3*that + 5*uhat) + 
     -       shat*(6*that**2 - 5*that*uhat - 
     -          4*uhat**2))*loopba(3))/
     -   (shat*that**2*uhat) - 
     -  (2*shat*(-12*that**4 + 
     -       6*shat**2*uhat**2 + 
     -       9*that**2*uhat**2 - 
     -       17*that*uhat**3 - 26*uhat**4 + 
     -       shat*(-4*that**3 + 6*that**2*uhat + 
     -          9*that*uhat**2 - 12*uhat**3))*
     -     loopba(4))/(that**2*uhat**3) + 
     -  (2*(6*shat**4*uhat - 
     -       3*shat*that*uhat**2*
     -        (that + 3*uhat) - 
     -       2*that*uhat**3*(that + 3*uhat) + 
     -       3*shat**2*that*
     -        (4*that**2 + 2*that*uhat - uhat**2) 
     -        + shat**3*
     -        (4*that**2 + 6*that*uhat + 
     -          6*uhat**2))*loopba(5))/
     -   (shat**2*uhat**3) - 
     -  ((2*shat**4*(9*that - 8*uhat) + 
     -       4*that**3*uhat*(that + 3*uhat) + 
     -       3*shat*that**3*(5*that + 9*uhat) + 
     -       shat**3*
     -        (21*that**2 - 24*that*uhat - 
     -          32*uhat**2) + 
     -       shat**2*
     -        (18*that**3 - 27*that**2*uhat - 
     -          58*that*uhat**2 - 16*uhat**3))*
     -     loopba(6))/(shat**2*that**3) - 
     -  (2*shat*that*
     -     (2*shat*that + 6*that**2 - 
     -       3*shat*uhat - 3*uhat**2)*loopba(7))/
     -   uhat**3 + (shat*
     -     (3*that**3 + 
     -       shat**2*(6*that - 8*uhat) - 
     -       9*that**2*uhat - 26*that*uhat**2 - 
     -       8*uhat**3 + 
     -       shat*(9*that**2 - 12*that*uhat - 
     -          16*uhat**2))*loopba(8))/that**3 + 
     -  (that*(3*shat**2 + 
     -       3*shat*(that + 3*uhat) + 
     -       2*uhat*(that + 3*uhat))*loopba(9))/
     -   shat**2
      return
      end


      ! need to multiply 1/48
      double complex function DB111tsu_FormFactor_Massless(shat,that,
     $     uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DB111tsu_FormFactor_Massless=8/uhat-(8*(4*shat*that + 2*that**2 + 
     -       3*that*uhat + uhat**2)*loopba(1))/
     -   (that*uhat**2) + 
     -  (4*(8*shat**2 + 4*shat*that + 
     -       9*shat*uhat + 3*that*uhat + 7*uhat**2
     -       )*loopba(2))/(shat*uhat**2) + 
     -  (4*(3*shat**2 + 6*shat*that + 3*that**2 + 
     -       5*shat*uhat - that*uhat)*loopba(3))/
     -   (shat*that*uhat) + 
     -  (2*shat*(-3*that*uhat**2*
     -        (that**2 + 3*that*uhat + 2*uhat**2) 
     -        + 4*shat**2*
     -        (3*that**3 + 2*uhat**3) + 
     -       shat*(4*that**4 + 6*that**3*uhat - 
     -          3*that**2*uhat**2 + 
     -          6*that*uhat**3 + 8*uhat**4))*
     -     loopba(4))/(that**3*uhat**3) + 
     -  (2*that*(12*shat**4 + 4*shat**3*that - 
     -       3*shat**2*uhat*(2*that + 3*uhat) + 
     -       shat*uhat**2*(-9*that + 17*uhat) + 
     -       2*uhat**2*
     -        (-3*that**2 + 6*that*uhat + 
     -          13*uhat**2))*loopba(5))/
     -   (shat**2*uhat**3) + 
     -  ((16*shat**4*uhat + 
     -       shat*that**3*(-21*that + 43*uhat) - 
     -       3*shat**2*that*
     -        (6*that**2 + 9*that*uhat + 
     -          4*uhat**2) + 
     -       shat**3*
     -        (-15*that**2 + 12*that*uhat + 
     -          16*uhat**2) + 
     -       2*that**3*
     -        (-9*that**2 + 12*that*uhat + 
     -          29*uhat**2))*loopba(6))/
     -   (shat**2*that**3) - 
     -  (2*shat**2*that*
     -     (6*shat + 2*that + 3*uhat)*loopba(7))/
     -   uhat**3 - (shat*
     -     (8*shat**2*uhat - 
     -       3*that*(that**2 + 3*that*uhat + 
     -          2*uhat**2) + 
     -       shat*(-3*that**2 + 6*that*uhat + 
     -          8*uhat**2))*loopba(8))/that**3 + 
     -  (that*(3*shat**2 + 9*shat*that + 
     -       6*that**2 - 17*shat*uhat - 
     -       12*that*uhat - 26*uhat**2)*loopba(9))
     -    /shat**2
      return
      end


      ! need to multiply 1/48
      double complex function DB111ust_FormFactor_Massless(shat,that,
     $     uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DB111ust_FormFactor_Massless=8/that-(8*(that**2 + 3*that*uhat+ 
     -       2*uhat*(2*shat + uhat))*loopba(1))/
     -   (that**2*uhat) + 
     -  (4*(3*shat**2 + 5*shat*that + 
     -       6*shat*uhat - that*uhat + 3*uhat**2)*
     -     loopba(2))/(shat*that*uhat) + 
     -  (4*(8*shat**2 + 9*shat*that + 7*that**2 + 
     -       4*shat*uhat + 3*that*uhat)*loopba(3))
     -    /(shat*that**2) + 
     -  (2*shat*(-3*that**2*uhat*
     -        (2*that**2 + 3*that*uhat + uhat**2) 
     -        + 4*shat**2*
     -        (2*that**3 + 3*uhat**3) + 
     -       shat*(8*that**4 + 6*that**3*uhat - 
     -          3*that**2*uhat**2 + 
     -          6*that*uhat**3 + 4*uhat**4))*
     -     loopba(4))/(that**3*uhat**3) + 
     -  ((16*shat**4*that + 
     -       shat*(43*that - 21*uhat)*uhat**3 + 
     -       shat**3*
     -        (16*that**2 + 12*that*uhat - 
     -          15*uhat**2) + 
     -       2*uhat**3*
     -        (29*that**2 + 12*that*uhat - 
     -          9*uhat**2) - 
     -       3*shat**2*uhat*
     -        (4*that**2 + 9*that*uhat + 
     -          6*uhat**2))*loopba(5))/
     -   (shat**2*uhat**3) + 
     -  (2*uhat*(12*shat**4 + 
     -       shat*that**2*(17*that - 9*uhat) + 
     -       4*shat**3*uhat - 
     -       3*shat**2*that*(3*that + 2*uhat) + 
     -       2*that**2*
     -        (13*that**2 + 6*that*uhat - 
     -          3*uhat**2))*loopba(6))/
     -   (shat**2*that**3) - 
     -  (shat*(8*shat**2*that + 
     -       shat*(8*that**2 + 6*that*uhat - 
     -          3*uhat**2) - 
     -       3*uhat*(2*that**2 + 3*that*uhat + 
     -          uhat**2))*loopba(7))/uhat**3 - 
     -  (2*shat**2*uhat*
     -     (6*shat + 3*that + 2*uhat)*loopba(8))/
     -   that**3 + (uhat*
     -     (3*shat**2 - 17*shat*that - 
     -       26*that**2 + 9*shat*uhat - 
     -       12*that*uhat + 6*uhat**2)*loopba(9))/
     -   shat**2
      return
      end

      ! need to multiply 1/48
      double complex function DBhat111stu_FormFactor_Massless(shat,that,
     $     uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DBhat111stu_FormFactor_Massless=12/uhat + (4*
     -     (-(shat**2*that) + 4*shat*that**2 + 
     -       8*that**3 + 3*shat*that*uhat + 
     -       12*that**2*uhat + 4*shat*uhat**2 + 
     -       11*that*uhat**2 + 4*uhat**3)*
     -     loopba(1))/(that**2*uhat**2) - 
     -  (4*(4*shat**2*that + 8*shat*that**2 + 
     -       that**3 - 3*shat**2*uhat + 
     -       3*shat*that*uhat - 3*shat*uhat**2 + 
     -       2*that*uhat**2)*loopba(2))/
     -   (shat*that*uhat**2) + 
     -  (2*(3*shat**2*that + 9*shat*that**2 + 
     -       6*that**3 - 8*shat**2*uhat - 
     -       11*shat*that*uhat + 
     -       12*that**2*uhat - 8*shat*uhat**2)*
     -     loopba(3))/(shat*that**2*uhat) - 
     -  ((-12*shat**2*that**3 - 24*shat*that**4 + 
     -       24*shat**2*that**2*uhat + 
     -       3*shat*that**3*uhat + 
     -       3*that**4*uhat + 
     -       12*shat**3*uhat**2 + 
     -       30*shat**2*that*uhat**2 + 
     -       36*shat*that**2*uhat**2 + 
     -       9*that**3*uhat**2 - 
     -       28*shat**2*uhat**3 - 
     -       34*shat*that*uhat**3 + 
     -       6*that**2*uhat**3 - 52*shat*uhat**4)*
     -     loopba(4))/(that**2*uhat**3) + 
     -  (3*(4*shat**3*that**2 + 
     -       8*shat**2*that**3 + 4*shat**4*uhat + 
     -       shat**3*that*uhat + 
     -       5*shat**2*that**2*uhat + 
     -       4*shat**3*uhat**2 - 
     -       5*shat**2*that*uhat**2 - 
     -       2*shat*that**2*uhat**2 - 
     -       6*shat*that*uhat**3 - 4*that*uhat**4)
     -      *loopba(5))/(shat**2*uhat**3) - 
     -  ((3*shat**3*that**3 + 3*shat**2*that**4 + 
     -       36*shat**4*that*uhat + 
     -       66*shat**3*that**2*uhat + 
     -       63*shat**2*that**3*uhat + 
     -       30*shat*that**4*uhat - 
     -       32*shat**4*uhat**2 - 
     -       56*shat**3*that*uhat**2 - 
     -       54*shat**2*that**2*uhat**2 + 
     -       54*shat*that**3*uhat**2 - 
     -       64*shat**3*uhat**3 - 
     -       116*shat**2*that*uhat**3 + 
     -       24*that**3*uhat**3 - 
     -       32*shat**2*uhat**4)*loopba(6))/
     -   (2.*shat**2*that**3*uhat) - 
     -  (3*shat*that*
     -     (2*shat*that + 4*that**2 - 
     -       4*shat*uhat - 3*uhat**2)*loopba(7))/
     -   uhat**3 + (shat*
     -     (6*shat**2*that + 15*shat*that**2 + 
     -       6*that**3 - 8*shat**2*uhat - 
     -       14*shat*that*uhat - 9*that**2*uhat - 
     -       16*shat*uhat**2 - 26*that*uhat**2 - 
     -       8*uhat**3)*loopba(8))/that**3 + 
     -  (3*that*(shat**3 + shat**2*that + 
     -       3*shat**2*uhat + shat*that*uhat + 
     -       3*shat*uhat**2 + 2*uhat**3)*loopba(9)
     -     )/(shat**2*uhat)
      return
      end

      ! need to multiply 1/48
      double complex function DBhat111tsu_FormFactor_Massless(shat,that,
     $     uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DBhat111tsu_FormFactor_Massless=12/uhat - (4*shat*
     -     (shat + 4*that - 2*uhat)*loopba(1))/
     -   (that*uhat**2) + 
     -  (4*(8*shat**2 + 4*shat*that - that**2 + 
     -       12*shat*uhat + 3*that*uhat + 
     -       7*uhat**2)*loopba(2))/(shat*uhat**2) 
     -   + (6*(2*shat**2 + 3*shat*that + 
     -       that**2 + 4*shat*uhat - that*uhat)*
     -     loopba(3))/(shat*that*uhat) + 
     -  (4*shat*(6*shat**2*that**3 + 
     -       3*shat*that**4 + 
     -       6*shat*that**3*uhat + 
     -       4*shat**2*uhat**3 + 
     -       4*shat*that*uhat**3 - 
     -       3*that**2*uhat**3 + 4*shat*uhat**4 - 
     -       3*that*uhat**4)*loopba(4))/
     -   (that**3*uhat**3) - 
     -  ((-24*shat**4*that - 12*shat**3*that**2 + 
     -       3*shat**4*uhat + 
     -       3*shat**3*that*uhat + 
     -       24*shat**2*that**2*uhat + 
     -       9*shat**3*uhat**2 + 
     -       36*shat**2*that*uhat**2 + 
     -       30*shat*that**2*uhat**2 + 
     -       12*that**3*uhat**2 + 
     -       6*shat**2*uhat**3 - 
     -       34*shat*that*uhat**3 - 
     -       28*that**2*uhat**3 - 52*that*uhat**4)
     -      *loopba(5))/(shat**2*uhat**3) + 
     -  ((-3*shat**3*that**3 - 
     -       3*shat**2*that**4 - 
     -       30*shat**3*that**2*uhat - 
     -       63*shat**2*that**3*uhat - 
     -       66*shat*that**4*uhat - 
     -       36*that**5*uhat + 
     -       32*shat**4*uhat**2 + 
     -       32*shat**3*that*uhat**2 - 
     -       54*shat**2*that**2*uhat**2 + 
     -       86*shat*that**3*uhat**2 + 
     -       56*that**4*uhat**2 + 
     -       32*shat**3*uhat**3 - 
     -       24*shat**2*that*uhat**3 + 
     -       116*that**3*uhat**3)*loopba(6))/
     -   (2.*shat**2*that**3*uhat) - 
     -  (3*shat*that*
     -     (2*shat**2 - 2*that*uhat - uhat**2)*
     -     loopba(7))/uhat**3 - 
     -  (3*shat*(shat**2*that - that**3 - 
     -       3*that**2*uhat - 3*that*uhat**2 - 
     -       2*uhat**3)*loopba(8))/(that**2*uhat) 
     -   + (that*(6*shat**2 + 15*shat*that + 
     -       6*that**2 - 17*shat*uhat - 
     -       14*that*uhat - 26*uhat**2)*loopba(9))
     -    /shat**2
      return
      end

      ! need to multiply 1/48
      double complex function DBhat111ust_FormFactor_Massless(shat,that
     $     ,uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DBhat111ust_FormFactor_Massless=12/that - (4*shat*
     -     (shat - 2*that + 4*uhat)*loopba(1))/
     -   (that**2*uhat) + 
     -  (6*(2*shat**2 + 4*shat*that + 
     -       3*shat*uhat - that*uhat + uhat**2)*
     -     loopba(2))/(shat*that*uhat) + 
     -  (4*(8*shat**2 + 12*shat*that + 
     -       7*that**2 + 4*shat*uhat + 
     -       3*that*uhat - uhat**2)*loopba(3))/
     -   (shat*that**2) + 
     -  (6*shat*(-2*that**4 - shat*that**2*uhat - 
     -       3*that**3*uhat + 4*shat**2*uhat**2 + 
     -       4*shat*that*uhat**2 - 
     -       that**2*uhat**2 + 2*shat*uhat**3)*
     -     loopba(4))/(that**3*uhat**2) - 
     -  ((24*shat**2*that**3 + 
     -       30*shat**3*that*uhat + 
     -       54*shat**2*that**2*uhat + 
     -       3*shat**3*uhat**2 + 
     -       63*shat**2*that*uhat**2 - 
     -       86*shat*that**2*uhat**2 - 
     -       116*that**3*uhat**2 + 
     -       3*shat**2*uhat**3 + 
     -       66*shat*that*uhat**3 - 
     -       56*that**2*uhat**3 + 36*that*uhat**4)
     -      *loopba(5))/(2.*shat**2*that*uhat**2) 
     -   - ((3*shat**4*that + 9*shat**3*that**2 + 
     -       6*shat**2*that**3 - 
     -       24*shat**4*uhat + 
     -       3*shat**3*that*uhat + 
     -       36*shat**2*that**2*uhat - 
     -       34*shat*that**3*uhat - 
     -       52*that**4*uhat - 
     -       12*shat**3*uhat**2 + 
     -       24*shat**2*that*uhat**2 + 
     -       30*shat*that**2*uhat**2 - 
     -       28*that**3*uhat**2 + 
     -       12*that**2*uhat**3)*loopba(6))/
     -   (shat**2*that**3) - 
     -  (3*shat*(-2*that**3 + shat**2*uhat - 
     -       3*that**2*uhat - 3*that*uhat**2 - 
     -       uhat**3)*loopba(7))/(that*uhat**2) - 
     -  (3*shat*uhat*
     -     (2*shat**2 - that**2 - 2*that*uhat)*
     -     loopba(8))/that**3 - 
     -  (uhat*(3*shat**3 - 3*shat**2*that + 
     -       17*shat*that**2 + 26*that**3 + 
     -       3*shat**2*uhat - 15*shat*that*uhat + 
     -       14*that**2*uhat - 6*that*uhat**2)*
     -     loopba(9))/(shat**2*that)
      return
      end

      double complex function DC2111stu_FormFactor_Massless(shat,that
     $     ,uhat,loopba)
      implicit none
      double precision shat,that,uhat
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      DC2111stu_FormFactor_Massless=-(4*shat**2*that + 8*shat*that**2 + 
     -     4*that**3 + 13*shat*that*uhat + 
     -     10*that**2*uhat + 3*shat*uhat**2 + 
     -     12*that*uhat**2 + 3*uhat**3)/
     -  (3.*shat*that**2*uhat**2)
      return
      end

      ! in UT basis of loopba
      ! should multiply MQ**4/8
      double complex function ASstu_FormFactor_Massive_UT(xs,xt,xu
     $     ,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      double precision r1,r2,r3
      double precision rat1_UT1L,rat2_UT1L,rat3_UT1L
      external rat1_UT1L,rat2_UT1L,rat3_UT1L
      ASstu_FormFactor_Massive_UT=2d0
      ! xs,xt,xu
      r1=rat1_UT1L(xs,xt,xu)
      r2=rat2_UT1L(xs,xt,xu)
      r3=rat3_UT1L(xs,xt,xu)
      ASstu_FormFactor_Massive_UT=ASstu_FormFactor_Massive_UT
     $     +r1*loopba(1)-r2*loopba(4)-r3*loopba(7)
      ! xt,xu,xs
      r1=rat1_UT1L(xt,xu,xs)
      r2=rat2_UT1L(xt,xu,xs)
      r3=rat3_UT1L(xt,xu,xs)
      ASstu_FormFactor_Massive_UT=ASstu_FormFactor_Massive_UT
     $     +r1*loopba(2)-r2*loopba(5)-r3*loopba(8)
      ! xu,xs,xt
      r1=rat1_UT1L(xu,xs,xt)
      r2=rat2_UT1L(xu,xs,xt)
      r3=rat3_UT1L(xu,xs,xt)
      ASstu_FormFactor_Massive_UT=ASstu_FormFactor_Massive_UT
     $     +r1*loopba(3)-r2*loopba(6)-r3*loopba(9)
      return
      end


      ! should multiply MQ**2/8
      double complex function DBhat111stu_FormFactor_Massive_UT(xs,xt,xu
     $     ,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      double precision r1,r4,r5,r6,r7
      double precision rat1_UT1L,rat4_UT1L,rat5_UT1L,rat6_UT1L,rat7_UT1L
      external rat1_UT1L,rat4_UT1L,rat5_UT1L,rat6_UT1L,rat7_UT1L
      DBhat111stu_FormFactor_Massive_UT=2d0
      ! xs,xt,xu
      r1=rat1_UT1L(xs,xt,xu)
      r5=rat5_UT1L(xs,xt,xu)
      r6=rat6_UT1L(xs,xt,xu)
      r7=rat7_UT1L(xs,xt,xu)
      DBhat111stu_FormFactor_Massive_UT=
     $     DBhat111stu_FormFactor_Massive_UT
     $     -r1*loopba(1)+(r5+r6)*loopba(4)-r7*loopba(7)
      ! xt,xu,xs
      r4=rat4_UT1L(xt,xu,xs)
      r5=rat5_UT1L(xt,xu,xs)
      r7=rat7_UT1L(xt,xu,xs)
      DBhat111stu_FormFactor_Massive_UT=
     $     DBhat111stu_FormFactor_Massive_UT
     $     +r4*loopba(2)+r5*loopba(5)+(r7-4d0*xu)*loopba(8)
      ! xu,xs,xt
      r4=rat4_UT1L(xu,xs,xt)
      r5=rat5_UT1L(xu,xs,xt)
      r7=rat7_UT1L(xu,xs,xt)
      DBhat111stu_FormFactor_Massive_UT=
     $     DBhat111stu_FormFactor_Massive_UT
     $     -r4*loopba(3)-r5*loopba(6)-(r7+4d0*(xu-xs))*loopba(9)
      DBhat111stu_FormFactor_Massive_UT=
     $     DBhat111stu_FormFactor_Massive_UT/xu
      return
      end

      ! should multiply MQ**2/8
      double complex function DBhat111tsu_FormFactor_Massive_UT(xs,xt,xu
     $     ,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      double precision r1,r4,r5,r6,r7
      double precision rat1_UT1L,rat4_UT1L,rat5_UT1L,rat6_UT1L,rat7_UT1L
      external rat1_UT1L,rat4_UT1L,rat5_UT1L,rat6_UT1L,rat7_UT1L
      DBhat111tsu_FormFactor_Massive_UT=2d0
      ! xt,xs,xu
      r1=rat1_UT1L(xt,xs,xu)
      r5=rat5_UT1L(xt,xs,xu)
      r6=rat6_UT1L(xt,xs,xu)
      r7=rat7_UT1L(xt,xs,xu)
      DBhat111tsu_FormFactor_Massive_UT=
     $     DBhat111tsu_FormFactor_Massive_UT
     $     -r1*loopba(2)+(r5+r6)*loopba(5)-r7*loopba(7)
      ! xs,xu,xt
      r4=rat4_UT1L(xs,xu,xt)
      r5=rat5_UT1L(xs,xu,xt)
      r7=rat7_UT1L(xs,xu,xt)
      DBhat111tsu_FormFactor_Massive_UT=
     $     DBhat111tsu_FormFactor_Massive_UT
     $     +r4*loopba(1)+r5*loopba(4)+(r7-4d0*xu)*loopba(9)
      ! xu,xt,xs
      r4=rat4_UT1L(xu,xt,xs)
      r5=rat5_UT1L(xu,xt,xs)
      r7=rat7_UT1L(xu,xt,xs)
      DBhat111tsu_FormFactor_Massive_UT=
     $     DBhat111tsu_FormFactor_Massive_UT
     $     -r4*loopba(3)-r5*loopba(6)-(r7+4d0*(xu-xt))*loopba(8)
      DBhat111tsu_FormFactor_Massive_UT=
     $     DBhat111tsu_FormFactor_Massive_UT/xu
      return
      end

      ! should multiply MQ**2/8
      double complex function DBhat111ust_FormFactor_Massive_UT(xs,xt,xu
     $     ,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      double precision r1,r4,r5,r6,r7
      double precision rat1_UT1L,rat4_UT1L,rat5_UT1L,rat6_UT1L,rat7_UT1L
      external rat1_UT1L,rat4_UT1L,rat5_UT1L,rat6_UT1L,rat7_UT1L
      DBhat111ust_FormFactor_Massive_UT=2d0
      ! xu,xs,xt
      r1=rat1_UT1L(xu,xs,xt)
      r5=rat5_UT1L(xu,xs,xt)
      r6=rat6_UT1L(xu,xs,xt)
      r7=rat7_UT1L(xu,xs,xt)
      DBhat111ust_FormFactor_Massive_UT=
     $     DBhat111ust_FormFactor_Massive_UT
     $     -r1*loopba(3)+(r5+r6)*loopba(6)-r7*loopba(9)
      ! xs,xt,xu
      r4=rat4_UT1L(xs,xt,xu)
      r5=rat5_UT1L(xs,xt,xu)
      r7=rat7_UT1L(xs,xt,xu)
      DBhat111ust_FormFactor_Massive_UT=
     $     DBhat111ust_FormFactor_Massive_UT
     $     +r4*loopba(1)+r5*loopba(4)+(r7-4d0*xt)*loopba(7)
      ! xt,xu,xs
      r4=rat4_UT1L(xt,xu,xs)
      r5=rat5_UT1L(xt,xu,xs)
      r7=rat7_UT1L(xt,xu,xs)
      DBhat111ust_FormFactor_Massive_UT=
     $     DBhat111ust_FormFactor_Massive_UT
     $     -r4*loopba(2)-r5*loopba(5)-(r7+4d0*(xt-xu))*loopba(8)
      DBhat111ust_FormFactor_Massive_UT=
     $     DBhat111ust_FormFactor_Massive_UT/xt
      return
      end


      double complex function DC2111stu_FormFactor_Massive_UT(xs,xt,xu
     $     ,loopba)
      implicit none
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer i
      double precision r8,rr
      double precision rat8_UT1L
      external rat8_UT1L
      DC2111stu_FormFactor_Massive_UT=1d0
      rr=2d0*(1d0/xs+1d0/xt+1d0/xu)
      ! xs,xt,xu
      r8=rat8_UT1L(xs,xt,xu)
      DC2111stu_FormFactor_Massive_UT=DC2111stu_FormFactor_Massive_UT
     $     +rr*loopba(4)-r8*loopba(7)
      ! xt,xu,xs
      r8=rat8_UT1L(xt,xu,xs)
      DC2111stu_FormFactor_Massive_UT=DC2111stu_FormFactor_Massive_UT
     $     +rr*loopba(5)-r8*loopba(8)
      ! xu,xs,xt
      r8=rat8_UT1L(xu,xs,xt)
      DC2111stu_FormFactor_Massive_UT=DC2111stu_FormFactor_Massive_UT
     $     +rr*loopba(6)-r8*loopba(9)
      DC2111stu_FormFactor_Massive_UT=DC2111stu_FormFactor_Massive_UT
     $     /(xs*xu)
      return
      end

      ! rational coefficients in UT basis
      double precision function rat1_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat1_UT1L=2d0*(xi-4d0)*(xi*xj-xk**2)/(xj*xk)
      return
      end

      double precision function rat2_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
c      rat2_UT1L=(xi**4*(6d0-xi)+2d0*xi**2*(2d0*xi-5d0)*(xj**2+xk**2)
c     $     +2d0*xi*(xi-4d0)*(xj**3+xk**3)+(xi-4d0)*(xj**4+xk**4))/
c     $     (xi*xj**2*xk**2)
      rat2_UT1L=-2d0*((4d0-xi)/xi
     $     +xi*(xj**3+xk**3-6d0*xj*xk)/(xj**2*xk**2))
      return
      end

      double precision function rat3_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat3_UT1L=(32d0*xk**2-xi*xj*(xi**2+xj**2-16d0*xk))/xk**2
      return
      end

      double precision function rat4_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat4_UT1L=2d0*xi*(xi-4d0)*(xj-xk)/(xj*xk)
      return
      end

      double precision function rat5_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat5_UT1L=2d0*(2d0*xj*xk*(xj-xk)+xi*(xj**3-xk**3))/(xj**2*xk**2)
      return
      end

      double precision function rat6_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat6_UT1L=2d0*(xi**2+xj**2-4d0*xk)/xk**2
      return
      end

      double precision function rat7_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat7_UT1L=xi*(4d0*(xj**2-xi**2)+xj*(xi**2+xj**2))/xk**2
      return
      end

      double precision function rat8_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat8_UT1L=2d0*(xi*xj+2d0*xk)/xk
      return
      end

      double precision function rat9_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat9_UT1L=(xi-4d0)*(xi-xj)/xk
      end

      double precision function rat10_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      rat10_UT1L=1d0-4d0/xi-2d0*xj*xk/xi**2
      end

      double precision function rat11_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xjxk
      xjxk=xj*xk
      rat11_UT1L=4d0-2d0*xi-xjxk+2d0*xjxk*(xjxk+4d0*xi)/xi**2
      end

      double precision function rat12_UT1L(xi,xj,xk)
      implicit none
      double precision xi,xj,xk
      double precision xjxk,xi2
      xjxk=xj*xk
      xi2=xi**2
      rat12_UT1L=4d0-2d0*xi-xjxk+2d0*xjxk*(xjxk+4d0*xi)/xi2
     $     +(4d0*xi2-10d0*xi-5d0*xjxk)/3d0
      end

      ! get the form factors from helicity amplitudes
      ! for massive one-loop case
      subroutine Get_OneLoop_FormFactors_Massive(shat,that
     $     ,uhat,MQ2,loopba,FFs)
      implicit none
      double precision shat,that,uhat
      double precision MQ2
      double precision xs,xt,xu
      double complex FFs(5)
      double complex amp(5)
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer ihel
      double complex OneLoop_HelAmp_Massive
      external OneLoop_HelAmp_Massive
      xs=shat/MQ2
      xt=that/MQ2
      xu=uhat/MQ2
      do ihel=1,5
         amp(ihel)=OneLoop_HelAmp_Massive(ihel,xs,xt,xu,loopba)
      enddo
      ! AS(s,t,u)
      FFs(1)=0.5d0*(amp(1)+4d0*amp(2)+amp(3)+amp(4)+amp(5))
      ! DBhat111(s,t,u)
      FFs(2)=-0.5d0/uhat*(-amp(1)-amp(3)+amp(4)+amp(5))
      ! DBhat111(t,u,s)
      FFs(3)=-0.5d0/shat*(-amp(1)+amp(3)+amp(4)-amp(5))
      ! DBhat111(u,s,t)
      FFs(4)=-0.5d0/that*(-amp(1)+amp(3)-amp(4)+amp(5))
      ! DChat2111(s,t,u)
      FFs(5)=-0.5d0/(shat*uhat)*(amp(1)-4d0*amp(2)+amp(3)+amp(4)+amp(5))
      return
      end

      ! get the form factors from helicity amplitudes
      ! for massive one-loop W case
      subroutine Get_OneLoop_FormFactors_MassiveW(shat,that
     $     ,uhat,MW2,loopba,FFs)
      implicit none
      double precision shat,that,uhat
      double precision MW2
      double precision xs,xt,xu
      double complex FFs(5)
      double complex amp(5)
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      integer ihel
      double complex OneLoop_HelAmp_MassiveW
      external OneLoop_HelAmp_MassiveW
      xs=shat/MW2
      xt=that/MW2
      xu=uhat/MW2
      do ihel=1,5
         amp(ihel)=OneLoop_HelAmp_MassiveW(ihel,xs,xt,xu,loopba)
      enddo
      ! AS(s,t,u)
      FFs(1)=0.5d0*(amp(1)+4d0*amp(2)+amp(3)+amp(4)+amp(5))
      ! DBhat111(s,t,u)
      FFs(2)=-0.5d0/uhat*(-amp(1)-amp(3)+amp(4)+amp(5))
      ! DBhat111(t,u,s)
      FFs(3)=-0.5d0/shat*(-amp(1)+amp(3)+amp(4)-amp(5))
      ! DBhat111(u,s,t)
      FFs(4)=-0.5d0/that*(-amp(1)+amp(3)-amp(4)+amp(5))
      ! DChat2111(s,t,u)
      FFs(5)=-0.5d0/(shat*uhat)*(amp(1)-4d0*amp(2)+amp(3)+amp(4)+amp(5))
      return
      end


      ! get the form factors from helicity amplitudes
      ! for massless one-loop case
      subroutine Get_OneLoop_FormFactors_Massless(shat,that
     $     ,uhat,FFs)
      implicit none
      double precision shat,that,uhat
      double complex FFs(5)
      double complex amp(5)
      integer ihel
      double complex OneLoop_HelAmp_Massless
      external OneLoop_HelAmp_Massless
      do ihel=1,5
         amp(ihel)=OneLoop_HelAmp_Massless(ihel,shat,that,uhat)
      enddo
      ! AS(s,t,u)
      FFs(1)=0.5d0*(amp(1)+4d0*amp(2)+amp(3)+amp(4)+amp(5))
      ! DBhat111(s,t,u)
      FFs(2)=-0.5d0/uhat*(-amp(1)-amp(3)+amp(4)+amp(5))
      ! DBhat111(t,u,s)
      FFs(3)=-0.5d0/shat*(-amp(1)+amp(3)+amp(4)-amp(5))
      ! DBhat111(u,s,t)
      FFs(4)=-0.5d0/that*(-amp(1)+amp(3)-amp(4)+amp(5))
      ! DChat2111(s,t,u)
      FFs(5)=-0.5d0/(shat*uhat)*(amp(1)-4d0*amp(2)+amp(3)+amp(4)+amp(5))
      return
      end

      ! helicity amplitude for massive particles
      double complex function OneLoop_HelAmp_Massive(ihel,xs,xt,xu
     $     ,loopba)
      implicit none
      ! we use the notation of 2 -> 2 instead 4 -> 0
      ! 1: --++,++--
      ! 2: -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
      ! 3: ++++,----
      ! 4: +--+,-++-
      ! 5: +-+-,-+-+
      integer ihel
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      double precision rat8_UT1L,rat9_UT1L,rat10_UT1L,rat11_UT1L
      external rat8_UT1L,rat9_UT1L,rat10_UT1L,rat11_UT1L
      double precision r88,r91,r92,r10,r11
      select case(ihel)
      case(1)
         ! --++,++--
         OneLoop_HelAmp_Massive=1d0-4d0*(loopba(7)+loopba(8)+loopba(9))
      case(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         OneLoop_HelAmp_Massive=1d0+2d0*(xs**(-1)+xt**(-1)+xu**(-1))*
     $        (loopba(4)+loopba(5)+loopba(6))
         ! s,t,u
         r88=rat8_UT1L(xs,xt,xu)
         OneLoop_HelAmp_Massive=OneLoop_HelAmp_Massive-r88*loopba(7)
         ! t,u,s
         r88=rat8_UT1L(xt,xu,xs)
         OneLoop_HelAmp_Massive=OneLoop_HelAmp_Massive-r88*loopba(8)
         ! u,s,t
         r88=rat8_UT1L(xu,xs,xt)
         OneLoop_HelAmp_Massive=OneLoop_HelAmp_Massive-r88*loopba(9)
      case(3)
         ! ++++,----
         r91=rat9_UT1L(xt,xu,xs)
         r92=rat9_UT1L(xu,xt,xs)
         r10=rat10_UT1L(xs,xt,xu)
         r11=rat11_UT1L(xs,xt,xu)
         OneLoop_HelAmp_Massive=-1d0+r91*loopba(2)+r92*loopba(3)
     $        -r10*(loopba(5)+loopba(6))+2d0*(xs-2d0)*(loopba(7)
     $        +loopba(9))-r11*loopba(8)
      case(4)
         ! +--+,-++-
         ! s<->u from ihel=3
         r91=rat9_UT1L(xt,xs,xu)
         r92=rat9_UT1L(xs,xt,xu)
         r10=rat10_UT1L(xu,xt,xs)
         r11=rat11_UT1L(xu,xt,xs)
         OneLoop_HelAmp_Massive=-1d0+r91*loopba(2)+r92*loopba(1)
     $        -r10*(loopba(5)+loopba(4))+2d0*(xu-2d0)*(loopba(8)
     $        +loopba(9))-r11*loopba(7)
      case(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         r91=rat9_UT1L(xu,xs,xt)
         r92=rat9_UT1L(xs,xu,xt)
         r10=rat10_UT1L(xt,xu,xs)
         r11=rat11_UT1L(xt,xu,xs)
         OneLoop_HelAmp_Massive=-1d0+r91*loopba(3)+r92*loopba(1)
     $        -r10*(loopba(6)+loopba(4))+2d0*(xt-2d0)*(loopba(8)
     $        +loopba(7))-r11*loopba(9)
      case default
         write(*,*)"ERROR: unknown helicity configuration =",ihel
         stop
      end select
      return
      end

      ! helicity amplitude for the massive W boson
      ! I should multiply -12*alpha**2
      double complex function OneLoop_HelAmp_MassiveW(ihel,xs,xt,xu
     $     ,loopba)
      implicit none
      ! we use the notation of 2 -> 2 instead 4 -> 0
      ! 1: --++,++--
      ! 2: -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
      ! 3: ++++,----
      ! 4: +--+,-++-
      ! 5: +-+-,-+-+
      integer ihel
      double precision xs,xt,xu
      integer NDIM
      parameter (NDIM=9)
      double complex loopba(NDIM)
      double precision rat8_UT1L,rat9_UT1L,rat10_UT1L,rat12_UT1L
      external rat8_UT1L,rat9_UT1L,rat10_UT1L,rat12_UT1L
      double precision r88,r91,r92,r10,r12
      select case(ihel)
      case(1)
         ! --++,++--
         OneLoop_HelAmp_MassiveW=1d0-4d0*(loopba(7)+loopba(8)+loopba(9))
      case(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         OneLoop_HelAmp_MassiveW=1d0+2d0*(xs**(-1)+xt**(-1)+xu**(-1))*
     $        (loopba(4)+loopba(5)+loopba(6))
         ! s,t,u
         r88=rat8_UT1L(xs,xt,xu)
         OneLoop_HelAmp_MassiveW=OneLoop_HelAmp_MassiveW-r88*loopba(7)
         ! t,u,s
         r88=rat8_UT1L(xt,xu,xs)
         OneLoop_HelAmp_MassiveW=OneLoop_HelAmp_MassiveW-r88*loopba(8)
         ! u,s,t
         r88=rat8_UT1L(xu,xs,xt)
         OneLoop_HelAmp_MassiveW=OneLoop_HelAmp_MassiveW-r88*loopba(9)
      case(3)
         ! ++++,----
         r91=rat9_UT1L(xt,xu,xs)
         r92=rat9_UT1L(xu,xt,xs)
         r10=rat10_UT1L(xs,xt,xu)
         r10=r10+5d0/3d0
         r12=rat12_UT1L(xs,xt,xu)
         OneLoop_HelAmp_MassiveW=-1d0+r91*loopba(2)+r92*loopba(3)
     $        -r10*(loopba(5)+loopba(6))-4d0/3d0*(xs-1d0)*(xs-3d0)
     $        *(loopba(7)+loopba(9))-r12*loopba(8)
      case(4)
         ! +--+,-++-
         ! s<->u from ihel=3
         r91=rat9_UT1L(xt,xs,xu)
         r92=rat9_UT1L(xs,xt,xu)
         r10=rat10_UT1L(xu,xt,xs)
         r10=r10+5d0/3d0
         r12=rat12_UT1L(xu,xt,xs)
         OneLoop_HelAmp_MassiveW=-1d0+r91*loopba(2)+r92*loopba(1)
     $        -r10*(loopba(5)+loopba(4))-4d0/3d0*(xu-1d0)*(xu-3d0)
     $        *(loopba(8)+loopba(9))-r12*loopba(7)
      case(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         r91=rat9_UT1L(xu,xs,xt)
         r92=rat9_UT1L(xs,xu,xt)
         r10=rat10_UT1L(xt,xu,xs)
         r10=r10+5d0/3d0
         r12=rat12_UT1L(xt,xu,xs)
         OneLoop_HelAmp_MassiveW=-1d0+r91*loopba(3)+r92*loopba(1)
     $        -r10*(loopba(6)+loopba(4))-4d0/3d0*(xt-1d0)*(xt-3d0)
     $        *(loopba(8)+loopba(7))-r12*loopba(9)
      case default
         write(*,*)"ERROR: unknown helicity configuration =",ihel
         stop
      end select
      return
      end

      ! helicity amplitude from hep-ph/0109079
      ! eqs.(2.4-2.5)
      double complex function OneLoop_HelAmp_Massless(ihel,shat,that,
     $     uhat)
      implicit none
      ! 1: --++,++--
      ! 2: -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
      ! 3: ++++,----
      ! 4: +--+,-++-
      ! 5: +-+-,-+-+
      integer ihel
      double precision shat,that,uhat
      double complex logtou
      double precision pipi,pi2
      parameter (pipi=3.14159265358979323846264338328d0)
      parameter (pi2=9.86960440108935861883449099988d0)
      select case(ihel)
      case(1)
         ! --++,++--
         OneLoop_HelAmp_Massless=dcmplx(1d0,0d0)
      case(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         OneLoop_HelAmp_Massless=dcmplx(1d0,0d0)
      case(3)
         ! ++++,----
         logtou=log(dcmplx(that/uhat,0d0))
         OneLoop_HelAmp_Massless=-0.5d0*(that**2+uhat**2)/(shat**2)*
     $        (logtou**2+pi2)-(that-uhat)/shat*logtou-1d0
      case(4)
         ! +--+,-++-
         logtou=log(dcmplx(-that/shat,0d0))
         OneLoop_HelAmp_Massless=-0.5d0*(that**2+shat**2)/(uhat**2)*
     $        logtou**2-(that-shat)/uhat*logtou-1d0-dcmplx(0d0,pipi)*
     $        ((that**2+shat**2)/uhat**2*logtou+(that-shat)/uhat)
      case(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4 (see also eq.(2.12) in hep-ph/0109079)
         logtou=log(dcmplx(-uhat/shat,0d0))
         OneLoop_HelAmp_Massless=-0.5d0*(uhat**2+shat**2)/(that**2)*
     $        logtou**2-(uhat-shat)/that*logtou-1d0-dcmplx(0d0,pipi)*
     $        ((uhat**2+shat**2)/that**2*logtou+(uhat-shat)/that)
      case default
         write(*,*)"ERROR: unknown helicity configuration =",ihel
         stop
      end select
      return
      end


      ! get the form factors from helicity amplitudes
      ! for low-energy limit one-loop case
      subroutine Get_OneLoop_FormFactors_LowEnergyLimit(shat,that
     $     ,uhat,MQ2,FFs)
      implicit none
      double precision shat,that,uhat
      double precision MQ2
      double complex FFs(5)
      double complex amp(5)
      integer ihel
      double precision xs,xt,xu
      double complex OneLoop_HelAmp_LowEnergyLimit
      external OneLoop_HelAmp_LowEnergyLimit
      xs=shat/MQ2
      xt=that/MQ2
      xu=uhat/MQ2
      do ihel=1,5
         amp(ihel)=OneLoop_HelAmp_LowEnergyLimit(ihel,xs,xt,xu)
      enddo
      ! AS(s,t,u)
      FFs(1)=0.5d0*(amp(1)+4d0*amp(2)+amp(3)+amp(4)+amp(5))
      ! DBhat111(s,t,u)
      FFs(2)=-0.5d0/uhat*(-amp(1)-amp(3)+amp(4)+amp(5))
      ! DBhat111(t,u,s)
      FFs(3)=-0.5d0/shat*(-amp(1)+amp(3)+amp(4)-amp(5))
      ! DBhat111(u,s,t)
      FFs(4)=-0.5d0/that*(-amp(1)+amp(3)-amp(4)+amp(5))
      ! DChat2111(s,t,u)
      FFs(5)=-0.5d0/(shat*uhat)*(amp(1)-4d0*amp(2)+amp(3)+amp(4)+amp(5))
      return
      end

      ! helicity amplitude from hep-th/0301022
      double complex function OneLoop_HelAmp_LowEnergyLimit(ihel,xs,xt,
     $     xu)
      implicit none
      ! 1: --++,++--
      ! 2: -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
      ! 3: ++++,----
      ! 4: +--+,-++-
      ! 5: +-+-,-+-+
      integer ihel
      double precision xs,xt,xu
      select case(ihel)
      case(1)
         ! --++,++--
         OneLoop_HelAmp_LowEnergyLimit=dcmplx(-1d0/120d0,0d0)
     $        *(xs**2+xt**2+xu**2)
      case(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         OneLoop_HelAmp_LowEnergyLimit=dcmplx(0d0,0d0)
      case(3)
         ! ++++,----
         OneLoop_HelAmp_LowEnergyLimit=dcmplx(11d0/360d0,0d0)
     $        *xs**2
      case(4)
         ! +--+,-++-
         OneLoop_HelAmp_LowEnergyLimit=dcmplx(11d0/360d0,0d0)
     $        *xu**2
      case(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         OneLoop_HelAmp_LowEnergyLimit=dcmplx(11d0/360d0,0d0)
     $        *xt**2
      case default
         write(*,*)"ERROR: unknown helicity configuration #5 =",ihel
         stop
      end select
      return
      end

      double complex function OneLoop_HelAmp_LowEnergyLimitW(ihel,xs,xt,
     $     xu)
      implicit none
      ! 1: --++,++--
      ! 2: -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
      ! 3: ++++,----
      ! 4: +--+,-++-
      ! 5: +-+-,-+-+
      integer ihel
      double precision xs,xt,xu
      select case(ihel)
      case(1)
         ! --++,++--
         OneLoop_HelAmp_LowEnergyLimitW=dcmplx(-1d0/120d0,0d0)
     $        *(xs**2+xt**2+xu**2)
      case(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         OneLoop_HelAmp_LowEnergyLimitW=dcmplx(0d0,0d0)
      case(3)
         ! ++++,----
         OneLoop_HelAmp_LowEnergyLimitW=dcmplx(-7d0/30d0,0d0)
     $        *xs**2
      case(4)
         ! +--+,-++-
         OneLoop_HelAmp_LowEnergyLimitW=dcmplx(-7d0/30d0,0d0)
     $        *xu**2
      case(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         OneLoop_HelAmp_LowEnergyLimitW=dcmplx(-7d0/30d0,0d0)
     $        *xt**2
      case default
         write(*,*)"ERROR: unknown helicity configuration #7 =",ihel
         stop
      end select
      return
      end

      function massive_1LAmp_xs_LElimit(log10xs,y0,ihel)
      implicit none
      integer massive_1LAmp_xs_LElimit
      double precision log10xs,y,y0
      integer ihel
      double precision log10xsmax
      if(y0.GT.1d0)then
         y=2d0-y0
      else
         y=y0
      endif
      select case(ihel)
      case(1)
         log10xsmax=-2d0
      case(2)
         log10xsmax=-2d0
      case(3)
         log10xsmax=-2d0
      case(4)
         if(y0.GT.1d0)then
            if(y.LT.8d-3)then
               log10xsmax=-1.4d0
            elseif(y.LT.5d-2)then
               log10xsmax=-1.5d0
            elseif(y.lt.1d-1)then
               log10xsmax=-1.7d0
            elseif(y.lt.1.136d-1)then
               log10xsmax=-1.8d0
            elseif(y.lt.0.4d0)then
               log10xsmax=-1.9d0
            else
               log10xsmax=-2d0
            endif
         else
            log10xsmax=-2d0
         endif
      case(5)
         if(y0.GT.1d0)then
            log10xsmax=-2d0
         else
            if(y.LT.8d-3)then
               log10xsmax=-1.4d0
            elseif(y.LT.5d-2)then
               log10xsmax=-1.5d0
            elseif(y.lt.1d-1)then
               log10xsmax=-1.7d0
            elseif(y.lt.1.136d-1)then
               log10xsmax=-1.8d0
            elseif(y.lt.0.4d0)then
               log10xsmax=-1.9d0
            else
               log10xsmax=-2d0
            endif
         endif
      case default
         write(*,*)"ERROR: do not know the helicity configuration = ",
     $        ihel
         stop
      end select
      if(log10xs.LE.log10xsmax)then
         ! use the low-energy limit                                          
         massive_1LAmp_xs_LElimit=0
      else
         ! just use the massive one-loop amps
         massive_1LAmp_xs_LElimit=1
      endif
      return
      end
