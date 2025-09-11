      subroutine loopbasis(MQ,shat,that,mu,cres)
      implicit none
      integer NDIM
      parameter (NDIM=9)
      double complex cres(NDIM)
      double precision MQ,shat,that,uhat,mu,MQ2,mu2
      double precision ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0d0,ONE=1d0,TWO=2d0,THREE=3d0,FOUR=4d0,HALF=0.5d0)
      double complex zolo(0:2)
      integer init
      DATA init/0/
      SAVE init
      MQ2=MQ**2
      uhat=-shat-that
      mu2=mu**2
      call oneloop_bubble(shat,MQ2,mu2,zolo)
      cres(1)=zolo(0)
      call oneloop_bubble(that,MQ2,mu2,zolo)
      cres(2)=zolo(0)
      call oneloop_bubble(uhat,MQ2,mu2,zolo)
      cres(3)=zolo(0)
      call oneloop_triangle(shat,MQ2,mu2,zolo)
      IF(MQ.GT.0d0)THEN
         cres(4)=MQ2*zolo(0)
      ELSE
         cres(4)=zolo(0)
      ENDIF
      call oneloop_triangle(that,MQ2,mu2,zolo)
      IF(MQ.GT.0d0)THEN
         cres(5)=MQ2*zolo(0)
      ELSE
         cres(5)=zolo(0)
      ENDIF
      call oneloop_triangle(uhat,MQ2,mu2,zolo)
      IF(MQ.GT.0d0)THEN
         cres(6)=MQ2*zolo(0)
      ELSE
         cres(6)=zolo(0)
      ENDIF
      call oneloop_box(shat,that,MQ2,mu2,zolo)
      IF(MQ.GT.0d0)THEN
         cres(7)=MQ2**2*zolo(0)
      ELSE
         cres(7)=zolo(0)
      ENDIF
      call oneloop_box(shat,uhat,MQ2,mu2,zolo)
      IF(MQ.GT.0d0)THEN
         cres(8)=MQ2**2*zolo(0)
      ELSE
         cres(8)=zolo(0)
      ENDIF
      call oneloop_box(that,uhat,MQ2,mu2,zolo)
      IF(MQ.GT.0d0)THEN
         cres(9)=MQ2**2*zolo(0)
      ELSE
         cres(9)=zolo(0)
      ENDIF
      return
      end
      
      ! B0(s,m2,m2)
      ! m2 can be either zero or not
      ! Eq.(18) in arXiv:0911.5634
      subroutine oneloop_bubble(s,m2,mu2,res)
      use Func_PSI
      implicit none
      double complex res(0:2)
      double precision s,m2,mu2
      double precision u,opu
      double complex betau
      double complex logmsomu2
      if(m2.ne.0d0)then
         res(2)=dcmplx(0d0,0d0)
         res(1)=1d0
         if(s.ne.0d0)then
            u=4d0*m2/(-s)
            opu=1d0+u
            betau=RSQRTP(opu)
            res(0)=2d0-RLOGM(m2/mu2)
     $           -betau*LOGM2((betau+1d0)/(betau-1d0),opu)
         else
            res(0)=-RLOGM(m2/mu2)
         endif
      else
         if(s.ne.0d0)then
            res(2)=dcmplx(0d0,0d0)
            res(1)=1d0
            logmsomu2=RLOGM(-s/mu2)
            res(0)=2d0-logmsomu2
         else
            res(0:2)=dcmplx(0d0,0d0)
         endif
      endif
      return
      end

      ! C0(0,0,s,m2,m2,m2), m2=!=0
      ! Eq.(22) in arXiv:0911.5634
      function oneloop_massive_triangle(s,m2)
      use Func_PSI
      implicit none
      double complex oneloop_massive_triangle
      double precision s,m2
      double precision u,opu
      double complex betau,x1,x2
      if(s.ne.0d0)then
         u=4d0*m2/(-s)
         opu=1d0+u
         betau=RSQRTP(opu)
         x1=0.5d0*(1d0+betau)
         x2=0.5d0*(1d0-betau)
         oneloop_massive_triangle=-1d0/s*(Li2M(1d0/x1,opu)
     $        +Li2P(1d0/x2,opu))
      else
         oneloop_massive_triangle=-0.5d0
      endif
      return
      end

      ! C0(0,0,s,m2,m2,m2)
      ! m2 can be either zero or not
      ! we use the same convention as OneLOop and QCDLoop
      subroutine oneloop_triangle(s,m2,mu2,res)
      use Func_PSI
      implicit none
      double complex res(0:2)
      double precision s,m2,mu2
      double complex oneloop_massive_triangle
      external oneloop_massive_triangle
      double complex logmsomu2
      if(m2.ne.0d0)then
         res(0)=oneloop_massive_triangle(s,m2)
         res(1:2)=dcmplx(0d0,0d0)
      else
         if(s.ne.0d0)then
            res(2)=dcmplx(1d0/s,0d0)
            logmsomu2=RLOGM(-s/mu2)
            res(1)=-logmsomu2/s
            res(0)=logmsomu2**2/(2d0*s)
         else
            ! scaless integral
            res(0:2)=dcmplx(0d0,0d0)
         endif
      endif
      return
      end

      ! D0(0,0,0,0,s,t,m2,m2,m2,m2), m2=!=0
      ! Eq.(2.6) in arXiv:1404.2922
      ! and eq.(9) in hep-ph/9307323
      function oneloop_massive_box(s,t,m2)
      use Func_PSI
      implicit none
      double complex oneloop_massive_box
      double precision s,t,m2
      double precision u,v
      double complex betau,betav,betauv
      double precision zeta2,zero_thr
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zero_thr=1d-10)
      if(s.ne.0d0.and.t.ne.0d0.and.m2.ne.0d0)then
         if(dabs(s-(4d0*m2*t/(t-4d0*m2))).lt.zero_thr)then
            ! shift it a bit to avoid the singularity
            s=(4d0*m2*t/(t-4d0*m2))*(1d0+1d-8)
         endif
         u=4d0*m2/(-s)
         v=4d0*m2/(-t)
         betau=RSQRTP(1d0+u)
         betav=RSQRTP(1d0+v)
         betauv=RSQRTP(1d0+u+v)
         oneloop_massive_box=2d0*LOG((betauv+betau)/(betauv+betav))**2+
     $        +LOG((betauv-betau)/(betauv+betau))
     $        *LOG((betauv-betav)/(betauv+betav))-3d0*zeta2
         oneloop_massive_box=oneloop_massive_box+
     $        2d0*Li2((betau-1d0)/(betauv+betau))
     $        +2d0*Li2((betav-1d0)/(betauv+betav))
         oneloop_massive_box=oneloop_massive_box-
     $        2d0*Li2(-(betauv-betau)/(betau+1d0))
     $        -2d0*Li2(-(betauv-betav)/(betav+1d0))
         oneloop_massive_box=oneloop_massive_box-
     $        LOG((betau+1d0)/(betauv+betau))**2
     $        -LOG((betav+1d0)/(betauv+betav))**2
         oneloop_massive_box=oneloop_massive_box*2d0/(s*t*betauv)
      elseif(t.eq.0d0.and.m2.ne.0d0.and.s.ne.0d0)then
         u=4d0*m2/(-s)
         betau=RSQRTP(1d0+u)
         oneloop_massive_box=2d0+betau*LOG(u/(1d0+betau)**2)
         oneloop_massive_box=oneloop_massive_box/s
      elseif(s.eq.0d0.and.m2.ne.0d0.and.t.ne.0d0)then
         v=4d0*m2/(-t)
         betav=RSQRTP(1d0+v)
         oneloop_massive_box=2d0+betav*LOG(v/(1d0+betav)**2)
         oneloop_massive_box=oneloop_massive_box/t
      else
         WRITE(*,*)"ERROR: do not understand in oneloop_massive_box"
         stop
      endif
      return
      end


      ! D0(0,0,0,0,s,t,m2,m2,m2,m2)
      ! m2 can be zero or not
      ! we use the same convention as OneLOop and QCDLoop
      subroutine oneloop_box(s,t,m2,mu2,res)
      use Func_PSI
      implicit none
      double precision s,t,m2,mu2
      double complex res(0:2)
      double complex oneloop_massive_box
      external oneloop_massive_box
      double complex logmsomu2,logmtomu2
      double precision zeta2,zero_thr
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zero_thr=1d-10)
      if(m2.ne.0d0)then
         res(0)=oneloop_massive_box(s,t,m2)
         res(1:2)=dcmplx(0d0,0d0)
      else
         if(s.ne.0d0.and.t.ne.0d0)then
            res(2)=dcmplx(4d0/(s*t),0d0)
            logmsomu2=RLOGM(-s/mu2)
            logmtomu2=RLOGM(-t/mu2)
            res(1)=-2d0/(s*t)*(logmsomu2+logmtomu2)
            res(0)=(-6d0*zeta2+logmsomu2**2+logmtomu2**2
     $           -(logmsomu2-logmtomu2)**2)/(s*t)
         else
            WRITE(*,*)"WARNING: (s,t)=",s,t
            WRITE(*,*)"WARNING: set the value to be zero"
            res(0:2)=dcmplx(0d0,0d0)
         endif
      endif
      return
      end

      subroutine loopbasis_QP(MQ,shat,that,mu,cres)
      implicit none
      integer NDIM
      parameter (NDIM=9)
      complex*32 cres(NDIM)
      real*16 MQ,shat,that,uhat,mu,MQ2,mu2
      real*16 ZERO,ONE,TWO,THREE,FOUR,HALF
      parameter (ZERO=0E0_16,ONE=1E0_16,TWO=2E0_16,
     $     THREE=3E0_16,FOUR=4E0_16,HALF=0.5E0_16)
      complex*32 zolo(0:2)
      integer init
      DATA init/0/
      SAVE init
      MQ2=MQ**2
      uhat=-shat-that
      mu2=mu**2
      call oneloop_bubble_QP(shat,MQ2,mu2,zolo)
      cres(1)=zolo(0)
      call oneloop_bubble_QP(that,MQ2,mu2,zolo)
      cres(2)=zolo(0)
      call oneloop_bubble_QP(uhat,MQ2,mu2,zolo)
      cres(3)=zolo(0)
      call oneloop_triangle_QP(shat,MQ2,mu2,zolo)
      IF(MQ.GT.0E0_16)THEN
         cres(4)=MQ2*zolo(0)
      ELSE
         cres(4)=zolo(0)
      ENDIF
      call oneloop_triangle_QP(that,MQ2,mu2,zolo)
      IF(MQ.GT.0E0_16)THEN
         cres(5)=MQ2*zolo(0)
      ELSE
         cres(5)=zolo(0)
      ENDIF
      call oneloop_triangle_QP(uhat,MQ2,mu2,zolo)
      IF(MQ.GT.0E0_16)THEN
         cres(6)=MQ2*zolo(0)
      ELSE
         cres(6)=zolo(0)
      ENDIF
      call oneloop_box_QP(shat,that,MQ2,mu2,zolo)
      IF(MQ.GT.0E0_16)THEN
         cres(7)=MQ2**2*zolo(0)
      ELSE
         cres(7)=zolo(0)
      ENDIF
      call oneloop_box_QP(shat,uhat,MQ2,mu2,zolo)
      IF(MQ.GT.0E0_16)THEN
         cres(8)=MQ2**2*zolo(0)
      ELSE
         cres(8)=zolo(0)
      ENDIF
      call oneloop_box_QP(that,uhat,MQ2,mu2,zolo)
      IF(MQ.GT.0E0_16)THEN
         cres(9)=MQ2**2*zolo(0)
      ELSE
         cres(9)=zolo(0)
      ENDIF
      return
      end
      
      ! B0(s,m2,m2)
      ! m2 can be either zero or not
      ! Eq.(18) in arXiv:0911.5634
      subroutine oneloop_bubble_QP(s,m2,mu2,res)
      use Func_PSI
      implicit none
      complex*32 res(0:2)
      real*16 s,m2,mu2
      real*16 u,opu
      complex*32 betau
      complex*32 logmsomu2
      if(m2.ne.0E0_16)then
         res(2)=cmplx(0E0_16,0E0_16,kind=16)
         res(1)=1E0_16
         if(s.ne.0E0_16)then
            u=4E0_16*m2/(-s)
            opu=1E0_16+u
            betau=RSQRTP_QP(opu)
            res(0)=2E0_16-RLOGM_QP(m2/mu2)
     $           -betau*LOGM2_QP((betau+1E0_16)/(betau-1E0_16),opu)
         else
            res(0)=-RLOGM_QP(m2/mu2)
         endif
      else
         if(s.ne.0E0_16)then
            res(2)=cmplx(0E0_16,0E0_16,kind=16)
            res(1)=1E0_16
            logmsomu2=RLOGM_QP(-s/mu2)
            res(0)=2E0_16-logmsomu2
         else
            res(0:2)=cmplx(0E0_16,0E0_16,kind=16)
         endif
      endif
      return
      end

      ! C0(0,0,s,m2,m2,m2), m2=!=0
      ! Eq.(22) in arXiv:0911.5634
      function oneloop_massive_triangle_QP(s,m2)
      use Func_PSI
      implicit none
      complex*32 oneloop_massive_triangle_QP
      real*16 s,m2
      real*16 u,opu
      complex*32 betau,x1,x2
      if(s.ne.0E0_16)then
         u=4E0_16*m2/(-s)
         opu=1E0_16+u
         betau=RSQRTP_QP(opu)
         x1=0.5E0_16*(1E0_16+betau)
         x2=0.5E0_16*(1E0_16-betau)
         oneloop_massive_triangle_QP=-1E0_16/s*(Li2M_QP(1E0_16/x1,opu)
     $        +Li2P_QP(1E0_16/x2,opu))
      else
         oneloop_massive_triangle_QP=-0.5E0_16
      endif
      return
      end

      ! C0(0,0,s,m2,m2,m2)
      ! m2 can be either zero or not
      ! we use the same convention as OneLOop and QCDLoop
      subroutine oneloop_triangle_QP(s,m2,mu2,res)
      use Func_PSI
      implicit none
      complex*32 res(0:2)
      real*16 s,m2,mu2
      complex*32 oneloop_massive_triangle_QP
      external oneloop_massive_triangle_QP
      complex*32 logmsomu2
      if(m2.ne.0E0_16)then
         res(0)=oneloop_massive_triangle_QP(s,m2)
         res(1:2)=cmplx(0E0_16,0E0_16,kind=16)
      else
         if(s.ne.0E0_16)then
            res(2)=cmplx(1E0_16/s,0E0_16,kind=16)
            logmsomu2=RLOGM_QP(-s/mu2)
            res(1)=-logmsomu2/s
            res(0)=logmsomu2**2/(2E0_16*s)
         else
            ! scaless integral
            res(0:2)=cmplx(0E0_16,0E0_16,kind=16)
         endif
      endif
      return
      end

      ! D0(0,0,0,0,s,t,m2,m2,m2,m2), m2=!=0
      ! Eq.(2.6) in arXiv:1404.2922
      ! and eq.(9) in hep-ph/9307323
      function oneloop_massive_box_QP(s,t,m2)
      use Func_PSI
      implicit none
      complex*32 oneloop_massive_box_QP
      real*16 s,t,m2
      real*16 u,v
      complex*32 betau,betav,betauv
      real*16 zeta2,zero_thr
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      parameter (zero_thr=1E-28_16)
      if(s.ne.0E0_16.and.t.ne.0E0_16.and.m2.ne.0E0_16)then
         if(abs(s-(4E0_16*m2*t/(t-4E0_16*m2))).lt.zero_thr)then
            ! shift it a bit to avoid the singularity
            s=(4E0_16*m2*t/(t-4E0_16*m2))*(1E0_16+1E-26_16)
         endif
         u=4E0_16*m2/(-s)
         v=4E0_16*m2/(-t)
         betau=RSQRTP_QP(1E0_16+u)
         betav=RSQRTP_QP(1E0_16+v)
         betauv=RSQRTP_QP(1E0_16+u+v)
         oneloop_massive_box_QP=2E0_16
     $        *LOG((betauv+betau)/(betauv+betav))**2+
     $        +LOG((betauv-betau)/(betauv+betau))
     $        *LOG((betauv-betav)/(betauv+betav))-3E0_16*zeta2
         oneloop_massive_box_QP=oneloop_massive_box_QP+
     $        2E0_16*Li2_QP((betau-1E0_16)/(betauv+betau))
     $        +2E0_16*Li2_QP((betav-1E0_16)/(betauv+betav))
         oneloop_massive_box_QP=oneloop_massive_box_QP-
     $        2E0_16*Li2_QP(-(betauv-betau)/(betau+1E0_16))
     $        -2E0_16*Li2_QP(-(betauv-betav)/(betav+1E0_16))
         oneloop_massive_box_QP=oneloop_massive_box_QP-
     $        LOG((betau+1E0_16)/(betauv+betau))**2
     $        -LOG((betav+1E0_16)/(betauv+betav))**2
         oneloop_massive_box_QP=oneloop_massive_box_QP*2E0_16
     $        /(s*t*betauv)
      elseif(t.eq.0E0_16.and.m2.ne.0E0_16.and.s.ne.0E0_16)then
         u=4E0_16*m2/(-s)
         betau=RSQRTP_QP(1E0_16+u)
         oneloop_massive_box_QP=2E0_16+betau*LOG(u/(1E0_16+betau)**2)
         oneloop_massive_box_QP=oneloop_massive_box_QP/s
      elseif(s.eq.0E0_16.and.m2.ne.0E0_16.and.t.ne.0E0_16)then
         v=4E0_16*m2/(-t)
         betav=RSQRTP_QP(1E0_16+v)
         oneloop_massive_box_QP=2E0_16+betav*LOG(v/(1E0_16+betav)**2)
         oneloop_massive_box_QP=oneloop_massive_box_QP/t
      else
         WRITE(*,*)"ERROR: do not understand in oneloop_massive_box_QP"
         stop
      endif
      return
      end


      ! D0(0,0,0,0,s,t,m2,m2,m2,m2)
      ! m2 can be zero or not
      ! we use the same convention as OneLOop and QCDLoop
      subroutine oneloop_box_QP(s,t,m2,mu2,res)
      use Func_PSI
      implicit none
      real*16 s,t,m2,mu2
      complex*32 res(0:2)
      complex*32 oneloop_massive_box_QP
      external oneloop_massive_box_QP
      complex*32 logmsomu2,logmtomu2
      real*16 zeta2
      parameter (zeta2=
     $     1.6449340668482264364724151666460251892189499012068E0_16)
      if(m2.ne.0E0_16)then
         res(0)=oneloop_massive_box_QP(s,t,m2)
         res(1:2)=cmplx(0E0_16,0E0_16,kind=16)
      else
         if(s.ne.0E0_16.and.t.ne.0E0_16)then
            res(2)=cmplx(4E0_16/(s*t),0E0_16,kind=16)
            logmsomu2=RLOGM_QP(-s/mu2)
            logmtomu2=RLOGM_QP(-t/mu2)
            res(1)=-2E0_16/(s*t)*(logmsomu2+logmtomu2)
            res(0)=(-6E0_16*zeta2+logmsomu2**2+logmtomu2**2
     $           -(logmsomu2-logmtomu2)**2)/(s*t)
         else
            WRITE(*,*)"WARNING: (s,t)=",s,t
            WRITE(*,*)"WARNING: set the value to be zero"
            res(0:2)=cmplx(0E0_16,0E0_16,kind=16)
         endif
      endif
      return
      end
