      ! We need to multiply -I*Nc*Qf^4*a^2*a/Pi*Qf^2 for QED
      !                     -I*Nc*Qf^4*a^2*as/Pi*CF for QCD

      ! get the form factors from helicity amplitudes
      ! for low-energy expansion two-loop case
      subroutine Get_TwoLoop_FormFactors_LE(iterm,m2,shat,that,uhat,FFs)
      implicit none
      ! iterm:
      ! iterm=-1, sum all terms
      ! iterm>=0 means add expansion terms up to (xs)^(2+iterm)
      integer iterm
      double precision m2,shat,that,uhat
      double precision xs,xt,xu
      double complex FFs(5)
      double complex amp(5)
      integer ihel
      double complex TwoLoop_HelAmp_LE
      external TwoLoop_HelAmp_LE
      xs=shat/m2
      xt=that/m2
      xu=uhat/m2
      do ihel=1,5
         amp(ihel)=TwoLoop_HelAmp_LE(ihel,iterm,xs,xt,xu)
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

      double complex function TwoLoop_HelAmp_LE(ihel,iterm,xs,xt,xu)
      implicit none
      ! ihel:
      ! 1: --++,++--
      ! 2: -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
      ! 3: ++++,----
      ! 4: +--+,-++-
      ! 5: +-+-,-+-+
      integer ihel
      ! iterm:
      ! iterm=-1, sum all terms
      ! iterm>=0 means add expansion terms up to (xs)^(2+iterm)
      integer iterm
      double precision xs,xt,xu
      double complex TwoLoop_HelAmp_LE_pppp
      double complex TwoLoop_HelAmp_LE_mppp
      double complex TwoLoop_HelAmp_LE_mmpp
      double complex TwoLoop_HelAmp_LE_pmpm
      double complex TwoLoop_HelAmp_LE_pmmp
      external TwoLoop_HelAmp_LE_pppp
      external TwoLoop_HelAmp_LE_mppp
      external TwoLoop_HelAmp_LE_mmpp
      external TwoLoop_HelAmp_LE_pmpm
      external TwoLoop_HelAmp_LE_pmmp
      integer iterm_used
      SELECT CASE(ihel)
      CASE(1)
         ! --++,++--
         if(iterm.EQ.-1)then
            iterm_used=3
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_LE=TwoLoop_HelAmp_LE_pppp(iterm_used,xs,xt,xu)
      CASE(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         if(iterm.EQ.-1)then
            iterm_used=3
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_LE=TwoLoop_HelAmp_LE_mppp(iterm_used,xs,xt,xu)
      CASE(3)
         ! ++++,----
         if(iterm.EQ.-1)then
            iterm_used=3
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_LE=TwoLoop_HelAmp_LE_mmpp(iterm_used,xs,xt,xu)
      CASE(4)
         ! +--+,-++-
         if(iterm.EQ.-1)then
            iterm_used=3
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_LE=TwoLoop_HelAmp_LE_pmpm(iterm_used,xs,xt,xu)
      CASE(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         if(iterm.EQ.-1)then
            iterm_used=3
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_LE=TwoLoop_HelAmp_LE_pmmp(iterm_used,xs,xt,xu)
      CASE DEFAULT
         write(*,*)"ERROR: unknown helicity configuration #6 =",ihel
         stop
      end select
      return
      end

      ! ++++ (4 -> 0)
      ! --++,++-- (2 -> 2)
      function TwoLoop_HelAmp_LE_pppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 3
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double precision squaresum,allproduct
      IF(iterm.LT.0.OR.iterm.GT.3)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>3"
         STOP
      ENDIF
      squaresum=xs**2+xt**2+xu**2
      res=dcmplx(5d0/12d0*squaresum,0d0)
      IF(iterm.LE.0)return
      allproduct=xs*xt*xu
      res=res+49d0/180d0*allproduct
      IF(iterm.LE.1)return
      res=res+59d0/5670d0*squaresum**2
      IF(iterm.LE.2)return
      res=res+1949d0/170100d0*allproduct*squaresum
      IF(iterm.LE.3)return
      WRITE(*,*)"ERROR: cannot reach here (++++)"
      STOP
      return
      end

      ! -+++ (4 -> 0)
      ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+ (2 -> 2)
      function TwoLoop_HelAmp_LE_mppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 3
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double precision squaresum,allproduct
      IF(iterm.LT.0.OR.iterm.GT.3)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>3"
         STOP
      ENDIF
      res=dcmplx(0d0,0d0)
      IF(iterm.LE.0)return
      allproduct=xs*xt*xu
      res=res+53d0/2700d0*allproduct
      IF(iterm.LE.2)return
      squaresum=xs**2+xt**2+xu**2
      res=res+18971d0/95256000d0*allproduct*squaresum
      IF(iterm.LE.3)return
      WRITE(*,*)"ERROR: cannot reach here (-+++)"
      STOP
      return
      end

      ! --++ (4 -> 0)
      ! ++++,---- (2 -> 2)
      function TwoLoop_HelAmp_LE_mmpp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 3
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double precision squaresum
      IF(iterm.LT.0.OR.iterm.GT.3)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>3"
         STOP
      ENDIF
      res=dcmplx(-391d0/324d0*xs**2,0d0)
      IF(iterm.LE.0)return
      res=res-1849d0/16200d0*xs**3
      IF(iterm.LE.1)return
      res=res-xs**2*(178007d0/5670000d0*xs**2
     $     +118901d0/15876000d0*(xt**2+xu**2))
      IF(iterm.LE.2)return
      res=res-(123234401d0/17288964000d0*xs**5
     $     -3568207d0/1920996000d0*xs**3*(xt**2+xu**2)
     $     -27757d0/411642000d0*xs**2*(xt**3+xu**3)
     $     +1090403d0/2881494000d0*xs*(xt**4+xu**4)
     $     +475021d0/960498000d0*(xt**5+xu**5)
     $     +475021d0/2881494000d0*(xt**6+xu**6)/xs)
      IF(iterm.LE.3)return
      WRITE(*,*)"ERROR: cannot reach here (--++)"
      STOP
      return
      end

      ! +-+- (4 -> 0)
      ! +--+,-++- (2 -> 2)
      function TwoLoop_HelAmp_LE_pmpm(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 3
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double complex TwoLoop_HelAmp_LE_mmpp
      external TwoLoop_HelAmp_LE_mmpp
      res=TwoLoop_HelAmp_LE_mmpp(iterm,xu,xt,xs)
      return
      end

      ! +--+ (4 -> 0)
      ! +-+-,-+-+ (2 -> 2)
      function TwoLoop_HelAmp_LE_pmmp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm  ! iterm from 0 to 3
                     ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double complex TwoLoop_HelAmp_LE_mmpp
      external TwoLoop_HelAmp_LE_mmpp
      res=TwoLoop_HelAmp_LE_mmpp(iterm,xt,xs,xu)
      return
      end

      ! get the form factors from helicity amplitudes
      ! for low-energy limit two-loop case
      subroutine Get_TwoLoop_FormFactors_LowEnergyLimit(shat,that
     $     ,uhat,MQ2,FFs)
      implicit none
      double precision shat,that,uhat
      double precision MQ2
      double complex FFs(5)
      double complex amp(5)
      integer ihel
      double precision xs,xt,xu
      double complex TwoLoop_HelAmp_LowEnergyLimit
      external TwoLoop_HelAmp_LowEnergyLimit
      xs=shat/MQ2
      xt=that/MQ2
      xu=uhat/MQ2
      do ihel=1,5
         amp(ihel)=TwoLoop_HelAmp_LowEnergyLimit(ihel,xs,xt,xu)
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
      double complex function TwoLoop_HelAmp_LowEnergyLimit(ihel,xs,xt,
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
         TwoLoop_HelAmp_LowEnergyLimit=dcmplx(5d0/12d0,0d0)
     $        *(xs**2+xt**2+xu**2)
      case(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         TwoLoop_HelAmp_LowEnergyLimit=dcmplx(0d0,0d0)
      case(3)
         ! ++++,----
         TwoLoop_HelAmp_LowEnergyLimit=dcmplx(-391d0/324d0,0d0)
     $        *xs**2
      case(4)
         ! +--+,-++-
         TwoLoop_HelAmp_LowEnergyLimit=dcmplx(-391d0/324d0,0d0)
     $        *xu**2
      case(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         TwoLoop_HelAmp_LowEnergyLimit=dcmplx(-391d0/324d0,0d0)
     $        *xt**2
      case default
         write(*,*)"ERROR: unknown helicity configuration #6 =",ihel
         stop
      end select
      return
      end
