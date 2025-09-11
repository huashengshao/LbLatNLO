      ! We need to multiply -I*Nc*Qf^4*a^2*a/Pi*Qf^2 for QED
      !                     -I*Nc*Qf^4*a^2*as/Pi*CF for QCD

      ! get the form factors from helicity amplitudes
      ! for high-energy expansion two-loop case
      subroutine Get_TwoLoop_FormFactors_HE(iterm,m2,shat,that,uhat,FFs)
      implicit none
      ! iterm:
      ! iterm=-1, sum all terms
      ! iterm>=0 means add expansion terms up to (-1/xs)^iterm
      integer iterm
      double precision m2,shat,that,uhat
      double precision xs,xt,xu
      double complex FFs(5)
      double complex amp(5)
      integer ihel
      double complex TwoLoop_HelAmp_HE
      external TwoLoop_HelAmp_HE
      xs=shat/m2
      xt=that/m2
      xu=uhat/m2
      do ihel=1,5
         amp(ihel)=TwoLoop_HelAmp_HE(ihel,iterm,xs,xt,xu)
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

      
      double complex function TwoLoop_HelAmp_HE(ihel,iterm,xs,xt,xu)
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
      ! iterm>=0 means add expansion terms up to (-1/xs)^iterm
      integer iterm
      double precision xs,xt,xu
      double complex TwoLoop_HelAmp_HE_pppp
      double complex TwoLoop_HelAmp_HE_mppp
      double complex TwoLoop_HelAmp_HE_mmpp
      double complex TwoLoop_HelAmp_HE_pmpm
      double complex TwoLoop_HelAmp_HE_pmmp
      external TwoLoop_HelAmp_HE_pppp
      external TwoLoop_HelAmp_HE_mppp
      external TwoLoop_HelAmp_HE_mmpp
      external TwoLoop_HelAmp_HE_pmpm
      external TwoLoop_HelAmp_HE_pmmp
      integer iterm_used
      SELECT CASE(ihel)
      CASE(1)
         ! --++,++--
         if(iterm.EQ.-1)then
            iterm_used=8
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_HE=TwoLoop_HelAmp_HE_pppp(iterm_used,xs,xt,xu)
      CASE(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_HE=TwoLoop_HelAmp_HE_mppp(iterm_used,xs,xt,xu)
      CASE(3)
         ! ++++,----
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_HE=TwoLoop_HelAmp_HE_mmpp(iterm_used,xs,xt,xu)
      CASE(4)
         ! +--+,-++-
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_HE=TwoLoop_HelAmp_HE_pmpm(iterm_used,xs,xt,xu)
      CASE(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         TwoLoop_HelAmp_HE=TwoLoop_HelAmp_HE_pmmp(iterm_used,xs,xt,xu)
      CASE DEFAULT
         write(*,*)"ERROR: unknown helicity configuration #3 =",ihel
         stop
      end select
      return
      end
      
      ! ++++ (4 -> 0)
      ! --++,++-- (2 -> 2)
      function TwoLoop_HelAmp_HE_pppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 8
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x,y
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision four
      parameter (four=4d0)
      double complex fxy,fyx,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_2_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_2_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_2_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_3_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_3_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_4_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_4_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_5_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_5_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_6_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_6_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_7_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_7_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_7_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_8_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_8_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_8_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_8_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppppp_HE_8_0
      IF(iterm.LT.0.OR.iterm.GT.8)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>8"
         STOP
      ENDIF
      x=xt/xs
      y=xu/xs
      fxy=TwoLoop_HelAmppppp_HE_0_0(x)
      fyx=TwoLoop_HelAmppppp_HE_0_0(y)
      res=four*(fxy+fyx)
      IF(iterm.LE.0)return
      logm2oms=-DLOG(xs)+IPi
      fxy=TwoLoop_HelAmppppp_HE_1_0(x)
      fyx=TwoLoop_HelAmppppp_HE_1_0(y)
      res=res+four*(-1d0/xs)*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_1_1(x)
      fyx=TwoLoop_HelAmppppp_HE_1_1(y)
      res=res+four*(-1d0/xs)*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_1_2(x)
      fyx=TwoLoop_HelAmppppp_HE_1_2(y)
      res=res+four*(-1d0/xs)*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.1)return
      fxy=TwoLoop_HelAmppppp_HE_2_0(x)
      fyx=TwoLoop_HelAmppppp_HE_2_0(y)
      res=res+four*(-1d0/xs)**2*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_2_1(x)
      fyx=TwoLoop_HelAmppppp_HE_2_1(y)
      res=res+four*(-1d0/xs)**2*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_2_2(x)
      fyx=TwoLoop_HelAmppppp_HE_2_2(y)
      res=res+four*(-1d0/xs)**2*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_2_3(x)
      fyx=TwoLoop_HelAmppppp_HE_2_3(y)
      res=res+four*(-1d0/xs)**2*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_2_4(x)
      fyx=TwoLoop_HelAmppppp_HE_2_4(y)
      res=res+four*(-1d0/xs)**2*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.2)return
      fxy=TwoLoop_HelAmppppp_HE_3_0(x)
      fyx=TwoLoop_HelAmppppp_HE_3_0(y)
      res=res+four*(-1d0/xs)**3*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_3_1(x)
      fyx=TwoLoop_HelAmppppp_HE_3_1(y)
      res=res+four*(-1d0/xs)**3*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_3_2(x)
      fyx=TwoLoop_HelAmppppp_HE_3_2(y)
      res=res+four*(-1d0/xs)**3*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_3_3(x)
      fyx=TwoLoop_HelAmppppp_HE_3_3(y)
      res=res+four*(-1d0/xs)**3*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_3_4(x)
      fyx=TwoLoop_HelAmppppp_HE_3_4(y)
      res=res+four*(-1d0/xs)**3*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.3)return
      fxy=TwoLoop_HelAmppppp_HE_4_0(x)
      fyx=TwoLoop_HelAmppppp_HE_4_0(y)
      res=res+four*(-1d0/xs)**4*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_4_1(x)
      fyx=TwoLoop_HelAmppppp_HE_4_1(y)
      res=res+four*(-1d0/xs)**4*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_4_2(x)
      fyx=TwoLoop_HelAmppppp_HE_4_2(y)
      res=res+four*(-1d0/xs)**4*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_4_3(x)
      fyx=TwoLoop_HelAmppppp_HE_4_3(y)
      res=res+four*(-1d0/xs)**4*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_4_4(x)
      fyx=TwoLoop_HelAmppppp_HE_4_4(y)
      res=res+four*(-1d0/xs)**4*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.4)return
      fxy=TwoLoop_HelAmppppp_HE_5_0(x)
      fyx=TwoLoop_HelAmppppp_HE_5_0(y)
      res=res+four*(-1d0/xs)**5*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_5_1(x)
      fyx=TwoLoop_HelAmppppp_HE_5_1(y)
      res=res+four*(-1d0/xs)**5*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_5_2(x)
      fyx=TwoLoop_HelAmppppp_HE_5_2(y)
      res=res+four*(-1d0/xs)**5*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_5_3(x)
      fyx=TwoLoop_HelAmppppp_HE_5_3(y)
      res=res+four*(-1d0/xs)**5*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_5_4(x)
      fyx=TwoLoop_HelAmppppp_HE_5_4(y)
      res=res+four*(-1d0/xs)**5*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.5)return
      fxy=TwoLoop_HelAmppppp_HE_6_0(x)
      fyx=TwoLoop_HelAmppppp_HE_6_0(y)
      res=res+four*(-1d0/xs)**6*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_6_1(x)
      fyx=TwoLoop_HelAmppppp_HE_6_1(y)
      res=res+four*(-1d0/xs)**6*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_6_2(x)
      fyx=TwoLoop_HelAmppppp_HE_6_2(y)
      res=res+four*(-1d0/xs)**6*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_6_3(x)
      fyx=TwoLoop_HelAmppppp_HE_6_3(y)
      res=res+four*(-1d0/xs)**6*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_6_4(x)
      fyx=TwoLoop_HelAmppppp_HE_6_4(y)
      res=res+four*(-1d0/xs)**6*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.6)return
      fxy=TwoLoop_HelAmppppp_HE_7_0(x)
      fyx=TwoLoop_HelAmppppp_HE_7_0(y)
      res=res+four*(-1d0/xs)**7*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_7_1(x)
      fyx=TwoLoop_HelAmppppp_HE_7_1(y)
      res=res+four*(-1d0/xs)**7*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_7_2(x)
      fyx=TwoLoop_HelAmppppp_HE_7_2(y)
      res=res+four*(-1d0/xs)**7*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_7_3(x)
      fyx=TwoLoop_HelAmppppp_HE_7_3(y)
      res=res+four*(-1d0/xs)**7*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_7_4(x)
      fyx=TwoLoop_HelAmppppp_HE_7_4(y)
      res=res+four*(-1d0/xs)**7*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.7)return
      fxy=TwoLoop_HelAmppppp_HE_8_0(x)
      fyx=TwoLoop_HelAmppppp_HE_8_0(y)
      res=res+four*(-1d0/xs)**8*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_8_1(x)
      fyx=TwoLoop_HelAmppppp_HE_8_1(y)
      res=res+four*(-1d0/xs)**8*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_8_2(x)
      fyx=TwoLoop_HelAmppppp_HE_8_2(y)
      res=res+four*(-1d0/xs)**8*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_8_3(x)
      fyx=TwoLoop_HelAmppppp_HE_8_3(y)
      res=res+four*(-1d0/xs)**8*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmppppp_HE_8_4(x)
      fyx=TwoLoop_HelAmppppp_HE_8_4(y)
      res=res+four*(-1d0/xs)**8*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.8)return
      WRITE(*,*)"ERROR: cannot reach here (++++)"
      STOP
      return
      end

      ! -+++ (4 -> 0)
      ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+ (2 -> 2)
      function TwoLoop_HelAmp_HE_mppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 7
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x,y
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,fyx,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_1_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_1_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_2_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_2_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_2_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_3_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_3_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_4_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_4_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_5_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_5_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_6_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_6_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_7_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_7_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmppp_HE_7_0
      IF(iterm.LT.0.OR.iterm.GT.7)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>7"
         STOP
      ENDIF
      x=xt/xs
      y=xu/xs
      fxy=TwoLoop_HelAmpmppp_HE_0_0(x)
      fyx=TwoLoop_HelAmpmppp_HE_0_0(y)
      res=(fxy+fyx)
      IF(iterm.LE.0)return
      logm2oms=-DLOG(xs)+IPi
      fxy=TwoLoop_HelAmpmppp_HE_1_0(x)
      fyx=TwoLoop_HelAmpmppp_HE_1_0(y)
      res=res+(-1d0/xs)*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_1_1(x)
      fyx=TwoLoop_HelAmpmppp_HE_1_1(y)
      res=res+(-1d0/xs)*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_1_2(x)
      fyx=TwoLoop_HelAmpmppp_HE_1_2(y)
      res=res+(-1d0/xs)*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_1_3(x)
      fyx=TwoLoop_HelAmpmppp_HE_1_3(y)
      res=res+(-1d0/xs)*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_1_4(x)
      fyx=TwoLoop_HelAmpmppp_HE_1_4(y)
      res=res+(-1d0/xs)*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.1)return
      fxy=TwoLoop_HelAmpmppp_HE_2_0(x)
      fyx=TwoLoop_HelAmpmppp_HE_2_0(y)
      res=res+(-1d0/xs)**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_2_1(x)
      fyx=TwoLoop_HelAmpmppp_HE_2_1(y)
      res=res+(-1d0/xs)**2*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_2_2(x)
      fyx=TwoLoop_HelAmpmppp_HE_2_2(y)
      res=res+(-1d0/xs)**2*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_2_3(x)
      fyx=TwoLoop_HelAmpmppp_HE_2_3(y)
      res=res+(-1d0/xs)**2*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_2_4(x)
      fyx=TwoLoop_HelAmpmppp_HE_2_4(y)
      res=res+(-1d0/xs)**2*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.2)return
      fxy=TwoLoop_HelAmpmppp_HE_3_0(x)
      fyx=TwoLoop_HelAmpmppp_HE_3_0(y)
      res=res+(-1d0/xs)**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_3_1(x)
      fyx=TwoLoop_HelAmpmppp_HE_3_1(y)
      res=res+(-1d0/xs)**3*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_3_2(x)
      fyx=TwoLoop_HelAmpmppp_HE_3_2(y)
      res=res+(-1d0/xs)**3*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_3_3(x)
      fyx=TwoLoop_HelAmpmppp_HE_3_3(y)
      res=res+(-1d0/xs)**3*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_3_4(x)
      fyx=TwoLoop_HelAmpmppp_HE_3_4(y)
      res=res+(-1d0/xs)**3*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.3)return
      fxy=TwoLoop_HelAmpmppp_HE_4_0(x)
      fyx=TwoLoop_HelAmpmppp_HE_4_0(y)
      res=res+(-1d0/xs)**4*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_4_1(x)
      fyx=TwoLoop_HelAmpmppp_HE_4_1(y)
      res=res+(-1d0/xs)**4*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_4_2(x)
      fyx=TwoLoop_HelAmpmppp_HE_4_2(y)
      res=res+(-1d0/xs)**4*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_4_3(x)
      fyx=TwoLoop_HelAmpmppp_HE_4_3(y)
      res=res+(-1d0/xs)**4*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_4_4(x)
      fyx=TwoLoop_HelAmpmppp_HE_4_4(y)
      res=res+(-1d0/xs)**4*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.4)return
      fxy=TwoLoop_HelAmpmppp_HE_5_0(x)
      fyx=TwoLoop_HelAmpmppp_HE_5_0(y)
      res=res+(-1d0/xs)**5*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_5_1(x)
      fyx=TwoLoop_HelAmpmppp_HE_5_1(y)
      res=res+(-1d0/xs)**5*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_5_2(x)
      fyx=TwoLoop_HelAmpmppp_HE_5_2(y)
      res=res+(-1d0/xs)**5*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_5_3(x)
      fyx=TwoLoop_HelAmpmppp_HE_5_3(y)
      res=res+(-1d0/xs)**5*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_5_4(x)
      fyx=TwoLoop_HelAmpmppp_HE_5_4(y)
      res=res+(-1d0/xs)**5*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.5)return
      fxy=TwoLoop_HelAmpmppp_HE_6_0(x)
      fyx=TwoLoop_HelAmpmppp_HE_6_0(y)
      res=res+(-1d0/xs)**6*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_6_1(x)
      fyx=TwoLoop_HelAmpmppp_HE_6_1(y)
      res=res+(-1d0/xs)**6*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_6_2(x)
      fyx=TwoLoop_HelAmpmppp_HE_6_2(y)
      res=res+(-1d0/xs)**6*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_6_3(x)
      fyx=TwoLoop_HelAmpmppp_HE_6_3(y)
      res=res+(-1d0/xs)**6*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_6_4(x)
      fyx=TwoLoop_HelAmpmppp_HE_6_4(y)
      res=res+(-1d0/xs)**6*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.6)return
      fxy=TwoLoop_HelAmpmppp_HE_7_0(x)
      fyx=TwoLoop_HelAmpmppp_HE_7_0(y)
      res=res+(-1d0/xs)**7*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_7_1(x)
      fyx=TwoLoop_HelAmpmppp_HE_7_1(y)
      res=res+(-1d0/xs)**7*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_7_2(x)
      fyx=TwoLoop_HelAmpmppp_HE_7_2(y)
      res=res+(-1d0/xs)**7*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_7_3(x)
      fyx=TwoLoop_HelAmpmppp_HE_7_3(y)
      res=res+(-1d0/xs)**7*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmppp_HE_7_4(x)
      fyx=TwoLoop_HelAmpmppp_HE_7_4(y)
      res=res+(-1d0/xs)**7*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.7)return
      WRITE(*,*)"ERROR: cannot reach here (-+++)"
      STOP
      return
      end

      ! --++ (4 -> 0)
      ! ++++,---- (2 -> 2)
      function TwoLoop_HelAmp_HE_mmpp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 7
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x,y
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,fyx,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_1_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_1_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_2_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_2_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_2_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_3_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_3_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_4_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_4_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_5_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_5_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_6_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_6_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_7_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_7_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmpmmpp_HE_7_0
      IF(iterm.LT.0.OR.iterm.GT.7)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>7"
         STOP
      ENDIF
      x=xt/xs
      y=xu/xs
      fxy=TwoLoop_HelAmpmmpp_HE_0_0(x)
      fyx=TwoLoop_HelAmpmmpp_HE_0_0(y)
      res=(fxy+fyx)
      IF(iterm.LE.0)return
      logm2oms=-DLOG(xs)+IPi
      fxy=TwoLoop_HelAmpmmpp_HE_1_0(x)
      fyx=TwoLoop_HelAmpmmpp_HE_1_0(y)
      res=res+(-1d0/xs)*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_1_1(x)
      fyx=TwoLoop_HelAmpmmpp_HE_1_1(y)
      res=res+(-1d0/xs)*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_1_2(x)
      fyx=TwoLoop_HelAmpmmpp_HE_1_2(y)
      res=res+(-1d0/xs)*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_1_3(x)
      fyx=TwoLoop_HelAmpmmpp_HE_1_3(y)
      res=res+(-1d0/xs)*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_1_4(x)
      fyx=TwoLoop_HelAmpmmpp_HE_1_4(y)
      res=res+(-1d0/xs)*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.1)return
      fxy=TwoLoop_HelAmpmmpp_HE_2_0(x)
      fyx=TwoLoop_HelAmpmmpp_HE_2_0(y)
      res=res+(-1d0/xs)**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_2_1(x)
      fyx=TwoLoop_HelAmpmmpp_HE_2_1(y)
      res=res+(-1d0/xs)**2*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_2_2(x)
      fyx=TwoLoop_HelAmpmmpp_HE_2_2(y)
      res=res+(-1d0/xs)**2*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_2_3(x)
      fyx=TwoLoop_HelAmpmmpp_HE_2_3(y)
      res=res+(-1d0/xs)**2*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_2_4(x)
      fyx=TwoLoop_HelAmpmmpp_HE_2_4(y)
      res=res+(-1d0/xs)**2*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.2)return
      fxy=TwoLoop_HelAmpmmpp_HE_3_0(x)
      fyx=TwoLoop_HelAmpmmpp_HE_3_0(y)
      res=res+(-1d0/xs)**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_3_1(x)
      fyx=TwoLoop_HelAmpmmpp_HE_3_1(y)
      res=res+(-1d0/xs)**3*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_3_2(x)
      fyx=TwoLoop_HelAmpmmpp_HE_3_2(y)
      res=res+(-1d0/xs)**3*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_3_3(x)
      fyx=TwoLoop_HelAmpmmpp_HE_3_3(y)
      res=res+(-1d0/xs)**3*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_3_4(x)
      fyx=TwoLoop_HelAmpmmpp_HE_3_4(y)
      res=res+(-1d0/xs)**3*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.3)return
      fxy=TwoLoop_HelAmpmmpp_HE_4_0(x)
      fyx=TwoLoop_HelAmpmmpp_HE_4_0(y)
      res=res+(-1d0/xs)**4*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_4_1(x)
      fyx=TwoLoop_HelAmpmmpp_HE_4_1(y)
      res=res+(-1d0/xs)**4*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_4_2(x)
      fyx=TwoLoop_HelAmpmmpp_HE_4_2(y)
      res=res+(-1d0/xs)**4*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_4_3(x)
      fyx=TwoLoop_HelAmpmmpp_HE_4_3(y)
      res=res+(-1d0/xs)**4*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_4_4(x)
      fyx=TwoLoop_HelAmpmmpp_HE_4_4(y)
      res=res+(-1d0/xs)**4*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.4)return
      fxy=TwoLoop_HelAmpmmpp_HE_5_0(x)
      fyx=TwoLoop_HelAmpmmpp_HE_5_0(y)
      res=res+(-1d0/xs)**5*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_5_1(x)
      fyx=TwoLoop_HelAmpmmpp_HE_5_1(y)
      res=res+(-1d0/xs)**5*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_5_2(x)
      fyx=TwoLoop_HelAmpmmpp_HE_5_2(y)
      res=res+(-1d0/xs)**5*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_5_3(x)
      fyx=TwoLoop_HelAmpmmpp_HE_5_3(y)
      res=res+(-1d0/xs)**5*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_5_4(x)
      fyx=TwoLoop_HelAmpmmpp_HE_5_4(y)
      res=res+(-1d0/xs)**5*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.5)return
      fxy=TwoLoop_HelAmpmmpp_HE_6_0(x)
      fyx=TwoLoop_HelAmpmmpp_HE_6_0(y)
      res=res+(-1d0/xs)**6*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_6_1(x)
      fyx=TwoLoop_HelAmpmmpp_HE_6_1(y)
      res=res+(-1d0/xs)**6*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_6_2(x)
      fyx=TwoLoop_HelAmpmmpp_HE_6_2(y)
      res=res+(-1d0/xs)**6*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_6_3(x)
      fyx=TwoLoop_HelAmpmmpp_HE_6_3(y)
      res=res+(-1d0/xs)**6*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_6_4(x)
      fyx=TwoLoop_HelAmpmmpp_HE_6_4(y)
      res=res+(-1d0/xs)**6*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.6)return
      fxy=TwoLoop_HelAmpmmpp_HE_7_0(x)
      fyx=TwoLoop_HelAmpmmpp_HE_7_0(y)
      res=res+(-1d0/xs)**7*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_7_1(x)
      fyx=TwoLoop_HelAmpmmpp_HE_7_1(y)
      res=res+(-1d0/xs)**7*logm2oms*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_7_2(x)
      fyx=TwoLoop_HelAmpmmpp_HE_7_2(y)
      res=res+(-1d0/xs)**7*logm2oms**2*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_7_3(x)
      fyx=TwoLoop_HelAmpmmpp_HE_7_3(y)
      res=res+(-1d0/xs)**7*logm2oms**3*(fxy+fyx)
      fxy=TwoLoop_HelAmpmmpp_HE_7_4(x)
      fyx=TwoLoop_HelAmpmmpp_HE_7_4(y)
      res=res+(-1d0/xs)**7*logm2oms**4*(fxy+fyx)
      IF(iterm.LE.7)return
      WRITE(*,*)"ERROR: cannot reach here (--++)"
      STOP
      return
      end

      ! +-+- (4 -> 0)
      ! +--+,-++- (2 -> 2)
      function TwoLoop_HelAmp_HE_pmpm(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 7
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_1_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_1_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_2_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_2_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_2_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_3_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_3_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_4_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_4_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_5_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_5_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_6_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_6_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_7_4
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_7_3
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::TwoLoop_HelAmppmpm_HE_7_0
      IF(iterm.LT.0.OR.iterm.GT.7)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>7"
         STOP
      ENDIF
      x=xt/xs
      fxy=TwoLoop_HelAmppmpm_HE_0_0(x)
      res=fxy
      IF(iterm.LE.0)return
      logm2oms=-DLOG(xs)+IPi
      fxy=TwoLoop_HelAmppmpm_HE_1_0(x)
      res=res+(-1d0/xs)*fxy
      fxy=TwoLoop_HelAmppmpm_HE_1_1(x)
      res=res+(-1d0/xs)*logm2oms*fxy
      fxy=TwoLoop_HelAmppmpm_HE_1_2(x)
      res=res+(-1d0/xs)*logm2oms**2*fxy
      fxy=TwoLoop_HelAmppmpm_HE_1_3(x)
      res=res+(-1d0/xs)*logm2oms**3*fxy
      fxy=TwoLoop_HelAmppmpm_HE_1_4(x)
      res=res+(-1d0/xs)*logm2oms**4*fxy
      IF(iterm.LE.1)return
      fxy=TwoLoop_HelAmppmpm_HE_2_0(x)
      res=res+(-1d0/xs)**2*fxy
      fxy=TwoLoop_HelAmppmpm_HE_2_1(x)
      res=res+(-1d0/xs)**2*logm2oms*fxy
      fxy=TwoLoop_HelAmppmpm_HE_2_2(x)
      res=res+(-1d0/xs)**2*logm2oms**2*fxy
      fxy=TwoLoop_HelAmppmpm_HE_2_3(x)
      res=res+(-1d0/xs)**2*logm2oms**3*fxy
      fxy=TwoLoop_HelAmppmpm_HE_2_4(x)
      res=res+(-1d0/xs)**2*logm2oms**4*fxy
      IF(iterm.LE.2)return
      fxy=TwoLoop_HelAmppmpm_HE_3_0(x)
      res=res+(-1d0/xs)**3*fxy
      fxy=TwoLoop_HelAmppmpm_HE_3_1(x)
      res=res+(-1d0/xs)**3*logm2oms*fxy
      fxy=TwoLoop_HelAmppmpm_HE_3_2(x)
      res=res+(-1d0/xs)**3*logm2oms**2*fxy
      fxy=TwoLoop_HelAmppmpm_HE_3_3(x)
      res=res+(-1d0/xs)**3*logm2oms**3*fxy
      fxy=TwoLoop_HelAmppmpm_HE_3_4(x)
      res=res+(-1d0/xs)**3*logm2oms**4*fxy
      IF(iterm.LE.3)return
      fxy=TwoLoop_HelAmppmpm_HE_4_0(x)
      res=res+(-1d0/xs)**4*fxy
      fxy=TwoLoop_HelAmppmpm_HE_4_1(x)
      res=res+(-1d0/xs)**4*logm2oms*fxy
      fxy=TwoLoop_HelAmppmpm_HE_4_2(x)
      res=res+(-1d0/xs)**4*logm2oms**2*fxy
      fxy=TwoLoop_HelAmppmpm_HE_4_3(x)
      res=res+(-1d0/xs)**4*logm2oms**3*fxy
      fxy=TwoLoop_HelAmppmpm_HE_4_4(x)
      res=res+(-1d0/xs)**4*logm2oms**4*fxy
      IF(iterm.LE.4)return
      fxy=TwoLoop_HelAmppmpm_HE_5_0(x)
      res=res+(-1d0/xs)**5*fxy
      fxy=TwoLoop_HelAmppmpm_HE_5_1(x)
      res=res+(-1d0/xs)**5*logm2oms*fxy
      fxy=TwoLoop_HelAmppmpm_HE_5_2(x)
      res=res+(-1d0/xs)**5*logm2oms**2*fxy
      fxy=TwoLoop_HelAmppmpm_HE_5_3(x)
      res=res+(-1d0/xs)**5*logm2oms**3*fxy
      fxy=TwoLoop_HelAmppmpm_HE_5_4(x)
      res=res+(-1d0/xs)**5*logm2oms**4*fxy
      IF(iterm.LE.5)return
      fxy=TwoLoop_HelAmppmpm_HE_6_0(x)
      res=res+(-1d0/xs)**6*fxy
      fxy=TwoLoop_HelAmppmpm_HE_6_1(x)
      res=res+(-1d0/xs)**6*logm2oms*fxy
      fxy=TwoLoop_HelAmppmpm_HE_6_2(x)
      res=res+(-1d0/xs)**6*logm2oms**2*fxy
      fxy=TwoLoop_HelAmppmpm_HE_6_3(x)
      res=res+(-1d0/xs)**6*logm2oms**3*fxy
      fxy=TwoLoop_HelAmppmpm_HE_6_4(x)
      res=res+(-1d0/xs)**6*logm2oms**4*fxy
      IF(iterm.LE.6)return
      fxy=TwoLoop_HelAmppmpm_HE_7_0(x)
      res=res+(-1d0/xs)**7*fxy
      fxy=TwoLoop_HelAmppmpm_HE_7_1(x)
      res=res+(-1d0/xs)**7*logm2oms*fxy
      fxy=TwoLoop_HelAmppmpm_HE_7_2(x)
      res=res+(-1d0/xs)**7*logm2oms**2*fxy
      fxy=TwoLoop_HelAmppmpm_HE_7_3(x)
      res=res+(-1d0/xs)**7*logm2oms**3*fxy
      fxy=TwoLoop_HelAmppmpm_HE_7_4(x)
      res=res+(-1d0/xs)**7*logm2oms**4*fxy
      IF(iterm.LE.7)return
      WRITE(*,*)"ERROR: cannot reach here (+-+-)"
      STOP
      return
      end

      ! +--+ (4 -> 0)
      ! +-+-,-+-+ (2 -> 2)
      function TwoLoop_HelAmp_HE_pmmp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm  ! iterm from 0 to 7
                     ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double complex TwoLoop_HelAmp_HE_pmpm
      external TwoLoop_HelAmp_HE_pmpm
      res=TwoLoop_HelAmp_HE_pmpm(iterm,xs,xu,xt)
      return
      end

      ! ++++: 4*(-m2/s)^0*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_0_0(x) RESULT(res)
      !use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2,zeta3,zeta4
      !parameter (zeta2=1.64493406684822643647241516665d0)
      !parameter (zeta3=1.20205690315959428539973816151d0)
      !parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      !y=-1d0-x
      res=dcmplx(1.5d0,0d0)
      return
      end


      ! ++++: 4*(-m2/s)^1*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_1_2(x) RESULT(res)
      !use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2,zeta3,zeta4
      !parameter (zeta2=1.64493406684822643647241516665d0)
      !parameter (zeta3=1.20205690315959428539973816151d0)
      !parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((1d0-x*y)/(x*y),0d0)
      return
      end


      ! ++++: 4*(-m2/s)^1*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_1_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2,zeta3,zeta4
      !parameter (zeta2=1.64493406684822643647241516665d0)
      !parameter (zeta3=1.20205690315959428539973816151d0)
      !parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(4d0*lins(1))/x
      return
      end


      ! ++++: 4*(-m2/s)^1*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_1_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(-12d0*(1d0-x*y)*zeta2)/(x*y) - 
     -     (2d0*(3d0*y+2d0*(1d0-x*y))*lins(1)**2)/
     -     (x*y) - 4d0*lins(1)*lins(2)
      return
      end


      ! ++++: 4*(-m2/s)^2*(log(-m2/s))^4
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_2_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(-1 + x*y)**2/(2d0*x**2*y**2)
      return
      end


      ! ++++: 4*(-m2/s)^2*(log(-m2/s))^3
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_2_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=((4 + 4*x - 8*x**2)*lins(1))/(x**2*y)
      return
      end


      ! ++++: 4*(-m2/s)^2*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_2_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*(2*x**2 + 3*y)*lins(1)**2)/(x**2*y) - 
     -  (8*lins(1)*lins(2))/(x*y)
      return
      end


      ! ++++: 4*(-m2/s)^2*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_2_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (4*(-1 + x*y)**2)/(x**2*y**2) + 
     -  (8*(6 + 6*zeta2)*lins(1))/(3d0*y) + 
     -  (8*lins(1)**2)/y - (4*lins(1)**3)/x**2 + 
     -  (8*lins(1)*lins(2))/(x*y) + 
     -  (8*lins(1)**2*lins(2))/(x*y)
      return
      end


      ! ++++: 4*(-m2/s)^2*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_2_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-216*IPi*zeta2 + 24*zeta3)/(x*y) + 
     -  ((24 + 24*x - 
     -       8*x**2*(6 + 96*zeta2 - 3*zeta3))*
     -     lins(1))/(3d0*x**2*y) + 
     -  (24*IPi*(-1 + y)*lins(1)**2)/(x*y) - 
     -  (8*lins(1)**3)/y + lins(1)**4/x**2 + 
     -  (4*(6 + 6*zeta2)*lins(1)*lins(2))/
     -   (3d0*x*y) + ((8 - 24*y)*lins(1)**2*
     -     lins(2))/(x*y) - 
     -  (2*lins(1)**2*lins(2)**2)/(x*y) + 
     -  (48*(1 - y)*lins(1)*lins(3))/(x*y) - 
     -  (48*(1 - y)*lins(4))/(x*y)
      return
      end


      ! ++++: 4*(-m2/s)^3*(log(-m2/s))^4
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_3_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=-12/(x*y)
      return
      end


      ! ++++: 4*(-m2/s)^3*(log(-m2/s))^3
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_3_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (4*(1 - 3*x*y + 
     -       x**2*y**2*(-18 + y + y**2)))/
     -   (x**3*y**3) - 
     -  (16*(x**2 - 3*y)*lins(1))/(x*y**2)
      return
      end


      ! ++++: 4*(-m2/s)^3*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_3_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (8*(9 + 6*zeta2))/(x*y) - 
     -  (8*(10 + 12*y + y**2 + 12*y**3 + 10*y**4)*
     -     lins(1))/(x**3*y**2) + 
     -  (8*(x**2 - 3*y)*lins(1)**2)/(x*y**2) - 
     -  (16*lins(1)*lins(2))/(x**2*y**2)
      return
      end


      ! ++++: 4*(-m2/s)^3*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_3_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (6*(1 - 3*x*y + 
     -       x**2*y**2*
     -        (24 + y + y**2 + 24*zeta2 + 8*zeta3)
     -       ))/(x**3*y**3) + 
     -  (16*(x**2 - 3*y)*(9 + 6*zeta2)*lins(1))/
     -   (3d0*x*y**2) + 
     -  (8*(4 + 6*y + y**2 + 6*y**3 + 4*y**4)*
     -     lins(1)**2)/(x**3*y**2) - 
     -  (48*lins(1)*lins(2))/(x**2*y**2) + 
     -  (16*lins(1)**2*lins(2))/(x**2*y**2)
      return
      end


      ! ++++: 4*(-m2/s)^3*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_3_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (20 - 60*x*y + 
     -     2*x**2*y**2*
     -      (-1260*zeta2 + 
     -        10*(-66 + y + y**2 + 18*zeta3) - 
     -        270*zeta4))/(5d0*x**3*y**3) + 
     -  (4*(10 - 5*y**2 + 10*y**4 + 
     -       24*x**2*(1 + y**2)*zeta2 + 4*zeta3 + 
     -       y*(6 + 4*zeta3) + 
     -       y**3*(6 - 4*x*zeta3))*lins(1))/
     -   (x**3*y**2) + 
     -  (16*(x**2 - 3*y)*lins(1)**2)/(x*y**2) + 
     -  (8*lins(1)**3)/x**3 + 
     -  (8*(15 + 6*zeta2)*lins(1)*lins(2))/
     -   (3d0*x**2*y**2) - 
     -  (16*(-1 + x)*lins(1)**2*lins(2))/
     -   (x**2*y**2) - 
     -  (4*lins(1)**2*lins(2)**2)/(x**2*y**2)
      return
      end


      ! ++++: 4*(-m2/s)^4*(log(-m2/s))^4
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_4_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(62*(-1 + x*y))/(x**2*y**2)
      return
      end


      ! ++++: 4*(-m2/s)^4*(log(-m2/s))^3
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_4_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-2*(x**2 - y)*
     -     (-9 + 27*x*y + 587*x**2*y**2 + 
     -       9*x**3*y**3))/(3d0*x**4*y**4) - 
     -  (16*(46 + 92*x + 97*x**2 + 51*x**3 + 
     -       11*x**4)*lins(1))/(3d0*x**2*y**3)
      return
      end


      ! ++++: 4*(-m2/s)^4*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_4_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*(-1 + x*y)*
     -     (-18 + 54*x*y + 18*x**3*y**3 - 
     -       31*x**2*y**2*(-9 + 12*zeta2)))/
     -   (3d0*x**4*y**4) - 
     -  (4*(-9 - 27*x + 255*x**2 + 555*x**3 + 
     -       610*x**4 + 328*x**5 + 72*x**6)*
     -     lins(1))/(x**4*y**3) + 
     -  (8*(15 + 30*x + 33*x**2 + 18*x**3 + 
     -       4*x**4)*lins(1)**2)/(x**2*y**3) + 
     -  (8*(-7 + 2*x*y)*lins(1)*lins(2))/
     -   (x**3*y**3)
      return
      end


      ! ++++: 4*(-m2/s)^4*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_4_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (4*(-1 + x*y)*
     -     (-10 + 30*x*y + 10*x**3*y**3 - 
     -       3*x**2*y**2*
     -        (206 + 246*zeta2 + 62*zeta3)))/
     -   (3d0*x**4*y**4) + 
     -  (8*(-3*(7 + y - 19*y**2 - 20*y**3 - 
     -          19*y**4 + y**5 + 7*y**6) + 
     -       12*x**2*
     -        (5 - y + 2*y**2 - y**3 + 5*y**4)*
     -        zeta2)*lins(1))/(3d0*x**4*y**3) + 
     -  (4*(-9 - 27*x + 49*x**2 + 143*x**3 + 
     -       186*x**4 + 110*x**5 + 26*x**6)*
     -     lins(1)**2)/(x**4*y**3) + 
     -  (8*(-23 + 6*x*y)*lins(1)*lins(2))/
     -   (x**3*y**3) + 
     -  (16*(3 + x**2)*lins(1)**2*lins(2))/
     -   (x**3*y**3)
      return
      end


      ! ++++: 4*(-m2/s)^4*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_4_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (540 + x*y*(12960*IPi*zeta2 - 
     -        5*(432 - 2023*y - 4262*y**2 - 
     -           4370*y**3 - 1915*y**4 + 
     -           324*y**5 + 108*y**6 + 288*zeta3) 
     -         + 4590*zeta4) + 
     -     6*x**3*y**2*
     -      (11490*zeta2 + 3600*IPi*zeta2 - 
     -        3090*zeta3 + 3420*y*zeta4) - 
     -     6*x**2*(11490*y**4*zeta2 + 
     -        3600*IPi*y**4*zeta2 - 
     -        3090*y**4*zeta3 + 6480*y**2*zeta4))/
     -   (45d0*x**4*y**4) + 
     -  (8*(10 + 3*x*
     -        (10 + 18*IPi*y*zeta2 + 8*zeta3) + 
     -       x**6*(76 + 258*zeta2 + 24*zeta3) + 
     -       2*x**3*
     -        (337 + 480*zeta2 + 90*zeta3) + 
     -       x**5*(338 + 774*zeta2 + 108*zeta3) + 
     -       x**2*(362 + 480*zeta2 - 
     -          54*IPi*zeta2 + 138*zeta3) + 
     -       x**4*(670 + 1254*zeta2 + 198*zeta3))*
     -     lins(1))/(3d0*x**4*y**3) - 
     -  (4*(-18 - 18*y + 7*y**2 + 8*y**3 + 
     -       7*y**4 + 18*x*y**5 - 
     -       8*IPi*(x + y**5 + y**6) + 
     -       6*x*(5 + 10*y + 2*(-1 + x)*y**4)*
     -        zeta2)*lins(1)**2)/(x**4*y**3) - 
     -  (4*(-8 - 8*y + 9*y**3 + 8*x*y**5)*
     -     lins(1)**3)/(3d0*x**4*y**3) + 
     -  ((-2 - 4*y - 4*(-1 + x)*y**4)*lins(1)**4)/
     -   (3d0*x**3*y**3) + 
     -  ((132 - 72*x*y + 6*(28 - 8*x*y)*zeta2)*
     -     lins(1)*lins(2))/(3d0*x**3*y**3) + 
     -  (8*(13 + 4*IPi*(x - y) + 10*y + 3*y**2 + 
     -       4*y**5)*lins(1)**2*lins(2))/
     -   (x**3*y**3) - 
     -  (8*(x - y)*lins(1)**3*lins(2))/
     -   (3d0*x**3*y**3) + 
     -  (2*(-7 + 2*x*y)*lins(1)**2*lins(2)**2)/
     -   (x**3*y**3) + 
     -  (48*(x - y + 4*y**4 + 2*y**5)*zeta2*
     -     lins(3))/(x**3*y**3) + 
     -  (64*(-1 + y**5)*lins(1)*lins(3))/
     -   (x**3*y**3) + 
     -  (16*(x - y + 2*y**4 + y**5)*lins(1)**2*
     -     lins(3))/(x**3*y**3) - 
     -  (16*(x - y)*lins(1)*lins(2)*lins(3))/
     -   (x**3*y**3) - 
     -  (64*(-1 + y**5)*lins(4))/(x**3*y**3) - 
     -  (64*(x - y + 2*y**4 + y**5)*lins(1)*
     -     lins(4))/(x**3*y**3) - 
     -  (64*(x - y)*lins(1)*lins(5))/
     -   (x**3*y**3) - 
     -  (96*(-1 + x)*y*lins(6))/x**3 - 
     -  (48*(x - y)*lins(7))/(x**3*y**3)
      return
      end


      ! ++++: 4*(-m2/s)^5*(log(-m2/s))^4
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_5_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(-300*(-1 + x*y)**2)/(x**3*y**3)
      return
      end


      ! ++++: 4*(-m2/s)^5*(log(-m2/s))^3
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_5_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-8*(x**2 - y)**2*
     -     (-15 + 45*x*y + 2233*x**2*y**2 + 
     -       15*x**3*y**3))/(9d0*x**5*y**5) - 
     -  (32*(110 + 330*x + 559*x**2 + 568*x**3 + 
     -       360*x**4 + 131*x**5 + 21*x**6)*
     -     lins(1))/(3d0*x**3*y**4)
      return
      end


      ! ++++: 4*(-m2/s)^5*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_5_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-4*(x**2 - y)**2*
     -     (-81 + 243*x*y + 81*x**3*y**3 - 
     -       5*x**2*y**2*(-821 + 540*zeta2)))/
     -   (9d0*x**5*y**5) - 
     -  (16*(-15 - 60*x + 963*x**2 + 3099*x**3 + 
     -       5417*x**4 + 5599*x**5 + 3597*x**6 + 
     -       1324*x**7 + 214*x**8)*lins(1))/
     -   (3d0*x**5*y**4) + 
     -  (16*(8 + 7*y**2 - 5*y**3 + 7*y**4 + 
     -       8*y**6)*lins(1)**2)/(x**3*y**4) + 
     -  (16*(-13 + 8*x*y)*lins(1)*lins(2))/
     -   (x**4*y**4)
      return
      end


      ! ++++: 4*(-m2/s)^5*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_5_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -((x**2 - y)**2*
     -      (-459 + 1377*x*y + 459*x**3*y**3 - 
     -        2*x**2*y**2*
     -         (14677 + 24600*zeta2 + 5400*zeta3))
     -      )/(9d0*x**5*y**5) + 
     -  (16*(-268 - 100*y + 507*y**2 + 371*y**3 + 
     -       145*y**4 + 371*y**5 + 507*y**6 - 
     -       100*y**7 - 268*y**8 + 
     -       36*x**2*
     -        (11 + 5*y + 8*y**2 - 2*y**3 + 
     -          8*y**4 + 5*y**5 + 11*y**6)*zeta2)*
     -     lins(1))/(9d0*x**5*y**4) + 
     -  (16*(-15 - 60*x + 156*x**2 + 678*x**3 + 
     -       1382*x**4 + 1564*x**5 + 1086*x**6 + 
     -       427*x**7 + 73*x**8)*lins(1)**2)/
     -   (3d0*x**5*y**4) + 
     -  (16*(-47 + 30*x*y)*lins(1)*lins(2))/
     -   (x**4*y**4) + 
     -  (32*(5 + x**2 - 2*x**3)*lins(1)**2*
     -     lins(2))/(x**4*y**4)
      return
      end


      ! ++++: 4*(-m2/s)^5*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_5_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (14445 - 2179440*x**6*y**2*zeta2 + 
     -     4*x**4*y**2*
     -      (55205*y**2 + 1089720*y*zeta2 - 
     -        541890*zeta4) + 
     -     135*x**5*y**3*
     -      (-107*y**2 + 10464*IPi*zeta2 - 
     -        6456*zeta3 + 5040*zeta4) + 
     -     27*x*y*(27360*IPi*zeta2 - 
     -        5*(535 + 608*zeta3) + 9180*zeta4) - 
     -     40*x**3*y**3*
     -      (11041 + 35316*IPi*(-1 + y)*zeta2 - 
     -        21789*(-1 + y)*zeta3 + 17010*y*zeta4
     -        ) + x**2*y**2*
     -      (293045 - 1879200*IPi*zeta2 - 
     -        2179440*y**2*zeta2 + 923400*zeta3 + 
     -        2167560*y*zeta4))/(405d0*x**5*y**5) 
     -   + (2*(459 + 
     -       108*x*(17 - 36*IPi*zeta2 + 
     -          16*zeta3) + 
     -       2*x**8*(1445 + 8088*zeta2 + 
     -          576*zeta3) + 
     -       4*x**2*(4975 + 7896*zeta2 - 
     -          2592*IPi*zeta2 + 2412*zeta3) + 
     -       6*x**6*(7261 + 26400*zeta2 + 
     -          3048*zeta3) + 
     -       6*x**3*(8879 + 15792*zeta2 - 
     -          1296*IPi*zeta2 + 3096*zeta3) + 
     -       2*x**7*(8339 + 32712*zeta2 + 
     -          3456*zeta3) + 
     -       x**5*(70922 + 217536*zeta2 + 
     -          27792*zeta3) + 
     -       x**4*(78785 + 187728*zeta2 - 
     -          5184*IPi*zeta2 + 28800*zeta3))*
     -     lins(1))/(9d0*x**5*y**4) - 
     -  (8*(-312 - 318*y + 159*y**2 + 197*y**3 + 
     -       145*y**4 + 197*y**5 + 159*y**6 - 
     -       318*y**7 - 312*y**8 - 
     -       12*IPi*x*(-1 + y)*
     -        (-19 + 
     -          x*y*(31 + 12*y + 31*y**2 + 
     -             12*y**3 + 19*y**4)) + 
     -       54*x*(15 + 40*y + 30*y**2 + 
     -          20*y**3 + 4*(-1 + x)*y**4 + 
     -          4*(-1 + x)*y**5 + 6*(-1 + x)*y**6)
     -         *zeta2)*lins(1)**2)/(9d0*x**5*y**4) 
     -   - (16*(-38 - 62*y - 48*y**2 - 24*y**3 - 
     -       15*y**4 + 24*x*y**5 + 24*x*y**6 + 
     -       38*x*y**7)*lins(1)**3)/(9d0*x**5*y**4)
     -    + (4*(9 + 34*x + 48*x**2 + 26*x**3 + 
     -       22*x**4*y + 20*x**5*y + 6*x**6*y)*
     -     lins(1)**4)/(3d0*x**4*y**4) - 
     -  (8*(-44 + 174*x*y + 
     -       18*(-13 + 8*x*y)*zeta2)*lins(1)*
     -     lins(2))/(9d0*x**4*y**4) + 
     -  (16*(73 + 84*y + 57*y**2 + 16*y**3 + 
     -       24*y**5 + 24*y**6 + 38*y**7 - 
     -       12*IPi*(3 + 8*y + 6*y**2 + 4*y**3))*
     -     lins(1)**2*lins(2))/(3d0*x**4*y**4) + 
     -  (16*(x - y)*(-3 + 2*x*y)*lins(1)**3*
     -     lins(2))/(3d0*x**4*y**4) + 
     -  (4*(-13 + 8*x*y)*lins(1)**2*lins(2)**2)/
     -   (x**4*y**4) + 
     -  (96*(9 + 34*x + 48*x**2 + 26*x**3 + 
     -       22*x**4*y + 20*x**5*y + 6*x**6*y)*
     -     zeta2*lins(3))/(x**4*y**4) + 
     -  (64*(-19 - 12*y - 12*y**2 + 12*y**5 + 
     -       12*y**6 + 19*y**7)*lins(1)*lins(3))/
     -   (3d0*x**4*y**4) + 
     -  (32*(-3 - 9*x - 9*x**2 - 3*x**3 + 
     -       7*x**4 + 7*x**5 + 3*x**6)*(-1 + y)*
     -     lins(1)**2*lins(3))/(x**4*y**4) + 
     -  (32*(x - y)*(-3 + 2*x*y)*lins(1)*lins(2)*
     -     lins(3))/(x**4*y**4) - 
     -  (64*(-19 - 12*y - 12*y**2 + 12*y**5 + 
     -       12*y**6 + 19*y**7)*lins(4))/
     -   (3d0*x**4*y**4) + 
     -  (128*(1 - y)*
     -     (-3 - 3*x**3 + 7*x**4 + 7*x**5 + 
     -       3*x**6 + 9*x*y)*lins(1)*lins(4))/
     -   (x**4*y**4) + 
     -  (128*(x - y)*(-3 + 2*x*y)*lins(1)*
     -     lins(5))/(x**4*y**4) - 
     -  (192*(-1 + x)*(3 + 4*x + 3*x**2)*lins(6))/
     -   x**4 + (96*(x - y)*(-3 + 2*x*y)*lins(7))/
     -   (x**4*y**4)
      return
      end


      ! ++++: 4*(-m2/s)^6*(log(-m2/s))^4
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_6_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (6*(-233 + 699*x*y - 732*x**2*y**2 + 
     -      233*x**3*y**3))/(x**4*y**4)
      return
      end


      ! ++++: 4*(-m2/s)^6*(log(-m2/s))^3
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_6_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (105 - 630*x*y - 27649*x**2*y**2 + 
     -     85572*x**3*y**3 - 88096*x**4*y**4 + 
     -     28594*x**5*y**5 + 105*x**6*y**6)/
     -   (3d0*x**6*y**6) - 
     -  (16*(1014 + 4056*x + 9388*x**2 + 
     -       13968*x**3 + 14229*x**4 + 
     -       9910*x**5 + 4578*x**6 + 1275*x**7 + 
     -       163*x**8)*lins(1))/(3d0*x**4*y**5)
      return
      end


      ! ++++: 4*(-m2/s)^6*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_6_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (642 - 3852*x*y + 642*x**6*y**6 + 
     -     x**5*y**5*(63991 - 33552*zeta2) + 
     -     32*x**4*y**4*(-6731 + 3294*zeta2) - 
     -     3*x**3*y**3*(-63563 + 33552*zeta2) + 
     -     x**2*y**2*(-58213 + 33552*zeta2))/
     -   (6d0*x**6*y**6) - 
     -  (2*(-315 - 1575*x + 37380*x**2 + 
     -       158970*x**3 + 374573*x**4 + 
     -       560709*x**5 + 576390*x**6 + 
     -       406880*x**7 + 190704*x**8 + 
     -       53748*x**9 + 6916*x**10)*lins(1))/
     -   (3d0*x**6*y**5) + 
     -  (8*(315 + 1260*x + 2998*x**2 + 
     -       4584*x**3 + 4815*x**4 + 3460*x**5 + 
     -       1656*x**6 + 480*x**7 + 64*x**8)*
     -     lins(1)**2)/(x**4*y**5) - 
     -  (24*(33 - 32*x*y + 10*x**2*y**2)*lins(1)*
     -     lins(2))/(x**5*y**5)
      return
      end


      ! ++++: 4*(-m2/s)^6*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_6_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2592 - 15552*x*y + 
     -     160*x**4*y**4*
     -      (7998*zeta2 + 1647*zeta3) - 
     -     30*x**3*y**3*
     -      (42258*zeta2 + 8388*zeta3) - 
     -     10*x**5*y**5*
     -      (42258*zeta2 + 8388*zeta3) + 
     -     x**2*y**2*
     -      (235601 + 642003*y + 1102623*y**2 + 
     -        1133513*y**3 + 1100031*y**4 + 
     -        647187*y**5 + 227825*y**6 + 
     -        10368*y**7 + 2592*y**8 + 
     -        422580*zeta2 + 83880*zeta3))/
     -   (15d0*x**6*y**6) + 
     -  (2*(-10915 - 6127*y + 20490*y**2 + 
     -       27722*y**3 + 17159*y**4 + 
     -       8352*y**5 + 17159*y**6 + 
     -       27722*y**7 + 20490*y**8 - 
     -       6127*y**9 - 10915*y**10 + 
     -       432*x**2*
     -        (31 + 33*y + 45*y**2 + 3*y**3 + 
     -          2*y**4 + 3*y**5 + 45*y**6 + 
     -          33*y**7 + 31*y**8)*zeta2)*lins(1))
     -    /(9d0*x**6*y**5) + 
     -  (2*(-315 - 1575*x + 5370*x**2 + 
     -       30930*x**3 + 84449*x**4 + 
     -       138477*x**5 + 153210*x**6 + 
     -       114860*x**7 + 56796*x**8 + 
     -       16818*x**9 + 2266*x**10)*lins(1)**2)/
     -   (3d0*x**6*y**5) - 
     -  (4*(775 - 820*x*y + 158*x**2*y**2)*
     -     lins(1)*lins(2))/(x**5*y**5) + 
     -  (16*(35 + 6*x**2 - 8*x**3 + 15*x**4)*
     -     lins(1)**2*lins(2))/(x**5*y**5)
      return
      end


      ! ++++: 4*(-m2/s)^6*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_6_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=58.3098030445699937225360954175d0)
      double precision rat2
      parameter (rat2=236.549015222849968612680477087d0)
      double precision rat3
      parameter (rat3=742.104307909604519774011299435d0)
      double precision rat4
      parameter (rat4=1615.12314030131826741996233522d0)
      double precision rat5
      parameter (rat5=2113.5160891399874450721908349d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (48*IPi*(193*y**2 + 427*x**4*y**3 + 
     -       21*x**2*y*(22 + 39*y**3) - 
     -       7*x**3*y**2*(117 + 61*y**3) - 
     -       x*(193 + 462*y**3))*zeta2)/
     -   (x**5*y**5) + 
     -  (118*(1 + 6*x**11 + x**12 + 
     -       x**5*(rat4 - (630845*zeta2)/177d0 + 
     -          (118726*zeta3)/59d0 - 
     -          (176952*zeta4)/59d0) + 
     -       x**6*(rat5 - (736768*zeta2)/177d0 + 
     -          (140744*zeta3)/59d0 - 
     -          (168912*zeta4)/59d0) + 
     -       x**4*(rat3 - (382175*zeta2)/177d0 + 
     -          (70302*zeta3)/59d0 - 
     -          (126534*zeta4)/59d0) + 
     -       x**7*(rat4 - (630845*zeta2)/177d0 + 
     -          (118726*zeta3)/59d0 - 
     -          (110856*zeta4)/59d0) + 
     -       x**3*(rat2 - (299285*zeta2)/354d0 + 
     -          (27891*zeta3)/59d0 - 
     -          (59760*zeta4)/59d0) + 
     -       x**8*(rat3 - (382175*zeta2)/177d0 + 
     -          (69694*zeta3)/59d0 - 
     -          (48504*zeta4)/59d0) + 
     -       x**2*(rat1 - (59857*zeta2)/354d0 + 
     -          (19373*zeta3)/177d0 - 
     -          (16185*zeta4)/59d0) + 
     -       x**9*(rat2 - (299285*zeta2)/354d0 + 
     -          (26675*zeta3)/59d0 - 
     -          (13860*zeta4)/59d0) + 
     -       x**10*(rat1 - (59857*zeta2)/354d0 + 
     -          (5335*zeta3)/59d0 - 
     -          (2772*zeta4)/59d0) + 
     -       x*(6 + (1544*zeta3)/177d0 - 
     -          (1479*zeta4)/59d0)))/(x**6*y**6) + 
     -  ((144*IPi*(29*y + 38*x**2*y - 
     -          x*(29 + 38*y**2))*zeta2)/
     -      (x**5*y**5) + 
     -     (1728*(1 + 
     -          x**10*
     -           (6.14975565843621399176954732510d0 + 
     -             (31825*zeta2)/648d0 + 
     -             (80*zeta3)/27d0) + 
     -          x*(5 + (145*zeta3)/27d0) + 
     -          x**9*
     -           (42.8850951646090534979423868313d0 + 
     -             (18025*zeta2)/72d0 + 
     -             (200*zeta3)/9d0) + 
     -          x**2*
     -           (61.4730581275720164609053497942d0 + 
     -             (5635*zeta2)/54d0 + 
     -             (1165*zeta3)/36d0) + 
     -          x**8*
     -           (136.741640946502057613168724280d0 + 
     -             (84095*zeta2)/108d0 + 
     -             (230*zeta3)/3d0) + 
     -          x**3*
     -           (215.892232510288065843621399177d0 + 
     -             (11270*zeta2)/27d0 + 
     -             (715*zeta3)/9d0) + 
     -          x**4*
     -           (399.773276748971193415637860082d0 + 
     -             (28595*zeta2)/27d0 + 
     -             (8255*zeta3)/54d0) + 
     -          x**7*
     -           (273.648019547325102880658436214d0 + 
     -             (17875*zeta2)/12d0 + 
     -             (4325*zeta3)/27d0) + 
     -          x**5*
     -           (464.697016460905349794238683128d0 + 
     -             (46340*zeta2)/27d0 + 
     -             (1910*zeta3)/9d0) + 
     -          x**6*
     -           (403.134387860082304526748971193d0 + 
     -             (417215*zeta2)/216d0 + 
     -             (2675*zeta3)/12d0)))/
     -      (5d0*x**6*y**5))*lins(1) + 
     -  ((9696 + 11040*y - 4860*y**2 - 
     -       11492*y**3 - 7061*y**4 - 1620*y**5 - 
     -       7061*y**6 - 11492*y**7 - 4860*y**8 + 
     -       11040*y**9 + 9696*y**10 - 
     -       48*IPi*(193 + 421*y + 456*y**2 + 
     -          228*y**3 + 228*x*y**7 + 
     -          228*x*y**8 + 193*x*y**9) - 
     -       216*x*(145 + 480*y + 570*y**2 + 
     -          380*y**3 + 76*(-1 + x)*y**6 + 
     -          76*(-1 + x)*y**7 + 
     -          58*(-1 + x)*y**8)*zeta2)*
     -     lins(1)**2)/(9d0*x**6*y**5) + 
     -  ((3088 + 6736*y + 7296*y**2 + 3648*y**3 - 
     -       630*y**5 - 3648*x*y**7 - 
     -       3648*x*y**8 - 3088*x*y**9)*lins(1)**3
     -     )/(9d0*x**6*y**5) - 
     -  (2*(29 + 96*y + 114*y**2 + 76*y**3 + 
     -       76*(-1 + x)*y**6 + 
     -       76*(-1 + x)*y**7 + 58*(-1 + x)*y**8)*
     -     lins(1)**4)/(3d0*x**5*y**5) + 
     -  ((-1219 - 6132*x*y + 3366*x**2*y**2 + 
     -       216*(33 - 32*x*y + 10*x**2*y**2)*
     -        zeta2)*lins(1)*lins(2))/
     -   (9d0*x**5*y**5) + 
     -  (4*(1133 + 1788*y + 1914*y**2 + 
     -       1076*y**3 + 237*y**4 + 912*y**7 + 
     -       912*y**8 + 772*y**9 + 
     -       24*IPi*(-29*y - 38*x**2*y + 
     -          x*(29 + 38*y**2)))*lins(1)**2*
     -     lins(2))/(3d0*x**5*y**5) + 
     -  (8*(x - y)*(-29 + 38*x*y)*lins(1)**3*
     -     lins(2))/(3d0*x**5*y**5) - 
     -  (6*(33 - 32*x*y + 10*x**2*y**2)*
     -     lins(1)**2*lins(2)**2)/(x**5*y**5) - 
     -  (48*(29 + 96*y + 114*y**2 + 76*y**3 + 
     -       76*(-1 + x)*y**6 + 
     -       76*(-1 + x)*y**7 + 58*(-1 + x)*y**8)*
     -     zeta2*lins(3))/(x**5*y**5) + 
     -  (32*(-193 - 228*y - 228*y**2 + 228*y**7 + 
     -       228*y**8 + 193*y**9)*lins(1)*lins(3))
     -    /(3d0*x**5*y**5) - 
     -  (16*(29 + 96*y + 114*y**2 + 76*y**3 + 
     -       38*(-1 + x)*y**6 + 
     -       38*(-1 + x)*y**7 + 29*(-1 + x)*y**8)*
     -     lins(1)**2*lins(3))/(x**5*y**5) + 
     -  (16*(x - y)*(-29 + 38*x*y)*lins(1)*
     -     lins(2)*lins(3))/(x**5*y**5) - 
     -  (32*(-193 - 228*y - 228*y**2 + 228*y**7 + 
     -       228*y**8 + 193*y**9)*lins(4))/
     -   (3d0*x**5*y**5) + 
     -  (64*(29 + 96*y + 114*y**2 + 76*y**3 + 
     -       38*(-1 + x)*y**6 + 
     -       38*(-1 + x)*y**7 + 29*(-1 + x)*y**8)*
     -     lins(1)*lins(4))/(x**5*y**5) + 
     -  (64*(x - y)*(-29 + 38*x*y)*lins(1)*
     -     lins(5))/(x**5*y**5) - 
     -  (96*(-1 + x)*(29 + 20*x + 29*x**2)*y*
     -     lins(6))/x**5 + 
     -  (48*(x - y)*(-29 + 38*x*y)*lins(7))/
     -   (x**5*y**5)
      return
      end


      ! ++++: 4*(-m2/s)^7*(log(-m2/s))^4
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_7_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (28*(x**2 - y)*
     -    (-227 + 681*x*y - 810*x**2*y**2 + 
     -      227*x**3*y**3))/(x**5*y**5)
      return
      end


      ! ++++: 4*(-m2/s)^7*(log(-m2/s))^3
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_7_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (4*(x**2 - y)*
     -     (1134 - 6804*x*y - 497425*x**2*y**2 + 
     -       1520625*x**3*y**3 - 
     -       1706404*x**4*y**4 + 
     -       507631*x**5*y**5 + 1134*x**6*y**6))/
     -   (45d0*x**7*y**7) - 
     -  (32*(2282 + 11410*x + 33658*x**2 + 
     -       66172*x**3 + 92379*x**4 + 
     -       93457*x**5 + 68931*x**6 + 
     -       36481*x**7 + 13267*x**8 + 
     -       2997*x**9 + 319*x**10)*lins(1))/
     -   (3d0*x**5*y**6)
      return
      end


      ! ++++: 4*(-m2/s)^7*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_7_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-158.093508417508417508417508418d0)
      double precision rat2
      parameter (rat2=-845.467542087542087542087542088d0)
      double precision rat3
      parameter (rat3=-2369.380632996632996632996633d0)
      double precision rat4
      parameter (rat4=-4338.71727946127946127946127946d0)
      double precision rat5
      parameter (rat5=-5302.42136026936026936026936027d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (330*(x**2 - y)*
     -     (1 + 6*x + 6*x**11 + x**12 + 
     -       x**2*(rat1 + (12712*zeta2)/165d0) + 
     -       x**10*(rat1 + (12712*zeta2)/165d0) + 
     -       x**3*(rat2 + (12712*zeta2)/33d0) + 
     -       x**9*(rat2 + (12712*zeta2)/33d0) + 
     -       x**4*(rat3 + (3136*zeta2)/3d0) + 
     -       x**8*(rat3 + (3136*zeta2)/3d0) + 
     -       x**5*(rat4 + (61712*zeta2)/33d0) + 
     -       x**7*(rat4 + (61712*zeta2)/33d0) + 
     -       x**6*(rat5 + (373856*zeta2)/165d0)))/
     -   (x**7*y**7) - 
     -  (8*(-1134 - 6804*x + 220453*x**2 + 
     -       1164635*x**3 + 3455105*x**4 + 
     -       6757766*x**5 + 9432669*x**6 + 
     -       9612137*x**7 + 7185615*x**8 + 
     -       3868370*x**9 + 1430930*x**10 + 
     -       327534*x**11 + 35094*x**12)*lins(1))/
     -   (15d0*x**7*y**6) - 
     -  (16*(-128 - 128*y - 288*y**2 - 64*y**3 - 
     -       91*y**4 + 63*y**5 - 91*y**6 - 
     -       64*y**7 - 288*y**8 + 128*x*y**9)*
     -     lins(1)**2)/(x**5*y**6) - 
     -  (16*(191 - 256*x*y + 170*x**2*y**2)*
     -     lins(1)*lins(2))/(x**6*y**6)
      return
      end


      ! ++++: 4*(-m2/s)^7*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_7_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-1199.70341049382716049382716049d0)
      double precision rat2
      parameter (rat2=-1583.09564814814814814814814815d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (576*(x - y**2)*
     -     (-1 - 6*x - 6*x**11 - x**12 + 
     -       x**6*(rat2 - (118097*zeta2)/18d0 - 
     -          (11683*zeta3)/9d0) + 
     -       x**5*(-1555.51086419753086419753086420d0 - 
     -          (593831*zeta2)/108d0 - 
     -          (19285*zeta3)/18d0) + 
     -       x**7*(-1555.51086419753086419753086420d0 - 
     -          (593831*zeta2)/108d0 - 
     -          (19285*zeta3)/18d0) + 
     -       x**4*(rat1 - (171817*zeta2)/54d0 - 
     -          (5390*zeta3)/9d0) + 
     -       x**8*(rat1 - (171817*zeta2)/54d0 - 
     -          (5390*zeta3)/9d0) + 
     -       x**3*(-551.550462962962962962962962963d0 - 
     -          (28915*zeta2)/24d0 - 
     -          (7945*zeta3)/36d0) + 
     -       x**9*(-551.550462962962962962962962963d0 - 
     -          (28915*zeta2)/24d0 - 
     -          (7945*zeta3)/36d0) + 
     -       x**2*(-121.310092592592592592592592593d0 - 
     -          (5783*zeta2)/24d0 - 
     -          (1589*zeta3)/36d0) + 
     -       x**10*(-121.310092592592592592592592593d0 - 
     -          (5783*zeta2)/24d0 - 
     -          (1589*zeta3)/36d0)))/(x**7*y**7) + 
     -  (4*(-626303 - 514743*y + 1075230*y**2 + 
     -       2281890*y**3 + 2039955*y**4 + 
     -       813647*y**5 + 258074*y**6 + 
     -       813647*y**7 + 2039955*y**8 + 
     -       2281890*y**9 + 1075230*y**10 - 
     -       514743*y**11 - 626303*y**12 + 
     -       3600*x**2*
     -        (193 + 319*y + 503*y**2 + 
     -          213*y**3 + 82*y**4 + 2*y**5 + 
     -          82*y**6 + 213*y**7 + 503*y**8 + 
     -          319*y**9 + 193*y**10)*zeta2)*
     -     lins(1))/(225d0*x**7*y**6) + 
     -  (8*(-1134 - 6804*x + 28770*x**2 + 
     -       206220*x**3 + 711210*x**4 + 
     -       1532676*x**5 + 2301433*x**6 + 
     -       2480901*x**7 + 1943715*x**8 + 
     -       1090855*x**9 + 419805*x**10 + 
     -       99951*x**11 + 11141*x**12)*lins(1)**2
     -     )/(15d0*x**7*y**6) - 
     -  (8*(23953 - 35900*x*y + 16810*x**2*y**2)*
     -     lins(1)*lins(2))/(15d0*x**6*y**6) - 
     -  (32*(-63 - 10*x**2 + 12*x**3 - 15*x**4 + 
     -       28*x**5)*lins(1)**2*lins(2))/
     -   (x**6*y**6)
      return
      end


      ! ++++: 4*(-m2/s)^7*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_7_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-81.2639298385649669271655126753d0)
      double precision rat2
      parameter (rat2=189.205791613298435150637634349d0)
      double precision rat3
      parameter (rat3=-665.546583505039817341426741661d0)
      double precision rat4
      parameter (rat4=-351.319649192824834635827563377d0)
      double precision rat5
      parameter (rat5=946.028958066492175753188171744d0)
      double precision rat6
      parameter (rat6=1443.20766274990254496853594698d0)
      double precision rat7
      parameter (rat7=-1907.79417497354792003118561007d0)
      double precision rat8
      parameter (rat8=-1274.82065802256006633129760601d0)
      double precision rat9
      parameter (rat9=2457.70952831764771398340480036d0)
      double precision rat10
      parameter (rat10=3245.53321824358189007072450855d0)
      double precision rat11
      parameter (rat11=-3592.31163334632733752854040207d0)
      double precision rat12
      parameter (rat12=-3377.6616429680780630271079678d0)
      double precision rat13
      parameter (rat13=4476.24759146850810268975886841d0)
      double precision rat14
      parameter (rat14=5195.24085314918973102411315921d0)
      double precision rat15
      parameter (rat15=-5539.27137487702103172393309944d0)
      double precision rat16
      parameter (rat16=-4802.6276104026285014200590299d0)
      double precision rat17
      parameter (rat17=6080.46555660745113326279445342d0)
      double precision rat18
      parameter (rat18=6087.33209333407584785877373726d0)
      double precision rat19
      parameter (rat19=-5610.40553591604635765687166255d0)
      double precision rat20
      parameter (rat20=-4765.54168290917191067550258952d0)
      double precision rat21
      parameter (rat21=5022.17742384585398451857214457d0)
      double precision rat22
      parameter (rat22=6332.74582614022386812942028178d0)
      double precision rat23
      parameter (rat23=-3504.0848441647639731952256316d0)
      double precision rat24
      parameter (rat24=-3502.96820181544801470178760372d0)
      double precision rat25
      parameter (rat25=2752.4753577991869465946427577d0)
      double precision rat26
      parameter (rat26=5258.89892521022442501531436209d0)
      double precision rat27
      parameter (rat27=-1856.11070891574316422565016428d0)
      double precision rat28
      parameter (rat28=-1202.38968204909258537370137303d0)
      double precision rat29
      parameter (rat29=843.575207439995544912847357576d0)
      double precision rat30
      parameter (rat30=3534.07551372723728907946761709d0)
      double precision rat31
      parameter (rat31=-675.697499582335579439772790555d0)
      double precision rat32
      parameter (rat32=-88.6913230680700191197490300904d0)
      double precision rat33
      parameter (rat33=1794.42434705128919084479590132d0)
      double precision rat34
      parameter (rat34=77.6502346160271760316311187838d0)
      double precision rat35
      parameter (rat35=588.22166286127972378459653617d0)
      double precision rat36
      parameter (rat36=18.5058544609653926354934318403d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-128*IPi*(945 + 30770*y**3 + 
     -       44650*y**4 + 18028*y**5 + 420*y**7 + 
     -       840*y**8 + 2345*y**9 + 1925*y**10 + 
     -       1129*y**11)*zeta2)/(5d0*x**6*y**6) + 
     -  (35914*(-1 - x**13/2d0 + 
     -       x**11*(rat34 + rat35*zeta2 - 
     -          (2730996*zeta3)/17957d0 - 
     -          (1224720*zeta4)/17957d0) + 
     -       x**12*(rat36 + 
     -          (8058102*zeta2)/89785d0 - 
     -          (287436*zeta3)/17957d0 - 
     -          (287280*zeta4)/17957d0) + 
     -       x**10*(rat32 + rat33*zeta2 + 
     -          rat31*zeta3 + 
     -          (516240*zeta4)/17957d0) + 
     -       x*(-6 - (307488*zeta3)/17957d0 + 
     -          (596700*zeta4)/17957d0) + 
     -       x**2*(rat1 + rat2*zeta2 - 
     -          (2669796*zeta3)/17957d0 + 
     -          (6787800*zeta4)/17957d0) + 
     -       x**4*(rat8 + rat9*zeta2 + 
     -          rat7*zeta3 + rat10*zeta4) + 
     -       x**5*(rat12 + rat13*zeta2 + 
     -          rat11*zeta3 + rat14*zeta4) + 
     -       x**6*(rat15 + rat18*zeta2 + 
     -          rat16*zeta3 + rat17*zeta4) + 
     -       x**7*(rat19 + rat22*zeta2 + 
     -          rat20*zeta3 + rat21*zeta4) + 
     -       x**8*(rat23 + rat26*zeta2 + 
     -          rat24*zeta3 + rat25*zeta4) + 
     -       x**9*(rat28 + rat30*zeta2 + 
     -          rat27*zeta3 + rat29*zeta4) + 
     -       x**3*(rat4 + rat5*zeta2 + 
     -          rat3*zeta3 + rat6*zeta4)))/
     -   (45d0*x**7*y**6) + 
     -  (2*(820039 + 
     -       120*x**2*
     -        (71593 + 275443*y + 928867*y**2 + 
     -          2027913*y**3 + 3276392*y**4 + 
     -          3780208*y**5 + 3276392*y**6 + 
     -          2027913*y**7 + 928867*y**8 + 
     -          275443*y**9 + 71593*y**10)*zeta2 
     -        - 32400*IPi*x*
     -        (-65*y + 28*x**3*y**2 - 
     -          14*x**2*y*(9 + 2*y**2) + 
     -          x*(65 + 126*y**2))*zeta2 + 
     -       1396800*zeta3 + 
     -       60*y**6*(-18349 + 16800*zeta3) + 
     -       100*y**8*(29417 + 18252*zeta3) + 
     -       10*y**7*(89059 + 65880*zeta3) + 
     -       100*y**4*(29417 + 90828*zeta3) + 
     -       20*y**10*(293711 + 120960*zeta3) + 
     -       10*y**9*(490969 + 276480*zeta3) + 
     -       10*y**5*(89059 + 348120*zeta3) + 
     -       y**12*(820039 + 460800*zeta3) + 
     -       20*y**2*(293711 + 597600*zeta3) + 
     -       y**11*(3220409 + 1382400*zeta3) + 
     -       10*y**3*(490969 + 1385280*zeta3) + 
     -       y*(3220409 + 6004800*zeta3))*lins(1))
     -    /(225d0*x**7*y**6) - 
     -  (2*(-475260 - 633240*y + 126960*y**2 + 
     -       784780*y**3 + 812385*y**4 + 
     -       404457*y**5 + 258074*y**6 + 
     -       404457*y**7 + 812385*y**8 + 
     -       784780*y**9 + 126960*y**10 - 
     -       633240*y**11 - 475260*y**12 - 
     -       480*IPi*x*(-1 + y)*
     -        (-1129 + 
     -          x*y*(3054 + 2345*y + 3894*y**2 + 
     -             2765*y**3 + 3894*y**4 + 
     -             2765*y**5 + 3474*y**6 + 
     -             1925*y**7 + 1129*y**8)) + 
     -       5400*x*(325 + 1280*y + 2030*y**2 + 
     -          1820*y**3 + 700*y**4 + 280*y**5 + 
     -          56*(-1 + x)*y**6 + 
     -          112*(-1 + x)*y**7 + 
     -          308*(-1 + x)*y**8 + 
     -          252*(-1 + x)*y**9 + 
     -          130*(-1 + x)*y**10)*zeta2)*
     -     lins(1)**2)/(225d0*x**7*y**6) + 
     -  (16*(567 + 3402*x + 42781*x**2 + 
     -       182720*x**3 + 484845*x**4 + 
     -       880482*x**5 + 1175559*x**6 + 
     -       1174992*x**7 + 878760*x**8 + 
     -       479700*x**9 + 180760*x**10 + 
     -       41976*x**11 + 4516*x**12)*lins(1)**3)
     -    /(45d0*x**7*y**6) + 
     -  (4*(195 + 1174*x + 3248*x**2 + 
     -       5354*x**3 + 5288*x**4 + 2268*x**5 + 
     -       2212*x**6*y + 2936*x**7*y + 
     -       2054*x**8*y + 788*x**9*y + 
     -       130*x**10*y)*lins(1)**4)/
     -   (3d0*x**6*y**6) + 
     -  (4*(41580 + 2741460*y**3 + 4042365*y**4 + 
     -       1535108*y**5 + 
     -       240*IPi*
     -        (2074*y - 9014*x**3*y**2 + 
     -          x*(-2074 + 2223*y**2) + 
     -          x**2*y*(-2223 + 9014*y**2)) - 
     -       1800*y**3*(1312 + 1925*y + 804*y**2)*
     -        zeta2)*lins(1)*lins(2))/
     -   (225d0*x**6*y**6) + 
     -  (8*(11141 + 22600*y + 33050*y**2 + 
     -       27860*y**3 + 12265*y**4 + 
     -       2216*y**5 + 3360*y**7 + 6720*y**8 + 
     -       18760*y**9 + 15400*y**10 + 
     -       9032*y**11 + 
     -       120*IPi*
     -        (-65*y + 28*x**3*y**2 - 
     -          14*x**2*y*(9 + 2*y**2) + 
     -          x*(65 + 126*y**2)))*lins(1)**2*
     -     lins(2))/(15d0*x**6*y**6) - 
     -  (16*(x - y)*(65 - 126*x*y + 28*x**2*y**2)*
     -     lins(1)**3*lins(2))/(3d0*x**6*y**6) + 
     -  (8*(191 - 317*x + 804*x**2)*lins(1)**2*
     -     lins(2)**2)/(x**6*y**3) + 
     -  (96*(195 + 1174*x + 3248*x**2 + 
     -       5354*x**3 + 5288*x**4 + 2268*x**5 + 
     -       2212*x**6*y + 2936*x**7*y + 
     -       2054*x**8*y + 788*x**9*y + 
     -       130*x**10*y)*zeta2*lins(3))/
     -   (x**6*y**6) + 
     -  (128*(-1 + y)*
     -     (1129 + 3054*y + 5399*y**2 + 
     -       6239*y**3 + 6659*y**4 + 6659*y**5 + 
     -       6659*y**6 + 6239*y**7 + 5399*y**8 + 
     -       3054*y**9 + 1129*y**10)*lins(1)*
     -     lins(3))/(15d0*x**6*y**6) + 
     -  (32*(-65 - 325*x - 751*x**2 - 1054*x**3 - 
     -       830*x**4 - 166*x**5 + 636*x**6 + 
     -       969*x**7 + 763*x**8 + 329*x**9 + 
     -       65*x**10)*(-1 + y)*lins(1)**2*lins(3)
     -     )/(x**6*y**6) - 
     -  (32*(x - y)*(65 - 126*x*y + 28*x**2*y**2)*
     -     lins(1)*lins(2)*lins(3))/(x**6*y**6) - 
     -  (128*(-1 + y)*
     -     (1129 + 3054*y + 5399*y**2 + 
     -       6239*y**3 + 6659*y**4 + 6659*y**5 + 
     -       6659*y**6 + 6239*y**7 + 5399*y**8 + 
     -       3054*y**9 + 1129*y**10)*lins(4))/
     -   (15d0*x**6*y**6) - 
     -  (128*(-65 - 325*x - 751*x**2 - 
     -       1054*x**3 - 830*x**4 - 166*x**5 + 
     -       636*x**6 + 969*x**7 + 763*x**8 + 
     -       329*x**9 + 65*x**10)*(-1 + y)*
     -     lins(1)*lins(4))/(x**6*y**6) - 
     -  (128*(x - y)*
     -     (65 - 126*x*y + 28*x**2*y**2)*lins(1)*
     -     lins(5))/(x**6*y**6) - 
     -  (192*(-1 + x)*
     -     (65 + 134*x + 166*x**2 + 134*x**3 + 
     -       65*x**4)*lins(6))/x**6 - 
     -  (96*(x - y)*(65 - 126*x*y + 28*x**2*y**2)*
     -     lins(7))/(x**6*y**6)
      return
      end


      ! ++++: 4*(-m2/s)^8*(log(-m2/s))^4
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_8_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (4*(x**2 - y)**2*
     -    (-7099 + 21297*x*y - 30200*x**2*y**2 + 
     -      7099*x**3*y**3))/(x**6*y**6)
      return
      end


      ! ++++: 4*(-m2/s)^8*(log(-m2/s))^3
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_8_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (4*(x**2 - y)**2*
     -     (3465 - 20790*x*y - 
     -       2316437*x**2*y**2 + 
     -       7035936*x**3*y**3 - 
     -       9053390*x**4*y**4 + 
     -       2347622*x**5*y**5 + 3465*x**6*y**6))/
     -   (45d0*x**8*y**8) - 
     -  (32*(10102 + 60612*x + 217414*x**2 + 
     -       531460*x**3 + 944650*x**4 + 
     -       1256572*x**5 + 1269017*x**6 + 
     -       974449*x**7 + 564295*x**8 + 
     -       240955*x**9 + 72325*x**10 + 
     -       13755*x**11 + 1255*x**12)*lins(1))/
     -   (3d0*x**6*y**7)
      return
      end


      ! ++++: 4*(-m2/s)^8*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_8_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-256.806642611466312168977231964d0)
      double precision rat2
      parameter (rat2=-1339.03321305733156084488615982d0)
      double precision rat3
      parameter (rat3=-3888.52275748559853136671519909d0)
      double precision rat4
      parameter (rat4=1537.35519402418180667215294043d0)
      double precision rat5
      parameter (rat5=-7453.89175159840476039754383744d0)
      double precision rat6
      parameter (rat6=2913.81907957207064632525163006d0)
      double precision rat7
      parameter (rat7=-9284.19112911734295541347513241d0)
      double precision rat8
      parameter (rat8=3615.75489016901943406976008103d0)
      double precision rat9
      parameter (rat9=6634.31630591630591630591630592d0)
      double precision rat10
      parameter (rat10=28194.0291486291486291486291486d0)
      double precision rat11
      parameter (rat11=37437.1131313131313131313131313d0)
      double precision rat12
      parameter (rat12=38025.7336219336219336219336219d0)
      double precision rat13
      parameter (rat13=29564.3330447330447330447330447d0)
      double precision rat14
      parameter (rat14=17420.4906204906204906204906205d0)
      double precision rat15
      parameter (rat15=7587.15382395382395382395382395d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (15797*(x**2 - y)**2*
     -     (1 + 6*x + 6*x**11 + x**12 + 
     -       x**2*(rat1 + 
     -          (1703760*zeta2)/15797d0) + 
     -       x**10*(rat1 + 
     -          (1703760*zeta2)/15797d0) + 
     -       x**3*(rat2 + 
     -          (8518800*zeta2)/15797d0) + 
     -       x**9*(rat2 + 
     -          (8518800*zeta2)/15797d0) + 
     -       x**4*(rat3 + rat4*zeta2) + 
     -       x**8*(rat3 + rat4*zeta2) + 
     -       x**5*(rat5 + rat6*zeta2) + 
     -       x**7*(rat5 + rat6*zeta2) + 
     -       x**6*(rat7 + rat8*zeta2)))/
     -   (15d0*x**8*y**8) - 
     -  (1848*(-1 - 7*x + (1018478*x**2)/3465d0 + 
     -       (2142061*x**3)/1155d0 + rat9*x**4 + 
     -       (3696985*x**5)/231d0 + rat10*x**6 + 
     -       rat11*x**7 + rat12*x**8 + 
     -       rat13*x**9 + rat14*x**10 + 
     -       rat15*x**11 + 
     -       (8041384*x**12)/3465d0 + 
     -       (517193*x**13)/1155d0 + 
     -       (4321*x**14)/105d0)*lins(1))/
     -   (x**8*y**7) + 
     -  (16*(3003 + 18018*x + 66531*x**2 + 
     -       167490*x**3 + 305389*x**4 + 
     -       414814*x**5 + 427049*x**6 + 
     -       334558*x**7 + 198380*x**8 + 
     -       87232*x**9 + 27136*x**10 + 
     -       5376*x**11 + 512*x**12)*lins(1)**2)/
     -   (x**6*y**7) + 
     -  (16*(-743 + 1280*x*y - 1315*x**2*y**2 + 
     -       210*x**3*y**3)*lins(1)*lins(2))/
     -   (x**7*y**7)
      return
      end


      ! ++++: 4*(-m2/s)^8*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_8_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-155.224345547733525875602378335d0)
      double precision rat2
      parameter (rat2=-721.121727738667629378011891673d0)
      double precision rat3
      parameter (rat3=-4649.08757938266134987446462856d0)
      double precision rat4
      parameter (rat4=-1423.73611556912103360190791885d0)
      double precision rat5
      parameter (rat5=-8315.37557229360508049032639197d0)
      double precision rat6
      parameter (rat6=-1434.21409584447835813956032535d0)
      double precision rat7
      parameter (rat7=-10074.1692512184315463003987594d0)
      double precision rat8
      parameter (rat8=-1968.32845960714813173829567272d0)
      double precision rat9
      parameter (rat9=-1163.75072493258285608012383969d0)
      double precision rat10
      parameter (rat10=-694.58928910552636576565170602d0)
      double precision rat11
      parameter (rat11=-2625.68536219957375873056065498d0)
      double precision rat12
      parameter (rat12=740.039247958473127809077672976d0)
      double precision rat13
      parameter (rat13=-6594.5249941972104408009959697d0)
      double precision rat14
      parameter (rat14=2104.19699943027157055137051339d0)
      double precision rat15
      parameter (rat15=-11952.6988753138781625203097635d0)
      double precision rat16
      parameter (rat16=4206.96587959739190985630182946d0)
      double precision rat17
      parameter (rat17=-16170.1277162330400286974320019d0)
      double precision rat18
      parameter (rat18=6117.8806102424510983098056593d0)
      double precision rat19
      parameter (rat19=-16549.7330843409086113397058513d0)
      double precision rat20
      parameter (rat20=6672.34031778185731468000253213d0)
      double precision rat21
      parameter (rat21=-12809.975041674579561520119854d0)
      double precision rat22
      parameter (rat22=5526.86712666962081407862252326d0)
      double precision rat23
      parameter (rat23=-7390.28262750311240530902492034d0)
      double precision rat24
      parameter (rat24=3482.55997974298917515984047604d0)
      double precision rat25
      parameter (rat25=-3078.61362283977970500727986327d0)
      double precision rat26
      parameter (rat26=1640.40767234284990821041969994d0)
      double precision rat27
      parameter (rat27=-872.635112152096389609841531092d0)
      double precision rat28
      parameter (rat28=-149.802612310354286678905277373d0)
      double precision rat29
      parameter (rat29=6424.35036075036075036075036075d0)
      double precision rat30
      parameter (rat30=9048.99018759018759018759018759d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(-13542d0*(x-y**2)**2*
     -     (-1d0-6d0*x-6d0*x**11-x**12 + 
     -       x**5*(rat6 + rat5*zeta2 - 
     -          (3580080*zeta3)/2257d0) + 
     -       x**7*(rat6 + rat5*zeta2 - 
     -          (3580080*zeta3)/2257d0) + 
     -       x**4*(rat4 + rat3*zeta2 - 
     -          (1888880*zeta3)/2257d0) + 
     -       x**8*(rat4 + rat3*zeta2 - 
     -          (1888880*zeta3)/2257d0) + 
     -       x**3*(rat2 - (3867360*zeta2)/2257d0 - 
     -          (1987720*zeta3)/6771d0) + 
     -       x**9*(rat2 - (3867360*zeta2)/2257d0 - 
     -          (1987720*zeta3)/6771d0) + 
     -       x**2*(rat1 - (773472*zeta2)/2257d0 - 
     -          (397544*zeta3)/6771d0) + 
     -       x**10*(rat1 - (773472*zeta2)/2257d0 - 
     -          (397544*zeta3)/6771d0) + 
     -       x**6*(rat9 + rat7*zeta2 + rat8*zeta3)
     -       ))/(7d0*x**8*y**8) + 
     -  (63188d0*(1d0+7d0*x + 
     -       x**14*(-11.6459074507817940115211749066d0 + 
     -          (190320*zeta2)/15797d0) + 
     -       x**2*(-100.598214850921060960941951003d0 + 
     -          (458400*zeta2)/15797d0) + 
     -       x**13*(rat28 + 
     -          (1859760*zeta2)/15797d0) + 
     -       x**3*(rat10 + 
     -          (2750400*zeta2)/15797d0) + 
     -       x**12*(rat27 + 
     -          (8692560*zeta2)/15797d0) + 
     -       x**4*(rat11 + rat12*zeta2) + 
     -       x**5*(rat13 + rat14*zeta2) + 
     -       x**6*(rat15 + rat16*zeta2) + 
     -       x**7*(rat17 + rat18*zeta2) + 
     -       x**8*(rat19 + rat20*zeta2) + 
     -       x**9*(rat21 + rat22*zeta2) + 
     -       x**10*(rat23 + rat24*zeta2) + 
     -       x**11*(rat25 + rat26*zeta2))*lins(1))
     -    /(15d0*x**8*y**7) + 
     -  (1848*(-1d0-7d0*x + (389d0*x**2)/11d0 + 
     -       (3335*x**3)/11d0 + 
     -       (876539*x**4)/693d0 + 
     -       (2341117*x**5)/693d0 + rat29*x**6 + 
     -       rat30*x**7 + (1589411*x**8)/165d0 + 
     -       (3851201*x**9)/495d0 + 
     -       (468967*x**10)/99d0 + 
     -       (2456599*x**11)/1155d0 + 
     -       (2324611*x**12)/3465d0 + 
     -       (154472*x**13)/1155d0 + 
     -       (44102*x**14)/3465d0)*lins(1)**2)/
     -   (x**8*y**7) + 
     -  (8*(-98491 + 192220*x*y - 
     -       148895*x**2*y**2 + 16690*x**3*y**3)*
     -     lins(1)*lins(2))/(15d0*x**7*y**7) + 
     -  (32*(231 + 35*x**2 - 40*x**3 + 45*x**4 - 
     -       56*x**5 + 105*x**6)*lins(1)**2*
     -     lins(2))/(x**7*y**7)
      return
      end


      ! ++++: 4*(-m2/s)^8*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function TwoLoop_HelAmppppp_HE_8_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-107.151171318165135774146591588d0)
      double precision rat2
      parameter (rat2=210.465040333073119958365860005d0)
      double precision rat3
      parameter (rat3=-551.90702790899081464487954953d0)
      double precision rat4
      parameter (rat4=1262.79024199843871975019516003d0)
      double precision rat5
      parameter (rat5=-2686.76942253868977550253339257d0)
      double precision rat6
      parameter (rat6=3722.58253317720530835284933646d0)
      double precision rat7
      parameter (rat7=-9111.22896336058493134438266589d0)
      double precision rat8
      parameter (rat8=7396.25468384074941451990632319d0)
      double precision rat9
      parameter (rat9=14008.445550351288056206088993d0)
      double precision rat10
      parameter (rat10=-19233.809266013116583262674486d0)
      double precision rat11
      parameter (rat11=-14763.8795862607338017174082748d0)
      double precision rat12
      parameter (rat12=11974.1150273224043715846994536d0)
      double precision rat13
      parameter (rat13=-26882.9012551260485979455534491d0)
      double precision rat14
      parameter (rat14=-19198.5150273224043715846994536d0)
      double precision rat15
      parameter (rat15=16582.1034803538901899557637263d0)
      double precision rat16
      parameter (rat16=-26360.8986795197912628491417384d0)
      double precision rat17
      parameter (rat17=-19200.1436377829820452771272443d0)
      double precision rat18
      parameter (rat18=20372.0199063231850117096018735d0)
      double precision rat19
      parameter (rat19=-18349.0238072182880438149758993d0)
      double precision rat20
      parameter (rat20=-14731.0708430913348946135831382d0)
      double precision rat21
      parameter (rat21=15624.9396565183450429352068696d0)
      double precision rat22
      parameter (rat22=-8652.34035769142204544234208559d0)
      double precision rat23
      parameter (rat23=-8576.92330210772833723653395785d0)
      double precision rat24
      parameter (rat24=10741.7933255269320843091334895d0)
      double precision rat25
      parameter (rat25=-2403.29780867734161059687991772d0)
      double precision rat26
      parameter (rat26=6068.46543715846994535519125683d0)
      double precision rat27
      parameter (rat27=-204.903963284873755209430463139d0)
      double precision rat28
      parameter (rat28=2608.80575722092115534738485558d0)
      double precision rat29
      parameter (rat29=75.3153814859732599779438186932d0)
      double precision rat30
      parameter (rat30=729.668982565703877179287015353d0)
      double precision rat31
      parameter (rat31=16.3293795607778196715410945365d0)
      double precision rat32
      parameter (rat32=0.0000476528289776494086150909774214d0)
      double precision rat33
      parameter (rat33=0.00615221766538803181724368407194d0)
      double precision rat34
      parameter (rat34=0.0104989703723409496221414284729d0)
      double precision rat35
      parameter (rat35=0.0325768985553620947194888254863d0)
      double precision rat36
      parameter (rat36=0.0878370937615683558753524169858d0)
      double precision rat37
      parameter (rat37=0.229917307925998180498490864723d0)
      double precision rat38
      parameter (rat38=0.148513979018127087452065529371d0)
      double precision rat39
      parameter (rat39=0.572143169151238673274675757606d0)
      double precision rat40
      parameter (rat40=0.177138076642708489239777111847d0)
      double precision rat41
      parameter (rat41=1.05311385021627107288187362675d0)
      double precision rat42
      parameter (rat42=0.162185607483446743297176331967d0)
      double precision rat43
      parameter (rat43=1.47252843053215492694077424059d0)
      double precision rat44
      parameter (rat44=0.125842415680973019902798311161d0)
      double precision rat45
      parameter (rat45=1.58742075479753940337428853878d0)
      double precision rat46
      parameter (rat46=0.0896005931599943009596877351304d0)
      double precision rat47
      parameter (rat47=1.31848202339077571852245967702d0)
      double precision rat48
      parameter (rat48=0.0580969212462172567795615690544d0)
      double precision rat49
      parameter (rat49=0.0309937009130923982194513167093d0)
      double precision rat50
      parameter (rat50=0.393399860088873124254874720424d0)
      double precision rat51
      parameter (rat51=0.0119486702265337245089543277596d0)
      double precision rat52
      parameter (rat52=0.132892803773002407390985913643d0)
      double precision rat53
      parameter (rat53=0.00285869911950035702412713635162d0)
      double precision rat54
      parameter (rat54=0.0296665714220028310655250444201d0)
      double precision rat55
      parameter (rat55=0.000314731852385680728522106745235d0)
      double precision rat56
      parameter (rat56=0.0041155022185438076189162470975d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-16*IPi*(27720 + 914800*y**3 + 
     -       1326850*y**4 + 548548*y**5 + 
     -       44520*y**9 + 89040*y**10 + 
     -       134390*y**11 + 89870*y**12 + 
     -       40529*y**13)*zeta2)/(5d0*x**7*y**7) + 
     -  (13664*(-1 - x**15/2d0 + 
     -       x**12*(rat27 + rat28*zeta2 - 
     -          (1905591*zeta3)/1708d0 - 
     -          (10095*zeta4)/61d0) + 
     -       x**13*(rat29 + rat30*zeta2 - 
     -          (362721*zeta3)/1708d0 - 
     -          (8145*zeta4)/61d0) + 
     -       x**14*(rat31 + 
     -          (7383721*zeta2)/76860d0 - 
     -          (98491*zeta3)/5124d0 - 
     -          (1365*zeta4)/61d0) + 
     -       x*(-7 - (54389*zeta3)/2562d0 + 
     -          (71655*zeta4)/1708d0) + 
     -       x**2*(rat1 + rat2*zeta2 - 
     -          (51223*zeta3)/244d0 + 
     -          (462285*zeta4)/854d0) + 
     -       x**11*(rat25 + rat26*zeta2 - 
     -          (2702813*zeta3)/732d0 + 
     -          (407145*zeta4)/427d0) + 
     -       x**3*(rat3 + rat4*zeta2 - 
     -          (2822219*zeta3)/2562d0 + 
     -          (4232895*zeta4)/1708d0) + 
     -       x**10*(rat22 + rat24*zeta2 + 
     -          rat23*zeta3 + (2150175*zeta4)/427d0
     -          ) + x**4*
     -        (rat5 + rat6*zeta2 - 
     -          (911871*zeta3)/244d0 + 
     -          (847335*zeta4)/122d0) + 
     -       x**9*(rat19 + rat21*zeta2 + 
     -          rat20*zeta3 + (762240*zeta4)/61d0) 
     -        + x**6*
     -        (rat10 + rat12*zeta2 + 
     -          rat11*zeta3 + (2577765*zeta4)/122d0
     -          ) + x**7*
     -        (rat13 + rat15*zeta2 + 
     -          rat14*zeta3 + (5853855*zeta4)/244d0
     -          ) + x**8*
     -        (rat16 + (5767774*zeta2)/315d0 + 
     -          rat17*zeta3 + rat18*zeta4) + 
     -       x**5*(rat7 + rat8*zeta2 - 
     -          (3698005*zeta3)/427d0 + rat9*zeta4)
     -       ))/(5d0*x**8*y**7) + 
     -  ((-288*IPi*(-281*y + 371*x**3*y**2 - 
     -          x**2*y*(718 + 371*y**2) + 
     -          x*(281 + 718*y**2))*zeta2)/
     -      (x**7*y**7) + 
     -     (81194400*
     -        (rat32 + 
     -          x**14*
     -           (rat55 + rat56*zeta2 + 
     -             (512*zeta3)/2.537325d6) + 
     -          x*(0.000333569802843545860305636841950d0 + 
     -             (1124*zeta3)/2.537325d6) + 
     -          x**13*
     -           (rat53 + rat54*zeta2 + 
     -             (256*zeta3)/120825d0) + 
     -          x**2*
     -           (rat33 + rat34*zeta2 + 
     -             (8123*zeta3)/2.537325d6) + 
     -          x**12*
     -           (rat51 + rat52*zeta2 + 
     -             (27136*zeta3)/2.537325d6) + 
     -          x**3*
     -           (rat35 + 
     -             (266393*zeta2)/4.228875d6 + 
     -             (28118*zeta3)/2.537325d6) + 
     -          x**4*
     -           (rat36 + rat37*zeta2 + 
     -             (11173*zeta3)/362475d0) + 
     -          x**11*
     -           (rat49 + rat50*zeta2 + 
     -             (87232*zeta3)/2.537325d6) + 
     -          x**5*
     -           (rat38 + rat39*zeta2 + 
     -             (34982*zeta3)/507465d0) + 
     -          x**10*
     -           (rat48 + 
     -             (6358817*zeta2)/7.611975d6 + 
     -             (5668*zeta3)/72495d0) + 
     -          x**6*
     -           (rat40 + rat41*zeta2 + 
     -             (44051*zeta3)/362475d0) + 
     -          x**9*
     -           (rat46 + rat47*zeta2 + 
     -             (47794*zeta3)/362475d0) + 
     -          x**7*
     -           (rat42 + rat43*zeta2 + 
     -             (414814*zeta3)/2.537325d6) + 
     -          x**8*
     -           (rat44 + rat45*zeta2 + 
     -             (61007*zeta3)/362475d0)))/
     -      (x**8*y**7))*lins(1) - 
     -  (2*(-1874990 - 2961470*y - 437100*y**2 + 
     -       3412860*y**3 + 5718220*y**4 + 
     -       4599534*y**5 + 1832519*y**6 + 
     -       138495*y**7 + 1832519*y**8 + 
     -       4599534*y**9 + 5718220*y**10 + 
     -       3412860*y**11 - 437100*y**12 - 
     -       2961470*y**13 - 1874990*y**14 + 
     -       60*IPi*(40529 + 130399*y + 
     -          224260*y**2 + 223430*y**3 + 
     -          133560*y**4 + 44520*y**5 + 
     -          44520*x*y**9 + 89040*x*y**10 + 
     -          134390*x*y**11 + 89870*x*y**12 + 
     -          40529*x*y**13) + 
     -       5400*x*(1405 + 6400*y + 12625*y**2 + 
     -          14600*y**3 + 9275*y**4 + 
     -          3710*y**5 + 742*(-1 + x)*y**8 + 
     -          1484*(-1 + x)*y**9 + 
     -          2178*(-1 + x)*y**10 + 
     -          1436*(-1 + x)*y**11 + 
     -          562*(-1 + x)*y**12)*zeta2)*
     -     lins(1)**2)/(225d0*x**8*y**7) - 
     -  (8*(-40529 - 130399*y - 224260*y**2 - 
     -       223430*y**3 - 133560*y**4 - 
     -       44520*y**5 + 3465*y**7 + 
     -       44520*x*y**9 + 89040*x*y**10 + 
     -       134390*x*y**11 + 89870*x*y**12 + 
     -       40529*x*y**13)*lins(1)**3)/
     -   (45d0*x**8*y**7) - 
     -  (4*(281 + 1280*y + 2525*y**2 + 
     -       2920*y**3 + 1855*y**4 + 742*y**5 + 
     -       742*(-1 + x)*y**8 + 
     -       1484*(-1 + x)*y**9 + 
     -       2178*(-1 + x)*y**10 + 
     -       1436*(-1 + x)*y**11 + 
     -       562*(-1 + x)*y**12)*lins(1)**4)/
     -   (3d0*x**7*y**7) + 
     -  ((8*IPi*(68249*y - 274274*x**3*y**2 + 
     -          x*(-68249 + 46628*y**2) + 
     -          x**2*(-46628*y + 274274*y**3)))/
     -      (15d0*x**7*y**7) + 
     -     (23776*(-0.757638701959024973829819051892d0 + 
     -          x**6*
     -           (0.216147001644982802452519814566d0 + 
     -             (105*zeta2)/743d0) + zeta2 + 
     -          x*(0.529931209810079258262299985046d0 + 
     -             (1280*zeta2)/743d0) + 
     -          x**2*
     -           (2.45830342455510692388215941379d0 + 
     -             (2595*zeta2)/743d0) + 
     -          x**5*
     -           (-3.60627037535516674143861223269d0 + 
     -             (3528*zeta2)/743d0) + 
     -          x**3*
     -           (-4.12723194257514580529385374607d0 + 
     -             (7640*zeta2)/743d0) + 
     -          x**4*
     -           (-9.02415133841782563182294003290d0 + 
     -             (9005*zeta2)/743d0)))/
     -      (x**7*y**7))*lins(1)*lins(2) + 
     -  (8*(44102 + 109910*y + 203575*y**2 + 
     -       228640*y**3 + 150890*y**4 + 
     -       54502*y**5 + 8345*y**6 + 
     -       44520*y**9 + 89040*y**10 + 
     -       134390*y**11 + 89870*y**12 + 
     -       40529*y**13 + 
     -       120*IPi*
     -        (-281*y + 371*x**3*y**2 - 
     -          x**2*y*(718 + 371*y**2) + 
     -          x*(281 + 718*y**2)))*lins(1)**2*
     -     lins(2))/(15d0*x**7*y**7) - 
     -  (16*(x - y)*(281 - 718*x*y + 
     -       371*x**2*y**2)*lins(1)**3*lins(2))/
     -   (3d0*x**7*y**7) + 
     -  (8*(743 - 949*x + 3213*x**2 + 105*x**3)*
     -     lins(1)**2*lins(2)**2)/(x**7*y**4) - 
     -  (96*(281 + 1280*y + 2525*y**2 + 
     -       2920*y**3 + 1855*y**4 + 742*y**5 + 
     -       742*(-1 + x)*y**8 + 
     -       1484*(-1 + x)*y**9 + 
     -       2178*(-1 + x)*y**10 + 
     -       1436*(-1 + x)*y**11 + 
     -       562*(-1 + x)*y**12)*zeta2*lins(3))/
     -   (x**7*y**7) + 
     -  (16*(-40529 - 89870*y - 134390*y**2 - 
     -       89040*y**3 - 44520*y**4 + 
     -       44520*y**9 + 89040*y**10 + 
     -       134390*y**11 + 89870*y**12 + 
     -       40529*y**13)*lins(1)*lins(3))/
     -   (15d0*x**7*y**7) + 
     -  (32*(-281 - 1686*x - 4961*x**2 - 
     -       9350*x**3 - 11675*x**4 - 9146*x**5 - 
     -       1492*x**6 + 6811*x**7 + 11207*x**8 + 
     -       9819*x**9 + 5461*x**10 + 
     -       1811*x**11 + 281*x**12)*(-1 + y)*
     -     lins(1)**2*lins(3))/(x**7*y**7) - 
     -  (32*(x - y)*(281 - 718*x*y + 
     -       371*x**2*y**2)*lins(1)*lins(2)*
     -     lins(3))/(x**7*y**7) - 
     -  (16*(-40529 - 89870*y - 134390*y**2 - 
     -       89040*y**3 - 44520*y**4 + 
     -       44520*y**9 + 89040*y**10 + 
     -       134390*y**11 + 89870*y**12 + 
     -       40529*y**13)*lins(4))/(15d0*x**7*y**7)
     -    - (128*(-281 - 1686*x - 4961*x**2 - 
     -       9350*x**3 - 11675*x**4 - 9146*x**5 - 
     -       1492*x**6 + 6811*x**7 + 11207*x**8 + 
     -       9819*x**9 + 5461*x**10 + 
     -       1811*x**11 + 281*x**12)*(-1 + y)*
     -     lins(1)*lins(4))/(x**7*y**7) - 
     -  (128*(x - y)*
     -     (281 - 718*x*y + 371*x**2*y**2)*
     -     lins(1)*lins(5))/(x**7*y**7) - 
     -  (192*(-1 + x)*
     -     (281 + 406*x + 621*x**2 + 406*x**3 + 
     -       281*x**4)*y*lins(6))/x**7 - 
     -  (96*(x - y)*(281 - 718*x*y + 
     -       371*x**2*y**2)*lins(7))/(x**7*y**7)
      return
      end


      ! -+++: (-m2/s)^0*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_0_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-3*(2 + 6*x + 9*x**2 + 8*x**3 + 9*x**4 + 
     -       6*x**5 + 2*x**6)*zeta2)/(x**2*y**2) 
     -   + (4*(1 - x*y)*lins(1))/y - 
     -  (2*(x + 4*x**2 + 3*x**3 + x**4 - y)*
     -     lins(1)**2)/y**2 + 
     -  (1 - 2*x*y)*lins(1)*lins(2)
      return
      end


      ! -+++: (-m2/s)^1*(log(-m2/s))^4
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_1_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(1 - x*y)/(6d0*x*y)
      return
      end


      ! -+++: (-m2/s)^1*(log(-m2/s))^3
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_1_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(-2*(1 - x*y))/(x*y) + (4*lins(1))/(3d0*x)
      return
      end


      ! -+++: (-m2/s)^1*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_1_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*(1 - x*y))/(x*y) + 
     -  (8*(1 - x*y)*zeta2)/(x*y) - 
     -  ((x**2 + 14*y)*lins(1))/(x*y) + 
     -  (x*lins(1)**2)/y - 
     -  ((-1 - 2*x*y)*lins(1)*lins(2))/(x*y)
      return
      end


      ! -+++: (-m2/s)^1*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_1_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (4*(1 - x*y))/(x*y) + 
     -  (18*IPi*(-1 - 2*x*y)*zeta2)/(x*y) - 
     -  (24*(1 - x*y)*zeta2)/(x*y) + 
     -  (2*(17 - 14*x*y)*zeta3)/(x*y) - 
     -  (4*x*lins(1))/y - 
     -  (32*(x**2 + y)*zeta2*lins(1))/(x*y) + 
     -  (6*IPi*(2 + x)*lins(1)**2)/y - 
     -  (2*(5*x**2 - 8*y)*lins(1)**2)/(x*y) - 
     -  (2*(-2 + x)*(2 + 3*x)*lins(1)**3)/
     -   (3d0*x*y) + ((1 - 10*x*y)*lins(1)*
     -     lins(2))/(x*y) - 
     -  (2*(2 + 6*x + x**2)*lins(1)**2*lins(2))/
     -   (x*y) - (12*(2 + x)*lins(1)*lins(3))/y + 
     -  (12*(2 + x)*lins(4))/y
      return
      end


      ! -+++: (-m2/s)^1*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_1_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-4*(1 - x*y))/(x*y) - 
     -  (72*IPi*(-1 - 2*x*y)*zeta2)/(x*y) - 
     -  (2*(1 - x*y)*zeta2)/(x*y) - 
     -  (8*(4 - x*y)*zeta3)/(x*y) - 
     -  ((-373 - 122*x*y)*zeta4)/(x*y) - 
     -  (4*x*lins(1))/y - 
     -  (2*IPi*(-1 + 9*x)*zeta2*lins(1))/x - 
     -  (2*IPi*(-58 - 10*x + 21*x**2)*zeta2*
     -     lins(1))/(x*y) + 
     -  (6*(17*x**2 + 26*y)*zeta2*lins(1))/
     -   (x*y) + (16*(4 + x)*zeta3*lins(1))/x - 
     -  (24*IPi*(2 + x)*lins(1)**2)/y + 
     -  (4*(x**2 + y)*lins(1)**2)/(x*y) - 
     -  (10*(7 + 14*x + 4*x**2)*zeta2*lins(1)**2)/
     -   (x*y) - (16*IPi*(2 + x)*lins(1)**3)/
     -   (3d0*y) + (3*(3*x**2 - 2*y)*lins(1)**3)/
     -   (x*y) - ((15 + 20*x + 2*x**2)*
     -     lins(1)**4)/(6d0*x*y) - 
     -  (2*(-1 - 2*x*y)*lins(1)*lins(2))/(x*y) + 
     -  (26*(-1 - 6*x*y)*zeta2*lins(1)*lins(2))/
     -   (x*y) + ((-1 + 30*x + 7*x**2)*lins(1)**2*
     -     lins(2))/(x*y) + 
     -  (2*IPi*(-4 + 6*x + 17*x**2)*lins(1)**2*
     -     lins(2))/(x*y) + 
     -  (2*(8 + 30*x + 11*x**2)*lins(1)**3*
     -     lins(2))/(3d0*x*y) - 
     -  ((3 - 22*x*y)*lins(1)**2*lins(2)**2)/
     -   (x*y) - (6*(-1 - 2*x*y)*zeta2*lins(3))/
     -   (x*y) + (48*(2 + x)*lins(1)*lins(3))/y + 
     -  (4*(2 + x)*lins(1)**2*lins(3))/y - 
     -  (2*(5 + 18*x + 20*x**2)*lins(1)*lins(2)*
     -     lins(3))/(x*y) - 
     -  (48*(2 + x)*lins(4))/y + 
     -  (16*(2 + x)*lins(1)*lins(4))/y + 
     -  (4*(10 + 14*x + 3*x**2)*lins(1)*lins(5))/
     -   (x*y) + (40*(-1 + x)*lins(6))/x + 
     -  (20*(x - y)*lins(7))/(x*y)
      return
      end


      ! -+++: (-m2/s)^2*(log(-m2/s))^4
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_2_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(122 + 432*x + 288*x**2 + 22*x**3)/(6d0*x*y**2)
      return
      end


      ! -+++: (-m2/s)^2*(log(-m2/s))^3
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_2_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-35 + 16*x + 366*x**2 + 244*x**3 + 
     -     36*x**4)/(3d0*x**2*y**2) - 
     -  (4*(3 - 5*y - 5*y**3 + 3*y**4)*lins(1))/
     -   (3d0*x**2*y**2)
      return
      end


      ! -+++: (-m2/s)^2*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_2_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-3*(150 + 451*y + 687*y**2 + 458*y**3 + 
     -        78*y**4) + 
     -     24*x*(10 - 9*y - 33*y**2 + 5*y**3)*
     -      zeta2)/(6d0*x**2*y**2) - 
     -  (2*(34 + 68*x + 50*x**2 + 16*x**3 + x**4)*
     -     lins(1))/(x**2*y**2) - 
     -  (10*(1 + y**2)*lins(1)**2)/(x**2*y) - 
     -  (4*(3 + 8*y + 16*x*y)*lins(1)*lins(2))/
     -   (x*y**2)
      return
      end


      ! -+++: (-m2/s)^2*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_2_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (6*(21 + 52*x + 72*x**2 + 48*x**3 + 
     -        14*x**4)*zeta2 - 
     -     6*x*(34 + 8*zeta3 + 
     -        5*x**3*(-5 + 8*zeta3) + 
     -        9*x*(3 + 16*zeta3) + 
     -        6*x**2*(3 + 16*zeta3)))/
     -   (3d0*x**2*y**2) - 
     -  (2*(12 + 48*y**2 + 12*y**4 + 
     -       5*y*(33 + 12*zeta2) + 
     -       5*y**3*(33 + 12*zeta2))*lins(1))/
     -   (3d0*x**2*y**2) + 
     -  (4*(14 + 28*x + 15*x**2 + x**3 - x**4)*
     -     lins(1)**2)/(x**2*y**2) + 
     -  (8*(1 + y**2)*lins(1)**3)/(3d0*x**2*y) - 
     -  (2*(3 + 2*y)*(-5 + y + 15*y**2)*lins(1)*
     -     lins(2))/(x**2*y**2) + 
     -  (12*lins(1)**2*lins(2))/(x**2*y)
      return
      end


      ! -+++: (-m2/s)^2*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_2_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (180*x*(107 + 291*x + 194*x**2 - 10*x**3)*
     -      zeta2 - 1080*IPi*(-9 - 10*x + 4*x**4)*
     -      zeta2 + 90*
     -      (30*x**4 + 56*zeta3 + 
     -        x*(-57 + 354*zeta3) + 
     -        x**3*(-54 + 452*zeta3) + 
     -        x**2*(-81 + 678*zeta3)) + 
     -     90*x*(267 + 648*x + 398*x**2 - 
     -        68*x**3)*zeta4)/(90d0*x**2*y**2) - 
     -  (2*(6*IPi*(7 + 14*y + 2*(-1 + x)*y**3)*
     -        zeta2 + 
     -       6*(4 - 20*y - 42*y**2 - 20*y**3 + 
     -          4*y**4)*zeta2 + 
     -       3*(17 - 72*y**2 + 17*y**4 - 
     -          4*zeta3 + y*(-27 + 32*zeta3) + 
     -          y**3*(-27 + 40*zeta3)))*lins(1))/
     -   (3d0*x**2*y**2) + 
     -  ((-6*y*(16 + 79*y + 16*y**2) - 
     -       24*IPi*(-1 + y**4) + 
     -       6*(9 + 26*y + 12*y**3 + 2*y**4)*zeta2
     -       )*lins(1)**2)/(3d0*x**2*y**2) + 
     -  (4*(-2 - IPi*x**3*(-1 + y) - 7*y - 
     -       19*y**2 - 7*y**3 - 2*y**4)*lins(1)**3
     -     )/(3d0*x**2*y**2) + 
     -  ((6 + 12*y - 4*(-1 + x)*y**3)*lins(1)**4)/
     -   (12d0*x**2*y**2) + 
     -  ((33 + 456*y + 720*y**2 + 306*y**3 + 
     -       6*IPi*(5*y + 7*x**2*y - 
     -          x*(5 + 7*y**2)) + 
     -       12*(-99 - 189*y + 24*y**2 + 
     -          68*y**3 + 26*y**4)*zeta2)*lins(1)*
     -     lins(2))/(3d0*x**2*y**2) + 
     -  ((8 + 64*y + 52*y**2 - 16*(-1 + x)*y**3 - 
     -       8*IPi*(3 + 6*y + 8*(-1 + x)*y**3))*
     -     lins(1)**2*lins(2))/(2d0*x**2*y**2) + 
     -  ((14 + 20*x + 4*x**3*(-1 + y))*lins(1)**3*
     -     lins(2))/(3d0*x**2*y**2) + 
     -  ((7 + 24*x + 16*x**2*y)*lins(1)**2*
     -     lins(2)**2)/(x*y**2) + 
     -  (4*(1 + 2*x*y*(-1 + y + y**2))*zeta2*
     -     lins(3))/(x**2*y**2) + 
     -  (16*(-1 + y**4)*lins(1)*lins(3))/
     -   (x**2*y**2) + 
     -  (4*x*(-1 + y)*lins(1)**2*lins(3))/y**2 + 
     -  (4*(5 + 10*x + 8*x**3*(-1 + y))*lins(1)*
     -     lins(2)*lins(3))/(x**2*y**2) - 
     -  (16*(-1 + y**4)*lins(4))/(x**2*y**2) - 
     -  (8*x*(-1 + y)*lins(1)*lins(4))/y**2 + 
     -  (8*(x - y)*lins(1)*lins(5))/(x**2*y**2) + 
     -  (8*(-1 + x)*y*lins(6))/x**2 + 
     -  ((4 + 8*x)*lins(7))/(x**2*y**2)
      return
      end


      ! -+++: (-m2/s)^3*(log(-m2/s))^4
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_3_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (-30 + 435*x + 1246*x**2 + 1452*x**3 + 
     -    588*x**4 + 83*x**5)/(6d0*x**2*y**3)
      return
      end


      ! -+++: (-m2/s)^3*(log(-m2/s))^3
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_3_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (2*(-70 - 296*x + 379*x**2 + 1888*x**3 + 
     -       2384*x**4 + 926*x**5 + 105*x**6))/
     -   (3d0*x**3*y**3) + 
     -  ((-36 + 84*y - 6*y**2 + 8*y**3 - 6*y**4 + 
     -       84*y**5 - 36*y**6)*lins(1))/
     -   (3d0*x**3*y**3)
      return
      end


      ! -+++: (-m2/s)^3*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_3_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (3*(198 + 726*y + 477*y**2 - 2056*y**3 - 
     -        3693*y**4 - 1530*y**5 + 40*y**6) + 
     -     24*x*(26 + 22*y - 111*y**2 - 
     -        233*y**3 - 143*y**4 + 13*y**5)*zeta2
     -     )/(12d0*x**3*y**3) + 
     -  ((-54 + 412*y + 116*y**2 + 32*y**3 + 
     -       116*y**4 + 412*y**5 - 54*y**6)*
     -     lins(1))/(2d0*x**3*y**3) + 
     -  ((-42 + 19*y + 28*y**2 + 19*y**3 - 
     -       42*y**4)*lins(1)**2)/(x**3*y**2) + 
     -  ((100 + 56*x - 8*x**2)*lins(1)*lins(2))/
     -   (x*y**3)
      return
      end


      ! -+++: (-m2/s)^3*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_3_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-1296*IPi*x*
     -      (4 + 2*x - 2*x**3 - 2*x**4 + x**5)*
     -      zeta2 + 24*
     -      (84 + 114*x - 589*x**2 - 1644*x**3 - 
     -        1736*x**4 - 638*x**5 + 11*x**6)*
     -      zeta2 - x*
     -      (1228 - 2784*zeta3 + 
     -        4*x**5*(19 + 672*zeta3) + 
     -        4*x**2*(-3049 + 2880*zeta3) + 
     -        x*(4183 + 3216*zeta3) + 
     -        2*x**4*(-7673 + 3648*zeta3) + 
     -        x**3*(-30421 + 15936*zeta3)))/
     -   (12d0*x**3*y**3) + 
     -  (2*(-54 + 6*y*(-9 + 210*zeta2) + 
     -       6*y**5*(9*x + 210*zeta2) + 
     -       4*y**3*(-9 + 1236*zeta2) + 
     -       y**2*(6 + 3810*zeta2) + 
     -       y**4*(6 + 3810*zeta2))*lins(1))/
     -   (3d0*x**3*y**3) + 
     -  (2*(-3 - 54*y + 44*y**2 + 40*y**3 + 
     -       44*y**4 - 54*y**5 - 3*y**6 - 
     -       9*IPi*x*(x - y**2)*
     -        (-1 + 4*y - 4*y**2 + y**3))*
     -     lins(1)**2)/(x**3*y**3) - 
     -  (2*(58 + 174*x + 367*x**2 + 444*x**3 + 
     -       269*x**4 + 76*x**5 + 9*x**6)*
     -     lins(1)**3)/(3d0*x**3*y**3) + 
     -  (2*(69 + 9*IPi*(x - y) + y - 226*y**2 - 
     -       134*y**3 - 12*y**4)*lins(1)*lins(2))/
     -   (x**3*y**3) - 
     -  (2*(-18 + 70*x + 144*x**2 + 148*x**3 + 
     -       63*x**4 + 9*x**5)*lins(1)**2*lins(2))
     -    /(x**3*y**2) - 
     -  (36*(5 + 5*x + x**2)*(x**2 - y)*(-1 + y)*
     -     lins(1)*lins(3))/(x**2*y**3) + 
     -  (36*(5 + 5*x + x**2)*(x**2 - y)*(-1 + y)*
     -     lins(4))/(x**2*y**3)
      return
      end


      ! -+++: (-m2/s)^3*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_3_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (180*x*(182 + 789*y + 79*y**2 - 
     -        761*y**3 + 283*y**4 + 91*y**5)*zeta2
     -       - 540*IPi*
     -      (60 + 12*y - 617*y**2 + 292*y**3 + 
     -        1055*y**4 + 422*y**5 + 84*y**6)*
     -      zeta2 + 11520*IPi*x*(x - y)*y*zeta3 - 
     -     5*(x*(986 - 4158*y + 45335*y**2 + 
     -           98839*y**3 + 42723*y**4 + 
     -           493*y**5) + 
     -        72*(394 + 1355*y + 2643*y**2 + 
     -           2850*y**3 + 1416*y**4 + 
     -           480*y**5 + 120*y**6)*zeta3) + 
     -     90*x*(4107 - 1103*y + 5684*y**2 + 
     -        7392*y**3 + 4068*y**4 + 564*y**5)*
     -      zeta4)/(180d0*x**3*y**3) + 
     -  ((-473 - 473*y**6 + 
     -       6*(-24 + 1088*y + 3530*y**2 + 
     -          4832*y**3 + 3530*y**4 + 
     -          1088*y**5 - 24*y**6)*zeta2 + 
     -       36*IPi*(-55 + 94*y + 192*y**2 + 
     -          72*y**3 + 6*y**4 + 16*y**5 + 
     -          2*y**6)*zeta2 + 72*zeta3 - 
     -       22*y*(97 + 48*zeta3) + 
     -       24*y**3*(29 + 80*zeta3) - 
     -       2*y**5*(1067 + 384*zeta3) + 
     -       y**2*(-802 + 744*zeta3) + 
     -       y**4*(-802 + 2400*zeta3))*lins(1))/
     -   (6d0*x**3*y**3) + 
     -  ((-252*IPi*(-1 + y**6) - 
     -       6*(4 + 105*y + 199*y**2 + 136*y**3 + 
     -          199*y**4 + 105*y**5 + 4*y**6) + 
     -       6*(87 - 278*y - 1044*y**2 - 
     -          760*y**3 - 456*y**4 - 236*y**5 + 
     -          150*y**6)*zeta2)*lins(1)**2)/
     -   (6d0*x**3*y**3) + 
     -  ((12*IPi*(-9 + 8*y + 13*y**2 - 13*y**4 - 
     -          8*y**5 + 9*y**6) - 
     -       2*(42 + 26*y + 155*y**2 + 120*y**3 + 
     -          155*y**4 + 26*y**5 + 42*y**6))*
     -     lins(1)**3)/(6d0*x**3*y**3) + 
     -  ((53 + 258*x + 1058*x**2 + 1584*x**3 + 
     -       1178*x**4 + 456*x**5 + 54*x**6)*
     -     lins(1)**4)/(12d0*x**3*y**3) + 
     -  ((3*(-59 - 122*x - 166*x**2 - 176*x**3 + 
     -          74*x**4) + 
     -       36*IPi*(5*y - 51*x**2*y + 
     -          x*(-5 + 51*y**2)) + 
     -       24*(324 + 1032*x + 1613*x**2 + 
     -          1718*x**3 + 1011*x**4 + 
     -          660*x**5 + 45*x**6)*zeta2)*
     -     lins(1)*lins(2))/(6d0*x**3*y**3) + 
     -  ((6 + 134*y + 55*y**2 + 20*y**3 + 
     -       12*y**4 + 42*y**6 + 
     -       6*IPi*(-15 - 92*y - 211*y**2 - 
     -          208*y**3 - 84*y**4 + 16*y**5 + 
     -          12*y**6))*lins(1)**2*lins(2))/
     -   (x**3*y**3) + 
     -  ((27 + 342*x*y + 822*x**2*y + 
     -       818*x**3*y + 438*x**4*y + 54*x**5*y)*
     -     lins(1)**3*lins(2))/(3d0*x**3*y**3) - 
     -  ((18 - 29*x - 20*x**2 + 132*x**3 + 
     -       360*x**4 + 60*x**5)*lins(1)**2*
     -     lins(2)**2)/(2d0*x**2*y**3) + 
     -  (6*(1 + 2*x*y*
     -        (5 + 13*y - 25*y**2 - 11*y**3 + 
     -          y**4))*zeta2*lins(3))/(x**3*y**3) 
     -   + (84*(-1 + y**6)*lins(1)*lins(3))/
     -   (x**3*y**3) + 
     -  (6*(9 + 18*x + 35*x**2 + 26*x**3 + 
     -       5*x**4)*(-1 + y)*lins(1)**2*lins(3))/
     -   (x**2*y**3) - 
     -  (12*(-11 - 40*x - 84*x**2 + 
     -       46*x**3*(-1 + y) + 4*x**4*(-1 + y)*y)
     -      *lins(1)*lins(2)*lins(3))/(x**3*y**3) 
     -   - (84*(-1 + y**6)*lins(4))/(x**3*y**3) - 
     -  (12*(-12 - 24*x - 4*x**2 + 8*x**3 + x**4)*
     -     (-1 + y)*lins(1)*lins(4))/(x**2*y**3) 
     -   - (12*(6 + 30*x + 109*x**2 + 144*x**3 + 
     -       92*x**4 + 32*x**5 + 4*x**6)*lins(1)*
     -     lins(5))/(x**3*y**3) - 
     -  (12*(-1 + x)*
     -     (20 + 40*y + 56*y**2 + 36*y**3 + 
     -       3*y**4)*lins(6))/(x**3*y**2) - 
     -  (6*(x - y)*(3 - 36*x*y + 20*x**2*y**2)*
     -     lins(7))/(x**3*y**3)
      return
      end


      ! -+++: (-m2/s)^4*(log(-m2/s))^4
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_4_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (2*(-50 - 169*x - 655*x**2 + 126*x**3 + 
     -      1904*x**4 + 1879*x**5 + 555*x**6 + 
     -      82*x**7))/(3d0*x**3*y**4)
      return
      end


      ! -+++: (-m2/s)^4*(log(-m2/s))^3
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_4_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (4*(-420 - 1248*x - 1411*x**2 - 
     -       6954*x**3 + 1758*x**4 + 22763*x**5 + 
     -       21556*x**6 + 5799*x**7 + 703*x**8))/
     -   (9d0*x**4*y**4) - 
     -  (8*(134 + 536*x + 1459*x**2 + 2501*x**3 + 
     -       2614*x**4 + 1685*x**5 + 663*x**6 + 
     -       148*x**7 + 15*x**8)*lins(1))/
     -   (3d0*x**4*y**4)
      return
      end


      ! -+++: (-m2/s)^4*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_4_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (21768 + 53544*y + 283923*y**2 + 
     -     881360*y**3 + 1003451*y**4 + 
     -     354430*y**5 - 69332*y**6 - 
     -     35796*y**7 + 8040*y**8 + 
     -     144*x*(8 - 36*y - 299*y**2 - 
     -        575*y**3 - 1166*y**4 - 1160*y**5 - 
     -        370*y**6 + 4*y**7)*zeta2)/
     -   (36d0*x**4*y**4) - 
     -  (2*(2468 + 9872*x + 29581*x**2 + 
     -       54191*x**3 + 59194*x**4 + 
     -       39587*x**5 + 15368*x**6 + 
     -       2961*x**7 + 172*x**8)*lins(1))/
     -   (3d0*x**4*y**4) - 
     -  (4*(104 + 312*x + 796*x**2 + 1072*x**3 + 
     -       756*x**4 + 272*x**5 + 37*x**6)*
     -     lins(1)**2)/(x**4*y**3) - 
     -  (24*(16 + 101*x + 132*x**2 + 52*x**3)*
     -     lins(1)*lins(2))/(x**2*y**4)
      return
      end


      ! -+++: (-m2/s)^4*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_4_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-19440*IPi*x*
     -      (4 + 2*x - 2*x**5 - 2*x**6 + x**7)*
     -      zeta2 + 36*
     -      (504 + 180*x - 693*x**2 + 4842*x**3 + 
     -        111*x**4 - 16652*x**5 - 
     -        15690*x**6 - 3764*x**7 + 130*x**8)*
     -      zeta2 - x*
     -      (x**2*(390434 - 73440*zeta3) + 
     -        x*(102935 - 29808*zeta3) - 
     -        108*(129 + 280*zeta3) + 
     -        8*x**7*(-883 + 2916*zeta3) + 
     -        x**3*(376161 + 9504*zeta3) + 
     -        2*x**6*(-55547 + 34128*zeta3) + 
     -        2*x**4*(3821 + 88992*zeta3) + 
     -        2*x**5*(-106555 + 96336*zeta3)))/
     -   (27d0*x**4*y**4) + 
     -  (2*(-794 - 794*y**8 + 
     -       8*y*(454 + 4374*zeta2) + 
     -       8*y**7*(454 + 4374*zeta2) + 
     -       8*y**4*(-137 + 43902*zeta2) + 
     -       y**2*(3925 + 136944*zeta2) + 
     -       y**6*(3925 + 136944*zeta2) + 
     -       y**3*(167 + 277416*zeta2) + 
     -       y**5*(167 + 277416*zeta2))*lins(1))/
     -   (9d0*x**4*y**4) + 
     -  (4*(-10 - 362*y + 303*y**2 + 324*y**3 + 
     -       380*y**4 + 324*y**5 + 303*y**6 - 
     -       362*y**7 - 10*y**8 - 
     -       90*IPi*(-1 + 2*y + 2*y**2 - 2*y**6 - 
     -          2*y**7 + y**8))*lins(1)**2)/
     -   (3d0*x**4*y**4) - 
     -  (8*(74 + 296*x + 998*x**2 + 1958*x**3 + 
     -       2250*x**4 + 1582*x**5 + 666*x**6 + 
     -       152*x**7 + 15*x**8)*lins(1)**3)/
     -   (3d0*x**4*y**4) + 
     -  (4*(136 + 30*IPi*(x - y) - 173*y - 
     -       288*y**2 + 1893*y**3 + 3032*y**4 + 
     -       1232*y**5)*lins(1)*lins(2))/
     -   (x**4*y**4) - 
     -  (8*(-15 + 151*x + 480*x**2 + 825*x**3 + 
     -       821*x**4 + 465*x**5 + 135*x**6 + 
     -       15*x**7)*lins(1)**2*lins(2))/
     -   (x**4*y**3) + 
     -  (240*(-1 + 2*y + 2*y**2 - 2*y**6 - 
     -       2*y**7 + y**8)*lins(1)*lins(3))/
     -   (x**4*y**4) - 
     -  (240*(-1 + 2*y + 2*y**2 - 2*y**6 - 
     -       2*y**7 + y**8)*lins(4))/(x**4*y**4)
      return
      end


      ! -+++: (-m2/s)^4*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_4_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(240*x*(2322 - 5082*x - 42661*x**2 - 
     -         68487*x**3 - 55343*x**4 - 
     -         19155*x**5 + 808*x**6 + 2150*x**7)*
     -       zeta2 + 
     -      4320*IPi*
     -       (-432 + 876*x + 3040*x**2 + 
     -         2110*x**3 + 1490*x**4 + 
     -         2712*x**5 + 1809*x**6 + 421*x**7 + 
     -         322*x**8)*zeta2 + 
     -      172800*IPi*x**2*(x - y)*y**2*zeta3 + 
     -      5*(-193536*zeta3 - 
     -         432*x*(-483 + 1892*zeta3) + 
     -         20*x**8*(2561 + 11808*zeta3) - 
     -         15*x**4*(125977 + 22560*zeta3) - 
     -         9*x**2*(33293 + 27520*zeta3) + 
     -         4*x**7*(233327 + 74016*zeta3) + 
     -         6*x**6*(348863 + 367296*zeta3) - 
     -         2*x**3*(1078627 + 547920*zeta3) + 
     -         4*x**5*(216989 + 629712*zeta3)) + 
     -      2160*x*(-11610 - 5398*x + 1231*x**2 + 
     -         1193*x**3 - 86*x**4 - 898*x**5 - 
     -         554*x**6 + 696*x**7)*zeta4)/
     -   (1080d0*x**4*y**4) + 
     -  ((-6599 - 6599*y**8 + 
     -       648*IPi*
     -        (-80 + 141*y + 239*y**2 + 
     -          186*y**3 + 180*y**4 + 72*y**5 - 
     -          10*x*y**6 + 10*y**7)*zeta2 - 
     -       72*(20 - 3152*y - 13471*y**2 - 
     -          28278*y**3 - 35868*y**4 - 
     -          28278*y**5 - 13471*y**6 - 
     -          3152*y**7 + 20*y**8)*zeta2 - 
     -       64*y**7*(647 + 162*zeta3) - 
     -       64*y*(647 + 189*zeta3) + 
     -       108*y**2*(-323 + 428*zeta3) + 
     -       108*y**6*(-323 + 672*zeta3) + 
     -       24*y**4*(271 + 1584*zeta3) + 
     -       6*y**3*(607 + 7056*zeta3) + 
     -       6*y**5*(607 + 12528*zeta3))*lins(1))/
     -   (27d0*x**4*y**4) + 
     -  ((628 + 2512*x + 17143*x**2 + 
     -       42637*x**3 + 57409*x**4 + 
     -       46687*x**5 + 20214*x**6 + 
     -       3558*x**7 - 108*x**8 + 
     -       6*IPi*x*
     -        (1307 + 3921*x + 7486*x**2 + 
     -          8437*x**3 + 5516*x**4 + 
     -          1951*x**5 + 322*x**6)*(-1 + y) + 
     -       36*(132 + 1515*x + 7815*x**2 + 
     -          18020*x**3 + 23992*x**4 + 
     -          19374*x**5 + 9300*x**6 + 
     -          2470*x**7 + 240*x**8)*zeta2)*
     -     lins(1)**2)/(9d0*x**4*y**4) + 
     -  ((240*IPi*(-8 + 10*y + 13*y**2 - 
     -          13*y**6 - 10*y**7 + 8*y**8) - 
     -       4*(322 + 201*y + 1197*y**2 + 
     -          1316*y**3 + 1656*y**4 + 
     -          1316*y**5 + 1197*y**6 + 
     -          201*y**7 + 322*y**8))*lins(1)**3)/
     -   (18d0*x**4*y**4) + 
     -  ((108 + 573*x + 2921*x**2 + 6740*x**3 + 
     -       8892*x**4 + 7170*x**5 + 3488*x**6 + 
     -       946*x**7 + 80*x**8)*lins(1)**4)/
     -   (3d0*x**4*y**4) + 
     -  (2*(-711 + 996*x + 7261*x**2 - 
     -       4787*x**3 - 15547*x**4 - 6819*x**5 + 
     -       3*IPi*(110*y + 78*x**3*y**2 - 
     -          3*x**2*y*(359 + 26*y**2) + 
     -          x*(-110 + 1077*y**2)) + 
     -       36*(720 + 3200*x + 9568*x**2 + 
     -          18293*x**3 + 20188*x**4 + 
     -          13108*x**5 + 5390*x**6 + 
     -          2180*x**7 + 20*x**8)*zeta2)*
     -     lins(1)*lins(2))/(9d0*x**4*y**4) + 
     -  (2*(-212 + 2531*x + 9976*x**2 + 
     -       19441*x**3 + 24488*x**4 + 
     -       19405*x**5 + 9418*x**6 + 2595*x**7 + 
     -       322*x**8 + 
     -       12*IPi*(-45 - 266*x - 749*x**2 - 
     -          1156*x**3 - 980*x**4 - 392*x**5 + 
     -          80*x**6 + 200*x**7 + 20*x**8))*
     -     lins(1)**2*lins(2))/(3d0*x**4*y**4) - 
     -  (4*(-10 + 787*x + 3313*x**2 + 7182*x**3 + 
     -       9426*x**4 + 7560*x**5 + 3650*x**6 + 
     -       980*x**7 + 80*x**8)*lins(1)**3*
     -     lins(2))/(3d0*x**4*y**4) - 
     -  (2*(30 + 63*x + 293*x**2 + 388*x**3 + 
     -       148*x**4 + 200*x**5 + 440*x**6 + 
     -       20*x**7)*lins(1)**2*lins(2)**2)/
     -   (x**3*y**4) - 
     -  (24*(21 + 44*x + 40*x**2 - 
     -       10*x**3*(-1 + y) + 14*x**4*(-1 + y))*
     -     zeta2*lins(3))/(x**3*y**3) - 
     -  (4*(1307 + 3921*x + 7486*x**2 + 
     -       8437*x**3 + 5516*x**4 + 1951*x**5 + 
     -       322*x**6)*(-1 + y)*lins(1)*lins(3))/
     -   (3d0*x**3*y**4) + 
     -  (8*(73 + 219*x + 464*x**2 + 563*x**3 + 
     -       392*x**4 + 147*x**5 + 20*x**6)*
     -     (-1 + y)*lins(1)**2*lins(3))/
     -   (x**3*y**4) + 
     -  (8*(85 + 443*x + 1547*x**2 + 2938*x**3 + 
     -       3240*x**4 + 2136*x**5 + 720*x**6)*
     -     lins(1)*lins(2)*lins(3))/(x**4*y**4) + 
     -  (4*(1307 + 3921*x + 7486*x**2 + 
     -       8437*x**3 + 5516*x**4 + 1951*x**5 + 
     -       322*x**6)*(-1 + y)*lins(4))/
     -   (3d0*x**3*y**4) + 
     -  (16*(-34 - 68*x - 79*x**2 - 45*x**3 + 
     -       4*x**4)*(-1 + y)*lins(1)*lins(4))/
     -   (x**3*y**3) - 
     -  (16*(35 + 174*x + 831*x**2 + 1844*x**3 + 
     -       2320*x**4 + 1768*x**5 + 800*x**6 + 
     -       200*x**7 + 20*x**8)*lins(1)*lins(5))/
     -   (x**4*y**4) - 
     -  (16*(-1 + x)*
     -     (120 + 360*y + 716*y**2 + 832*y**3 + 
     -       537*y**4 + 181*y**5 + 20*y**6)*
     -     lins(6))/(x**4*y**3) + 
     -  (8*(x - y)*(-20 + 181*x*y - 
     -       356*x**2*y**2 + 120*x**3*y**3)*
     -     lins(7))/(x**4*y**4)
      return
      end


      ! -+++: (-m2/s)^5*(log(-m2/s))^4
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_5_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (-1050 - 525*x - 4416*x**2 - 2494*x**3 + 
     -    21282*x**4 + 57946*x**5 + 67508*x**6 + 
     -    38430*x**7 + 8708*x**8 + 1325*x**9)/
     -  (6d0*x**4*y**5)
      return
      end


      ! -+++: (-m2/s)^5*(log(-m2/s))^3
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_5_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-6720 - 19872*x + 30944*x**2 + 
     -     33052*x**3 + 59950*x**4 + 
     -     309919*x**5 + 733977*x**6 + 
     -     849762*x**7 + 468750*x**8 + 
     -     95385*x**9 + 11935*x**10)/
     -   (9d0*x**5*y**5) - 
     -  (2*(2230 + 11150*x + 42171*x**2 + 
     -       101784*x**3 + 156278*x**4 + 
     -       159420*x**5 + 109202*x**6 + 
     -       49152*x**7 + 14019*x**8 + 
     -       2394*x**9 + 210*x**10)*lins(1))/
     -   (3d0*x**5*y**5)
      return
      end


      ! -+++: (-m2/s)^5*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_5_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2955*(-1 + x**7*
     -        (150.558601240834743372814438804d0 - 
     -          (46976*zeta2)/985d0) + 
     -       x**6*(132.297010716300056401579244219d0 - 
     -          (39952*zeta2)/985d0) + 
     -       x**8*(79.0606880992667794698251551043d0 - 
     -          (5832*zeta2)/197d0) + 
     -       x**5*(55.4873096446700507614213197970d0 - 
     -          (11584*zeta2)/985d0) + 
     -       x**9*(13.2843391614965219026132731716d0 - 
     -          (8416*zeta2)/985d0) + 
     -       x*(1.81906373378454596728708403835d0 - 
     -          (256*zeta2)/197d0) + 
     -       x**10*(1.68257191201353637901861252115d0 + 
     -          (136*zeta2)/591d0) + 
     -       x**2*(21.0891144952058657642413987592d0 + 
     -          (288*zeta2)/197d0) + 
     -       x**4*(5.30479413423575860124083474337d0 + 
     -          (8688*zeta2)/985d0) + 
     -       x**3*(12.9197217522090618537319044933d0 + 
     -          (27136*zeta2)/2955d0)))/
     -   (4d0*x**5*y**5) - 
     -  ((42394 + 211970*x + 861237*x**2 + 
     -       2173128*x**3 + 3504436*x**4 + 
     -       3797634*x**5 + 2759231*x**6 + 
     -       1300448*x**7 + 370320*x**8 + 
     -       54438*x**9 + 2673*x**10)*lins(1))/
     -   (6d0*x**5*y**5) + 
     -  ((-562 + 1539*y + 1904*y**2 + 1038*y**3 + 
     -       660*y**4 + 1038*y**5 + 1904*y**6 + 
     -       1539*y**7 - 562*y**8)*lins(1)**2)/
     -   (x**5*y**4) - 
     -  (4*(736 + 1111*x + 510*x**2 + 30*x**3)*
     -     lins(1)*lins(2))/(x**2*y**5)
      return
      end


      ! -+++: (-m2/s)^5*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_5_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-59.3565167031801146384479717813d0)
      double precision rat2
      parameter (rat2=-103.911456783234126984126984127d0)
      double precision rat3
      parameter (rat3=-68.2660467992173721340388007055d0)
      double precision rat4
      parameter (rat4=16.834881107390873015873015873d0)
      double precision rat5
      parameter (rat5=6.91587835510361552028218694885d0)
      double precision rat6
      parameter (rat6=-0.164405841971752517439827409423d0)
      double precision rat7
      parameter (rat7=-0.0854967165116789056619429479087d0)
      double precision rat8
      parameter (rat8=-0.0275532696342525915257552319482d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-3780*IPi*(4 + 2*x - 2*x**7 - 2*x**8 + 
     -       x**9)*zeta2)/(x**4*y**5) + 
     -  (2688*(zeta2 + 
     -       x**7*(-2.50058008156966490299823633157d0 - 
     -          (12743*zeta2)/168d0 - (53*zeta3)/2d0
     -          ) + x**6*
     -        (rat3 - (14871*zeta2)/224d0 - 
     -          23*zeta3) + 
     -       x**8*(rat4 - (641*zeta2)/16d0 - 
     -          (125*zeta3)/8d0) + 
     -       x**5*(rat2 - (10771*zeta2)/448d0 - 
     -          11*zeta3) + 
     -       x**4*(rat1 + (16987*zeta2)/2688d0 - 
     -          (39*zeta3)/8d0) + 
     -       x**9*(rat5 - (27743*zeta2)/4032d0 - 
     -          4*zeta3) + 
     -       x**3*(-12.0603904872134038800705467372d0 + 
     -          (635*zeta2)/168d0 - (19*zeta3)/7d0) 
     -        + x**10*
     -        (0.672853457065696649029982363316d0 + 
     -          (3253*zeta2)/8064d0 - 
     -          (55*zeta3)/42d0) + 
     -       x**2*(0.671843998015873015873015873016d0 - 
     -          (919*zeta2)/224d0 - 
     -          (135*zeta3)/224d0) + 
     -       x*(0.767857142857142857142857142857d0 + 
     -          (5*zeta2)/14d0 + (635*zeta3)/336d0))
     -     )/(x**5*y**5) + 
     -  (2999520*(-0.00115585160292313436816557315837d0 - 
     -       (301*x**10)/1.199808d6 + 
     -       x**5*(-0.199335393662986077772443590975d0 - 
     -          (1267*zeta2)/4166d0) + 
     -       x**6*(-0.161614608337333973435749719955d0 - 
     -          (26584*zeta2)/93735d0) + 
     -       x**4*(rat6 - 
     -          (80339*zeta2)/374940d0) + 
     -       x**7*(rat7 - (16174*zeta2)/93735d0) + 
     -       x**3*(-0.0909014331181877989367187638911d0 - 
     -          (1022*zeta2)/10415d0) + 
     -       x**8*(rat8 - 
     -          (17027*zeta2)/249960d0) + 
     -       x**2*(-0.0313942453014704574954214896606d0 - 
     -          (6557*zeta2)/249960d0) + 
     -       x**9*(-0.00461101554026422005298625557867d0 - 
     -          (6193*zeta2)/374940d0) + 
     -       x*(-0.00577925801461567184082786579186d0 - 
     -          (85*zeta2)/74988d0) - 
     -       (17*zeta2)/74988d0)*lins(1))/
     -   (x**5*y**5) + 
     -  ((-210 - 11706*y + 19593*y**2 + 
     -       30856*y**3 + 24105*y**4 + 
     -       12048*y**5 + 24105*y**6 + 
     -       30856*y**7 + 19593*y**8 - 
     -       11706*y**9 - 210*y**10 - 
     -       3780*IPi*
     -        (-1 + 2*y + 2*y**2 - 2*y**8 - 
     -          2*y**9 + y**10))*lins(1)**2)/
     -   (6d0*x**5*y**5) - 
     -  (10*(278 + 1390*x + 6411*x**2 + 
     -       17304*x**3 + 29294*x**4 + 
     -       33156*x**5 + 25350*x**6 + 
     -       12848*x**7 + 4125*x**8 + 760*x**9 + 
     -       63*x**10)*lins(1)**3)/(3d0*x**5*y**5) 
     -   + ((4226 + 1260*IPi*(x - y) - 7020*y - 
     -       25448*y**2 + 3024*y**3 + 
     -       24678*y**4 + 9124*y**5 - 632*y**6)*
     -     lins(1)*lins(2))/(2d0*x**5*y**5) + 
     -  (2*(542 - 309*y - 144*y**2 + 422*y**3 + 
     -       300*y**4 + 60*y**5 - 630*y**7 - 
     -       630*y**8 + 315*y**9)*lins(1)**2*
     -     lins(2))/(x**5*y**4) + 
     -  (1260*(-1 + 2*y + 2*y**2 - 2*y**8 - 
     -       2*y**9 + y**10)*lins(1)*lins(3))/
     -   (x**5*y**5) - 
     -  (1260*(-1 + 2*y + 2*y**2 - 2*y**8 - 
     -       2*y**9 + y**10)*lins(4))/(x**5*y**5)
      return
      end


      ! -+++: (-m2/s)^5*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_5_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=2.15435445257385361552028218695d0)
      double precision rat2
      parameter (rat2=12.8691987892433449074074074074d0)
      double precision rat3
      parameter (rat3=19.7207545301649305555555555556d0)
      double precision rat4
      parameter (rat4=9.16887130172164351851851851852d0)
      double precision rat5
      parameter (rat5=-5.10061350343088624338624338624d0)
      double precision rat6
      parameter (rat6=-7.71325627725591104497354497354d0)
      double precision rat7
      parameter (rat7=-3.20975318330095348324514991182d0)
      double precision rat8
      parameter (rat8=-0.316408439515128968253968253968d0)
      double precision rat9
      parameter (rat9=-0.000159501878711831077491535637543d0)
      double precision rat10
      parameter (rat10=-0.000157944854777706415471348033968d0)
      double precision rat11
      parameter (rat11=-0.000797509393559155387457678187715d0)
      double precision rat12
      parameter (rat12=-0.00615307549239991436523228430743d0)
      double precision rat13
      parameter (rat13=-0.000517850147333428483075239658412d0)
      double precision rat14
      parameter (rat14=0.00271365577202121839244511049264d0)
      double precision rat15
      parameter (rat15=0.00861843550408549204310283308357d0)
      double precision rat16
      parameter (rat16=0.0130079718572337591284283641381d0)
      double precision rat17
      parameter (rat17=0.0108265092353479308895707953726d0)
      double precision rat18
      parameter (rat18=0.00473401589644932879786230246517d0)
      double precision rat19
      parameter (rat19=-0.017403175845643331192464140441d0)
      double precision rat20
      parameter (rat20=0.000666117418498299198363424439211d0)
      double precision rat21
      parameter (rat21=-0.000224424000258688360807821308785d0)
      double precision rat22
      parameter (rat22=-0.0000715006405882038107471633483194d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (IPi*((13824 - 27264*x + 28456*x**2 + 
     -          374336*x**3 + 503343*x**4 + 
     -          135242*x**5 - 165238*x**6 - 
     -          204512*x**7 - 111825*x**8 - 
     -          20066*x**9 - 12324*x**10)*zeta2 + 
     -       160*x*y*
     -        (10*y + 27*x**2*y - 
     -          x*(10 + 27*y**2))*zeta3))/
     -   (2d0*x**5*y**5) + 
     -  (3584*(zeta3 + 
     -       x**7*(rat5 + (137257*zeta2)/5376d0 - 
     -          (3053*zeta3)/96d0 - 
     -          (1893*zeta4)/224d0) + 
     -       x**4*(rat2 + (695015*zeta2)/21504d0 - 
     -          (45335*zeta3)/2688d0 - 
     -          (21333*zeta4)/3584d0) + 
     -       x**6*(rat4 + (20897*zeta2)/336d0 - 
     -          (5141*zeta3)/192d0 - 
     -          (9057*zeta4)/1792d0) + 
     -       x**5*(rat3 + (93661*zeta2)/1344d0 - 
     -          (44207*zeta3)/2688d0 - 
     -          (1251*zeta4)/256d0) + 
     -       x**8*(rat6 + (37019*zeta2)/7168d0 - 
     -          (3271*zeta3)/224d0 - 
     -          (4185*zeta4)/896d0) + 
     -       x**3*(rat1 + (4979*zeta2)/5376d0 - 
     -          (509*zeta3)/32d0 - (307*zeta4)/112d0
     -          ) + x**10*
     -        (rat8 - (769*zeta2)/1344d0 - 
     -          (1763*zeta3)/1344d0 - 
     -          (3275*zeta4)/1792d0) + 
     -       x**9*(rat7 + (163*zeta2)/256d0 - 
     -          (691*zeta3)/1344d0 + 
     -          (391*zeta4)/896d0) + 
     -       x**2*(-2.15460495721726190476190476190d0 - 
     -          (21739*zeta2)/5376d0 - 
     -          (32813*zeta3)/5376d0 + 
     -          (119055*zeta4)/7168d0) + 
     -       x*(-1.078125d0 - (129*zeta2)/224d0 + 
     -          (471*zeta3)/112d0 + 
     -          (121655*zeta4)/3584d0)))/
     -   (x**5*y**5) + 
     -  ((-2*IPi*(4795 - 8698*y - 14004*y**2 - 
     -          14256*y**3 - 16200*y**4 - 
     -          6480*y**5 - 630*y**8 - 
     -          1120*y**9 + 70*y**10)*zeta2)/
     -      (x**5*y**5) + 
     -     (11957760*
     -        (rat9 + 
     -          x**10*
     -           (rat22 - (7*zeta2)/597888d0) + 
     -          rat10*zeta2 + 
     -          x**9*
     -           (rat21 - (1021*zeta2)/221440d0 + 
     -             (7*zeta3)/62280d0) + 
     -          x**8*
     -           (rat20 + rat19*zeta2 + 
     -             (299*zeta3)/124560d0) + 
     -          x*(rat11 - 
     -             (2833*zeta2)/3.587328d6 + 
     -             (6607*zeta3)/1.49472d6) + 
     -          x**7*
     -           (rat18 - 
     -             (61277*zeta2)/1.49472d6 + 
     -             (247*zeta3)/18684d0) + 
     -          x**2*
     -           (rat13 + rat12*zeta2 + 
     -             (17039*zeta3)/996480d0) + 
     -          x**6*
     -           (rat17 - (57037*zeta2)/896832d0 + 
     -             (5411*zeta3)/149472d0) + 
     -          x**3*
     -           (rat14 - (4951*zeta2)/249120d0 + 
     -             (649*zeta3)/15570d0) + 
     -          x**5*
     -           (rat16 - (64457*zeta2)/996480d0 + 
     -             (1475*zeta3)/24912d0) + 
     -          x**4*
     -           (rat15 - 
     -             (156559*zeta2)/3.587328d6 + 
     -             (290*zeta3)/4671d0) + 
     -          (253*zeta3)/298944d0))/(x**5*y**5))
     -    *lins(1) + 
     -  ((65496 + 327480*x + 1360902*x**2 + 
     -       3478728*x**3 + 6010534*x**4 + 
     -       7231470*x**5 + 5906955*x**6 + 
     -       3165016*x**7 + 993489*x**8 + 
     -       134682*x**9 - 2568*x**10 + 
     -       24*IPi*x*
     -        (15637 + 62548*x + 165974*x**2 + 
     -          279004*x**3 + 302524*x**4 + 
     -          213014*x**5 + 95311*x**6 + 
     -          24880*x**7 + 3081*x**8)*(-1 + y) 
     -        + 72*(3525 + 38982*x + 
     -          238842*x**2 + 730128*x**3 + 
     -          1381748*x**4 + 1738560*x**5 + 
     -          1482972*x**6 + 844352*x**7 + 
     -          304518*x**8 + 62768*x**9 + 
     -          4970*x**10)*zeta2)*lins(1)**2)/
     -   (72d0*x**5*y**5) + 
     -  ((840*IPi*(-23 + 32*y + 39*y**2 - 
     -          39*y**8 - 32*y**9 + 23*y**10) - 
     -       2*(6162 + 3920*y + 28800*y**2 + 
     -          41568*y**3 + 35071*y**4 + 
     -          18648*y**5 + 35071*y**6 + 
     -          41568*y**7 + 28800*y**8 + 
     -          3920*y**9 + 6162*y**10))*
     -     lins(1)**3)/(36d0*x**5*y**5) + 
     -  ((2395 + 15026*x + 92550*x**2 + 
     -       281280*x**3 + 527796*x**4 + 
     -       659880*x**5 + 557364*x**6 + 
     -       313344*x**7 + 112626*x**8 + 
     -       23656*x**9 + 1610*x**10)*lins(1)**4)/
     -   (12d0*x**5*y**5) + 
     -  ((-8865 + 12382*x + 156044*x**2 + 
     -       126536*x**3 + 14132*x**4 + 
     -       10628*x**5 + 6986*x**6 + 
     -       4*IPi*(375*y - 24778*x**3*y**2 + 
     -          x*(-375 + 7334*y**2) + 
     -          x**2*(-7334*y + 24778*y**3)) - 
     -       48*(-6300 - 36120*x - 152880*x**2 - 
     -          403936*x**3 - 671501*x**4 - 
     -          741450*x**5 - 546930*x**6 - 
     -          262080*x**7 - 83055*x**8 - 
     -          24500*x**9 + 245*x**10)*zeta2)*
     -     lins(1)*lins(2))/(12d0*x**5*y**5) + 
     -  ((-3093 + 62460*x + 299865*x**2 + 
     -       804312*x**3 + 1455702*x**4 + 
     -       1770492*x**5 + 1458052*x**6 + 
     -       807272*x**7 + 290142*x**8 + 
     -       62084*x**9 + 6162*x**10 + 
     -       12*IPi*(-770 - 6226*x - 24783*x**2 - 
     -          57312*x**3 - 84600*x**4 - 
     -          80880*x**5 - 47040*x**6 - 
     -          13440*x**7 + 1680*x**8 + 
     -          3640*x**9 + 140*x**10))*
     -     lins(1)**2*lins(2))/(6d0*x**5*y**5) - 
     -  ((35 + 20302*x + 104736*x**2 + 
     -       303984*x**3 + 566000*x**4 + 
     -       700200*x**5 + 584820*x**6 + 
     -       325920*x**7 + 116340*x**8 + 
     -       24220*x**9 + 1610*x**10)*lins(1)**3*
     -     lins(2))/(3d0*x**5*y**5) + 
     -  ((-630 - 315*x - 1472*x**2 - 2362*x**3 - 
     -       1140*x**4 - 180*x**5 - 4200*x**7 - 
     -       8120*x**8 + 140*x**9)*lins(1)**2*
     -     lins(2)**2)/(2d0*x**4*y**5) - 
     -  (2*(35 + 2*x*y*
     -        (-143 - 721*y - 1655*y**2 - 
     -          1045*y**3 + 2125*y**4 + 
     -          3275*y**5 + 1477*y**6 + 
     -          251*y**7 + 35*y**8))*zeta2*lins(3)
     -     )/(x**5*y**5) - 
     -  (2*(15637 + 62548*x + 165974*x**2 + 
     -       279004*x**3 + 302524*x**4 + 
     -       213014*x**5 + 95311*x**6 + 
     -       24880*x**7 + 3081*x**8)*(-1 + y)*
     -     lins(1)*lins(3))/(3d0*x**4*y**5) + 
     -  (2*(1923 + 7692*x + 21618*x**2 + 
     -       37932*x**3 + 42414*x**4 + 
     -       30582*x**5 + 13893*x**6 + 
     -       3642*x**7 + 385*x**8)*(-1 + y)*
     -     lins(1)**2*lins(3))/(x**4*y**5) + 
     -  (4*(805 + 5354*x + 25122*x**2 + 
     -       66408*x**3 + 110820*x**4 + 
     -       123120*x**5 + 91140*x**6 + 
     -       43680*x**7 + 11970*x**8 + 700*x**9 + 
     -       140*x**10)*lins(1)*lins(2)*lins(3))/
     -   (x**5*y**5) + 
     -  (2*(15637 + 62548*x + 165974*x**2 + 
     -       279004*x**3 + 302524*x**4 + 
     -       213014*x**5 + 95311*x**6 + 
     -       24880*x**7 + 3081*x**8)*(-1 + y)*
     -     lins(4))/(3d0*x**4*y**5) + 
     -  (4*(564 + 2256*x + 5064*x**2 + 
     -       7296*x**3 + 7362*x**4 + 5196*x**5 + 
     -       2034*x**6 + 276*x**7 + 35*x**8)*
     -     (-1 + y)*lins(1)*lins(4))/(x**4*y**5) 
     -   + (4*(-385 + 844*y + 1887*y**2 + 
     -       3168*y**3 + 3600*y**4 + 1440*y**5 + 
     -       840*y**8 + 840*y**9 - 420*y**10)*
     -     lins(1)*lins(5))/(x**5*y**5) - 
     -  (4*(-1 + x)*(455 - 776*x + 344*x**2 + 
     -       2200*x**3 + 4190*x**4 + 2200*x**5 + 
     -       344*x**6 - 776*x**7 + 455*x**8)*
     -     lins(6))/(x**5*y**4) - 
     -  (2*(x - y)*(455 - 4416*x*y + 
     -       14100*x**2*y**2 - 13440*x**3*y**3 + 
     -       2940*x**4*y**4)*lins(7))/(x**5*y**5)
      return
      end


      ! -+++: (-m2/s)^6*(log(-m2/s))^4
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_6_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (2*(-1260 - 630*x + 5184*x**2 + 
     -      39840*x**3 + 84435*x**4 + 
     -      92652*x**5 + 97032*x**6 + 
     -      120765*x**7 + 104330*x**8 + 
     -      47106*x**9 + 8661*x**10 + 1351*x**11))
     -   /(3d0*x**5*y**6)
      return
      end


      ! -+++: (-m2/s)^6*(log(-m2/s))^3
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_6_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (8960*(-1 - (103*x)/35d0 + 
     -       (118*x**2)/35d0 + (8629*x**3)/600d0 + 
     -       (206677*x**4)/2688d0 + 
     -       (3598267*x**5)/22400d0 + 
     -       (1199881*x**6)/6720d0 + 
     -       (816593*x**7)/4200d0 + 
     -       (3270779*x**8)/13440d0 + 
     -       (349217*x**9)/1680d0 + 
     -       (1018279*x**10)/11200d0 + 
     -       (199849*x**11)/13440d0 + 
     -       (62281*x**12)/33600d0))/(3d0*x**6*y**6)
     -    - (8*(2324 + 13944*x + 67183*x**2 + 
     -       208095*x**3 + 427843*x**4 + 
     -       616186*x**5 + 631463*x**6 + 
     -       458353*x**7 + 231967*x**8 + 
     -       79398*x**9 + 17634*x**10 + 
     -       2438*x**11 + 189*x**12)*lins(1))/
     -   (3d0*x**6*y**6)
      return
      end


      ! -+++: (-m2/s)^6*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_6_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=98.0861895332775362170916093385d0)
      double precision rat2
      parameter (rat2=244.499112349534910622546117677d0)
      double precision rat3
      parameter (rat3=256.13484019569817842829049884d0)
      double precision rat4
      parameter (rat4=226.301993045774048897326501589d0)
      double precision rat5
      parameter (rat5=254.351018297371411462905124489d0)
      double precision rat6
      parameter (rat6=210.967516892236715863392328577d0)
      double precision rat7
      parameter (rat7=85.6615384353672062656078823633d0)
      double precision rat8
      parameter (rat8=97.6080637515763489522310368575d0)
      double precision rat9
      parameter (rat9=209.173389440921571608725594541d0)
      double precision rat10
      parameter (rat10=317.045175254228192721516157018d0)
      double precision rat11
      parameter (rat11=343.321678040229319643320305163d0)
      double precision rat12
      parameter (rat12=264.260788487509950004024720729d0)
      double precision rat13
      parameter (rat13=141.862461876950871575633446324d0)
      double precision rat14
      parameter (rat14=50.9601865681653534151991342379d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (16329*(-1 + 
     -       x**8*(rat5 - 
     -          (1167800*zeta2)/16329d0) + 
     -       x**6*(rat3 - 
     -          (1029280*zeta2)/16329d0) + 
     -       x**9*(rat6 - 
     -          (1016800*zeta2)/16329d0) + 
     -       x**7*(rat4 - 
     -          (972320*zeta2)/16329d0) + 
     -       x**5*(rat2 - 
     -          (970760*zeta2)/16329d0) + 
     -       x**10*(rat7 - 
     -          (503200*zeta2)/16329d0) + 
     -       x**4*(rat1 - 
     -          (437600*zeta2)/16329d0) + 
     -       x**11*(9.95123195949945904015351011493d0 - 
     -          (126680*zeta2)/16329d0) + 
     -       x*(1.79680323351093147161491824362d0 - 
     -          (6400*zeta2)/5443d0) + 
     -       x**3*(11.9049645824402392471472023190d0 - 
     -          (5840*zeta2)/5443d0) + 
     -       x**12*(1.77110457876579500683854900280d0 + 
     -          (7000*zeta2)/16329d0) + 
     -       x**2*(14.9591401800477677751240124931d0 + 
     -          (7200*zeta2)/5443d0)))/
     -   (5d0*x**6*y**6) - 
     -  (447236*(1 + 6*x + 
     -       (3412591*x**2)/111809d0 + rat8*x**3 + 
     -       rat9*x**4 + rat10*x**5 + 
     -       rat11*x**6 + rat12*x**7 + 
     -       rat13*x**8 + rat14*x**9 + 
     -       (2533777*x**10)/223618d0 + 
     -       (298699*x**11)/223618d0 + 
     -       (12753*x**12)/223618d0)*lins(1))/
     -   (15d0*x**6*y**6) - 
     -  (4*(1946 + 9730*x + 47362*x**2 + 
     -       131068*x**3 + 232975*x**4 + 
     -       281053*x**5 + 230108*x**6 + 
     -       125247*x**7 + 42585*x**8 + 
     -       7878*x**9 + 547*x**10)*lins(1)**2)/
     -   (x**6*y**5) + 
     -  (8*(1728 + 13280*x + 26883*x**2 + 
     -       21784*x**3 + 6264*x**4)*lins(1)*
     -     lins(2))/(x**3*y**6)
      return
      end


      ! -+++: (-m2/s)^6*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_6_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-2.71767710400132275132275132275d0)
      double precision rat2
      parameter (rat2=1.64958038814484126984126984127d0)
      double precision rat3
      parameter (rat3=-40.4044258432539682539682539683d0)
      double precision rat4
      parameter (rat4=-145.814560825892857142857142857d0)
      double precision rat5
      parameter (rat5=-180.966503348214285714285714286d0)
      double precision rat6
      parameter (rat6=-87.4509188988095238095238095238d0)
      double precision rat7
      parameter (rat7=14.9631477554563492063492063492d0)
      double precision rat8
      parameter (rat8=5.99748381696428571428571428571d0)
      double precision rat9
      parameter (rat9=0.866731099123677248677248677249d0)
      double precision rat10
      parameter (rat10=2.4714306d9)
      double precision rat11
      parameter (rat11=-6.89452227925531606403729618519d-6)
      double precision rat12
      parameter (rat12=-2.26589409389039692233316201555d-6)
      double precision rat13
      parameter (rat13=-0.0000413671336755318963842237771111d0)
      double precision rat14
      parameter (rat14=-0.000272700712966274135582308751323d0)
      double precision rat15
      parameter (rat15=-0.000219137854811702986925872003041d0)
      double precision rat16
      parameter (rat16=-0.000984304839472328294389492466428d0)
      double precision rat17
      parameter (rat17=-0.000971065098894543103901036104352d0)
      double precision rat18
      parameter (rat18=-0.00267152798059553037823518087055d0)
      double precision rat19
      parameter (rat19=-0.0023110350337169087410344437752d0)
      double precision rat20
      parameter (rat20=-0.0050092703392116290864084955491d0)
      double precision rat21
      parameter (rat21=-0.00379334956846451605802728185044d0)
      double precision rat22
      parameter (rat22=-0.00659524730332302270595824135219d0)
      double precision rat23
      parameter (rat23=-0.00443495377760377149799616285222d0)
      double precision rat24
      parameter (rat24=-0.00618222012788868115495535258d0)
      double precision rat25
      parameter (rat25=-0.00370717969853843626710240889089d0)
      double precision rat26
      parameter (rat26=-0.00410213582368042218138757365875d0)
      double precision rat27
      parameter (rat27=-0.00218283315114915403428461411963d0)
      double precision rat28
      parameter (rat28=-0.00187736447060257326262772662926d0)
      double precision rat29
      parameter (rat29=-0.00087151752421361853243128808787d0)
      double precision rat30
      parameter (rat30=-0.000575213400691890761569432700234d0)
      double precision rat31
      parameter (rat31=-0.000218223752671833067050314906678d0)
      double precision rat32
      parameter (rat32=-0.00011201285603569042157202391198d0)
      double precision rat33
      parameter (rat33=-0.0000289221860227657436771866284878d0)
      double precision rat34
      parameter (rat34=-1.23348800488267807317753531092d-6)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-18144*IPi*(4 + 2*x - 2*x**9 - 2*x**10 + 
     -       x**11)*zeta2)/(x**5*y**6) + 
     -  (10752*(zeta2 + 
     -       x**8*(rat6 - (1170391*zeta2)/8064d0 - 
     -          (17195*zeta3)/336d0) + 
     -       x**9*(-0.986302910052910052910052910053d0 - 
     -          (982549*zeta2)/8064d0 - 
     -          (955*zeta3)/21d0) + 
     -       x**7*(rat5 - 
     -          (1225271*zeta2)/10080d0 - 
     -          (809*zeta3)/21d0) + 
     -       x**6*(rat4 - 
     -          (4929697*zeta2)/40320d0 - 
     -          (779*zeta3)/24d0) + 
     -       x**5*(rat3 - (324143*zeta2)/2880d0 - 
     -          (9419*zeta3)/336d0) + 
     -       x**10*(rat7 - (58477*zeta2)/1152d0 - 
     -          (1789*zeta3)/84d0) + 
     -       x**4*(rat2 - (97337*zeta2)/2016d0 - 
     -          (305*zeta3)/21d0) + 
     -       x**11*(rat8 - (89813*zeta2)/13440d0 - 
     -          (1427*zeta3)/336d0) + 
     -       x**3*(rat1 - (703*zeta2)/112d0 - 
     -          (323*zeta3)/84d0) + 
     -       x**12*(rat9 + (21683*zeta2)/40320d0 - 
     -          (65*zeta3)/48d0) + 
     -       x**2*(-0.790085565476190476190476190476d0 - 
     -          (799*zeta2)/224d0 - (57*zeta3)/112d0
     -          ) + x*
     -        (0.767857142857142857142857142857d0 + 
     -          (5*zeta2)/14d0 + (349*zeta3)/168d0))
     -     )/(x**6*y**6) + 
     -  (rat10*(rat11 + rat34*x**12 + 
     -       x*(rat13 - (56*zeta2)/4.119051d6) + 
     -       rat12*zeta2 + 
     -       x**2*(rat14 + rat15*zeta2) + 
     -       x**3*(rat16 + rat17*zeta2) + 
     -       x**4*(rat19 + rat18*zeta2) + 
     -       x**5*(rat21 + rat20*zeta2) + 
     -       x**6*(rat23 + rat22*zeta2) + 
     -       x**7*(rat25 + rat24*zeta2) + 
     -       x**8*(rat27 + rat26*zeta2) + 
     -       x**9*(rat29 + rat28*zeta2) + 
     -       x**10*(rat31 + rat30*zeta2) + 
     -       x**11*(rat33 + rat32*zeta2))*lins(1))
     -    /(x**6*y**6) + 
     -  (2*(-756 - 58694*y + 167600*y**2 + 
     -       329940*y**3 + 287315*y**4 + 
     -       117977*y**5 + 66728*y**6 + 
     -       117977*y**7 + 287315*y**8 + 
     -       329940*y**9 + 167600*y**10 - 
     -       58694*y**11 - 756*y**12 - 
     -       22680*IPi*
     -        (-1 + 2*y + 2*y**2 - 2*y**10 - 
     -          2*y**11 + y**12))*lins(1)**2)/
     -   (15d0*x**6*y**6) - 
     -  (112*(112 + 672*x + 3880*x**2 + 
     -       13240*x**3 + 29530*x**4 + 
     -       46072*x**5 + 51479*x**6 + 
     -       41401*x**7 + 23725*x**8 + 
     -       9415*x**9 + 2453*x**10 + 379*x**11 + 
     -       27*x**12)*lins(1)**3)/(3d0*x**6*y**6) 
     -   + (4*(30789 + 11340*IPi*(x - y) - 
     -       59609*y - 163224*y**2 - 
     -       252265*y**3 - 1655660*y**4 - 
     -       3431616*y**5 - 2755969*y**6 - 
     -       783294*y**7)*lins(1)*lins(2))/
     -   (15d0*x**6*y**6) + 
     -  (8*(533 - 728*y - 762*y**2 + 335*y**3 + 
     -       683*y**4 + 336*y**5 + 56*y**6 - 
     -       756*y**9 - 756*y**10 + 378*y**11)*
     -     lins(1)**2*lins(2))/(x**6*y**5) - 
     -  (6048*(-1 + y)*(-x + y**2)*
     -     (1 - 3*y + y**2 - 2*y**4 + y**6 - 
     -       3*y**7 + y**8)*lins(1)*lins(3))/
     -   (x**5*y**6) + 
     -  (6048*(-1 + y)*(-x + y**2)*
     -     (1 - 3*y + y**2 - 2*y**4 + y**6 - 
     -       3*y**7 + y**8)*lins(4))/(x**5*y**6)
      return
      end


      ! -+++: (-m2/s)^6*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_6_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=0.061238057777364417989417989418d0)
      double precision rat2
      parameter (rat2=-25.311489236068052799823633157d0)
      double precision rat3
      parameter (rat3=-48.476249341423128858024691358d0)
      double precision rat4
      parameter (rat4=27.6573423549107142857142857143d0)
      double precision rat5
      parameter (rat5=-15.6127563372791969797178130511d0)
      double precision rat6
      parameter (rat6=95.0551943824404761904761904762d0)
      double precision rat7
      parameter (rat7=25.3245925371334876543209876543d0)
      double precision rat8
      parameter (rat8=22.6396130366787918871252204586d0)
      double precision rat9
      parameter (rat9=92.0820614769345238095238095238d0)
      double precision rat10
      parameter (rat10=7.59762562197730654761904761905d0)
      double precision rat11
      parameter (rat11=0.710685970784074625220458553792d0)
      double precision rat12
      parameter (rat12=-0.975509949828455687830687830688d0)
      double precision rat13
      parameter (rat13=-0.316834820265997023809523809524d0)
      double precision rat14
      parameter (rat14=-0.000140681846898754649846352903121d0)
      double precision rat15
      parameter (rat15=-0.000122337457544881125667151868025d0)
      double precision rat16
      parameter (rat16=-0.000844091081392527899078117418729d0)
      double precision rat17
      parameter (rat17=-0.000734024745269286754002911208151d0)
      double precision rat18
      parameter (rat18=-0.00573808375788452207666181465308d0)
      double precision rat19
      parameter (rat19=-0.00238507777373443312307941128902d0)
      double precision rat20
      parameter (rat20=-0.021961858624454148471615720524d0)
      double precision rat21
      parameter (rat21=-0.00418788728924065987384764677341d0)
      double precision rat22
      parameter (rat22=-0.0590892846312469674915089762251d0)
      double precision rat23
      parameter (rat23=-0.00274948691851649684619116933527d0)
      double precision rat24
      parameter (rat24=-0.112660258976225133430373605046d0)
      double precision rat25
      parameter (rat25=0.00484437416606016496846191169335d0)
      double precision rat26
      parameter (rat26=-0.153692979742843279961183891315d0)
      double precision rat27
      parameter (rat27=0.0133223195278384279475982532751d0)
      double precision rat28
      parameter (rat28=-0.150933566836487142163998059195d0)
      double precision rat29
      parameter (rat29=0.0147075242828117418728772440563d0)
      double precision rat30
      parameter (rat30=-0.105788255094614264919941775837d0)
      double precision rat31
      parameter (rat31=0.00889419594932678311499272197962d0)
      double precision rat32
      parameter (rat32=-0.0515709531174187287724405628336d0)
      double precision rat33
      parameter (rat33=0.0026608949185266052078279152515d0)
      double precision rat34
      parameter (rat34=-0.0169836987506065016982047549733d0)
      double precision rat35
      parameter (rat35=0.0000538950448811256671518680252305d0)
      double precision rat36
      parameter (rat36=-0.00355397713488597768073750606502d0)
      double precision rat37
      parameter (rat37=-0.000185651013868672165615397056445d0)
      double precision rat38
      parameter (rat38=-0.0000356349314653081028626880155264d0)
      double precision rat39
      parameter (rat39=22.6753593441374550819828181148d0)
      double precision rat40
      parameter (rat40=47.7914090573760738678279907665d0)
      double precision rat41
      parameter (rat41=73.6708971704623878536922015183d0)
      double precision rat42
      parameter (rat42=83.4162685324004664334499416958d0)
      double precision rat43
      parameter (rat43=40.8676608124509173984436353252d0)
      double precision rat44
      parameter (rat44=3.77376455434711607551113723953d0)
      double precision rat45
      parameter (rat45=3.20835346038278137043569142335d0)
      double precision rat46
      parameter (rat46=2258.07283561163687346652646337d0)
      double precision rat47
      parameter (rat47=2653.23743427970557308096740273d0)
      double precision rat48
      parameter (rat48=2257.21247809323519102698913424d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-2*IPi*((-69120 + 132480*x + 
     -          59640*x**2 - 842692*x**3 - 
     -          1449940*x**4 - 947847*x**5 + 
     -          46655*x**6 + 1321660*x**7 + 
     -          2234510*x**8 + 1958645*x**9 + 
     -          853047*x**10 + 124762*x**11 + 
     -          70668*x**12)*zeta2 + 
     -       320*x*y*
     -        (-46*y + 63*x**3*y**2 + 
     -          x*(46 + 15*y**2) - 
     -          3*x**2*y*(5 + 21*y**2))*zeta3))/
     -   (5d0*x**6*y**6) + 
     -  (14336*(zeta3 + 
     -       x**8*(rat8 + rat9*zeta2 - 
     -          (309709*zeta3)/5376d0 - 
     -          (15075*zeta4)/512d0) + 
     -       x**9*(rat10 + 
     -          (3563659*zeta2)/107520d0 - 
     -          (139751*zeta3)/2688d0 - 
     -          (12909*zeta4)/448d0) + 
     -       x**10*(rat11 + 
     -          (546593*zeta2)/67200d0 - 
     -          (37673*zeta3)/1920d0 - 
     -          (3165*zeta4)/256d0) + 
     -       x**7*(rat7 + (900451*zeta2)/6720d0 - 
     -          (1314121*zeta3)/26880d0 - 
     -          (4851*zeta4)/512d0) + 
     -       x**12*(rat13 - 
     -          (1251227*zeta2)/2.1504d6 - 
     -          (37333*zeta3)/26880d0 - 
     -          (531*zeta4)/256d0) + 
     -       x**11*(rat12 + 
     -          (621503*zeta2)/307200d0 + 
     -          (275*zeta3)/5376d0 - 
     -          (21*zeta4)/512d0) + 
     -       x**3*(rat1 + 
     -          (1412717*zeta2)/537600d0 - 
     -          (571873*zeta3)/53760d0 + 
     -          (2813*zeta4)/1792d0) + 
     -       x**4*(rat2 + (185063*zeta2)/35840d0 - 
     -          (319729*zeta3)/10752d0 + 
     -          (1995*zeta4)/128d0) + 
     -       x**2*(-0.289316328745039682539682539683d0 - 
     -          (46111*zeta2)/21504d0 - 
     -          (1185*zeta3)/256d0 + 
     -          (71613*zeta4)/3584d0) + 
     -       x**6*(rat5 + rat6*zeta2 - 
     -          (3090439*zeta3)/53760d0 + 
     -          (1455*zeta4)/64d0) + 
     -       x**5*(rat3 + rat4*zeta2 - 
     -          (3105083*zeta3)/53760d0 + 
     -          (239709*zeta4)/7168d0) + 
     -       x*(-1.078125d0 - (129*zeta2)/224d0 + 
     -          (67*zeta3)/16d0 + 
     -          (72913*zeta4)/1792d0)))/(x**6*y**6)
     -    + ((-72*IPi*
     -        (623 - 1149*y - 1778*y**2 - 
     -          2146*y**3 - 3205*y**4 - 
     -          2458*y**5 - 1176*y**6 - 
     -          336*y**7 - 84*y**10 - 140*y**11 + 
     -          14*y**12)*zeta2)/(x**6*y**6) + 
     -     (87936000*
     -        (rat14 + 
     -          x**12*
     -           (rat38 - (21*zeta2)/4.58d6) + 
     -          rat15*zeta2 + 
     -          x**11*
     -           (rat37 + rat36*zeta2 + 
     -             (13*zeta3)/229000d0) + 
     -          x**10*
     -           (rat35 + rat34*zeta2 + 
     -             (1157*zeta3)/687000d0) + 
     -          x*(rat16 + rat17*zeta2 + 
     -             (859*zeta3)/274800d0) + 
     -          x**9*
     -           (rat33 + rat32*zeta2 + 
     -             (8183*zeta3)/687000d0) + 
     -          x**2*
     -           (rat19 + rat18*zeta2 + 
     -             (21469*zeta3)/1.374d6) + 
     -          x**8*
     -           (rat31 + rat30*zeta2 + 
     -             (14581*zeta3)/343500d0) + 
     -          x**3*
     -           (rat21 + rat20*zeta2 + 
     -             (45247*zeta3)/916000d0) + 
     -          x**7*
     -           (rat29 + rat28*zeta2 + 
     -             (85337*zeta3)/916000d0) + 
     -          x**4*
     -           (rat23 + rat22*zeta2 + 
     -             (275857*zeta3)/2.748d6) + 
     -          x**6*
     -           (rat27 + rat26*zeta2 + 
     -             (75313*zeta3)/549600d0) + 
     -          x**5*
     -           (rat25 + rat24*zeta2 + 
     -             (19241*zeta3)/137400d0) + 
     -          (553*zeta3)/1.0992d6))/(x**6*y**6)
     -     )*lins(1) + 
     -  ((4*IPi*(107794 + 431176*x + 
     -          1266105*x**2 + 2289199*x**3 + 
     -          2487228*x**4 + 1662163*x**5 + 
     -          682206*x**6 + 160795*x**7 + 
     -          17667*x**8)*(x**2 - y)*(-1 + y))/
     -      (15d0*x**5*y**6) + 
     -     (18676*(0.281172984936103376882987077890d0 + zeta2 + 
     -          x**12*
     -           (-0.00588991218676376097665452987792d0 + 
     -             (846*zeta2)/667d0) + 
     -          x*(1.68703790961662026129792246734d0 + 
     -             (7911*zeta2)/667d0) + 
     -          x**11*
     -           (0.447689369600913828799885771400d0 + 
     -             (88138*zeta2)/4669d0) + 
     -          x**2*
     -           (7.62797470312462816210942147974d0 + 
     -             (394936*zeta2)/4669d0) + 
     -          x**10*
     -           (4.05601734846862283144142214607d0 + 
     -             (520622*zeta2)/4669d0) + 
     -          x**3*
     -           (rat39 + (1494676*zeta2)/4669d0) 
     -           + x**9*
     -           (16.4797619047619047619047619048d0 + 
     -             (1816594*zeta2)/4669d0) + 
     -          x**4*
     -           (rat40 + (522187*zeta2)/667d0) + 
     -          x**8*
     -           (rat43 + (4170046*zeta2)/4669d0) 
     -           + x**5*
     -           (rat41 + (6217870*zeta2)/4669d0) 
     -           + x**7*
     -           (69.0797577401775302824778087147d0 + 
     -             (950224*zeta2)/667d0) + 
     -          x**6*
     -           (rat42 + (1082096*zeta2)/667d0)))/
     -      (x**6*y**6))*lins(1)**2 + 
     -  ((15120*IPi*(-15 + 22*y + 26*y**2 - 
     -          26*y**10 - 22*y**11 + 15*y**12) - 
     -       4*(35334 + 21978*y + 196886*y**2 + 
     -          362660*y**3 + 364265*y**4 + 
     -          201208*y**5 + 146792*y**6 + 
     -          201208*y**7 + 364265*y**8 + 
     -          362660*y**9 + 196886*y**10 + 
     -          21978*y**11 + 35334*y**12))*
     -     lins(1)**3)/(90d0*x**6*y**6) + 
     -  ((2947 + 21591*x + 155856*x**2 + 
     -       585308*x**3 + 1417623*x**4 + 
     -       2389794*x**5 + 2880696*x**6 + 
     -       2501688*x**7 + 1551902*x**8 + 
     -       669558*x**9 + 191274*x**10 + 
     -       32926*x**11 + 1890*x**12)*lins(1)**4)
     -    /(3d0*x**6*y**6) + 
     -  ((4*IPi*(-387*y + 14825*x**4*y**3 - 
     -          x**3*y**2*(44894 + 14825*y**2) + 
     -          x*(387 + 30554*y**2) + 
     -          x**2*(-30554*y + 44894*y**3)))/
     -      (15d0*x**6*y**6) + 
     -     (108864*(-0.0299988977072310405643738977072d0 + 
     -          zeta2 + (625*x**8*zeta2)/3d0 + 
     -          (710*x**9*zeta2)/9d0 + 
     -          (1091*x**10*zeta2)/54d0 + 
     -          (253*x**11*zeta2)/54d0 - 
     -          (x**12*zeta2)/12d0 + 
     -          x*(0.0424382716049382716049382716049d0 + 
     -             (190*zeta2)/27d0) + 
     -          x**2*
     -           (0.386965020576131687242798353909d0 + 
     -             (1028*zeta2)/27d0) + 
     -          x**3*
     -           (0.154037453458749755046051342348d0 + 
     -             (8042*zeta2)/63d0) + 
     -          x**4*
     -           (1.36347379401005944215820759031d0 + 
     -             (477455*zeta2)/1701d0) + 
     -          x**7*
     -           (0.898530684564635181919132536416d0 + 
     -             (636511*zeta2)/1701d0) + 
     -          x**5*
     -           (rat44 + (652199*zeta2)/1512d0) + 
     -          x**6*(rat45 + (115277*zeta2)/243d0)
     -          ))/(x**6*y**6))*lins(1)*lins(2) + 
     -  ((8*IPi*(-819 - 8356*x - 42518*x**2 - 
     -          129268*x**3 - 261970*x**4 - 
     -          367036*x**5 - 357504*x**6 - 
     -          238224*x**7 - 102060*x**8 - 
     -          22680*x**9 + 2016*x**10 + 
     -          4032*x**11))/(x**6*y**6) + 
     -     (1902*(-1 + (433889*x)/14265d0 + 
     -          (275968*x**2)/1585d0 + 
     -          (1686170*x**3)/2853d0 + 
     -          (3936877*x**4)/2853d0 + 
     -          rat46*x**5 + rat47*x**6 + 
     -          rat48*x**7 + 
     -          (3936770*x**8)/2853d0 + 
     -          (1690600*x**9)/2853d0 + 
     -          (270576*x**10)/1585d0 + 
     -          (427592*x**11)/14265d0 + 
     -          (3926*x**12)/1585d0))/(x**6*y**6))*
     -   lins(1)**2*lins(2) - 
     -  (4*(189 + 28645*x + 175994*x**2 + 
     -       638446*x**3 + 1535995*x**4 + 
     -       2565550*x**5 + 3060176*x**6 + 
     -       2629928*x**7 + 1615950*x**8 + 
     -       691740*x**9 + 196308*x**10 + 
     -       33516*x**11 + 1890*x**12)*lins(1)**3*
     -     lins(2))/(3d0*x**6*y**6) + 
     -  (2*(-756 - 378*x + 1728*x**2 + 
     -       13280*x**3 + 27009*x**4 + 
     -       21896*x**5 + 6376*x**6 - 5040*x**9 - 
     -       9072*x**10 + 504*x**11)*lins(1)**2*
     -     lins(2)**2)/(x**5*y**6) - 
     -  (24*(21 + x*y*
     -        (-85 - 517*y - 1629*y**2 - 
     -          1576*y**3 - 210*y**4 + 
     -          1386*y**5 + 3194*y**6 + 
     -          3216*y**7 + 1076*y**8 + 
     -          128*y**9 + 42*y**10))*zeta2*
     -     lins(3))/(x**6*y**6) - 
     -  (8*(107794 + 431176*x + 1266105*x**2 + 
     -       2289199*x**3 + 2487228*x**4 + 
     -       1662163*x**5 + 682206*x**6 + 
     -       160795*x**7 + 17667*x**8)*(x**2 - y)*
     -     (-1 + y)*lins(1)*lins(3))/
     -   (15d0*x**5*y**6) + 
     -  (8*(2729 + 13645*x + 47984*x**2 + 
     -       110066*x**3 + 170636*x**4 + 
     -       183986*x**5 + 139077*x**6 + 
     -       72631*x**7 + 24985*x**8 + 
     -       5083*x**9 + 441*x**10)*(-1 + y)*
     -     lins(1)**2*lins(3))/(x**5*y**6) + 
     -  (8*(1827 + 14699*x + 85582*x**2 + 
     -       287654*x**3 + 635615*x**4 + 
     -       980342*x**5 + 1078392*x**6 + 
     -       849408*x**7 + 472500*x**8 + 
     -       178920*x**9 + 41328*x**10 + 
     -       3024*x**11 + 504*x**12)*lins(1)*
     -     lins(2)*lins(3))/(x**6*y**6) + 
     -  (8*(107794 + 431176*x + 1266105*x**2 + 
     -       2289199*x**3 + 2487228*x**4 + 
     -       1662163*x**5 + 682206*x**6 + 
     -       160795*x**7 + 17667*x**8)*(x**2 - y)*
     -     (-1 + y)*lins(4))/(15d0*x**5*y**6) + 
     -  (16*(590 + 2950*x + 8927*x**2 + 
     -       18008*x**3 + 27656*x**4 + 
     -       32330*x**5 + 26325*x**6 + 
     -       13876*x**7 + 4336*x**8 + 670*x**9 + 
     -       63*x**10)*(-1 + y)*lins(1)*lins(4))/
     -   (x**5*y**6) - 
     -  (16*(441 - 968*y - 2086*y**2 - 
     -       4292*y**3 - 6410*y**4 - 4916*y**5 - 
     -       2352*y**6 - 672*y**7 - 1008*y**10 - 
     -       1008*y**11 + 504*y**12)*lins(1)*
     -     lins(5))/(x**6*y**6) - 
     -  (48*(-1 + x)*
     -     (189 - 169*x + 906*x**2 + 3650*x**3 + 
     -       8257*x**4 + 10398*x**5 + 8257*x**6 + 
     -       3650*x**7 + 906*x**8 - 169*x**9 + 
     -       189*x**10)*lins(6))/(x**6*y**5) + 
     -  (24*(x - y)*(-189 + 2059*x*y - 
     -       8873*x**2*y**2 + 14616*x**3*y**3 - 
     -       9240*x**4*y**4 + 1344*x**5*y**5)*
     -     lins(7))/(x**6*y**6)
      return
      end


      ! -+++: (-m2/s)^7*(log(-m2/s))^4
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_7_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (-3850 - 1925*x + 34816*x**3 + 
     -    86176*x**4 + 83902*x**5 + 102234*x**6 + 
     -    279014*x**7 + 520594*x**8 + 
     -    585652*x**9 + 398922*x**10 + 
     -    149430*x**11 + 23124*x**12 + 3689*x**13)
     -   /(x**6*y**7)
      return
      end


      ! -+++: (-m2/s)^7*(log(-m2/s))^3
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_7_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=225.408227306547619047619047619d0)
      double precision rat2
      parameter (rat2=419.449825148809523809523809524d0)
      double precision rat3
      parameter (rat3=468.674516369047619047619047619d0)
      double precision rat4
      parameter (rat4=315.574549851190476190476190476d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (35840*(-1 - (1641*x)/560d0 + 
     -       (379*x**2)/112d0 - 
     -       (406619*x**3)/67200d0 - 
     -       (431507*x**4)/26880d0 - 
     -       (2030179*x**5)/268800d0 + 
     -       (215233*x**6)/26880d0 + 
     -       (1181527*x**7)/17920d0 + rat1*x**8 + 
     -       rat2*x**9 + rat3*x**10 + 
     -       rat4*x**11 + 
     -       (5135117*x**12)/44800d0 + 
     -       (2110093*x**13)/134400d0 + 
     -       (1032191*x**14)/537600d0))/
     -   (3d0*x**7*y**7) - 
     -  (25816*(1 + 7*x + (791663*x**2)/19362d0 + 
     -       (498006*x**3)/3227d0 + 
     -       (2565783*x**4)/6454d0 + 
     -       (7163321*x**5)/9681d0 + 
     -       (3283704*x**6)/3227d0 + 
     -       (480474*x**7)/461d0 + 
     -       (5121359*x**8)/6454d0 + 
     -       (1430077*x**9)/3227d0 + 
     -       (244916*x**10)/1383d0 + 
     -       (157408*x**11)/3227d0 + 
     -       (57147*x**12)/6454d0 + 
     -       (9955*x**13)/9681d0 + (33*x**14)/461d0)
     -      *lins(1))/(x**7*y**7)
      return
      end


      ! -+++: (-m2/s)^7*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_7_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-20.0388146309531168520341285352d0)
      double precision rat2
      parameter (rat2=-34.8338310279295429293289523529d0)
      double precision rat3
      parameter (rat3=13.1452866419721386127000882853d0)
      double precision rat4
      parameter (rat4=35.1063462797328933332699327478d0)
      double precision rat5
      parameter (rat5=70.7922915983129104197277261856d0)
      double precision rat6
      parameter (rat6=213.037501287824393058269893122d0)
      double precision rat7
      parameter (rat7=394.797734142324804369568352964d0)
      double precision rat8
      parameter (rat8=435.429806239885625343750049532d0)
      double precision rat9
      parameter (rat9=286.420523879038022916141630568d0)
      double precision rat10
      parameter (rat10=95.9662816269858252140958521752d0)
      double precision rat11
      parameter (rat11=8.04747990597693169696422146459d0)
      double precision rat12
      parameter (rat12=-164.434565751861567598724840899d0)
      double precision rat13
      parameter (rat13=-438.781925229307271208716937993d0)
      double precision rat14
      parameter (rat14=-853.426106754471986388606981721d0)
      double precision rat15
      parameter (rat15=-1231.23822825176799461875159742d0)
      double precision rat16
      parameter (rat16=-1327.17649575071730372933774927d0)
      double precision rat17
      parameter (rat17=-1066.54181428918221493125326837d0)
      double precision rat18
      parameter (rat18=-630.417132838341180987253756019d0)
      double precision rat19
      parameter (rat19=-266.758620477617877893142865087d0)
      double precision rat20
      parameter (rat20=-77.0658293934652908408628391218d0)
      double precision rat21
      parameter (rat21=-13.9997109931675683537251135976d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (70101*(-1 + 
     -       x**10*(rat8 - 
     -          (7953340*zeta2)/70101d0) + 
     -       x**9*(rat7 - 
     -          (7016600*zeta2)/70101d0) + 
     -       x**11*(rat9 - 
     -          (1823680*zeta2)/23367d0) + 
     -       x**8*(rat6 - 
     -          (1244140*zeta2)/23367d0) + 
     -       x**5*(rat3 - 
     -          (3370520*zeta2)/70101d0) + 
     -       x**6*(rat4 - 
     -          (2830660*zeta2)/70101d0) + 
     -       x**12*(rat10 - 
     -          (2257100*zeta2)/70101d0) + 
     -       x**7*(rat5 - 
     -          (1836080*zeta2)/70101d0) + 
     -       x**4*(rat2 - 
     -          (1726820*zeta2)/70101d0) + 
     -       x**13*
     -        (rat11 - (56800*zeta2)/7789d0) + 
     -       x**3*(rat1 - (57200*zeta2)/23367d0) + 
     -       x*(1.78128700018544671260039086461d0 - 
     -          (25600*zeta2)/23367d0) + 
     -       x**14*(1.80153259027847122168173223080d0 + 
     -          (13300*zeta2)/23367d0) + 
     -       x**2*(14.0412167206364150772932388031d0 + 
     -          (9600*zeta2)/7789d0)))/
     -   (5d0*x**7*y**7) + 
     -  (1870198*(-1 - 7*x - 
     -       (7238079*x**2)/170018d0 + 
     -       rat12*x**3 + rat13*x**4 + 
     -       rat14*x**5 + rat15*x**6 + 
     -       rat16*x**7 + rat17*x**8 + 
     -       rat18*x**9 + rat19*x**10 + 
     -       rat20*x**11 + rat21*x**12 - 
     -       (2567027*x**13)/1.870198d6 - 
     -       (48546*x**14)/935099d0)*lins(1))/
     -   (15d0*x**7*y**7) - 
     -  (2*(16590 + 99540*x + 592305*x**2 + 
     -       2049075*x**3 + 4734812*x**4 + 
     -       7739738*x**5 + 9119068*x**6 + 
     -       7779296*x**7 + 4735675*x**8 + 
     -       1984871*x**9 + 532933*x**10 + 
     -       78863*x**11 + 4306*x**12)*lins(1)**2)
     -    /(x**7*y**6) + 
     -  (8*(17408 + 43088*x + 36693*x**2 + 
     -       10110*x**3 - 210*x**4)*lins(1)*
     -     lins(2))/(x**3*y**7)
      return
      end


      ! -+++: (-m2/s)^7*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_7_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-8.22596780443948412698412698413d0)
      double precision rat2
      parameter (rat2=6.26993471714134149029982363316d0)
      double precision rat3
      parameter (rat3=59.3733387704908352229780801209d0)
      double precision rat4
      parameter (rat4=47.7663152791679461136306374402d0)
      double precision rat5
      parameter (rat5=-174.122555714994331065759637188d0)
      double precision rat6
      parameter (rat6=-442.49032846479158793146888385d0)
      double precision rat7
      parameter (rat7=-424.094314098939633723859914336d0)
      double precision rat8
      parameter (rat8=-183.316319698796040406903502142d0)
      double precision rat9
      parameter (rat9=-23.240879373740236835474930713d0)
      double precision rat10
      parameter (rat10=8.54093240712445279037540942303d0)
      double precision rat11
      parameter (rat11=4.51310947378806846812799193752d0)
      double precision rat12
      parameter (rat12=1.00986027934756078357268833459d0)
      double precision rat13
      parameter (rat13=4.1448192d9)
      double precision rat14
      parameter (rat14=-0.0000185420901136317625413217327094d0)
      double precision rat15
      parameter (rat15=-7.70118030721340028534899664622d-6)
      double precision rat16
      parameter (rat16=-0.000129794630795422337789252128966d0)
      double precision rat17
      parameter (rat17=-0.0000539082621504938019974429765236d0)
      double precision rat18
      parameter (rat18=-0.00100339718048229676432904211814d0)
      double precision rat19
      parameter (rat19=-0.000807660802188910917996133582859d0)
      double precision rat20
      parameter (rat20=-0.00433305288255329019471397503231d0)
      double precision rat21
      parameter (rat21=-0.0137037832675548308596910572119d0)
      double precision rat22
      parameter (rat22=-0.0125609050377878967555448498212d0)
      double precision rat23
      parameter (rat23=-0.0261783124661585560434899870502d0)
      double precision rat24
      parameter (rat24=-0.0537232311604810168800607756305d0)
      double precision rat25
      parameter (rat25=-0.0401805068379768598296827465424d0)
      double precision rat26
      parameter (rat26=-0.0674483963015805369749300524375d0)
      double precision rat27
      parameter (rat27=-0.0460126964905436111128267736691d0)
      double precision rat28
      parameter (rat28=-0.0634617268709814893735292482722d0)
      double precision rat29
      parameter (rat29=-0.0394016173585783642598666038048d0)
      double precision rat30
      parameter (rat30=-0.0446188321073208693879819896607d0)
      double precision rat31
      parameter (rat31=-0.0250139346697358154166692401605d0)
      double precision rat32
      parameter (rat32=-0.0230775441302723168238556702304d0)
      double precision rat33
      parameter (rat33=-0.0115106715942854368385691923278d0)
      double precision rat34
      parameter (rat34=-0.00849760973892419722433248716856d0)
      double precision rat35
      parameter (rat35=-0.00367588023247056094616731278519d0)
      double precision rat36
      parameter (rat36=-0.00213679959791732290759510089125d0)
      double precision rat37
      parameter (rat37=-0.000750312341193984475312645187944d0)
      double precision rat38
      parameter (rat38=-0.000348089489645290197459035125103d0)
      double precision rat39
      parameter (rat39=-0.0000822810681612146341898607087882d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-83160*IPi*(4 + 2*x - 2*x**11 - 
     -       2*x**12 + x**13)*zeta2)/(x**6*y**7) 
     -   + (43008*(zeta2 + 
     -       x**10*(rat8 - 
     -          (8921063*zeta2)/32256d0 - 
     -          (67937*zeta3)/672d0) + 
     -       x**9*(rat7 - 
     -          (5040341*zeta2)/20160d0 - 
     -          (14935*zeta3)/168d0) + 
     -       x**11*(rat9 - (738049*zeta2)/4032d0 - 
     -          (3971*zeta3)/56d0) + 
     -       x**8*(rat6 - (690859*zeta2)/5120d0 - 
     -          (21649*zeta3)/448d0) + 
     -       x**12*(rat10 - (32383*zeta2)/512d0 - 
     -          (18505*zeta3)/672d0) + 
     -       x**7*(rat5 - (93295*zeta2)/1792d0 - 
     -          (2071*zeta3)/168d0) + 
     -       x**13*(rat11 - 
     -          (362141*zeta2)/53760d0 - 
     -          (1005*zeta3)/224d0) + 
     -       x**14*(rat12 + 
     -          (42547*zeta2)/64512d0 - 
     -          (45*zeta3)/32d0) + 
     -       x**2*(-0.790085565476190476190476190476d0 - 
     -          (799*zeta2)/224d0 - 
     -          (765*zeta3)/1792d0) + 
     -       x**3*(rat1 + (351*zeta2)/448d0 + 
     -          (715*zeta3)/336d0) + 
     -       x*(0.767857142857142857142857142857d0 + 
     -          (5*zeta2)/14d0 + (6025*zeta3)/2688d0
     -          ) + x**6*
     -        (rat4 - (425301*zeta2)/8960d0 + 
     -          (571*zeta3)/96d0) + 
     -       x**4*(rat2 - (227993*zeta2)/16128d0 + 
     -          (12823*zeta3)/1344d0) + 
     -       x**5*(rat3 - 
     -          (1102679*zeta2)/23040d0 + 
     -          (8339*zeta3)/672d0)))/(x**7*y**7) 
     -   + (rat13*(rat14 - 
     -       (17*x**14)/5.828652d6 + 
     -       x**5*(rat23 - 
     -          (27465*zeta2)/863504d0) + 
     -       x**3*(rat20 - 
     -          (22371*zeta2)/5.3969d6) + 
     -       rat15*zeta2 + 
     -       x*(rat16 + rat17*zeta2) + 
     -       x**2*(rat18 + rat19*zeta2) + 
     -       x**4*(rat22 + rat21*zeta2) + 
     -       x**6*(rat25 + rat24*zeta2) + 
     -       x**7*(rat27 + rat26*zeta2) + 
     -       x**8*(rat29 + rat28*zeta2) + 
     -       x**9*(rat31 + rat30*zeta2) + 
     -       x**10*(rat33 + rat32*zeta2) + 
     -       x**11*(rat35 + rat34*zeta2) + 
     -       x**12*(rat37 + rat36*zeta2) + 
     -       x**13*(rat39 + rat38*zeta2))*lins(1))
     -    /(x**7*y**7) + 
     -  ((-4620 - 471084*y + 2037270*y**2 + 
     -       4760680*y**3 + 5087715*y**4 + 
     -       3002038*y**5 + 1291546*y**6 + 
     -       573780*y**7 + 1291546*y**8 + 
     -       3002038*y**9 + 5087715*y**10 + 
     -       4760680*y**11 + 2037270*y**12 - 
     -       471084*y**13 - 4620*y**14 - 
     -       207900*IPi*
     -        (-1 + 2*y + 2*y**2 - 2*y**12 - 
     -          2*y**13 + y**14))*lins(1)**2)/
     -   (15d0*x**7*y**7) - 
     -  (28*(658 + 4606*x + 31735*x**2 + 
     -       130532*x**3 + 360491*x**4 + 
     -       715688*x**5 + 1052139*x**6 + 
     -       1162092*x**7 + 967721*x**8 + 
     -       603476*x**9 + 276829*x**10 + 
     -       90392*x**11 + 19857*x**12 + 
     -       2644*x**13 + 165*x**14)*lins(1)**3)/
     -   (x**7*y**7) + 
     -  ((18432*(-1 - (19*x)/12d0 + 
     -          (949*x**2)/192d0 - 
     -          (5255*x**3)/1536d0 + 
     -          (214961*x**4)/18432d0 + 
     -          (1297093*x**5)/23040d0 + 
     -          (965167*x**6)/18432d0 + 
     -          (101597*x**7)/6912d0 - 
     -          (1669*x**8)/6912d0))/(x**7*y**7) + 
     -     (13860*IPi*(x - y))/(x**7*y**7))*
     -   lins(1)*lins(2) + 
     -  (4*(4222 - 9761*y - 14244*y**2 - 
     -       2426*y**3 + 7840*y**4 + 7674*y**5 + 
     -       2940*y**6 + 420*y**7 - 6930*y**11 - 
     -       6930*y**12 + 3465*y**13)*lins(1)**2*
     -     lins(2))/(x**7*y**6) + 
     -  (27720*(-1 + 2*y + 2*y**2 - 2*y**12 - 
     -       2*y**13 + y**14)*lins(1)*lins(3))/
     -   (x**7*y**7) - 
     -  (27720*(-1 + 2*y + 2*y**2 - 2*y**12 - 
     -       2*y**13 + y**14)*lins(4))/(x**7*y**7)
      return
      end


      ! -+++: (-m2/s)^7*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmppp_HE_7_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=7.3358573840913318452380952381d0)
      double precision rat2
      parameter (rat2=2.32417440787825004133597883598d0)
      double precision rat3
      parameter (rat3=17.658055671812996031746031746d0)
      double precision rat4
      parameter (rat4=-20.0724032061585459043074901918d0)
      double precision rat5
      parameter (rat5=-12.3856143430679563492063492063d0)
      double precision rat6
      parameter (rat6=-21.2580256181003010496526652989d0)
      double precision rat7
      parameter (rat7=-14.7710622054811507936507936508d0)
      double precision rat8
      parameter (rat8=55.0786632719494047619047619048d0)
      double precision rat9
      parameter (rat9=27.5415607856988022970431558867d0)
      double precision rat10
      parameter (rat10=110.338068343874007936507936508d0)
      double precision rat11
      parameter (rat11=92.2514405269751181309163877191d0)
      double precision rat12
      parameter (rat12=283.126790771484375d0)
      double precision rat13
      parameter (rat13=107.114503292984092818675089083d0)
      double precision rat14
      parameter (rat14=300.988402622767857142857142857d0)
      double precision rat15
      parameter (rat15=71.9653429276038292301047403088d0)
      double precision rat16
      parameter (rat16=167.273927137586805555555555556d0)
      double precision rat17
      parameter (rat17=34.0102632246981805701778065724d0)
      double precision rat18
      parameter (rat18=50.7137994481646825396825396825d0)
      double precision rat19
      parameter (rat19=10.4102266542983424975479609833d0)
      double precision rat20
      parameter (rat20=11.3882789636036706349206349206d0)
      double precision rat21
      parameter (rat21=0.859821375302366006753230392686d0)
      double precision rat22
      parameter (rat22=3.28444983103918650793650793651d0)
      double precision rat23
      parameter (rat23=-0.580363130115327380952380952381d0)
      double precision rat24
      parameter (rat24=-0.307635286706028465867166972609d0)
      double precision rat25
      parameter (rat25=-60.3577076099537037037037037037d0)
      double precision rat26
      parameter (rat26=1.4259247591d10)
      double precision rat27
      parameter (rat27=-4.44577462792995649297253577322d-6)
      double precision rat28
      parameter (rat28=-3.80562389333809929588264018897d-6)
      double precision rat29
      parameter (rat29=0.0000134077200623593548204629109171d0)
      double precision rat30
      parameter (rat30=-0.0000311204223955096954508077504125d0)
      double precision rat31
      parameter (rat31=-0.0000266393672533666950711784813228d0)
      double precision rat32
      parameter (rat32=0.0000966121102258936153148110379845d0)
      double precision rat33
      parameter (rat33=-0.000224249541424021503501315656013d0)
      double precision rat34
      parameter (rat34=-0.000137337944518926302609533978148d0)
      double precision rat35
      parameter (rat35=0.000588707079137777487799566464516d0)
      double precision rat36
      parameter (rat36=-0.000999185474250361985082573678881d0)
      double precision rat37
      parameter (rat37=-0.000419462175971931774796703113523d0)
      double precision rat38
      parameter (rat38=0.00227639724977407470278913400207d0)
      double precision rat39
      parameter (rat39=-0.00316076533344673492223301325988d0)
      double precision rat40
      parameter (rat40=-0.000813033150776556749792718691886d0)
      double precision rat41
      parameter (rat41=0.00584376415853764103421829699542d0)
      double precision rat42
      parameter (rat42=-0.00727953140614392931377122804787d0)
      double precision rat43
      parameter (rat43=-0.000961799207899723554904732970301d0)
      double precision rat44
      parameter (rat44=0.0106733261365108727916750569031d0)
      double precision rat45
      parameter (rat45=-0.0124630393012812742689778948613d0)
      double precision rat46
      parameter (rat46=-0.000563724821934847323837008573313d0)
      double precision rat47
      parameter (rat47=0.0142899096673662632105705471371d0)
      double precision rat48
      parameter (rat48=-0.0160776005515208066305700865317d0)
      double precision rat49
      parameter (rat49=0.000173303567555670068306774510946d0)
      double precision rat50
      parameter (rat50=0.0141621340615096133510990103124d0)
      double precision rat51
      parameter (rat51=-0.0157077741657773935298893242517d0)
      double precision rat52
      parameter (rat52=0.000652219126286267687655813675345d0)
      double precision rat53
      parameter (rat53=0.0103410987893267165866409704029d0)
      double precision rat54
      parameter (rat54=-0.0115775411112293098831570740765d0)
      double precision rat55
      parameter (rat55=0.000589556034159002481217933518512d0)
      double precision rat56
      parameter (rat56=0.00544263892640336439193539773637d0)
      double precision rat57
      parameter (rat57=-0.00633465079347374007362985458849d0)
      double precision rat58
      parameter (rat58=0.000268748966173789926539925338933d0)
      double precision rat59
      parameter (rat59=0.00197196828377871175713425481259d0)
      double precision rat60
      parameter (rat60=-0.00248987138393909197486585204588d0)
      double precision rat61
      parameter (rat61=0.000047924304087389880602965151614d0)
      double precision rat62
      parameter (rat62=0.000448705863276990349020442925837d0)
      double precision rat63
      parameter (rat63=-0.000672508686413387256448730995786d0)
      double precision rat64
      parameter (rat64=-0.0000102553051523319880307553010066d0)
      double precision rat65
      parameter (rat65=0.0000519073671507861539873306769626d0)
      double precision rat66
      parameter (rat66=-0.000116424151373023171458023391299d0)
      double precision rat67
      parameter (rat67=-6.23100408543211154267941268366d-6)
      double precision rat68
      parameter (rat68=1.33751797759915900460220853739d-6)
      double precision rat69
      parameter (rat69=-8.23000669745528753426458426761d-7)
      double precision rat70
      parameter (rat70=-8.64000707006168148946057528345d-8)
      double precision rat71
      parameter (rat71=10.1234928075781167366476525007d0)
      double precision rat72
      parameter (rat72=34.918412206454977259254339682d0)
      double precision rat73
      parameter (rat73=87.9136358934813142158562893105d0)
      double precision rat74
      parameter (rat74=1422.60078368864979743640831507d0)
      double precision rat75
      parameter (rat75=166.824066079761105330607887558d0)
      double precision rat76
      parameter (rat76=241.094306321164759100602894187d0)
      double precision rat77
      parameter (rat77=266.378794211625452908577036889d0)
      double precision rat78
      parameter (rat78=224.25347745849721674919257439d0)
      double precision rat79
      parameter (rat79=5070.72072790064421863585043501d0)
      double precision rat80
      parameter (rat80=142.207593207539744034397683863d0)
      double precision rat81
      parameter (rat81=3402.92541674968453211131035399d0)
      double precision rat82
      parameter (rat82=66.1532896105908658209913440039d0)
      double precision rat83
      parameter (rat83=1685.4785149764229262137211928d0)
      double precision rat84
      parameter (rat84=21.4347443664691456416235588714d0)
      double precision rat85
      parameter (rat85=4.32996222994595844433129416858d0)
      double precision rat86
      parameter (rat86=0.401968888593714256196822445079d0)
      double precision rat87
      parameter (rat87=24.1928895323450865864579544555d0)
      double precision rat88
      parameter (rat88=65.9644476617254329322897722776d0)
      double precision rat89
      parameter (rat89=127.800683069463922531510036168d0)
      double precision rat90
      parameter (rat90=181.416046307503092532301511006d0)
      double precision rat91
      parameter (rat91=191.235228989677923098881886165d0)
      double precision rat92
      parameter (rat92=149.80020507201976374920741354d0)
      double precision rat93
      parameter (rat93=86.1870995381877716248266981359d0)
      double precision rat94
      parameter (rat94=35.4303493249786790907465919894d0)
      double precision rat95
      parameter (rat95=9.89656957908656689340790389895d0)
      double precision rat96
      parameter (rat96=-36.8296985785119420126400341102d0)
      double precision rat97
      parameter (rat97=-129.978191471071652075840204661d0)
      double precision rat98
      parameter (rat98=-329.220547964046736491737213828d0)
      double precision rat99
      parameter (rat99=-621.46931800207687176348419308d0)
      double precision rat100
      parameter (rat100=-889.339230182035746951743678264d0)
      double precision rat101
      parameter (rat101=-972.301118897469930060311805847d0)
      double precision rat102
      parameter (rat102=-811.991748904828382997159774708d0)
      double precision rat103
      parameter (rat103=-513.632360000275082352779363322d0)
      double precision rat104
      parameter (rat104=-241.973503723927350750630970147d0)
      double precision rat105
      parameter (rat105=-82.5191299145181588738128477213d0)
      double precision rat106
      parameter (rat106=-19.4526511061749111140147581682d0)
      double precision rat107
      parameter (rat107=1943.1779602262360883050538223d0)
      double precision rat108
      parameter (rat108=3035.995840175150519978106185d0)
      double precision rat109
      parameter (rat109=3151.97350848385331143951833607d0)
      double precision rat110
      parameter (rat110=1028.14106914796569968983762087d0)
      double precision rat111
      parameter (rat111=-0.517010906495033479160463287447d0)
      double precision rat112
      parameter (rat112=1190.40266611695183123754552326d0)
      double precision rat113
      parameter (rat113=0.138895655344068042480740893439d0)
      double precision rat114
      parameter (rat114=1729.52045969903112760255617398d0)
      double precision rat115
      parameter (rat115=0.0473627526206891286256365621445d0)
      double precision rat116
      parameter (rat116=1885.54782519068233353947639662d0)
      double precision rat117
      parameter (rat117=0.017386544965910045275124640204d0)
      double precision rat118
      parameter (rat118=685.991221325131359733435858003d0)
      double precision rat119
      parameter (rat119=1287.77616301422529796232218378d0)
      double precision rat120
      parameter (rat120=1013.37967448417275406894784057d0)
      double precision rat121
      parameter (rat121=289.537049852620786876842240164d0)
      double precision rat122
      parameter (rat122=284.948138848136026784034909529d0)
      double precision rat123
      parameter (rat123=1166.20441635631794756047097769d0)
      double precision rat124
      parameter (rat124=3322.16306944287702667118082985d0)
      double precision rat125
      parameter (rat125=6814.33270323138848136026784035d0)
      double precision rat126
      parameter (rat126=10380.7961385095737877590941579d0)
      double precision rat127
      parameter (rat127=11927.2218711206410111725538878d0)
      double precision rat128
      parameter (rat128=10381.6917202723545122822856713d0)
      double precision rat129
      parameter (rat129=6814.12256893503366813376970244d0)
      double precision rat130
      parameter (rat130=3321.62621318135650603769326261d0)
      double precision rat131
      parameter (rat131=1167.9997366738140917127487492d0)
      double precision rat132
      parameter (rat132=281.288139976676823533837414889d0)
      double precision rat133
      parameter (rat133=10800.0763478945297127115308933d0)
      double precision rat134
      parameter (rat134=22668.8437622983077528532073987d0)
      double precision rat135
      parameter (rat135=1426.72455163883735312306740878d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (110592*IPi*(zeta2 - 
     -       (8728493*x**8*zeta2)/276480d0 + 
     -       rat25*x**9*zeta2 - 
     -       (2524463*x**10*zeta2)/36864d0 - 
     -       (645683*x**11*zeta2)/13824d0 - 
     -       (1862525*x**12*zeta2)/110592d0 - 
     -       (190753*x**13*zeta2)/92160d0 - 
     -       (2337*x**14*zeta2)/2048d0 + 
     -       x**5*((-56567*zeta2)/320d0 - 
     -          (1525*zeta3)/192d0) + 
     -       x**6*((-7877431*zeta2)/55296d0 - 
     -          (1715*zeta3)/288d0) + 
     -       x**4*((-182825*zeta2)/2048d0 - 
     -          (5725*zeta3)/1152d0) + 
     -       x**7*((-107401*zeta2)/2160d0 - 
     -          (245*zeta3)/144d0) + 
     -       x**3*((-844661*zeta2)/69120d0 - 
     -          (715*zeta3)/864d0) + 
     -       x*((-269*zeta2)/144d0 + 
     -          (515*zeta3)/1728d0) + 
     -       x**2*((-469*zeta2)/576d0 + 
     -          (865*zeta3)/1152d0)))/(x**7*y**7) 
     -   + (57344*(zeta3 + 
     -       x**10*(rat15 + rat16*zeta2 - 
     -          (2446427*zeta3)/21504d0 - 
     -          (2331591*zeta4)/28672d0) + 
     -       x**9*(rat13 + rat14*zeta2 - 
     -          (754033*zeta3)/7680d0 - 
     -          (984303*zeta4)/14336d0) + 
     -       x**11*(rat17 + rat18*zeta2 - 
     -          (546797*zeta3)/6720d0 - 
     -          (211257*zeta4)/3584d0) + 
     -       x**8*(rat11 + rat12*zeta2 - 
     -          (5598683*zeta3)/107520d0 - 
     -          (1004301*zeta4)/28672d0) + 
     -       x**12*(rat19 + rat20*zeta2 - 
     -          (2849311*zeta3)/107520d0 - 
     -          (42723*zeta4)/2048d0) + 
     -       x**7*(rat9 + rat10*zeta2 + 
     -          (1279*zeta3)/768d0 - 
     -          (102933*zeta4)/14336d0) + 
     -       x**14*(rat24 + rat23*zeta2 - 
     -          (5231*zeta3)/3584d0 - 
     -          (4707*zeta4)/2048d0) + 
     -       x**13*(rat21 + rat22*zeta2 + 
     -          (4253*zeta3)/13440d0 - 
     -          (177*zeta4)/512d0) + 
     -       x**3*(rat1 + 
     -          (3962453*zeta2)/368640d0 + 
     -          (1937381*zeta3)/215040d0 + 
     -          (3575*zeta4)/7168d0) + 
     -       x**4*(rat2 + rat3*zeta2 + 
     -          (1262213*zeta3)/24576d0 + 
     -          (242547*zeta4)/28672d0) + 
     -       x**6*(rat6 + rat7*zeta2 + 
     -          rat8*zeta3 + (10881*zeta4)/896d0) 
     -        + x**5*
     -        (rat4 + rat5*zeta2 + 
     -          (1398409*zeta3)/17920d0 + 
     -          (37503*zeta4)/2048d0) + 
     -       x**2*(-0.289316328745039682539682539683d0 - 
     -          (46111*zeta2)/21504d0 - 
     -          (1189*zeta3)/256d0 + 
     -          (1315005*zeta4)/57344d0) + 
     -       x*(-1.078125d0 - (129*zeta2)/224d0 + 
     -          (267*zeta3)/64d0 + 
     -          (1335805*zeta4)/28672d0)))/
     -   (x**7*y**7) + 
     -  ((-36*IPi*(5621 - 10466*y - 15614*y**2 - 
     -          21636*y**3 - 40850*y**4 - 
     -          42800*y**5 - 26460*y**6 - 
     -          7560*y**7 - 770*y**12 - 
     -          1232*y**13 + 154*y**14)*zeta2)/
     -      (x**7*y**7) + 
     -     (rat26*(rat27 + rat28*zeta2 + 
     -          x**14*(rat69 + rat70*zeta2) + 
     -          rat29*zeta3 + 
     -          x*(rat30 + rat31*zeta2 + 
     -             rat32*zeta3) + 
     -          x**2*
     -           (rat34 + rat33*zeta2 + 
     -             rat35*zeta3) + 
     -          x**3*
     -           (rat37 + rat36*zeta2 + 
     -             rat38*zeta3) + 
     -          x**4*
     -           (rat40 + rat39*zeta2 + 
     -             rat41*zeta3) + 
     -          x**5*
     -           (rat43 + rat42*zeta2 + 
     -             rat44*zeta3) + 
     -          x**6*
     -           (rat46 + rat45*zeta2 + 
     -             rat47*zeta3) + 
     -          x**7*
     -           (rat49 + rat48*zeta2 + 
     -             rat50*zeta3) + 
     -          x**8*
     -           (rat52 + rat51*zeta2 + 
     -             rat53*zeta3) + 
     -          x**9*
     -           (rat55 + rat54*zeta2 + 
     -             rat56*zeta3) + 
     -          x**10*
     -           (rat58 + rat57*zeta2 + 
     -             rat59*zeta3) + 
     -          x**11*
     -           (rat61 + rat60*zeta2 + 
     -             rat62*zeta3) + 
     -          x**12*
     -           (rat64 + rat63*zeta2 + 
     -             rat65*zeta3) + 
     -          x**13*
     -           (rat67 + rat66*zeta2 + 
     -             rat68*zeta3)))/(x**7*y**7))*
     -   lins(1) + ((2248966*IPi*
     -        (1 + 6*x + rat87*x**2 + 
     -          rat88*x**3 + rat89*x**4 + 
     -          rat90*x**5 + rat91*x**6 + 
     -          rat92*x**7 + rat93*x**8 + 
     -          rat94*x**9 + rat95*x**10 + 
     -          (3826441*x**11)/2.248966d6 + 
     -          (315495*x**12)/2.248966d6)*
     -        (-1 + y))/(15d0*x**6*y**7) + 
     -     (90342*(0.283764226802348606160786542005d0 + zeta2 + 
     -          x**14*
     -           (-0.00388572806053047801071975874393d0 + 
     -             (286*zeta2)/239d0) + 
     -          x*(1.98634958761644024312550579403d0 + 
     -             (27910*zeta2)/2151d0) + 
     -          x**13*
     -           (rat86 + (102736*zeta2)/5019d0) + 
     -          x**2*
     -           (rat71 + (1591460*zeta2)/15057d0) 
     -           + x**12*
     -           (rat85 + (237886*zeta2)/1673d0) + 
     -          x**3*
     -           (rat72 + (7163804*zeta2)/15057d0) 
     -           + x**11*
     -           (rat84 + (8978744*zeta2)/15057d0) 
     -           + x**5*
     -           (rat75 + (5081240*zeta2)/1673d0) 
     -           + x**6*
     -           (rat76 + (8013382*zeta2)/1673d0) 
     -           + x**7*
     -           (rat77 + (1355768*zeta2)/239d0) + 
     -          x**4*(rat73 + rat74*zeta2) + 
     -          x**8*(rat78 + rat79*zeta2) + 
     -          x**9*(rat80 + rat81*zeta2) + 
     -          x**10*(rat82 + rat83*zeta2)))/
     -      (x**7*y**7))*lins(1)**2 + 
     -  ((290822*(-1 - 7*x + rat96*x**2 + 
     -          rat97*x**3 + rat98*x**4 + 
     -          rat99*x**5 + rat100*x**6 + 
     -          rat101*x**7 + rat102*x**8 + 
     -          rat103*x**9 + rat104*x**10 + 
     -          rat105*x**11 + rat106*x**12 - 
     -          (4227479*x**13)/1.45411d6 - 
     -          (63099*x**14)/290822d0))/
     -      (9d0*x**7*y**7) + 
     -     (308*IPi*(-37 + 56*y + 65*y**2 - 
     -          65*y**12 - 56*y**13 + 37*y**14))/
     -      (x**7*y**7))*lins(1)**3 + 
     -  (9135*(1 + (76798*x)/9135d0 + 
     -       (1904764*x**2)/27405d0 + 
     -       (2833276*x**3)/9135d0 + 
     -       (8388668*x**4)/9135d0 + rat107*x**5 + 
     -       rat108*x**6 + (929408*x**7)/261d0 + 
     -       rat109*x**8 + (6378752*x**9)/3045d0 + 
     -       rat110*x**10 + 
     -       (1098712*x**11)/3045d0 + 
     -       (156698*x**12)/1827d0 + 
     -       (343432*x**13)/27405d0 + 
     -       (814*x**14)/1305d0)*lins(1)**4)/
     -   (2d0*x**7*y**7) + 
     -  ((2601*IPi*(1 + (475979*x)/39015d0 + 
     -          (783*x**2)/34d0 + 
     -          (194912*x**3)/1445d0 + 
     -          rat118*x**4 + rat119*x**5 + 
     -          rat120*x**6 + rat121*x**7))/
     -      (x**7*y**7) + 
     -     (465696*(-0.0301059059987631416202844774273d0 + 
     -          zeta2 + 946*x**9*zeta2 + 
     -          (2959*x**10*zeta2)/7d0 + 
     -          (928*x**11*zeta2)/7d0 + 
     -          (7195*x**12*zeta2)/252d0 + 
     -          (49*x**13*zeta2)/9d0 - 
     -          (29*x**14*zeta2)/252d0 + 
     -          x*(0.0429078196935339792482649625507d0 + 
     -             (526*zeta2)/63d0) + 
     -          x**2*
     -           (0.364943367919558395748871939348d0 + 
     -             (3440*zeta2)/63d0) + 
     -          x**3*(rat111 + (1552*zeta2)/7d0) + 
     -          x**4*
     -           (-1.18775643815326355008894691434d0 + 
     -             (8823082*zeta2)/14553d0) + 
     -          x**8*
     -           (rat117 + (714389*zeta2)/462d0) + 
     -          x**5*
     -           (-0.327468715881414294112706811120d0 + 
     -             rat112*zeta2) + 
     -          x**6*(rat113 + rat114*zeta2) + 
     -          x**7*(rat115 + rat116*zeta2)))/
     -      (x**7*y**7))*lins(1)*lins(2) + 
     -  ((-27720*IPi*
     -        (1 + (42587*x)/3465d0 + 
     -          (525097*x**2)/6930d0 + 
     -          (327892*x**3)/1155d0 + 
     -          (2515396*x**4)/3465d0 + 
     -          (4602148*x**5)/3465d0 + 
     -          (97832*x**6)/55d0 + 
     -          (19336*x**7)/11d0 + 
     -          (6358*x**8)/5d0 + 
     -          (9812*x**9)/15d0 + 
     -          (3344*x**10)/15d0 + 
     -          (608*x**11)/15d0 - (8*x**12)/3d0 - 
     -          (76*x**13)/15d0 + (2*x**14)/15d0))/
     -      (x**7*y**7) + 
     -     (35444*(-1 + (4533551*x)/106332d0 + 
     -          rat122*x**2 + rat123*x**3 + 
     -          rat124*x**4 + rat125*x**5 + 
     -          rat126*x**6 + rat127*x**7 + 
     -          rat128*x**8 + rat129*x**9 + 
     -          rat130*x**10 + rat131*x**11 + 
     -          rat132*x**12 + 
     -          (4457431*x**13)/106332d0 + 
     -          (105165*x**14)/35444d0))/
     -      (5d0*x**7*y**7))*lins(1)**2*lins(2) - 
     -  (1694*(1 + (300910*x)/2541d0 + 
     -       (2148514*x**2)/2541d0 + 
     -       (3109388*x**3)/847d0 + rat133*x**4 + 
     -       rat134*x**5 + (4247546*x**6)/121d0 + 
     -       (4937080*x**7)/121d0 + 
     -       (4331236*x**8)/121d0 + 23588*x**9 + 
     -       11486*x**10 + (44032*x**11)/11d0 + 
     -       (10400*x**12)/11d0 + 
     -       (1508*x**13)/11d0 + (74*x**14)/11d0)*
     -     lins(1)**3*lins(2))/(x**7*y**7) + 
     -  ((-6930 - 3465*x + 34816*x**3 + 
     -       86176*x**4 + 72462*x**5 + 
     -       19380*x**6 - 1260*x**7 - 
     -       46200*x**11 - 79464*x**12 + 
     -       6468*x**13)*lins(1)**2*lins(2)**2)/
     -   (x**6*y**7) - 
     -  (12*(231 + 2*x*y*
     -        (-305 - 2112*y - 8706*y**2 - 
     -          11719*y**3 - 9681*y**4 - 
     -          3549*y**5 + 7329*y**6 + 
     -          19131*y**7 + 23669*y**8 + 
     -          17181*y**9 + 4455*y**10 + 
     -          379*y**11 + 231*y**12))*zeta2*
     -     lins(3))/(x**7*y**7) + 
     -  (4497932*(1 + 6*x + rat87*x**2 + 
     -       rat88*x**3 + rat89*x**4 + 
     -       rat90*x**5 + rat91*x**6 + 
     -       rat92*x**7 + rat93*x**8 + 
     -       rat94*x**9 + rat95*x**10 + 
     -       (3826441*x**11)/2.248966d6 + 
     -       (315495*x**12)/2.248966d6)*(1 - y)*
     -     lins(1)*lins(3))/(15d0*x**6*y**7) + 
     -  (4*(28727 + 172362*x + 726687*x**2 + 
     -       2053450*x**3 + 4070073*x**4 + 
     -       5855574*x**5 + 6216033*x**6 + 
     -       4882098*x**7 + 2808033*x**8 + 
     -       1151830*x**9 + 319039*x**10 + 
     -       53278*x**11 + 3927*x**12)*(-1 + y)*
     -     lins(1)**2*lins(3))/(x**6*y**7) + 
     -  (64680*(1 + (76156*x)/8085d0 + 
     -       (526573*x**2)/8085d0 + 
     -       (713418*x**3)/2695d0 + 
     -       (5864531*x**4)/8085d0 + rat135*x**5 + 
     -       (798696*x**6)/385d0 + 
     -       (174204*x**7)/77d0 + 
     -       (64944*x**8)/35d0 + (5676*x**9)/5d0 + 
     -       (17754*x**10)/35d0 + 
     -       (5568*x**11)/35d0 + (222*x**12)/7d0 + 
     -       (12*x**13)/5d0 + (12*x**14)/35d0)*
     -     lins(1)*lins(2)*lins(3))/(x**7*y**7) + 
     -  (4497932*(1 + 6*x + rat87*x**2 + 
     -       rat88*x**3 + rat89*x**4 + 
     -       rat90*x**5 + rat91*x**6 + 
     -       rat92*x**7 + rat93*x**8 + 
     -       rat94*x**9 + rat95*x**10 + 
     -       (3826441*x**11)/2.248966d6 + 
     -       (315495*x**12)/2.248966d6)*(-1 + y)*
     -     lins(4))/(15d0*x**6*y**7) + 
     -  (8*(4916 + 29496*x + 113268*x**2 + 
     -       295960*x**3 + 592347*x**4 + 
     -       918084*x**5 + 1073118*x**6 + 
     -       924636*x**7 + 568035*x**8 + 
     -       236416*x**9 + 61852*x**10 + 
     -       8944*x**11 + 693*x**12)*(-1 + y)*
     -     lins(1)*lins(4))/(x**6*y**7) - 
     -  (8*(3927 - 8612*y - 17753*y**2 - 
     -       43272*y**3 - 81700*y**4 - 
     -       85600*y**5 - 52920*y**6 - 
     -       15120*y**7 - 9240*y**12 - 
     -       9240*y**13 + 4620*y**14)*lins(1)*
     -     lins(5))/(x**7*y**7) - 
     -  (24*(-1 + x)*
     -     (1771 - 456*x + 16616*x**2 + 
     -       72420*x**3 + 195285*x**4 + 
     -       337932*x**5 + 406308*x**6 + 
     -       337932*x**7 + 195285*x**8 + 
     -       72420*x**9 + 16616*x**10 - 
     -       456*x**11 + 1771*x**12)*lins(6))/
     -   (x**7*y**6) - 
     -  (12*(x - y)*(1771 - 21708*x*y + 
     -       116810*x**2*y**2 - 
     -       274820*x**3*y**3 + 
     -       301840*x**4*y**4 - 
     -       132440*x**5*y**5 + 13860*x**6*y**6)*
     -     lins(7))/(x**7*y**7)
      return
      end


      ! --++: (-m2/s)^0*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_0_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        2 - 48*IPi*zeta2 - 
     -  (2*(-3 - 18*x - 37*x**2 - 32*x**3 - 
     -       x**4 + 18*x**5 + 6*x**6)*zeta2)/
     -   (x**2*y**2) + 16*zeta3 + 
     -  18*(1 - 2*x*y)*zeta4 + 
     -  (4*(-1 - 2*x*y)*lins(1))/y - 
     -  16*IPi*x**2*zeta2*lins(1) + 
     -  4*(17 + 26*x)*zeta2*lins(1) + 
     -  4*IPi*(-1 + 2*x)*lins(1)**2 - 
     -  (2*(-2 - 4*x + x**2 + 6*x**3 + 2*x**4)*
     -     lins(1)**2)/y**2 + 
     -  48*x**2*zeta2*lins(1)**2 + 
     -  (8*IPi*x**2*lins(1)**3)/3d0 + 
     -  (2*(1 + 10*x)*lins(1)**3)/3d0 + 
     -  (4*x**2*lins(1)**4)/3d0 + 
     -  2*(-3 - 2*x*y)*lins(1)*lins(2) - 
     -  48*(1 - 2*x*y)*zeta2*lins(1)*lins(2) - 
     -  2*(1 + 10*x)*lins(1)**2*lins(2) - 
     -  8*IPi*(2 + 4*x + 3*x**2)*lins(1)**2*
     -   lins(2) - (16*x**2*lins(1)**3*lins(2))/
     -   3d0 + 8*(1 - 2*x*y)*lins(1)**2*
     -   lins(2)**2 + 16*x**2*zeta2*lins(3) - 
     -  8*(-1 + 2*x)*lins(1)*lins(3) + 
     -  16*(1 - 2*x*y)*lins(1)*lins(2)*lins(3) + 
     -  8*(-1 + 2*x)*lins(4) - 
     -  16*x**2*lins(1)*lins(4) - 
     -  16*x**2*lins(1)*lins(5) + 
     -  16*(1 - 2*x*y)*lins(6) + 8*(x - y)*lins(7)
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^4
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_1_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=13/(6d0*x*y)
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^3
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_1_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=2/(3d0*x*y) + (12*lins(1))/x
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_1_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (x - y)**2/(x*y) + 
     -  (6*(-1 - 3*x*y)*zeta2)/(x*y) - 
     -  (2*(7 + 4*x)*lins(1))/(x*y) - 
     -  (2*(4 + 3*x)*lins(1)**2)/x + 
     -  6*lins(1)*lins(2)
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_1_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        6/(x*y) - (4*(-22 - 39*x*y)*zeta2)/
     -   (x*y) + (24*zeta3)/(x*y) - 
     -  (4*(11 + 23*x)*lins(1))/(x*y) + 
     -  (8*(-5 + 6*x + 3*x**2)*zeta2*lins(1))/x - 
     -  (12*(-2 + 4*x + 5*x**2)*lins(1)**2)/
     -   (x*y) + 4*(2 + x)*lins(1)**3 + 
     -  (2*(1 - 30*x*y)*lins(1)*lins(2))/(x*y) + 
     -  12*y*lins(1)**2*lins(2)
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_1_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -14/(x*y) + (6*(7 - 38*x*y)*zeta2)/
     -   (x*y) - (12*IPi*(9 - 16*x*y)*zeta2)/
     -   (x*y) + (2*(35 - 12*x*y)*zeta3)/(x*y) - 
     -  (3*(-24 - 23*x*y)*zeta4)/(2d0*x*y) + 
     -  (4*(8 + 23*x)*lins(1))/(x*y) + 
     -  8*IPi*(5 + 9*x)*zeta2*lins(1) - 
     -  4*IPi*(29 + 60*x)*zeta2*lins(1) - 
     -  (12*(8 + 3*x - 8*x**2 + 10*x**3)*zeta2*
     -     lins(1))/(x*y) - 
     -  (2*IPi*(15 + 28*x + 16*x**2)*lins(1)**2)/
     -   (x*y) + (2*(25 + 13*x)*(x - y)*
     -     lins(1)**2)/(x*y) + 
     -  (2*(6 + 7*x + 30*x**2)*zeta2*lins(1)**2)/
     -   x - (8*IPi*(2 + 3*x)*lins(1)**3)/3d0 - 
     -  (2*(21 - 40*x - 26*x**2 + 30*x**3)*
     -     lins(1)**3)/(3d0*x*y) - 
     -  ((-2 + 15*x + 24*x**2)*lins(1)**4)/
     -   (6d0*x) - (4*(6 - 13*x*y)*lins(1)*
     -     lins(2))/(x*y) - 
     -  34*zeta2*lins(1)*lins(2) + 
     -  8*IPi*(1 + 3*x)*lins(1)**2*lins(2) + 
     -  (4*(10 + 12*x + 16*x**2 + 15*x**3)*
     -     lins(1)**2*lins(2))/(x*y) + 
     -  2*(1 + 6*x)*lins(1)**3*lins(2) + 
     -  (13*lins(1)**2*lins(2)**2)/2d0 + 
     -  (4*(6 + x + 6*x**2)*zeta2*lins(3))/x + 
     -  (4*(15 + 28*x + 16*x**2)*lins(1)*lins(3))/
     -   (x*y) + (4*(1 + 3*x + 6*x**2)*lins(1)**2*
     -     lins(3))/x - 
     -  12*(x - y)*lins(1)*lins(2)*lins(3) - 
     -  (4*(15 + 28*x + 16*x**2)*lins(4))/(x*y) - 
     -  (16*(3*x**2 - y)*lins(1)*lins(4))/x - 
     -  16*(1 + 3*x)*lins(1)*lins(5) - 
     -  (8*(-3 + 2*x)*lins(6))/x - 
     -  12*(x - y)*lins(7)
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^4
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_2_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(2*(19 + 10*x + x**2))/y**2
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^3
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_2_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-4*(-12 - 61*x - 95*x**2 + 28*x**3 + 
     -       80*x**4 + 32*x**5))/(3d0*x**2*y**2) - 
     -  (16*(3 + 7*y**2)*lins(1))/(3d0*x**2*y**2)
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_2_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(-24 + 573*x + 606*x**5 + 
     -      3*x**4*(505 + 72*zeta2) + 
     -      6*x**2*(538 + 78*zeta2) + 
     -      x**3*(3042 + 552*zeta2))/
     -   (3d0*x**2*y**2) - 
     -  (4*(35 + 104*x + 132*x**2 + 56*x**3)*
     -     lins(1))/(x**2*y**2) + 
     -  ((42 + 64*x + 4*x**2 - 48*x**3 - 24*x**4)*
     -     lins(1)**2)/(x**2*y**2) + 
     -  (4*(29 + 26*x + 6*x**2)*lins(1)*lins(2))/
     -   y**2
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_2_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*(-216*IPi*x*zeta2 + 
     -       6*(24 + 285*x + 322*x**2 + 
     -          156*x**3 + 48*x**4)*zeta2 + 
     -       3*(-48 + 116*x**2 + 138*x**3 + 
     -          52*x**4 + x*(11 + 32*y*zeta3))))/
     -   (3d0*x*y**2) + 
     -  (2*(-3*(-57 + 32*y + 34*y**2) + 
     -       6*(48 + 88*y + 98*y**2 + 76*y**3 + 
     -          36*y**4)*zeta2)*lins(1))/
     -   (3d0*x**2*y**2) + 
     -  (2*(-3 - 28*y + 4*(1 - 3*IPi)*y**2 + 
     -       24*y**4)*lins(1)**2)/(x**2*y**2) + 
     -  (4*(-9 - 16*x + 11*x**2 + 34*x**3 + 
     -       18*x**4)*lins(1)**3)/(3d0*x**2*y**2) 
     -   + (4*(-17 + y + 26*y**2 + 12*y**3)*
     -     lins(1)*lins(2))/(x*y**2) + 
     -  (4*(2 + 12*x + x**2 - 10*x**3 - 6*x**4)*
     -     lins(1)**2*lins(2))/(x**2*y**2) + 
     -  (48*lins(1)*lins(3))/x**2 - 
     -  (48*lins(4))/x**2
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_2_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(4320*IPi*(-12 - 54*x - 71*x**2 - 
     -         52*x**3 + 5*x**4 + 2*x**5)*zeta2 - 
     -      180*(-24 + 39*x + 1197*x**2 + 
     -         674*x**3 + 288*x**4 + 156*x**5)*
     -       zeta2 - 
     -      45*(x*(68 - 1376*zeta3) + 
     -         x**2*(531 - 688*zeta3) - 
     -         192*zeta3 + 
     -         4*x**3*(433 + 24*zeta3) + 
     -         5*x**4*(497 + 256*zeta3) + 
     -         x**5*(994 + 512*zeta3)) + 
     -      90*x**2*(-23 - 6*x + 54*x**2)*zeta4)/
     -   (90d0*x**2*y**2) + 
     -  ((-3 + 300*y + 24*y**3 + 
     -       6*(268 + 376*y - 442*y**2 - 
     -          564*y**3 - 72*y**4)*zeta2 + 
     -       12*IPi*(-27 - 108*y - 154*y**2 - 
     -          76*y**3 + 16*y**4)*zeta2 + 
     -       48*zeta3 + 6*y**2*(47 + 64*zeta3))*
     -     lins(1))/(3d0*x**2*y**2) - 
     -  ((-12*IPi*(-23 - 36*x - 6*x**2 + 
     -          12*x**3) - 
     -       3*(99 + 212*x + 68*x**2 + 
     -          68*x**3*(-1 + y)) + 
     -       6*(-3 + 100*x + 366*x**2 + 
     -          404*x**3 + 96*x**4)*zeta2)*
     -     lins(1)**2)/(3d0*x**2*y**2) - 
     -  (2*(39 + 52*x + 46*x**3 + 36*x**4 - 
     -       24*IPi*y**2 + x**2*(7 + 16*IPi*y**2))
     -      *lins(1)**3)/(3d0*x**2*y**2) + 
     -  ((19 + 28*x - 56*x**2 - 104*x**3 - 
     -       44*x**4)*lins(1)**4)/(6d0*x**2*y**2) 
     -   + (2*(3*(4 - 63*x - 391*x**2 - 
     -          206*x**3 + 34*x**4) + 
     -       6*IPi*(29*y + 62*x**2*y - 
     -          x*(29 + 62*y**2)) + 
     -       6*(96 + 192*x - 39*x**2 + 146*x**3 + 
     -          78*x**4)*zeta2)*lins(1)*lins(2))/
     -   (3d0*x**2*y**2) + 
     -  (2*(32 + 40*x - 33*x**2 - 26*x**3 + 
     -       12*x**4 - 
     -       4*IPi*(7 + 20*x + 18*x**2 + 
     -          12*x**3*y))*lins(1)**2*lins(2))/
     -   (x**2*y**2) + 
     -  (2*(-29 - 52*x + 6*x**2 + 36*x**3 + 
     -       8*x**4)*lins(1)**3*lins(2))/
     -   (3d0*x**2*y**2) - 
     -  ((-31 + 82*x + 46*x**2)*lins(1)**2*
     -     lins(2)**2)/y**2 - 
     -  (4*(25 + 68*x + 86*x**2 + 68*x**3 + 
     -       16*x**4)*zeta2*lins(3))/(x**2*y**2) 
     -   - (8*(-23 - 36*x - 6*x**2 + 12*x**3)*
     -     lins(1)*lins(3))/(x**2*y**2) + 
     -  (4*(3 + 12*y + 10*y**2 + 12*y**3)*
     -     lins(1)**2*lins(3))/(x**2*y**2) + 
     -  (4*(19 + 44*x + 18*x**2 - 52*x**3 - 
     -       32*x**4)*lins(1)*lins(2)*lins(3))/
     -   (x**2*y**2) - 
     -  (8*(5 + 12*y + 42*y**2 + 12*y**3)*
     -     lins(4))/(x**2*y**2) + 
     -  (16*(-3 - 12*y - 12*y**2 - 4*y**3 + 
     -       4*y**4)*lins(1)*lins(4))/(x**2*y**2) 
     -   + (16*(-1 + 4*x + 18*x**2 + 20*x**3 + 
     -       4*x**4)*lins(1)*lins(5))/(x**2*y**2) 
     -   - (64*(x - y + 2*x**2*y**2)*lins(6))/
     -   (x**2*y**2) - 
     -  (4*(x - y)*(-1 + 18*x*y)*lins(7))/
     -   (x**2*y**2)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^4
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_3_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (-258 - 292*y + 332*y**2 + 1132*y**3 + 
     -    910*y**4)/(3d0*x**2*y**3)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^3
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_3_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-4*(-48 - 896*x - 5845*x**2 - 
     -       12728*x**3 - 13277*x**4 - 
     -       4793*x**5 + 132*x**6 + 48*x**7))/
     -   (9d0*x**3*y**3) + 
     -  (8*(-24 + 6*y**3 - 38*y**4 + 17*y**5)*
     -     lins(1))/(3d0*x**3*y**3)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_3_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-12536 - 34402*y - 71399*y**2 - 
     -     136062*y**3 - 133413*y**4 - 
     -     27570*y**5 + 28098*y**6 + 8028*y**7 - 
     -     144*x*(-17 + 28*y + 120*y**2 + 
     -        192*y**3 + 141*y**4)*zeta2)/
     -   (18d0*x**3*y**3) - 
     -  (2*(387 + 1395*x + 2600*x**2 + 
     -       2344*x**3 + 999*x**4 + 161*x**5)*
     -     lins(1))/(x**3*y**3) + 
     -  (8*(24 + 82*x + 128*x**2 + 94*x**3 + 
     -       31*x**4 + 4*x**5)*lins(1)**2)/
     -   (x**3*y**3) + 
     -  (16*(12 + 100*x + 198*x**2 + 197*x**3 + 
     -       78*x**4)*lins(1)*lins(2))/(x**2*y**3)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_3_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (21456 - 45804*x**5 - 13272*x**6 - 
     -     23328*IPi*x**3*zeta2 + 
     -     144*(-171 - 1224*x - 2748*x**2 - 
     -        2565*x**3 - 880*x**4 + 63*x**5 + 
     -        18*x**6)*zeta2 + 
     -     x**3*(441213 - 98496*zeta3) + 
     -     x*(120313 - 43200*zeta3) - 
     -     5184*zeta3 - 
     -     36*x**4*(-3881 + 1008*zeta3) - 
     -     54*x**2*(-7109 + 1728*zeta3))/
     -   (27d0*x**2*y**3) + 
     -  (2*(2348 - 248*x - 10106*x**2 - 
     -       18054*x**3 - 12051*x**4 - 
     -       3105*x**5 + 
     -       72*(3 - 13*x + 8*x**2 + 44*x**3 + 
     -          95*x**4 + 9*x**5)*zeta2)*lins(1))/
     -   (9d0*x**3*y**3) - 
     -  (8*(1 + 28*y + 49*y**2 + 39*y**3 + 
     -       (-32 + 18*IPi)*y**4 + 16*y**5)*
     -     lins(1)**2)/(x**3*y**3) - 
     -  (32*(7 + 28*x + 41*x**2 + 27*x**3 + 
     -       7*x**4 + x**5)*lins(1)**3)/
     -   (3d0*x**3*y**3) + 
     -  (8*(89 + 668*x + 1410*x**2 + 1448*x**3 + 
     -       572*x**4)*lins(1)*lins(2))/
     -   (x**2*y**3) + 
     -  (16*(5 + 36*x + 54*x**2 + 38*x**3 + 
     -       12*x**4 + 2*x**5)*lins(1)**2*lins(2))
     -    /(x**3*y**3) + 
     -  (288*y*lins(1)*lins(3))/x**3 - 
     -  (288*y*lins(4))/x**3
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_3_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (550780 + 120*x*
     -      (-18938 - 33701*y - 44449*y**2 - 
     -        51880*y**3 - 22410*y**4 + 
     -        7920*y**5 + 3168*y**6)*zeta2 + 
     -     2160*IPi*(-837 - 2279*y - 2100*y**2 - 
     -        830*y**3 + 307*y**4 + 317*y**5 + 
     -        42*y**6 + 12*y**7)*zeta2 + 
     -     773280*zeta3 - 
     -     20*y**7*(24563 + 3456*zeta3) + 
     -     450*y**3*(33421 + 7168*zeta3) + 
     -     30*y**5*(136559 + 36288*zeta3) - 
     -     5*y**6*(342154 + 48384*zeta3) + 
     -     10*y*(218879 + 184752*zeta3) + 
     -     15*y**4*(993181 + 217920*zeta3) + 
     -     5*y**2*(1419205 + 444672*zeta3) + 
     -     6480*x*(29 + 247*y + 461*y**2 + 
     -        453*y**3 + 246*y**4)*zeta4)/
     -   (540d0*x**3*y**3) + 
     -  ((-11893*y - 11347*y**2 - 
     -       1296*IPi*
     -        (9 + 36*y + 72*y**2 + 108*y**3 + 
     -          94*y**4 + 36*y**5)*zeta2 - 
     -       36*(-1505 - 4639*y - 5896*y**2 - 
     -          3170*y**3 + 791*y**4 + 1181*y**5)*
     -        zeta2 + 18*y**3*(-767 + 48*zeta3) - 
     -       9*y**5*(515 + 96*zeta3) + 
     -       162*y**4*(-29 + 128*zeta3) + 
     -       2*(977 + 864*zeta3))*lins(1))/
     -   (27d0*x**3*y**3) - 
     -  ((-884 - 3941*x - 7781*x**2 - 7231*x**3 - 
     -       3135*x**4 - 408*x**5 - 
     -       2*IPi*(-507 - 1587*x - 2060*x**2 - 
     -          996*x**3 - 3*x**4 + 151*x**5) + 
     -       24*(-52 - 199*x - 164*x**2 + 
     -          288*x**3 + 491*x**4 + 227*x**5)*
     -        zeta2)*lins(1)**2)/(3d0*x**3*y**3) - 
     -  (2*(-202 - 560*y - 1088*y**2 - 
     -       1174*y**3 + (82 - 432*IPi)*y**4 + 
     -       87*y**5)*lins(1)**3)/(9d0*x**3*y**3) 
     -   + ((76 + 298*x + 408*x**2 + 184*x**3 - 
     -       62*x**4 - 42*x**5)*lins(1)**4)/
     -   (3d0*x**3*y**3) - 
     -  (2*(-189 - 2205*x - 4537*x**2 - 
     -       16500*x**3 - 20037*x**4 - 
     -       8505*x**5 - 
     -       6*IPi*(165*y + 28*x**3*y**2 - 
     -          4*x**2*y*(-4 + 7*y**2) - 
     -          x*(165 + 16*y**2)) + 
     -       72*(-144 - 564*x - 764*x**2 - 
     -          382*x**3 + 335*x**4 + 74*x**5)*
     -        zeta2)*lins(1)*lins(2))/
     -   (9d0*x**3*y**3) + 
     -  (2*(-52 + 88*y + 100*y**2 - 70*y**3 + 
     -       346*y**4 + 87*y**5 - 
     -       144*IPi*(3 + 4*x*y*(-3 - y + y**3)))*
     -     lins(1)**2*lins(2))/(3d0*x**3*y**3) + 
     -  (8*(-45 - 180*x - 264*x**2 - 148*x**3 + 
     -       6*x**4 + 20*x**5)*lins(1)**3*lins(2))
     -    /(3d0*x**3*y**3) + 
     -  (4*(12 + 100*x + 194*x**2 + 275*x**3 + 
     -       74*x**4)*lins(1)**2*lins(2)**2)/
     -   (x**2*y**3) - 
     -  (48*(4 + 19*x + 44*x**2 + 64*x**3 + 
     -       49*x**4 + 17*x**5)*zeta2*lins(3))/
     -   (x**3*y**3) - 
     -  (4*(-507 - 1587*x - 2060*x**2 - 
     -       996*x**3 - 3*x**4 + 151*x**5)*
     -     lins(1)*lins(3))/(3d0*x**3*y**3) - 
     -  (8*(23 + 95*x + 164*x**2 + 164*x**3 + 
     -       95*x**4 + 29*x**5)*lins(1)**2*lins(3)
     -     )/(x**3*y**3) + 
     -  (48*(9 + 36*x + 56*x**2 + 44*x**3 + 
     -       26*x**4 + 4*x**5)*lins(1)*lins(2)*
     -     lins(3))/(x**3*y**3) + 
     -  (4*(-507 - 1587*x - 2060*x**2 - 
     -       996*x**3 - 3*x**4 + 151*x**5)*lins(4)
     -     )/(3d0*x**3*y**3) + 
     -  (32*(5 + 23*x + 56*x**2 + 92*x**3 + 
     -       77*x**4 + 29*x**5)*lins(1)*lins(4))/
     -   (x**3*y**3) + 
     -  (192*(-1 + 4*x*y + 4*x**3*y**2)*lins(1)*
     -     lins(5))/(x**3*y**3) - 
     -  (48*(1 + 7*x + 20*x**2 + 28*x**3 + 
     -       11*x**4 + 5*x**5)*lins(6))/
     -   (x**3*y**3) + 
     -  (48*(x - y)*(-x + 4*x**2 + 12*x**3 + 
     -       6*x**4 + y)*lins(7))/(x**3*y**3)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^4
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_4_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (-2*(-408 + 144*x + 673*x**2 + 446*x**3 - 
     -      75*x**4 + 144*x**5))/(3d0*x**2*y**4)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^3
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_4_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-2*(-360 + 1472*x + 5024*x**2 + 
     -       24564*x**3 + 33731*x**4 + 
     -       16114*x**5 + 607*x**6 + 3728*x**7 + 
     -       180*x**8 + 64*x**9))/(9d0*x**4*y**4) 
     -   + (8*(-98 - 28*y - 24*y**2 + 25*y**4 + 
     -       94*y**5 - 115*y**6 + 80*y**7)*lins(1)
     -     )/(3d0*x**4*y**4)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_4_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-44256 - 119132*y - 133082*y**2 - 
     -     91536*y**3 + 72346*y**4 + 
     -     266916*y**5 + 263267*y**6 + 
     -     156244*y**7 + 71991*y**8 + 
     -     15806*y**9 - 
     -     72*x**2*(101 - 292*y + 149*y**2 + 
     -        490*y**3 + 316*y**4 + 96*y**5)*zeta2
     -     )/(18d0*x**4*y**4) - 
     -  (2*(5463 + 27534*x + 77424*x**2 + 
     -       119572*x**3 + 109210*x**4 + 
     -       58944*x**5 + 17332*x**6 + 2164*x**7)*
     -     lins(1))/(3d0*x**4*y**4) + 
     -  (2*(425 + 2246*x + 5748*x**2 + 
     -       7912*x**3 + 6282*x**4 + 2836*x**5 + 
     -       656*x**6 + 64*x**7)*lins(1)**2)/
     -   (x**4*y**4) + 
     -  (4*(143 - 180*y + 153*y**2 + 234*y**3 + 
     -       30*y**4)*lins(1)*lins(2))/(x**2*y**4)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_4_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-128736 - 624420*x**7 - 140040*x**8 - 
     -     466560*IPi*x**5*zeta2 + 
     -     72*(216 - 5292*x - 2208*x**2 + 
     -        1963*x**3 - 1338*x**4 + 4776*x**5 + 
     -        4816*x**6 + 432*x**7 + 96*x**8)*
     -      zeta2 + x*(477988 - 117504*zeta3) + 
     -     x**5*(8276037 - 58752*zeta3) + 
     -     16*x**2*(433657 + 2592*zeta3) + 
     -     12*x**6*(137629 + 3456*zeta3) + 
     -     18*x**4*(809915 + 14976*zeta3) + 
     -     3*x**3*(4769375 + 116352*zeta3))/
     -   (108d0*x**3*y**4) + 
     -  ((15905 + 11134*x - 103690*x**2 - 
     -       337296*x**3 - 448224*x**4 - 
     -       321888*x**5 - 119792*x**6 - 
     -       18704*x**7 + 
     -       72*(-61 - 598*x - 1170*x**2 - 
     -          1008*x**3 + 45*x**4 + 706*x**5 + 
     -          1028*x**6 + 96*x**7)*zeta2)*
     -     lins(1))/(9d0*x**4*y**4) - 
     -  ((27 + 3234*y + 6888*y**2 + 8808*y**3 + 
     -       7478*y**4 + 3276*y**5 + 
     -       (-4524 + 2160*IPi)*y**6 + 1568*y**7)*
     -     lins(1)**2)/(3d0*x**4*y**4) - 
     -  (4*(285 + 1726*x + 4324*x**2 + 
     -       5736*x**3 + 4187*x**4 + 1574*x**5 + 
     -       210*x**6)*lins(1)**3)/(3d0*x**4*y**4) 
     -   + (2*(-36 + 1562*x + 48*x**2 - 
     -       1303*x**3 - 414*x**4 + 158*x**5)*
     -     lins(1)*lins(2))/(x**3*y**4) + 
     -  (4*(120 + 1072*x + 2684*x**2 + 
     -       3600*x**3 + 2669*x**4 + 1034*x**5 + 
     -       150*x**6)*lins(1)**2*lins(2))/
     -   (x**4*y**4) + 
     -  (1440*y**2*lins(1)*lins(3))/x**4 - 
     -  (1440*y**2*lins(4))/x**4
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_4_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-52.4908966692386831275720164609d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-4*IPi*(-720 + 2620*x + 24106*x**2 + 
     -       49200*x**3 + 49751*x**4 + 
     -       19222*x**5 + 1931*x**6 + 556*x**7 + 
     -       36*x**8 + 8*x**9)*zeta2)/(x**4*y**4) 
     -   + (480*(x**9*
     -        (2.56427790637860082304526748971d0 + 
     -          (329*zeta2)/120d0 + (8*zeta3)/45d0) 
     -        + x**8*
     -        (11.6392505787037037037037037037d0 + 
     -          (987*zeta2)/80d0 + (4*zeta3)/5d0) - 
     -       zeta3 + 
     -       x*(1.49166666666666666666666666667d0 + 
     -          (62*zeta2)/15d0 + (727*zeta3)/180d0)
     -         + x**6*
     -        (rat1 - (3559*zeta2)/30d0 - 
     -          (43*zeta3)/180d0 - (561*zeta4)/80d0)
     -         + x**3*
     -        (-72.8064429012345679012345679012d0 - 
     -          (29783*zeta2)/240d0 + 
     -          (313*zeta3)/5d0 - (9*zeta4)/5d0) + 
     -       x**4*(-134.817463991769547325102880658d0 - 
     -          (564839*zeta2)/2160d0 + 
     -          (7741*zeta3)/120d0 + 
     -          (139*zeta4)/160d0) + 
     -       x**7*(-0.590798611111111111111111111111d0 - 
     -          (7153*zeta2)/360d0 + 
     -          (212*zeta3)/45d0 + (8*zeta4)/5d0) + 
     -       x**5*(-121.523225308641975308641975309d0 - 
     -          (275833*zeta2)/1080d0 + 
     -          (229*zeta3)/12d0 + (323*zeta4)/80d0)
     -         + x**2*
     -        (-6.00890239197530864197530864198d0 + 
     -          (23311*zeta2)/2880d0 + 
     -          (12773*zeta3)/360d0 + 
     -          (51*zeta4)/10d0)))/(x**4*y**4) - 
     -  ((245556*y**3 + 
     -       648*IPi*
     -        (159 + 714*y + 1584*y**2 + 
     -          2376*y**3 + 1980*y**4 + 
     -          792*y**5 + 80*y**6)*zeta2 + 
     -       36*(-12639 - 57218*y - 126926*y**2 - 
     -          144360*y**3 - 80089*y**4 - 
     -          12458*y**5 + 13374*y**6 + 
     -          8616*y**7)*zeta2 + 
     -       y**2*(155819 - 3456*zeta3) + 
     -       144*y**5*(157 + 216*zeta3) + 
     -       24*y**7*(341 + 576*zeta3) + 
     -       6*y**4*(14825 + 3744*zeta3) - 
     -       3*(5741 + 7200*zeta3) - 
     -       2*y*(-62035 + 9504*zeta3) - 
     -       18*y**6*(2489 + 10272*zeta3))*lins(1)
     -     )/(54d0*x**4*y**4) - 
     -  ((-40873 - 237574*x - 675780*x**2 - 
     -       1035016*x**3 - 933670*x**4 - 
     -       500508*x**5 - 149244*x**6 - 
     -       16704*x**7 - 
     -       24*IPi*(-2112 - 9772*x - 
     -          21679*x**2 - 24120*x**3 - 
     -          12832*x**4 - 988*x**5 + 
     -          1569*x**6 + 492*x**7) + 
     -       72*(-1293 - 8654*x - 21692*x**2 - 
     -          25896*x**3 - 16732*x**4 - 
     -          5512*x**5 - 828*x**6 + 192*x**7)*
     -        zeta2)*lins(1)**2)/(36d0*x**4*y**4) 
     -   - ((-1806 - 5620*y - 11992*y**2 - 
     -       14640*y**3 - 11581*y**4 - 
     -       5098*y**5 + 
     -       24*(191 - 180*IPi)*y**6 + 984*y**7)*
     -     lins(1)**3)/(9d0*x**4*y**4) + 
     -  ((813 + 4846*x + 11860*x**2 + 
     -       15176*x**3 + 10702*x**4 + 
     -       3820*x**5 + 216*x**6 - 64*x**7)*
     -     lins(1)**4)/(6d0*x**4*y**4) - 
     -  ((-1968 - 2128*x - 2857*x**2 - 
     -       203364*x**3 - 284094*x**4 - 
     -       106596*x**5 + 10470*x**6 - 
     -       24*IPi*(693*y + 4817*x**3*y**2 + 
     -          x*(-693 + 1874*y**2) - 
     -          x**2*y*(1874 + 4817*y**2)) + 
     -       72*(-2880 - 17280*x - 42928*x**2 - 
     -          57696*x**3 - 43507*x**4 - 
     -          17302*x**5 + 2970*x**6)*zeta2)*
     -     lins(1)*lins(2))/(18d0*x**4*y**4) + 
     -  ((2214 + 16284*x + 36990*x**2 + 
     -       41472*x**3 + 21259*x**4 + 526*x**5 - 
     -       3612*x**6 - 984*x**7 - 
     -       24*IPi*(173 + 958*x + 2328*x**2 + 
     -          3192*x**3 + 2460*x**4 + 
     -          984*x**5 + 360*x**6))*lins(1)**2*
     -     lins(2))/(3d0*x**4*y**4) - 
     -  (2*(907 + 5522*x + 13872*x**2 + 
     -       18408*x**3 + 13864*x**4 + 
     -       5680*x**5 + 840*x**6)*lins(1)**3*
     -     lins(2))/(3d0*x**4*y**4) + 
     -  ((272 - 96*x - 307*x**2 - 22*x**3 + 
     -       1770*x**4)*lins(1)**2*lins(2)**2)/
     -   (x**2*y**4) - 
     -  (12*(37 + 286*x + 988*x**2 + 1896*x**3 + 
     -       1996*x**4 + 1192*x**5 + 364*x**6 + 
     -       64*x**7)*zeta2*lins(3))/(x**4*y**4) 
     -   - (4*(-2112 - 9772*x - 21679*x**2 - 
     -       24120*x**3 - 12832*x**4 - 988*x**5 + 
     -       1569*x**6 + 492*x**7)*lins(1)*lins(3)
     -     )/(3d0*x**4*y**4) - 
     -  (4*(205 + 1222*x + 3158*x**2 + 
     -       4544*x**3 + 3728*x**4 + 1688*x**5 + 
     -       342*x**6 + 32*x**7)*lins(1)**2*
     -     lins(3))/(x**4*y**4) + 
     -  (4*(533 + 3118*x + 7728*x**2 + 
     -       10392*x**3 + 7860*x**4 + 3144*x**5 + 
     -       960*x**6)*lins(1)*lins(2)*lins(3))/
     -   (x**4*y**4) + 
     -  (4*(-2112 - 9772*x - 21679*x**2 - 
     -       24120*x**3 - 12832*x**4 - 988*x**5 + 
     -       1569*x**6 + 492*x**7)*lins(4))/
     -   (3d0*x**4*y**4) + 
     -  (16*(25 + 142*x + 458*x**2 + 944*x**3 + 
     -       1028*x**4 + 608*x**5 + 162*x**6 + 
     -       32*x**7)*lins(1)*lins(4))/(x**4*y**4)
     -    - (16*(67 + 482*x + 1272*x**2 + 
     -       1608*x**3 + 1140*x**4 + 456*x**5 + 
     -       120*x**6)*lins(1)*lins(5))/
     -   (x**4*y**4) - 
     -  (48*(-4 + 12*x + 115*x**2 + 276*x**3 + 
     -       334*x**4 + 232*x**5 + 51*x**6 + 
     -       16*x**7)*lins(6))/(x**4*y**4) - 
     -  (12*(x - y)*(27 - 188*x*y + 
     -       108*x**2*y**2)*lins(7))/(x**4*y**4)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^4
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_5_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (-2*(2592 + 26208*x + 69120*x**2 + 
     -      92686*x**3 + 64948*x**4 + 
     -      18917*x**5 - 556*x**6 + 564*x**7))/
     -  (3d0*x**3*y**5)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^3
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_5_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (896*(1 + (3*x)/14d0 - (1842*x**2)/35d0 - 
     -       (180529*x**3)/420d0 - 
     -       (765311*x**4)/672d0 - 
     -       (5210887*x**5)/3360d0 - 
     -       (615617*x**6)/560d0 - 
     -       (143441*x**7)/448d0 + 
     -       (319*x**8)/224d0 - (621*x**9)/56d0 - 
     -       x**10/56d0 - x**11/28d0))/
     -   (3d0*x**5*y**5) + 
     -  (8*(-400 - 232*y - 216*y**2 - 16*y**3 + 
     -       10*y**5 + 84*y**6 + 365*y**7 - 
     -       524*y**8 + 316*y**9)*lins(1))/
     -   (3d0*x**5*y**5)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_5_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-1804.69854870102394854870102395d0)
      double precision rat2
      parameter (rat2=-2440.77511635778962511635778963d0)
      double precision rat3
      parameter (rat3=-1771.66644241347211644241347212d0)
      double precision rat4
      parameter (rat4=-587.980612676652280612676652281d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (1313*(1 + (10828*x)/3939d0 - 
     -       (57767*x**10)/1313d0 - 
     -       (818*x**11)/101d0 + 
     -       x**9*(-126.141850723533891850723533892d0 + 
     -          (7520*zeta2)/1313d0) + 
     -       x**8*(-154.888359989845138359989845138d0 + 
     -          (1440*zeta2)/101d0) + 
     -       x**2*(-120.582922907675382922907675383d0 + 
     -          (34560*zeta2)/1313d0) + 
     -       x**7*(rat4 + (256760*zeta2)/1313d0) + 
     -       x**3*(-701.913345180671913345180671913d0 + 
     -          (26880*zeta2)/101d0) + 
     -       x**6*(rat3 + (873520*zeta2)/1313d0) + 
     -       x**4*(rat1 + (921600*zeta2)/1313d0) + 
     -       x**5*(rat2 + (1240320*zeta2)/1313d0)))
     -    /(5d0*x**5*y**5) - 
     -  ((48775 + 320928*x + 1210161*x**2 + 
     -       2655908*x**3 + 3726222*x**4 + 
     -       3454422*x**5 + 2113042*x**6 + 
     -       821786*x**7 + 183655*x**8 + 
     -       17907*x**9)*lins(1))/(3d0*x**5*y**5) 
     -   + (8*(462 + 3319*x + 11799*x**2 + 
     -       24034*x**3 + 30914*x**4 + 
     -       25900*x**5 + 14050*x**6 + 
     -       4712*x**7 + 862*x**8 + 64*x**9)*
     -     lins(1)**2)/(x**5*y**5) - 
     -  (16*(432 + 4368*x + 11520*x**2 + 
     -       15501*x**3 + 10907*x**4 + 3132*x**5)*
     -     lins(1)*lins(2))/(x**3*y**5)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_5_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-0.000193557478916999556147359076787d0)
      double precision rat2
      parameter (rat2=-0.000611951898428980837621169031141d0)
      double precision rat3
      parameter (rat3=0.00123061596883168121516989692755d0)
      double precision rat4
      parameter (rat4=-0.00101749742592866575704273588576d0)
      double precision rat5
      parameter (rat5=0.000110828776446219855008137298417d0)
      double precision rat6
      parameter (rat6=0.0045128756719435813976426493071d0)
      double precision rat7
      parameter (rat7=0.00289712103943712909536256185169d0)
      double precision rat8
      parameter (rat8=0.00319601814864131774917394091828d0)
      double precision rat9
      parameter (rat9=0.000898960398481037628840558267988d0)
      double precision rat10
      parameter (rat10=0.00419643227219674179283588959577d0)
      double precision rat11
      parameter (rat11=0.00243208820749289013825187815423d0)
      double precision rat12
      parameter (rat12=0.000455004812513356676694448554191d0)
      double precision rat13
      parameter (rat13=-0.0000608414541927635580542815340862d0)
      double precision rat14
      parameter (rat14=9.76475810031069684864624944518d-7)
      double precision rat15
      parameter (rat15=-0.0000111992733967878219986520030905d0)
      double precision rat16
      parameter (rat16=-6.92363326918679608816117875377d0)
      double precision rat17
      parameter (rat17=-21.3578852684119131649735837132d0)
      double precision rat18
      parameter (rat18=-14.7385406811593584000453438504d0)
      double precision rat19
      parameter (rat19=-62.0085326411667034826639356832d0)
      double precision rat20
      parameter (rat20=-37.804658847699401329180437079d0)
      double precision rat21
      parameter (rat21=-120.636727820043922972910543338d0)
      double precision rat22
      parameter (rat22=-39.4649891512508574762808570143d0)
      double precision rat23
      parameter (rat23=-141.979767636059697042517235904d0)
      double precision rat24
      parameter (rat24=-22.9175894929602507577042812282d0)
      double precision rat25
      parameter (rat25=-105.679647249689716845512181695d0)
      double precision rat26
      parameter (rat26=-49.0857080298601677955396092897d0)
      double precision rat27
      parameter (rat27=4.0949223816546856153234262714d0)
      double precision rat28
      parameter (rat28=-12.9254916682615951951047276011d0)
      double precision rat29
      parameter (rat29=6.33944297719646892529251248023d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-20160*IPi*x**3*zeta2)/y**5 + 
     -  (270360000*(rat1 + 
     -       x**9*(rat15 + zeta2/5.6325d6) + 
     -       (253*zeta2)/1.8775d6 + 
     -       x**8*(rat13 + rat14*zeta2) + 
     -       x**6*(rat11 + 
     -          (251*zeta2)/5.06925d6 - 
     -          (134*zeta3)/4.224375d6) + 
     -       x**7*(rat12 + (347*zeta2)/7.51d6 + 
     -          (47*zeta3)/8.44875d6) + 
     -       x**5*(rat10 + rat9*zeta2 + 
     -          (1409*zeta3)/8.44875d6) + 
     -       x*(rat2 + rat3*zeta2 + 
     -          (364*zeta3)/1.408125d6) + 
     -       x**4*(rat7 + rat8*zeta2 + 
     -          (104*zeta3)/168975d0) + 
     -       x**2*(rat4 + 
     -          (13387*zeta2)/4.0554d6 + 
     -          (64*zeta3)/93875d0) + 
     -       x**3*(rat5 + rat6*zeta2 + 
     -          (1256*zeta3)/1.408125d6) + 
     -       (12*zeta3)/469375d0))/(x**3*y**5) + 
     -  (2575873*(1 + 
     -       x**6*(rat25 - 
     -          (9381600*zeta2)/2.575873d6) + 
     -       x**9*(-1.47442245793950245217834885493d0 + 
     -          (1411200*zeta2)/2.575873d6) - 
     -       (1821600*zeta2)/2.575873d6 + 
     -       x*(0.801227777922281106250191682587d0 + 
     -          rat16*zeta2) + 
     -       x**2*(rat18 + rat17*zeta2) + 
     -       x**3*(rat19 + rat20*zeta2) + 
     -       x**4*(rat21 + rat22*zeta2) + 
     -       x**5*(rat23 + rat24*zeta2) + 
     -       x**7*(rat26 + rat27*zeta2) + 
     -       x**8*(rat28 + rat29*zeta2))*lins(1))/
     -   (450d0*x**5*y**5) + 
     -  (2*(76512 + 587579*x + 2166804*x**2 + 
     -       4582745*x**3 + 6110620*x**4 + 
     -       5317620*x**5 + 2998580*x**6 + 
     -       1048400*x**7 + 199420*x**8 + 
     -       15180*x**9 - 25200*IPi*y**8)*
     -     lins(1)**2)/(15d0*x**5*y**5) - 
     -  (16*(336 + 2717*x + 9567*x**2 + 
     -       19190*x**3 + 24020*x**4 + 
     -       19299*x**5 + 9733*x**6 + 2828*x**7 + 
     -       350*x**8)*lins(1)**3)/(3d0*x**5*y**5) 
     -   - (2*(203580 + 1921900*x + 
     -       5122750*x**2 + 6964191*x**3 + 
     -       4918147*x**4 + 1412082*x**5)*lins(1)*
     -     lins(2))/(15d0*x**3*y**5) + 
     -  (16*(154 + 1668*x + 5860*x**2 + 
     -       11768*x**3 + 14700*x**4 + 
     -       11789*x**5 + 5923*x**6 + 1708*x**7 + 
     -       210*x**8)*lins(1)**2*lins(2))/
     -   (x**5*y**5) + 
     -  (6720*y**3*lins(1)*lins(3))/x**5 - 
     -  (6720*y**3*lins(4))/x**5
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_5_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=179.086545290454144620811287478d0)
      double precision rat2
      parameter (rat2=461.244156599633487654320987654d0)
      double precision rat3
      parameter (rat3=571.46556031987158289241622575d0)
      double precision rat4
      parameter (rat4=-427.024854910714285714285714286d0)
      double precision rat5
      parameter (rat5=273.474400341366291887125220459d0)
      double precision rat6
      parameter (rat6=-546.811253720238095238095238095d0)
      double precision rat7
      parameter (rat7=-85.7483180052634479717813051146d0)
      double precision rat8
      parameter (rat8=-112.429604478408840388007054674d0)
      double precision rat9
      parameter (rat9=-16.4298480334408068783068783069d0)
      double precision rat10
      parameter (rat10=5.74354183201058201058201058201d0)
      double precision rat11
      parameter (rat11=0.000175998684836391550178173068232d0)
      double precision rat12
      parameter (rat12=0.000870340368535728152081804559181d0)
      double precision rat13
      parameter (rat13=0.00100302979486465012776103594057d0)
      double precision rat14
      parameter (rat14=8.23770897377566438908362678541d-6)
      double precision rat15
      parameter (rat15=0.00386539018630178174300228970734d0)
      double precision rat16
      parameter (rat16=0.00703786024628890966143847715481d0)
      double precision rat17
      parameter (rat17=0.00376540915886811465436410813894d0)
      double precision rat18
      parameter (rat18=0.00904367407987460390833996813321d0)
      double precision rat19
      parameter (rat19=0.0253697955986664499294780692278d0)
      double precision rat20
      parameter (rat20=0.0143400864338733375499141380811d0)
      double precision rat21
      parameter (rat21=0.0172022343366983526587670429222d0)
      double precision rat22
      parameter (rat22=0.0514925295377388085660628794501d0)
      double precision rat23
      parameter (rat23=0.0163623916995505127471072860443d0)
      double precision rat24
      parameter (rat24=0.0387396037854392493340410718929d0)
      double precision rat25
      parameter (rat25=0.065372579983433595480648061152d0)
      double precision rat26
      parameter (rat26=0.0135187654523059252120563715477d0)
      double precision rat27
      parameter (rat27=0.0508250337844718240105768791609d0)
      double precision rat28
      parameter (rat28=0.0533474815821899557966688837714d0)
      double precision rat29
      parameter (rat29=0.00765640343028506957154504868279d0)
      double precision rat30
      parameter (rat30=0.0275894766105075695691399376899d0)
      double precision rat31
      parameter (rat31=0.0415695144501595860443622151943d0)
      double precision rat32
      parameter (rat32=0.00274935086054300622992231687163d0)
      double precision rat33
      parameter (rat33=0.0084451192234716787418729057496d0)
      double precision rat34
      parameter (rat34=0.0211090803277884192389247206945d0)
      double precision rat35
      parameter (rat35=0.000495713350294991347388997646106d0)
      double precision rat36
      parameter (rat36=0.00127733406605243556132869158326d0)
      double precision rat37
      parameter (rat37=0.00787969801132867609271244683747d0)
      double precision rat38
      parameter (rat38=0.0000245719455472218052004348027594d0)
      double precision rat39
      parameter (rat39=0.0000500915320020562965226937875787d0)
      double precision rat40
      parameter (rat40=0.00121696985658902006337067973377d0)
      double precision rat41
      parameter (rat41=0.349706056605051120590425892071d0)
      double precision rat42
      parameter (rat42=9.54252564493195206175096485883d0)
      double precision rat43
      parameter (rat43=28.0348140530841627733766673438d0)
      double precision rat44
      parameter (rat44=14.9272246766876565779673640734d0)
      double precision rat45
      parameter (rat45=-4.60329546957671957671957671958d0)
      double precision rat46
      parameter (rat46=-3.22433213458994708994708994709d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*IPi*(-147509 - 638623*y - 
     -       1731998*y**2 - 3363264*y**3 - 
     -       3808740*y**4 - 2376184*y**5 - 
     -       667864*y**6 + 85960*y**7 + 
     -       113845*y**8 + 23235*y**9 + 
     -       330*y**10 + 60*y**11)*zeta2)/
     -   (5d0*x**5*y**5) + 
     -  (1792*(x**11*
     -        (1.02980522486772486772486772487d0 + 
     -          (21317*zeta2)/13440d0 + zeta3/28d0) 
     -        + x**10*
     -        (rat10 + (234487*zeta2)/26880d0 + 
     -          (11*zeta3)/56d0) - zeta3 - 
     -       (3*x*zeta3)/14d0 + 
     -       x**5*(rat3 - 
     -          (4638733*zeta2)/38400d0 + 
     -          (1187877*zeta3)/4480d0 - 
     -          (20265*zeta4)/64d0) + 
     -       x**4*(rat2 + 
     -          (1047773*zeta2)/26880d0 + 
     -          (251287*zeta3)/1344d0 - 
     -          (1620*zeta4)/7d0) + 
     -       x**6*(rat5 + rat4*zeta2 + 
     -          (2578979*zeta3)/13440d0 - 
     -          (95253*zeta4)/448d0) + 
     -       x**3*(rat1 + 
     -          (5603617*zeta2)/134400d0 + 
     -          (228127*zeta3)/3360d0 - 
     -          (351*zeta4)/4d0) + 
     -       x**7*(rat7 + rat6*zeta2 + 
     -          (281411*zeta3)/6720d0 - 
     -          (5889*zeta4)/112d0) + 
     -       x**8*(rat8 - 
     -          (7759951*zeta2)/26880d0 - 
     -          (1411*zeta3)/168d0 - 
     -          (1073*zeta4)/112d0) + 
     -       x**2*(31.3774962797619047619047619048d0 + 
     -          (12455*zeta2)/448d0 + 
     -          (18969*zeta3)/1120d0 - 
     -          (243*zeta4)/28d0) + 
     -       x**9*(rat9 - (378131*zeta2)/7680d0 + 
     -          (143*zeta3)/32d0 + (111*zeta4)/56d0)
     -       ))/(x**5*y**5) + 
     -  ((-16*IPi*(522 + 2601*y + 6507*y**2 + 
     -          11214*y**3 + 12960*y**4 + 
     -          10476*y**5 + 5292*y**6 + 
     -          1512*y**7 + 280*y**8)*zeta2)/
     -      (x**5*y**5) + 
     -     (20442577*
     -        (rat13 + rat11*zeta2 + 
     -          rat12*zeta3 + 
     -          x*(rat15 + rat14*zeta2 + 
     -             rat16*zeta3) + 
     -          x**2*
     -           (rat18 + rat17*zeta2 + 
     -             rat19*zeta3) + 
     -          x**3*
     -           (rat20 + rat21*zeta2 + 
     -             rat22*zeta3) + 
     -          x**4*
     -           (rat23 + rat24*zeta2 + 
     -             rat25*zeta3) + 
     -          x**5*
     -           (rat26 + rat27*zeta2 + 
     -             rat28*zeta3) + 
     -          x**6*
     -           (rat29 + rat31*zeta2 + 
     -             rat30*zeta3) + 
     -          x**7*
     -           (rat32 + rat34*zeta2 + 
     -             rat33*zeta3) + 
     -          x**8*
     -           (rat35 + rat37*zeta2 + 
     -             rat36*zeta3) + 
     -          x**9*
     -           (rat38 + rat40*zeta2 + 
     -             rat39*zeta3)))/(x**5*y**5))*
     -   lins(1) + (-(IPi*
     -         (30832 + 85829*y + 187109*y**2 + 
     -           242880*y**3 + 222240*y**4 + 
     -           120934*y**5 + 36268*y**6 + 
     -           3944*y**7 + 89420*y**8 + 
     -           22485*y**9))/(15d0*x**5*y**5) + 
     -     (13128*(rat41 + 
     -          x**9*
     -           (0.133759902498476538695917123705d0 - 
     -             (68*zeta2)/547d0) + 
     -          x**8*
     -           (1.31213183018484663822872232379d0 + 
     -             (296*zeta2)/547d0) + zeta2 + 
     -          x**7*
     -           (5.71106286816981515336177127768d0 + 
     -             (12901*zeta2)/1641d0) + 
     -          x*(2.51707664703094319182070553186d0 + 
     -             (15262*zeta2)/1641d0) + 
     -          x**6*
     -           (rat44 + (51443*zeta2)/1641d0) + 
     -          x**2*
     -           (rat42 + (18740*zeta2)/547d0) + 
     -          x**5*
     -           (25.1266478773105829778590290473d0 + 
     -             (36113*zeta2)/547d0) + 
     -          x**3*
     -           (20.6002539102173471460491570181d0 + 
     -             (112753*zeta2)/1641d0) + 
     -          x**4*
     -           (rat43 + (139561*zeta2)/1641d0)))/
     -      (x**5*y**5))*lins(1)**2 + 
     -  ((-74741 - 757412*x - 2719163*x**2 - 
     -       5385440*x**3 - 6426120*x**4 - 
     -       4709232*x**5 - 1950924*x**6 - 
     -       319140*x**7 + 61905*x**8 + 
     -       22485*x**9 + 100800*IPi*y**8)*
     -     lins(1)**3)/(45d0*x**5*y**5) + 
     -  (2*(975 + 7792*x + 27014*x**2 + 
     -       53295*x**3 + 65667*x**4 + 
     -       51539*x**5 + 24943*x**6 + 
     -       6559*x**7 + 296*x**8 - 68*x**9)*
     -     lins(1)**4)/(3d0*x**5*y**5) + 
     -  ((IPi*(57712*y + 6342*x**4*y**3 - 
     -          2*x**3*y**2*(32719 + 3171*y**2) + 
     -          x*(-57712 + 23835*y**2) + 
     -          x**2*y*(-23835 + 65438*y**2)))/
     -      (15d0*x**5*y**5) + 
     -     (53760*(0.00488467261904761904761904761905d0 + zeta2 - 
     -          x**8*zeta2 + 
     -          x*(0.0134275793650793650793650793651d0 + 
     -             8*zeta2) + 
     -          x**7*
     -           (-0.890638764880952380952380952381d0 + 
     -             (7517*zeta2)/840d0) + 
     -          x**2*
     -           (-0.333440145502645502645502645503d0 + 
     -             (1969*zeta2)/70d0) + 
     -          x**6*
     -           (rat46 + (105073*zeta2)/3360d0) + 
     -          x**3*
     -           (-1.36911086309523809523809523810d0 + 
     -             (573*zeta2)/10d0) + 
     -          x**5*
     -           (rat45 + (203719*zeta2)/3360d0) + 
     -          x**4*
     -           (-3.42115988756613756613756613757d0 + 
     -             (514*zeta2)/7d0)))/(x**5*y**5))*
     -   lins(1)*lins(2) + 
     -  ((45789 + 462603*x + 1551544*x**2 + 
     -       2898830*x**3 + 3235680*x**4 + 
     -       2090392*x**5 + 610404*x**6 - 
     -       89180*x**7 - 112945*x**8 - 
     -       22485*x**9 - 
     -       480*IPi*
     -        (198 + 1409*x + 4643*x**2 + 
     -          9086*x**3 + 11240*x**4 + 
     -          9004*x**5 + 4508*x**6 + 
     -          1288*x**7 + 420*x**8))*lins(1)**2*
     -     lins(2))/(15d0*x**5*y**5) - 
     -  (8*(1062 + 8671*x + 30637*x**2 + 
     -       61474*x**3 + 76960*x**4 + 
     -       61440*x**5 + 30600*x**6 + 
     -       8680*x**7 + 840*x**8)*lins(1)**3*
     -     lins(2))/(3d0*x**5*y**5) + 
     -  (4*(-432 - 4368*x - 11520*x**2 - 
     -       15559*x**3 - 10993*x**4 - 
     -       3188*x**5 + 1960*x**6)*lins(1)**2*
     -     lins(2)**2)/(x**3*y**5) - 
     -  (16*(71 + 766*x + 3800*x**2 + 
     -       10135*x**3 + 15719*x**4 + 
     -       15709*x**5 + 10493*x**6 + 
     -       4795*x**7 + 1352*x**8 + 204*x**9)*
     -     zeta2*lins(3))/(x**5*y**5) - 
     -  (2*(-89797 - 547034*x - 1738065*x**2 - 
     -       3142840*x**3 - 3449840*x**4 - 
     -       2188260*x**5 - 623680*x**6 + 
     -       98044*x**7 + 112945*x**8 + 22485*x**9
     -       )*lins(1)*lins(3))/(15d0*x**5*y**5) - 
     -  (8*(455 + 3531*x + 12443*x**2 + 
     -       25531*x**3 + 32813*x**4 + 
     -       27307*x**5 + 14539*x**6 + 
     -       4753*x**7 + 824*x**8 + 68*x**9)*
     -     lins(1)**2*lins(3))/(x**5*y**5) + 
     -  (16*(618 + 4769*x + 16403*x**2 + 
     -       32606*x**3 + 40640*x**4 + 
     -       32524*x**5 + 16268*x**6 + 
     -       4648*x**7 + 1120*x**8)*lins(1)*
     -     lins(2)*lins(3))/(x**5*y**5) + 
     -  (2*(-89797 - 547034*x - 1738065*x**2 - 
     -       3142840*x**3 - 3449840*x**4 - 
     -       2188260*x**5 - 623680*x**6 + 
     -       98044*x**7 + 112945*x**8 + 22485*x**9
     -       )*lins(4))/(15d0*x**5*y**5) + 
     -  (32*(35 + 171*x + 683*x**2 + 2011*x**3 + 
     -       3413*x**4 + 3787*x**5 + 2779*x**6 + 
     -       1393*x**7 + 404*x**8 + 68*x**9)*
     -     lins(1)*lins(4))/(x**5*y**5) - 
     -  (64*(82 + 831*x + 3197*x**2 + 6594*x**3 + 
     -       8360*x**4 + 6676*x**5 + 3332*x**6 + 
     -       952*x**7 + 140*x**8)*lins(1)*lins(5))
     -    /(x**5*y**5) - 
     -  (16*(-103 - 101*x + 1631*x**2 + 
     -       6397*x**3 + 11399*x**4 + 
     -       12217*x**5 + 8729*x**6 + 4291*x**7 + 
     -       792*x**8 + 204*x**9)*lins(6))/
     -   (x**5*y**5) + 
     -  (16*(x - y)*(-106 + 1161*x*y - 
     -       2188*x**2*y**2 + 868*x**3*y**3)*
     -     lins(7))/(x**5*y**5)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^4
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_6_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (-4*(10464 + 9708*x - 7080*x**2 - 
     -      10363*x**3 - 952*x**4 + 2772*x**5 + 
     -      2448*x**6 - 843*x**7 + 1104*x**8))/
     -  (3d0*x**3*y**6)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^3
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_6_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-4*(-12600 - 3360*x - 160184*x**2 - 
     -       441592*x**3 - 2847140*x**4 - 
     -       5770602*x**5 - 4546277*x**6 - 
     -       1041698*x**7 + 373785*x**8 + 
     -       235810*x**9 - 30815*x**10 + 
     -       148822*x**11 - 951*x**12 + 96*x**13))
     -    /(45d0*x**6*y**6) + 
     -  (16*(-814 - 728*y - 834*y**2 - 300*y**3 - 
     -       102*y**4 + 205*y**6 + 944*y**7 + 
     -       1524*y**8 + 1616*y**9 - 947*y**10 + 
     -       624*y**11)*lins(1))/(3d0*x**6*y**6)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_6_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-811.145442231379731379731379731d0)
      double precision rat2
      parameter (rat2=-1398.24477258852258852258852259d0)
      double precision rat3
      parameter (rat3=-369.498328754578754578754578755d0)
      double precision rat4
      parameter (rat4=-313.320531898656898656898656899d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2912*(1 + (7783*x)/1820d0 + 
     -       (476117*x**2)/43680d0 - 
     -       (475611*x**12)/7280d0 - 
     -       (36997*x**13)/3640d0 + 
     -       x**6*(-1100.85721153846153846153846154d0 - 
     -          (36585*zeta2)/364d0) + 
     -       x**5*(rat2 - (5900*zeta2)/91d0) + 
     -       x**7*(rat3 - (465*zeta2)/26d0) + 
     -       x**11*(-232.956578144078144078144078144d0 + 
     -          (920*zeta2)/91d0) + 
     -       x**8*(-178.508119658119658119658119658d0 + 
     -          (515*zeta2)/26d0) + 
     -       x**9*(-245.966880341880341880341880342d0 + 
     -          (2040*zeta2)/91d0) + 
     -       x**10*(rat4 + (5945*zeta2)/182d0) + 
     -       x**4*(rat1 + (8090*zeta2)/91d0) + 
     -       x**3*(-153.947649572649572649572649573d0 + 
     -          (8720*zeta2)/91d0)))/(5d0*x**6*y**6)
     -    + ((-239165 - 31472*y + 146446*y**2 + 
     -       464148*y**3 + 538720*y**4 + 
     -       403200*y**5 + 243398*y**6 + 
     -       334600*y**7 + 363180*y**8 + 
     -       52040*y**9 - 533560*y**10 + 
     -       359048*y**11)*lins(1))/
     -   (15d0*x**6*y**6) + 
     -  (4*(3969 + 36148*x + 164549*x**2 + 
     -       444442*x**3 + 790923*x**4 + 
     -       970108*x**5 + 834296*x**6 + 
     -       500800*x**7 + 204338*x**8 + 
     -       53580*x**9 + 8014*x**10 + 512*x**11)*
     -     lins(1)**2)/(x**6*y**6) + 
     -  (8*(-6976 - 6472*x + 4720*x**2 + 
     -       7553*x**3 + 2418*x**4 + 210*x**5)*
     -     lins(1)*lins(2))/(x**3*y**6)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_6_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=0.824950280301970535699841363645d0)
      double precision rat2
      parameter (rat2=-24.4173646951793390183578520238d0)
      double precision rat3
      parameter (rat3=-48.8365765075449810673647808258d0)
      double precision rat4
      parameter (rat4=3.45238877516936595894106664154d0)
      double precision rat5
      parameter (rat5=116.646351590312809576420946851d0)
      double precision rat6
      parameter (rat6=180.954348095935502473756484154d0)
      double precision rat7
      parameter (rat7=143.862792589864592879349771752d0)
      double precision rat8
      parameter (rat8=60.6629335211846624243099033051d0)
      double precision rat9
      parameter (rat9=9.97555045863702155722471072894d0)
      double precision rat10
      parameter (rat10=-0.611075097708385078095335976955d0)
      double precision rat11
      parameter (rat11=-0.0953776810923925921484996160182d0)
      double precision rat12
      parameter (rat12=0.755052602115666743880781406841d0)
      double precision rat13
      parameter (rat13=-27.4232544784186549301212261601d0)
      double precision rat14
      parameter (rat14=-133.396013628291251640799938229d0)
      double precision rat15
      parameter (rat15=-337.647705775615782565052891669d0)
      double precision rat16
      parameter (rat16=-550.651298355339356034283066945d0)
      double precision rat17
      parameter (rat17=-612.366852173577329935912284766d0)
      double precision rat18
      parameter (rat18=-471.751876302988186240444753301d0)
      double precision rat19
      parameter (rat19=-247.903952397498262682418346074d0)
      double precision rat20
      parameter (rat20=-16.9773189329009342907883561115d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-90720*IPi*x**4*zeta2)/y**6 + 
     -  (233518*(1 + 
     -       x**11*
     -        (rat11 + (288*zeta2)/583795d0) + 
     -       x**10*(rat10 + 
     -          (1872*zeta2)/583795d0) - 
     -       (7560*zeta2)/116759d0 + 
     -       x**3*(rat3 - (57744*zeta2)/116759d0 - 
     -          (56640*zeta3)/116759d0) + 
     -       x**8*(rat8 + (64796*zeta2)/116759d0 - 
     -          (54624*zeta3)/116759d0) + 
     -       x**4*(rat4 - 
     -          (1058202*zeta2)/583795d0 - 
     -          (47232*zeta3)/116759d0) + 
     -       x**9*(rat9 + 
     -          (373072*zeta2)/583795d0 + 
     -          (8832*zeta3)/116759d0) + 
     -       x**7*(rat7 - (41680*zeta2)/116759d0 + 
     -          (19584*zeta3)/116759d0) + 
     -       x**5*(rat5 - 
     -          (162516*zeta2)/583795d0 + 
     -          (36288*zeta3)/116759d0) + 
     -       x**6*(rat6 + (96996*zeta2)/583795d0 + 
     -          (55104*zeta3)/116759d0) + 
     -       x**2*(rat2 + 
     -          (394766*zeta2)/116759d0 + 
     -          (77664*zeta3)/116759d0) + 
     -       x*(rat1 + (339640*zeta2)/116759d0 + 
     -          (83712*zeta3)/116759d0)))/
     -   (3d0*x**4*y**6) + 
     -  (23024*(rat12 + 
     -       x**5*(rat16 - 
     -          (284936*zeta2)/1439d0) + 
     -       x**4*(rat15 - 
     -          (261169*zeta2)/1439d0) + 
     -       x**6*(rat17 - 
     -          (203601*zeta2)/1439d0) + 
     -       x**3*(rat14 - 
     -          (161394*zeta2)/1439d0) + 
     -       x**7*(rat18 - (84094*zeta2)/1439d0) + 
     -       x**2*(rat13 - (65167*zeta2)/1439d0) + 
     -       x*(-0.131473245309242529534398888117d0 - 
     -          (15928*zeta2)/1439d0) + 
     -       x**8*(rat19 - (8164*zeta2)/1439d0) + 
     -       x**11*(-1.49770828507451162072426839626d0 + 
     -          (800*zeta2)/1439d0) - zeta2 + 
     -       x**9*(-84.8360493398193189715079916609d0 + 
     -          (10108*zeta2)/1439d0) + 
     -       x**10*(rat20 + (10764*zeta2)/1439d0))*
     -     lins(1))/(x**6*y**6) - 
     -  (2*(-4340 + 157808*y + 490392*y**2 + 
     -       904064*y**3 + 1100585*y**4 + 
     -       889340*y**5 + 481378*y**6 + 
     -       201320*y**7 + 111460*y**8 + 
     -       15520*y**9 + 
     -       (-281220 + 113400*IPi)*
     -        y**10 + 58928*y**11)*lins(1)**2)/
     -   (15d0*x**6*y**6) - 
     -  (56*(-30 - 208*y - 678*y**2 - 1380*y**3 - 
     -       1910*y**4 - 1800*y**5 - 1129*y**6 - 
     -       434*y**7 - 90*y**8 + 450*y**10)*
     -     lins(1)**3)/(3d0*x**6*y**6) + 
     -  (2*(18900 - 1380300*x - 1463255*x**2 + 
     -       541320*x**3 + 1156392*x**4 + 
     -       353912*x**5 + 33380*x**6)*lins(1)*
     -     lins(2))/(15d0*x**4*y**6) + 
     -  (8*(-709 - 1400*y - 3121*y**2 - 
     -       5158*y**3 - 6753*y**4 - 6300*y**5 - 
     -       3843*y**6 - 1358*y**7 - 210*y**8 + 
     -       1890*y**10)*lins(1)**2*lins(2))/
     -   (x**6*y**6) + 
     -  (30240*y**4*lins(1)*lins(3))/x**6 - 
     -  (30240*y**4*lins(4))/x**6
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_6_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=9.32917709435626102292768959436d0)
      double precision rat2
      parameter (rat2=146.361689821704144620811287478d0)
      double precision rat3
      parameter (rat3=275.029019082524985302763080541d0)
      double precision rat4
      parameter (rat4=509.51176455026455026455026455d0)
      double precision rat5
      parameter (rat5=85.7673167080026455026455026455d0)
      double precision rat6
      parameter (rat6=-1024.14664087301587301587301587d0)
      double precision rat7
      parameter (rat7=-350.736828202895355673133450911d0)
      double precision rat8
      parameter (rat8=-1629.92804828042328042328042328d0)
      double precision rat9
      parameter (rat9=-590.311960436875367430922986479d0)
      double precision rat10
      parameter (rat10=-1316.81752645502645502645502646d0)
      double precision rat11
      parameter (rat11=-469.184513756613756613756613757d0)
      double precision rat12
      parameter (rat12=-545.024666005291005291005291005d0)
      double precision rat13
      parameter (rat13=-192.252117614638447971781305115d0)
      double precision rat14
      parameter (rat14=-28.4650380092592592592592592593d0)
      double precision rat15
      parameter (rat15=3.17225244047619047619047619048d0)
      double precision rat16
      parameter (rat16=301.491448412698412698412698413d0)
      double precision rat17
      parameter (rat17=0.000148017326110554780425375709432d0)
      double precision rat18
      parameter (rat18=0.000655715724937293280333339173148d0)
      double precision rat19
      parameter (rat19=0.000804427657516980408815360419432d0)
      double precision rat20
      parameter (rat20=-0.000186471118009273966447382060369d0)
      double precision rat21
      parameter (rat21=0.00383847738688744792318882055181d0)
      double precision rat22
      parameter (rat22=0.00674680710672882344483906049684d0)
      double precision rat23
      parameter (rat23=0.00165401560585852742398189658694d0)
      double precision rat24
      parameter (rat24=0.0109491776658362207014534998014d0)
      double precision rat25
      parameter (rat25=0.0312645580001133769021533577817d0)
      double precision rat26
      parameter (rat26=0.0140786880527884736807055773856d0)
      double precision rat27
      parameter (rat27=0.0216683202264169275797157492616d0)
      double precision rat28
      parameter (rat28=0.0845166799156578597370574234702d0)
      double precision rat29
      parameter (rat29=0.0326845834344990293156347891906d0)
      double precision rat30
      parameter (rat30=0.0483293067855880065761513362434d0)
      double precision rat31
      parameter (rat31=0.1495109754356509144154399496d0)
      double precision rat32
      parameter (rat32=0.0385205589346403593190993694598d0)
      double precision rat33
      parameter (rat33=0.0955794673797810222987473872602d0)
      double precision rat34
      parameter (rat34=0.181294518597820354659504004343d0)
      double precision rat35
      parameter (rat35=0.0345789862326233894157626037689d0)
      double precision rat36
      parameter (rat36=0.123112957686314720395778568111d0)
      double precision rat37
      parameter (rat37=0.0225177229805460303203959304163d0)
      double precision rat38
      parameter (rat38=0.0892912359562295314964366821607d0)
      double precision rat39
      parameter (rat39=0.107222034515238709935610689092d0)
      double precision rat40
      parameter (rat40=0.00987064751064441708185366149162d0)
      double precision rat41
      parameter (rat41=0.0346484379620286097789903399874d0)
      double precision rat42
      parameter (rat42=0.0630059157094750768353206428377d0)
      double precision rat43
      parameter (rat43=0.0026230110916820928564663556925d0)
      double precision rat44
      parameter (rat44=0.00825218642772374009497754174482d0)
      double precision rat45
      parameter (rat45=0.0241307729561665079521551263752d0)
      double precision rat46
      parameter (rat46=0.000330323112875792736483029560811d0)
      double precision rat47
      parameter (rat47=0.00100358621399661110009232083199d0)
      double precision rat48
      parameter (rat48=0.00675560236510407479434892561226d0)
      double precision rat49
      parameter (rat49=5.88345851545595363828063716155d-6)
      double precision rat50
      parameter (rat50=0.0000343841101155155837290730906034d0)
      double precision rat51
      parameter (rat51=0.000878773685116358047592438068986d0)
      double precision rat52
      parameter (rat52=0.308593573682356915425031085811d0)
      double precision rat53
      parameter (rat53=2.65114438910091927751543509354d0)
      double precision rat54
      parameter (rat54=12.5318111348297647540806865472d0)
      double precision rat55
      parameter (rat55=34.9909638398344006727472669642d0)
      double precision rat56
      parameter (rat56=64.4567367265382984381625949645d0)
      double precision rat57
      parameter (rat57=82.8387050333858018701799023941d0)
      double precision rat58
      parameter (rat58=76.0533316802150491263629241506d0)
      double precision rat59
      parameter (rat59=49.9553648000804996729700785591d0)
      double precision rat60
      parameter (rat60=23.0071848833114115473905887257d0)
      double precision rat61
      parameter (rat61=-1.61088206385949441504997060553d0)
      double precision rat62
      parameter (rat62=-2.7604798739711934156378600823d0)
      double precision rat63
      parameter (rat63=-0.0334681621105232216343327454439d0)
      double precision rat64
      parameter (rat64=184.734025303382390911438161632d0)
      double precision rat65
      parameter (rat65=31.4118065824772623176077995158d0)
      double precision rat66
      parameter (rat66=78.3542707583589609369888110973d0)
      double precision rat67
      parameter (rat67=136.522567558725381142445854871d0)
      double precision rat68
      parameter (rat68=94.8326951514754956487600601976d0)
      double precision rat69
      parameter (rat69=-31.4118065824772623176077995158d0)
      double precision rat70
      parameter (rat70=-78.3542707583589609369888110973d0)
      double precision rat71
      parameter (rat71=-136.522567558725381142445854871d0)
      double precision rat72
      parameter (rat72=-94.8326951514754956487600601976d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (40320*IPi*(1 + (4*x)/15d0 + 
     -       (146009*x**2)/12600d0 + 
     -       (1476817*x**3)/12600d0 + 
     -       (631133*x**4)/2016d0 + 
     -       (298349*x**5)/700d0 + rat16*x**6 + 
     -       (547447*x**7)/6300d0 - 
     -       (109*x**8)/25200d0 - 
     -       (629*x**9)/2520d0 - 
     -       (19849*x**10)/5040d0 - 
     -       (12283*x**11)/12600d0 - 
     -       (13*x**12)/4200d0 - x**13/2100d0)*zeta2
     -     )/(x**6*y**6) + 
     -  (6720*(x**2*(-5.51814649470899470899470899471d0 - 
     -          (2753*zeta2)/210d0 - 
     -          (158609*zeta3)/12600d0) + 
     -       x**13*(0.477232976190476190476190476190d0 + 
     -          (32959*zeta2)/31500d0 + 
     -          (4*zeta3)/525d0) + 
     -       x**12*(rat15 + 
     -          (428467*zeta2)/63000d0 + 
     -          (26*zeta3)/525d0) - zeta3 - 
     -       (4*x*zeta3)/15d0 + 
     -       x**3*(rat1 - (693073*zeta2)/15750d0 - 
     -          (1440617*zeta3)/12600d0 - 
     -          (654*zeta4)/35d0) + 
     -       x**4*(rat2 + 
     -          (6036017*zeta2)/28800d0 - 
     -          (3056497*zeta3)/10080d0 - 
     -          (2427*zeta4)/140d0) + 
     -       x**10*(rat13 + rat12*zeta2 - 
     -          (19463*zeta3)/1260d0 - 
     -          (421*zeta4)/40d0) + 
     -       x**11*(rat14 - 
     -          (1569329*zeta2)/18900d0 + 
     -          (464*zeta3)/105d0 + (12*zeta4)/5d0) 
     -        + x**5*
     -        (rat3 + rat4*zeta2 - 
     -          (2655631*zeta3)/6300d0 + 
     -          (177*zeta4)/14d0) + 
     -       x**7*(rat7 + rat6*zeta2 - 
     -          (1058117*zeta3)/12600d0 + 
     -          (7341*zeta4)/560d0) + 
     -       x**8*(rat9 + rat8*zeta2 + 
     -          (3769*zeta3)/1575d0 + 
     -          (10509*zeta4)/560d0) + 
     -       x**9*(rat11 + rat10*zeta2 - 
     -          (5167*zeta3)/315d0 + (136*zeta4)/7d0
     -          ) + x**6*
     -        (rat5 + (1950353*zeta2)/40320d0 - 
     -          (7465343*zeta3)/25200d0 + 
     -          (31429*zeta4)/1120d0)))/(x**6*y**6)
     -    + ((-72*IPi*
     -        (499 + 2744*y + 7695*y**2 + 
     -          14874*y**3 + 20055*y**4 + 
     -          18900*y**5 + 10878*y**6 + 
     -          3108*y**7 + 280*y**10)*zeta2)/
     -      (x**6*y**6) + 
     -     (119124793*
     -        (rat19 + rat17*zeta2 + 
     -          x**6*
     -           (rat35 + rat36*zeta2 + 
     -             (493056*zeta3)/3.219589d6) + 
     -          rat18*zeta3 + 
     -          x*(rat21 + rat20*zeta2 + 
     -             rat22*zeta3) + 
     -          x**2*
     -           (rat24 + rat23*zeta2 + 
     -             rat25*zeta3) + 
     -          x**3*
     -           (rat27 + rat26*zeta2 + 
     -             rat28*zeta3) + 
     -          x**4*
     -           (rat29 + rat30*zeta2 + 
     -             rat31*zeta3) + 
     -          x**5*
     -           (rat32 + rat33*zeta2 + 
     -             rat34*zeta3) + 
     -          x**7*
     -           (rat37 + rat39*zeta2 + 
     -             rat38*zeta3) + 
     -          x**8*
     -           (rat40 + rat42*zeta2 + 
     -             rat41*zeta3) + 
     -          x**9*
     -           (rat43 + rat45*zeta2 + 
     -             rat44*zeta3) + 
     -          x**10*
     -           (rat46 + rat48*zeta2 + 
     -             rat47*zeta3) + 
     -          x**11*
     -           (rat49 + rat51*zeta2 + 
     -             rat50*zeta3)))/(x**6*y**6))*
     -   lins(1) + (-(IPi*
     -         (138345 + 438816*y + 
     -           1058456*y**2 + 1612240*y**3 + 
     -           1738520*y**4 + 1118880*y**5 + 
     -           372706*y**6 + 30104*y**7 - 
     -           212*y**8 + 23960*y**9 + 
     -           395660*y**10 + 97400*y**11))/
     -      (15d0*x**6*y**6) + 
     -     (61836*(rat52 + 
     -          x**11*
     -           (0.109045432002932488086767147508d0 - 
     -             (576*zeta2)/5153d0) + 
     -          x**10*
     -           (1.30164629018694611553140565366d0 + 
     -             (2060*zeta2)/5153d0) + zeta2 + 
     -          x**9*
     -           (7.05122151066261293313495913923d0 + 
     -             (57544*zeta2)/5153d0) + 
     -          x*(rat53 + 
     -             (182648*zeta2)/15459d0) + 
     -          x**2*
     -           (rat54 + (290669*zeta2)/5153d0) + 
     -          x**8*
     -           (rat60 + (294804*zeta2)/5153d0) + 
     -          x**3*
     -           (rat55 + (790174*zeta2)/5153d0) + 
     -          x**7*
     -           (rat59 + (2418148*zeta2)/15459d0) 
     -           + x**4*
     -           (rat56 + (4186267*zeta2)/15459d0) 
     -           + x**6*
     -           (rat58 + (4233026*zeta2)/15459d0) 
     -           + x**5*
     -           (rat57 + (1683108*zeta2)/5153d0)))
     -       /(x**6*y**6))*lins(1)**2 - 
     -  ((-171725 - 670400*y - 1813740*y**2 - 
     -       3149960*y**3 - 3866700*y**4 - 
     -       3123920*y**5 - 1630236*y**6 - 
     -       452868*y**7 - 100352*y**8 + 
     -       23960*y**9 + 
     -       20*(31057 - 22680*IPi)*y**10 + 
     -       97400*y**11)*lins(1)**3)/
     -   (45d0*x**6*y**6) + 
     -  ((8899 + 89144*x + 398631*x**2 + 
     -       1054490*x**3 + 1832899*x**4 + 
     -       2186268*x**5 + 1809304*x**6 + 
     -       1020128*x**7 + 369984*x**8 + 
     -       74344*x**9 + 2060*x**10 - 576*x**11)*
     -     lins(1)**4)/(3d0*x**6*y**6) + 
     -  ((IPi*(79715*y + 1464908*x**4*y**3 + 
     -          x*(-79715 + 4198*y**2) - 
     -          2*x**3*y**2*
     -           (377385 + 732454*y**2) + 
     -          x**2*(-4198*y + 754770*y**3)))/
     -      (5d0*x**6*y**6) + 
     -     (241920*(0.00240740740740740740740740740741d0 + 
     -          zeta2 + 10*x**9*zeta2 - 
     -          x**10*zeta2 + 
     -          x*(0.0102949735449735449735449735450d0 + 
     -             10*zeta2) + 
     -          x**8*(rat63 + (2159*zeta2)/48d0) + 
     -          x**2*
     -           (0.0225331790123456790123456790123d0 + 45*zeta2) 
     -           + x**7*
     -           (-0.812804912551440329218106995885d0 + 
     -             (1812869*zeta2)/15120d0) + 
     -          x**3*
     -           (-0.103158068783068783068783068783d0 + 
     -             (113618*zeta2)/945d0) + 
     -          x**6*
     -           (rat62 + (906059*zeta2)/4320d0) + 
     -          x**4*
     -           (rat61 + (794609*zeta2)/3780d0) + 
     -          x**5*
     -           (-3.37302248677248677248677248677d0 + 
     -             (95197*zeta2)/378d0)))/
     -      (x**6*y**6))*lins(1)*lins(2) + 
     -  ((-16*IPi*(1759 + 15344*x + 64395*x**2 + 
     -          166074*x**3 + 284655*x**4 + 
     -          336420*x**5 + 275478*x**6 + 
     -          154308*x**7 + 56700*x**8 + 
     -          12600*x**9 + 3780*x**10))/
     -      (x**6*y**6) + 
     -     (12910*(1 + (1231654*x)/96825d0 + 
     -          (1823638*x**2)/32275d0 + 
     -          (944094*x**3)/6455d0 + 
     -          (4685843*x**4)/19365d0 + 
     -          (5101274*x**5)/19365d0 + 
     -          rat64*x**6 + 
     -          (2390222*x**7)/32275d0 + 
     -          (742234*x**8)/96825d0 - 
     -          (142436*x**9)/19365d0 - 
     -          (67574*x**10)/19365d0 - 
     -          (1948*x**11)/3873d0))/(x**6*y**6))*
     -   lins(1)**2*lins(2) - 
     -  (4*(9581 + 98056*x + 445905*x**2 + 
     -       1194726*x**3 + 2096745*x**4 + 
     -       2521260*x**5 + 2106790*x**6 + 
     -       1207780*x**7 + 454440*x**8 + 
     -       100800*x**9 + 7560*x**10)*lins(1)**3*
     -     lins(2))/(3d0*x**6*y**6) + 
     -  (2*(-6976 - 6472*x + 4720*x**2 + 
     -       7987*x**3 + 3062*x**4 + 630*x**5 + 
     -       17640*x**7)*lins(1)**2*lins(2)**2)/
     -   (x**3*y**6) - 
     -  (24*(131 + 1800*x + 12423*x**2 + 
     -       44650*x**3 + 94131*x**4 + 
     -       129612*x**5 + 123410*x**6 + 
     -       85956*x**7 + 45396*x**8 + 
     -       18056*x**9 + 4660*x**10 + 576*x**11)*
     -     zeta2*lins(3))/(x**6*y**6) + 
     -  (152830*(1 + (2894428*x)/382075d0 + 
     -       rat65*x**2 + rat66*x**3 + 
     -       (9718774*x**4)/76415d0 + rat67*x**5 + 
     -       rat68*x**6 + (2888568*x**7)/76415d0 + 
     -       (1517848*x**8)/382075d0 - 
     -       (284872*x**9)/76415d0 - 
     -       (135148*x**10)/76415d0 - 
     -       (3896*x**11)/15283d0)*lins(1)*lins(3))
     -    /(3d0*x**6*y**6) - 
     -  (8*(1995 + 19072*x + 85659*x**2 + 
     -       231362*x**3 + 409893*x**4 + 
     -       497616*x**5 + 419944*x**6 + 
     -       246132*x**7 + 98298*x**8 + 
     -       25828*x**9 + 4010*x**10 + 288*x**11)*
     -     lins(1)**2*lins(3))/(x**6*y**6) + 
     -  (8*(5539 + 53144*x + 234495*x**2 + 
     -       619674*x**3 + 1078455*x**4 + 
     -       1288980*x**5 + 1069278*x**6 + 
     -       607908*x**7 + 226800*x**8 + 
     -       50400*x**9 + 10080*x**10)*lins(1)*
     -     lins(2)*lins(3))/(x**6*y**6) + 
     -  (152830*(-1 - (2894428*x)/382075d0 + 
     -       rat69*x**2 + rat70*x**3 - 
     -       (9718774*x**4)/76415d0 + rat71*x**5 + 
     -       rat72*x**6 - (2888568*x**7)/76415d0 - 
     -       (1517848*x**8)/382075d0 + 
     -       (284872*x**9)/76415d0 + 
     -       (135148*x**10)/76415d0 + 
     -       (3896*x**11)/15283d0)*lins(4))/
     -   (3d0*x**6*y**6) + 
     -  (32*(105 + 172*x + 609*x**2 + 4562*x**3 + 
     -       12993*x**4 + 21336*x**5 + 
     -       23044*x**6 + 19332*x**7 + 
     -       13248*x**8 + 6928*x**9 + 
     -       2120*x**10 + 288*x**11)*lins(1)*
     -     lins(4))/(x**6*y**6) - 
     -  (32*(499 + 2744*y + 7695*y**2 + 
     -       14874*y**3 + 20055*y**4 + 
     -       18900*y**5 + 10878*y**6 + 
     -       3108*y**7 + 1260*y**10)*lins(1)*
     -     lins(5))/(x**6*y**6) - 
     -  (96*(-92 - 236*x + 1182*x**2 + 
     -       7444*x**3 + 18519*x**4 + 
     -       27678*x**5 + 28133*x**6 + 
     -       20712*x**7 + 11349*x**8 + 
     -       4514*x**9 + 745*x**10 + 144*x**11)*
     -     lins(6))/(x**6*y**6) - 
     -  (24*(x - y)*(341 - 4974*x*y + 
     -       15183*x**2*y**2 - 15246*x**3*y**3 + 
     -       4200*x**4*y**4)*lins(7))/(x**6*y**6)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^4
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_7_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (4*(19200 + 220144*x + 725696*x**2 + 
     -      1236032*x**3 + 1208202*x**4 + 
     -      641456*x**5 + 139953*x**6 - 
     -      5232*x**7 - 4924*x**8 + 766*x**9 - 
     -      1444*x**10))/(x**4*y**7)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^3
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_7_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-5242.57053872053872053872053872d0)
      double precision rat2
      parameter (rat2=-8946.23438552188552188552188552d0)
      double precision rat3
      parameter (rat3=-8772.72503006253006253006253006d0)
      double precision rat4
      parameter (rat4=-4651.63870851370851370851370851d0)
      double precision rat5
      parameter (rat5=-994.53921657046657046657046657d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-4224*(-1 - (10*x)/33d0 - (10*x**2)/33d0 - 
     -       (7108517*x**3)/41580d0 - 
     -       (9640639*x**4)/5940d0 + rat1*x**5 + 
     -       rat2*x**6 + rat3*x**7 + rat4*x**8 + 
     -       rat5*x**9 + (274325*x**10)/4752d0 + 
     -       (10336*x**11)/297d0 - 
     -       (37493*x**12)/23760d0 + 
     -       (74651*x**13)/5940d0 - 
     -       (41*x**14)/495d0 + x**15/594d0))/
     -   (x**7*y**7) + 
     -  (16*(-3304 - 4048*y - 5728*y**2 - 
     -       3680*y**3 - 1664*y**4 - 128*y**5 - 
     -       26*y**7 + 1072*y**8 + 6245*y**9 + 
     -       10608*y**10 + 9788*y**11 - 
     -       3338*y**12 + 2468*y**13)*lins(1))/
     -   (3d0*x**7*y**7)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_7_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=1277.33147868321640582838640043d0)
      double precision rat2
      parameter (rat2=9765.20187593691911015170594232d0)
      double precision rat3
      parameter (rat3=29585.9132613779456736823169635d0)
      double precision rat4
      parameter (rat4=-18677.2239611440906637884511603d0)
      double precision rat5
      parameter (rat5=48589.5090693769862685135216166d0)
      double precision rat6
      parameter (rat6=-18245.3318942255801403130059363d0)
      double precision rat7
      parameter (rat7=46594.1783956518387171382314394d0)
      double precision rat8
      parameter (rat8=-9674.52131678359417161359956827d0)
      double precision rat9
      parameter (rat9=24274.302486872424810902954454d0)
      double precision rat10
      parameter (rat10=4821.67227186287358980289363452d0)
      double precision rat11
      parameter (rat11=-953.547211728728188523115668286d0)
      double precision rat12
      parameter (rat12=-888.666897523535408047010853271d0)
      double precision rat13
      parameter (rat13=-798.5640241050548659830904839d0)
      double precision rat14
      parameter (rat14=-530.385469508904479222881813276d0)
      double precision rat15
      parameter (rat15=192.850584374858930020086173761d0)
      double precision rat16
      parameter (rat16=461.548615509085673688865475229d0)
      double precision rat17
      parameter (rat17=793.55771586500934184581549734d0)
      double precision rat18
      parameter (rat18=1005.78298868065438063102053804d0)
      double precision rat19
      parameter (rat19=949.256302294504592909839244869d0)
      double precision rat20
      parameter (rat20=665.814982365978588480625366782d0)
      double precision rat21
      parameter (rat21=342.211471076654852215636886858d0)
      double precision rat22
      parameter (rat22=125.123257580306345924847118491d0)
      double precision rat23
      parameter (rat23=30.8171544601780538719682828666d0)
      double precision rat24
      parameter (rat24=4.59139691164669371411891908949d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (7412*(1 + (13091*x)/1635d0 + 
     -       (4850921*x**2)/972825d0 - 
     -       (1395611*x**14)/11118d0 - 
     -       (1409471*x**15)/83385d0 + 
     -       x**5*(rat3 - (1195264*zeta2)/109d0) + 
     -       x**4*(rat2 - 
     -          (6164032*zeta2)/1853d0) + 
     -       x**9*(rat10 - 
     -          (3897852*zeta2)/1853d0) + 
     -       x**3*(rat1 - (537600*zeta2)/1853d0) + 
     -       x**13*
     -        (rat14 + (40432*zeta2)/1853d0) + 
     -       x**11*(rat12 + 
     -          (137872*zeta2)/1853d0) + 
     -       x**10*(rat11 + (1344*zeta2)/17d0) + 
     -       x**12*(rat13 + 
     -          (159656*zeta2)/1853d0) + 
     -       x**6*(rat5 + rat4*zeta2) + 
     -       x**7*(rat7 + rat6*zeta2) + 
     -       x**8*(rat9 + rat8*zeta2)))/
     -   (7d0*x**7*y**7) - 
     -  (4563338*(1 + (965525*x)/99203d0 + 
     -       (9671760*x**2)/175513d0 + 
     -       rat15*x**3 + rat16*x**4 + 
     -       rat17*x**5 + rat18*x**6 + 
     -       rat19*x**7 + rat20*x**8 + 
     -       rat21*x**9 + rat22*x**10 + 
     -       rat23*x**11 + rat24*x**12 + 
     -       (717060*x**13)/2.281669d6)*lins(1))/
     -   (15d0*x**7*y**7) + 
     -  (16*(4224 + 46668*x + 259020*x**2 + 
     -       871758*x**3 + 1983543*x**4 + 
     -       3215586*x**5 + 3815218*x**6 + 
     -       3345930*x**7 + 2161691*x**8 + 
     -       1011752*x**9 + 331602*x**10 + 
     -       71660*x**11 + 9097*x**12 + 512*x**13)
     -      *lins(1)**2)/(x**7*y**7) + 
     -  (32*(9600 + 110072*x + 362848*x**2 + 
     -       618016*x**3 + 603668*x**4 + 
     -       320169*x**5 + 71018*x**6)*lins(1)*
     -     lins(2))/(x**4*y**7)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_7_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=4)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=9.315676944d12)
      double precision rat2
      parameter (rat2=-1.76682812198645088609676458527d-7)
      double precision rat3
      parameter (rat3=-3.29766695267229095101175075699d-8)
      double precision rat4
      parameter (rat4=2.9256563694875148626222371057d-7)
      double precision rat5
      parameter (rat5=-1.92241330833910177438773721964d-6)
      double precision rat6
      parameter (rat6=-3.78104996681817093291422322212d-7)
      double precision rat7
      parameter (rat7=1.20439658873806066502675159243d-6)
      double precision rat8
      parameter (rat8=-6.33465905785207458062892367871d-6)
      double precision rat9
      parameter (rat9=-1.24640818587837023645074139445d-6)
      double precision rat10
      parameter (rat10=2.7391162500565706077259663546d-6)
      double precision rat11
      parameter (rat11=-0.0000108420705520192056443930143513d0)
      double precision rat12
      parameter (rat12=-2.12292806189866517122966474566d-6)
      double precision rat13
      parameter (rat13=4.43648648972205082233892426359d-6)
      double precision rat14
      parameter (rat14=-0.0000106316269420165050489948540642d0)
      double precision rat15
      parameter (rat15=-2.0850598530588116968088798078d-6)
      double precision rat16
      parameter (rat16=6.84562443791244181942677086339d-6)
      double precision rat17
      parameter (rat17=-5.64601891464769418172178421439d-6)
      double precision rat18
      parameter (rat18=-1.11451524805044806628923391313d-6)
      double precision rat19
      parameter (rat19=0.0000111060474255687228854723707364d0)
      double precision rat20
      parameter (rat20=-1.22756223189102262114478842118d-6)
      double precision rat21
      parameter (rat21=-2.49715829991109232265364826073d-7)
      double precision rat22
      parameter (rat22=0.0000154623515245985485434268070527d0)
      double precision rat23
      parameter (rat23=8.98614244603199284150702081281d-9)
      double precision rat24
      parameter (rat24=3.23199986943070904613656884522d-8)
      double precision rat25
      parameter (rat25=0.0000151671392937199423193733683303d0)
      double precision rat26
      parameter (rat26=-1.52860603535330153458357196548d-9)
      double precision rat27
      parameter (rat27=8.45714170570747950144888579554d-9)
      double precision rat28
      parameter (rat28=9.41041050273704108113724491557d-6)
      double precision rat29
      parameter (rat29=-1.63921528105751796023209110546d-8)
      double precision rat30
      parameter (rat30=1.85342623019152219325822941505d-8)
      double precision rat31
      parameter (rat31=3.2689879723540688771589302728d-6)
      double precision rat32
      parameter (rat32=2.48012035398895215274008754849d-9)
      double precision rat33
      parameter (rat33=2.15292996102849828494439040307d-8)
      double precision rat34
      parameter (rat34=4.69786602777747614756221340391d-7)
      double precision rat35
      parameter (rat35=-1.54533730782964994586357017444d-8)
      double precision rat36
      parameter (rat36=2.5763023067752273054779302789d-11)
      double precision rat37
      parameter (rat37=-2.09342641329958950399487774015d-9)
      double precision rat38
      parameter (rat38=3.43506974236696974063724037187d-12)
      double precision rat39
      parameter (rat39=0.39983069234358558752576287388d0)
      double precision rat40
      parameter (rat40=-1.52587371799594594849881079897d0)
      double precision rat41
      parameter (rat41=-37.9960956470897161975037168436d0)
      double precision rat42
      parameter (rat42=-206.202139231606740632011390134d0)
      double precision rat43
      parameter (rat43=-636.146802084297571662192600821d0)
      double precision rat44
      parameter (rat44=-1318.81997810903496824074441403d0)
      double precision rat45
      parameter (rat45=-1945.40299319413037824590171161d0)
      double precision rat46
      parameter (rat46=-2093.86947159739578512141689037d0)
      double precision rat47
      parameter (rat47=-1653.29274584220432595883859422d0)
      double precision rat48
      parameter (rat48=-948.269994445987225770619272424d0)
      double precision rat49
      parameter (rat49=-384.056985019593322842420315345d0)
      double precision rat50
      parameter (rat50=-103.867764124163041130550155821d0)
      double precision rat51
      parameter (rat51=-16.7516151763399055817828381005d0)
      double precision rat52
      parameter (rat52=66.0093109824011527532889859864d0)
      double precision rat53
      parameter (rat53=227.822546488542104452746468202d0)
      double precision rat54
      parameter (rat54=531.807405574009791704701725495d0)
      double precision rat55
      parameter (rat55=885.685725651742220120168322238d0)
      double precision rat56
      parameter (rat56=1080.78658380435541756061818356d0)
      double precision rat57
      parameter (rat57=976.10127230079737820510464185d0)
      double precision rat58
      parameter (rat58=650.169560190148746180667448402d0)
      double precision rat59
      parameter (rat59=313.987695632942179767268325138d0)
      double precision rat60
      parameter (rat60=106.098646503286433297840752263d0)
      double precision rat61
      parameter (rat61=23.5273744189570409071397268174d0)
      double precision rat62
      parameter (rat62=36.4540633702444907356649490187d0)
      double precision rat63
      parameter (rat63=60.9907249518379876893197800993d0)
      double precision rat64
      parameter (rat64=32.3894358701270224129559728726d0)
      double precision rat65
      parameter (rat65=7.18618271806036305542938587561d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-399168*IPi*x**5*zeta2)/y**7 + 
     -  (rat1*(rat4 + rat2*zeta2 + 
     -       x**11*(rat35 + rat36*zeta2) + 
     -       x**12*(rat37 + rat38*zeta2) + 
     -       rat3*zeta3 + 
     -       x**3*(rat13 + rat11*zeta2 + 
     -          rat12*zeta3) + 
     -       x**4*(rat16 + rat14*zeta2 + 
     -          rat15*zeta3) + 
     -       x**5*(rat19 + rat17*zeta2 + 
     -          rat18*zeta3) + 
     -       x**6*(rat22 + rat20*zeta2 + 
     -          rat21*zeta3) + 
     -       x**7*(rat25 + rat24*zeta2 + 
     -          rat23*zeta3) + 
     -       x**8*(rat28 + rat26*zeta2 + 
     -          rat27*zeta3) + 
     -       x**9*(rat31 + rat30*zeta2 + 
     -          rat29*zeta3) + 
     -       x**10*(rat34 + rat33*zeta2 + 
     -          rat32*zeta3) + 
     -       x*(rat7 + rat5*zeta2 + rat6*zeta3) + 
     -       x**2*(rat10 + rat8*zeta2 + 
     -          rat9*zeta3)))/(x**4*y**7) + 
     -  (115232*(rat39 + 
     -       x**6*(rat45 - 
     -          (2577970*zeta2)/3601d0) + 
     -       x**5*(rat44 - 
     -          (2354216*zeta2)/3601d0) + 
     -       x**7*(rat46 - 
     -          (2014214*zeta2)/3601d0) + 
     -       x**4*(rat43 - 
     -          (1548226*zeta2)/3601d0) + 
     -       x**8*(rat47 - 
     -          (1071079*zeta2)/3601d0) + 
     -       x**3*(rat42 - (55670*zeta2)/277d0) + 
     -       x**9*(rat48 - 
     -          (333091*zeta2)/3601d0) + 
     -       x**2*(rat41 - 
     -          (229878*zeta2)/3601d0) + 
     -       x*(rat40 - (45385*zeta2)/3601d0) + 
     -       x**10*
     -        (rat49 - (16968*zeta2)/3601d0) + 
     -       x**13*(-1.20823478817612391619611836218d0 + 
     -          (1628*zeta2)/3601d0) - zeta2 + 
     -       x**12*
     -        (rat51 + (24880*zeta2)/3601d0) + 
     -       x**11*(rat50 + (31556*zeta2)/3601d0))*
     -     lins(1))/(x**7*y**7) + 
     -  ((6529064*(1 + (9452267*x)/816133d0 + 
     -          rat52*x**2 + rat53*x**3 + 
     -          rat54*x**4 + rat55*x**5 + 
     -          rat56*x**6 + rat57*x**7 + 
     -          rat58*x**8 + rat59*x**9 + 
     -          rat60*x**10 + rat61*x**11 + 
     -          (7391650*x**12)/2.448399d6 + 
     -          (402500*x**13)/2.448399d6))/
     -      (35d0*x**7*y**7) - 
     -     (66528*IPi*y**5)/x**7)*lins(1)**2 - 
     -  (8*(264*x**9 - 162*x*y**7 + 
     -       121*x**2*y**7 + 
     -       8*x**7*y*(-35 + 33*y) + 
     -       11*y**7*(-1 + 13*y**2 + 420*y**5))*
     -     lins(1)**3)/(x**7*y**7) + 
     -  (3648400*(1 + (5045599*x)/456050d0 + 
     -       rat62*x**2 + (2028939*x**3)/32575d0 + 
     -       rat63*x**4 + rat64*x**5 + rat65*x**6)
     -      *lins(1)*lins(2))/(3d0*x**4*y**7) + 
     -  (32*(-727 - 1738*y - 4272*y**2 - 
     -       7748*y**3 - 11262*y**4 - 
     -       12328*y**5 - 9576*y**6 - 4908*y**7 - 
     -       1479*y**8 - 198*y**9 + 2079*y**12)*
     -     lins(1)**2*lins(2))/(x**7*y**7) + 
     -  (133056*y**5*lins(1)*lins(3))/x**7 - 
     -  (133056*y**5*lins(4))/x**7
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function TwoLoop_HelAmpmmpp_HE_7_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=7)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-90.6096022683530950726717922485d0)
      double precision rat2
      parameter (rat2=-640.916607223444975299189206975d0)
      double precision rat3
      parameter (rat3=-231.878728097299525870954442383d0)
      double precision rat4
      parameter (rat4=-1935.86245849072760684627653637d0)
      double precision rat5
      parameter (rat5=-351.216908526878764974003069241d0)
      double precision rat6
      parameter (rat6=-3262.13628643402881167431431982d0)
      double precision rat7
      parameter (rat7=-1465.76569865319865319865319865d0)
      double precision rat8
      parameter (rat8=-504.637928290844957511624178291d0)
      double precision rat9
      parameter (rat9=-3407.89618129615045520792740315d0)
      double precision rat10
      parameter (rat10=-1463.49936267436267436267436267d0)
      double precision rat11
      parameter (rat11=-1183.25564446906708811470716233d0)
      double precision rat12
      parameter (rat12=-2696.34521321491559586797682036d0)
      double precision rat13
      parameter (rat13=-2450.59424331345644113062588389d0)
      double precision rat14
      parameter (rat14=-4197.77070044621383907098192812d0)
      double precision rat15
      parameter (rat15=-1672.95533459641517183730497627d0)
      double precision rat16
      parameter (rat16=-4199.62513878868045534712201379d0)
      double precision rat17
      parameter (rat17=-1322.94737065900502750937369229d0)
      double precision rat18
      parameter (rat18=-2627.42957301186467853134519801d0)
      double precision rat19
      parameter (rat19=-823.609859674425242145385758387d0)
      double precision rat20
      parameter (rat20=-906.290001603334936668270001603d0)
      double precision rat21
      parameter (rat21=-282.574532677416304825601877756d0)
      double precision rat22
      parameter (rat22=-122.080098980880230880230880231d0)
      double precision rat23
      parameter (rat23=-37.9632547806814875621564963969d0)
      double precision rat24
      parameter (rat24=1.96999779511622269558777495285d0)
      double precision rat25
      parameter (rat25=0.254268972756985323122889260455d0)
      double precision rat26
      parameter (rat26=4.0364681857d10)
      double precision rat27
      parameter (rat27=1.98458265824670535437137627627d-6)
      double precision rat28
      parameter (rat28=8.36850396088085288039583618909d-6)
      double precision rat29
      parameter (rat29=0.0000106287435690048606693697723126d0)
      double precision rat30
      parameter (rat30=-4.92152451820007982904559949171d-6)
      double precision rat31
      parameter (rat31=0.0000600834383480458113784106671123d0)
      double precision rat32
      parameter (rat32=0.000104604713966493631665637532539d0)
      double precision rat33
      parameter (rat33=-4.18143386381670643592356476278d-6)
      double precision rat34
      parameter (rat34=0.000199870810217166420696596275728d0)
      double precision rat35
      parameter (rat35=0.000592269055524690494032152678587d0)
      double precision rat36
      parameter (rat36=0.000135193263407830224798659105326d0)
      double precision rat37
      parameter (rat37=0.000464816224458152940516983458025d0)
      double precision rat38
      parameter (rat38=0.00199937322151851389977870970638d0)
      double precision rat39
      parameter (rat39=0.000795490925633664987103018091139d0)
      double precision rat40
      parameter (rat40=0.000839104856363535137914955021809d0)
      double precision rat41
      parameter (rat41=0.00453598124837562992612489996666d0)
      double precision rat42
      parameter (rat42=0.00122732803365235748692245268138d0)
      double precision rat43
      parameter (rat43=0.00228967631474028986547847573191d0)
      double precision rat44
      parameter (rat44=0.00730701520316456782819528218832d0)
      double precision rat45
      parameter (rat45=0.00145530909925690092170336836584d0)
      double precision rat46
      parameter (rat46=0.00419888551251580019771804705263d0)
      double precision rat47
      parameter (rat47=0.00858816688381461457780070916019d0)
      double precision rat48
      parameter (rat48=0.0013610671653338331601802994732d0)
      double precision rat49
      parameter (rat49=0.00531105988333482418126306912649d0)
      double precision rat50
      parameter (rat50=0.00742880087751759138062862894423d0)
      double precision rat51
      parameter (rat51=0.000962306903915502235440184158599d0)
      double precision rat52
      parameter (rat52=0.00470206703653445321889137663221d0)
      double precision rat53
      parameter (rat53=0.00478126254644159728660590752942d0)
      double precision rat54
      parameter (rat54=0.00048946162140324680407747164767d0)
      double precision rat55
      parameter (rat55=0.00213108757563717517497390540625d0)
      double precision rat56
      parameter (rat56=0.00308201522978180792877860663547d0)
      double precision rat57
      parameter (rat57=0.000166786341328554319592783805595d0)
      double precision rat58
      parameter (rat58=0.000661742859627365054596818149276d0)
      double precision rat59
      parameter (rat59=0.00139908976697507910531215495251d0)
      double precision rat60
      parameter (rat60=0.0000336275237099405881854264227006d0)
      double precision rat61
      parameter (rat61=0.000129329695165049148872324283113d0)
      double precision rat62
      parameter (rat62=0.000428481051692817095038830676875d0)
      double precision rat63
      parameter (rat63=2.8610857072467360144351409582d-6)
      double precision rat64
      parameter (rat64=0.0000132551521623652766351097367451d0)
      double precision rat65
      parameter (rat65=0.0000962019226054320193226538676346d0)
      double precision rat66
      parameter (rat66=-5.4704996714271225769328371604d-8)
      double precision rat67
      parameter (rat67=4.05899396359510863467475192195d-7)
      double precision rat68
      parameter (rat68=0.0000108119601903328172026953214525d0)
      double precision rat69
      parameter (rat69=0.288682468343433448900095990587d0)
      double precision rat70
      parameter (rat70=2.8785310287565218314496443014d0)
      double precision rat71
      parameter (rat71=16.1737828726328946560924244084d0)
      double precision rat72
      parameter (rat72=55.2813014305109200621362320574d0)
      double precision rat73
      parameter (rat73=128.621643907096544986233235634d0)
      double precision rat74
      parameter (rat74=215.646286567118187536138889342d0)
      double precision rat75
      parameter (rat75=267.932434891089396056850664593d0)
      double precision rat76
      parameter (rat76=249.613761983610390599085357564d0)
      double precision rat77
      parameter (rat77=174.298796144490303448850896607d0)
      double precision rat78
      parameter (rat78=90.0914144874576529252788245594d0)
      double precision rat79
      parameter (rat79=33.5188624973037443061423876772d0)
      double precision rat80
      parameter (rat80=1.3107282110819281083070051895d0)
      double precision rat81
      parameter (rat81=9.07743832372822315015278518799d0)
      double precision rat82
      parameter (rat82=46.5211783640300014550970285155d0)
      double precision rat83
      parameter (rat83=148.086698384842298347803464895d0)
      double precision rat84
      parameter (rat84=318.06288805894465780968212744d0)
      double precision rat85
      parameter (rat85=477.983444346948044217312127131d0)
      double precision rat86
      parameter (rat86=509.714936438716163482355846183d0)
      double precision rat87
      parameter (rat87=380.410092200238988663471332384d0)
      double precision rat88
      parameter (rat88=187.353717684720158385106861444d0)
      double precision rat89
      parameter (rat89=48.9962213334861920110763749565d0)
      double precision rat90
      parameter (rat90=16.336810164130426557498619004d0)
      double precision rat91
      parameter (rat91=95.751089932836571151831724964d0)
      double precision rat92
      parameter (rat92=326.086782544210789172991502145d0)
      double precision rat93
      parameter (rat93=732.424859441093900159665836808d0)
      double precision rat94
      parameter (rat94=1150.94435305067687720864768333d0)
      double precision rat95
      parameter (rat95=1295.92356925032727712994983012d0)
      double precision rat96
      parameter (rat96=1048.02034080062093470996922352d0)
      double precision rat97
      parameter (rat97=595.460852466388557678067405576d0)
      double precision rat98
      parameter (rat98=223.305138818472807621584399664d0)
      double precision rat99
      parameter (rat99=45.6395154028346361359354072236d0)
      double precision rat100
      parameter (rat100=-0.00796467366407842598318788794979d0)
      double precision rat101
      parameter (rat101=-0.00496023668004960521967324688413d0)
      double precision rat102
      parameter (rat102=-0.752048995243439687884132328577d0)
      double precision rat103
      parameter (rat103=-4.46365441069806149171228536308d0)
      double precision rat104
      parameter (rat104=-12.3832647287309985722684135383d0)
      double precision rat105
      parameter (rat105=-19.4397750702413400826099238798d0)
      double precision rat106
      parameter (rat106=-18.2345186429951169180194123505d0)
      double precision rat107
      parameter (rat107=-9.52275608895754870811560153964d0)
      double precision rat108
      parameter (rat108=-2.13944936096332071388760731164d0)
      double precision rat109
      parameter (rat109=15.2575763101399411421261703586d0)
      double precision rat110
      parameter (rat110=84.510872173938111358986141343d0)
      double precision rat111
      parameter (rat111=279.319493779216633861720465736d0)
      double precision rat112
      parameter (rat112=610.858742144787035290758799766d0)
      double precision rat113
      parameter (rat113=928.102040227998717672192971579d0)
      double precision rat114
      parameter (rat114=996.270953920599179129593951525d0)
      double precision rat115
      parameter (rat115=746.743243032031451069225903994d0)
      double precision rat116
      parameter (rat116=368.717746868074101319873753924d0)
      double precision rat117
      parameter (rat117=96.6391650420490292410335173119d0)
      double precision rat118
      parameter (rat118=-5.30989646557529872645954224509d0)
      double precision rat119
      parameter (rat119=-13.658044991922932515588861809d0)
      double precision rat120
      parameter (rat120=-4.36935486615964008684540760276d0)
      double precision rat121
      parameter (rat121=967.571171512453830855194620703d0)
      double precision rat122
      parameter (rat122=-9.07743832372822315015278518799d0)
      double precision rat123
      parameter (rat123=-46.5211783640300014550970285155d0)
      double precision rat124
      parameter (rat124=-148.086698384842298347803464895d0)
      double precision rat125
      parameter (rat125=-318.06288805894465780968212744d0)
      double precision rat126
      parameter (rat126=-477.983444346948044217312127131d0)
      double precision rat127
      parameter (rat127=-509.714936438716163482355846183d0)
      double precision rat128
      parameter (rat128=-380.410092200238988663471332384d0)
      double precision rat129
      parameter (rat129=-187.353717684720158385106861444d0)
      double precision rat130
      parameter (rat130=-48.9962213334861920110763749565d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (152064*IPi*(1 + (10*x)/33d0 + 
     -       (10*x**2)/33d0 + 
     -       (408313*x**3)/15120d0 + 
     -       (1441127*x**4)/47520d0 - 
     -       (123091*x**5)/23760d0 - 
     -       (465671*x**6)/23760d0 - 
     -       (539939*x**7)/66528d0 - 
     -       (1556693*x**8)/665280d0 - 
     -       (1727963*x**9)/1.33056d6 - 
     -       (25295*x**10)/19008d0 - 
     -       (2663*x**11)/2376d0 - 
     -       (89711*x**12)/19008d0 - 
     -       (20767*x**13)/19008d0 - 
     -       (5*x**14)/6336d0 - x**15/9504d0)*zeta2)
     -    /(x**7*y**7) + 
     -  (25344*(x**15*
     -        (rat25 + (757957*zeta2)/997920d0 + 
     -          zeta3/594d0) + 
     -       x**14*(rat24 + 
     -          (757957*zeta2)/133056d0 + 
     -          (5*zeta3)/396d0) - zeta3 - 
     -       (10*x*zeta3)/33d0 - 
     -       (10*x**2*zeta3)/33d0 + 
     -       x**12*(rat21 + rat20*zeta2 - 
     -          (16813*zeta3)/792d0 - 
     -          (5789*zeta4)/528d0) + 
     -       x**13*(rat23 + rat22*zeta2 + 
     -          (7099*zeta3)/1584d0 + 
     -          (371*zeta4)/132d0) + 
     -       x**3*(rat1 - 
     -          (6203471*zeta2)/71280d0 - 
     -          (276571*zeta3)/5040d0 + 
     -          (300*zeta4)/11d0) + 
     -       x**11*(rat19 + rat18*zeta2 - 
     -          (10381*zeta3)/528d0 + 
     -          (4181*zeta4)/132d0) + 
     -       x**10*(rat17 + rat16*zeta2 + 
     -          (961*zeta3)/594d0 + 37*zeta4) + 
     -       x**9*(rat15 + rat14*zeta2 - 
     -          (966853*zeta3)/5544d0 + 
     -          (10723*zeta4)/48d0) + 
     -       x**4*(rat2 + rat3*zeta2 - 
     -          (1256197*zeta3)/4320d0 + 
     -          (13759*zeta4)/44d0) + 
     -       x**8*(rat13 + rat12*zeta2 - 
     -          (1858009*zeta3)/2376d0 + 
     -          (970061*zeta4)/1056d0) + 
     -       x**5*(rat4 + rat5*zeta2 - 
     -          (6778879*zeta3)/7920d0 + 
     -          (11339*zeta4)/11d0) + 
     -       x**7*(rat9 + rat11*zeta2 + 
     -          rat10*zeta3 + (225691*zeta4)/132d0)
     -         + x**6*
     -        (rat6 + rat8*zeta2 + rat7*zeta3 + 
     -          (19313*zeta4)/11d0)))/(x**7*y**7) 
     -   + ((-288*IPi*
     -        (529 + 3190*y + 9952*y**2 + 
     -          21404*y**3 + 32954*y**4 + 
     -          36920*y**5 + 28728*y**6 + 
     -          15336*y**7 + 5346*y**8 + 
     -          1188*y**9 + 308*y**12)*zeta2)/
     -      (x**7*y**7) + 
     -     (rat26*(rat29 + rat27*zeta2 + 
     -          rat28*zeta3 + 
     -          x*(rat31 + rat30*zeta2 + 
     -             rat32*zeta3) + 
     -          x**2*
     -           (rat34 + rat33*zeta2 + 
     -             rat35*zeta3) + 
     -          x**3*
     -           (rat37 + rat36*zeta2 + 
     -             rat38*zeta3) + 
     -          x**4*
     -           (rat40 + rat39*zeta2 + 
     -             rat41*zeta3) + 
     -          x**5*
     -           (rat42 + rat43*zeta2 + 
     -             rat44*zeta3) + 
     -          x**6*
     -           (rat45 + rat46*zeta2 + 
     -             rat47*zeta3) + 
     -          x**7*
     -           (rat48 + rat49*zeta2 + 
     -             rat50*zeta3) + 
     -          x**8*
     -           (rat51 + rat53*zeta2 + 
     -             rat52*zeta3) + 
     -          x**9*
     -           (rat54 + rat56*zeta2 + 
     -             rat55*zeta3) + 
     -          x**10*
     -           (rat57 + rat59*zeta2 + 
     -             rat58*zeta3) + 
     -          x**11*
     -           (rat60 + rat62*zeta2 + 
     -             rat61*zeta3) + 
     -          x**12*
     -           (rat63 + rat65*zeta2 + 
     -             rat64*zeta3) + 
     -          x**13*
     -           (rat66 + rat68*zeta2 + 
     -             rat67*zeta3)))/(x**7*y**7))*
     -   lins(1) + ((-2267890*IPi*
     -        (1 + rat81*x + rat82*x**2 + 
     -          rat83*x**3 + rat84*x**4 + 
     -          rat85*x**5 + rat86*x**6 + 
     -          rat87*x**7 + rat88*x**8 + 
     -          rat89*x**9 - 
     -          (3057586*x**10)/1.133945d6 - 
     -          (7864682*x**11)/1.133945d6 - 
     -          (2515996*x**12)/1.133945d6 - 
     -          (290052*x**13)/1.133945d6))/
     -      (21d0*x**7*y**7) + 
     -     (280224*(rat69 + 
     -          x**13*
     -           (0.0931115425120221283290827655336d0 - 
     -             (302*zeta2)/2919d0) + 
     -          x**12*
     -           (rat80 + (815*zeta2)/2919d0) + 
     -          zeta2 + 
     -          x**11*
     -           (8.48687678428685622930227246774d0 + 
     -             (36478*zeta2)/2919d0) + 
     -          x*(rat70 + (83347*zeta2)/5838d0) + 
     -          x**10*
     -           (rat79 + (77788*zeta2)/973d0) + 
     -          x**2*
     -           (rat71 + (81047*zeta2)/973d0) + 
     -          x**9*
     -           (rat78 + (1637723*zeta2)/5838d0) 
     -           + x**3*
     -           (rat72 + (275969*zeta2)/973d0) + 
     -          x**4*
     -           (rat73 + (1881788*zeta2)/2919d0) 
     -           + x**8*
     -           (rat77 + (3766247*zeta2)/5838d0) 
     -           + x**5*
     -           (rat74 + (3027778*zeta2)/2919d0) 
     -           + x**7*
     -           (rat76 + (3036421*zeta2)/2919d0) 
     -           + x**6*
     -           (rat75 + (1181553*zeta2)/973d0)))/
     -      (x**7*y**7))*lins(1)**2 + 
     -  ((-264302*(1 + rat90*x + rat91*x**2 + 
     -          rat92*x**3 + rat93*x**4 + 
     -          rat94*x**5 + rat95*x**6 + 
     -          rat96*x**7 + rat97*x**8 + 
     -          rat98*x**9 + rat99*x**10 + 
     -          (262466*x**11)/660755d0 - 
     -          (1307132*x**12)/660755d0 - 
     -          (41436*x**13)/132151d0))/
     -      (9d0*x**7*y**7) + (44352*IPi*y**5)/x**7
     -     )*lins(1)**3 + 
     -  (4*(9868 + 118807*x + 650010*x**2 + 
     -       2152526*x**3 + 4817404*x**4 + 
     -       7675028*x**5 + 8918166*x**6 + 
     -       7601618*x**7 + 4704185*x**8 + 
     -       2049695*x**9 + 588696*x**10 + 
     -       95132*x**11 + 1630*x**12 - 604*x**13)
     -      *lins(1)**4)/(3d0*x**7*y**7) + 
     -  ((4*IPi*(1728593*y + 68016*x**5*y**4 - 
     -          4*x**4*y**3*
     -           (39893 + 17004*y**2) + 
     -          4*x**3*y**2*
     -           (-2166937 + 39893*y**2) - 
     -          x*(1728593 + 571242*y**2) + 
     -          x**2*(571242*y + 8667748*y**3)))/
     -      (105d0*x**7*y**7) - 
     -     (1064448*(-0.000994747646533360819075104789391d0 + 
     -          x**6*
     -           (rat105 - (268910*zeta2)/297d0) + 
     -          x**5*
     -           (rat104 - (1623890*zeta2)/2079d0) 
     -           + x**7*
     -           (rat106 - (919351*zeta2)/1188d0) 
     -           + x**4*
     -           (rat103 - (2044451*zeta2)/4158d0) 
     -           + x**8*
     -           (rat107 - (1794013*zeta2)/3696d0) 
     -           + x**3*
     -           (rat102 - (152260*zeta2)/693d0) + 
     -          x**9*
     -           (rat108 - (3623729*zeta2)/16632d0)
     -            + x**2*(rat101 - 66*zeta2) + 
     -          x*(rat100 - 12*zeta2) - zeta2 - 
     -          66*x**10*zeta2 - 12*x**11*zeta2 + 
     -          x**12*zeta2))/(x**7*y**7))*
     -   lins(1)*lins(2) + 
     -  ((-64*IPi*(1915 + 19822*x + 101428*x**2 + 
     -          326324*x**3 + 719024*x**4 + 
     -          1134632*x**5 + 1309392*x**6 + 
     -          1113048*x**7 + 691416*x**8 + 
     -          306108*x**9 + 91476*x**10 + 
     -          16632*x**11 + 4158*x**12))/
     -      (x**7*y**7) + 
     -     (1919426*(1 + rat109*x + rat110*x**2 + 
     -          rat111*x**3 + rat112*x**4 + 
     -          rat113*x**5 + rat114*x**6 + 
     -          rat115*x**7 + rat116*x**8 + 
     -          rat117*x**9 + rat118*x**10 + 
     -          rat119*x**11 + rat120*x**12 - 
     -          (483420*x**13)/959713d0))/
     -      (35d0*x**7*y**7))*lins(1)**2*lins(2) - 
     -  (168944*(1 + (129866*x)/10559d0 + 
     -       (721856*x**2)/10559d0 + 
     -       (2417956*x**3)/10559d0 + 
     -       (5455606*x**4)/10559d0 + 
     -       (8744776*x**5)/10559d0 + 
     -       rat121*x**6 + 
     -       (8765544*x**7)/10559d0 + 
     -       (5482002*x**8)/10559d0 + 
     -       (2437380*x**9)/10559d0 + 
     -       (731808*x**10)/10559d0 + 
     -       (133056*x**11)/10559d0 + 
     -       (8316*x**12)/10559d0)*lins(1)**3*
     -     lins(2))/(3d0*x**7*y**7) + 
     -  (8*(9600 + 110072*x + 362848*x**2 + 
     -       618016*x**3 + 603260*x**4 + 
     -       319563*x**5 + 70622*x**6 + 19404*x**9
     -       )*lins(1)**2*lins(2)**2)/(x**4*y**7) 
     -   - (96*(98 + 1589*x + 14958*x**2 + 
     -       69946*x**3 + 192020*x**4 + 
     -       350332*x**5 + 454218*x**6 + 
     -       438454*x**7 + 324037*x**8 + 
     -       185989*x**9 + 82128*x**10 + 
     -       26836*x**11 + 5762*x**12 + 604*x**13)
     -      *zeta2*lins(3))/(x**7*y**7) - 
     -  (4535780*(-1 + rat122*x + rat123*x**2 + 
     -       rat124*x**3 + rat125*x**4 + 
     -       rat126*x**5 + rat127*x**6 + 
     -       rat128*x**7 + rat129*x**8 + 
     -       rat130*x**9 + 
     -       (3057586*x**10)/1.133945d6 + 
     -       (7864682*x**11)/1.133945d6 + 
     -       (2515996*x**12)/1.133945d6 + 
     -       (290052*x**13)/1.133945d6)*lins(1)*
     -     lins(3))/(21d0*x**7*y**7) - 
     -  (16*(4323 + 49131*x + 268846*x**2 + 
     -       904470*x**3 + 2054494*x**4 + 
     -       3314484*x**5 + 3898050*x**6 + 
     -       3381022*x**7 + 2158903*x**8 + 
     -       1000297*x**9 + 326064*x**10 + 
     -       71188*x**11 + 9458*x**12 + 604*x**13)
     -      *lins(1)**2*lins(3))/(x**7*y**7) + 
     -  (32*(6073 + 69718*x + 375856*x**2 + 
     -       1241084*x**3 + 2777234*x**4 + 
     -       4427768*x**5 + 5151384*x**6 + 
     -       4406184*x**7 + 2749626*x**8 + 
     -       1220868*x**9 + 365904*x**10 + 
     -       66528*x**11 + 11088*x**12)*lins(1)*
     -     lins(2)*lins(3))/(x**7*y**7) + 
     -  (4535780*(-1 + rat122*x + rat123*x**2 + 
     -       rat124*x**3 + rat125*x**4 + 
     -       rat126*x**5 + rat127*x**6 + 
     -       rat128*x**7 + rat129*x**8 + 
     -       rat130*x**9 + 
     -       (3057586*x**10)/1.133945d6 + 
     -       (7864682*x**11)/1.133945d6 + 
     -       (2515996*x**12)/1.133945d6 + 
     -       (290052*x**13)/1.133945d6)*lins(4))/
     -   (21d0*x**7*y**7) + 
     -  (64*(165 - 765*x - 5582*x**2 - 
     -       10290*x**3 - 3716*x**4 + 
     -       21348*x**5 + 56058*x**6 + 
     -       87886*x**7 + 100693*x**8 + 
     -       85537*x**9 + 51636*x**10 + 
     -       21292*x**11 + 5300*x**12 + 604*x**13)
     -      *lins(1)*lins(4))/(x**7*y**7) - 
     -  (128*(529 + 3190*y + 9952*y**2 + 
     -       21404*y**3 + 32954*y**4 + 
     -       36920*y**5 + 28728*y**6 + 
     -       15336*y**7 + 5346*y**8 + 1188*y**9 + 
     -       1386*y**12)*lins(1)*lins(5))/
     -   (x**7*y**7) - 
     -  (96*(-431 - 1601*x + 5006*x**2 + 
     -       48542*x**3 + 159066*x**4 + 
     -       313412*x**5 + 425490*x**6 + 
     -       423118*x**7 + 318691*x**8 + 
     -       184801*x**9 + 82128*x**10 + 
     -       26836*x**11 + 3914*x**12 + 604*x**13)
     -      *lins(6))/(x**7*y**7) + 
     -  (96*(x - y)*(-395 + 7108*x*y - 
     -       29708*x**2*y**2 + 48828*x**3*y**3 - 
     -       31746*x**4*y**4 + 5544*x**5*y**5)*
     -     lins(7))/(x**7*y**7)
      return
      end


      ! +-+-: (-m2/s)^0*(log(-m2/s))^0
      function TwoLoop_HelAmppmpm_HE_0_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=8)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(8)=dcmplx(Li4(-y),0d0)
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(-180*(3 - 8*x**2 - 4*x**3 - 8*x**4 + 
     -         3*x**6)*zeta2 + 
     -      1080*IPi*x**2*(-1 + 5*x)*y*zeta2 + 
     -      180*x**2*y*(-y + 2*(3 + x)*zeta3) + 
     -      90*x**2*(149 + 183*x**2)*zeta4)/
     -   (45d0*x**2*y**2) - 
     -  (2*(6*y*(26 + 9*y)*zeta2 - 
     -       24*IPi*(-3 + 2*(-1 + x)*y)*zeta2 + 
     -       6*(-2*y - 2*y**2 + y**3 + 
     -          4*x**2*zeta3))*lins(1))/(3d0*y**2) 
     -   + (2*(3*(2 + 4*x - x**2 + x**4) - 
     -       6*IPi*(y + 3*x*y) - 
     -       6*(-1 + x**2)*zeta2)*lins(1)**2)/
     -   (3d0*y**2) + 
     -  ((-8*IPi*x**3 - 2*x*y*(10 + 9*y))*
     -     lins(1)**3)/(3d0*x*y**2) + 
     -  ((1 + 3*x**2)*lins(1)**4)/(3d0*y**2) - 
     -  (4*(3 + 3*x**4 - 
     -       6*x**2*(-5 + 18*y*zeta2) + 
     -       2*x*(9 + 18*IPi*zeta2 + 30*y*zeta2 - 
     -          6*zeta3) + 
     -       6*x**3*(3 + 6*IPi*zeta2 - 2*zeta3))*
     -     lins(2))/(3d0*x*y**2) - 
     -  (4*(-4*IPi*(-1 + x)*y + 
     -       y**2*(2 + 4*y + y**2) + 
     -       12*(-2 + (-1 + x)*y)*zeta2)*lins(1)*
     -     lins(2))/y**2 + 
     -  (2*(3 - 24*x**3 + 3*x**6 + 
     -       x**4*(-15 - 12*IPi + 48*zeta2) + 
     -       x**2*(-15 + 12*IPi + 48*zeta2))*
     -     lins(2)**2)/(3d0*x**2*y**2) + 
     -  (8*(-1 + x**2 + IPi*(1 + x**2))*lins(1)*
     -     lins(2)**2)/y**2 - 
     -  (8*(1 + x**2)*lins(1)*lins(2)**3)/
     -   (3d0*y**2) + 
     -  (2*(1 + x**2)*lins(2)**4)/(3d0*y**2) + 
     -  (16*(-1 + x)*zeta2*lins(3))/y + 
     -  (8*(1 + 3*x)*lins(1)*lins(3))/y - 
     -  (16*(-1 + x)*lins(2)*lins(3))/y - 
     -  (8*(1 + 3*x)*lins(4))/y + 
     -  (16*x**2*lins(1)*lins(4))/y**2 - 
     -  (16*(1 + x**2)*lins(2)*lins(4))/y**2 - 
     -  (16*(-1 + x)*lins(5))/y + 
     -  (16*(-1 + x)*lins(6))/y + 
     -  (16*(1 + x**2)*lins(7))/y**2 - 
     -  (16*(1 + x**2)*lins(8))/y**2
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^4
      function TwoLoop_HelAmppmpm_HE_1_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(13*y)/(3d0*x)
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^3
      function TwoLoop_HelAmppmpm_HE_1_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (4*y)/(3d0*x) + (12*lins(1))/x - 
     -  (16*y*lins(2))/(3d0*x)
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^2
      function TwoLoop_HelAmppmpm_HE_1_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-2*(-(-1 + x)**2 + 
     -       6*(1 + 5*x + x**2)*zeta2))/(x*y) + 
     -  (2*(7 + 3*x)*lins(1))/x + 
     -  (2*(4 + x)*lins(1)**2)/(x*y) + 
     -  (16*y*lins(2))/x - 
     -  (20*lins(1)*lins(2))/x - 
     -  (2*y*lins(2)**2)/x
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^1
      function TwoLoop_HelAmppmpm_HE_1_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (4*(6*(22 + 83*x + 22*x**2)*zeta2 + 
     -       9*y**2*(1 + 4*zeta3)))/(3d0*x*y) + 
     -  (4*(3*y**2*(23 + 12*y) + 
     -       6*(3 - 8*y**2)*zeta2)*lins(1))/
     -   (3d0*x*y**2) + 
     -  (12*(2 + 8*x + x**2)*lins(1)**2)/(x*y) + 
     -  (4*(-1 + y)*lins(1)**3)/y**2 - 
     -  (8*(3 + 3*x**2 + 6*y**2*zeta2)*lins(2))/
     -   (3d0*x*y) + (8*(3 + 2*x)*lins(1)*lins(2))/
     -   x + (16*lins(1)**2*lins(2))/x + 
     -  (4*y*lins(2)**2)/x + 
     -  (4*lins(1)*lins(2)**2)/x + 
     -  (8*y*lins(2)**3)/(3d0*x)
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^0
      function TwoLoop_HelAmppmpm_HE_1_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=8)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(8)=dcmplx(Li4(-y),0d0)
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (540*y*(38 + 38*y + 7*y**2)*zeta2 - 
     -     540*IPi*y*(-16 + 20*y + 9*y**2)*
     -      zeta2 + 180*y*
     -      (-4*zeta3 + 8*y*zeta3 + 
     -        y**2*(-7 + 32*zeta3)) + 
     -     90*(-51 - 295*y + 107*y**2 + 297*y**3)*
     -      zeta4)/(45d0*x*y**2) + 
     -  (2*(2*(-8 + 15*x)*y**2 + 
     -       6*(8 + 21*x + 10*x**2 - 13*x**3)*
     -        zeta2 - 
     -       12*IPi*(-1 + x**3 + y)*zeta2 - 
     -       24*x**2*zeta3 + 8*y*zeta3)*lins(1))/
     -   (x*y**2) + (2*
     -     (3*(-1 + x)*y*(-13 + 12*y) + 
     -       3*IPi*y*(16 + 4*y + 3*y**2) + 
     -       6*(-6 + 11*x*y)*zeta2)*lins(1)**2)/
     -   (3d0*x*y**2) + 
     -  (2*(30 + 116*y + 102*y**2 - 5*y**3 - 
     -       4*IPi*x*(3 + y))*lins(1)**3)/
     -   (3d0*x*y**2) + 
     -  ((2 + 22*x - 10*x**2)*lins(1)**4)/
     -   (6d0*x*y**2) + 
     -  (2*(36*IPi*(-2 - 2*y + y**2)*zeta2 + 
     -       6*(-60 + 108*y + 25*y**2)*zeta2 + 
     -       24*(y**2*(1 - 3*zeta3) + zeta3 + 
     -          2*y*zeta3))*lins(2))/(3d0*x*y) - 
     -  (4*(12*IPi*(-1 + x)*y - 2*y*(7 + 6*y) + 
     -       6*(-2 + y**2)*zeta2)*lins(1)*lins(2))
     -    /(x*y) + (2*
     -     (-11 + 18*x + x**2 - 6*IPi*y)*
     -     lins(1)**2*lins(2))/(x*y) - 
     -  (4*lins(1)**3*lins(2))/(3d0*x) + 
     -  (4*(-18*IPi*(-1 + x)*y - 6*(x + 2*y**2) + 
     -       6*(4 + 17*y + 7*y**2)*zeta2)*
     -     lins(2)**2)/(3d0*x*y) + 
     -  (4*(IPi*(-2 - 3*x + x**2) + 
     -       (-15 + 4*x)*y)*lins(1)*lins(2)**2)/
     -   (x*y) - ((0,4)*
     -     ((0,-1)*IPi*(-1 + x) - (0,4)*y)*
     -     lins(2)**3)/x - 
     -  (4*(2 + 8*y + 3*y**2)*lins(1)*lins(2)**3)/
     -   (3d0*x*y) + ((-2 - 3*x + x**2)*
     -     lins(2)**4)/(3d0*x*y) + 
     -  (24*(-1 + x)*(x**2 - y)*zeta2*lins(3))/
     -   (x*y**2) - (4*(15 + 2*x + 3*x**2)*
     -     lins(1)*lins(3))/(x*y) + 
     -  (4*(-1 + x - 4*x**2)*lins(1)**2*lins(3))/
     -   (x*y**2) + (48*(-1 + x)*lins(2)*lins(3))/
     -   x + (8*lins(1)*lins(2)*lins(3))/x + 
     -  (4*(-1 + x)*lins(2)**2*lins(3))/x + 
     -  (4*(15 + 2*x + 3*x**2)*lins(4))/(x*y) + 
     -  (16*(3*x**2 - y)*lins(1)*lins(4))/
     -   (x*y**2) + (16*(x - y)*lins(2)*lins(4))/
     -   (x*y) + (48*(-1 + x)*lins(5))/x + 
     -  (16*lins(1)*lins(5))/x + 
     -  (16*(-1 + x)*lins(2)*lins(5))/x - 
     -  (24*(-1 + x)*lins(6))/y**2 - 
     -  (8*(3 + 5*x)*lins(7))/(x*y) + 
     -  (8*(5 + 3*x)*lins(8))/y
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^4
      function TwoLoop_HelAmppmpm_HE_2_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(4*(x - 5*y**2)*(x - y**2))/(x**2*y**2)
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^3
      function TwoLoop_HelAmppmpm_HE_2_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (152*(1 - x + x**2))/(3d0*x**2) - 
     -  (16*(7 + 3*y**2)*lins(1))/(3d0*x**2) + 
     -  (16*(-2 - 4*x - 7*x**2 + 2*x**3*(-1 + y))*
     -     lins(2))/(3d0*x**2*y**2)
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^2
      function TwoLoop_HelAmppmpm_HE_2_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-2*(3*(79 + 98*x + 79*x**2)*y**2 + 
     -       6*(11 + 32*x + 78*x**2 + 32*x**3 + 
     -          11*x**4)*zeta2))/(3d0*x**2*y**2) + 
     -  (4*(35 + x + 29*x**2 + 7*x**3)*lins(1))/
     -   (x**2*y) + (2*
     -     (21 + 52*x + 32*x**2 + 16*x**3 + 
     -       3*x**4)*lins(1)**2)/(x**2*y**2) + 
     -  (8*(2 + 13*x + 2*x**2)*lins(2))/x**2 + 
     -  (40*lins(1)*lins(2))/x**2 + 
     -  (4*(-x + 4*x**2 + x**3*(-1 + y) + y)*
     -     lins(2)**2)/(x**2*y**2)
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^1
      function TwoLoop_HelAmppmpm_HE_2_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=5)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*(48 + 96*y + 87*y**4 + 
     -       216*IPi*y**2*(1 + (-1 + x)*y)*
     -        zeta2 + 
     -       6*(72 + 144*y + 60*y**2 - 12*y**3 + 
     -          47*y**4)*zeta2 + 
     -       6*y**2*(-9 + 4*zeta3) + 
     -       6*y**3*(-17 + 16*zeta3)))/
     -   (3d0*x**2*y**2) + 
     -  ((6*x*(55 + 146*x + 57*x**2)*y**3 + 
     -       24*x*(9 + 24*x + 61*x**2 + 52*x**3 + 
     -          24*x**4)*y*zeta2)*lins(1))/
     -   (3d0*x**3*y**3) + 
     -  ((48 + 8*(1 + 3*IPi)*y**2 - 56*y**3 - 
     -       6*y**4)*lins(1)**2)/(x**2*y**2) + 
     -  (4*(-9 - 20*x + 5*x**2 + 2*x**4)*
     -     lins(1)**3)/(3d0*x**2*y**2) + 
     -  (4*(69 - 144*x + 69*x**2 + 
     -       24*(-16 + 5*x**2)*zeta2)*lins(2))/
     -   (3d0*x**2) + 
     -  (16*(-16 - 16*y - 3*IPi*(-1 + x)*y**2)*
     -     lins(1)*lins(2))/(x**2*y) - 
     -  (56*lins(1)**2*lins(2))/x**2 - 
     -  (8*(7 + (7 + 3*IPi*(-1 + x))*y + 2*y**2)*
     -     lins(2)**2)/x**2 - 
     -  (8*(-5 + 3*x**2)*lins(1)*lins(2)**2)/
     -   x**2 + (8*(-x - 8*x**2 + x**3*(-1 + y) + 
     -       y)*lins(2)**3)/(3d0*x**2*y**2) - 
     -  (48*lins(1)*lins(3))/x**2 + 
     -  (48*(-1 + x)*y*lins(2)*lins(3))/x**2 + 
     -  (48*lins(4))/x**2 + 
     -  (48*(-1 + x)*y*lins(5))/x**2
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^0
      function TwoLoop_HelAmppmpm_HE_2_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=8)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(8)=dcmplx(Li4(-y),0d0)
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (360*(142 + 382*x + 429*x**2 + 382*x**3 + 
     -        142*x**4)*zeta2 + 
     -     1080*IPi*(-41 - 63*x + 27*x**2 + 
     -        13*x**3)*y*zeta2 + 
     -     45*y*((111 - 196*x + 111*x**2)*y - 
     -        16*(19 + 47*x + 62*x**2 + 28*x**3)*
     -         zeta3) + 
     -     90*(-1226 - 2548*x + 935*x**2 + 
     -        1468*x**3 + 629*x**4)*zeta4)/
     -   (45d0*x**2*y**2) + 
     -  ((192*IPi*(3 + 6*y - 3*y**2 + 2*y**3 + 
     -          y**4)*zeta2 + 
     -       6*(-72 - 564*y - 442*y**2 + 
     -          376*y**3 + 268*y**4)*zeta2 - 
     -       3*(-64*zeta3 + 8*y*(-1 + 8*zeta3) + 
     -          y**4*(1 + 32*zeta3) + 
     -          4*y**3*(-25 + 48*zeta3) + 
     -          y**2*(-94 + 64*zeta3)))*lins(1))/
     -   (3d0*x**2*y**2) - 
     -  ((-3*(99 + 184*x + 26*x**2 + 32*x**3 + 
     -          23*x**4) + 
     -       12*IPi*(23 + 33*x + 3*x**2 + 5*x**3)*
     -        y + 12*
     -        (-29 - 48*x - 24*x**2 + 5*x**4)*
     -        zeta2)*lins(1)**2)/(3d0*x**2*y**2) - 
     -  (2*(39 + 104*x + 85*x**2 + 
     -       8*IPi*(3 + 6*x + x**2) + 
     -       16*x**3*(-1 + y))*lins(1)**3)/
     -   (3d0*x**2*y**2) + 
     -  ((20 + 32*x - 26*x**2 - 6*x**4)*
     -     lins(1)**4)/(6d0*x**2*y**2) - 
     -  (2*(72*IPi*(6 + 12*x - 3*x**2 + 
     -          x**3*(-1 + y))*zeta2 + 
     -       24*(-101 - 157*x + 53*x**2 + 
     -          25*x**3)*y*zeta2 + 
     -       3*(21 + 21*x**4 + 72*zeta3 + 
     -          36*x*(1 + 4*zeta3) + 
     -          46*x**2*(1 + 4*zeta3) + 
     -          x**3*(36 + 48*zeta3 - 48*y*zeta3))
     -       )*lins(2))/(3d0*x**2*y**2) + 
     -  (4*(-111*y**2 - 
     -       36*IPi*(-1 + x)*y*
     -        (-2 - 2*y + 3*y**2) + 
     -       6*(-48 - 96*y + 22*y**2 + 
     -          6*(-1 + x)*y**3)*zeta2)*lins(1)*
     -     lins(2))/(3d0*x**2*y**2) + 
     -  (4*(23 + 9*x + 3*x**2 + 5*x**3 + 
     -       14*IPi*y)*lins(1)**2*lins(2))/
     -   (x**2*y) + (56*lins(1)**3*lins(2))/
     -   (3d0*x**2) + 
     -  (4*(18*IPi*(-3 - 8*x + 8*x**3 + 3*x**4) + 
     -       3*(7 + 24*x + 7*x**2)*y**2 + 
     -       24*(4 + 8*x + x**2 - 
     -          5*x**3*(-1 + y))*zeta2)*lins(2)**2
     -     )/(3d0*x**2*y**2) + 
     -  (8*(-2*IPi*(-2 - 4*x + x**2 + 
     -          x**3*(-1 + y)) + 
     -       (-18 - 34*x + 15*x**2 + 9*x**3)*y)*
     -     lins(1)*lins(2)**2)/(x**2*y**2) - 
     -  (28*lins(1)**2*lins(2)**2)/x**2 + 
     -  ((-56d0/3d0-8d0/3d0*x*(13d0+7d0*x)+8d0/3d0*IPi*(-1 + x**2)
     -   )*lins(2)**3)/x**2 + (8*
     -     (-2 - 4*x + x**2 + 5*x**3*(-1 + y))*
     -     lins(1)*lins(2)**3)/(3d0*x**2*y**2) + 
     -  (4*(2 + 4*x + 7*x**2 - 6*x**3*(-1 + y))*
     -     lins(2)**4)/(3d0*x**2*y**2) + 
     -  (8*(-1 + x)*(18 + 18*y + 17*y**2)*zeta2*
     -     lins(3))/(x**2*y) + 
     -  (8*(23 + 33*x + 3*x**2 + 5*x**3)*lins(1)*
     -     lins(3))/(x**2*y) + 
     -  ((44 + 64*x + 32*x**2 - 12*x**4)*
     -     lins(1)**2*lins(3))/(x**2*y**2) + 
     -  (48*(-1 + x)*(3 + 8*x + 3*x**2)*lins(2)*
     -     lins(3))/(x**2*y) - 
     -  (16*lins(1)*lins(2)*lins(3))/x**2 - 
     -  (16*(-1 + x)*y*lins(2)**2*lins(3))/x**2 - 
     -  (8*(23 + 33*x + 3*x**2 + 5*x**3)*lins(4))/
     -   (x**2*y) + (16*
     -     (-5 - 4*x - 6*x**2 + 3*x**4)*lins(1)*
     -     lins(4))/(x**2*y**2) + 
     -  (16*(-x + 3*x**2 + 4*x**3*(-1 + y) + y)*
     -     lins(2)*lins(4))/(x**2*y**2) + 
     -  (48*(-1 + x)*(3 + 8*x + 3*x**2)*lins(5))/
     -   (x**2*y) - (96*lins(1)*lins(5))/x**2 + 
     -  (48*(-1 + x)*y*lins(2)*lins(5))/x**2 + 
     -  (8*(-1 + x)*(1 - 16*x + x**2)*lins(6))/
     -   (x**2*y) - (64*
     -     (2 - 2*(-1 + x)*y + (-1 + x)*y**3)*
     -     lins(7))/(x**2*y**2) - 
     -  (64*(-2 + 2*(-1 + x)*y + (-1 + x)*y**3)*
     -     lins(8))/(x**2*y**2)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^4
      function TwoLoop_HelAmppmpm_HE_3_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (2*(129 + 169*x + 313*x**2 + 169*x**3 + 
     -      129*x**4))/(3d0*x**3*y)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^3
      function TwoLoop_HelAmppmpm_HE_3_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (4*(239 + 703*x + 1352*x**2 + 1752*x**3 + 
     -       1352*x**4 + 703*x**5 + 239*x**6))/
     -   (3d0*x**3*y**3) - 
     -  (8*(-17 + 38*y - 6*y**2 + 24*y**5)*
     -     lins(1))/(3d0*x**3*y**2) - 
     -  (32*(5 - 8*x + 5*x**2)*(x**2 - y)*
     -     lins(2))/(3d0*x**3*y)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^2
      function TwoLoop_HelAmppmpm_HE_3_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(5890 + 14549*x + 11561*x**2 + 
     -      14549*x**3 + 5890*x**4 + 
     -      72*(17 + 55*x + 115*x**2 + 55*x**3 + 
     -         17*x**4)*zeta2)/(9d0*x**3*y) + 
     -  (2*(387 + 540*x + 890*x**2 + 956*x**3 + 
     -       466*x**4 + 86*x**5)*lins(1))/
     -   (x**3*y**2) - 
     -  (8*(24 + 38*x + 40*x**2 + 38*x**3 + 
     -       19*x**4 + 3*x**5)*lins(1)**2)/
     -   (x**3*y**2) - 
     -  (2*(5 - 73*x - 148*x**2 - 188*x**3 - 
     -       148*x**4 - 73*x**5 + 5*x**6)*lins(2))
     -    /(x**3*y**3) - 
     -  (8*(19 + 12*x + 2*x**2)*lins(1)*lins(2))/
     -   (x**3*y**2) + 
     -  (4*(1 - 5*x - x**2 - 5*x**3 + x**4)*
     -     lins(2)**2)/(x**3*y)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^1
      function TwoLoop_HelAmppmpm_HE_3_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=5)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*(648 + 1944*y + 
     -       y**4*(-34631 - 16344*zeta2 - 
     -          11664*IPi*(-1 + x)**2*zeta2 - 
     -          17280*zeta3) + 
     -       3*y**2*(-6449 - 9960*zeta2 + 
     -          3888*IPi*zeta2 - 3888*zeta3) + 
     -       4*y**5*(-3335 + 3384*zeta2 - 
     -          2376*zeta3) + 
     -       4*y**6*(-2008 + 990*zeta2 - 
     -          864*zeta3) + 
     -       (0,6)*y**3*
     -        ((0,9960)*zeta2 - 
     -          (0,3888)*IPi*(-1 + x)*zeta2 + 
     -          (0,1)*(6989 + 2592*zeta3))))/
     -   (27d0*x**3*y**3) - 
     -  (2*(2348 + 11988*x + 14366*x**2 + 
     -       12704*x**3 + 6471*x**4 + 1598*x**5 + 
     -       72*(3 + 28*x + 90*x**2 + 88*x**3 + 
     -          98*x**4 + 66*x**5)*zeta2)*lins(1))
     -    /(9d0*x**3*y**2) + 
     -  (8*(-65 - 70*x - 50*x**2 - 53*x**3 - 
     -       23*x**4 + x**5 + 18*IPi*y)*lins(1)**2
     -     )/(x**3*y**2) - 
     -  (32*(-7 - 7*x + x**2 + 2*x**3 + x**4 + 
     -       x**5)*lins(1)**3)/(3d0*x**3*y**2) + 
     -  (8*(162 + 3*x - 83*x**2 + 3*x**3 + 
     -       162*x**4 + 
     -       12*(-89 - x - 3*x**2 - x**3 + 
     -          37*x**4)*zeta2)*lins(2))/
     -   (3d0*x**3*y) + 
     -  (4*(-37 + 114*x + 54*x**2 - 72*IPi*x**4 - 
     -       72*IPi*x**5 - 72*IPi*y)*lins(1)*
     -     lins(2))/(x**3*y**2) + 
     -  (16*(5 + x)*(3 + 2*x)*lins(1)**2*lins(2))/
     -   (x**3*y**2) - 
     -  (8*(-12 - 36*y + 35*y**2 + 
     -       2*(65 + 18*IPi*(-1 + x))*y**3 + 
     -       6*(20 - 3*IPi*(-1 + x)**2)*y**4 + 
     -       49*y**5 + 8*y**6)*lins(2)**2)/
     -   (x**3*y**3) - 
     -  (8*(29 + 32*x + 2*x**2 + 18*x**4*y)*
     -     lins(1)*lins(2)**2)/(x**3*y**2) - 
     -  (16*(6 - x - x**2 - x**3 + 6*x**4)*
     -     lins(2)**3)/(3d0*x**3*y) - 
     -  (288*lins(1)*lins(3))/(x**3*y) + 
     -  (288*(-1 + x)*(1 + x**2)*lins(2)*lins(3))/
     -   x**3 + (288*lins(4))/(x**3*y) + 
     -  (288*(-1 + x)*(1 + x**2)*lins(5))/x**3
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^0
      function TwoLoop_HelAmppmpm_HE_3_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=8)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(8)=dcmplx(Li4(-y),0d0)
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(-8640 - 10*y**6*
     -       (27539 + 113628*zeta2 - 
     -         24948*IPi*zeta2 + 11340*zeta4) - 
     -      36*y*(720 + 13590*IPi*zeta2 - 
     -         1510*zeta3 + 34290*zeta4) - 
     -      y**2*(422065 + 1241400*zeta2 + 
     -         1634040*IPi*zeta2 - 330480*zeta3 + 
     -         450360*zeta4) - 
     -      10*y**3*(80093 + 248280*zeta2 + 
     -         6048*IPi*zeta2 - 30456*zeta3 + 
     -         537840*zeta4) - 
     -      y**5*(677455 + 2860260*zeta2 - 
     -         1040040*IPi*zeta2 + 302400*zeta3 - 
     -         302400*x*zeta3 + 897480*zeta4) - 
     -      2*y**4*(2050830*zeta2 - 
     -         464400*IPi*zeta2 + 
     -         1760*(305 + 126*zeta3) + 
     -         1769040*zeta4))/(270d0*x**3*y**3) + 
     -  ((-16243 - 1954*x**5 - 
     -       1296*IPi*
     -        (-42 - 36*x - 4*x**2 + 2*x**3 - 
     -          2*x**4 + x**5)*zeta2 - 
     -       36*(-18 - 474*x + 1734*x**2 + 
     -          2390*x**3 + 2886*x**4 + 1505*x**5)
     -         *zeta2 - 26784*zeta3 - 
     -       9*x*(5135 + 3264*zeta3) - 
     -       x**2*(70663 + 28512*zeta3) - 
     -       9*x**4*(2407 + 384*y*zeta3) + 
     -       x**3*(-55765 + 6912*y*zeta3))*lins(1)
     -     )/(27d0*x**3*y**2) + 
     -  ((-884 - 479*x - 857*x**2 - 1306*x**3 - 
     -       672*x**4 - 220*x**5 - 
     -       2*IPi*(507 + 948*x + 782*x**2 + 
     -          732*x**3 + 378*x**4 + 138*x**5) + 
     -       48*(-20 - 11*x - 16*x**2 + 5*x**5 - 
     -          5*x**3*(-1 + y))*zeta2)*lins(1)**2
     -     )/(3d0*x**3*y**2) - 
     -  (2*(-439 - 396*x + 750*x**2 + 868*x**3 + 
     -       450*x**4 + 202*x**5 + 432*IPi*y)*
     -     lins(1)**3)/(9d0*x**3*y**2) + 
     -  (2*(-35 - 38*x - 6*x**2 + 6*x**5 - 
     -       6*x**3*(-1 + y))*lins(1)**4)/
     -   (3d0*x**3*y**2) + 
     -  ((-432 + 2*y**3*
     -        (9385 - 12216*zeta2 + 
     -          3888*IPi*zeta2 - 2880*zeta3) + 
     -       4*y**4*(2698 + 5346*zeta2 + 
     -          864*IPi*zeta2 - 2808*zeta3) + 
     -       3*y**5*(397 + 12600*zeta2 - 
     -          144*IPi*zeta2 - 2688*zeta3) + 
     -       y**2*(8305 - 95376*zeta2 - 
     -          14256*IPi*zeta2 - 1728*zeta3) + 
     -       3*y**6*(-237 + 3504*zeta2 - 
     -          144*IPi*zeta2 - 768*zeta3) - 
     -       4*y*(324 + 6342*zeta2 - 360*zeta3))*
     -     lins(2))/(9d0*x**3*y**3) + 
     -  (2*(-627 - 1436*y + 105*y**2 - 
     -       2*IPi*(-1 + x)*
     -        (-151 - 302*y + 386*y**2 + 
     -          537*y**3 + 369*y**4) + 
     -       24*(-42 + 103*y - 70*y**2 - 
     -          24*y**3 + 3*y**4 + 3*y**5)*zeta2)*
     -     lins(1)*lins(2))/(3d0*x**3*y**2) - 
     -  (4*(-32 + 108*y + 106*y**2 + 100*y**3 + 
     -       52*y**4 + 23*y**5 + 
     -       6*IPi*(5 + 2*y**2))*lins(1)**2*
     -     lins(2))/(x**3*y**2) - 
     -  (8*(35 + 32*x + 2*x**2)*lins(1)**3*
     -     lins(2))/(3d0*x**3*y**2) + 
     -  ((y*(-1203 - 2406*y - 859*y**2 + 
     -          344*y**3 + 266*y**4) - 
     -       2*IPi*(-1 + x)*
     -        (-151 - 302*y + 386*y**2 + 
     -          537*y**3 + 369*y**4) + 
     -       48*(65 + 261*y + 208*y**2 + 
     -          180*y**3 + 89*y**4 + 19*y**5)*
     -        zeta2)*lins(2)**2)/(3d0*x**3*y**2) + 
     -  (2*(-750 - 1152*x - 152*x**2 + 50*x**3 + 
     -       570*x**4 + 369*x**5 - 
     -       12*IPi*(38 + 44*x - 4*x**2 + 
     -          2*x**3 + 2*x**4 + 5*x**5))*
     -     lins(1)*lins(2)**2)/(3d0*x**3*y**2) + 
     -  (8*(13 + 9*x + 2*x**2)*lins(1)**2*
     -     lins(2)**2)/(x**3*y**2) - 
     -  (2*(95 + 325*x + 248*x**2 - 12*x**3 + 
     -       248*x**4 + 325*x**5 + 95*x**6 - 
     -       12*IPi*(5 + 13*x + 6*x**2 + 
     -          3*x**4*(-1 + y) + 5*x**5*(-1 + y))
     -       )*lins(2)**3)/(3d0*x**3*y**3) + 
     -  (8*(41 + 58*x - 6*x**2 + 6*x**3 + 9*x**5)*
     -     lins(1)*lins(2)**3)/(3d0*x**3*y**2) + 
     -  (2*(15 + 135*y + 288*y**2 + 404*y**3 + 
     -       263*y**4 + 65*y**5)*lins(2)**4)/
     -   (3d0*x**3*y**2) + 
     -  (48*(-1 + x)*
     -     (7 + 11*x + 25*x**2 + 11*x**3 + 
     -       7*x**4)*zeta2*lins(3))/(x**3*y**2) + 
     -  (4*(507 + 948*x + 782*x**2 + 732*x**3 + 
     -       378*x**4 + 138*x**5)*lins(1)*lins(3))
     -    /(3d0*x**3*y**2) + 
     -  (8*(-23 - 20*x - 14*x**2 + 6*x**5 - 
     -       6*x**3*(-1 + y))*lins(1)**2*lins(3))/
     -   (x**3*y**2) + 
     -  (4*(-1 + x)*(369 + 939*x + 989*x**2 + 
     -       939*x**3 + 369*x**4)*lins(2)*lins(3))
     -    /(3d0*x**3*y**2) + 
     -  (16*(-1 - 4*x + 2*x**2)*lins(1)*lins(2)*
     -     lins(3))/(x**3*y**2) - 
     -  (8*(-1 + x)*(19 + 41*x + 39*x**2 + 
     -       41*x**3 + 19*x**4)*lins(2)**2*lins(3)
     -     )/(x**3*y**2) - 
     -  (4*(507 + 948*x + 782*x**2 + 732*x**3 + 
     -       378*x**4 + 138*x**5)*lins(4))/
     -   (3d0*x**3*y**2) - 
     -  (32*(-5 - 2*x - 14*x**2 + 6*x**5 - 
     -       6*x**3*(-1 + y))*lins(1)*lins(4))/
     -   (x**3*y**2) - 
     -  (32*(-10 - 13*x + 2*x**2 + 12*x**4*y)*
     -     lins(2)*lins(4))/(x**3*y**2) + 
     -  (4*(-1 + x)*(369 + 939*x + 989*x**2 + 
     -       939*x**3 + 369*x**4)*lins(5))/
     -   (3d0*x**3*y**2) + 
     -  (32*(11 + 8*x + 2*x**2)*lins(1)*lins(5))/
     -   (x**3*y**2) - 
     -  (32*(-1 + x)*(x - y**2)*
     -     (5 + 5*y + 2*y**2)*lins(2)*lins(5))/
     -   (x**3*y**2) + 
     -  (96*(-1 + x)*
     -     (-x + 4*x**2 + x**3*(-1 + y) + y)*
     -     lins(6))/(x**3*y**2) + 
     -  (48*(1 - 2*x + 2*x**2 + 8*x**4*y)*
     -     lins(7))/(x**3*y**2) - 
     -  (48*(-8 - 8*x + 2*x**3 - 2*x**4 + x**5)*
     -     lins(8))/(x**3*y**2)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^4
      function TwoLoop_HelAmppmpm_HE_4_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (2*(544 + 1020*x + 2759*x**2 + 
     -      3408*x**3 + 2759*x**4 + 1020*x**5 + 
     -      544*x**6))/(3d0*x**4*y**2)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^3
      function TwoLoop_HelAmppmpm_HE_4_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*(7120 + 26080*x + 72045*x**2 + 
     -       143458*x**3 + 180962*x**4 + 
     -       143458*x**5 + 72045*x**6 + 
     -       26080*x**7 + 7120*x**8))/
     -   (9d0*x**4*y**4) - 
     -  (8*(-80 + 115*y - 94*y**2 - 25*y**3 + 
     -       24*y**5 + 28*y**6 + 98*y**7)*lins(1))
     -    /(3d0*x**4*y**3) - 
     -  (64*(11 - 6*x + 2*x**2 - 8*x**3 + 
     -       2*x**4 - 6*x**5 + 11*x**6)*lins(2))/
     -   (3d0*x**4*y**2)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^2
      function TwoLoop_HelAmppmpm_HE_4_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(-1728 - 6912*y + 
     -      y**6*(112027 - 4464*zeta2) + 
     -      48*y**5*(-2517 + 600*zeta2) + 
     -      12*y**8*(3524 + 606*zeta2) - 
     -      8*y**7*(-14648 + 810*zeta2) + 
     -      6*y**2*(-20145 + 4128*zeta2) + 
     -      18*y**3*(-18801 + 4128*zeta2) + 
     -      3*y**4*(-112797 + 25440*zeta2))/
     -   (18d0*x**4*y**4) + 
     -  (2*(5463 + 10707*x + 26943*x**2 + 
     -       45743*x**3 + 45687*x**4 + 
     -       27207*x**5 + 8943*x**6 + 1215*x**7)*
     -     lins(1))/(3d0*x**4*y**3) - 
     -  (2*(425 + 729*x + 1197*x**2 + 2013*x**3 + 
     -       2069*x**4 + 1253*x**5 + 421*x**6 + 
     -       53*x**7)*lins(1)**2)/(x**4*y**3) - 
     -  (2*(442 - 248*x - 1755*x**2 - 2122*x**3 - 
     -       1898*x**4 - 2122*x**5 - 1755*x**6 - 
     -       248*x**7 + 442*x**8)*lins(2))/
     -   (3d0*x**4*y**4) + 
     -  (8*(-74 - 54*x - 11*x**2 + 17*x**3)*
     -     lins(1)*lins(2))/(x**4*y**3) + 
     -  (4*(70 + 210*y + 433*y**2 + 516*y**3 + 
     -       335*y**4 + 112*y**5 + 14*y**6)*
     -     lins(2)**2)/(x**4*y**2)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^1
      function TwoLoop_HelAmppmpm_HE_4_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=5)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(2537168*x**7 + 462024*x**8 + 
     -      108*(4278 - 198*y**2*zeta2 - 
     -         8640*IPi*y**2*zeta2 + 
     -         3104*y**2*zeta3) + 
     -      16*x*(158573 + 28512*y**2*zeta2 + 
     -         7776*y**2*zeta3) + 
     -      6*x**5*(2362123 + 76032*y**2*zeta2 + 
     -         20736*y**2*zeta3) + 
     -      3*x**6*(2534429 - 7128*y**2*zeta2 + 
     -         155520*IPi*y**2*zeta2 + 
     -         59904*y**2*zeta3) + 
     -      6*x**3*(2362123 + 458304*y**2*zeta2 + 
     -         163584*y**2*zeta3) + 
     -      2*x**4*(8638835 + 
     -         1288548*y**2*zeta2 + 
     -         319680*y**2*zeta3) + 
     -      x**2*(7603287 + 2577096*y**2*zeta2 + 
     -         639360*y**2*zeta3))/
     -   (108d0*x**4*y**4) - 
     -  ((15905 + 100201*x + 163511*x**2 + 
     -       208511*x**3 + 198055*x**4 + 
     -       131087*x**5 + 54577*x**6 + 
     -       10953*x**7 + 
     -       72*(-61 + 171*x + 1137*x**2 + 
     -          1993*x**3 + 2202*x**4 + 
     -          1466*x**5 + 1094*x**6 + 646*x**7)*
     -        zeta2)*lins(1))/(9d0*x**4*y**3) + 
     -  ((-7017 - 8649*x - 12753*x**2 - 
     -       22609*x**3 - 21933*x**4 - 
     -       11949*x**5 - 3045*x**6 + 27*x**7 + 
     -       2160*IPi*y)*lins(1)**2)/
     -   (3d0*x**4*y**3) + 
     -  (4*(-285 + 16*x + 31*x**2 + 62*x**4 + 
     -       32*x**5 + 30*x**6)*lins(1)**3)/
     -   (3d0*x**4*y**2) + 
     -  ((-1728 - 6912*y + 
     -       480*y**5*(405 + 2292*zeta2) + 
     -       30*y**3*(-415 + 10656*zeta2) + 
     -       2*y**8*(7715 + 27936*zeta2) - 
     -       2*y**2*(6107 + 37440*zeta2) + 
     -       3*y**6*(60693 + 276960*zeta2) + 
     -       y**7*(83956 + 334080*zeta2) + 
     -       y**4*(90857 + 816480*zeta2))*lins(2))
     -    /(9d0*x**4*y**4) + 
     -  (8*(-121 + 173*x + 111*x**2 + 47*x**3 - 
     -       180*IPi*x**6 - 180*IPi*x**7 - 
     -       180*IPi*y)*lins(1)*lins(2))/
     -   (x**4*y**3) + 
     -  (16*(65 + 57*x + 4*x**2 - 4*x**3)*
     -     lins(1)**2*lins(2))/(x**4*y**3) + 
     -  (2*(-284 + 64*x - 51*x**2 - 1174*x**3 - 
     -       1334*x**4 - 1174*x**5 - 51*x**6 + 
     -       64*x**7 - 284*x**8 + 
     -       1080*IPi*
     -        (-1 + x**6 + 2*x**7 + x**8 + 
     -          x*(-1 + y)))*lins(2)**2)/
     -   (3d0*x**4*y**4) + 
     -  (8*(-146 - 150*x + 3*x**2 - 9*x**3 + 
     -       90*x**6 + 90*x**7)*lins(1)*lins(2)**2
     -     )/(x**4*y**3) - 
     -  (16*(35 + x**2 + x**4 + 35*x**6 + 
     -       12*x*y + 12*x**4*y)*lins(2)**3)/
     -   (3d0*x**4*y**2) - 
     -  (1440*lins(1)*lins(3))/(x**4*y**2) + 
     -  (1440*(-1 + x)*(1 - x + x**2)*(x**2 - y)*
     -     lins(2)*lins(3))/(x**4*y) + 
     -  (1440*lins(4))/(x**4*y**2) + 
     -  (1440*(-1 + x)*(1 - x + x**2)*(x**2 - y)*
     -     lins(5))/(x**4*y)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^0
      function TwoLoop_HelAmppmpm_HE_4_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=8)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=2.93416566094795419429148237668d-6)
      double precision rat2
      parameter (rat2=0.0000130076500641603641831295327885d0)
      double precision rat3
      parameter (rat3=0.0000464289433967191780056829990351d0)
      double precision rat4
      parameter (rat4=0.0000559563688196881461169864639197d0)
      double precision rat5
      parameter (rat5=0.000200394140520995594136556332074d0)
      double precision rat6
      parameter (rat6=0.0000955885094611503093617505310997d0)
      double precision rat7
      parameter (rat7=0.000128518874593112858534809649564d0)
      double precision rat8
      parameter (rat8=0.000468058420890252607277580409905d0)
      double precision rat9
      parameter (rat9=0.000116147335077470626484487231345d0)
      double precision rat10
      parameter (rat10=0.00020662087995258159875079768896d0)
      double precision rat11
      parameter (rat11=0.000787521607022866511509874450749d0)
      double precision rat12
      parameter (rat12=0.000113256434593828875368127660907d0)
      double precision rat13
      parameter (rat13=0.000202215538932287794924947503607d0)
      double precision rat14
      parameter (rat14=0.000242384638481451828082517001015d0)
      double precision rat15
      parameter (rat15=0.000946856766513780640726334747764d0)
      double precision rat16
      parameter (rat16=-0.0000280004361129872465270255536723d0)
      double precision rat17
      parameter (rat17=0.000116312529390821583691136349656d0)
      double precision rat18
      parameter (rat18=0.000103624032846293299199468357501d0)
      double precision rat19
      parameter (rat19=-0.0000222481341302307009383508982087d0)
      double precision rat20
      parameter (rat20=8.62156987774519516606826993254d-6)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(8)=dcmplx(Li4(-y),0d0)
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-4*IPi*(-1476 - 5319*y + 1128*y**2 + 
     -       9678*y**3 + 17784*y**4 + 
     -       15223*y**5 + 6484*y**6 + 780*y**7)*
     -     zeta2)/(x**4*y**3) + 
     -  (338994720*(rat2 + rat3*zeta2 + 
     -       rat1*zeta3 + 
     -       x*(rat4 + rat5*zeta2 + 
     -          (248*zeta3)/6.356151d6 - 
     -          (643*zeta4)/2.35413d6) + 
     -       x**6*(rat7 + rat8*zeta2 + 
     -          rat18*zeta3 - (55*zeta4)/313884d0) 
     -        + x**7*
     -        (rat4 + rat5*zeta2 + 
     -          (334*zeta3)/6.356151d6 - 
     -          (745*zeta4)/5.649912d6) + 
     -       x**2*(rat7 + rat8*zeta2 + 
     -          rat6*zeta3 + (527*zeta4)/7.06239d6
     -          ) + x**3*
     -        (rat10 + rat11*zeta2 + rat9*zeta3 + 
     -          (2351*zeta4)/7.06239d6) - 
     -       (169*zeta4)/941652d0 + 
     -       x**4*(rat14 + rat15*zeta2 + 
     -          rat12*zeta3 + rat13*zeta4) + 
     -       x**5*(rat10 + rat11*zeta2 + 
     -          rat17*zeta3 + rat16*zeta4) + 
     -       x**8*(rat2 + rat3*zeta2 + 
     -          rat20*zeta3 + rat19*zeta4)))/
     -   (x**4*y**4) + 
     -  ((2592*IPi*(192 + 120*x - 82*x**2 - 
     -          42*x**3 + 21*x**4 + 41*x**5 + 
     -          40*x**6 + 4*x**7)*zeta2 - 
     -       36*(860 + 4332*x + 38058*x**2 + 
     -          69914*x**3 + 74365*x**4 + 
     -          49037*x**5 + 31255*x**6 + 
     -          12639*x**7)*zeta2 - 
     -       54*(4031 + 4352*zeta3) + 
     -       3*x**7*(-5741 + 8064*zeta3) - 
     -       x**6*(244631 + 17280*zeta3) - 
     -       4*x**5*(237571 + 39744*zeta3) - 
     -       4*x**4*(482579 + 93312*zeta3) - 
     -       2*x*(445639 + 131328*zeta3) - 
     -       x**2*(1893637 + 279936*zeta3) - 
     -       x**3*(2419289 + 466560*zeta3))*
     -     lins(1))/(54d0*x**4*y**3) + 
     -  ((-40873 - 48537*x - 108669*x**2 - 
     -       210829*x**3 - 230481*x**4 - 
     -       142929*x**5 - 48741*x**6 - 
     -       9765*x**7 - 
     -       24*IPi*(2112 + 5012*x + 7399*x**2 + 
     -          11615*x**3 + 11622*x**4 + 
     -          7350*x**5 + 3018*x**6 + 666*x**7) 
     -        - 144*(291 + 35*x - 41*x**2 + 
     -          23*x**3 + 275*x**4*y + 
     -          176*x**5*y + 99*x**6*y)*zeta2)*
     -     lins(1)**2)/(36d0*x**4*y**3) - 
     -  ((-3579 + 285*x + 15351*x**2 + 
     -       23751*x**3 + 24230*x**4 + 
     -       16198*x**5 + 7022*x**6 + 1806*x**7 + 
     -       4320*IPi*y)*lins(1)**3)/
     -   (9d0*x**4*y**3) - 
     -  ((366 + 502*x + 248*x**2 + 144*x**3 + 
     -       133*x**4*y + 80*x**5*y + 53*x**6*y)*
     -     lins(1)**4)/(3d0*x**4*y**3) + 
     -  ((-3840 + y**6*
     -        (285725 + 2422080*zeta2 - 
     -          60480*IPi*zeta2 - 223488*zeta3) + 
     -       y**4*(738955 + 645408*zeta2 + 
     -          281664*IPi*zeta2 - 190080*zeta3) 
     -        + 2*y**3*
     -        (244569 - 305952*zeta2 + 
     -          129600*IPi*zeta2 - 42624*zeta3) + 
     -       8*y**5*(80595 + 297456*zeta2 + 
     -          16416*IPi*zeta2 - 30528*zeta3) + 
     -       4*y**7*(11857 + 280608*zeta2 - 
     -          20736*IPi*zeta2 - 29952*zeta3) + 
     -       y**8*(-2590 + 154752*zeta2 - 
     -          24192*IPi*zeta2 - 27648*zeta3) + 
     -       2*y**2*(72563 - 739824*zeta2 - 
     -          138240*IPi*zeta2 - 9216*zeta3) - 
     -       192*y*(80 + 1722*zeta2 - 96*zeta3))*
     -     lins(2))/(36d0*x**4*y**4) + 
     -  (2*(-5176 - 8412*y + 6702*y**2 - 
     -       315*y**3 - 
     -       6*IPi*(-1 + x)*
     -        (-492 - 1476*y + 1267*y**2 + 
     -          4994*y**3 + 7979*y**4 + 
     -          5236*y**5 + 1446*y**6) + 
     -       72*(-128 + 606*y - 638*y**2 - 
     -          691*y**3 - 228*y**4 + 105*y**5 + 
     -          144*y**6 + 42*y**7)*zeta2)*
     -     lins(1)*lins(2))/(9d0*x**4*y**3) + 
     -  (4*(1413 + 2037*x + 3471*x**2 + 
     -       5767*x**3 + 5811*x**4 + 3675*x**5 + 
     -       1509*x**6 + 333*x**7 + 
     -       18*IPi*(-26 + 10*x + 41*x**2 + 
     -          21*x**3))*lins(1)**2*lins(2))/
     -   (3d0*x**4*y**3) + 
     -  (8*(-166 - 130*x + 41*x**2 + 21*x**3)*
     -     lins(1)**3*lins(2))/(3d0*x**4*y**3) + 
     -  ((1728 + 12*y**8*(423 + 960*zeta2) + 
     -       48*y*(144 + 123*IPi*(-1 + x) + 
     -          1248*zeta2) + 
     -       24*y**7*
     -        (586 - 723*IPi*(-1 + x) + 
     -          4752*zeta2) + 
     -       6*y**3*(-17333 - 2534*IPi*(-1 + x) + 
     -          80832*zeta2) + 
     -       2*y**2*(-13301 + 8856*IPi*(-1 + x) + 
     -          172656*zeta2) + 
     -       4*y**5*(-20920 - 
     -          23937*IPi*(-1 + x) + 174816*zeta2)
     -         + 3*y**4*
     -        (-44179 - 19976*IPi*(-1 + x) + 
     -          254592*zeta2) + 
     -       y**6*(-9291 - 62832*IPi*(-1 + x) + 
     -          396576*zeta2))*lins(2)**2)/
     -   (18d0*x**4*y**4) + 
     -  (2*(-2880 - 4088*x - 307*x**2 - 
     -       187*x**3 - 7*x**4 + 49*x**5 + 
     -       1994*x**6 + 1446*x**7 - 
     -       12*IPi*(208 + 280*x + 82*x**2 + 
     -          42*x**3 - 21*x**4 - 41*x**5 - 
     -          20*x**6 + 16*x**7))*lins(1)*
     -     lins(2)**2)/(3d0*x**4*y**3) - 
     -  (8*(-57 + 7*x + 86*x**2 + 38*x**3)*
     -     lins(1)**2*lins(2)**2)/(x**4*y**3) + 
     -  (2*(-1150 - 1150*x**8 - 1224*IPi*y + 
     -       x*(-3592 - 2520*IPi*y) + 
     -       x**2*(-2301 - 1476*IPi*y) + 
     -       x**3*(722 - 756*IPi*y) + 
     -       (0,8)*x**7*
     -        ((0,449) - (0,153)*IPi*y) + 
     -       (0,3)*x**6*
     -        ((0,767) - (0,840)*IPi*y) + 
     -       2*x**5*(361 + 738*IPi*y) + 
     -       x**4*(946 + 756*IPi*y))*lins(2)**3)/
     -   (9d0*x**4*y**4) + 
     -  (8*(240 + 444*x + 251*x**2 + 127*x**3 - 
     -       63*x**4 - 123*x**5 - 90*x**6 + 
     -       18*x**7)*lins(1)*lins(2)**3)/
     -   (3d0*x**4*y**3) - 
     -  (2*(208 + 280*x + 82*x**2 + 42*x**3 - 
     -       21*x**4 - 41*x**5 + 280*x**6 + 
     -       316*x**7)*lins(2)**4)/(3d0*x**4*y**3) 
     -   + (24*(-1 + x)*
     -     (45 + 98*x + 229*x**2 + 320*x**3 + 
     -       229*x**4 + 98*x**5 + 45*x**6)*zeta2*
     -     lins(3))/(x**4*y**3) + 
     -  (4*(2112 + 5012*x + 7399*x**2 + 
     -       11615*x**3 + 11622*x**4 + 
     -       7350*x**5 + 3018*x**6 + 666*x**7)*
     -     lins(1)*lins(3))/(3d0*x**4*y**3) - 
     -  (4*(205 + 213*x + 131*x**2 + 91*x**3 + 
     -       133*x**4*y + 80*x**5*y + 53*x**6*y)*
     -     lins(1)**2*lins(3))/(x**4*y**3) + 
     -  (4*(-1 + x)*(1446 + 3440*x + 3489*x**2 + 
     -       3482*x**3 + 3489*x**4 + 3440*x**5 + 
     -       1446*x**6)*lins(2)*lins(3))/
     -   (3d0*x**4*y**3) - 
     -  (16*(14 + 50*x + 41*x**2 + 21*x**3)*
     -     lins(1)*lins(2)*lins(3))/(x**4*y**3) - 
     -  (8*(-1 + x)*(104 + 244*x + 285*x**2 + 
     -       306*x**3 + 285*x**4 + 244*x**5 + 
     -       104*x**6)*lins(2)**2*lins(3))/
     -   (x**4*y**3) - 
     -  (4*(2112 + 5012*x + 7399*x**2 + 
     -       11615*x**3 + 11622*x**4 + 
     -       7350*x**5 + 3018*x**6 + 666*x**7)*
     -     lins(4))/(3d0*x**4*y**3) + 
     -  (16*(25 + 33*x + 131*x**2 + 91*x**3 + 
     -       133*x**4*y + 80*x**5*y + 53*x**6*y)*
     -     lins(1)*lins(4))/(x**4*y**3) - 
     -  (32*(-59 - 95*x - 41*x**2 - 21*x**3 + 
     -       60*x**6*y)*lins(2)*lins(4))/
     -   (x**4*y**3) + 
     -  (4*(-1 + x)*(1446 + 3440*x + 3489*x**2 + 
     -       3482*x**3 + 3489*x**4 + 3440*x**5 + 
     -       1446*x**6)*lins(5))/(3d0*x**4*y**3) - 
     -  (32*(-46 - 10*x + 41*x**2 + 21*x**3)*
     -     lins(1)*lins(5))/(x**4*y**3) + 
     -  (32*(-1 + x)*
     -     (1 - 34*x - 75*x**2 - 96*x**3 - 
     -       75*x**4 - 34*x**5 + x**6)*lins(2)*
     -     lins(5))/(x**4*y**3) - 
     -  (24*(-1 + x)*
     -     (108 + 216*y + 296*y**2 + 188*y**3 + 
     -       27*y**4)*lins(6))/(x**4*y) + 
     -  (48*(-4 - 40*x - 41*x**2 - 21*x**3 + 
     -       40*x**6*y)*lins(7))/(x**4*y**3) + 
     -  (48*(40 + 40*x + 21*x**4 + 41*x**5 + 
     -       40*x**6 + 4*x**7)*lins(8))/
     -   (x**4*y**3)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^4
      function TwoLoop_HelAmppmpm_HE_5_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (2*(2271 + 5523*x + 19562*x**2 + 
     -      36538*x**3 + 46644*x**4 + 
     -      36538*x**5 + 19562*x**6 + 5523*x**7 + 
     -      2271*x**8))/(3d0*x**5*y**3)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^3
      function TwoLoop_HelAmppmpm_HE_5_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=69.8560698050462214357628825091d0)
      double precision rat2
      parameter (rat2=83.8094273423437166305641150807d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (65554*(1 + (139759*x)/32777d0 + 
     -       (2502809*x**2)/163885d0 + 
     -       (6547507*x**3)/163885d0 + rat1*x**4 + 
     -       rat2*x**5 + rat1*x**6 + 
     -       (6547507*x**7)/163885d0 + 
     -       (2502809*x**8)/163885d0 + 
     -       (139759*x**9)/32777d0 + x**10))/
     -   (9d0*x**5*y**5) + 
     -  (8*(316 - 524*y + 365*y**2 + 84*y**3 + 
     -       10*y**4 + 16*x*y**6 + 200*x*y**7 - 
     -       32*y**8 - 400*y**9)*lins(1))/
     -   (3d0*x**5*y**4) - 
     -  (32*(93 - 37*x + 25*x**2 - 101*x**3 + 
     -       62*x**4 - 101*x**5 + 25*x**6 - 
     -       37*x**7 + 93*x**8)*lins(2))/
     -   (3d0*x**5*y**3)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^2
      function TwoLoop_HelAmppmpm_HE_5_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-0.000132592420189385767059057440043d0)
      double precision rat2
      parameter (rat2=-0.0000179831223613893052490103662975d0)
      double precision rat3
      parameter (rat3=-0.000826969930556079221882199144342d0)
      double precision rat4
      parameter (rat4=-0.00195090395383159977504020876124d0)
      double precision rat5
      parameter (rat5=-0.00135715180884782693017531423952d0)
      double precision rat6
      parameter (rat6=-0.00306158959546280721227151689757d0)
      double precision rat7
      parameter (rat7=-0.00388886997933104729798724062395d0)
      double precision rat8
      parameter (rat8=-0.00710142023462522351960919996771d0)
      double precision rat9
      parameter (rat9=-0.00417220509016281774086532889419d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (62725481*(rat1 + 
     -       x**4*(rat7 - 
     -          (53488*zeta2)/8.960783d6) + 
     -       x**6*(rat7 - 
     -          (53488*zeta2)/8.960783d6) + 
     -       x**3*(rat6 - 
     -          (16952*zeta2)/4.825037d6) + 
     -       x**7*(rat6 - 
     -          (16952*zeta2)/4.825037d6) + 
     -       x*(rat3 - (1352*zeta2)/4.825037d6) + 
     -       x**9*(rat3 - 
     -          (1352*zeta2)/4.825037d6) + 
     -       rat2*zeta2 + 
     -       x**10*(rat1 + rat2*zeta2) + 
     -       x**2*(rat4 + rat5*zeta2) + 
     -       x**8*(rat4 + rat5*zeta2) + 
     -       x**5*(rat9 + rat8*zeta2)))/
     -   (x**5*y**5) + 
     -  ((48775 + 118047*x + 398637*x**2 + 
     -       926335*x**3 + 1377837*x**4 + 
     -       1374393*x**5 + 920181*x**6 + 
     -       397705*x**7 + 99861*x**8 + 10904*x**9
     -       )*lins(1))/(3d0*x**5*y**4) - 
     -  (8*(462 + 839*x + 1879*x**2 + 4435*x**3 + 
     -       6837*x**4 + 7007*x**5 + 4819*x**6 + 
     -       2147*x**7 + 553*x**8 + 58*x**9)*
     -     lins(1)**2)/(x**5*y**4) - 
     -  ((29375 + 9655*x - 65632*x**2 - 
     -       119276*x**3 - 97006*x**4 - 
     -       52084*x**5 - 97006*x**6 - 
     -       119276*x**7 - 65632*x**8 + 
     -       9655*x**9 + 29375*x**10)*lins(2))/
     -   (15d0*x**5*y**5) + 
     -  (8*(-291 - 262*x - 149*x**2 + 28*x**3 + 
     -       18*x**4)*lins(1)*lins(2))/(x**5*y**4)
     -    + (4*(700 + 2800*y + 6453*y**2 + 
     -       9559*y**3 + 9715*y**4 + 6765*y**5 + 
     -       3023*y**6 + 767*y**7 + 81*y**8)*
     -     lins(2)**2)/(x**5*y**3)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^1
      function TwoLoop_HelAmppmpm_HE_5_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=5)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-0.691628003716964623493548508008d0)
      double precision rat2
      parameter (rat2=-0.0799664735120451426605511454866d0)
      double precision rat3
      parameter (rat3=-6.43953190071701710450620851653d0)
      double precision rat4
      parameter (rat4=-1.88607294228401888722950403018d0)
      double precision rat5
      parameter (rat5=-1.60608176595362077840552059391d0)
      double precision rat6
      parameter (rat6=-23.1805319772731793592649466964d0)
      double precision rat7
      parameter (rat7=-12.46684797808751426066600552d0)
      double precision rat8
      parameter (rat8=-2.98789085287768189856522524903d0)
      double precision rat9
      parameter (rat9=-54.2138913827520302831888371637d0)
      double precision rat10
      parameter (rat10=-35.4612363748862923952440352519d0)
      double precision rat11
      parameter (rat11=-8.05790031945559830174805782657d0)
      double precision rat12
      parameter (rat12=-87.9251490503128860097969313047d0)
      double precision rat13
      parameter (rat13=-61.5152467088394648158552665242d0)
      double precision rat14
      parameter (rat14=-15.7424951473652198820288865421d0)
      double precision rat15
      parameter (rat15=-102.922963724734854579782917243d0)
      double precision rat16
      parameter (rat16=-73.4295036865374258734165678153d0)
      double precision rat17
      parameter (rat17=-19.5160638371012946478681637574d0)
      double precision rat18
      parameter (rat18=-2.684037545805375264812829461d0)
      double precision rat19
      parameter (rat19=-0.998375151809007510900729017836d0)
      double precision rat20
      parameter (rat20=-0.387774696644657989741152719971d0)
      double precision rat21
      parameter (rat21=-8.19877222207771889374980831741d0)
      double precision rat22
      parameter (rat22=-14.8516370954623927499531226889d0)
      double precision rat23
      parameter (rat23=-8.5727828972934612847760739757d0)
      double precision rat24
      parameter (rat24=-22.7582648678719797132855540626d0)
      double precision rat25
      parameter (rat25=-20.4043700912273237073411616178d0)
      double precision rat26
      parameter (rat26=-37.4664434154944750769933144996d0)
      double precision rat27
      parameter (rat27=-23.0363581589620295721101156773d0)
      double precision rat28
      parameter (rat28=-40.6864779435942688168244319499d0)
      double precision rat29
      parameter (rat29=-22.9892498582034129788231019154d0)
      double precision rat30
      parameter (rat30=-30.2660884290491029643153990899d0)
      double precision rat31
      parameter (rat31=-19.3251185132186252971322732138d0)
      double precision rat32
      parameter (rat32=-14.7920336134584274923491957872d0)
      double precision rat33
      parameter (rat33=-11.7423514280401246490024935236d0)
      double precision rat34
      parameter (rat34=-7.91591821491199294375149706527d0)
      double precision rat35
      parameter (rat35=-4.41400294191522641061884650369d0)
      double precision rat36
      parameter (rat36=-4.17039194090702453110071808664d0)
      double precision rat37
      parameter (rat37=0.000268128870437261176064642104948d0)
      double precision rat38
      parameter (rat38=0.000881362211154169782754746140254d0)
      double precision rat39
      parameter (rat39=0.000835459400936178860897097085242d0)
      double precision rat40
      parameter (rat40=0.000182291883696261644665805493234d0)
      double precision rat41
      parameter (rat41=-0.000123923624589791220271057627708d0)
      double precision rat42
      parameter (rat42=-0.000144292971494282115445489110689d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-20160*IPi*(-2 + x**8)*zeta2)/
     -   (x**5*y**3) + 
     -  (298565123*(-1 + rat2*zeta2 + 
     -       rat1*zeta3 + 
     -       x**3*(rat9 + rat10*zeta2 + 
     -          rat11*zeta3) + 
     -       x**7*(rat9 + rat10*zeta2 + 
     -          rat11*zeta3) + 
     -       x**4*(rat12 + rat13*zeta2 + 
     -          rat14*zeta3) + 
     -       x**6*(rat12 + rat13*zeta2 + 
     -          rat14*zeta3) + 
     -       x**5*(rat15 + rat16*zeta2 + 
     -          rat17*zeta3) + 
     -       x**8*(rat6 + rat7*zeta2 + 
     -          rat18*zeta3) + 
     -       x**9*(rat3 + rat4*zeta2 + 
     -          rat19*zeta3) + 
     -       x**10*(-1 + rat2*zeta2 + 
     -          rat20*zeta3) + 
     -       x*(rat3 + rat4*zeta2 + rat5*zeta3) + 
     -       x**2*(rat6 + rat7*zeta2 + rat8*zeta3)
     -       ))/(13500d0*x**5*y**5) + 
     -  (2575873*(-1 + 
     -       x*(rat21 - 
     -          (1440000*zeta2)/2.575873d6) + 
     -       (1821600*zeta2)/2.575873d6 + 
     -       x**2*(rat22 + rat23*zeta2) + 
     -       x**3*(rat25 + rat24*zeta2) + 
     -       x**4*(rat27 + rat26*zeta2) + 
     -       x**5*(rat29 + rat28*zeta2) + 
     -       x**6*(rat31 + rat30*zeta2) + 
     -       x**7*(rat33 + rat32*zeta2) + 
     -       x**8*(rat35 + rat34*zeta2) + 
     -       x**9*(-0.766795567949196253076141564433d0 + 
     -          rat36*zeta2))*lins(1))/
     -   (450d0*x**5*y**4) - 
     -  (2*(76512 + 101029*x + 220604*x**2 + 
     -       559679*x**3 + 853122*x**4 + 
     -       842427*x**5 + 540124*x**6 + 
     -       211749*x**7 + 40114*x**8 + 
     -       412*x**9 - 25200*IPi*y)*lins(1)**2)/
     -   (15d0*x**5*y**4) - 
     -  (16*(336 - 29*x - 44*x**2 - 29*x**3 + 
     -       58*x**5*y + 30*x**6*y + 28*x**7*y)*
     -     lins(1)**3)/(3d0*x**5*y**3) + 
     -  (24318240*(rat37 + 
     -       x*(rat38 - (3977*zeta2)/759945d0) + 
     -       x**2*(rat39 - (123*zeta2)/50663d0) + 
     -       x**5*(rat42 - (8*zeta2)/50663d0) + 
     -       x**4*(rat41 - (64*zeta2)/759945d0) + 
     -       x**6*(rat41 - (64*zeta2)/759945d0) + 
     -       x**3*(rat40 + (109*zeta2)/759945d0) + 
     -       x**7*(rat40 + (109*zeta2)/759945d0) + 
     -       x**10*
     -        (rat37 + (307*zeta2)/253315d0) + 
     -       x**8*(rat39 + (73*zeta2)/50663d0) + 
     -       x**9*(rat38 + 
     -          (1903*zeta2)/759945d0) - 
     -       (673*zeta2)/253315d0)*lins(2))/
     -   (x**5*y**5) - 
     -  (2*(2477 - 2060*x - 870*x**2 + 
     -       3360*IPi*x**8 + 3360*IPi*x**9 + 
     -       278*x**3*(-1 + y) + 3360*IPi*y)*
     -     lins(1)*lins(2))/(x**5*y**4) + 
     -  (32*(140 + 125*x + 6*x**2 - 4*x**3 + 
     -       7*x**4)*lins(1)**2*lins(2))/
     -   (x**5*y**4) - 
     -  (2*(-2400 - 12000*y + 44495*y**2 + 
     -       20*(12499 + 5040*IPi*(-1 + x))*
     -        y**3 + 
     -       7*(91939 - 21600*IPi*(-1 + x)**2)*
     -        y**4 + 
     -       (0,7)*((0,-143627) - 
     -          (0,14400)*IPi*(-1 + x)**3)*y**5 - 
     -       (0,15)*((0,64193) - 
     -          (0,1680)*IPi*(-1 + x)**4)*y**6 + 
     -       565785*y**7 + 201191*y**8 + 
     -       40600*y**9 + 3890*y**10)*lins(2)**2)/
     -   (15d0*x**5*y**5) - 
     -  (8*(689 + 658*x - 125*x**2 + 12*x**3 + 
     -       46*x**4 + 420*x**8*y)*lins(1)*
     -     lins(2)**2)/(x**5*y**4) - 
     -  (16*(175 - 75*x - 86*x**2 - 46*x**3 - 
     -       46*x**5 - 86*x**6 - 75*x**7 + 
     -       175*x**8)*lins(2)**3)/(3d0*x**5*y**3) 
     -   - (6720*lins(1)*lins(3))/(x**5*y**3) + 
     -  (6720*(-1 + x)*(1 + x**2)*(1 + x**4)*
     -     lins(2)*lins(3))/(x**5*y**2) + 
     -  (6720*lins(4))/(x**5*y**3) + 
     -  (6720*(-1 + x)*(1 + x**2)*(1 + x**4)*
     -     lins(5))/(x**5*y**2)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^0
      function TwoLoop_HelAmppmpm_HE_5_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=8)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=4.7107413d11)
      double precision rat2
      parameter (rat2=-5.97018562662313891021780372444d-7)
      double precision rat3
      parameter (rat3=-1.14942985017382862155191300076d-9)
      double precision rat4
      parameter (rat4=3.80644813459627198885886968235d-8)
      double precision rat5
      parameter (rat5=1.26898866922140966079655729202d-7)
      double precision rat6
      parameter (rat6=-7.81108484985155096502539844419d-7)
      double precision rat7
      parameter (rat7=1.25587311137916517159058030492d-7)
      double precision rat8
      parameter (rat8=1.95127185752911616199134591764d-7)
      double precision rat9
      parameter (rat9=6.62723338256762263722696892737d-7)
      double precision rat10
      parameter (rat10=3.48623969932998301845472459575d-7)
      double precision rat11
      parameter (rat11=5.24081826868184718062825357743d-7)
      double precision rat12
      parameter (rat12=8.33074828371492189562606632633d-7)
      double precision rat13
      parameter (rat13=1.94533296348354628035577613513d-6)
      double precision rat14
      parameter (rat14=3.87333801016271756068059465149d-7)
      double precision rat15
      parameter (rat15=9.58426041416516822160515648842d-7)
      double precision rat16
      parameter (rat16=2.3085793312402869586576533082d-6)
      double precision rat17
      parameter (rat17=4.2081072180012658871050012164d-6)
      double precision rat18
      parameter (rat18=2.50824783493559000292940447964d-7)
      double precision rat19
      parameter (rat19=1.32527420670146614362655264753d-6)
      double precision rat20
      parameter (rat20=2.50600048871289111121427958695d-6)
      double precision rat21
      parameter (rat21=6.74258898488015039161670796908d-6)
      double precision rat22
      parameter (rat22=1.70763781912625938512055416841d-7)
      double precision rat23
      parameter (rat23=1.46712886756836831814061689396d-6)
      double precision rat24
      parameter (rat24=2.1408095579351810297882416086d-6)
      double precision rat25
      parameter (rat25=7.88798051805561897444888344856d-6)
      double precision rat26
      parameter (rat26=2.49714696354336701387812006007d-7)
      double precision rat27
      parameter (rat27=9.03552058781067005314004400963d-7)
      double precision rat28
      parameter (rat28=-6.98412370893727490405809378664d-7)
      double precision rat29
      parameter (rat29=3.81703547733913273196867479576d-7)
      double precision rat30
      parameter (rat30=-1.24506094189464405527851847861d-6)
      double precision rat31
      parameter (rat31=3.63776857512142869460198688191d-7)
      double precision rat32
      parameter (rat32=-6.81718607642495672602526485587d-7)
      double precision rat33
      parameter (rat33=1.61942523426903815187926650384d-7)
      double precision rat34
      parameter (rat34=-1.12398445654402630855572561372d-7)
      double precision rat35
      parameter (rat35=1.55400877847682557590953537044d-8)
      double precision rat36
      parameter (rat36=-0.922333500360091444580622630132d0)
      double precision rat37
      parameter (rat37=-0.175467055652261051470413995005d0)
      double precision rat38
      parameter (rat38=-5.14628577726010475023265563787d0)
      double precision rat39
      parameter (rat39=-1.57099067507399585709649156149d0)
      double precision rat40
      parameter (rat40=-1.0175320511586795632259999236d0)
      double precision rat41
      parameter (rat41=-14.1866425877288269641759620289d0)
      double precision rat42
      parameter (rat42=-10.0051465994010681005011519839d0)
      double precision rat43
      parameter (rat43=-1.29532454365292030287972382912d0)
      double precision rat44
      parameter (rat44=-24.9137261977364106060166448662d0)
      double precision rat45
      parameter (rat45=-23.6372474394630719368271780692d0)
      double precision rat46
      parameter (rat46=-3.57696849885842570385581568234d0)
      double precision rat47
      parameter (rat47=-36.2046201162286808707791066629d0)
      double precision rat48
      parameter (rat48=-30.067833127322987832888604096d0)
      double precision rat49
      parameter (rat49=-5.52931910621963449771429099587d0)
      double precision rat50
      parameter (rat50=-38.1125122200509588608685625396d0)
      double precision rat51
      parameter (rat51=-25.4478287439340388922819890102d0)
      double precision rat52
      parameter (rat52=-5.21875334213899456967445359588d0)
      double precision rat53
      parameter (rat53=-27.6488584474696143372933532921d0)
      double precision rat54
      parameter (rat54=-14.6802967826381382875744867833d0)
      double precision rat55
      parameter (rat55=-3.13374901645188429901504270948d0)
      double precision rat56
      parameter (rat56=-13.150827947869032262748043555d0)
      double precision rat57
      parameter (rat57=-5.40880271722934617505620122058d0)
      double precision rat58
      parameter (rat58=-1.04874469076477402835563182309d0)
      double precision rat59
      parameter (rat59=-5.41410545027363830330453724685d0)
      double precision rat60
      parameter (rat60=-1.0692256933280524485346697998d0)
      double precision rat61
      parameter (rat61=-0.0624252792121889302592637989937d0)
      double precision rat62
      parameter (rat62=-1.76332556138005045909737746218d0)
      double precision rat63
      parameter (rat63=-0.0628570323846290661279562558035d0)
      double precision rat64
      parameter (rat64=0.0749103350546267163111165587925d0)
      double precision rat65
      parameter (rat65=-5.54972434086629001883239171375d0)
      double precision rat66
      parameter (rat66=-14.1698632297551789077212806026d0)
      double precision rat67
      parameter (rat67=-22.0644967043314500941619585687d0)
      double precision rat68
      parameter (rat68=-21.9716247645951035781544256121d0)
      double precision rat69
      parameter (rat69=-14.382696327683615819209039548d0)
      double precision rat70
      parameter (rat70=3.1783008d9)
      double precision rat71
      parameter (rat71=-0.0000292670431529535110920485143089d0)
      double precision rat72
      parameter (rat72=-6.54437742330744780355591264364d-7)
      double precision rat73
      parameter (rat73=1.01490813372143169499039654564d-7)
      double precision rat74
      parameter (rat74=-0.0000821664624485301496111800787809d0)
      double precision rat75
      parameter (rat75=-2.14678809926780162951641749369d-6)
      double precision rat76
      parameter (rat76=-0.000123424105526240100790124500907d0)
      double precision rat77
      parameter (rat77=-5.21724704107441449356978560508d-6)
      double precision rat78
      parameter (rat78=-1.29880721170255502562878881697d-6)
      double precision rat79
      parameter (rat79=-0.000227660767665540026922561892191d0)
      double precision rat80
      parameter (rat80=-4.61264403363653378000666967149d-6)
      double precision rat81
      parameter (rat81=-2.34590759943174667419773483995d-6)
      double precision rat82
      parameter (rat82=-0.000381117356796436636834373889344d0)
      double precision rat83
      parameter (rat83=-3.09095979839290227029486951015d-6)
      double precision rat84
      parameter (rat84=-1.77168959100549716516587871244d-6)
      double precision rat85
      parameter (rat85=-0.000447947280509132426987401570046d0)
      double precision rat86
      parameter (rat86=-3.06075497951609866504768837487d-6)
      double precision rat87
      parameter (rat87=-1.27656891380450837126555170612d-7)
      double precision rat88
      parameter (rat88=-0.000383420809844891542885640864032d0)
      double precision rat89
      parameter (rat89=-5.97048586464817930385947107335d-6)
      double precision rat90
      parameter (rat90=-0.000239343655997989030281002142193d0)
      double precision rat91
      parameter (rat91=-0.00009198156029368481003014776533d0)
      double precision rat92
      parameter (rat92=-6.7286687695932786894598942093d-6)
      double precision rat93
      parameter (rat93=-3.90648990806659961196876016266d-6)
      double precision rat94
      parameter (rat94=-7.65188744879024666261922093717d-7)
      double precision rat95
      parameter (rat95=5.3640402234216891407299565017d-6)
      double precision rat96
      parameter (rat96=6.26617566020578596008141357996d-6)
      double precision rat97
      parameter (rat97=0.0000349383597518011816229145447442d0)
      double precision rat98
      parameter (rat98=0.0000545414685494836584246506398609d0)
      double precision rat99
      parameter (rat99=0.000042310863844943412557458959105d0)
      double precision rat100
      parameter (rat100=0.0000266422969025192373419683137619d0)
      double precision rat101
      parameter (rat101=0.0000185588812513897348144281059969d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(8)=dcmplx(Li4(-y),0d0)
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-2*IPi*(-67455 - 291785*y - 
     -       105932*y**2 + 551144*y**3 + 
     -       1946392*y**4 + 3005360*y**5 + 
     -       2657080*y**6 + 1363847*y**7 + 
     -       375376*y**8 + 28133*y**9)*zeta2)/
     -   (5d0*x**5*y**4) + 
     -  (rat1*(rat4 + rat5*zeta2 + rat3*zeta3 + 
     -       rat2*zeta4 + 
     -       x**2*(rat11 + rat13*zeta2 + 
     -          rat10*zeta3 + rat12*zeta4) + 
     -       x**3*(rat15 + rat17*zeta2 + 
     -          rat14*zeta3 + rat16*zeta4) + 
     -       x**4*(rat19 + rat21*zeta2 + 
     -          rat18*zeta3 + rat20*zeta4) + 
     -       x**5*(rat23 + rat25*zeta2 + 
     -          rat22*zeta3 + rat24*zeta4) + 
     -       x**6*(rat19 + rat21*zeta2 + 
     -          rat26*zeta3 + rat27*zeta4) + 
     -       x**7*(rat15 + rat17*zeta2 + 
     -          rat29*zeta3 + rat28*zeta4) + 
     -       x**8*(rat11 + rat13*zeta2 + 
     -          rat31*zeta3 + rat30*zeta4) + 
     -       x**9*(rat8 + rat9*zeta2 + 
     -          rat33*zeta3 + rat32*zeta4) + 
     -       x**10*(rat4 + rat5*zeta2 + 
     -          rat35*zeta3 + rat34*zeta4) + 
     -       x*(rat8 + rat9*zeta2 + rat7*zeta3 + 
     -          rat6*zeta4)))/(x**5*y**5) + 
     -  ((16*IPi*(2594 + 1148*x - 2538*x**2 - 
     -          1608*x**3 - 108*x**4 + 54*x**5 + 
     -          804*x**6 + 1269*x**7 + 826*x**8 + 
     -          103*x**9)*zeta2)/(x**5*y**4) + 
     -     (553621873*
     -        (-1 + rat37*zeta2 + rat36*zeta3 + 
     -          x*(rat38 + rat39*zeta2 + 
     -             rat40*zeta3) + 
     -          x**2*
     -           (rat41 + rat42*zeta2 + 
     -             rat43*zeta3) + 
     -          x**3*
     -           (rat44 + rat45*zeta2 + 
     -             rat46*zeta3) + 
     -          x**4*
     -           (rat48 + rat47*zeta2 + 
     -             rat49*zeta3) + 
     -          x**5*
     -           (rat51 + rat50*zeta2 + 
     -             rat52*zeta3) + 
     -          x**6*
     -           (rat54 + rat53*zeta2 + 
     -             rat55*zeta3) + 
     -          x**7*
     -           (rat57 + rat56*zeta2 + 
     -             rat58*zeta3) + 
     -          x**8*
     -           (rat60 + rat59*zeta2 + 
     -             rat61*zeta3) + 
     -          x**9*
     -           (rat63 + rat62*zeta2 + 
     -             rat64*zeta3)))/
     -      (27000d0*x**5*y**4))*lins(1) + 
     -  (-(IPi*(89797 + 261139*x + 594485*x**2 + 
     -           1249611*x**3 + 1772683*x**4 + 
     -           1772657*x**5 + 1253559*x**6 + 
     -           610429*x**7 + 191659*x**8 + 
     -           30832*x**9))/(15d0*x**5*y**4) + 
     -     (4720*(-0.972657015065913370998116760829d0 + 
     -          x**3*(rat66 - (689*zeta2)/295d0) + 
     -          x**4*(rat67 - (643*zeta2)/295d0) + 
     -          x*(-1.75302707156308851224105461394d0 + 
     -             (19*zeta2)/295d0) + 
     -          x**2*(rat65 + (64*zeta2)/295d0) + 
     -          x**9*
     -           (-0.215836158192090395480225988701d0 + 
     -             (2*zeta2)/5d0) - zeta2 + 
     -          x**8*
     -           (-1.51877683615819209039548022599d0 + 
     -             (527*zeta2)/295d0) + 
     -          x**5*(rat68 + (697*zeta2)/295d0) + 
     -          x**7*
     -           (-6.00596610169491525423728813559d0 + 
     -             (241*zeta2)/59d0) + 
     -          x**6*(rat69 + (1493*zeta2)/295d0)))
     -       /(x**5*y**4))*lins(1)**2 - 
     -  ((-74741 + 84743*x + 649457*x**2 + 
     -       1280591*x**3 + 1781803*x**4 + 
     -       1791001*x**5 + 1299727*x**6 + 
     -       656597*x**7 + 218867*x**8 + 
     -       39696*x**9 + 100800*IPi*y)*lins(1)**3
     -     )/(45d0*x**5*y**4) - 
     -  (2*(869 + 1402*x + 1275*x**2 + 
     -       1312*x**3 + 502*x**4 + 542*x**5*y + 
     -       648*x**6*y + 350*x**7*y + 116*x**8*y)
     -      *lins(1)**4)/(3d0*x**5*y**4) + 
     -  ((-48*IPi*(784 - 784*y - 1407*y**2 - 
     -          1477*y**3 - 533*y**4 + 481*y**5 + 
     -          763*y**6 + 407*y**7 + 81*y**8)*
     -        zeta2)/(x**5*y**3) + 
     -     (rat70*(rat73 + rat71*zeta2 + 
     -          x**7*
     -           (rat80 + rat90*zeta2 - 
     -             (77*zeta3)/8.276825d6) + 
     -          x**8*
     -           (rat77 + rat91*zeta2 - 
     -             (67*zeta3)/8.276825d6) + 
     -          x*(rat75 + rat74*zeta2 - 
     -             (5*zeta3)/3.972876d6) + 
     -          rat72*zeta3 + 
     -          x**2*
     -           (rat77 + rat76*zeta2 + 
     -             rat78*zeta3) + 
     -          x**3*
     -           (rat80 + rat79*zeta2 + 
     -             rat81*zeta3) + 
     -          x**4*
     -           (rat84 + rat82*zeta2 + 
     -             rat83*zeta3) + 
     -          x**5*
     -           (rat87 + rat85*zeta2 + 
     -             rat86*zeta3) + 
     -          x**6*
     -           (rat84 + rat88*zeta2 + 
     -             rat89*zeta3) + 
     -          x**9*
     -           (rat75 + rat92*zeta2 + 
     -             rat93*zeta3) + 
     -          x**10*
     -           (rat73 + rat95*zeta2 + 
     -             rat94*zeta3)))/(x**5*y**5))*
     -   lins(2) + ((-443505 - 433280*y + 
     -       707700*y**2 + 37512*y**3 + 
     -       103173*y**4 - 
     -       12*IPi*(-1 + x)*
     -        (-22485 - 89940*y - 6024*y**2 + 
     -          296718*y**3 + 885304*y**4 + 
     -          1171148*y**5 + 864406*y**6 + 
     -          343275*y**7 + 58965*y**8) + 
     -       1440*(-552 + 2360*y - 4842*y**2 - 
     -          6053*y**3 - 4223*y**4 - 
     -          1599*y**5 + 1443*y**6 + 
     -          2289*y**7 + 1221*y**8 + 243*y**9)*
     -        zeta2)*lins(1)*lins(2))/
     -   (90d0*x**5*y**4) + 
     -  ((124072 + 236299*x + 589789*x**2 + 
     -       1255319*x**3 + 1776817*x**4 + 
     -       1772657*x**5 + 1253559*x**6 + 
     -       610429*x**7 + 191659*x**8 + 
     -       30832*x**9 + 
     -       120*IPi*
     -        (-317 + 406*x + 1269*x**2 + 
     -          804*x**3 + 54*x**4))*lins(1)**2*
     -     lins(2))/(15d0*x**5*y**4) + 
     -  (8*(-759 - 518*x + 423*x**2 + 268*x**3 + 
     -       18*x**4)*lins(1)**3*lins(2))/
     -   (3d0*x**5*y**4) + 
     -  (-(IPi*(-1 + x)*
     -         (-22485 - 89940*y - 6024*y**2 + 
     -           296718*y**3 + 885304*y**4 + 
     -           1171148*y**5 + 864406*y**6 + 
     -           343275*y**7 + 58965*y**8))/
     -      (15d0*x**5*y**4) + 
     -     (156304800*
     -        (rat96 + 
     -          x**7*
     -           (rat99 - (2869*zeta2)/9.76905d6) 
     -           + x**8*
     -           (rat98 - (307*zeta2)/1.08545d6) 
     -           + x**6*
     -           (rat100 - (179*zeta2)/1.628175d6)
     -            + x**9*
     -           (rat97 - (59*zeta2)/651270d0) + 
     -          x**10*(rat96 + zeta2/114930d0) + 
     -          x**5*
     -           (rat101 + (7*zeta2)/287325d0) + 
     -          x**4*
     -           (rat100 + 
     -             (1322*zeta2)/4.884525d6) + 
     -          x**3*
     -           (rat99 + (1019*zeta2)/1.628175d6)
     -            + x*
     -           (rat97 + (413*zeta2)/542725d0) + 
     -          x**2*
     -           (rat98 + (4231*zeta2)/4.884525d6)
     -            + (1339*zeta2)/4.884525d6))/
     -      (x**5*y**5))*lins(2)**2 + 
     -  ((-115050 - 145020*x + 23534*x**2 - 
     -       6152*x**3 - 8356*x**4 + 26*x**5 - 
     -       3948*x**6 - 15944*x**7 + 
     -       69480*x**8 + 58965*x**9 - 
     -       120*IPi*
     -        (1002 + 1484*x + 846*x**2 + 
     -          536*x**3 + 36*x**4 - 18*x**5 - 
     -          268*x**6 - 423*x**7 - 182*x**8 + 
     -          59*x**9))*lins(1)*lins(2)**2)/
     -   (15d0*x**5*y**4) - 
     -  (16*(-129 + 97*x + 429*x**2 + 264*x**3 + 
     -       25*x**4)*lins(1)**2*lins(2)**2)/
     -   (x**5*y**4) + 
     -  ((-48305 - 48305*x**10 - 62760*IPi*y + 
     -       x**2*(-46268 - 152280*IPi*y) + 
     -       x*(-135025 - 149520*IPi*y) + 
     -       x**4*(66066 - 6480*IPi*y) + 
     -       48*x**3*(1987 - 2010*IPi*y) + 
     -       12*x**5*(2257 + 540*IPi*y) + 
     -       24*x**7*(3974 + 6345*IPi*y) + 
     -       x**9*(-135025 + 62760*IPi*y) + 
     -       x**6*(66066 + 96480*IPi*y) + 
     -       x**8*(-46268 + 149520*IPi*y))*
     -     lins(2)**3)/(45d0*x**5*y**5) + 
     -  (8*(1175 + 2530*x + 2437*x**2 + 
     -       1604*x**3 + 182*x**4 - 54*x**5 - 
     -       804*x**6 - 1269*x**7 - 686*x**8 + 
     -       37*x**9)*lins(1)*lins(2)**3)/
     -   (3d0*x**5*y**4) - 
     -  (2*(1002 + 1484*x + 846*x**2 + 536*x**3 + 
     -       36*x**4 - 18*x**5 - 268*x**6 - 
     -       423*x**7 + 1218*x**8 + 1459*x**9)*
     -     lins(2)**4)/(3d0*x**5*y**4) + 
     -  (16*(-1 + x)*
     -     (245 + 817*x + 2542*x**2 + 5308*x**3 + 
     -       6880*x**4 + 5308*x**5 + 2542*x**6 + 
     -       817*x**7 + 245*x**8)*zeta2*lins(3))/
     -   (x**5*y**4) + 
     -  (2*(89797 + 261139*x + 594485*x**2 + 
     -       1249611*x**3 + 1772683*x**4 + 
     -       1772657*x**5 + 1253559*x**6 + 
     -       610429*x**7 + 191659*x**8 + 
     -       30832*x**9)*lins(1)*lins(3))/
     -   (15d0*x**5*y**4) - 
     -  (8*(455 + 564*x + 575*x**2 + 922*x**3 + 
     -       524*x**4 + 542*x**5*y + 648*x**6*y + 
     -       350*x**7*y + 116*x**8*y)*lins(1)**2*
     -     lins(3))/(x**5*y**4) + 
     -  (2*(-1 + x)*(58965 + 128445*x + 
     -       112501*x**2 + 108553*x**3 + 
     -       108579*x**4 + 108553*x**5 + 
     -       112501*x**6 + 128445*x**7 + 
     -       58965*x**8)*lins(2)*lins(3))/
     -   (15d0*x**5*y**4) - 
     -  (16*(81 + 322*x + 423*x**2 + 268*x**3 + 
     -       18*x**4)*lins(1)*lins(2)*lins(3))/
     -   (x**5*y**4) - 
     -  (8*(-1 + x)*(501 + 1243*x + 1666*x**2 + 
     -       1934*x**3 + 1952*x**4 + 1934*x**5 + 
     -       1666*x**6 + 1243*x**7 + 501*x**8)*
     -     lins(2)**2*lins(3))/(x**5*y**4) - 
     -  (2*(89797 + 261139*x + 594485*x**2 + 
     -       1249611*x**3 + 1772683*x**4 + 
     -       1772657*x**5 + 1253559*x**6 + 
     -       610429*x**7 + 191659*x**8 + 
     -       30832*x**9)*lins(4))/(15d0*x**5*y**4) 
     -   + (32*(35 + 144*x + 575*x**2 + 
     -       922*x**3 + 524*x**4 + 542*x**5*y + 
     -       648*x**6*y + 350*x**7*y + 116*x**8*y)
     -      *lins(1)*lins(4))/(x**5*y**4) - 
     -  (32*(-291 - 532*x - 423*x**2 - 268*x**3 - 
     -       18*x**4 + 280*x**8*y)*lins(2)*lins(4)
     -     )/(x**5*y**4) + 
     -  (2*(-1 + x)*(58965 + 128445*x + 
     -       112501*x**2 + 108553*x**3 + 
     -       108579*x**4 + 108553*x**5 + 
     -       112501*x**6 + 128445*x**7 + 
     -       58965*x**8)*lins(5))/(15d0*x**5*y**4) 
     -   - (32*(-199 + 42*x + 423*x**2 + 
     -       268*x**3 + 18*x**4)*lins(1)*lins(5))/
     -   (x**5*y**4) - 
     -  (32*(-1 + x)*
     -     (11 + 263*x + 686*x**2 + 954*x**3 + 
     -       972*x**4 + 954*x**5 + 686*x**6 + 
     -       263*x**7 + 11*x**8)*lins(2)*lins(5))/
     -   (x**5*y**4) - 
     -  (32*(-1 + x)*
     -     (868 + 2604*y + 4792*y**2 + 
     -       5244*y**3 + 3349*y**4 + 1161*y**5 + 
     -       106*y**6)*lins(6))/(x**5*y**2) + 
     -  (16*(-103 - 826*x - 1269*x**2 - 
     -       804*x**3 - 54*x**4 + 560*x**8*y)*
     -     lins(7))/(x**5*y**4) + 
     -  (16*(560 + 560*x + 54*x**5 + 804*x**6 + 
     -       1269*x**7 + 826*x**8 + 103*x**9)*
     -     lins(8))/(x**5*y**4)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^4
      function TwoLoop_HelAmppmpm_HE_6_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (4*(4710 + 13996*x + 60978*x**2 + 
     -      151140*x**3 + 260227*x**4 + 
     -      307728*x**5 + 260227*x**6 + 
     -      151140*x**7 + 60978*x**8 + 
     -      13996*x**9 + 4710*x**10))/
     -  (3d0*x**6*y**4)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^3
      function TwoLoop_HelAmppmpm_HE_6_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=68.1146440160545547386742363929d0)
      double precision rat2
      parameter (rat2=150.526950677816273081167743742d0)
      double precision rat3
      parameter (rat3=238.422788243497740612423062774d0)
      double precision rat4
      parameter (rat4=277.314557264356582422348994394d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (1457032*(1 + (882158*x)/182129d0 + 
     -       (3861338*x**2)/182129d0 + rat1*x**3 + 
     -       rat2*x**4 + rat3*x**5 + rat4*x**6 + 
     -       rat3*x**7 + rat2*x**8 + rat1*x**9 + 
     -       (3861338*x**10)/182129d0 + 
     -       (882158*x**11)/182129d0 + x**12))/
     -   (45d0*x**6*y**6) - 
     -  (16*(-624 + 947*y - 1616*y**2 - 
     -       1524*y**3 - 944*y**4 - 205*y**5 + 
     -       102*y**7 + 300*y**8 + 834*y**9 + 
     -       728*y**10 + 814*y**11)*lins(1))/
     -   (3d0*x**6*y**5) - 
     -  (64*(193 - 36*x + 90*x**2 - 244*x**3 + 
     -       11*x**4 - 120*x**5 + 11*x**6 - 
     -       244*x**7 + 90*x**8 - 36*x**9 + 
     -       193*x**10)*lins(2))/(3d0*x**6*y**4)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^2
      function TwoLoop_HelAmppmpm_HE_6_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=1.593137861d9)
      double precision rat2
      parameter (rat2=-0.0000181908488332636518767662379973d0)
      double precision rat3
      parameter (rat3=-1.74247318324198686531623392258d-6)
      double precision rat4
      parameter (rat4=-0.000135943202106990929281682686858d0)
      double precision rat5
      parameter (rat5=-0.0000525654446172251253778972239239d0)
      double precision rat6
      parameter (rat6=-0.000332605210324188845156905936675d0)
      double precision rat7
      parameter (rat7=-0.00030832736577591134154836334029d0)
      double precision rat8
      parameter (rat8=-0.000976749117633329536444931679393d0)
      double precision rat9
      parameter (rat9=-0.000469464213764820779269229440109d0)
      double precision rat10
      parameter (rat10=-0.00210327560597720299862988442279d0)
      double precision rat11
      parameter (rat11=-0.000384143482203989599086902596407d0)
      double precision rat12
      parameter (rat12=-0.00328741920470873800920860796742d0)
      double precision rat13
      parameter (rat13=-0.0000754014260832802257196916034291d0)
      double precision rat14
      parameter (rat14=-0.00380677664404587268797574574747d0)
      double precision rat15
      parameter (rat15=0.000108798703223671195730457461856d0)
      double precision rat16
      parameter (rat16=11.9834199963295672183295766297d0)
      double precision rat17
      parameter (rat17=34.941060378619258478464441506d0)
      double precision rat18
      parameter (rat18=68.3866603296801547228587965686d0)
      double precision rat19
      parameter (rat19=95.0228555025904785209097026479d0)
      double precision rat20
      parameter (rat20=95.2542715837917452907876842863d0)
      double precision rat21
      parameter (rat21=68.75023457830021316744232009d0)
      double precision rat22
      parameter (rat22=35.0028648198429242996767195742d0)
      double precision rat23
      parameter (rat23=11.9457613559896286745502543422d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (rat1*(rat2 + rat3*zeta2 + 
     -       x**4*(rat11 + rat10*zeta2) + 
     -       x**8*(rat11 + rat10*zeta2) + 
     -       x**5*(rat13 + rat12*zeta2) + 
     -       x**7*(rat13 + rat12*zeta2) + 
     -       x**6*(rat15 + rat14*zeta2) + 
     -       x**12*(rat2 + rat3*zeta2) + 
     -       x*(rat4 + rat5*zeta2) + 
     -       x**11*(rat4 + rat5*zeta2) + 
     -       x**2*(rat6 + rat7*zeta2) + 
     -       x**10*(rat6 + rat7*zeta2) + 
     -       x**3*(rat9 + rat8*zeta2) + 
     -       x**9*(rat9 + rat8*zeta2)))/
     -   (x**6*y**6) + 
     -  (212509*(1 + (605787*x)/212509d0 + 
     -       rat16*x**2 + rat17*x**3 + 
     -       rat18*x**4 + rat19*x**5 + 
     -       rat20*x**6 + rat21*x**7 + 
     -       rat22*x**8 + rat23*x**9 + 
     -       (2599343*x**10)/1.062545d6 + 
     -       (47833*x**11)/212509d0)*lins(1))/
     -   (3d0*x**6*y**5) - 
     -  (4*(3969 + 7511*x + 21364*x**2 + 
     -       64724*x**3 + 131161*x**4 + 
     -       186691*x**5 + 191835*x**6 + 
     -       142289*x**7 + 74732*x**8 + 
     -       26380*x**9 + 5545*x**10 + 499*x**11)*
     -     lins(1)**2)/(x**6*y**5) - 
     -  (2*(77661 + 63638*x - 81703*x**2 - 
     -       249592*x**3 - 355053*x**4 - 
     -       251250*x**5 - 149918*x**6 - 
     -       251250*x**7 - 355053*x**8 - 
     -       249592*x**9 - 81703*x**10 + 
     -       63638*x**11 + 77661*x**12)*lins(2))/
     -   (15d0*x**6*y**6) + 
     -  (16*(-575 - 643*x - 618*x**2 - 226*x**3 + 
     -       109*x**4 + 153*x**5)*lins(1)*lins(2))
     -    /(x**6*y**5) + 
     -  (8*(197 - 212*x - 190*x**2 - 652*x**3 - 
     -       171*x**4 - 112*x**5 - 171*x**6 - 
     -       652*x**7 - 190*x**8 - 212*x**9 + 
     -       197*x**10)*lins(2)**2)/(x**6*y**4)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^1
      function TwoLoop_HelAmppmpm_HE_6_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=5)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=-0.680732173245751360998665120791d0)
      double precision rat2
      parameter (rat2=-0.142585654483437542190803391621d0)
      double precision rat3
      parameter (rat3=-7.43823634086616290632615011892d0)
      double precision rat4
      parameter (rat4=-2.68949191610464431749644317696d0)
      double precision rat5
      parameter (rat5=-1.58663196999619770448762525668d0)
      double precision rat6
      parameter (rat6=-31.1254905383599631777337975822d0)
      double precision rat7
      parameter (rat7=-18.2777485700776438097129194259d0)
      double precision rat8
      parameter (rat8=-3.51870552632541790260448726467d0)
      double precision rat9
      parameter (rat9=-86.6240111370021092147622623688d0)
      double precision rat10
      parameter (rat10=-60.9617860044343006098398035008d0)
      double precision rat11
      parameter (rat11=-12.115986823228040943070683135d0)
      double precision rat12
      parameter (rat12=-172.93235960217925665826239487d0)
      double precision rat13
      parameter (rat13=-132.799439942816720167997103627d0)
      double precision rat14
      parameter (rat14=-29.7313544839109137125914950677d0)
      double precision rat15
      parameter (rat15=-258.874912397074377153160415406d0)
      double precision rat16
      parameter (rat16=-208.504922012839282558577290607d0)
      double precision rat17
      parameter (rat17=-49.1173025283274262624594902067d0)
      double precision rat18
      parameter (rat18=-295.745330887747565402128794457d0)
      double precision rat19
      parameter (rat19=-241.87285153200085193202542168d0)
      double precision rat20
      parameter (rat20=-57.7782582761391638676463022903d0)
      double precision rat21
      parameter (rat21=-3.22801781565337315061906542141d0)
      double precision rat22
      parameter (rat22=-1.00525654865210820051678157015d0)
      double precision rat23
      parameter (rat23=-0.390044462573706609013243277525d0)
      double precision rat24
      parameter (rat24=-0.755052602115666743880781406841d0)
      double precision rat25
      parameter (rat25=-8.43705186858157671222299436337d0)
      double precision rat26
      parameter (rat26=-17.0866987105242838390857848815d0)
      double precision rat27
      parameter (rat27=-17.7751518222531078681182920238d0)
      double precision rat28
      parameter (rat28=-12.429991796000308856458960698d0)
      double precision rat29
      parameter (rat29=-0.000156331256195700631513712431451d0)
      double precision rat30
      parameter (rat30=0.0000131903394518671773173137144358d0)
      double precision rat31
      parameter (rat31=0.0000547925382245927782628124559591d0)
      double precision rat32
      parameter (rat32=-0.000128918045634882102047747839893d0)
      double precision rat33
      parameter (rat33=0.000065002208909782542865868754651d0)
      double precision rat34
      parameter (rat34=0.0000280408449548649217981681273571d0)
      double precision rat35
      parameter (rat35=0.000028310872367595962420769477427d0)
      double precision rat36
      parameter (rat36=-1.40232324657805996582267755542d-6)
      double precision rat37
      parameter (rat37=3.83232540585750223012855121268d-6)
      double precision rat38
      parameter (rat38=-0.0000102479220112189504079733850947d0)
      double precision rat39
      parameter (rat39=-7.80275262814230183791939255916d-6)
      double precision rat40
      parameter (rat40=-9.33308379102337234182175188963d-6)
      double precision rat41
      parameter (rat41=0.0000994678332817609362612644688626d0)
      double precision rat42
      parameter (rat42=0.0000720546227209424067952998773052d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-90720*IPi*(-2 + x**10)*zeta2)/
     -   (x**6*y**4) + 
     -  (351098434*(-1 + rat2*zeta2 + 
     -       rat1*zeta3 + 
     -       x**3*(rat9 + rat10*zeta2 + 
     -          rat11*zeta3) + 
     -       x**9*(rat9 + rat10*zeta2 + 
     -          rat11*zeta3) + 
     -       x**4*(rat12 + rat13*zeta2 + 
     -          rat14*zeta3) + 
     -       x**8*(rat12 + rat13*zeta2 + 
     -          rat14*zeta3) + 
     -       x**5*(rat15 + rat16*zeta2 + 
     -          rat17*zeta3) + 
     -       x**7*(rat15 + rat16*zeta2 + 
     -          rat17*zeta3) + 
     -       x**6*(rat18 + rat19*zeta2 + 
     -          rat20*zeta3) + 
     -       x**10*(rat6 + rat7*zeta2 + 
     -          rat21*zeta3) + 
     -       x**11*(rat3 + rat4*zeta2 + 
     -          rat22*zeta3) + 
     -       x**12*(-1 + rat2*zeta2 + 
     -          rat23*zeta3) + 
     -       x*(rat3 + rat4*zeta2 + rat5*zeta3) + 
     -       x**2*(rat6 + rat7*zeta2 + rat8*zeta3)
     -       ))/(3375d0*x**6*y**6) + 
     -  (23024*(rat24 + 
     -       x**6*(2.24862443054590379121303374257d0 - 
     -          (197526*zeta2)/1439d0) + 
     -       x**5*(4.90395297660412323372712531851d0 - 
     -          (181819*zeta2)/1439d0) + 
     -       x**7*(-11.1850185313875376418809358351d0 - 
     -          (155762*zeta2)/1439d0) + 
     -       x**4*(-7.22739016292178210176820322755d0 - 
     -          (120461*zeta2)/1439d0) + 
     -       x**8*(rat27 - (87968*zeta2)/1439d0) + 
     -       x**3*(rat26 - (54216*zeta2)/1439d0) + 
     -       x**9*(rat28 - (34080*zeta2)/1439d0) + 
     -       x**2*(-15.4193710910354412786657400973d0 - 
     -          (14968*zeta2)/1439d0) + 
     -       x**10*(-4.50914639796154737085939309706d0 - 
     -          (13960*zeta2)/1439d0) + 
     -       x**11*(-0.700388000926569376882094046792d0 - 
     -          (6668*zeta2)/1439d0) + 
     -       x*(rat25 - (99*zeta2)/1439d0) + zeta2)
     -      *lins(1))/(x**6*y**5) + 
     -  (2*(-58928 + 60*(4687 + 1890*IPi)*y - 
     -       15520*y**2 - 111460*y**3 - 
     -       201320*y**4 - 481378*y**5 - 
     -       889340*y**6 - 1100585*y**7 - 
     -       904064*y**8 - 490392*y**9 - 
     -       157808*y**10 + 4340*y**11)*lins(1)**2
     -     )/(15d0*x**6*y**5) + 
     -  (56*(-435 + 46*x + 78*x**2 + 78*x**3 + 
     -       31*x**4 + 62*x**6*y**2 + 
     -       32*x**7*y**2 + 30*x**8*y**2)*
     -     lins(1)**3)/(3d0*x**6*y**4) + 
     -  (1853704800*(rat30 + 
     -       x*(rat31 - (392*zeta2)/1.287295d6) + 
     -       x**6*(rat40 - (4*zeta2)/772377d0) + 
     -       x**11*(rat31 + 
     -          (196*zeta2)/1.287295d6) + 
     -       rat29*zeta2 + 
     -       x**2*(rat33 + rat32*zeta2) + 
     -       x**3*(rat34 + rat35*zeta2) + 
     -       x**9*(rat34 + rat35*zeta2) + 
     -       x**4*(rat36 + rat37*zeta2) + 
     -       x**8*(rat36 + rat37*zeta2) + 
     -       x**5*(rat38 + rat39*zeta2) + 
     -       x**7*(rat38 + rat39*zeta2) + 
     -       x**10*(rat33 + rat41*zeta2) + 
     -       x**12*(rat30 + rat42*zeta2))*lins(2))
     -    /(x**6*y**6) + 
     -  (4*(-87305 + 32675*x - 43560*x**2 - 
     -       41080*x**3 + 27205*x**4 + 
     -       25341*x**5 - 113400*IPi*x**10 - 
     -       113400*IPi*x**11 - 113400*IPi*y)*
     -     lins(1)*lins(2))/(15d0*x**6*y**5) - 
     -  (16*(-1197 - 1085*x - 40*x**2 + 24*x**3 - 
     -       28*x**4 + 52*x**5)*lins(1)**2*lins(2)
     -     )/(x**6*y**5) + 
     -  (8*(-2411 + 2252*x - 23147*x**2 - 
     -       83558*x**3 - 92232*x**4 - 
     -       49676*x**5 - 24809*x**6 - 
     -       49676*x**7 - 92232*x**8 - 
     -       83558*x**9 - 23147*x**10 + 
     -       2252*x**11 - 2411*x**12 + 
     -       28350*IPi*
     -        (-x - x**2 + y + x**10*y**2))*
     -     lins(2)**2)/(15d0*x**6*y**6) - 
     -  (16*(1567 + 1387*x - 578*x**2 - 
     -       250*x**3 + 137*x**4 + 101*x**5 + 
     -       945*x**10*y)*lins(1)*lins(2)**2)/
     -   (x**6*y**5) - 
     -  (112*(117 - 56*x - 84*x**2 - 72*x**3 - 
     -       26*x**4 - 26*x**6 - 72*x**7 - 
     -       84*x**8 - 56*x**9 + 117*x**10)*
     -     lins(2)**3)/(3d0*x**6*y**4) - 
     -  (30240*lins(1)*lins(3))/(x**6*y**4) + 
     -  (30240*(-1 + x)*
     -     (1 - x + x**2 - x**3 + x**4)*
     -     (x**2 + x**3 + x**4 - y)*lins(2)*
     -     lins(3))/(x**6*y**3) + 
     -  (30240*lins(4))/(x**6*y**4) + 
     -  (30240*(-1 + x)*
     -     (1 - x + x**2 - x**3 + x**4)*
     -     (x**2 + x**3 + x**4 - y)*lins(5))/
     -   (x**6*y**3)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^0
      function TwoLoop_HelAmppmpm_HE_6_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=8)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=3.83904522d12)
      double precision rat2
      parameter (rat2=-3.29513180363111221701108277125d-7)
      double precision rat3
      parameter (rat3=-4.47534886464991070531507484214d-9)
      double precision rat4
      parameter (rat4=1.88521615826379422434867352671d-8)
      double precision rat5
      parameter (rat5=5.97574078877641121172079111668d-8)
      double precision rat6
      parameter (rat6=-3.71304821462874042416202641135d-7)
      double precision rat7
      parameter (rat7=6.93212638601497553255355160069d-8)
      double precision rat8
      parameter (rat8=1.14790221308537738614416203886d-7)
      double precision rat9
      parameter (rat9=3.68940057560573471037271200584d-7)
      double precision rat10
      parameter (rat10=2.13415772164309124756806068567d-7)
      double precision rat11
      parameter (rat11=3.49497605752139989344589572421d-7)
      double precision rat12
      parameter (rat12=8.12170688627627053582869753225d-7)
      double precision rat13
      parameter (rat13=1.2981035732050527338612015041d-6)
      double precision rat14
      parameter (rat14=2.06631376954710629847699475652d-7)
      double precision rat15
      parameter (rat15=6.84469862459997569685076830408d-7)
      double precision rat16
      parameter (rat16=2.25376037638858549313988023303d-6)
      double precision rat17
      parameter (rat17=3.40809546506056639896636707094d-6)
      double precision rat18
      parameter (rat18=-7.04742588748841741089641727811d-8)
      double precision rat19
      parameter (rat19=9.60118526094469851825923465701d-7)
      double precision rat20
      parameter (rat20=3.20498438932167618489265932637d-6)
      double precision rat21
      parameter (rat21=6.75769620308527302699150458735d-6)
      double precision rat22
      parameter (rat22=-4.93540630917600913281245486345d-7)
      double precision rat23
      parameter (rat23=1.08220746428582438391148068186d-6)
      double precision rat24
      parameter (rat24=3.77336321138723132831449169541d-6)
      double precision rat25
      parameter (rat25=0.0000101027145586879947081101471255d0)
      double precision rat26
      parameter (rat26=-7.12117130692893132075861994318d-7)
      double precision rat27
      parameter (rat27=1.10624440372291252429274524952d-6)
      double precision rat28
      parameter (rat28=3.53666373328105783552088506006d-6)
      double precision rat29
      parameter (rat29=0.0000115283857942620894102877427888d0)
      double precision rat30
      parameter (rat30=-4.94639098485708034110279812403d-7)
      double precision rat31
      parameter (rat31=2.27666607167497756121768214025d-6)
      double precision rat32
      parameter (rat32=-7.5841635089327410075848668088d-8)
      double precision rat33
      parameter (rat33=4.05503949755507177901905515976d-7)
      double precision rat34
      parameter (rat34=-9.68190731548611453969797209109d-7)
      double precision rat35
      parameter (rat35=1.95137963669692504082217261996d-7)
      double precision rat36
      parameter (rat36=-1.0689251532181743876411020759d-6)
      double precision rat37
      parameter (rat37=2.140290496500064669725354264d-7)
      double precision rat38
      parameter (rat38=-4.74893077711650398324820982442d-7)
      double precision rat39
      parameter (rat39=8.56151762998335646938103358244d-8)
      double precision rat40
      parameter (rat40=-7.1483398676924154594876066607d-8)
      double precision rat41
      parameter (rat41=3.98960308851654179438570579449d-9)
      double precision rat42
      parameter (rat42=-17.174252362956512012527864111d0)
      double precision rat43
      parameter (rat43=-31.7642045045980776759533720568d0)
      double precision rat44
      parameter (rat44=-42.4762761563106718546512092425d0)
      double precision rat45
      parameter (rat45=-42.4774937880010546416215913903d0)
      double precision rat46
      parameter (rat46=-17.7646982686300045541342750537d0)
      double precision rat47
      parameter (rat47=95827.2781851851851851851851852d0)
      double precision rat48
      parameter (rat48=-0.850196327631848580982669092014d0)
      double precision rat49
      parameter (rat49=-0.184003278265491921549280065797d0)
      double precision rat50
      parameter (rat50=-6.22831251384911175939417646165d0)
      double precision rat51
      parameter (rat51=-2.25584201173126799353545705057d0)
      double precision rat52
      parameter (rat52=-0.907633000197707950946934246699d0)
      double precision rat53
      parameter (rat53=-20.8942653864942116143623654063d0)
      double precision rat54
      parameter (rat54=-14.494379467286851114976593781d0)
      double precision rat55
      parameter (rat55=-1.38449095615147062716187983326d0)
      double precision rat56
      parameter (rat56=-45.8380060679319961871013074418d0)
      double precision rat57
      parameter (rat57=-41.7955688176813271120727442168d0)
      double precision rat58
      parameter (rat58=-5.31155647577069266657907878145d0)
      double precision rat59
      parameter (rat59=-82.6259705651407630137987876135d0)
      double precision rat60
      parameter (rat60=-72.538851197002259595137758252d0)
      double precision rat61
      parameter (rat61=-11.2008190186282266705894291769d0)
      double precision rat62
      parameter (rat62=-118.09969994274161889257241653d0)
      double precision rat63
      parameter (rat63=-85.5947810601587417555674882988d0)
      double precision rat64
      parameter (rat64=-15.6715293227661641419939750543d0)
      double precision rat65
      parameter (rat65=-124.219694281583923429587470464d0)
      double precision rat66
      parameter (rat66=-75.0242904735169950995214785186d0)
      double precision rat67
      parameter (rat67=-15.8157888724664620944623619544d0)
      double precision rat68
      parameter (rat68=-95.8183561148687396233322817281d0)
      double precision rat69
      parameter (rat69=-48.1386856717224286872052724648d0)
      double precision rat70
      parameter (rat70=-11.1781114969161427336268127204d0)
      double precision rat71
      parameter (rat71=-52.8498951020291152224226468236d0)
      double precision rat72
      parameter (rat72=-21.8256142488301167099326725344d0)
      double precision rat73
      parameter (rat73=-5.23608735949229605255626526425d0)
      double precision rat74
      parameter (rat74=-19.6179546743156518614906988417d0)
      double precision rat75
      parameter (rat75=-6.4763963836012980068057332029d0)
      double precision rat76
      parameter (rat76=-1.40519487300660598145132424949d0)
      double precision rat77
      parameter (rat77=-5.92512567840465186401105132474d0)
      double precision rat78
      parameter (rat78=-1.05538474852063802534884106538d0)
      double precision rat79
      parameter (rat79=-0.0707940382788499211187454232157d0)
      double precision rat80
      parameter (rat80=-1.61922578767335301596569167728d0)
      double precision rat81
      parameter (rat81=-0.054910512952599848346514521087d0)
      double precision rat82
      parameter (rat82=0.0547651994232612597333691009781d0)
      double precision rat83
      parameter (rat83=-0.956788619244997102999509738379d0)
      double precision rat84
      parameter (rat84=-9.27978344921335294379819048892d0)
      double precision rat85
      parameter (rat85=-29.1810437558497125284128894237d0)
      double precision rat86
      parameter (rat86=-60.0649414471631679814591968623d0)
      double precision rat87
      parameter (rat87=-84.0649295248919195970940856621d0)
      double precision rat88
      parameter (rat88=-82.4996096291839372465124571021d0)
      double precision rat89
      parameter (rat89=-57.2125477447965414271070107412d0)
      double precision rat90
      parameter (rat90=-27.5617305121005482016312341222d0)
      double precision rat91
      parameter (rat91=-8.84170243348041181976200026742d0)
      double precision rat92
      parameter (rat92=-28.452269842308447294379375777d0)
      double precision rat93
      parameter (rat93=-52.1090675914414709153961918472d0)
      double precision rat94
      parameter (rat94=-69.5737067329712752731793496041d0)
      double precision rat95
      parameter (rat95=-52.1911823594843944251783026893d0)
      double precision rat96
      parameter (rat96=-28.7746358699208270627494601845d0)
      double precision rat97
      parameter (rat97=1.039486788d11)
      double precision rat98
      parameter (rat98=-3.70804905314486786916237361547d-6)
      double precision rat99
      parameter (rat99=-5.20256732690670812066155861521d-8)
      double precision rat100
      parameter (rat100=2.82828264715226418485700315072d-8)
      double precision rat101
      parameter (rat101=-0.0000107249488837819328461408656852d0)
      double precision rat102
      parameter (rat102=-2.43125424569289154511761496931d-7)
      double precision rat103
      parameter (rat103=-4.74080099611617189693420134167d-8)
      double precision rat104
      parameter (rat104=-0.0000208556173266789675316841705415d0)
      double precision rat105
      parameter (rat105=-7.9725970326440373011085172941d-7)
      double precision rat106
      parameter (rat106=4.77158541816887431184935849324d-8)
      double precision rat107
      parameter (rat107=-0.000054717765846838898607210259864d0)
      double precision rat108
      parameter (rat108=-9.18270301215147263541675650356d-7)
      double precision rat109
      parameter (rat109=-1.69930009730917330331667476663d-7)
      double precision rat110
      parameter (rat110=-0.000117463707484851649696965653016d0)
      double precision rat111
      parameter (rat111=-5.9968054158664304254726131257d-7)
      double precision rat112
      parameter (rat112=-5.60001187389363486124034849739d-7)
      double precision rat113
      parameter (rat113=-0.000178939548003182508944019402005d0)
      double precision rat114
      parameter (rat114=-5.02401767900103411415364713611d-7)
      double precision rat115
      parameter (rat115=-2.43843472638873163001806376156d-7)
      double precision rat116
      parameter (rat116=-0.000204714338963456519340259923214d0)
      double precision rat117
      parameter (rat117=-3.13385416496510583836299802975d-7)
      double precision rat118
      parameter (rat118=-1.65821138808897456581201841079d-7)
      double precision rat119
      parameter (rat119=-0.000179507510328580850931732413066d0)
      double precision rat120
      parameter (rat120=-9.05062008349450998505620256137d-7)
      double precision rat121
      parameter (rat121=-0.000120238907740691746050359612651d0)
      double precision rat122
      parameter (rat122=-1.7614846298556321814452922128d-6)
      double precision rat123
      parameter (rat123=-0.0000606604317257886430523187499458d0)
      double precision rat124
      parameter (rat124=-1.94557547373079262263793197918d-6)
      double precision rat125
      parameter (rat125=-0.0000205385223873251063068505943018d0)
      double precision rat126
      parameter (rat126=-1.33234978644096051752800151992d-6)
      double precision rat127
      parameter (rat127=-2.30018636209288052378144640097d-6)
      double precision rat128
      parameter (rat128=-5.17178290485400570574640146364d-7)
      double precision rat129
      parameter (rat129=-7.63453666907019889895897358919d-8)
      double precision rat130
      parameter (rat130=6.68751805883141890079190373189d-7)
      double precision rat131
      parameter (rat131=0.105283455160504340832209684669d0)
      double precision rat132
      parameter (rat132=0.00432854599862796584108059517896d0)
      double precision rat133
      parameter (rat133=20.5159919914326954416352376961d0)
      double precision rat134
      parameter (rat134=37.1459067840014899660101503934d0)
      double precision rat135
      parameter (rat135=49.4781543046049262001210597383d0)
      double precision rat136
      parameter (rat136=49.5085421613819434744144899194d0)
      double precision rat137
      parameter (rat137=20.4759864040601573776598221353d0)
      double precision rat138
      parameter (rat138=0.0184014370636998254799301919721d0)
      double precision rat139
      parameter (rat139=0.212344613510762070971495055265d0)
      double precision rat140
      parameter (rat140=0.109808732366201279813845258871d0)
      double precision rat141
      parameter (rat141=0.0508813945244328097731239092496d0)
      double precision rat142
      parameter (rat142=28.452269842308447294379375777d0)
      double precision rat143
      parameter (rat143=52.1090675914414709153961918472d0)
      double precision rat144
      parameter (rat144=69.5737067329712752731793496041d0)
      double precision rat145
      parameter (rat145=52.1911823594843944251783026893d0)
      double precision rat146
      parameter (rat146=28.7746358699208270627494601845d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(8)=dcmplx(Li4(-y),0d0)
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (250322*IPi*(-zeta2 - 
     -       (306763*x*zeta2)/125161d0 - 
     -       (3863751*x**2*zeta2)/625805d0 + 
     -       rat42*x**3*zeta2 + 
     -       rat43*x**4*zeta2 + 
     -       rat44*x**5*zeta2 + 
     -       rat45*x**6*zeta2 - 
     -       (3994464*x**7*zeta2)/125161d0 + 
     -       rat46*x**8*zeta2 - 
     -       (4487031*x**9*zeta2)/625805d0 - 
     -       (857561*x**10*zeta2)/625805d0 + 
     -       (21077*x**11*zeta2)/125161d0))/
     -   (x**6*y**5) + 
     -  (rat1*(rat4 + rat5*zeta2 + rat3*zeta3 + 
     -       rat2*zeta4 + 
     -       x**2*(rat11 + rat13*zeta2 + 
     -          rat10*zeta3 + rat12*zeta4) + 
     -       x**3*(rat15 + rat17*zeta2 + 
     -          rat14*zeta3 + rat16*zeta4) + 
     -       x**4*(rat19 + rat21*zeta2 + 
     -          rat18*zeta3 + rat20*zeta4) + 
     -       x**5*(rat23 + rat25*zeta2 + 
     -          rat22*zeta3 + rat24*zeta4) + 
     -       x**6*(rat27 + rat29*zeta2 + 
     -          rat26*zeta3 + rat28*zeta4) + 
     -       x**7*(rat23 + rat25*zeta2 + 
     -          rat30*zeta3 + rat31*zeta4) + 
     -       x**8*(rat19 + rat21*zeta2 + 
     -          rat32*zeta3 + rat33*zeta4) + 
     -       x**9*(rat15 + rat17*zeta2 + 
     -          rat35*zeta3 + rat34*zeta4) + 
     -       x**10*(rat11 + rat13*zeta2 + 
     -          rat37*zeta3 + rat36*zeta4) + 
     -       x**11*(rat8 + rat9*zeta2 + 
     -          rat39*zeta3 + rat38*zeta4) + 
     -       x**12*(rat4 + rat5*zeta2 + 
     -          rat41*zeta3 + rat40*zeta4) + 
     -       x*(rat8 + rat9*zeta2 + rat7*zeta3 + 
     -          rat6*zeta4)))/(x**6*y**6) + 
     -  ((96*IPi*(1916 + 548*x - 3036*x**2 - 
     -          2732*x**3 - 1042*x**4 - 
     -          266*x**5 + 133*x**6 + 521*x**7 + 
     -          1366*x**8 + 1518*x**9 + 
     -          776*x**10 + 92*x**11)*zeta2)/
     -      (x**6*y**5) + 
     -     (rat47*(-1 + rat49*zeta2 + 
     -          rat48*zeta3 + 
     -          x*(rat50 + rat51*zeta2 + 
     -             rat52*zeta3) + 
     -          x**2*
     -           (rat53 + rat54*zeta2 + 
     -             rat55*zeta3) + 
     -          x**3*
     -           (rat56 + rat57*zeta2 + 
     -             rat58*zeta3) + 
     -          x**4*
     -           (rat60 + rat59*zeta2 + 
     -             rat61*zeta3) + 
     -          x**5*
     -           (rat63 + rat62*zeta2 + 
     -             rat64*zeta3) + 
     -          x**6*
     -           (rat66 + rat65*zeta2 + 
     -             rat67*zeta3) + 
     -          x**7*
     -           (rat69 + rat68*zeta2 + 
     -             rat70*zeta3) + 
     -          x**8*
     -           (rat72 + rat71*zeta2 + 
     -             rat73*zeta3) + 
     -          x**9*
     -           (rat75 + rat74*zeta2 + 
     -             rat76*zeta3) + 
     -          x**10*
     -           (rat78 + rat77*zeta2 + 
     -             rat79*zeta3) + 
     -          x**11*
     -           (rat81 + rat80*zeta2 + 
     -             rat82*zeta3)))/(x**6*y**5))*
     -   lins(1) + ((76415*IPi*
     -        (-1 - (1308397*x)/382075d0 - 
     -          (4071511*x**2)/382075d0 + 
     -          rat92*x**3 + rat93*x**4 - 
     -          (5316424*x**5)/76415d0 + 
     -          rat94*x**6 + rat95*x**7 + 
     -          rat96*x**8 - 
     -          (138041*x**9)/12325d0 - 
     -          (1082979*x**10)/382075d0 - 
     -          (27669*x**11)/76415d0))/
     -      (3d0*x**6*y**5) + 
     -     (19944*(rat83 + 
     -          x**4*
     -           (rat86 - (31115*zeta2)/2493d0) + 
     -          x**3*
     -           (rat85 - (6316*zeta2)/831d0) + 
     -          x**5*
     -           (rat87 - (14609*zeta2)/2493d0) + 
     -          x**2*
     -           (rat84 - (1412*zeta2)/2493d0) + 
     -          x*(-2.30485108303249097472924187726d0 + 
     -             (265*zeta2)/2493d0) + 
     -          x**11*
     -           (-0.194894036635913892231581762268d0 + 
     -             (359*zeta2)/831d0) - zeta2 + 
     -          x**10*
     -           (-1.77520824976601149886348442305d0 + 
     -             (6527*zeta2)/2493d0) + 
     -          x**6*
     -           (rat88 + (16205*zeta2)/2493d0) + 
     -          x**9*
     -           (rat91 + (19628*zeta2)/2493d0) + 
     -          x**8*
     -           (rat90 + (11780*zeta2)/831d0) + 
     -          x**7*(rat89 + (37367*zeta2)/2493d0)
     -          ))/(x**6*y**5))*lins(1)**2 + 
     -  ((-97400 + (-621140 - 453600*IPi)*y - 
     -       23960*y**2 + 100352*y**3 + 
     -       452868*y**4 + 1630236*y**5 + 
     -       3123920*y**6 + 3866700*y**7 + 
     -       3149960*y**8 + 1813740*y**9 + 
     -       670400*y**10 + 171725*y**11)*
     -     lins(1)**3)/(45d0*x**6*y**5) - 
     -  (2*(3938 + 7230*x + 9868*x**2 + 
     -       14876*x**3 + 13752*x**4 + 
     -       5740*x**5 + 5691*x**6*y + 
     -       7782*x**7*y + 5454*x**8*y + 
     -       2246*x**9*y + 499*x**10*y)*lins(1)**4
     -     )/(3d0*x**6*y**5) + 
     -  ((-96*IPi*(1669 - 2680*y - 6012*y**2 - 
     -          7968*y**3 - 5950*y**4 - 
     -          1218*y**5 + 3531*y**6 + 
     -          5156*y**7 + 3543*y**8 + 
     -          1286*y**9 + 197*y**10)*zeta2)/
     -      (x**6*y**4) + 
     -     (rat97*(rat100 + rat98*zeta2 + 
     -          rat99*zeta3 + 
     -          x*(rat102 + rat101*zeta2 + 
     -             rat103*zeta3) + 
     -          x**2*
     -           (rat105 + rat104*zeta2 + 
     -             rat106*zeta3) + 
     -          x**3*
     -           (rat108 + rat107*zeta2 + 
     -             rat109*zeta3) + 
     -          x**4*
     -           (rat112 + rat110*zeta2 + 
     -             rat111*zeta3) + 
     -          x**5*
     -           (rat115 + rat113*zeta2 + 
     -             rat114*zeta3) + 
     -          x**6*
     -           (rat118 + rat116*zeta2 + 
     -             rat117*zeta3) + 
     -          x**7*
     -           (rat115 + rat119*zeta2 + 
     -             rat120*zeta3) + 
     -          x**8*
     -           (rat112 + rat121*zeta2 + 
     -             rat122*zeta3) + 
     -          x**9*
     -           (rat108 + rat123*zeta2 + 
     -             rat124*zeta3) + 
     -          x**10*
     -           (rat105 + rat125*zeta2 + 
     -             rat126*zeta3) + 
     -          x**11*
     -           (rat102 + rat127*zeta2 + 
     -             rat128*zeta3) + 
     -          x**12*
     -           (rat100 + rat130*zeta2 + 
     -             rat129*zeta3)))/(x**6*y**6))*
     -   lins(2) + ((-4*IPi*(-1 + x)*
     -        (-48700 - 243500*y - 240330*y**2 + 
     -          499680*y**3 + 3353344*y**4 + 
     -          7288452*y**5 + 9116519*y**6 + 
     -          7155578*y**7 + 3503453*y**8 + 
     -          984076*y**9 + 121865*y**10))/
     -      (15d0*x**6*y**5) + 
     -     (300608*(rat131 + 
     -          x*(0.371122820712984647410876919074d0 - 
     -             (337*zeta2)/671d0) + 
     -          x**5*
     -           (rat132 + (879*zeta2)/9394d0) + 
     -          x**4*
     -           (0.0753667382253447827218319021598d0 + 
     -             (525*zeta2)/1342d0) - zeta2 - 
     -          (57*x**6*zeta2)/1342d0 - 
     -          (1563*x**7*zeta2)/9394d0 - 
     -          (2049*x**8*zeta2)/4697d0 - 
     -          (207*x**9*zeta2)/427d0 - 
     -          (2643*x**10*zeta2)/9394d0 - 
     -          (591*x**11*zeta2)/9394d0 + 
     -          x**3*
     -           (0.162138806093724126511011756913d0 + 
     -             (4769*zeta2)/4697d0) + 
     -          x**2*
     -           (0.287090311782934733754405885553d0 + 
     -             (5333*zeta2)/4697d0)))/
     -      (x**6*y**5))*lins(1)*lins(2) + 
     -  ((48*IPi*(-223 + 461*x + 1518*x**2 + 
     -          1366*x**3 + 521*x**4 + 133*x**5))/
     -      (x**6*y**5) + 
     -     (35795*(1 + (424253*x)/178975d0 + 
     -          (4189871*x**2)/536925d0 + 
     -          rat133*x**3 + rat134*x**4 + 
     -          rat135*x**5 + rat136*x**6 + 
     -          (6646982*x**7)/178975d0 + 
     -          rat137*x**8 + 
     -          (4279271*x**9)/536925d0 + 
     -          (360993*x**10)/178975d0 + 
     -          (9223*x**11)/35795d0))/(x**6*y**5))
     -    *lins(1)**2*lins(2) + 
     -  (16*(-1693 - 1009*x + 1518*x**2 + 
     -       1366*x**3 + 521*x**4 + 133*x**5)*
     -     lins(1)**3*lins(2))/(3d0*x**6*y**5) + 
     -  ((-2*IPi*(-121865 - 112709*x + 
     -          103880*x**2 + 61584*x**3 + 
     -          15687*x**4 + 127*x**5 - 
     -          127*x**6 - 15687*x**7 - 
     -          61584*x**8 - 103880*x**9 + 
     -          112709*x**10 + 121865*x**11))/
     -      (15d0*x**6*y**5) + 
     -     (195584*(rat138 + 
     -          x**9*
     -           (rat139 - (3087*zeta2)/1528d0) + 
     -          x**10*
     -           (0.197790275959860383944153577661d0 - 
     -             (597*zeta2)/382d0) + 
     -          x**8*
     -           (0.189165678174083769633507853403d0 - 
     -             (7657*zeta2)/6112d0) + 
     -          x**11*
     -           (0.106456424520069808027923211169d0 - 
     -             (357*zeta2)/764d0) + 
     -          x**7*
     -           (rat140 - (1201*zeta2)/3056d0) + 
     -          x**12*
     -           (rat138 + (43*zeta2)/3056d0) + 
     -          x**6*
     -           (rat141 + (763*zeta2)/6112d0) + 
     -          x**5*
     -           (rat140 + (1525*zeta2)/1528d0) + 
     -          zeta2 + 
     -          x**4*
     -           (0.189165678174083769633507853403d0 + 
     -             (8437*zeta2)/3056d0) + 
     -          x*(0.106456424520069808027923211169d0 + 
     -             (2261*zeta2)/764d0) + 
     -          x**2*
     -           (0.197790275959860383944153577661d0 + 
     -             (6275*zeta2)/1528d0) + 
     -          x**3*(rat139 + (3143*zeta2)/764d0))
     -        )/(x**6*y**6))*lins(2)**2 + 
     -  (2*(-233850 - 240274*x + 192140*x**2 + 
     -       91924*x**3 - 13328*x**4 - 
     -       17056*x**5 - 127*x**6 - 15687*x**7 - 
     -       61584*x**8 - 103880*x**9 + 
     -       112709*x**10 + 121865*x**11 - 
     -       120*IPi*
     -        (2284 + 3652*x + 3036*x**2 + 
     -          2732*x**3 + 1042*x**4 + 
     -          266*x**5 - 133*x**6 - 521*x**7 - 
     -          1366*x**8 - 1518*x**9 - 
     -          566*x**10 + 118*x**11))*lins(1)*
     -     lins(2)**2)/(15d0*x**6*y**5) - 
     -  (8*(-1165 + 1459*x + 6112*x**2 + 
     -       5440*x**3 + 2112*x**4 + 480*x**5)*
     -     lins(1)**2*lins(2)**2)/(x**6*y**5) + 
     -  (2*(-102341 - 102341*x**12 - 
     -       146520*IPi*y + 
     -       x**2*(53283 - 546480*IPi*y) + 
     -       x*(-248558 - 392760*IPi*y) + 
     -       x**4*(370253 - 187560*IPi*y) + 
     -       2*x**5*(76253 - 23940*IPi*y) + 
     -       48*x**3*(9434 - 10245*IPi*y) + 
     -       10*x**6*(6487 + 4788*IPi*y) + 
     -       48*x**9*(9434 + 11385*IPi*y) + 
     -       2*x**7*(76253 + 93780*IPi*y) + 
     -       x**11*(-248558 + 146520*IPi*y) + 
     -       x**10*(53283 + 392760*IPi*y) + 
     -       x**8*(370253 + 491760*IPi*y))*
     -     lins(2)**3)/(45d0*x**6*y**6) + 
     -  (16*(2686 + 6498*x + 8570*x**2 + 
     -       7922*x**3 + 3291*x**4 + 847*x**5 - 
     -       399*x**6 - 1563*x**7 - 4098*x**8 - 
     -       4554*x**9 - 2013*x**10 + 39*x**11)*
     -     lins(1)*lins(2)**3)/(3d0*x**6*y**5) - 
     -  (4*(2284 + 3652*x + 3036*x**2 + 
     -       2732*x**3 + 1042*x**4 + 266*x**5 - 
     -       133*x**6 - 521*x**7 - 1366*x**8 - 
     -       1518*x**9 + 2584*x**10 + 3268*x**11)*
     -     lins(2)**4)/(3d0*x**6*y**5) + 
     -  (48*(-1 + x)*
     -     (315 + 1508*x + 6172*x**2 + 
     -       16676*x**3 + 29107*x**4 + 
     -       34532*x**5 + 29107*x**6 + 
     -       16676*x**7 + 6172*x**8 + 1508*x**9 + 
     -       315*x**10)*zeta2*lins(3))/(x**6*y**5)
     -    + (152830*(1 + (1308397*x)/382075d0 + 
     -       (4071511*x**2)/382075d0 + 
     -       rat142*x**3 + rat143*x**4 + 
     -       (5316424*x**5)/76415d0 + 
     -       rat144*x**6 + rat145*x**7 + 
     -       rat146*x**8 + (138041*x**9)/12325d0 + 
     -       (1082979*x**10)/382075d0 + 
     -       (27669*x**11)/76415d0)*lins(1)*lins(3)
     -     )/(3d0*x**6*y**5) - 
     -  (8*(1995 + 2873*x + 4664*x**2 + 
     -       10504*x**3 + 12431*x**4 + 
     -       5425*x**5 + 5691*x**6*y + 
     -       7782*x**7*y + 5454*x**8*y + 
     -       2246*x**9*y + 499*x**10*y)*
     -     lins(1)**2*lins(3))/(x**6*y**5) + 
     -  (4*(-1 + x)*(121865 + 234574*x + 
     -       130694*x**2 + 69110*x**3 + 
     -       53423*x**4 + 53296*x**5 + 
     -       53423*x**6 + 69110*x**7 + 
     -       130694*x**8 + 234574*x**9 + 
     -       121865*x**10)*lins(2)*lins(3))/
     -   (15d0*x**6*y**5) - 
     -  (32*(197 + 881*x + 1518*x**2 + 
     -       1366*x**3 + 521*x**4 + 133*x**5)*
     -     lins(1)*lins(2)*lins(3))/(x**6*y**5) - 
     -  (16*(-1 + x)*
     -     (1142 + 2968*x + 4486*x**2 + 
     -       5852*x**3 + 6373*x**4 + 6506*x**5 + 
     -       6373*x**6 + 5852*x**7 + 4486*x**8 + 
     -       2968*x**9 + 1142*x**10)*lins(2)**2*
     -     lins(3))/(x**6*y**5) + 
     -  (152830*(-1 - (1308397*x)/382075d0 - 
     -       (4071511*x**2)/382075d0 + 
     -       rat92*x**3 + rat93*x**4 - 
     -       (5316424*x**5)/76415d0 + rat94*x**6 + 
     -       rat95*x**7 + rat96*x**8 - 
     -       (138041*x**9)/12325d0 - 
     -       (1082979*x**10)/382075d0 - 
     -       (27669*x**11)/76415d0)*lins(4))/
     -   (3d0*x**6*y**5) + 
     -  (32*(105 + 983*x + 4664*x**2 + 
     -       10504*x**3 + 12431*x**4 + 
     -       5425*x**5 + 5691*x**6*y + 
     -       7782*x**7*y + 5454*x**8*y + 
     -       2246*x**9*y + 499*x**10*y)*lins(1)*
     -     lins(4))/(x**6*y**5) + 
     -  (32*(1339 + 2707*x + 3036*x**2 + 
     -       2732*x**3 + 1042*x**4 + 266*x**5 - 
     -       1260*x**10*y)*lins(2)*lins(4))/
     -   (x**6*y**5) + 
     -  (4*(-1 + x)*(121865 + 234574*x + 
     -       130694*x**2 + 69110*x**3 + 
     -       53423*x**4 + 53296*x**5 + 
     -       53423*x**6 + 69110*x**7 + 
     -       130694*x**8 + 234574*x**9 + 
     -       121865*x**10)*lins(5))/
     -   (15d0*x**6*y**5) - 
     -  (64*(-433 + 251*x + 1518*x**2 + 
     -       1366*x**3 + 521*x**4 + 133*x**5)*
     -     lins(1)*lins(5))/(x**6*y**5) - 
     -  (32*(-1 + x)*
     -     (79 + 1526*x + 4562*x**2 + 7294*x**3 + 
     -       8336*x**4 + 8602*x**5 + 8336*x**6 + 
     -       7294*x**7 + 4562*x**8 + 1526*x**9 + 
     -       79*x**10)*lins(2)*lins(5))/
     -   (x**6*y**5) - 
     -  (48*(-1 + x)*
     -     (4200 + 16800*y + 40446*y**2 + 
     -       62538*y**3 + 65121*y**4 + 
     -       45612*y**5 + 20157*y**6 + 
     -       4974*y**7 + 341*y**8)*lins(6))/
     -   (x**6*y**3) + 
     -  (96*(-92 - 776*x - 1518*x**2 - 
     -       1366*x**3 - 521*x**4 - 133*x**5 + 
     -       420*x**10*y)*lins(7))/(x**6*y**5) + 
     -  (96*(420 + 420*x + 133*x**6 + 521*x**7 + 
     -       1366*x**8 + 1518*x**9 + 776*x**10 + 
     -       92*x**11)*lins(8))/(x**6*y**5)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^4
      function TwoLoop_HelAmppmpm_HE_7_4(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (4*(6483 + 22643*x + 116765*x**2 + 
     -      359751*x**3 + 781234*x**4 + 
     -      1220050*x**5 + 1416228*x**6 + 
     -      1220050*x**7 + 781234*x**8 + 
     -      359751*x**9 + 116765*x**10 + 
     -      22643*x**11 + 6483*x**12))/(x**7*y**5)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^3
      function TwoLoop_HelAmppmpm_HE_7_3(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=27.8773091223248273687844725512d0)
      double precision rat2
      parameter (rat2=105.681146139979258049797429651d0)
      double precision rat3
      parameter (rat3=281.456114710530749531451105359d0)
      double precision rat4
      parameter (rat4=551.610188661337370422468879195d0)
      double precision rat5
      parameter (rat5=818.468691532693605267738559217d0)
      double precision rat6
      parameter (rat6=932.17971025925096472040465263d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2115244*(1 + (2865133*x)/528811d0 + 
     -       rat1*x**2 + rat2*x**3 + rat3*x**4 + 
     -       rat4*x**5 + rat5*x**6 + rat6*x**7 + 
     -       rat5*x**8 + rat4*x**9 + rat3*x**10 + 
     -       rat2*x**11 + rat1*x**12 + 
     -       (2865133*x**13)/528811d0 + x**14))/
     -   (15d0*x**7*y**7) - 
     -  (16*(-2468 + 3338*y - 9788*y**2 - 
     -       10608*y**3 - 6245*y**4 - 1072*y**5 + 
     -       26*y**6 + 128*y**8 + 1664*y**9 + 
     -       3680*y**10 + 5728*y**11 + 
     -       4048*y**12 + 3304*y**13)*lins(1))/
     -   (3d0*x**7*y**6) - 
     -  (64*(793 + 53*x + 630*x**2 - 930*x**3 - 
     -       567*x**4 - 803*x**5 + 466*x**6 - 
     -       803*x**7 - 567*x**8 - 930*x**9 + 
     -       630*x**10 + 53*x**11 + 793*x**12)*
     -     lins(2))/(3d0*x**7*y**5)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^2
      function TwoLoop_HelAmppmpm_HE_7_2(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=9.05253016439d11)
      double precision rat2
      parameter (rat2=-1.08528999303864760082746712945d-7)
      double precision rat3
      parameter (rat3=-5.3200595994087180548643129557d-9)
      double precision rat4
      parameter (rat4=-9.75116721947299486706378766677d-7)
      double precision rat5
      parameter (rat5=-4.31207621417855463071510442459d-7)
      double precision rat6
      parameter (rat6=-2.95427681701650633034704379375d-6)
      double precision rat7
      parameter (rat7=-2.38049609183524029982426044631d-6)
      double precision rat8
      parameter (rat8=-0.0000110551898952709721057433552318d0)
      double precision rat9
      parameter (rat9=-2.24104563182380163504835128765d-6)
      double precision rat10
      parameter (rat10=-0.0000287988612316949186715395938578d0)
      double precision rat11
      parameter (rat11=3.96061507172731763473334009747d-6)
      double precision rat12
      parameter (rat12=-0.0000557023496020549778811207680198d0)
      double precision rat13
      parameter (rat13=0.0000199481225677575028452232496139d0)
      double precision rat14
      parameter (rat14=-0.0000820387832477378613386945941667d0)
      double precision rat15
      parameter (rat15=0.0000397866532876697934891654730745d0)
      double precision rat16
      parameter (rat16=-0.0000932169884746097792174009248742d0)
      double precision rat17
      parameter (rat17=0.0000490496497968026690183254678844d0)
      double precision rat18
      parameter (rat18=16.3118147286043681182502808251d0)
      double precision rat19
      parameter (rat19=137.633637043760510398309307792d0)
      double precision rat20
      parameter (rat20=243.791518401661240083465217786d0)
      double precision rat21
      parameter (rat21=323.475753056205786202994387004d0)
      double precision rat22
      parameter (rat22=324.759042613104705371375076753d0)
      double precision rat23
      parameter (rat23=246.687935015990487664950525251d0)
      double precision rat24
      parameter (rat24=140.008405250717785971584835487d0)
      double precision rat25
      parameter (rat25=57.7008952657024309836352249165d0)
      double precision rat26
      parameter (rat26=16.3348952017141837838880223205d0)
      double precision rat27
      parameter (rat27=-6.45041953479529473564860396213d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (rat1*(rat2 + rat3*zeta2 + 
     -       x**4*(rat11 + rat10*zeta2) + 
     -       x**10*(rat11 + rat10*zeta2) + 
     -       x**5*(rat13 + rat12*zeta2) + 
     -       x**9*(rat13 + rat12*zeta2) + 
     -       x**6*(rat15 + rat14*zeta2) + 
     -       x**8*(rat15 + rat14*zeta2) + 
     -       x**7*(rat17 + rat16*zeta2) + 
     -       x**14*(rat2 + rat3*zeta2) + 
     -       x*(rat4 + rat5*zeta2) + 
     -       x**13*(rat4 + rat5*zeta2) + 
     -       x**2*(rat7 + rat6*zeta2) + 
     -       x**12*(rat7 + rat6*zeta2) + 
     -       x**3*(rat9 + rat8*zeta2) + 
     -       x**11*(rat9 + rat8*zeta2)))/
     -   (x**7*y**7) + 
     -  (4563338*(1 + (324114*x)/99203d0 + 
     -       rat18*x**2 + (5649168*x**3)/99203d0 + 
     -       rat19*x**4 + rat20*x**5 + 
     -       rat21*x**6 + rat22*x**7 + 
     -       rat23*x**8 + rat24*x**9 + 
     -       rat25*x**10 + rat26*x**11 + 
     -       (6473998*x**12)/2.281669d6 + 
     -       (516990*x**13)/2.281669d6)*lins(1))/
     -   (15d0*x**7*y**6) - 
     -  (16*(4224 + 8244*x + 28476*x**2 + 
     -       105438*x**3 + 265263*x**4 + 
     -       481119*x**5 + 651046*x**6 + 
     -       667012*x**7 + 518747*x**8 + 
     -       302701*x**9 + 128774*x**10 + 
     -       37686*x**11 + 6711*x**12 + 529*x**13)
     -      *lins(1)**2)/(x**7*y**6) - 
     -  (748414*(1 + (463519*x)/374207d0 + 
     -       (233522*x**2)/2.619449d6 - 
     -       (5334550*x**3)/2.619449d6 - 
     -       (349985*x**4)/63889d0 + rat27*x**5 - 
     -       (8942630*x**6)/2.619449d6 - 
     -       (3032172*x**7)/2.619449d6 - 
     -       (8942630*x**8)/2.619449d6 + 
     -       rat27*x**9 - (349985*x**10)/63889d0 - 
     -       (5334550*x**11)/2.619449d6 + 
     -       (233522*x**12)/2.619449d6 + 
     -       (463519*x**13)/374207d0 + x**14)*
     -     lins(2))/(15d0*x**7*y**7) + 
     -  (16*(-2279 - 3120*x - 4184*x**2 - 
     -       3212*x**3 - 411*x**4 + 1124*x**5 + 
     -       222*x**6)*lins(1)*lins(2))/
     -   (x**7*y**6) + 
     -  (8*(893 - 629*x - 823*x**2 - 3589*x**3 - 
     -       2810*x**4 - 1546*x**5 + 420*x**6 - 
     -       1546*x**7 - 2810*x**8 - 3589*x**9 - 
     -       823*x**10 - 629*x**11 + 893*x**12)*
     -     lins(2)**2)/(x**7*y**5)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^1
      function TwoLoop_HelAmppmpm_HE_7_1(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=5)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=467247.634360436237987258395422d0)
      double precision rat2
      parameter (rat2=-0.678150035866356366143538696274d0)
      double precision rat3
      parameter (rat3=-0.181346807207808580804249737274d0)
      double precision rat4
      parameter (rat4=-8.48997921219714121447935311135d0)
      double precision rat5
      parameter (rat5=-3.44438169751870807494380993795d0)
      double precision rat6
      parameter (rat6=-1.60312422132495150663380668576d0)
      double precision rat7
      parameter (rat7=-40.4340952144992920427548630475d0)
      double precision rat8
      parameter (rat8=-25.016433741208535362528970522d0)
      double precision rat9
      parameter (rat9=-4.16176745905137786889623909263d0)
      double precision rat10
      parameter (rat10=-130.075131368503089002357688362d0)
      double precision rat11
      parameter (rat11=-95.9353344968666533301169565227d0)
      double precision rat12
      parameter (rat12=-17.3997328228921663396444888157d0)
      double precision rat13
      parameter (rat13=-306.246859832557080341931524495d0)
      double precision rat14
      parameter (rat14=-250.641914290504668752864027899d0)
      double precision rat15
      parameter (rat15=-51.3528464041202041317589712735d0)
      double precision rat16
      parameter (rat16=-552.56991576790056146089908114d0)
      double precision rat17
      parameter (rat17=-485.791445973833829143209016704d0)
      double precision rat18
      parameter (rat18=-104.787124427110364098686032636d0)
      double precision rat19
      parameter (rat19=-781.712515447414175025875051663d0)
      double precision rat20
      parameter (rat20=-716.880264474166325310308333148d0)
      double precision rat21
      parameter (rat21=-157.673513105833623345642551726d0)
      double precision rat22
      parameter (rat22=-876.526750299936315678610102945d0)
      double precision rat23
      parameter (rat23=-815.097594289736294916471596285d0)
      double precision rat24
      parameter (rat24=-180.152591067088159534953945303d0)
      double precision rat25
      parameter (rat25=-3.87700197236865621969154974713d0)
      double precision rat26
      parameter (rat26=-1.03359324795950820822442799477d0)
      double precision rat27
      parameter (rat27=-0.393384549183634716938849350778d0)
      double precision rat28
      parameter (rat28=-0.39983069234358558752576287388d0)
      double precision rat29
      parameter (rat29=-6.72367271846255858633372815941d0)
      double precision rat30
      parameter (rat30=-11.5011829716613110114915169067d0)
      double precision rat31
      parameter (rat31=-3.30433051161777309276019951626d0)
      double precision rat32
      parameter (rat32=42.3395073733927524541242952795d0)
      double precision rat33
      parameter (rat33=126.87116554295512521402155234d0)
      double precision rat34
      parameter (rat34=195.981446388978622088472526843d0)
      double precision rat35
      parameter (rat35=189.053635617720197400444950729d0)
      double precision rat36
      parameter (rat36=112.300078741669767971985609943d0)
      double precision rat37
      parameter (rat37=31.430907056240991259041800558d0)
      double precision rat38
      parameter (rat38=-6.91232551300627628631754469815d0)
      double precision rat39
      parameter (rat39=-3.63631373969563758114557495682d0)
      double precision rat40
      parameter (rat40=-0.516049743048195858923037881264d0)
      double precision rat41
      parameter (rat41=19.8828679475853404612565190559d0)
      double precision rat42
      parameter (rat42=51.1040234863680307008784107492d0)
      double precision rat43
      parameter (rat43=94.1250968490021438499198864237d0)
      double precision rat44
      parameter (rat44=127.970348378675207758212611588d0)
      double precision rat45
      parameter (rat45=130.035386389228226281745744872d0)
      double precision rat46
      parameter (rat46=98.6685344177970992473040546087d0)
      double precision rat47
      parameter (rat47=54.9640744829580472790586828372d0)
      double precision rat48
      parameter (rat48=21.6105969656089550763580609206d0)
      double precision rat49
      parameter (rat49=5.50395666719354157553568678961d0)
      double precision rat50
      parameter (rat50=3.565641744d11)
      double precision rat51
      parameter (rat51=-1.7856645330995429360218977737d-6)
      double precision rat52
      parameter (rat52=1.27541800580279568874223379704d-7)
      double precision rat53
      parameter (rat53=-3.43931355993234075172976772285d-6)
      double precision rat54
      parameter (rat54=6.53618685546012005986275347421d-7)
      double precision rat55
      parameter (rat55=-1.29700074545683241249376622735d-6)
      double precision rat56
      parameter (rat56=9.19796142337410152933653747337d-7)
      double precision rat57
      parameter (rat57=5.88916508932889419983983394342d-7)
      double precision rat58
      parameter (rat58=6.55680005972018932028747305355d-7)
      double precision rat59
      parameter (rat59=2.14139863480681512439123907916d-7)
      double precision rat60
      parameter (rat60=3.2460916802638823941253476642d-7)
      double precision rat61
      parameter (rat61=-7.75499061933912307531735454674d-8)
      double precision rat62
      parameter (rat62=-6.12961188172582713626655364847d-8)
      double precision rat63
      parameter (rat63=-1.47844934684021166128655003895d-7)
      double precision rat64
      parameter (rat64=-1.331821966688305632535807557d-7)
      double precision rat65
      parameter (rat65=-9.5300836359854943234930882795d-8)
      double precision rat66
      parameter (rat66=-9.26172688424751625860480726972d-8)
      double precision rat67
      parameter (rat67=1.31512931939692929509297331134d-6)
      double precision rat68
      parameter (rat68=1.78494656977518266344371135453d-6)
      double precision rat69
      parameter (rat69=8.26465531754218771564841764989d-7)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-399168*IPi*(-2 + x**12)*zeta2)/
     -   (x**7*y**5) + 
     -  (rat1*(-1 + rat3*zeta2 + rat2*zeta3 + 
     -       x**3*(rat10 + rat11*zeta2 + 
     -          rat12*zeta3) + 
     -       x**11*(rat10 + rat11*zeta2 + 
     -          rat12*zeta3) + 
     -       x**4*(rat13 + rat14*zeta2 + 
     -          rat15*zeta3) + 
     -       x**10*(rat13 + rat14*zeta2 + 
     -          rat15*zeta3) + 
     -       x**5*(rat16 + rat17*zeta2 + 
     -          rat18*zeta3) + 
     -       x**9*(rat16 + rat17*zeta2 + 
     -          rat18*zeta3) + 
     -       x**6*(rat19 + rat20*zeta2 + 
     -          rat21*zeta3) + 
     -       x**8*(rat19 + rat20*zeta2 + 
     -          rat21*zeta3) + 
     -       x**7*(rat22 + rat23*zeta2 + 
     -          rat24*zeta3) + 
     -       x**12*(rat7 + rat8*zeta2 + 
     -          rat25*zeta3) + 
     -       x**13*(rat4 + rat5*zeta2 + 
     -          rat26*zeta3) + 
     -       x**14*(-1 + rat3*zeta2 + 
     -          rat27*zeta3) + 
     -       x*(rat4 + rat5*zeta2 + rat6*zeta3) + 
     -       x**2*(rat7 + rat8*zeta2 + rat9*zeta3)
     -       ))/(x**7*y**7) + 
     -  (115232*(rat28 + 
     -       x**7*(rat35 - 
     -          (1367376*zeta2)/3601d0) + 
     -       x**6*(rat34 - 
     -          (1270686*zeta2)/3601d0) + 
     -       x**8*(rat36 - 
     -          (1118172*zeta2)/3601d0) + 
     -       x**5*(rat33 - 
     -          (888350*zeta2)/3601d0) + 
     -       x**9*(rat37 - (53140*zeta2)/277d0) + 
     -       x**4*(rat32 - 
     -          (455569*zeta2)/3601d0) + 
     -       x**10*(rat38 - 
     -          (315192*zeta2)/3601d0) + 
     -       x**3*(rat31 - (12352*zeta2)/277d0) + 
     -       x**11*(-9.84851044462957820358542380203d0 - 
     -          (101240*zeta2)/3601d0) + 
     -       x**2*(rat30 - (33864*zeta2)/3601d0) + 
     -       x**12*
     -        (rat39 - (33270*zeta2)/3601d0) + 
     -       x**13*
     -        (rat40 - (14590*zeta2)/3601d0) + 
     -       x*(rat29 + (1428*zeta2)/3601d0) + 
     -       zeta2)*lins(1))/(x**7*y**6) + 
     -  ((-6529064*(1 + (1157462*x)/816133d0 + 
     -          (4103547*x**2)/816133d0 + 
     -          rat41*x**3 + rat42*x**4 + 
     -          rat43*x**5 + rat44*x**6 + 
     -          rat45*x**7 + rat46*x**8 + 
     -          rat47*x**9 + rat48*x**10 + 
     -          rat49*x**11 + 
     -          (1792624*x**12)/2.448399d6 + 
     -          (48232*x**13)/2.448399d6))/
     -      (35d0*x**7*y**6) + 
     -     (66528*IPi)/(x**7*y**5))*lins(1)**2 - 
     -  (32*(1122 - 134*x - 272*x**2 - 342*x**3 - 
     -       239*x**4 - 68*x**5 + 136*x**7*y**3 + 
     -       70*x**8*y**3 + 66*x**9*y**3)*
     -     lins(1)**3)/(x**7*y**5) + 
     -  (2*rat50*(rat52 + rat51*zeta2 + 
     -       x*(rat54 + rat53*zeta2) + 
     -       x**2*(rat56 + rat55*zeta2) + 
     -       x**3*(rat57 + rat58*zeta2) + 
     -       x**11*(rat57 + rat58*zeta2) + 
     -       x**4*(rat59 + rat60*zeta2) + 
     -       x**10*(rat59 + rat60*zeta2) + 
     -       x**5*(rat61 + rat62*zeta2) + 
     -       x**9*(rat61 + rat62*zeta2) + 
     -       x**6*(rat63 + rat64*zeta2) + 
     -       x**8*(rat63 + rat64*zeta2) + 
     -       x**7*(rat65 + rat66*zeta2) + 
     -       x**12*(rat56 + rat67*zeta2) + 
     -       x**13*(rat54 + rat68*zeta2) + 
     -       x**14*(rat52 + rat69*zeta2))*lins(2))
     -    /(x**7*y**7) + 
     -  (4*(-393155 - 20760*x - 613190*x**2 - 
     -       693760*x**3 + 17895*x**4 + 
     -       252008*x**5 + 38878*x**6 - 
     -       498960*IPi*x**12 - 
     -       498960*IPi*x**13 - 498960*IPi*y)*
     -     lins(1)*lins(2))/(15d0*x**7*y**6) + 
     -  (32*(2541 + 2331*x + 70*x**2 - 40*x**3 + 
     -       42*x**4 - 52*x**5 + 98*x**6)*
     -     lins(1)**2*lins(2))/(x**7*y**6) + 
     -  (8*(-33159 + 86807*x - 1051032*x**2 - 
     -       3620800*x**3 - 4770110*x**4 - 
     -       3751806*x**5 - 1726607*x**6 - 
     -       621970*x**7 - 1726607*x**8 - 
     -       3751806*x**9 - 4770110*x**10 - 
     -       3620800*x**11 - 1051032*x**12 + 
     -       86807*x**13 - 33159*x**14 + 
     -       873180*IPi*
     -        (-1 + x**12 + 2*x**13 + x**14 + 
     -          x*(-1 + y)))*lins(2)**2)/
     -   (105d0*x**7*y**7) - 
     -  (16*(6961 + 5700*x - 4044*x**2 - 
     -       3292*x**3 - 327*x**4 + 1020*x**5 + 
     -       418*x**6 + 4158*x**12*y)*lins(1)*
     -     lins(2)**2)/(x**7*y**6) - 
     -  (32*(693*x**12 + 34*x**2*y**3 + 
     -       77*x**9*y**3 + 
     -       2*x**7*(17 - 42*y)*y**3 - 
     -       84*x*y**4 + 77*(9 + y**3))*lins(2)**3
     -     )/(x**7*y**5) - 
     -  (133056*lins(1)*lins(3))/(x**7*y**5) + 
     -  (133056*(-1 + x)*(1 + x**2)*
     -     (1 - x + x**2)*(1 - x**2 + x**4)*
     -     (x**2 - y)*lins(2)*lins(3))/(x**7*y**4)
     -    + (133056*lins(4))/(x**7*y**5) + 
     -  (133056*(-1 + x)*(1 + x**2)*
     -     (1 - x + x**2)*(1 - x**2 + x**4)*
     -     (x**2 - y)*lins(5))/(x**7*y**4)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^0
      function TwoLoop_HelAmppmpm_HE_7_0(x) RESULT(res)
      use nielsen_generalized_polylog_wrapper
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=8)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2,zeta3,zeta4
      parameter (zeta2=1.64493406684822643647241516665d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision rat1
      parameter (rat1=8.1205636456728d16)
      double precision rat2
      parameter (rat2=-6.87487253791169390637698922188d-11)
      double precision rat3
      parameter (rat3=-1.49657122769278063332179685104d-12)
      double precision rat4
      parameter (rat4=3.57143944421207031368031833693d-12)
      double precision rat5
      parameter (rat5=1.09054405290471160369400211507d-11)
      double precision rat6
      parameter (rat6=-6.64558796097322483578136087997d-11)
      double precision rat7
      parameter (rat7=1.45488493530634322135654882795d-11)
      double precision rat8
      parameter (rat8=2.55308797280943070940762256954d-11)
      double precision rat9
      parameter (rat9=7.80262540883983897696863957746d-11)
      double precision rat10
      parameter (rat10=4.94410785113230652208432689996d-11)
      double precision rat11
      parameter (rat11=8.74773402020194926168995307817d-11)
      double precision rat12
      parameter (rat12=2.49207331941591267100979342856d-10)
      double precision rat13
      parameter (rat13=3.19717491373497806646061205647d-10)
      double precision rat14
      parameter (rat14=4.0714618807219574202331863358d-11)
      double precision rat15
      parameter (rat15=1.841760007564713049675661584d-10)
      double precision rat16
      parameter (rat16=7.47661598987070469072285559115d-10)
      double precision rat17
      parameter (rat17=9.85727032016832043433760540039d-10)
      double precision rat18
      parameter (rat18=-9.50155208958993781085008420214d-11)
      double precision rat19
      parameter (rat19=2.47376379894257720282050333978d-10)
      double precision rat20
      parameter (rat20=1.31693695987453403099292050484d-9)
      double precision rat21
      parameter (rat21=2.32093760000729656844832573896d-9)
      double precision rat22
      parameter (rat22=-3.98538983050445646136032842634d-10)
      double precision rat23
      parameter (rat23=1.9829910844633313053665901083d-10)
      double precision rat24
      parameter (rat24=1.94187013218015481424986625309d-9)
      double precision rat25
      parameter (rat25=4.19948876353722866420389099798d-9)
      double precision rat26
      parameter (rat26=-7.54874734697208065234409903d-10)
      double precision rat27
      parameter (rat27=6.74649371063582529916799553718d-11)
      double precision rat28
      parameter (rat28=2.38101304831261554887983052791d-9)
      double precision rat29
      parameter (rat29=5.94632919529855213695304570872d-9)
      double precision rat30
      parameter (rat30=-9.17340724854967147135576755197d-10)
      double precision rat31
      parameter (rat31=-4.27109656287086293359741650892d-12)
      double precision rat32
      parameter (rat32=2.33984526555924211407762963302d-9)
      double precision rat33
      parameter (rat33=6.66929535513186906135406948137d-9)
      double precision rat34
      parameter (rat34=-7.54926911230221095116456468298d-10)
      double precision rat35
      parameter (rat35=1.72116289088477448993785008666d-9)
      double precision rat36
      parameter (rat36=-3.99528466793802230545043280939d-10)
      double precision rat37
      parameter (rat37=7.82695915866244305238148383656d-10)
      double precision rat38
      parameter (rat38=-9.83612023329381088292928174186d-11)
      double precision rat39
      parameter (rat39=-2.58227395473638284418043762804d-11)
      double precision rat40
      parameter (rat40=-3.79963378729772967476152173853d-10)
      double precision rat41
      parameter (rat41=3.54960882503670151764400826882d-11)
      double precision rat42
      parameter (rat42=-3.08049355826795492519008758121d-10)
      double precision rat43
      parameter (rat43=4.76849489460605567967256046602d-11)
      double precision rat44
      parameter (rat44=-1.15735513076214938871071749248d-10)
      double precision rat45
      parameter (rat45=1.72652749152743981185573121329d-11)
      double precision rat46
      parameter (rat46=-1.57812690832475289541051386612d-11)
      double precision rat47
      parameter (rat47=1.6556955328520179865482161677d-13)
      double precision rat48
      parameter (rat48=2.65794831204850977289916750825d0)
      double precision rat49
      parameter (rat49=8.94525674212380481815347823859d0)
      double precision rat50
      parameter (rat50=30.5430986199237063467184437693d0)
      double precision rat51
      parameter (rat51=69.8324231527705294272418533123d0)
      double precision rat52
      parameter (rat52=117.128590104402298361501315992d0)
      double precision rat53
      parameter (rat53=150.386378378660006964205546529d0)
      double precision rat54
      parameter (rat54=150.386557476838806428951643926d0)
      double precision rat55
      parameter (rat55=117.16462856557337902037420882d0)
      double precision rat56
      parameter (rat56=70.4832200202674011790521559947d0)
      double precision rat57
      parameter (rat57=32.2146564397626612643419650696d0)
      double precision rat58
      parameter (rat58=10.8960637910807804425657105791d0)
      double precision rat59
      parameter (rat59=429025.85270251592700572292409d0)
      double precision rat60
      parameter (rat60=-0.811960393075764795093158922384d0)
      double precision rat61
      parameter (rat61=-0.186718462569185516649808569502d0)
      double precision rat62
      parameter (rat62=-7.34707988221120904128340426211d0)
      double precision rat63
      parameter (rat63=-2.89037917567180326978375663951d0)
      double precision rat64
      parameter (rat64=-0.827922135140638363543456185786d0)
      double precision rat65
      parameter (rat65=-28.9697033731170078073292362896d0)
      double precision rat66
      parameter (rat66=-19.727101922871819190609367419d0)
      double precision rat67
      parameter (rat67=-1.49279582096439064936565154075d0)
      double precision rat68
      parameter (rat68=-76.0274638717906529677537413772d0)
      double precision rat69
      parameter (rat69=-66.9149855702595277141420653067d0)
      double precision rat70
      parameter (rat70=-7.42355263660157252673451416747d0)
      double precision rat71
      parameter (rat71=-161.382332028211090031622245712d0)
      double precision rat72
      parameter (rat72=-147.245286221641841903616176497d0)
      double precision rat73
      parameter (rat73=-19.9341273867942997985492804775d0)
      double precision rat74
      parameter (rat74=-290.384378239553347585556262673d0)
      double precision rat75
      parameter (rat75=-218.695675059855410816226836267d0)
      double precision rat76
      parameter (rat76=-36.1687108183655669095530270078d0)
      double precision rat77
      parameter (rat77=-396.987166543067504430618121634d0)
      double precision rat78
      parameter (rat78=-251.837940316641216643084581791d0)
      double precision rat79
      parameter (rat79=-48.690921230998108558368944594d0)
      double precision rat80
      parameter (rat80=-416.047706081792308287314574108d0)
      double precision rat81
      parameter (rat81=-225.469136696518165861517751569d0)
      double precision rat82
      parameter (rat82=-49.5834362101956651942393046588d0)
      double precision rat83
      parameter (rat83=-334.278691202266358310532267294d0)
      double precision rat84
      parameter (rat84=-155.613178676866503379866668724d0)
      double precision rat85
      parameter (rat85=-37.9364364582604434154971631883d0)
      double precision rat86
      parameter (rat86=-203.088871315388760720913781655d0)
      double precision rat87
      parameter (rat87=-80.944971314973037299281832276d0)
      double precision rat88
      parameter (rat88=-21.1476673092031453998873013913d0)
      double precision rat89
      parameter (rat89=-90.3222044028525692421246995554d0)
      double precision rat90
      parameter (rat90=-30.4008733593016899504183136391d0)
      double precision rat91
      parameter (rat91=-8.09364745300729280223671330018d0)
      double precision rat92
      parameter (rat92=-27.5384108792064989051971743924d0)
      double precision rat93
      parameter (rat93=-7.60238784894619638898254503888d0)
      double precision rat94
      parameter (rat94=-1.81158313678340761925616791355d0)
      double precision rat95
      parameter (rat95=-6.54240036481986819983339497723d0)
      double precision rat96
      parameter (rat96=-1.06525395406552900496091683222d0)
      double precision rat97
      parameter (rat97=-0.089505095690879823085779047112d0)
      double precision rat98
      parameter (rat98=-1.54464582099600884973698293436d0)
      double precision rat99
      parameter (rat99=-0.0506681194773178477311283564533d0)
      double precision rat100
      parameter (rat100=0.0417690446557439174400302219856d0)
      double precision rat101
      parameter (rat101=-0.946813623701665435212786737666d0)
      double precision rat102
      parameter (rat102=-2.86764219470559759484318071798d0)
      double precision rat103
      parameter (rat103=-13.6066169520242553950419118958d0)
      double precision rat104
      parameter (rat104=-49.8868520726642717011898392316d0)
      double precision rat105
      parameter (rat105=-126.264218528922180607573866001d0)
      double precision rat106
      parameter (rat106=-228.35630779871249373657078312d0)
      double precision rat107
      parameter (rat107=-302.775866741827817269229789294d0)
      double precision rat108
      parameter (rat108=-298.020645872081666624202739773d0)
      double precision rat109
      parameter (rat109=-218.006433801922766609765004629d0)
      double precision rat110
      parameter (rat110=-117.181724845431306105463408834d0)
      double precision rat111
      parameter (rat111=-45.0363978933645867835273934792d0)
      double precision rat112
      parameter (rat112=-11.8095630016051364365971107544d0)
      double precision rat113
      parameter (rat113=-1.96772383372824785344849550307d0)
      double precision rat114
      parameter (rat114=-0.175950654793752707075339499096d0)
      double precision rat115
      parameter (rat115=-3.92256167627177684984721481201d0)
      double precision rat116
      parameter (rat116=-15.5919184792913236532636062596d0)
      double precision rat117
      parameter (rat117=-50.5353342534249897481800263681d0)
      double precision rat118
      parameter (rat118=-113.824283011962661328371305487d0)
      double precision rat119
      parameter (rat119=-190.343580685130231184052136567d0)
      double precision rat120
      parameter (rat120=-244.365253341211434417013170833d0)
      double precision rat121
      parameter (rat121=-244.36535034768000211650476875d0)
      double precision rat122
      parameter (rat122=-190.363100503110821071568726878d0)
      double precision rat123
      parameter (rat123=-114.176779649806648470604835332d0)
      double precision rat124
      parameter (rat124=-51.4407143203594530598926755707d0)
      double precision rat125
      parameter (rat125=-16.6485506792657492206412127572d0)
      double precision rat126
      parameter (rat126=-3.52618195767872339487364907469d0)
      double precision rat127
      parameter (rat127=22.2906320367285475381517030842d0)
      double precision rat128
      parameter (rat128=65.0542641156166592977513818067d0)
      double precision rat129
      parameter (rat129=141.23125580369642086920049251d0)
      double precision rat130
      parameter (rat130=233.817024896844194465854536531d0)
      double precision rat131
      parameter (rat131=299.644192736231388984678781956d0)
      double precision rat132
      parameter (rat132=299.742425385678936541207730983d0)
      double precision rat133
      parameter (rat133=234.237021502458767405684190272d0)
      double precision rat134
      parameter (rat134=141.639176396697717005546685231d0)
      double precision rat135
      parameter (rat135=64.8322877401068258496503458706d0)
      double precision rat136
      parameter (rat136=4.80613713533328216531521841357d0)
      double precision rat137
      parameter (rat137=3.319926027012d14)
      double precision rat138
      parameter (rat138=-4.82809724791129423521319626784d-9)
      double precision rat139
      parameter (rat139=-4.08683804687403595075267969776d-11)
      double precision rat140
      parameter (rat140=4.96512523478327616870717369589d-11)
      double precision rat141
      parameter (rat141=-1.42305899349459022397324671107d-8)
      double precision rat142
      parameter (rat142=-2.7997545658636872742687531159d-10)
      double precision rat143
      parameter (rat143=7.53751734116862290917121585766d-11)
      double precision rat144
      parameter (rat144=-3.5054728043065323173082620049d-8)
      double precision rat145
      parameter (rat145=-1.22516579947224959894638830134d-9)
      double precision rat146
      parameter (rat146=4.01551115643330985161218481624d-10)
      double precision rat147
      parameter (rat147=-1.20318854550327887667166377533d-7)
      double precision rat148
      parameter (rat148=-1.72769913611712091057872605227d-9)
      double precision rat149
      parameter (rat149=1.6578682642979579800223134623d-10)
      double precision rat150
      parameter (rat150=-3.15708026609112751737575638897d-7)
      double precision rat151
      parameter (rat151=-1.40637821282738986342059438116d-9)
      double precision rat152
      parameter (rat152=-8.67296432683315459548882356614d-10)
      double precision rat153
      parameter (rat153=-5.92834923711770681960562590803d-7)
      double precision rat154
      parameter (rat154=-1.37545233322859653342548913413d-9)
      double precision rat155
      parameter (rat155=-9.05770197310499499364088308302d-10)
      double precision rat156
      parameter (rat156=-8.48872283103893332114004657701d-7)
      double precision rat157
      parameter (rat157=-8.51488851558613999739367274765d-10)
      double precision rat158
      parameter (rat158=-3.37137849920924577668990851135d-10)
      double precision rat159
      parameter (rat159=-9.54158417167699485797318707914d-7)
      double precision rat160
      parameter (rat160=-4.6574531703998447438766387499d-10)
      double precision rat161
      parameter (rat161=2.70818761152187787432070088077d-12)
      double precision rat162
      parameter (rat162=-8.49050956964447350992351597379d-7)
      double precision rat163
      parameter (rat163=-1.3361261557964124138993784427d-9)
      double precision rat164
      parameter (rat164=-5.96223322100310843014981625925d-7)
      double precision rat165
      parameter (rat165=-3.18542037200691368161496600593d-9)
      double precision rat166
      parameter (rat166=-3.27165012806151648240766370956d-7)
      double precision rat167
      parameter (rat167=-4.34592815701548426763058542954d-9)
      double precision rat168
      parameter (rat168=-1.38189244688728067344324145475d-7)
      double precision rat169
      parameter (rat169=-3.78148184563590043443229079959d-9)
      double precision rat170
      parameter (rat170=-4.10684363318116320881160787025d-8)
      double precision rat171
      parameter (rat171=-2.13749340866694861424272228721d-9)
      double precision rat172
      parameter (rat172=-4.9284340790432407485755433919d-9)
      double precision rat173
      parameter (rat173=-6.9013585885891739168370723198d-10)
      double precision rat174
      parameter (rat174=-7.94234563826463590429294356357d-11)
      double precision rat175
      parameter (rat175=8.63754873567223808448701799949d-10)
      double precision rat176
      parameter (rat176=0.089461798775395001353632054007d0)
      double precision rat177
      parameter (rat177=0.385221815172583286772230766425d0)
      double precision rat178
      parameter (rat178=0.376135039677739568872990912425d0)
      double precision rat179
      parameter (rat179=0.3123027777297766411108615052d0)
      double precision rat180
      parameter (rat180=0.223022777979382551778826122603d0)
      double precision rat181
      parameter (rat181=0.0676870345089771725794466812974d0)
      double precision rat182
      parameter (rat182=0.0107858438887832863882216730892d0)
      double precision rat183
      parameter (rat183=2.83609913699370608080820632623d0)
      double precision rat184
      parameter (rat184=11.5676747292620643806423152676d0)
      double precision rat185
      parameter (rat185=36.2927814400835047126729585148d0)
      double precision rat186
      parameter (rat186=80.4319358306047257205698699588d0)
      double precision rat187
      parameter (rat187=134.106318150470024666198609497d0)
      double precision rat188
      parameter (rat188=172.191548378678960416032407781d0)
      double precision rat189
      parameter (rat189=172.164391950245108699028884926d0)
      double precision rat190
      parameter (rat190=134.117817445277696661675437561d0)
      double precision rat191
      parameter (rat191=80.4417476343437983460599320282d0)
      double precision rat192
      parameter (rat192=36.2418783589832804180206152259d0)
      double precision rat193
      parameter (rat193=11.7295173005113421021565837626d0)
      double precision rat194
      parameter (rat194=0.0158385996074352238735800379636d0)
      double precision rat195
      parameter (rat195=0.0935761615333533141752319834512d0)
      double precision rat196
      parameter (rat196=0.203124557357189451122914919392d0)
      double precision rat197
      parameter (rat197=0.283828565950630529886889965168d0)
      double precision rat198
      parameter (rat198=0.321279509783056749788648027396d0)
      double precision rat199
      parameter (rat199=0.254437430423854102914768276804d0)
      double precision rat200
      parameter (rat200=0.121037381909446488702848781127d0)
      double precision rat201
      parameter (rat201=0.0558213572112789333924363278571d0)
      double precision rat202
      parameter (rat202=8.07766732551563632722605260757d0)
      double precision rat203
      parameter (rat203=7.73386640703559793590665402327d0)
      double precision rat204
      parameter (rat204=4.37712521610762217223855712405d0)
      double precision rat205
      parameter (rat205=3.92256167627177684984721481201d0)
      double precision rat206
      parameter (rat206=15.5919184792913236532636062596d0)
      double precision rat207
      parameter (rat207=50.5353342534249897481800263681d0)
      double precision rat208
      parameter (rat208=113.824283011962661328371305487d0)
      double precision rat209
      parameter (rat209=190.343580685130231184052136567d0)
      double precision rat210
      parameter (rat210=244.365253341211434417013170833d0)
      double precision rat211
      parameter (rat211=244.36535034768000211650476875d0)
      double precision rat212
      parameter (rat212=190.363100503110821071568726878d0)
      double precision rat213
      parameter (rat213=114.176779649806648470604835332d0)
      double precision rat214
      parameter (rat214=51.4407143203594530598926755707d0)
      double precision rat215
      parameter (rat215=16.6485506792657492206412127572d0)
      double precision rat216
      parameter (rat216=3.52618195767872339487364907469d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(8)=dcmplx(Li4(-y),0d0)
      lins(7)=dcmplx(Li4(-(x/y)),0d0)
      lins(6)=dcmplx(Li4(-x),0d0)
      lins(5)=dcmplx(Li3(-y),0d0)
      lins(4)=dcmplx(Li3(-x),0d0)
      lins(3)=dcmplx(Li2_S11(-x),0d0)
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-36851296*IPi*
     -     (1 + rat48*x + rat49*x**2 + 
     -       rat50*x**3 + rat51*x**4 + 
     -       rat52*x**5 + rat53*x**6 + 
     -       rat54*x**7 + rat55*x**8 + 
     -       rat56*x**9 + rat57*x**10 + 
     -       rat58*x**11 + 
     -       (8872559*x**12)/4.606412d6 - 
     -       (1416473*x**13)/9.212824d6)*zeta2)/
     -   (35d0*x**7*y**6) + 
     -  (rat1*(rat4 + rat5*zeta2 + rat3*zeta3 + 
     -       rat2*zeta4 + 
     -       x**2*(rat11 + rat13*zeta2 + 
     -          rat10*zeta3 + rat12*zeta4) + 
     -       x**3*(rat15 + rat17*zeta2 + 
     -          rat14*zeta3 + rat16*zeta4) + 
     -       x**4*(rat19 + rat21*zeta2 + 
     -          rat18*zeta3 + rat20*zeta4) + 
     -       x**5*(rat23 + rat25*zeta2 + 
     -          rat22*zeta3 + rat24*zeta4) + 
     -       x**6*(rat27 + rat29*zeta2 + 
     -          rat26*zeta3 + rat28*zeta4) + 
     -       x**7*(rat31 + rat33*zeta2 + 
     -          rat30*zeta3 + rat32*zeta4) + 
     -       x**8*(rat27 + rat29*zeta2 + 
     -          rat34*zeta3 + rat35*zeta4) + 
     -       x**9*(rat23 + rat25*zeta2 + 
     -          rat36*zeta3 + rat37*zeta4) + 
     -       x**10*(rat19 + rat21*zeta2 + 
     -          rat38*zeta3 + rat39*zeta4) + 
     -       x**11*(rat15 + rat17*zeta2 + 
     -          rat41*zeta3 + rat40*zeta4) + 
     -       x**12*(rat11 + rat13*zeta2 + 
     -          rat43*zeta3 + rat42*zeta4) + 
     -       x**13*(rat8 + rat9*zeta2 + 
     -          rat45*zeta3 + rat44*zeta4) + 
     -       x**14*(rat4 + rat5*zeta2 + 
     -          rat47*zeta3 + rat46*zeta4) + 
     -       x*(rat8 + rat9*zeta2 + rat7*zeta3 + 
     -          rat6*zeta4)))/(x**7*y**7) + 
     -  ((96*IPi*(8378 + 1236*x - 18800*x**2 - 
     -          22152*x**3 - 13938*x**4 - 
     -          4840*x**5 - 188*x**6 + 94*x**7 + 
     -          2420*x**8 + 6969*x**9 + 
     -          11076*x**10 + 9400*x**11 + 
     -          4002*x**12 + 431*x**13)*zeta2)/
     -      (x**7*y**6) + 
     -     (rat59*(-1 + rat61*zeta2 + 
     -          rat60*zeta3 + 
     -          x**13*
     -           (rat99 + rat98*zeta2 + 
     -             rat100*zeta3) + 
     -          x*(rat62 + rat63*zeta2 + 
     -             rat64*zeta3) + 
     -          x**2*
     -           (rat65 + rat66*zeta2 + 
     -             rat67*zeta3) + 
     -          x**3*
     -           (rat68 + rat69*zeta2 + 
     -             rat70*zeta3) + 
     -          x**4*
     -           (rat72 + rat71*zeta2 + 
     -             rat73*zeta3) + 
     -          x**5*
     -           (rat75 + rat74*zeta2 + 
     -             rat76*zeta3) + 
     -          x**6*
     -           (rat78 + rat77*zeta2 + 
     -             rat79*zeta3) + 
     -          x**7*
     -           (rat81 + rat80*zeta2 + 
     -             rat82*zeta3) + 
     -          x**8*
     -           (rat84 + rat83*zeta2 + 
     -             rat85*zeta3) + 
     -          x**9*
     -           (rat87 + rat86*zeta2 + 
     -             rat88*zeta3) + 
     -          x**10*
     -           (rat90 + rat89*zeta2 + 
     -             rat91*zeta3) + 
     -          x**11*
     -           (rat93 + rat92*zeta2 + 
     -             rat94*zeta3) + 
     -          x**12*
     -           (rat96 + rat95*zeta2 + 
     -             rat97*zeta3)))/(x**7*y**6))*
     -   lins(1) + ((2267890*IPi*
     -        (-1 + rat115*x + rat116*x**2 + 
     -          rat117*x**3 + rat118*x**4 + 
     -          rat119*x**5 + rat120*x**6 + 
     -          rat121*x**7 + rat122*x**8 + 
     -          rat123*x**9 + rat124*x**10 + 
     -          rat125*x**11 + rat126*x**12 - 
     -          (2126626*x**13)/5.669725d6))/
     -      (21d0*x**7*y**6) + 
     -     (85440*(rat101 + 
     -          x**5*
     -           (rat106 - (36691*zeta2)/890d0) + 
     -          x**4*
     -           (rat105 - (16708*zeta2)/445d0) + 
     -          x**6*
     -           (rat107 - (1631*zeta2)/89d0) + 
     -          x**3*
     -           (rat104 - (1529*zeta2)/89d0) + 
     -          x**2*(rat103 - (191*zeta2)/89d0) + 
     -          x*(rat102 + (59*zeta2)/890d0) + 
     -          x**13*
     -           (rat114 + (397*zeta2)/890d0) - 
     -          zeta2 + 
     -          x**12*
     -           (rat113 + (3019*zeta2)/890d0) + 
     -          x**11*
     -           (rat112 + (1131*zeta2)/89d0) + 
     -          x**7*
     -           (rat108 + (8202*zeta2)/445d0) + 
     -          x**10*
     -           (rat111 + (13183*zeta2)/445d0) + 
     -          x**8*
     -           (rat109 + (39111*zeta2)/890d0) + 
     -          x**9*(rat110 + (8077*zeta2)/178d0))
     -        )/(x**7*y**6))*lins(1)**2 + 
     -  ((-264302*(-1 + (2204814*x)/660755d0 + 
     -          rat127*x**2 + rat128*x**3 + 
     -          rat129*x**4 + rat130*x**5 + 
     -          rat131*x**6 + rat132*x**7 + 
     -          rat133*x**8 + rat134*x**9 + 
     -          rat135*x**10 + 
     -          (2852192*x**11)/132151d0 + 
     -          rat136*x**12 + 
     -          (2569114*x**13)/4.625285d6))/
     -      (9d0*x**7*y**6) - 
     -     (44352*IPi)/(x**7*y**5))*lins(1)**3 - 
     -  (4*(-604 - 9482*y - 4804*y**2 - 
     -       5520*y**3 - 1741*y**4 + 8230*y**5 + 
     -       30358*y**6 + 57456*y**7 + 
     -       73840*y**8 + 65908*y**9 + 
     -       42808*y**10 + 19904*y**11 + 
     -       6380*y**12 + 1058*y**13)*lins(1)**4)/
     -   (3d0*x**7*y**6) + 
     -  ((-96*IPi*(6954 - 16488*y - 46116*y**2 - 
     -          79440*y**3 - 88145*y**4 - 
     -          57308*y**5 + 1398*y**6 + 
     -          52492*y**7 + 69624*y**8 + 
     -          53098*y**9 + 25486*y**10 + 
     -          7145*y**11 + 893*y**12)*zeta2)/
     -      (x**7*y**5) + 
     -     (rat137*(rat140 + rat138*zeta2 + 
     -          rat139*zeta3 + 
     -          x*(rat142 + rat141*zeta2 + 
     -             rat143*zeta3) + 
     -          x**2*
     -           (rat145 + rat144*zeta2 + 
     -             rat146*zeta3) + 
     -          x**3*
     -           (rat148 + rat147*zeta2 + 
     -             rat149*zeta3) + 
     -          x**4*
     -           (rat151 + rat150*zeta2 + 
     -             rat152*zeta3) + 
     -          x**5*
     -           (rat155 + rat153*zeta2 + 
     -             rat154*zeta3) + 
     -          x**6*
     -           (rat158 + rat156*zeta2 + 
     -             rat157*zeta3) + 
     -          x**7*
     -           (rat161 + rat159*zeta2 + 
     -             rat160*zeta3) + 
     -          x**8*
     -           (rat158 + rat162*zeta2 + 
     -             rat163*zeta3) + 
     -          x**9*
     -           (rat155 + rat164*zeta2 + 
     -             rat165*zeta3) + 
     -          x**10*
     -           (rat151 + rat166*zeta2 + 
     -             rat167*zeta3) + 
     -          x**11*
     -           (rat148 + rat168*zeta2 + 
     -             rat169*zeta3) + 
     -          x**12*
     -           (rat145 + rat170*zeta2 + 
     -             rat171*zeta3) + 
     -          x**13*
     -           (rat142 + rat172*zeta2 + 
     -             rat173*zeta3) + 
     -          x**14*
     -           (rat140 + rat175*zeta2 + 
     -             rat174*zeta3)))/(x**7*y**7))*
     -   lins(2) + ((-674876*IPi*(-1 + x)*
     -        (1 + (827209*x)/506157d0 - 
     -          (200351*x**2)/3.543099d6 - 
     -          (1777869*x**3)/1.181033d6 - 
     -          (7332166*x**4)/3.543099d6 - 
     -          (2480946*x**5)/1.181033d6 - 
     -          (7443388*x**6)/3.543099d6 - 
     -          (2480946*x**7)/1.181033d6 - 
     -          (7332166*x**8)/3.543099d6 - 
     -          (1777869*x**9)/1.181033d6 - 
     -          (200351*x**10)/3.543099d6 + 
     -          (827209*x**11)/506157d0 + x**12))/
     -      (5d0*x**7*y**6) + 
     -     (1322720*(rat176 + 
     -          x*(rat177 - (2394*zeta2)/5905d0) + 
     -          x**6*
     -           (rat182 + (122*zeta2)/5905d0) + 
     -          x**5*
     -           (rat181 + (16836*zeta2)/41335d0) 
     -           - zeta2 - 
     -          (282*x**7*zeta2)/41335d0 - 
     -          (1452*x**8*zeta2)/8267d0 - 
     -          (20907*x**9*zeta2)/41335d0 - 
     -          (33228*x**10*zeta2)/41335d0 - 
     -          (5640*x**11*zeta2)/8267d0 - 
     -          (13392*x**12*zeta2)/41335d0 - 
     -          (2679*x**13*zeta2)/41335d0 + 
     -          x**4*
     -           (rat180 + (6981*zeta2)/5905d0) + 
     -          x**2*
     -           (rat178 + (1884*zeta2)/1181d0) + 
     -          x**3*
     -           (rat179 + (77452*zeta2)/41335d0)))
     -       /(x**7*y**6))*lins(1)*lins(2) + 
     -  ((48*IPi*(-955 + 2616*x + 9400*x**2 + 
     -          11076*x**3 + 6969*x**4 + 
     -          2420*x**5 + 94*x**6))/(x**7*y**6) 
     -      + (3218980*
     -        (1 + rat183*x + rat184*x**2 + 
     -          rat185*x**3 + rat186*x**4 + 
     -          rat187*x**5 + rat188*x**6 + 
     -          rat189*x**7 + rat190*x**8 + 
     -          rat191*x**9 + rat192*x**10 + 
     -          rat193*x**11 + 
     -          (9996241*x**12)/4.023725d6 + 
     -          (1063313*x**13)/4.023725d6))/
     -      (21d0*x**7*y**6))*lins(1)**2*lins(2) + 
     -  (16*(-7423 - 3852*x + 9400*x**2 + 
     -       11076*x**3 + 6969*x**4 + 2420*x**5 + 
     -       94*x**6)*lins(1)**3*lins(2))/
     -   (3d0*x**7*y**6) + 
     -  ((-2*IPi*(-3543099 - 2247364*x + 
     -          5990814*x**2 + 5133256*x**3 + 
     -          1998559*x**4 + 110672*x**5 + 
     -          550*x**6 - 550*x**7 - 
     -          110672*x**8 - 1998559*x**9 - 
     -          5133256*x**10 - 5990814*x**11 + 
     -          2247364*x**12 + 3543099*x**13))/
     -      (105d0*x**7*y**6) + 
     -     (864320*(rat194 + 
     -          x**11*
     -           (rat197 - (8918*zeta2)/2701d0) + 
     -          x**10*
     -           (rat198 - (15159*zeta2)/5402d0) + 
     -          x**12*
     -           (rat196 - (28657*zeta2)/13505d0) 
     -           + x**9*
     -           (rat199 - (36883*zeta2)/27010d0) 
     -           + x**13*
     -           (rat195 - (3131*zeta2)/5402d0) + 
     -          x**8*
     -           (rat200 - (4263*zeta2)/13505d0) + 
     -          x**14*
     -           (rat194 + (31*zeta2)/5402d0) + 
     -          x**7*
     -           (rat201 + (849*zeta2)/13505d0) + 
     -          x**6*
     -           (rat200 + (12078*zeta2)/13505d0) 
     -           + zeta2 + 
     -          x*(rat195 + 
     -             (42239*zeta2)/13505d0) + 
     -          x**5*
     -           (rat199 + (1151*zeta2)/365d0) + 
     -          x**2*
     -           (rat196 + (69082*zeta2)/13505d0) 
     -           + x**4*
     -           (rat198 + (15879*zeta2)/2701d0) + 
     -          x**3*(rat197 + (2392*zeta2)/365d0))
     -        )/(x**7*y**7))*lins(2)**2 + 
     -  ((1917668*(-1 - (352352*x)/479417d0 + 
     -          (5792782*x**2)/3.355919d6 + 
     -          (4789968*x**3)/3.355919d6 + 
     -          (976127*x**4)/3.355919d6 - 
     -          (780422*x**5)/3.355919d6 - 
     -          (245068*x**6)/3.355919d6 - 
     -          (275*x**7)/3.355919d6 - 
     -          (55336*x**8)/3.355919d6 - 
     -          (1998559*x**9)/6.711838d6 - 
     -          (2566628*x**10)/3.355919d6 - 
     -          (2995407*x**11)/3.355919d6 + 
     -          (160526*x**12)/479417d0 + 
     -          (506157*x**13)/958834d0))/
     -      (15d0*x**7*y**6) - 
     -     (16*IPi*(10102 + 17244*x + 
     -          18800*x**2 + 22152*x**3 + 
     -          13938*x**4 + 4840*x**5 + 
     -          188*x**6 - 94*x**7 - 2420*x**8 - 
     -          6969*x**9 - 11076*x**10 - 
     -          9400*x**11 - 3078*x**12 + 
     -          493*x**13))/(x**7*y**6))*lins(1)*
     -   lins(2)**2 - 
     -  (16*(-2603 + 4329*x + 18870*x**2 + 
     -       22112*x**3 + 13980*x**4 + 
     -       4788*x**5 + 286*x**6)*lins(1)**2*
     -     lins(2)**2)/(x**7*y**6) + 
     -  ((867458*(-1 - (885933*x)/433729d0 + 
     -          (7113314*x**2)/3.036103d6 + 
     -          rat202*x**3 + rat203*x**4 + 
     -          rat204*x**5 + 
     -          (4744226*x**6)/3.036103d6 + 
     -          (1506508*x**7)/3.036103d6 + 
     -          (4744226*x**8)/3.036103d6 + 
     -          rat204*x**9 + rat203*x**10 + 
     -          rat202*x**11 + 
     -          (7113314*x**12)/3.036103d6 - 
     -          (885933*x**13)/433729d0 - x**14))/
     -      (45d0*x**7*y**7) + 
     -     (16*IPi*(-1817 - 5388*x - 9400*x**2 - 
     -          11076*x**3 - 6969*x**4 - 
     -          2420*x**5 - 94*x**6 + 94*x**7 + 
     -          2420*x**8 + 6969*x**9 + 
     -          11076*x**10 + 9400*x**11 + 
     -          5388*x**12 + 1817*x**13))/
     -      (x**7*y**6))*lins(2)**3 + 
     -  (16*(11857 + 31602*x + 52496*x**2 + 
     -       63084*x**3 + 41571*x**4 + 
     -       15436*x**5 + 1178*x**6 - 282*x**7 - 
     -       7260*x**8 - 20907*x**9 - 
     -       33228*x**10 - 28200*x**11 - 
     -       10620*x**12 + 93*x**13)*lins(1)*
     -     lins(2)**3)/(3d0*x**7*y**6) - 
     -  (4*(10102 + 17244*x + 18800*x**2 + 
     -       22152*x**3 + 13938*x**4 + 
     -       4840*x**5 + 188*x**6 - 94*x**7 - 
     -       2420*x**8 - 6969*x**9 - 
     -       11076*x**10 - 9400*x**11 + 
     -       10782*x**12 + 14353*x**13)*lins(2)**4
     -     )/(3d0*x**7*y**6) + 
     -  (96*(-1 + x)*
     -     (627 + 3999*x + 20467*x**2 + 
     -       67035*x**3 + 145484*x**4 + 
     -       223742*x**5 + 257000*x**6 + 
     -       223742*x**7 + 145484*x**8 + 
     -       67035*x**9 + 20467*x**10 + 
     -       3999*x**11 + 627*x**12)*zeta2*lins(3)
     -     )/(x**7*y**6) + 
     -  (4535780*(1 + rat205*x + rat206*x**2 + 
     -       rat207*x**3 + rat208*x**4 + 
     -       rat209*x**5 + rat210*x**6 + 
     -       rat211*x**7 + rat212*x**8 + 
     -       rat213*x**9 + rat214*x**10 + 
     -       rat215*x**11 + rat216*x**12 + 
     -       (2126626*x**13)/5.669725d6)*lins(1)*
     -     lins(3))/(21d0*x**7*y**6) - 
     -  (16*(604 - 1606*y + 4804*y**2 + 
     -       5520*y**3 + 6097*y**4 + 12548*y**5 + 
     -       30578*y**6 + 57456*y**7 + 
     -       73840*y**8 + 65908*y**9 + 
     -       42808*y**10 + 19904*y**11 + 
     -       6380*y**12 + 1058*y**13)*lins(1)**2*
     -     lins(3))/(x**7*y**6) + 
     -  (4*(-1 + x)*(3543099 + 5790463*x - 
     -       200351*x**2 - 5333607*x**3 - 
     -       7332166*x**4 - 7442838*x**5 - 
     -       7443388*x**6 - 7442838*x**7 - 
     -       7332166*x**8 - 5333607*x**9 - 
     -       200351*x**10 + 5790463*x**11 + 
     -       3543099*x**12)*lins(2)*lins(3))/
     -   (105d0*x**7*y**6) - 
     -  (32*(893 + 4464*x + 9400*x**2 + 
     -       11076*x**3 + 6969*x**4 + 2420*x**5 + 
     -       94*x**6)*lins(1)*lins(2)*lins(3))/
     -   (x**7*y**6) - 
     -  (16*(-1 + x)*
     -     (5051 + 13673*x + 23073*x**2 + 
     -       34149*x**3 + 41118*x**4 + 
     -       43538*x**5 + 43632*x**6 + 
     -       43538*x**7 + 41118*x**8 + 
     -       34149*x**9 + 23073*x**10 + 
     -       13673*x**11 + 5051*x**12)*lins(2)**2*
     -     lins(3))/(x**7*y**6) - 
     -  (4535780*(1 + rat205*x + rat206*x**2 + 
     -       rat207*x**3 + rat208*x**4 + 
     -       rat209*x**5 + rat210*x**6 + 
     -       rat211*x**7 + rat212*x**8 + 
     -       rat213*x**9 + rat214*x**10 + 
     -       rat215*x**11 + rat216*x**12 + 
     -       (2126626*x**13)/5.669725d6)*lins(4))/
     -   (21d0*x**7*y**6) + 
     -  (64*(604 + 2552*y + 4804*y**2 + 
     -       5520*y**3 + 6097*y**4 + 12548*y**5 + 
     -       30578*y**6 + 57456*y**7 + 
     -       73840*y**8 + 65908*y**9 + 
     -       42808*y**10 + 19904*y**11 + 
     -       6380*y**12 + 1058*y**13)*lins(1)*
     -     lins(4))/(x**7*y**6) - 
     -  (64*(-2972 - 6543*x - 9400*x**2 - 
     -       11076*x**3 - 6969*x**4 - 2420*x**5 - 
     -       94*x**6 + 2772*x**12*y)*lins(2)*
     -     lins(4))/(x**7*y**6) + 
     -  (4*(-1 + x)*(3543099 + 5790463*x - 
     -       200351*x**2 - 5333607*x**3 - 
     -       7332166*x**4 - 7442838*x**5 - 
     -       7443388*x**6 - 7442838*x**7 - 
     -       7332166*x**8 - 5333607*x**9 - 
     -       200351*x**10 + 5790463*x**11 + 
     -       3543099*x**12)*lins(5))/
     -   (105d0*x**7*y**6) - 
     -  (64*(-1879 + 1692*x + 9400*x**2 + 
     -       11076*x**3 + 6969*x**4 + 2420*x**5 + 
     -       94*x**6)*lins(1)*lins(5))/(x**7*y**6)
     -    - (64*(-1 + x)*
     -     (200 + 3971*x + 13371*x**2 + 
     -       24447*x**3 + 31416*x**4 + 
     -       33836*x**5 + 33930*x**6 + 
     -       33836*x**7 + 31416*x**8 + 
     -       24447*x**9 + 13371*x**10 + 
     -       3971*x**11 + 200*x**12)*lins(2)*
     -     lins(5))/(x**7*y**6) - 
     -  (192*(-1 + x)*
     -     (395 - 3158*x - 9381*x**2 - 
     -       22204*x**3 - 33044*x**4 - 
     -       38880*x**5 - 33044*x**6 - 
     -       22204*x**7 - 9381*x**8 - 3158*x**9 + 
     -       395*x**10)*lins(6))/(x**7*y**4) + 
     -  (96*(-431 - 4002*x - 9400*x**2 - 
     -       11076*x**3 - 6969*x**4 - 2420*x**5 - 
     -       94*x**6 + 1848*x**12*y)*lins(7))/
     -   (x**7*y**6) + 
     -  (96*(1848 + 1848*x + 94*x**7 + 
     -       2420*x**8 + 6969*x**9 + 
     -       11076*x**10 + 9400*x**11 + 
     -       4002*x**12 + 431*x**13)*lins(8))/
     -   (x**7*y**6)
      return
      end


