      ! For one-loop fermion loop
      ! We need to multiply I*8*Nc*Qf^4*a^2

      ! get the form factors from helicity amplitudes
      ! for high-energy expansion one-loop case
      subroutine Get_OneLoop_FormFactors_HE(iterm,m2,shat,that,uhat,FFs)
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
      double complex OneLoop_HelAmp_HE
      external OneLoop_HelAmp_HE
      xs=shat/m2
      xt=that/m2
      xu=uhat/m2
      do ihel=1,5
         amp(ihel)=OneLoop_HelAmp_HE(ihel,iterm,xs,xt,xu)
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

      double complex function OneLoop_HelAmp_HE(ihel,iterm,xs,xt,xu)
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
      double complex OneLoop_HelAmp_HE_pppp
      double complex OneLoop_HelAmp_HE_mppp
      double complex OneLoop_HelAmp_HE_mmpp
      double complex OneLoop_HelAmp_HE_pmpm
      double complex OneLoop_HelAmp_HE_pmmp
      external OneLoop_HelAmp_HE_pppp
      external OneLoop_HelAmp_HE_mppp
      external OneLoop_HelAmp_HE_mmpp
      external OneLoop_HelAmp_HE_pmpm
      external OneLoop_HelAmp_HE_pmmp
      integer iterm_used
      SELECT CASE(ihel)
      CASE(1)
         ! --++,++--
         if(iterm.EQ.-1)then
            iterm_used=10
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_HE=OneLoop_HelAmp_HE_pppp(iterm_used,xs,xt,xu)
      CASE(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         if(iterm.EQ.-1)then
            iterm_used=9
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_HE=OneLoop_HelAmp_HE_mppp(iterm_used,xs,xt,xu)
      CASE(3)
         ! ++++,----
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_HE=OneLoop_HelAmp_HE_mmpp(iterm_used,xs,xt,xu)
      CASE(4)
         ! +--+,-++-
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_HE=OneLoop_HelAmp_HE_pmpm(iterm_used,xs,xt,xu)
      CASE(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4                                                    
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_HE=OneLoop_HelAmp_HE_pmmp(iterm_used,xs,xt,xu)
      CASE DEFAULT
         write(*,*)"ERROR: unknown helicity configuration #4 =",ihel
         stop
      end select
      return
      end

      ! ++++ (4 -> 0)
      ! --++,++-- (2 -> 2)
      function OneLoop_HelAmp_HE_pppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 10
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x,y
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,fyx,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_7_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_8_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_8_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_8_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_9_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_9_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_9_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_10_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_10_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppppp_HE_10_0
      IF(iterm.LT.0.OR.iterm.GT.10)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>10"
         STOP
      ENDIF
      x=xt/xs
      y=xu/xs
      fxy=OneLoop_HelAmppppp_HE_0_0(x)
      fyx=OneLoop_HelAmppppp_HE_0_0(y)
      res=(fxy+fyx)
      IF(iterm.LE.1)return
      logm2oms=-DLOG(xs)+IPi
      fxy=OneLoop_HelAmppppp_HE_2_0(x)
      fyx=OneLoop_HelAmppppp_HE_2_0(y)
      res=res+(-1d0/xs)**2*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_2_1(x)
      fyx=OneLoop_HelAmppppp_HE_2_1(y)
      res=res+(-1d0/xs)**2*logm2oms*(fxy+fyx)
      IF(iterm.LE.2)return
      fxy=OneLoop_HelAmppppp_HE_3_0(x)
      fyx=OneLoop_HelAmppppp_HE_3_0(y)
      res=res+(-1d0/xs)**3*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_3_1(x)
      fyx=oneLoop_HelAmppppp_HE_3_1(y)
      res=res+(-1d0/xs)**3*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_3_2(x)
      fyx=OneLoop_HelAmppppp_HE_3_2(y)
      res=res+(-1d0/xs)**3*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.3)return
      fxy=OneLoop_HelAmppppp_HE_4_0(x)
      fyx=OneLoop_HelAmppppp_HE_4_0(y)
      res=res+(-1d0/xs)**4*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_4_1(x)
      fyx=OneLoop_HelAmppppp_HE_4_1(y)
      res=res+(-1d0/xs)**4*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_4_2(x)
      fyx=OneLoop_HelAmppppp_HE_4_2(y)
      res=res+(-1d0/xs)**4*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.4)return
      fxy=OneLoop_HelAmppppp_HE_5_0(x)
      fyx=OneLoop_HelAmppppp_HE_5_0(y)
      res=res+(-1d0/xs)**5*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_5_1(x)
      fyx=OneLoop_HelAmppppp_HE_5_1(y)
      res=res+(-1d0/xs)**5*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_5_2(x)
      fyx=OneLoop_HelAmppppp_HE_5_2(y)
      res=res+(-1d0/xs)**5*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.5)return
      fxy=OneLoop_HelAmppppp_HE_6_0(x)
      fyx=OneLoop_HelAmppppp_HE_6_0(y)
      res=res+(-1d0/xs)**6*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_6_1(x)
      fyx=OneLoop_HelAmppppp_HE_6_1(y)
      res=res+(-1d0/xs)**6*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_6_2(x)
      fyx=OneLoop_HelAmppppp_HE_6_2(y)
      res=res+(-1d0/xs)**6*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.6)return
      fxy=OneLoop_HelAmppppp_HE_7_0(x)
      fyx=OneLoop_HelAmppppp_HE_7_0(y)
      res=res+(-1d0/xs)**7*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_7_1(x)
      fyx=OneLoop_HelAmppppp_HE_7_1(y)
      res=res+(-1d0/xs)**7*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_7_2(x)
      fyx=OneLoop_HelAmppppp_HE_7_2(y)
      res=res+(-1d0/xs)**7*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.7)return
      fxy=OneLoop_HelAmppppp_HE_8_0(x)
      fyx=OneLoop_HelAmppppp_HE_8_0(y)
      res=res+(-1d0/xs)**8*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_8_1(x)
      fyx=OneLoop_HelAmppppp_HE_8_1(y)
      res=res+(-1d0/xs)**8*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_8_2(x)
      fyx=OneLoop_HelAmppppp_HE_8_2(y)
      res=res+(-1d0/xs)**8*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.8)return
      fxy=OneLoop_HelAmppppp_HE_9_0(x)
      fyx=OneLoop_HelAmppppp_HE_9_0(y)
      res=res+(-1d0/xs)**9*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_9_1(x)
      fyx=OneLoop_HelAmppppp_HE_9_1(y)
      res=res+(-1d0/xs)**9*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_9_2(x)
      fyx=OneLoop_HelAmppppp_HE_9_2(y)
      res=res+(-1d0/xs)**9*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.9)return
      fxy=OneLoop_HelAmppppp_HE_10_0(x)
      fyx=OneLoop_HelAmppppp_HE_10_0(y)
      res=res+(-1d0/xs)**10*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_10_1(x)
      fyx=OneLoop_HelAmppppp_HE_10_1(y)
      res=res+(-1d0/xs)**10*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmppppp_HE_10_2(x)
      fyx=OneLoop_HelAmppppp_HE_10_2(y)
      res=res+(-1d0/xs)**10*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.10)return
      WRITE(*,*)"ERROR: cannot reach here (++++)"
      STOP
      return
      end

      ! -+++ (4 -> 0)
      ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+ (2 -> 2)
      function OneLoop_HelAmp_HE_mppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 9
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x,y
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,fyx,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_7_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_8_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_8_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_8_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_9_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_9_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmppp_HE_9_0
      IF(iterm.LT.0.OR.iterm.GT.9)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>9"
         STOP
      ENDIF
      x=xt/xs
      y=xu/xs
      fxy=OneLoop_HelAmpmppp_HE_0_0(x)
      fyx=OneLoop_HelAmpmppp_HE_0_0(y)
      res=(fxy+fyx)
      IF(iterm.LE.0)return
      logm2oms=-DLOG(xs)+IPi
      fxy=OneLoop_HelAmpmppp_HE_1_0(x)
      fyx=OneLoop_HelAmpmppp_HE_1_0(y)
      res=res+(-1d0/xs)*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_1_1(x)
      fyx=OneLoop_HelAmpmppp_HE_1_1(y)
      res=res+(-1d0/xs)*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_1_2(x)
      fyx=OneLoop_HelAmpmppp_HE_1_2(y)
      res=res+(-1d0/xs)*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.1)return
      fxy=OneLoop_HelAmpmppp_HE_2_0(x)
      fyx=OneLoop_HelAmpmppp_HE_2_0(y)
      res=res+(-1d0/xs)**2*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_2_1(x)
      fyx=OneLoop_HelAmpmppp_HE_2_1(y)
      res=res+(-1d0/xs)**2*logm2oms*(fxy+fyx)
      IF(iterm.LE.2)return
      fxy=OneLoop_HelAmpmppp_HE_3_0(x)
      fyx=OneLoop_HelAmpmppp_HE_3_0(y)
      res=res+(-1d0/xs)**3*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_3_1(x)
      fyx=OneLoop_HelAmpmppp_HE_3_1(y)
      res=res+(-1d0/xs)**3*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_3_2(x)
      fyx=OneLoop_HelAmpmppp_HE_3_2(y)
      res=res+(-1d0/xs)**3*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.3)return
      fxy=OneLoop_HelAmpmppp_HE_4_0(x)
      fyx=OneLoop_HelAmpmppp_HE_4_0(y)
      res=res+(-1d0/xs)**4*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_4_1(x)
      fyx=OneLoop_HelAmpmppp_HE_4_1(y)
      res=res+(-1d0/xs)**4*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_4_2(x)
      fyx=OneLoop_HelAmpmppp_HE_4_2(y)
      res=res+(-1d0/xs)**4*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.4)return
      fxy=OneLoop_HelAmpmppp_HE_5_0(x)
      fyx=OneLoop_HelAmpmppp_HE_5_0(y)
      res=res+(-1d0/xs)**5*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_5_1(x)
      fyx=OneLoop_HelAmpmppp_HE_5_1(y)
      res=res+(-1d0/xs)**5*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_5_2(x)
      fyx=OneLoop_HelAmpmppp_HE_5_2(y)
      res=res+(-1d0/xs)**5*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.5)return
      fxy=OneLoop_HelAmpmppp_HE_6_0(x)
      fyx=OneLoop_HelAmpmppp_HE_6_0(y)
      res=res+(-1d0/xs)**6*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_6_1(x)
      fyx=OneLoop_HelAmpmppp_HE_6_1(y)
      res=res+(-1d0/xs)**6*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_6_2(x)
      fyx=OneLoop_HelAmpmppp_HE_6_2(y)
      res=res+(-1d0/xs)**6*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.6)return
      fxy=OneLoop_HelAmpmppp_HE_7_0(x)
      fyx=OneLoop_HelAmpmppp_HE_7_0(y)
      res=res+(-1d0/xs)**7*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_7_1(x)
      fyx=OneLoop_HelAmpmppp_HE_7_1(y)
      res=res+(-1d0/xs)**7*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_7_2(x)
      fyx=OneLoop_HelAmpmppp_HE_7_2(y)
      res=res+(-1d0/xs)**7*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.7)return
      fxy=OneLoop_HelAmpmppp_HE_8_0(x)
      fyx=OneLoop_HelAmpmppp_HE_8_0(y)
      res=res+(-1d0/xs)**8*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_8_1(x)
      fyx=OneLoop_HelAmpmppp_HE_8_1(y)
      res=res+(-1d0/xs)**8*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_8_2(x)
      fyx=OneLoop_HelAmpmppp_HE_8_2(y)
      res=res+(-1d0/xs)**8*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.8)return
      fxy=OneLoop_HelAmpmppp_HE_9_0(x)
      fyx=OneLoop_HelAmpmppp_HE_9_0(y)
      res=res+(-1d0/xs)**9*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_9_1(x)
      fyx=OneLoop_HelAmpmppp_HE_9_1(y)
      res=res+(-1d0/xs)**9*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmppp_HE_9_2(x)
      fyx=OneLoop_HelAmpmppp_HE_9_2(y)
      res=res+(-1d0/xs)**9*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.9)return
      WRITE(*,*)"ERROR: cannot reach here (-+++)"
      STOP
      return
      end

      ! --++ (4 -> 0)
      ! ++++,---- (2 -> 2)
      function OneLoop_HelAmp_HE_mmpp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 7
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x,y
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,fyx,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_2_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmpmmpp_HE_7_0
      IF(iterm.LT.0.OR.iterm.GT.7)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>7"
         STOP
      ENDIF
      x=xt/xs
      y=xu/xs
      fxy=OneLoop_HelAmpmmpp_HE_0_0(x)
      fyx=OneLoop_HelAmpmmpp_HE_0_0(y)
      res=(fxy+fyx)
      IF(iterm.LE.0)return
      logm2oms=-DLOG(xs)+IPi
      fxy=OneLoop_HelAmpmmpp_HE_1_0(x)
      fyx=OneLoop_HelAmpmmpp_HE_1_0(y)
      res=res+(-1d0/xs)*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_1_1(x)
      fyx=OneLoop_HelAmpmmpp_HE_1_1(y)
      res=res+(-1d0/xs)*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_1_2(x)
      fyx=OneLoop_HelAmpmmpp_HE_1_2(y)
      res=res+(-1d0/xs)*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.1)return
      fxy=OneLoop_HelAmpmmpp_HE_2_0(x)
      fyx=OneLoop_HelAmpmmpp_HE_2_0(y)
      res=res+(-1d0/xs)**2*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_2_1(x)
      fyx=OneLoop_HelAmpmmpp_HE_2_1(y)
      res=res+(-1d0/xs)**2*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_2_2(x)
      fyx=OneLoop_HelAmpmmpp_HE_2_2(y)
      res=res+(-1d0/xs)**2*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.2)return
      fxy=OneLoop_HelAmpmmpp_HE_3_0(x)
      fyx=OneLoop_HelAmpmmpp_HE_3_0(y)
      res=res+(-1d0/xs)**3*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_3_1(x)
      fyx=OneLoop_HelAmpmmpp_HE_3_1(y)
      res=res+(-1d0/xs)**3*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_3_2(x)
      fyx=OneLoop_HelAmpmmpp_HE_3_2(y)
      res=res+(-1d0/xs)**3*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.3)return
      fxy=OneLoop_HelAmpmmpp_HE_4_0(x)
      fyx=OneLoop_HelAmpmmpp_HE_4_0(y)
      res=res+(-1d0/xs)**4*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_4_1(x)
      fyx=OneLoop_HelAmpmmpp_HE_4_1(y)
      res=res+(-1d0/xs)**4*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_4_2(x)
      fyx=OneLoop_HelAmpmmpp_HE_4_2(y)
      res=res+(-1d0/xs)**4*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.4)return
      fxy=OneLoop_HelAmpmmpp_HE_5_0(x)
      fyx=OneLoop_HelAmpmmpp_HE_5_0(y)
      res=res+(-1d0/xs)**5*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_5_1(x)
      fyx=OneLoop_HelAmpmmpp_HE_5_1(y)
      res=res+(-1d0/xs)**5*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_5_2(x)
      fyx=OneLoop_HelAmpmmpp_HE_5_2(y)
      res=res+(-1d0/xs)**5*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.5)return
      fxy=OneLoop_HelAmpmmpp_HE_6_0(x)
      fyx=OneLoop_HelAmpmmpp_HE_6_0(y)
      res=res+(-1d0/xs)**6*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_6_1(x)
      fyx=OneLoop_HelAmpmmpp_HE_6_1(y)
      res=res+(-1d0/xs)**6*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_6_2(x)
      fyx=OneLoop_HelAmpmmpp_HE_6_2(y)
      res=res+(-1d0/xs)**6*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.6)return
      fxy=OneLoop_HelAmpmmpp_HE_7_0(x)
      fyx=OneLoop_HelAmpmmpp_HE_7_0(y)
      res=res+(-1d0/xs)**7*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_7_1(x)
      fyx=OneLoop_HelAmpmmpp_HE_7_1(y)
      res=res+(-1d0/xs)**7*logm2oms*(fxy+fyx)
      fxy=OneLoop_HelAmpmmpp_HE_7_2(x)
      fyx=OneLoop_HelAmpmmpp_HE_7_2(y)
      res=res+(-1d0/xs)**7*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.7)return
      WRITE(*,*)"ERROR: cannot reach here (--++)"
      STOP
      return
      end

      ! +-+- (4 -> 0)
      ! +--+,-++- (2 -> 2)
      function OneLoop_HelAmp_HE_pmpm(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 7
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_2_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoop_HelAmppmpm_HE_7_0
      IF(iterm.LT.0.OR.iterm.GT.7)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>7"
         STOP
      ENDIF
      x=xt/xs
      fxy=OneLoop_HelAmppmpm_HE_0_0(x)
      res=fxy
      IF(iterm.LE.0)return
      logm2oms=-DLOG(xs)+IPi
      fxy=OneLoop_HelAmppmpm_HE_1_0(x)
      res=res+(-1d0/xs)*fxy
      fxy=OneLoop_HelAmppmpm_HE_1_1(x)
      res=res+(-1d0/xs)*logm2oms*fxy
      fxy=OneLoop_HelAmppmpm_HE_1_2(x)
      res=res+(-1d0/xs)*logm2oms**2*fxy
      IF(iterm.LE.1)return
      fxy=OneLoop_HelAmppmpm_HE_2_0(x)
      res=res+(-1d0/xs)**2*fxy
      fxy=OneLoop_HelAmppmpm_HE_2_1(x)
      res=res+(-1d0/xs)**2*logm2oms*fxy
      fxy=OneLoop_HelAmppmpm_HE_2_2(x)
      res=res+(-1d0/xs)**2*logm2oms**2*fxy
      IF(iterm.LE.2)return
      fxy=OneLoop_HelAmppmpm_HE_3_0(x)
      res=res+(-1d0/xs)**3*fxy
      fxy=OneLoop_HelAmppmpm_HE_3_1(x)
      res=res+(-1d0/xs)**3*logm2oms*fxy
      fxy=OneLoop_HelAmppmpm_HE_3_2(x)
      res=res+(-1d0/xs)**3*logm2oms**2*fxy
      IF(iterm.LE.3)return
      fxy=OneLoop_HelAmppmpm_HE_4_0(x)
      res=res+(-1d0/xs)**4*fxy
      fxy=OneLoop_HelAmppmpm_HE_4_1(x)
      res=res+(-1d0/xs)**4*logm2oms*fxy
      fxy=OneLoop_HelAmppmpm_HE_4_2(x)
      res=res+(-1d0/xs)**4*logm2oms**2*fxy
      IF(iterm.LE.4)return
      fxy=OneLoop_HelAmppmpm_HE_5_0(x)
      res=res+(-1d0/xs)**5*fxy
      fxy=OneLoop_HelAmppmpm_HE_5_1(x)
      res=res+(-1d0/xs)**5*logm2oms*fxy
      fxy=OneLoop_HelAmppmpm_HE_5_2(x)
      res=res+(-1d0/xs)**5*logm2oms**2*fxy
      IF(iterm.LE.5)return
      fxy=OneLoop_HelAmppmpm_HE_6_0(x)
      res=res+(-1d0/xs)**6*fxy
      fxy=OneLoop_HelAmppmpm_HE_6_1(x)
      res=res+(-1d0/xs)**6*logm2oms*fxy
      fxy=OneLoop_HelAmppmpm_HE_6_2(x)
      res=res+(-1d0/xs)**6*logm2oms**2*fxy
      IF(iterm.LE.6)return
      fxy=OneLoop_HelAmppmpm_HE_7_0(x)
      res=res+(-1d0/xs)**7*fxy
      fxy=OneLoop_HelAmppmpm_HE_7_1(x)
      res=res+(-1d0/xs)**7*logm2oms*fxy
      fxy=OneLoop_HelAmppmpm_HE_7_2(x)
      res=res+(-1d0/xs)**7*logm2oms**2*fxy
      IF(iterm.LE.7)return
      WRITE(*,*)"ERROR: cannot reach here (+-+-)"
      STOP
      return
      end

      ! +--+ (4 -> 0)
      ! +-+-,-+-+ (2 -> 2)
      function OneLoop_HelAmp_HE_pmmp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm  ! iterm from 0 to 7
                     ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double complex OneLoop_HelAmp_HE_pmpm
      external OneLoop_HelAmp_HE_pmpm
      res=OneLoop_HelAmp_HE_pmpm(iterm,xs,xu,xt)
      return
      end
      

      ! ++++: (-m2/s)^0*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_0_0(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      !y=-1d0-x
      res=dcmplx(0.5d0,0d0)
      return
      end


      ! ++++: (-m2/s)^2*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_2_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(-4d0*lins(1))/y
      return
      end


      ! ++++: (-m2/s)^2*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_2_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(-2d0*lins(1)*lins(2))/(x*y)
      return
      end


      ! ++++: (-m2/s)^3*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_3_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx(-12d0/(x*y),0d0)
      return
      end


      ! ++++: (-m2/s)^3*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_3_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=-36d0/(x*y) + (8d0*(1d0+y**3)*lins(1))/(x**2*y**2)
      return
      end


      ! ++++: (-m2/s)^3*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_3_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res= (24d0 + 36d0*zeta2)/(x*y) + 
     -  lins(1)*((-8d0*(-1d0+y)**2)/(x*y**2)- 
     -     (4d0*lins(2))/(x**2*y**2))
      return
      end


      ! ++++: (-m2/s)^4*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_4_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-12d0*(1d0+2d0*x**5))/(x**3*y**3),0d0)
      return
      end


      ! ++++: (-m2/s)^4*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_4_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(206d0*(-1d0-x-x**2))/(x**2*y**2) + 
     -  (24d0*(1d0+y**5)*lins(1))/(x**3*y**3)
      return
      end


      ! ++++: (-m2/s)^4*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_4_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(24d0*(-4d0*x*y+4d0*y**3 + 
     -       3d0*(-1d0+x**4)*zeta2))/(x**2*y**3) + 
     -  lins(1)*((-4d0*
     -        (7d0-13d0*y+16d0*y**2-13d0*y**3 + 
     -          7d0*y**4))/(x**2*y**3) - 
     -     (12d0*lins(2))/(x**3*y**3))
      return
      end


      ! ++++: (-m2/s)^5*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_5_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-40d0*(1d0+2d0*x**7))/(x**4*y**4),0d0)
      return
      end


      ! ++++: (-m2/s)^5*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_5_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(1044d0*(-1d0-2d0*x-3d0*x**2-2d0*x**3- 
     -       x**4))/(x**3*y**3) + 
     -  (80d0*(1d0+y**7)*lins(1))/(x**4*y**4)
      return
      end


      ! ++++: (-m2/s)^5*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_5_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.99507389162561576354679802956d0)
      double precision rat2
      parameter (rat2=1.3300492610837438423645320197d0)
      double precision rat3
      parameter (rat3=0.665024630541871921182266009852d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(3248d0*(-1d0-5d0*x**3-3d0*x**4-x**5+ 
     -       rat3*x**6*zeta2 + 
     -       x*(-3d0+rat1*zeta2) + 
     -       x**2*(-5d0 + rat2*zeta2)))/
     -   (9d0*x**3*y**4) - 
     -  (8d0*(37d0-67d0*y+82d0*y**2-92d0*y**3 + 
     -       82d0*y**4-67d0*y**5+37d0*y**6)*lins(1))
     -    /(3d0*x**3*y**4) - 
     -  (40d0*lins(1)*lins(2))/(x**4*y**4)
      return
      end


      ! ++++: (-m2/s)^6*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_6_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((140d0*(-1d0-2d0*x**9))/(x**5*y**5),0d0)
      return
      end


      ! ++++: (-m2/s)^6*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_6_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.3933161953727506426735218509d0)
      double precision rat2
      parameter (rat2=0.415809768637532133676092544987d0)
      double precision rat3
      parameter (rat3=-0.370822622107969151670951156812d0)
      double precision rat4
      parameter (rat4=0.460796915167095115681233933162d0)
      double precision rat5
      parameter (rat5=-0.145886889460154241645244215938d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(3112d0*(1d0+rat1*x+rat2*x**2+ 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat2*x**6))/(x**3*y**4) + 
     -  ((-2520d0*x-10080d0*x**2-23520d0*x**3 - 
     -       35280d0*x**4 - 35280d0*x**5 - 
     -       23520d0*x**6 - 10080d0*x**7 - 
     -       2520d0*x**8 - 280d0*x**9)*lins(1))/
     -   (x**5*y**5)
      return
      end


      ! ++++: (-m2/s)^6*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_6_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=0.418253968253968253968253968254d0)
      double precision rat2
      parameter (rat2=0.341975308641975308641975308642d0)
      double precision rat3
      parameter (rat3=-0.0283068783068783068783068783069d0)
      double precision rat4
      parameter (rat4=-0.0566137566137566137566137566138d0)
      double precision rat5
      parameter (rat5=-0.113227513227513227513227513228d0)
      double precision rat6
      parameter (rat6=-0.359259259259259259259259259259d0)
      double precision rat7
      parameter (rat7=-0.302645502645502645502645502646d0)
      double precision rat8
      parameter (rat8=0.0479717813051146384479717813051d0)
      double precision rat9
      parameter (rat9=-8.47637457044673539518900343643d0)
      double precision rat10
      parameter (rat10=-11.4291237113402061855670103093d0)
      double precision rat11
      parameter (rat11=-10.3128221649484536082474226804d0)
      double precision rat12
      parameter (rat12=-6.24377147766323024054982817869d0)
      double precision rat13
      parameter (rat13=-2.44394329896907216494845360825d0)
      double precision rat14
      parameter (rat14=-0.560244845360824742268041237113d0)
      double precision rat15
      parameter (rat15=-0.0572379725085910652920962199313d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(2520d0*(rat1 + rat3*x**2 + rat4*x**3 + 
     -       rat5*x**4 + rat6*x**5 + rat7*x**6 + 
     -       x**7*(rat8 + zeta2/3d0) + 
     -       x*(rat2 + (2d0*zeta2)/3d0) + zeta2))/
     -   (x**3*y**5) + 
     -  (6208d0*(-1d0-4d0*x+rat9*x**2 + 
     -       rat10*x**3 + rat11*x**4 + 
     -       rat12*x**5 + rat13*x**6 + 
     -       rat14*x**7 + rat15*x**8)*lins(1))/
     -   (x**4*y**5) - 
     -  (140d0*lins(1)*lins(2))/(x**5*y**5)
      return
      end


      ! ++++: (-m2/s)^7*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_7_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((504d0*(-1d0-2d0*x**11))/(x**6*y**6),0d0)
      return
      end


      ! ++++: (-m2/s)^7*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_7_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.467242096505823627287853577371d0)
      double precision rat2
      parameter (rat2=-0.127516638935108153078202995008d0)
      double precision rat3
      parameter (rat3=0.127516638935108153078202995008d0)
      double precision rat4
      parameter (rat4=-0.0882071547420965058236272878536d0)
      double precision rat5
      parameter (rat5=0.166826123128119800332778702163d0)
      double precision rat6
      parameter (rat6=-0.0226913477537437603993344425957d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(38464d0*(-1d0+rat1*x+rat2*x**2+ 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat3*x**7))/(x**3*y**5)
     -   + ((-11088d0*x - 55440d0*x**2 - 
     -       166320d0*x**3 - 332640d0*x**4 - 
     -       465696d0*x**5 - 465696d0*x**6 - 
     -       332640d0*x**7 - 166320d0*x**8 - 
     -       55440d0*x**9 - 11088d0*x**10 - 1008d0*x**11
     -       )*lins(1))/(x**6*y**6)
      return
      end


      ! ++++: (-m2/s)^7*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_7_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.374338624338624338624338624339d0)
      double precision rat2
      parameter (rat2=-0.599338624338624338624338624339d0)
      double precision rat3
      parameter (rat3=-0.228989417989417989417989417989d0)
      double precision rat4
      parameter (rat4=-0.0537301587301587301587301587302d0)
      double precision rat5
      parameter (rat5=-0.10746031746031746031746031746d0)
      double precision rat6
      parameter (rat6=-0.183121693121693121693121693122d0)
      double precision rat7
      parameter (rat7=-0.129391534391534391534391534392d0)
      double precision rat8
      parameter (rat8=0.00398941798941798941798941798942d0)
      double precision rat9
      parameter (rat9=-13.7494586401039411000433087917d0)
      double precision rat10
      parameter (rat10=-24.9978345604157644001732351667d0)
      double precision rat11
      parameter (rat11=-31.9393947596362061498484192291d0)
      double precision rat12
      parameter (rat12=-29.3257633174534430489389346037d0)
      double precision rat13
      parameter (rat13=-19.3391349068860978778692074491d0)
      double precision rat14
      parameter (rat14=-8.96613793850151580770896491988d0)
      double precision rat15
      parameter (rat15=-2.78028908618449545257687310524d0)
      double precision rat16
      parameter (rat16=-0.518541576440017323516673884799d0)
      double precision rat17
      parameter (rat17=-0.0440396275443915114768297964487d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(30240d0*(rat1 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + 
     -       x*(rat2 - (3d0*zeta2)/2d0) + 
     -       x**2*(rat3 - (3d0*zeta2)/5d0) + 
     -       x**8*(rat8 + zeta2/10d0) - zeta2))/
     -   (x**3*y**6) + 
     -  (147776d0*(-1d0-5d0*x+rat9*x**2 + 
     -       rat10*x**3 + rat11*x**4 + 
     -       rat12*x**5 + rat13*x**6 + 
     -       rat14*x**7 + rat15*x**8 + 
     -       rat16*x**9 + rat17*x**10)*lins(1))/
     -   (5d0*x**5*y**6) - 
     -  (504d0*lins(1)*lins(2))/(x**6*y**6)
      return
      end


      ! ++++: (-m2/s)^8*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_8_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((1848d0*(-1d0-2d0*x**13))/(x**7*y**7),0d0)
      return
      end


      ! ++++: (-m2/s)^8*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_8_1(x) RESULT(res)      
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-16.3789222916071523209625360156d0)
      double precision rat2
      parameter (rat2=-35.5156891664286092838501440624d0)
      double precision rat3
      parameter (rat3=-56.0313783328572185677002881248d0)
      double precision rat4
      parameter (rat4=-64.789222916071523209625360156d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(1547246d0*(-1d0-5d0*x+rat1*x**2+ 
     -       rat2*x**3 + rat3*x**4 + rat4*x**5 + 
     -       rat3*x**6 + rat2*x**7 + rat1*x**8 - 
     -       5d0*x**9 - x**10))/(15d0*x**6*y**6) + 
     -  ((-48048d0*x - 288288d0*x**2 - 1057056d0*x**3 - 
     -       2642640d0*x**4 - 4756752d0*x**5 - 
     -       6342336d0*x**6 - 6342336d0*x**7 - 
     -       4756752d0*x**8 - 2642640d0*x**9 - 
     -       1057056d0*x**10 - 288288d0*x**11 - 
     -       48048d0*x**12 - 3696d0*x**13)*lins(1))/
     -   (x**7*y**7)
      return
      end


      ! ++++: (-m2/s)^8*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_8_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-25.9786765738411000025322341902d0)
      double precision rat2
      parameter (rat2=-7.17888392929081328456096814219d0)
      double precision rat3
      parameter (rat3=-74.8933828692055000126611709511d0)
      double precision rat4
      parameter (rat4=-10.7683258939362199268414522133d0)
      double precision rat5
      parameter (rat5=-146.744118886093200030386810283d0)
      double precision rat6
      parameter (rat6=-4.30733035757448797073658088532d0)
      double precision rat7
      parameter (rat7=-203.616178329139800045580215424d0)
      double precision rat8
      parameter (rat8=0.717888392929081328456096814219d0)
      double precision rat9
      parameter (rat9=-20.2941243300843075187262983044d0)
      double precision rat10
      parameter (rat10=-46.4706216504215375936314915222d0)
      double precision rat11
      parameter (rat11=-76.9236158993324348888958535745d0)
      double precision rat12
      parameter (rat12=-94.8707336948005139937944651644d0)
      double precision rat13
      parameter (rat13=-88.2237009120255743253831447645d0)
      double precision rat14
      parameter (rat14=-61.8001457360453818911210706115d0)
      double precision rat15
      parameter (rat15=-32.1765600025072868022690945561d0)
      double precision rat16
      parameter (rat16=-12.0941564546964615915002977403d0)
      double precision rat17
      parameter (rat17=-3.1058936910395837903908233303d0)
      double precision rat18
      parameter (rat18=-0.488237299025292255617889491334d0)
      double precision rat19
      parameter (rat19=-0.0354683141630363243175478735074d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(3475192d0*(-1d0-6d0*x + rat7*x**5 + 
     -       rat7*x**6 + rat5*x**7 + rat3*x**8 + 
     -       rat1*x**9 - 6*x**10 - x**11 + 
     -       rat8*x**12*zeta2 + 
     -       x**2*(rat1 + rat2*zeta2) + 
     -       x**3*(rat3 + rat4*zeta2) + 
     -       x**4*(rat5 + rat6*zeta2)))/
     -   (225d0*x**6*y**7) + 
     -  (2042048d0*(-1d0-6d0*x+rat9*x**2 + 
     -       rat10*x**3 + rat11*x**4 + 
     -       rat12*x**5 + rat13*x**6 + 
     -       rat14*x**7 + rat15*x**8 + 
     -       rat16*x**9 + rat17*x**10 + 
     -       rat18*x**11 + rat19*x**12)*lins(1))/
     -   (15d0*x**6*y**7) - 
     -  (1848d0*lins(1)*lins(2))/(x**7*y**7)
      return
      end


      ! ++++: (-m2/s)^9*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_9_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((6864d0*(-1d0-2d0*x**15))/(x**8*y**8),0d0)
      return
      end


      ! ++++: (-m2/s)^9*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_9_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.37950750569733536726665804188d0)
      double precision rat2
      parameter (rat2=0.603415415717110772122223659687d0)
      double precision rat3
      parameter (rat3=-0.0382600568301713739793691570706d0)
      double precision rat4
      parameter (rat4=0.0382600568301713739793691570706d0)
      double precision rat5
      parameter (rat5=-0.0285017262098548822015872722513d0)
      double precision rat6
      parameter (rat6=0.0577767180708043575349329267091d0)
      double precision rat7
      parameter (rat7=0.00809201361633196196509479582103d0)
      double precision rat8
      parameter (rat8=0.0748537966563582181460512251429d0)
      double precision rat9
      parameter (rat9=0.00565243096125283902064932461621d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(5627192d0*(1d0+rat1*x+rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat4*x**10))/
     -   (3d0*x**4*y**7) + 
     -  ((-205920d0*x - 1441440d0*x**2 - 
     -       6246240d0*x**3 - 18738720d0*x**4 - 
     -       41225184d0*x**5 - 68708640d0*x**6 - 
     -       88339680d0*x**7 - 88339680d0*x**8 - 
     -       68708640d0*x**9 - 41225184d0*x**10 - 
     -       18738720d0*x**11 - 6246240d0*x**12 - 
     -       1441440d0*x**13 - 205920d0*x**14 - 
     -       13728d0*x**15)*lins(1))/(x**8*y**8)
      return
      end


      ! ++++: (-m2/s)^9*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_9_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=0.332588090921424254757588090921d0)
      double precision rat2
      parameter (rat2=0.823596329596329596329596329596d0)
      double precision rat3
      parameter (rat3=0.685589533922867256200589533923d0)
      double precision rat4
      parameter (rat4=0.198002634805355893791267940928d0)
      double precision rat5
      parameter (rat5=-0.0148393757917567441376965186489d0)
      double precision rat6
      parameter (rat6=-0.0445181273752702324130895559467d0)
      double precision rat7
      parameter (rat7=-0.099054675483246911818340389769d0)
      double precision rat8
      parameter (rat8=-0.123912472007710102948198186293d0)
      double precision rat9
      parameter (rat9=-0.114647072504215361358218501076d0)
      double precision rat10
      parameter (rat10=-0.0601105243962386819529676672534d0)
      double precision rat11
      parameter (rat11=-0.00342133955739397916268664567984d0)
      double precision rat12
      parameter (rat12=28.1125549357674104124408384043d0)
      double precision rat13
      parameter (rat13=77.675329614604462474645030426d0)
      double precision rat14
      parameter (rat14=158.023499209521536809449946307d0)
      double precision rat15
      parameter (rat15=244.926974580400111363003619298d0)
      double precision rat16
      parameter (rat16=294.375240499343753728671996182d0)
      double precision rat17
      parameter (rat17=276.367740275623433957761603627d0)
      double precision rat18
      parameter (rat18=202.498909984886449508809609036d0)
      double precision rat19
      parameter (rat19=114.774306467008710177783080778d0)
      double precision rat20
      parameter (rat20=49.3911973511514139124209521537d0)
      double precision rat21
      parameter (rat21=15.6146112983733046971324026568d0)
      double precision rat22
      parameter (rat22=3.42249395955136618541940102613d0)
      double precision rat23
      parameter (rat23=0.464933704211907886887006323828d0)
      double precision rat24
      parameter (rat24=0.0295026200135226504394861392833d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(1441440d0*(rat1 + rat5*x**5 + rat6*x**6 + 
     -       rat7*x**7 + rat8*x**8 + rat9*x**9 + 
     -       rat10*x**10 + 
     -       x**11*(rat11 + zeta2/35d0) + 
     -       x**3*(rat4 + (4d0*zeta2)/7d0) + zeta2 + 
     -       x**2*(rat3 + 2d0*zeta2) + 
     -       x*(rat2 + (12d0*zeta2)/5d0)))/
     -   (x**4*y**8) - 
     -  (4291072d0*(1d0 + 7d0*x + rat12*x**2 + 
     -       rat13*x**3 + rat14*x**4 + 
     -       rat15*x**5 + rat16*x**6 + 
     -       rat17*x**7 + rat18*x**8 + 
     -       rat19*x**9 + rat20*x**10 + 
     -       rat21*x**11 + rat22*x**12 + 
     -       rat23*x**13 + rat24*x**14)*lins(1))/
     -   (7d0*x**7*y**8) - 
     -  (6864d0*lins(1)*lins(2))/(x**8*y**8)
      return
      end


      ! ++++: (-m2/s)^10*(log(-m2/s))^2
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_10_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((25740d0*(-1d0-2d0*x**17))/(x**9*y**9),0d0)
      return
      end


      ! ++++: (-m2/s)^10*(log(-m2/s))^1
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_10_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.37959462175928154223798913367d0)
      double precision rat2
      parameter (rat2=0.603400896373453076293668477722d0)
      double precision rat3
      parameter (rat3=-0.0382188693042852572412228245592d0)
      double precision rat4
      parameter (rat4=0.0391298236900316169627411668063d0)
      double precision rat5
      parameter (rat5=-0.0373079149185388975197044823122d0)
      double precision rat6
      parameter (rat6=0.0445955500045097752918512202886d0)
      double precision rat7
      parameter (rat7=-0.009371980422317199393141986736d0)
      double precision rat8
      parameter (rat8=0.0895359663679968548867561044763d0)
      double precision rat9
      parameter (rat9=0.0398195564079862255688484946047d0)
      double precision rat10
      parameter (rat10=0.0937870868348132002538417016292d0)
      double precision rat11
      parameter (rat11=0.0118836219117645274422859990285d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(7064020d0*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10 + 
     -       rat11*x**11 + rat4*x**12))/
     -   (x**5*y**8) + 
     -  ((-875160d0*x-7001280d0*x**2 - 
     -       35006400d0*x**3 - 122522400d0*x**4 - 
     -       318558240d0*x**5 - 637116480d0*x**6 - 
     -       1001183040d0*x**7 - 1251478800d0*x**8 - 
     -       1251478800d0*x**9 - 1001183040d0*x**10 - 
     -       637116480d0*x**11 - 318558240d0*x**12 - 
     -       122522400d0*x**13 - 35006400d0*x**14 - 
     -       7001280d0*x**15 - 875160d0*x**16 - 
     -       51480d0*x**17)*lins(1))/(x**9*y**9)
      return
      end


      ! ++++: (-m2/s)^10*(log(-m2/s))^0
      ! ++++: F(x)+F(-1-x)
      function OneLoop_HelAmppppp_HE_10_0(x) RESULT(res)      
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=0.322438456605123271789938456605d0)
      double precision rat2
      parameter (rat2=0.799459429459429459429459429459d0)
      double precision rat3
      parameter (rat3=0.665475450475450475450475450475d0)
      double precision rat4
      parameter (rat4=0.192270870978353971551250462815d0)
      double precision rat5
      parameter (rat5=-0.00142945678659964374250088535803d0)
      double precision rat6
      parameter (rat6=-0.00285891357319928748500177071606d0)
      double precision rat7
      parameter (rat7=-0.0114356542927971499400070828642d0)
      double precision rat8
      parameter (rat8=-0.0549699859223668747478271287795d0)
      double precision rat9
      parameter (rat9=-0.124885167742310599453456596314d0)
      double precision rat10
      parameter (rat10=-0.199830936259507688079116650545d0)
      double precision rat11
      parameter (rat11=-0.204861522956761051999147237242d0)
      double precision rat12
      parameter (rat12=-0.155544878402021259164116306973d0)
      double precision rat13
      parameter (rat13=-0.0720223691652263080834509405938d0)
      double precision rat14
      parameter (rat14=-0.00524585014380932748279687055197d0)
      double precision rat15
      parameter (rat15=-37.2064857593125138904790447391d0)
      double precision rat16
      parameter (rat16=-120.445400315187597233353313174d0)
      double precision rat17
      parameter (rat17=-290.820671179528969646356821993d0)
      double precision rat18
      parameter (rat18=-543.133822979735053844547860698d0)
      double precision rat19
      parameter (rat19=-801.033196140298889435154192736d0)
      double precision rat20
      parameter (rat20=-943.721310899227520995669537927d0)
      double precision rat21
      parameter (rat21=-892.511359938932403036037795573d0)
      double precision rat22
      parameter (rat22=-677.134699974071105783153627015d0)
      double precision rat23
      parameter (rat23=-409.571609418249900662028649744d0)
      double precision rat24
      parameter (rat24=-195.008504866718748947690981459d0)
      double precision rat25
      parameter (rat25=-71.5320403893038260272219715388d0)
      double precision rat26
      parameter (rat26=-19.516787064849174652990578046d0)
      double precision rat27
      parameter (rat27=-3.73144884431214347770451836237d0)
      double precision rat28
      parameter (rat28=-0.446285570149444717577096367934d0)
      double precision rat29
      parameter (rat29=-0.0251367686031397533724399426197d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (5405400*(rat1 + rat5*x**4 + rat6*x**5 + 
     -       rat7*x**6 + rat8*x**7 + rat9*x**8 + 
     -       rat10*x**9 + rat11*x**10 + 
     -       rat12*x**11 + rat13*x**12 + 
     -       x**13*(rat14 + zeta2/35d0) + 
     -       x**3*(rat4 + (4*zeta2)/7d0) + zeta2 + 
     -       x**2*(rat3 + 2*zeta2) + 
     -       x*(rat2 + (12*zeta2)/5d0)))/
     -   (x**5*y**9) + 
     -  (19005824*(-1 - 8*x + rat15*x**2 + 
     -       rat16*x**3 + rat17*x**4 + 
     -       rat18*x**5 + rat19*x**6 + 
     -       rat20*x**7 + rat21*x**8 + 
     -       rat22*x**9 + rat23*x**10 + 
     -       rat24*x**11 + rat25*x**12 + 
     -       rat26*x**13 + rat27*x**14 + 
     -       rat28*x**15 + rat29*x**16)*lins(1))/
     -   (7d0*x**8*y**9) - 
     -  (25740*lins(1)*lins(2))/(x**9*y**9)
      return
      end


      ! -+++: (-m2/s)^0*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_0_0(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx(0.5d0,0d0)
      return
      end


      ! -+++: (-m2/s)^1*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_1_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx(-(1d0-x)/(2d0*y),0d0)
      return
      end


      ! -+++: (-m2/s)^1*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_1_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(2d0*lins(1))/x
      return
      end


      ! -+++: (-m2/s)^1*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_1_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(-3d0*(1d0-x)*zeta2)/y + 
     -  ((-x + y**2)*lins(1)**2)/(x*y) + 
     -  lins(1)*lins(2)
      return
      end


      ! -+++: (-m2/s)^2*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_2_1(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((2d0*(-x+y**2)**2)/(x**2*y**2),0d0)
      return
      end


      ! -+++: (-m2/s)^2*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_2_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(4d0*(-x+y**2)*lins(1))/(x**2*y)
      return
      end


      ! -+++: (-m2/s)^3*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_3_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx(6d0/(x*y),0d0)
      return
      end


      ! -+++: (-m2/s)^3*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_3_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (-6 + (y**2*(-8 - 61*y + 3*y**2))/x)/
     -   y**3 - (4*(1 + y**3)*lins(1))/(x**2*y**2)
      return
      end


      ! -+++: (-m2/s)^3*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_3_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2*(-2 + (y**2*(6 + 19*y + y**2))/x))/
     -   y**3 - (18*zeta2)/(x*y) + 
     -  lins(1)*((2*
     -        (3 + y - y**2 + y**3 + 3*y**4))/
     -      (x**3*y**2) + (2*lins(2))/(x**2*y**2))
      return
      end


      ! -+++: (-m2/s)^4*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_4_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((8 + 16*x**5)/(x**3*y**3),0d0)
      return
      end


      ! -+++: (-m2/s)^4*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_4_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-4.31666666666666666666666666667d0)
      double precision rat2
      parameter (rat2=14.3166666666666666666666666667d0)
      double precision rat3
      parameter (rat3=13.35d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (80*(-1 + rat1*x + (6*x**2)/5d0 + 
     -       rat2*x**3 + rat3*x**4 + 
     -       (52*x**5)/15d0 + x**6/12d0))/
     -   (x**2*y**4) - 
     -  (16*(1 + y**5)*lins(1))/(x**3*y**3)
      return
      end


      ! -+++: (-m2/s)^4*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_4_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=4.17777777777777777777777777778d0)
      double precision rat2
      parameter (rat2=1.78333333333333333333333333333d0)
      double precision rat3
      parameter (rat3=-7.18888888888888888888888888889d0)
      double precision rat4
      parameter (rat4=-6.96666666666666666666666666667d0)
      double precision rat5
      parameter (rat5=-1.32222222222222222222222222222d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (40*(1 + rat1*x + rat2*x**2 + rat3*x**3 + 
     -       rat4*x**4 + rat5*x**5 + (3*x**6)/20d0)
     -     )/(x**2*y**4) + 
     -  (120*(-zeta2 - x*zeta2 - x**2*zeta2))/
     -   (x**2*y**2) + 
     -  (8*(8 + 2*y - 3*y**2 + y**3 - 3*y**4 + 
     -       2*y**5 + 8*y**6)*lins(1))/
     -   (3d0*x**4*y**3) + 
     -  (8*lins(1)*lins(2))/(x**3*y**3)
      return
      end


      ! -+++: (-m2/s)^5*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_5_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((30d0+60d0*x**7)/(x**4*y**4),0d0)
      return
      end


      ! -+++: (-m2/s)^5*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_5_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.713450292397660818713450292398d0)
      double precision rat2
      parameter (rat2=4.73464912280701754385964912281d0)
      double precision rat3
      parameter (rat3=12.4788011695906432748538011696d0)
      double precision rat4
      parameter (rat4=14.2207602339181286549707602339d0)
      double precision rat5
      parameter (rat5=7.74122807017543859649122807018d0)
      double precision rat6
      parameter (rat6=1.52558479532163742690058479532d0)
      double precision rat7
      parameter (rat7=0.0255847953216374269005847953216d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(684*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7))/(x**2*y**5)
     -   - (60*(1 + y**7)*lins(1))/(x**4*y**4)
      return
      end


      ! -+++: (-m2/s)^5*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_5_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.36460078168620882188721384701d0)
      double precision rat2
      parameter (rat2=-2.13986599664991624790619765494d0)
      double precision rat3
      parameter (rat3=-6.85622557230597431602456728085d0)
      double precision rat4
      parameter (rat4=-7.9168062534896705750977107761d0)
      double precision rat5
      parameter (rat5=-3.84087102177554438860971524288d0)
      double precision rat6
      parameter (rat6=-0.365438302624232272473478503629d0)
      double precision rat7
      parameter (rat7=0.0896147403685092127303182579564d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res= (199*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7))/(x**2*y**5)
     -   + (630*(-zeta2 - 2*x*zeta2 - 
     -       3*x**2*zeta2 - 2*x**3*zeta2 - 
     -       x**4*zeta2))/(x**3*y**3) + 
     -  ((79 + 19*y - 30*y**2 + 10*y**3 - 
     -       5*y**4 + 10*y**5 - 30*y**6 + 
     -       19*y**7 + 79*y**8)*lins(1))/
     -   (x**5*y**4) + 
     -  (30*lins(1)*lins(2))/(x**4*y**4)
      return
      end


      ! -+++: (-m2/s)^6*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_6_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((112d0*(1d0+2d0*x**9))/(x**5*y**5),0d0)
      return
      end


      ! -+++: (-m2/s)^6*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_6_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=7.7866323907455012853470437018d0)
      double precision rat2
      parameter (rat2=16.6322622107969151670951156812d0)
      double precision rat3
      parameter (rat3=18.0353470437017994858611825193d0)
      double precision rat4
      parameter (rat4=18.7455012853470437017994858612d0)
      double precision rat5
      parameter (rat5=22.5886889460154241645244215938d0)
      double precision rat6
      parameter (rat6=18.747429305912596401028277635d0)
      double precision rat7
      parameter (rat7=7.99935732647814910025706940874d0)
      double precision rat8
      parameter (rat8=1.26863753213367609254498714653d0)
      double precision rat9
      parameter (rat9=0.0161953727506426735218508997429d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (3112*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9))/(x**3*y**6) + 
     -  ((2016*x + 8064*x**2 + 18816*x**3 + 
     -       28224*x**4 + 28224*x**5 + 
     -       18816*x**6 + 8064*x**7 + 2016*x**8 + 
     -       224*x**9)*lins(1))/(x**5*y**5)
      return
      end


      ! -+++: (-m2/s)^6*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_6_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-7.18764495045329959940965633565d0)
      double precision rat2
      parameter (rat2=-14.8792831541218637992831541219d0)
      double precision rat3
      parameter (rat3=-13.0075521821631878557874762808d0)
      double precision rat4
      parameter (rat4=-6.64888045540796963946869070209d0)
      double precision rat5
      parameter (rat5=-5.01663504111321948134092346616d0)
      double precision rat6
      parameter (rat6=-4.33130929791271347248576850095d0)
      double precision rat7
      parameter (rat7=-1.55865485979337971747838920514d0)
      double precision rat8
      parameter (rat8=0.0368037107316044697448872021927d0)
      double precision rat9
      parameter (rat9=0.0521821631878557874762808349146d0)
      double precision rat10
      parameter (rat10=60.2698412698412698412698412698d0)
      double precision rat11
      parameter (rat11=211.079365079365079365079365079d0)
      double precision rat12
      parameter (rat12=431.962962962962962962962962963d0)
      double precision rat13
      parameter (rat13=578.111111111111111111111111111d0)
      double precision rat14
      parameter (rat14=522.111111111111111111111111111d0)
      double precision rat15
      parameter (rat15=316.962962962962962962962962963d0)
      double precision rat16
      parameter (rat16=124.412698412698412698412698413d0)
      double precision rat17
      parameter (rat17=28.6031746031746031746031746032d0)
      double precision rat18
      parameter (rat18=2.93121693121693121693121693122d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (1054*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9))/(x**3*y**6) + 
     -  (2520*(-zeta2 - (17*x*zeta2)/3d0 - 
     -       (59*x**2*zeta2)/15d0 - 
     -       (49*x**3*zeta2)/15d0 - 
     -       (77*x**4*zeta2)/15d0 - 
     -       (67*x**5*zeta2)/15d0 - 
     -       (14*x**6*zeta2)/15d0))/(x**3*y**4) + 
     -  (504*(1 + 5*x + rat10*x**2 + rat11*x**3 + 
     -       rat12*x**4 + rat13*x**5 + 
     -       rat14*x**6 + rat15*x**7 + 
     -       rat16*x**8 + rat17*x**9 + rat18*x**10
     -       )*lins(1))/(5d0*x**6*y**5) + 
     -  (112*lins(1)*lins(2))/(x**5*y**5)
      return
      end


      ! -+++: (-m2/s)^7*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_7_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((420d0*(1d0+2d0*x**11))/(x**6*y**6),0d0)
      return
      end


      ! -+++: (-m2/s)^7*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_7_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=2.46780457890433360588716271464d0)
      double precision rat2
      parameter (rat2=2.34008994276369582992641046607d0)
      double precision rat3
      parameter (rat3=2.80961978740801308258381030253d0)
      double precision rat4
      parameter (rat4=7.48520850367947669664758789861d0)
      double precision rat5
      parameter (rat5=13.8550899427636958299264104661d0)
      double precision rat6
      parameter (rat6=15.2053556827473426001635322976d0)
      double precision rat7
      parameter (rat7=9.90830335241210139002452984464d0)
      double precision rat8
      parameter (rat8=3.47576860179885527391659852821d0)
      double precision rat9
      parameter (rat9=0.460883074407195421095666394113d0)
      double precision rat10
      parameter (rat10=0.00472199509403107113654946852003d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (97840*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10))/
     -   (3d0*x**3*y**7) + 
     -  ((9240*x + 46200*x**2 + 138600*x**3 + 
     -       277200*x**4 + 388080*x**5 + 
     -       388080*x**6 + 277200*x**7 + 
     -       138600*x**8 + 46200*x**9 + 
     -       9240*x**10 + 840*x**11)*lins(1))/
     -   (x**6*y**6)
      return
      end


      ! -+++: (-m2/s)^7*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_7_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-2.61684882560033629341600546477d0)
      double precision rat2
      parameter (rat2=-2.24172350375702800693605170511d0)
      double precision rat3
      parameter (rat3=-0.723637486206715359150859124586d0)
      double precision rat4
      parameter (rat4=-0.320684935105879880195470548053d0)
      double precision rat5
      parameter (rat5=-0.616576900845988124638747307025d0)
      double precision rat6
      parameter (rat6=-0.780519941148652199043665598234d0)
      double precision rat7
      parameter (rat7=-0.637426567179864431716672797015d0)
      double precision rat8
      parameter (rat8=-0.110955546214071777625978666386d0)
      double precision rat9
      parameter (rat9=0.119854185276653880510745625558d0)
      double precision rat10
      parameter (rat10=0.0207516683306184646103725500499d0)
      double precision rat11
      parameter (rat11=-3.18d0)
      double precision rat12
      parameter (rat12=-3.42d0)
      double precision rat13
      parameter (rat13=95.9653679653679653679653679654d0)
      double precision rat14
      parameter (rat14=424.826839826839826839826839827d0)
      double precision rat15
      parameter (rat15=1129.48051948051948051948051948d0)
      double precision rat16
      parameter (rat16=2034.96103896103896103896103896d0)
      double precision rat17
      parameter (rat17=2597.04545454545454545454545455d0)
      double precision rat18
      parameter (rat18=2387.04545454545454545454545455d0)
      double precision rat19
      parameter (rat19=1576.46103896103896103896103896d0)
      double precision rat20
      parameter (rat20=731.980519480519480519480519481d0)
      double precision rat21
      parameter (rat21=227.326839826839826839826839827d0)
      double precision rat22
      parameter (rat22=42.4653679653679653679653679654d0)
      double precision rat23
      parameter (rat23=3.61255411255411255411255411255d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (76124*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10))/
     -   (9d0*x**3*y**7) + 
     -  (25200*(-zeta2 - (x*zeta2)/2d0 - 
     -       (8*x**2*zeta2)/25d0 - 
     -       (61*x**3*zeta2)/50d0 + 
     -       rat11*x**4*zeta2 + 
     -       rat12*x**5*zeta2 - 
     -       (52*x**6*zeta2)/25d0 - 
     -       (17*x**7*zeta2)/50d0))/(x**3*y**5) + 
     -  (308*(1 + 6*x + rat13*x**2 + rat14*x**3 + 
     -       rat15*x**4 + rat16*x**5 + 
     -       rat17*x**6 + rat18*x**7 + 
     -       rat19*x**8 + rat20*x**9 + 
     -       rat21*x**10 + rat22*x**11 + 
     -       rat23*x**12)*lins(1))/(x**7*y**6) + 
     -  (420*lins(1)*lins(2))/(x**6*y**6)
      return
      end


      ! -+++: (-m2/s)^8*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_8_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((1584d0*(1d0+2d0*x**13))/(x**7*y**7),0d0)
      return
      end


      ! -+++: (-m2/s)^8*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_8_1(x) RESULT(res)      
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-9.06499962482179035041644781271d0)
      double precision rat2
      parameter (rat2=-24.358599084565168455016132663d0)
      double precision rat3
      parameter (rat3=-29.1260762254928233768182743946d0)
      double precision rat4
      parameter (rat4=-14.600886224528079409147916689d0)
      double precision rat5
      parameter (rat5=4.85566492298127324764977650098d0)
      double precision rat6
      parameter (rat6=18.890397313724018908981766339d0)
      double precision rat7
      parameter (rat7=25.7427496810985217978539806408d0)
      double precision rat8
      parameter (rat8=22.2645737975538380730847152397d0)
      double precision rat9
      parameter (rat9=11.9340455466346514594432355369d0)
      double precision rat10
      parameter (rat10=3.55409507015832520447212425902d0)
      double precision rat11
      parameter (rat11=0.404747612258680015864678579468d0)
      double precision rat12
      parameter (rat12=0.00344895968442152879760743496018d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (426464*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10 + 
     -       rat11*x**11 + rat12*x**12))/
     -   (3d0*x**4*y**8) + 
     -  ((41184*x + 247104*x**2 + 906048*x**3 + 
     -       2265120*x**4 + 4077216*x**5 + 
     -       5436288*x**6 + 5436288*x**7 + 
     -       4077216*x**8 + 2265120*x**9 + 
     -       906048*x**10 + 247104*x**11 + 
     -       41184*x**12 + 3168*x**13)*lins(1))/
     -   (x**7*y**7)
      return
      end


      ! -+++: (-m2/s)^8*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_8_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=8.66758050380890317071526482551d0)
      double precision rat2
      parameter (rat2=23.2861167164782813869351250534d0)
      double precision rat3
      parameter (rat3=28.1609747270068225937596449335d0)
      double precision rat4
      parameter (rat4=16.399315947497625597957211693d0)
      double precision rat5
      parameter (rat5=4.73344038588447221319034793531d0)
      double precision rat6
      parameter (rat6=2.50569252055331019848671137406d0)
      double precision rat7
      parameter (rat7=3.35851264979599174336371253294d0)
      double precision rat8
      parameter (rat8=2.77387275406331157887302693037d0)
      double precision rat9
      parameter (rat9=1.33641272908794986950534016892d0)
      double precision rat10
      parameter (rat10=0.469149778335639327527150272707d0)
      double precision rat11
      parameter (rat11=0.162668621526046880854316963297d0)
      double precision rat12
      parameter (rat12=0.0145580998179816183905979640907d0)
      double precision rat13
      parameter (rat13=141.0004662004662004662004662d0)
      double precision rat14
      parameter (rat14=755.002797202797202797202797203d0)
      double precision rat15
      parameter (rat15=2472.01025641025641025641025641d0)
      double precision rat16
      parameter (rat16=5606.02564102564102564102564103d0)
      double precision rat17
      parameter (rat17=9259.94615384615384615384615385d0)
      double precision rat18
      parameter (rat18=11422.6615384615384615384615385d0)
      double precision rat19
      parameter (rat19=10630.6615384615384615384615385d0)
      double precision rat20
      parameter (rat20=7453.24615384615384615384615385d0)
      double precision rat21
      parameter (rat21=3884.02564102564102564102564103d0)
      double precision rat22
      parameter (rat22=1461.21025641025641025641025641d0)
      double precision rat23
      parameter (rat23=375.602797202797202797202797203d0)
      double precision rat24
      parameter (rat24=59.1004662004662004662004662005d0)
      double precision rat25
      parameter (rat25=4.29766899766899766899766899767d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (356008*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10 + 
     -       rat11*x**11 + rat12*x**12))/
     -   (9d0*x**4*y**8) + 
     -  (61776*(-zeta2 - 5*x*zeta2 - 
     -       17*x**2*zeta2 - 38*x**3*zeta2 - 
     -       61*x**4*zeta2 - 71*x**5*zeta2 - 
     -       61*x**6*zeta2 - 38*x**7*zeta2 - 
     -       17*x**8*zeta2 - 5*x**9*zeta2 - 
     -       x**10*zeta2))/(x**6*y**6) + 
     -  (6864*(1 + 7*x + rat13*x**2 + 
     -       rat14*x**3 + rat15*x**4 + 
     -       rat16*x**5 + rat17*x**6 + 
     -       rat18*x**7 + rat19*x**8 + 
     -       rat20*x**9 + rat21*x**10 + 
     -       rat22*x**11 + rat23*x**12 + 
     -       rat24*x**13 + rat25*x**14)*lins(1))/
     -   (7d0*x**8*y**7) + 
     -  (1584*lins(1)*lins(2))/(x**7*y**7)
      return
      end


      ! -+++: (-m2/s)^9*(log(-m2/s))^2
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_9_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((6006d0*(1d0+2d0*x**15))/(x**8*y**8),0d0)
      return
      end


      ! -+++: (-m2/s)^9*(log(-m2/s))^1
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_9_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-3.37969328040076975743624565744d0)
      double precision rat2
      parameter (rat2=-4.36277101406807788863311703863d0)
      double precision rat3
      parameter (rat3=-2.46678835459834156108818313543d0)
      double precision rat4
      parameter (rat4=0.248832545384425320941665406911d0)
      double precision rat5
      parameter (rat5=3.58514260707001746254786905337d0)
      double precision rat6
      parameter (rat6=9.12446753144296513591306458371d0)
      double precision rat7
      parameter (rat7=14.8206009131309976296268866081d0)
      double precision rat8
      parameter (rat8=15.9492339053919648819301414133d0)
      double precision rat9
      parameter (rat9=11.3706390709457623693909924212d0)
      double precision rat10
      parameter (rat10=5.17440159766011403758498753345d0)
      double precision rat11
      parameter (rat11=1.33880763614005068043215392757d0)
      double precision rat12
      parameter (rat12=0.133572722924066236124306844262d0)
      double precision rat13
      parameter (rat13=0.000971303552259099612424474293587d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (4968838*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10 + 
     -       rat11*x**11 + rat12*x**12 + 
     -       rat13*x**13))/(3d0*x**4*y**9) + 
     -  ((180180*x + 1261260*x**2 + 
     -       5465460*x**3 + 16396380*x**4 + 
     -       36072036*x**5 + 60120060*x**6 + 
     -       77297220*x**7 + 77297220*x**8 + 
     -       60120060*x**9 + 36072036*x**10 + 
     -       16396380*x**11 + 5465460*x**12 + 
     -       1261260*x**13 + 180180*x**14 + 
     -       12012*x**15)*lins(1))/(x**8*y**8)
      return
      end


      ! -+++: (-m2/s)^9*(log(-m2/s))^0
      ! -+++: F(x)+F(-1-x)
      function OneLoop_HelAmpmppp_HE_9_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=3.48319777215851406192443100349d0)
      double precision rat2
      parameter (rat2=4.55014621130838995246550686632d0)
      double precision rat3
      parameter (rat3=2.69073872215913720583275017913d0)
      double precision rat4
      parameter (rat4=0.855647740205958843735778949023d0)
      double precision rat5
      parameter (rat5=1.15348921091789366647188436935d0)
      double precision rat6
      parameter (rat6=2.9301549389669691636192249672d0)
      double precision rat7
      parameter (rat7=4.73319444075700155320661591461d0)
      double precision rat8
      parameter (rat8=5.04903603584900994781614044346d0)
      double precision rat9
      parameter (rat9=3.52445280391543578417145904627d0)
      double precision rat10
      parameter (rat10=1.5261715483237964600795988224d0)
      double precision rat11
      parameter (rat11=0.414243979535147195476675367801d0)
      double precision rat12
      parameter (rat12=0.0832332691366174790551890182294d0)
      double precision rat13
      parameter (rat13=0.00492718565238762031237681403254d0)
      double precision rat14
      parameter (rat14=-0.0897959183673469387755102040816d0)
      double precision rat15
      parameter (rat15=-0.46122448979591836734693877551d0)
      double precision rat16
      parameter (rat16=-1.68163265306122448979591836735d0)
      double precision rat17
      parameter (rat17=-3.03265306122448979591836734694d0)
      double precision rat18
      parameter (rat18=-3.39591836734693877551020408163d0)
      double precision rat19
      parameter (rat19=-2.1755102040816326530612244898d0)
      double precision rat20
      parameter (rat20=-0.824489795918367346938775510204d0)
      double precision rat21
      parameter (rat21=-0.093877551020408163265306122449d0)
      double precision rat22
      parameter (rat22=195.708313908313908313908313908d0)
      double precision rat23
      parameter (rat23=1229.95819735819735819735819736d0)
      double precision rat24
      parameter (rat24=4786.92996632996632996632996633d0)
      double precision rat25
      parameter (rat25=13096.1232323232323232323232323d0)
      double precision rat26
      parameter (rat26=26572.0311111111111111111111111d0)
      double precision rat27
      parameter (rat27=41173.0296296296296296296296296d0)
      double precision rat28
      parameter (rat28=49504.8d0)
      double precision rat29
      parameter (rat29=46501.8d0)
      double precision rat30
      parameter (rat30=34091.9185185185185185185185185d0)
      double precision rat31
      parameter (rat31=19334.0311111111111111111111111d0)
      double precision rat32
      parameter (rat32=8324.92323232323232323232323232d0)
      double precision rat33
      parameter (rat33=2633.41885521885521885521885522d0)
      double precision rat34
      parameter (rat34=577.558197358197358197358197358d0)
      double precision rat35
      parameter (rat35=78.5083139083139083139083139083d0)
      double precision rat36
      parameter (rat36=4.984998704998704998704998705d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (14113751*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10 + 
     -       rat11*x**11 + rat12*x**12 + 
     -       rat13*x**13))/(36d0*x**4*y**9) + 
     -  (1261260*(zeta2 + (7*x*zeta2)/5d0 + 
     -       (3*x**2*zeta2)/5d0 + 
     -       rat14*x**3*zeta2 + 
     -       rat15*x**4*zeta2 + 
     -       rat16*x**5*zeta2 + 
     -       rat17*x**6*zeta2 + 
     -       rat18*x**7*zeta2 + 
     -       rat19*x**8*zeta2 + 
     -       rat20*x**9*zeta2 + rat21*x**10*zeta2)
     -     )/(x**4*y**7) + 
     -  (6435*(1 + 8*x + rat22*x**2 + 
     -       rat23*x**3 + rat24*x**4 + 
     -       rat25*x**5 + rat26*x**6 + 
     -       rat27*x**7 + rat28*x**8 + 
     -       rat29*x**9 + rat30*x**10 + 
     -       rat31*x**11 + rat32*x**12 + 
     -       rat33*x**13 + rat34*x**14 + 
     -       rat35*x**15 + rat36*x**16)*lins(1))/
     -   (2d0*x**9*y**8) + 
     -  (6006*lins(1)*lins(2))/(x**8*y**8)
      return
      end


      ! --++: (-m2/s)^0*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_0_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -0.5d0+(-1.5d0+3d0*x*y)*zeta2 + 
     -  (-0.5d0+x*y)*lins(1)**2 + 
     -  lins(1)*(-x + y + (0.5d0 - x*y)*lins(2))
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_1_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx(-2d0/y,0d0)
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_1_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=1d0/(x*y) + (2d0*lins(1))/x
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_1_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -(1/(x*y)) + (-6 - 3/(x*y))*zeta2 - 
     -  2*lins(1)**2 + lins(1)*(2/y + 2*lins(2))
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_2_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((3d0-4d0*x*y)/(x**2*y**2),0d0)
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_2_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        -8/x + 18/y**2 - 
     -  ((2 + 4*y**2)*lins(1))/(x**2*y**2)
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_2_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-11 + 16*x*y)/(2d0*x**2*y**2) + 
     -  (6*(-3 + 2*y)*zeta2)/y**2 + 
     -  lins(1)*((-3 + 4*y + 4*y**3)/
     -      (x**2*y**2) + lins(2)/(x**2*y**2))
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_3_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((4d0-12d0*x**5-8d0*x**4*y)/(x**3*y**3),0d0)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_3_1(x) RESULT(res)      
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=17.0666666666666666666666666667d0)
      double precision rat2
      parameter (rat2=17.6333333333333333333333333333d0)
      double precision rat3
      parameter (rat3=6.38333333333333333333333333333d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (40*(1 + (87*x)/10d0 + rat1*x**2 + 
     -       rat2*x**3 + rat3*x**4))/(x**2*y**3)
     -   + (4*(-2 - 2*y**4 + y**5)*lins(1))/
     -   (x**3*y**3)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_3_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-15.3444444444444444444444444444d0)
      double precision rat2
      parameter (rat2=-16.0888888888888888888888888889d0)
      double precision rat3
      parameter (rat3=-5.95555555555555555555555555556d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (20*(-1 - (15*x)/2d0 + rat1*x**2 + 
     -       rat2*x**3 + rat3*x**4))/(x**2*y**3)
     -   - (12*(5*x + y**2*(8 + 24*y + 19*y**2))*
     -     zeta2)/(x**2*y**3) + 
     -  (2*(-16 + 10*y - 8*y**2 + 15*y**3 - 
     -       18*y**4 + 9*y**5)*lins(1))/
     -   (3d0*x**3*y**3) + 
     -  (4*lins(1)*lins(2))/(x**3*y**3)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_4_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((15d0-40d0*x**7-2d0*x*y-24d0*x**6*y)/(x**4*y**4),0d0)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_4_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.18320610687022900763358778626d0)
      double precision rat2
      parameter (rat2=-1.07124681933842239185750636132d0)
      double precision rat3
      parameter (rat3=-0.575063613231552162849872773537d0)
      double precision rat4
      parameter (rat4=0.137404580152671755725190839695d0)
      double precision rat5
      parameter (rat5=-0.305343511450381679389312977099d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (262*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5))/
     -   (x**2*y**4) + 
     -  ((-30 + 40*y**7 + 4*x*(y + 6*y**6))*
     -     lins(1))/(x**4*y**4)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_4_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.55742296918767507002801120448d0)
      double precision rat2
      parameter (rat2=3.90569561157796451914098972923d0)
      double precision rat3
      parameter (rat3=0.939309056956115779645191409897d0)
      double precision rat4
      parameter (rat4=-1.36694677871148459383753501401d0)
      double precision rat5
      parameter (rat5=0.0149393090569561157796451914099d0)
      double precision rat6
      parameter (rat6=-3.56717618664521319388576025744d0)
      double precision rat7
      parameter (rat7=-7.66210780370072405470635559131d0)
      double precision rat8
      parameter (rat8=-9.78921962992759452936444086887d0)
      double precision rat9
      parameter (rat9=-7.64601769911504424778761061947d0)
      double precision rat10
      parameter (rat10=-3.59131134352373290426387771521d0)
      double precision rat11
      parameter (rat11=-0.933226065969428801287208366854d0)
      double precision rat12
      parameter (rat12=-0.102976669348350764279967819791d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (119*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5))/
     -   (2d0*x**2*y**4) + 
     -  (198*(-zeta2 + (4*x*zeta2)/11d0 + 
     -       (50*x**2*zeta2)/33d0 + 
     -       (20*x**3*zeta2)/33d0 - 
     -       (4*x**4*zeta2)/11d0 + 
     -       (8*x**5*zeta2)/33d0))/(x**2*y**4) + 
     -  (1243*(-1 + rat6*x + rat7*x**2 + 
     -       rat8*x**3 + rat9*x**4 + rat10*x**5 + 
     -       rat11*x**6 + rat12*x**7)*lins(1))/
     -   (6d0*x**4*y**4) + 
     -  ((15 - 2*x*y)*lins(1)*lins(2))/(x**4*y**4)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_5_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-4*(-14 + 35*x**9 + 3*x*y + 20*x**8*y))/
     -  (x**5*y**5),0d0)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_5_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-10.2262210796915167095115681234d0)
      double precision rat2
      parameter (rat2=-26.9768637532133676092544987147d0)
      double precision rat3
      parameter (rat3=-36.3334190231362467866323907455d0)
      double precision rat4
      parameter (rat4=-25.4894601542416452442159383033d0)
      double precision rat5
      parameter (rat5=-7.39562982005141388174807197943d0)
      double precision rat6
      parameter (rat6=0.0539845758354755784061696658098d0)
      double precision rat7
      parameter (rat7=-0.194730077120822622107969151671d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (1556*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7))/(x**3*y**5)
     -   + (4*(-28 + 6*x*y + 20*x*y**8 + 35*y**9)*
     -     lins(1))/(x**5*y**5)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_5_0(x) RESULT(res)      
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=9.5652540586126923887834703774d0)
      double precision rat2
      parameter (rat2=25.4206198608475648323845667299d0)
      double precision rat3
      parameter (rat3=33.7701960784313725490196078431d0)
      double precision rat4
      parameter (rat4=23.702314990512333965844402277d0)
      double precision rat5
      parameter (rat5=6.41553447185325743200506008855d0)
      double precision rat6
      parameter (rat6=-0.600463841450558718110900274088d0)
      double precision rat7
      parameter (rat7=-0.0101201771030993042378241619228d0)
      double precision rat8
      parameter (rat8=10.0952380952380952380952380952d0)
      double precision rat9
      parameter (rat9=26.7142857142857142857142857143d0)
      double precision rat10
      parameter (rat10=35.8857142857142857142857142857d0)
      double precision rat11
      parameter (rat11=25.2d0)
      double precision rat12
      parameter (rat12=-4.63213359273670557717250324254d0)
      double precision rat13
      parameter (rat13=-13.769617380025940337224383917d0)
      double precision rat14
      parameter (rat14=-25.7360570687418936446173800259d0)
      double precision rat15
      parameter (rat15=-31.6767996108949416342412451362d0)
      double precision rat16
      parameter (rat16=-26.2050097276264591439688715953d0)
      double precision rat17
      parameter (rat17=-14.480382619974059662775616083d0)
      double precision rat18
      parameter (rat18=-5.13537613488975356679636835279d0)
      double precision rat19
      parameter (rat19=-1.05747405966277561608300907912d0)
      double precision rat20
      parameter (rat20=-0.0960603112840466926070038910506d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (527*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7))/(x**3*y**5)
     -   + (1260*(zeta2 + rat8*x*zeta2 + 
     -       rat9*x**2*zeta2 + rat10*x**3*zeta2 + 
     -       rat11*x**4*zeta2 + 
     -       (36*x**5*zeta2)/5d0 - 
     -       (4*x**6*zeta2)/21d0 + (x**7*zeta2)/7d0)
     -     )/(x**3*y**5) + 
     -  (4112*(-1 + rat12*x + rat13*x**2 + 
     -       rat14*x**3 + rat15*x**4 + 
     -       rat16*x**5 + rat17*x**6 + 
     -       rat18*x**7 + rat19*x**8 + rat20*x**9)
     -      *lins(1))/(5d0*x**5*y**5) + 
     -  ((56 - 12*x*y)*lins(1)*lins(2))/
     -   (x**5*y**5)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_6_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-14*(-15 + 36*x**11 + 4*x*y + 
     -      20*x**10*y))/(x**6*y**6),0d0)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_6_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.991638035569927243330638641876d0)
      double precision rat2
      parameter (rat2=0.505860953920776071139854486661d0)
      double precision rat3
      parameter (rat3=0.919704931285367825383993532741d0)
      double precision rat4
      parameter (rat4=0.235489086499595796281325788197d0)
      double precision rat5
      parameter (rat5=-0.01909862570735650767987065481d0)
      double precision rat6
      parameter (rat6=-0.0721503637833468067906224737268d0)
      double precision rat7
      parameter (rat7=0.00894300727566693613581244947454d0)
      double precision rat8
      parameter (rat8=-0.0877930476960388035569927243331d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (39584*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8))/
     -   (3d0*x**3*y**6) + 
     -  (28*(2*x*y*(2 + 5*y**9) + 
     -       3*(-5 + 6*y**11))*lins(1))/
     -   (x**6*y**6)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_6_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=0.648883678611422172452407614782d0)
      double precision rat2
      parameter (rat2=-1.73380459126539753639417693169d0)
      double precision rat3
      parameter (rat3=-1.91912653975363941769316909295d0)
      double precision rat4
      parameter (rat4=-0.676839305711086226203807390817d0)
      double precision rat5
      parameter (rat5=-0.136730123180291153415453527436d0)
      double precision rat6
      parameter (rat6=-0.346696528555431131019036954087d0)
      double precision rat7
      parameter (rat7=-0.414039753639417693169092945129d0)
      double precision rat8
      parameter (rat8=-0.0190761478163493840985442329227d0)
      double precision rat9
      parameter (rat9=-5.67740339011011671546995504599d0)
      double precision rat10
      parameter (rat10=-21.5992906338928527240483358766d0)
      double precision rat11
      parameter (rat11=-53.3162865509135150740297768796d0)
      double precision rat12
      parameter (rat12=-90.0487689198663752216769084835d0)
      double precision rat13
      parameter (rat13=-107.589310017734152678681898792d0)
      double precision rat14
      parameter (rat14=-92.2581350270136511733410318802d0)
      double precision rat15
      parameter (rat15=-56.6057656617313482080257351425d0)
      double precision rat16
      parameter (rat16=-24.30135686889099682434940405d0)
      double precision rat17
      parameter (rat17=-6.9398276075390769992163979049d0)
      double precision rat18
      parameter (rat18=-1.18451767228935538417123767889d0)
      double precision rat19
      parameter (rat19=-0.0913927496185095063306800841341d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (28576*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8))/
     -   (9d0*x**3*y**6) + 
     -  (10080*(zeta2 + (23*x*zeta2)/24d0 - 
     -       (2*x**2*zeta2)/3d0 - 
     -       (21*x**3*zeta2)/20d0 - 
     -       (3*x**4*zeta2)/10d0 - 
     -       (x**7*zeta2)/12d0 + (x**8*zeta2)/15d0))
     -    /(x**3*y**6) + 
     -  (48494*(-1 + rat9*x + rat10*x**2 + 
     -       rat11*x**3 + rat12*x**4 + 
     -       rat13*x**5 + rat14*x**6 + 
     -       rat15*x**7 + rat16*x**8 + 
     -       rat17*x**9 + rat18*x**10 + 
     -       rat19*x**11)*lins(1))/(15d0*x**6*y**6)
     -    + (210*(1 + (4*x)/15d0 + (4*x**2)/15d0)*
     -     lins(1)*lins(2))/(x**6*y**6)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_7_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-24*(-33 + 77*x**13 + 10*x*y + 
     -      42*x**12*y))/(x**7*y**7),0d0)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_7_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=11.5238425752232310347415022136d0)
      double precision rat2
      parameter (rat2=38.0234336309747129886696180686d0)
      double precision rat3
      parameter (rat3=64.8039318676371276356269227883d0)
      double precision rat4
      parameter (rat4=63.334069397249407754397624586d0)
      double precision rat5
      parameter (rat5=33.6148634887285746443846541393d0)
      double precision rat6
      parameter (rat6=7.4308385768954539120367889033d0)
      double precision rat7
      parameter (rat7=-0.0441209574547910257372251819614d0)
      double precision rat8
      parameter (rat8=-0.0732723043445636677421775343288d0)
      double precision rat9
      parameter (rat9=-0.00426577624371576498836947550086d0)
      double precision rat10
      parameter (rat10=-0.0626266226457567344488632100248d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (213232*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10))/
     -   (3d0*x**4*y**7) + 
     -  (24*(-66 + 20*x*y + 42*x*y**12 + 
     -       77*y**13)*lins(1))/(x**7*y**7)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoop_HelAmpmmpp_HE_7_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-11.0952338149704501022448933732d0)
      double precision rat2
      parameter (rat2=-36.6806134693602390957506572886d0)
      double precision rat3
      parameter (rat3=-62.2557560504258331273454529112d0)
      double precision rat4
      parameter (rat4=-60.7600789525940485629995015017d0)
      double precision rat5
      parameter (rat5=-32.2308408111902094380818775455d0)
      double precision rat6
      parameter (rat6=-7.32951580083731207140953952422d0)
      double precision rat7
      parameter (rat7=-0.216161434574503943731601537044d0)
      double precision rat8
      parameter (rat8=-0.309508775083705984135187973304d0)
      double precision rat9
      parameter (rat9=-0.283118132176805015617626570189d0)
      double precision rat10
      parameter (rat10=-0.0190827172423091615918743399025d0)
      double precision rat11
      parameter (rat11=-11.4545454545454545454545454545d0)
      double precision rat12
      parameter (rat12=-37.8363636363636363636363636364d0)
      double precision rat13
      parameter (rat13=-64.4545454545454545454545454545d0)
      double precision rat14
      parameter (rat14=-62.987012987012987012987012987d0)
      double precision rat15
      parameter (rat15=-33.4285714285714285714285714286d0)
      double precision rat16
      parameter (rat16=-6.71040315691959720400871040316d0)
      double precision rat17
      parameter (rat17=-31.1541254105559499031509041254d0)
      double precision rat18
      parameter (rat18=-95.7238567595856543028669738568d0)
      double precision rat19
      parameter (rat19=-205.93017992276134217206655518d0)
      double precision rat20
      parameter (rat20=-322.683365115075975408751308365d0)
      double precision rat21
      parameter (rat21=-376.831431441667970018888581431d0)
      double precision rat22
      parameter (rat22=-331.14185986356909972449139186d0)
      double precision rat23
      parameter (rat23=-218.567986862209603099171067987d0)
      double precision rat24
      parameter (rat24=-106.873608922147764049134373609d0)
      double precision rat25
      parameter (rat25=-37.5864182677847423573430864183d0)
      double precision rat26
      parameter (rat26=-8.99395445084757997569749395445d0)
      double precision rat27
      parameter (rat27=-1.31111568955353168348993611569d0)
      double precision rat28
      parameter (rat28=-0.0878484462036357511519628484462d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (178004*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10))/
     -   (9d0*x**4*y**7) + 
     -  (55440*(-zeta2 + rat11*x*zeta2 + 
     -       rat12*x**2*zeta2 + 
     -       rat13*x**3*zeta2 + 
     -       rat14*x**4*zeta2 + 
     -       rat15*x**5*zeta2 - 
     -       (52*x**6*zeta2)/7d0 - 
     -       (3*x**9*zeta2)/55d0 + 
     -       (x**10*zeta2)/22d0))/(x**4*y**7) + 
     -  (1329904*(-1 + rat16*x + rat17*x**2 + 
     -       rat18*x**3 + rat19*x**4 + 
     -       rat20*x**5 + rat21*x**6 + 
     -       rat22*x**7 + rat23*x**8 + 
     -       rat24*x**9 + rat25*x**10 + 
     -       rat26*x**11 + rat27*x**12 + 
     -       rat28*x**13)*lins(1))/
     -   (105d0*x**7*y**7) + 
     -  (792*(1 + (10*x)/33d0 + (10*x**2)/33d0)*
     -     lins(1)*lins(2))/(x**7*y**7)
      return
      end


      ! +-+-: (-m2/s)^0*(log(-m2/s))^0
      function OneLoop_HelAmppmpm_HE_0_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        -1 + (-3 + (6*x)/y**2)*zeta2 + 
     -  ((2 + y)*lins(1))/y + 
     -  (-0.5d0 + x/y**2)*lins(1)**2
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^2
      function OneLoop_HelAmppmpm_HE_1_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((2d0*y)/x,0d0)
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^1
      function OneLoop_HelAmppmpm_HE_1_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(2*y)/x + (2*lins(1))/x - (2*y*lins(2))/x
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^0
      function OneLoop_HelAmppmpm_HE_1_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-2*y)/x + 6*(1 + 1/x - 2/y)*zeta2 - 
     -  (2*lins(1)**2)/y + 
     -  lins(1)*(2 - (2*lins(2))/x)
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^2
      function OneLoop_HelAmppmpm_HE_2_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-8*x + 6*y**2)/x**2,0d0)
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^1
      function OneLoop_HelAmppmpm_HE_2_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-28*x + 18*y**2)/x**2 - 
     -  (2*(2 + y**2)*lins(1))/x**2 + 
     -  (-4 - 4/x**2)*lins(2)
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^0
      function OneLoop_HelAmppmpm_HE_2_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (16*x - 11*y**2)/x**2 + 
     -  (6*(4*x - 3*y**2)*zeta2)/x**2 - 
     -  (4*(2 + y)**2*lins(2))/x**2 + 
     -  lins(1)*((4 + 4*y**2 - 3*y**3)/(x**2*y) + 
     -     (4*lins(2))/x**2)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^2
      function OneLoop_HelAmppmpm_HE_3_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-4*(3 + 2*x + 2*x**4 + 3*x**5 - 2*y**5))/
     -  (x**3*y**2),0d0)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^1
      function OneLoop_HelAmppmpm_HE_3_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.44247787610619469026548672566d0)
      double precision rat2
      parameter (rat2=2.6637168141592920353982300885d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (226*(1 + rat1*x + rat2*x**2 + 
     -       rat1*x**3 + x**4))/(3d0*x**3*y) + 
     -  ((4 - 8*y - 8*y**5)*lins(1))/
     -   (x**3*y**2) + 
     -  (4*(3 + 2*x + 2*x**4 + 3*x**5)*lins(2))/
     -   (x**3*y**2)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^0
      function OneLoop_HelAmppmpm_HE_3_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-2.36206896551724137931034482759d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (232*(-1 - (5*x)/4d0 + rat1*x**2 - 
     -       (5*x**3)/4d0 - x**4))/(9d0*x**3*y) - 
     -  (12*(9 + 18*y + 22*y**2 + 13*y**3 + 
     -       5*y**4)*zeta2)/(x**3*y) - 
     -  (2*(-9 + 18*y - 15*y**2 + 8*y**3 - 
     -       10*y**4 + 16*y**5)*lins(1))/
     -   (3d0*x**3*y**2) - 
     -  (2*(40 + 80*y + 77*y**2 + 37*y**3 + 
     -       7*y**4)*lins(2))/(x**3*y) + 
     -  ((-4 + 8*y)*lins(1)*lins(2))/(x**3*y**2)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^2
      function OneLoop_HelAmppmpm_HE_4_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-2*(20 + 12*x**6 + 20*x**7 - 15*y**7 + 
     -      2*x*(6 + y**5)))/(x**4*y**3),0d0)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^1
      function OneLoop_HelAmppmpm_HE_4_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=2.36936936936936936936936936937d0)
      double precision rat2
      parameter (rat2=6.0045045045045045045045045045d0)
      double precision rat3
      parameter (rat3=7.13513513513513513513513513514d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (296*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat2*x**4 + rat1*x**5 + 
     -       x**6))/(x**4*y**2) + 
     -  ((40 - 30*y**7 + 4*x*(6 + y**5))*lins(1))/
     -   (x**4*y**3) + 
     -  (8*(5 + 3*x + 3*x**6 + 5*x**7)*lins(2))/
     -   (x**4*y**3)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^0
      function OneLoop_HelAmppmpm_HE_4_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-1.83493282149712092130518234165d0)
      double precision rat2
      parameter (rat2=-7.99808061420345489443378119002d0)
      double precision rat3
      parameter (rat3=-11.4088291746641074856046065259d0)
      double precision rat4
      parameter (rat4=3.43282381335478680611423974256d0)
      double precision rat5
      parameter (rat5=7.25905068382944489139179404666d0)
      double precision rat6
      parameter (rat6=10.0136765888978278358809332261d0)
      double precision rat7
      parameter (rat7=8.76669348350764279967819790829d0)
      double precision rat8
      parameter (rat8=4.7248592115848753016894609815d0)
      double precision rat9
      parameter (rat9=1.4392598551890587288817377313d0)
      double precision rat10
      parameter (rat10=0.190667739340305711987127916331d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (521*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat2*x**4 + rat1*x**5 - 
     -       x**6))/(9d0*x**4*y**2) + 
     -  (210*(-zeta2 - (16*x*zeta2)/7d0 - 
     -       (45*x**2*zeta2)/7d0 - 8*x**3*zeta2 - 
     -       (45*x**4*zeta2)/7d0 - 
     -       (16*x**5*zeta2)/7d0 - x**6*zeta2))/
     -   (x**4*y**2) + 
     -  (1243*(1 + rat4*x + rat5*x**2 + 
     -       rat6*x**3 + rat7*x**4 + rat8*x**5 + 
     -       rat9*x**6 + rat10*x**7)*lins(1))/
     -   (6d0*x**4*y**3) + 
     -  (148*(-1 + (46*x)/37d0 - (43*x**2)/37d0 + 
     -       (44*x**3)/37d0 - (43*x**4)/37d0 + 
     -       (46*x**5)/37d0 - x**6)*lins(2))/
     -   (3d0*x**4*y**2) + 
     -  (8*(-2 + 3*y)*lins(1)*lins(2))/(x**4*y**3)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^2
      function OneLoop_HelAmppmpm_HE_5_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-4*(35 + 20*x**8 + 35*x**9 - 28*y**9 + 
     -      x*(20 + 6*y**7)))/(x**5*y**4),0d0)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^1
      function OneLoop_HelAmppmpm_HE_5_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=3.31573444851019341348667015159d0)
      double precision rat2
      parameter (rat2=10.9895452169367485624673288029d0)
      double precision rat3
      parameter (rat3=19.9545216936748562467328802927d0)
      double precision rat4
      parameter (rat4=25.0836382645060115002613695766d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (5739*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat3*x**5 + 
     -       rat2*x**6 + rat1*x**7 + x**8))/
     -   (5d0*x**5*y**3) + 
     -  (4*(35 - 28*y**9 + x*(20 + 6*y**7))*
     -     lins(1))/(x**5*y**4) + 
     -  (20*(7 + 4*x + 4*x**8 + 7*x**9)*lins(2))/
     -   (x**5*y**4)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^0
      function OneLoop_HelAmppmpm_HE_5_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-2.176822675316651220265678097d0)
      double precision rat2
      parameter (rat2=-21.6443080012357120790855730615d0)
      double precision rat3
      parameter (rat3=-51.8130599320358356502934816188d0)
      double precision rat4
      parameter (rat4=-67.7730151374729687982700030893d0)
      double precision rat5
      parameter (rat5=-12.1111111111111111111111111111d0)
      double precision rat6
      parameter (rat6=-23.2222222222222222222222222222d0)
      double precision rat7
      parameter (rat7=-29.4444444444444444444444444444d0)
      double precision rat8
      parameter (rat8=4.36786640726329442282749675746d0)
      double precision rat9
      parameter (rat9=12.7125486381322957198443579767d0)
      double precision rat10
      parameter (rat10=24.9515239948119325551232166018d0)
      double precision rat11
      parameter (rat11=33.022940985732814526588845655d0)
      double precision rat12
      parameter (rat12=29.8253891050583657587548638132d0)
      double precision rat13
      parameter (rat13=18.2443255512321660181582360571d0)
      double precision rat14
      parameter (rat14=7.26507782101167315175097276265d0)
      double precision rat15
      parameter (rat15=1.70573929961089494163424124514d0)
      double precision rat16
      parameter (rat16=0.179636835278858625162127107652d0)
      double precision rat17
      parameter (rat17=1.23264540337711069418386491557d0)
      double precision rat18
      parameter (rat18=-1.17636022514071294559099437148d0)
      double precision rat19
      parameter (rat19=1.21388367729831144465290806754d0)
      double precision rat20
      parameter (rat20=-1.26078799249530956848030018762d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (8632*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat3*x**5 + 
     -       rat2*x**6 + rat1*x**7 - x**8))/
     -   (75d0*x**5*y**3) + 
     -  (756*(-zeta2 - (29*x*zeta2)/9d0 + 
     -       rat5*x**2*zeta2 + rat6*x**3*zeta2 + 
     -       rat7*x**4*zeta2 + rat6*x**5*zeta2 + 
     -       rat5*x**6*zeta2 - 
     -       (29*x**7*zeta2)/9d0 - x**8*zeta2))/
     -   (x**5*y**3) + 
     -  (4112*(1 + rat8*x + rat9*x**2 + 
     -       rat10*x**3 + rat11*x**4 + 
     -       rat12*x**5 + rat13*x**6 + 
     -       rat14*x**7 + rat15*x**8 + rat16*x**9)
     -      *lins(1))/(5d0*x**5*y**4) + 
     -  (533*(-1 + rat17*x + rat18*x**2 + 
     -       rat19*x**3 + rat20*x**4 + 
     -       rat19*x**5 + rat18*x**6 + 
     -       rat17*x**7 - x**8)*lins(2))/
     -   (3d0*x**5*y**3) + 
     -  (20*(-3 + 4*y)*lins(1)*lins(2))/
     -   (x**5*y**4)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^2
      function OneLoop_HelAmppmpm_HE_6_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=        (-28*(18 + 10*x**10 + 18*x**11 - 
     -      15*y**11 + 2*x*(5 + 2*y**9)))/
     -  (x**6*y**5)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^1
      function OneLoop_HelAmppmpm_HE_6_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=4.27744328163240792180297593129d0)
      double precision rat2
      parameter (rat2=17.6736835783246497800333328328d0)
      double precision rat3
      parameter (rat3=42.9835588054233419918619840543d0)
      double precision rat4
      parameter (rat4=73.1401030014564345880692482095d0)
      double precision rat5
      parameter (rat5=86.1825498115643909250611852675d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (66601*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat4*x**6 + rat3*x**7 + rat2*x**8 + 
     -       rat1*x**9 + x**10))/(15d0*x**6*y**4)
     -   + (28*(18 - 15*y**11 + 2*x*(5 + 2*y**9))*
     -     lins(1))/(x**6*y**5) + 
     -  (56*(9 + 5*x + 5*x**10 + 9*x**11)*
     -     lins(2))/(x**6*y**5)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^0
      function OneLoop_HelAmppmpm_HE_6_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.661560915896357444404337795634d0)
      double precision rat2
      parameter (rat2=-79.1681131216748536543260252044d0)
      double precision rat3
      parameter (rat3=-284.845650433936502456096297808d0)
      double precision rat4
      parameter (rat4=-557.197884461463613678808518652d0)
      double precision rat5
      parameter (rat5=-689.854454715234074608829391547d0)
      double precision rat6
      parameter (rat6=-19.7272727272727272727272727273d0)
      double precision rat7
      parameter (rat7=-50.9090909090909090909090909091d0)
      double precision rat8
      parameter (rat8=-88.9090909090909090909090909091d0)
      double precision rat9
      parameter (rat9=-105.818181818181818181818181818d0)
      double precision rat10
      parameter (rat10=5.32259660988988328453004495401d0)
      double precision rat11
      parameter (rat11=19.8252567327916855693487854168d0)
      double precision rat12
      parameter (rat12=50.5941765991669072462572689405d0)
      double precision rat13
      parameter (rat13=89.8045325194869468387841794861d0)
      double precision rat14
      parameter (rat14=113.981750319627170371592361942d0)
      double precision rat15
      parameter (rat15=104.83934094939580154245886089d0)
      double precision rat16
      parameter (rat16=69.6680207860766280364581185301d0)
      double precision rat17
      parameter (rat17=32.7203159153709737287087062317d0)
      double precision rat18
      parameter (rat18=10.3307831896729492308326803316d0)
      double precision rat19
      parameter (rat19=1.97135728131315214253309687796d0)
      double precision rat20
      parameter (rat20=0.17208314430651214583247412051d0)
      double precision rat21
      parameter (rat21=1.22843679573857816021307109199d0)
      double precision rat22
      parameter (rat22=-1.18541282524072935873796353206d0)
      double precision rat23
      parameter (rat23=-1.27863142798606842860069657857d0)
      double precision rat24
      parameter (rat24=1.32595779553370211022331489449d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (63719*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat4*x**6 + rat3*x**7 + rat2*x**8 + 
     -       rat1*x**9 - x**10))/(450d0*x**6*y**4)
     -   + (2772*(-zeta2 - (46*x*zeta2)/11d0 + 
     -       rat6*x**2*zeta2 + rat7*x**3*zeta2 + 
     -       rat8*x**4*zeta2 + rat9*x**5*zeta2 + 
     -       rat8*x**6*zeta2 + rat7*x**7*zeta2 + 
     -       rat6*x**8*zeta2 - 
     -       (46*x**9*zeta2)/11d0 - x**10*zeta2))/
     -   (x**6*y**4) + 
     -  (48494*(1 + rat10*x + rat11*x**2 + 
     -       rat12*x**3 + rat13*x**4 + 
     -       rat14*x**5 + rat15*x**6 + 
     -       rat16*x**7 + rat17*x**8 + 
     -       rat18*x**9 + rat19*x**10 + 
     -       rat20*x**11)*lins(1))/(15d0*x**6*y**5)
     -    + (3254*(-1 + rat21*x + rat22*x**2 + 
     -       rat21*x**3 + rat23*x**4 + 
     -       rat24*x**5 + rat23*x**6 + 
     -       rat21*x**7 + rat22*x**8 + 
     -       rat21*x**9 - x**10)*lins(2))/
     -   (5d0*x**6*y**4) + 
     -  (56*(-4 + 5*y)*lins(1)*lins(2))/
     -   (x**6*y**5)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^2
      function OneLoop_HelAmppmpm_HE_7_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-24*(77 + 42*x**12 + 77*x**13 - 
     -      66*y**13 + x*(42 + 20*y**11)))/
     -  (x**7*y**6),0d0)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^1
      function OneLoop_HelAmppmpm_HE_7_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=5.24915911486241623010828716546d0)
      double precision rat2
      parameter (rat2=26.0906471407688820965441421753d0)
      double precision rat3
      parameter (rat3=79.4799921979158239344447609116d0)
      double precision rat4
      parameter (rat4=171.370168997132955715414546676d0)
      double precision rat5
      parameter (rat5=266.480602843992672247077820248d0)
      double precision rat6
      parameter (rat6=308.829497862738731030905784292d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (1804646*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat5*x**7 + rat4*x**8 + 
     -       rat3*x**9 + rat2*x**10 + 
     -       rat1*x**11 + x**12))/(105d0*x**7*y**5)
     -    + (24*(77 - 66*y**13 + 
     -       x*(42 + 20*y**11))*lins(1))/
     -   (x**7*y**6) + 
     -  (1848*(1 + (6*x)/11d0 + (6*x**12)/11d0 + 
     -       x**13)*lins(2))/(x**7*y**6)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^0
      function OneLoop_HelAmppmpm_HE_7_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=10.8295812900088041405385562247d0)
      double precision rat2
      parameter (rat2=-124.525559747345017571986033667d0)
      double precision rat3
      parameter (rat3=-657.955421131389094272745844477d0)
      double precision rat4
      parameter (rat4=-1763.07319465861400636096561408d0)
      double precision rat5
      parameter (rat5=-3047.85855510469046031448099643d0)
      double precision rat6
      parameter (rat6=-3634.7571737153369372583138982d0)
      double precision rat7
      parameter (rat7=29.3076923076923076923076923077d0)
      double precision rat8
      parameter (rat8=211.923076923076923076923076923d0)
      double precision rat9
      parameter (rat9=335.923076923076923076923076923d0)
      double precision rat10
      parameter (rat10=391.461538461538461538461538462d0)
      double precision rat11
      parameter (rat11=6.28959684308040279599128959684d0)
      double precision rat12
      parameter (rat12=28.6292875275207834550463792875d0)
      double precision rat13
      parameter (rat13=90.0849143998363791672180849144d0)
      double precision rat14
      parameter (rat14=200.879815385170658934780254815d0)
      double precision rat15
      parameter (rat15=328.89583007495277854642139583d0)
      double precision rat16
      parameter (rat16=404.210261793332451064136960262d0)
      double precision rat17
      parameter (rat17=376.462553688085756565887462554d0)
      double precision rat18
      parameter (rat18=265.265847760439851297537265848d0)
      double precision rat19
      parameter (rat19=139.507543401628989761667007543d0)
      double precision rat20
      parameter (rat20=53.1806085251266256812521806085d0)
      double precision rat21
      parameter (rat21=13.9046668030173606515959046668d0)
      double precision rat22
      parameter (rat22=2.23349354539876562518798349355d0)
      double precision rat23
      parameter (rat23=0.16636088018383281800791636088d0)
      double precision rat24
      parameter (rat24=-1.22632131219970177279505163749d0)
      double precision rat25
      parameter (rat25=1.19152813828905947975920914563d0)
      double precision rat26
      parameter (rat26=-1.23791903683658253714033246811d0)
      double precision rat27
      parameter (rat27=1.29010879770254597669409620589d0)
      double precision rat28
      parameter (rat28=-1.33881924117744518694427569448d0)
      double precision rat29
      parameter (rat29=1.38289059479759209145634285083d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (3857276*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat5*x**7 + rat4*x**8 + 
     -       rat3*x**9 + rat2*x**10 + 
     -       rat1*x**11 + x**12))/
     -   (11025d0*x**7*y**5) - 
     -  (10296*(1 + (67*x)/13d0 + rat7*x**2 + 
     -       95*x**3 + rat8*x**4 + rat9*x**5 + 
     -       rat10*x**6 + rat9*x**7 + rat8*x**8 + 
     -       95*x**9 + rat7*x**10 + 
     -       (67*x**11)/13d0 + x**12)*zeta2)/
     -   (x**7*y**5) + 
     -  (1329904*(1 + rat11*x + rat12*x**2 + 
     -       rat13*x**3 + rat14*x**4 + 
     -       rat15*x**5 + rat16*x**6 + 
     -       rat17*x**7 + rat18*x**8 + 
     -       rat19*x**9 + rat20*x**10 + 
     -       rat21*x**11 + rat22*x**12 + 
     -       rat23*x**13)*lins(1))/
     -   (105d0*x**7*y**6) - 
     -  (36214*(1 + rat24*x + rat25*x**2 + 
     -       rat26*x**3 + rat27*x**4 + 
     -       rat28*x**5 + rat29*x**6 + 
     -       rat28*x**7 + rat27*x**8 + 
     -       rat26*x**9 + rat25*x**10 + 
     -       rat24*x**11 + x**12)*lins(2))/
     -   (15d0*x**7*y**5) + 
     -  (1848*(-1 - (6*x)/11.)*lins(1)*lins(2))/
     -   (x**7*y**6)
      return
      end



      ! For one-loop W loop
      ! We need to multiply I*(-12)*alpha**2

      ! get the form factors from helicity amplitudes
      ! for high-energy expansion one-loop case of W boson
      subroutine Get_OneLoopW_FormFactors_HE(iterm,m2,shat,
     $     that,uhat,FFs)
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
      double complex OneLoopW_HelAmp_HE
      external OneLoopW_HelAmp_HE
      xs=shat/m2
      xt=that/m2
      xu=uhat/m2
      do ihel=1,5
         amp(ihel)=OneLoopW_HelAmp_HE(ihel,iterm,xs,xt,xu)
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

      double complex function OneLoopW_HelAmp_HE(ihel,iterm,xs,xt,xu)
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
      double complex OneLoopW_HelAmp_HE_pppp
      double complex OneLoopW_HelAmp_HE_mppp
      double complex OneLoopW_HelAmp_HE_mmpp
      double complex OneLoopW_HelAmp_HE_pmpm
      double complex OneLoopW_HelAmp_HE_pmmp
      external OneLoopW_HelAmp_HE_pppp
      external OneLoopW_HelAmp_HE_mppp
      external OneLoopW_HelAmp_HE_mmpp
      external OneLoopW_HelAmp_HE_pmpm
      external OneLoopW_HelAmp_HE_pmmp
      integer iterm_used
      SELECT CASE(ihel)
      CASE(1)
         ! --++,++--
         if(iterm.EQ.-1)then
            iterm_used=10
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_HE=OneLoopW_HelAmp_HE_pppp(iterm_used,xs,xt,xu)
      CASE(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         if(iterm.EQ.-1)then
            iterm_used=9
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_HE=OneLoopW_HelAmp_HE_mppp(iterm_used,xs,xt,xu)
      CASE(3)
         ! ++++,----
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_HE=OneLoopW_HelAmp_HE_mmpp(iterm_used,xs,xt,xu)
      CASE(4)
         ! +--+,-++-
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_HE=OneLoopW_HelAmp_HE_pmpm(iterm_used,xs,xt,xu)
      CASE(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4                                                    
         if(iterm.EQ.-1)then
            iterm_used=7
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_HE=OneLoopW_HelAmp_HE_pmmp(iterm_used,xs,xt,xu)
      CASE DEFAULT
         write(*,*)"ERROR: unknown helicity configuration #5 =",ihel
         stop
      end select
      return
      end

      ! ++++ (4 -> 0)
      ! --++,++-- (2 -> 2)
      function OneLoopW_HelAmp_HE_pppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 10
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double complex OneLoop_HelAmp_HE_pppp
      external OneLoop_HelAmp_HE_pppp
      res=OneLoop_HelAmp_HE_pppp(iterm,xs,xt,xu)
      return
      end

      ! -+++ (4 -> 0)
      ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+ (2 -> 2)
      function OneLoopW_HelAmp_HE_mppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 9
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double complex OneLoop_HelAmp_HE_mppp
      external OneLoop_HelAmp_HE_mppp
      res=OneLoop_HelAmp_HE_mppp(iterm,xs,xt,xu)
      return
      end

      ! --++ (4 -> 0)
      ! ++++,---- (2 -> 2)
      function OneLoopW_HelAmp_HE_mmpp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 7
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x,y
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,fyx,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_0_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_2_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmpmmpp_HE_7_0
      IF(iterm.LT.0.OR.iterm.GT.7)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>7"
         STOP
      ENDIF
      x=xt/xs
      y=xu/xs
      logm2oms=-DLOG(xs)+IPi
      fxy=OneLoopW_HelAmpmmpp_HE_0_0(x)
      fyx=OneLoopW_HelAmpmmpp_HE_0_0(y)
      res=(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_0_1(x)
      fyx=OneLoopW_HelAmpmmpp_HE_0_1(y)
      res=res+logm2oms*(fxy+fyx)
      IF(iterm.LE.0)return
      fxy=OneLoopW_HelAmpmmpp_HE_1_0(x)
      fyx=OneLoopW_HelAmpmmpp_HE_1_0(y)
      res=res+(-1d0/xs)*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_1_1(x)
      fyx=OneLoopW_HelAmpmmpp_HE_1_1(y)
      res=res+(-1d0/xs)*logm2oms*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_1_2(x)
      fyx=OneLoopW_HelAmpmmpp_HE_1_2(y)
      res=res+(-1d0/xs)*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.1)return
      fxy=OneLoopW_HelAmpmmpp_HE_2_0(x)
      fyx=OneLoopW_HelAmpmmpp_HE_2_0(y)
      res=res+(-1d0/xs)**2*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_2_1(x)
      fyx=OneLoopW_HelAmpmmpp_HE_2_1(y)
      res=res+(-1d0/xs)**2*logm2oms*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_2_2(x)
      fyx=OneLoopW_HelAmpmmpp_HE_2_2(y)
      res=res+(-1d0/xs)**2*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.2)return
      fxy=OneLoopW_HelAmpmmpp_HE_3_0(x)
      fyx=OneLoopW_HelAmpmmpp_HE_3_0(y)
      res=res+(-1d0/xs)**3*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_3_1(x)
      fyx=OneLoopW_HelAmpmmpp_HE_3_1(y)
      res=res+(-1d0/xs)**3*logm2oms*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_3_2(x)
      fyx=OneLoopW_HelAmpmmpp_HE_3_2(y)
      res=res+(-1d0/xs)**3*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.3)return
      fxy=OneLoopW_HelAmpmmpp_HE_4_0(x)
      fyx=OneLoopW_HelAmpmmpp_HE_4_0(y)
      res=res+(-1d0/xs)**4*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_4_1(x)
      fyx=OneLoopW_HelAmpmmpp_HE_4_1(y)
      res=res+(-1d0/xs)**4*logm2oms*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_4_2(x)
      fyx=OneLoopW_HelAmpmmpp_HE_4_2(y)
      res=res+(-1d0/xs)**4*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.4)return
      fxy=OneLoopW_HelAmpmmpp_HE_5_0(x)
      fyx=OneLoopW_HelAmpmmpp_HE_5_0(y)
      res=res+(-1d0/xs)**5*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_5_1(x)
      fyx=OneLoopW_HelAmpmmpp_HE_5_1(y)
      res=res+(-1d0/xs)**5*logm2oms*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_5_2(x)
      fyx=OneLoopW_HelAmpmmpp_HE_5_2(y)
      res=res+(-1d0/xs)**5*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.5)return
      fxy=OneLoopW_HelAmpmmpp_HE_6_0(x)
      fyx=OneLoopW_HelAmpmmpp_HE_6_0(y)
      res=res+(-1d0/xs)**6*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_6_1(x)
      fyx=OneLoopW_HelAmpmmpp_HE_6_1(y)
      res=res+(-1d0/xs)**6*logm2oms*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_6_2(x)
      fyx=OneLoopW_HelAmpmmpp_HE_6_2(y)
      res=res+(-1d0/xs)**6*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.6)return
      fxy=OneLoopW_HelAmpmmpp_HE_7_0(x)
      fyx=OneLoopW_HelAmpmmpp_HE_7_0(y)
      res=res+(-1d0/xs)**7*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_7_1(x)
      fyx=OneLoopW_HelAmpmmpp_HE_7_1(y)
      res=res+(-1d0/xs)**7*logm2oms*(fxy+fyx)
      fxy=OneLoopW_HelAmpmmpp_HE_7_2(x)
      fyx=OneLoopW_HelAmpmmpp_HE_7_2(y)
      res=res+(-1d0/xs)**7*logm2oms**2*(fxy+fyx)
      IF(iterm.LE.7)return
      WRITE(*,*)"ERROR: cannot reach here (--++)"
      STOP
      return
      end

      ! +-+- (4 -> 0)
      ! +--+,-++- (2 -> 2)
      function OneLoopW_HelAmp_HE_pmpm(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 7
                    ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double precision x
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double complex fxy,logm2oms
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_0_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_0_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_1_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_1_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_1_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_2_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_2_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_2_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_3_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_3_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_3_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_4_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_4_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_4_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_5_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_5_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_5_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_6_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_6_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_6_0
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_7_2
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_7_1
      COMPLEX(KIND(1d0)),EXTERNAL::OneLoopW_HelAmppmpm_HE_7_0
      IF(iterm.LT.0.OR.iterm.GT.7)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>7"
         STOP
      ENDIF
      x=xt/xs
      logm2oms=-DLOG(xs)+IPi
      fxy=OneLoopW_HelAmppmpm_HE_0_0(x)
      res=fxy
      fxy=OneLoopW_HelAmppmpm_HE_0_1(x)
      res=res+logm2oms*fxy
      IF(iterm.LE.0)return
      fxy=OneLoopW_HelAmppmpm_HE_1_0(x)
      res=res+(-1d0/xs)*fxy
      fxy=OneLoopW_HelAmppmpm_HE_1_1(x)
      res=res+(-1d0/xs)*logm2oms*fxy
      fxy=OneLoopW_HelAmppmpm_HE_1_2(x)
      res=res+(-1d0/xs)*logm2oms**2*fxy
      IF(iterm.LE.1)return
      fxy=OneLoopW_HelAmppmpm_HE_2_0(x)
      res=res+(-1d0/xs)**2*fxy
      fxy=OneLoopW_HelAmppmpm_HE_2_1(x)
      res=res+(-1d0/xs)**2*logm2oms*fxy
      fxy=OneLoopW_HelAmppmpm_HE_2_2(x)
      res=res+(-1d0/xs)**2*logm2oms**2*fxy
      IF(iterm.LE.2)return
      fxy=OneLoopW_HelAmppmpm_HE_3_0(x)
      res=res+(-1d0/xs)**3*fxy
      fxy=OneLoopW_HelAmppmpm_HE_3_1(x)
      res=res+(-1d0/xs)**3*logm2oms*fxy
      fxy=OneLoopW_HelAmppmpm_HE_3_2(x)
      res=res+(-1d0/xs)**3*logm2oms**2*fxy
      IF(iterm.LE.3)return
      fxy=OneLoopW_HelAmppmpm_HE_4_0(x)
      res=res+(-1d0/xs)**4*fxy
      fxy=OneLoopW_HelAmppmpm_HE_4_1(x)
      res=res+(-1d0/xs)**4*logm2oms*fxy
      fxy=OneLoopW_HelAmppmpm_HE_4_2(x)
      res=res+(-1d0/xs)**4*logm2oms**2*fxy
      IF(iterm.LE.4)return
      fxy=OneLoopW_HelAmppmpm_HE_5_0(x)
      res=res+(-1d0/xs)**5*fxy
      fxy=OneLoopW_HelAmppmpm_HE_5_1(x)
      res=res+(-1d0/xs)**5*logm2oms*fxy
      fxy=OneLoopW_HelAmppmpm_HE_5_2(x)
      res=res+(-1d0/xs)**5*logm2oms**2*fxy
      IF(iterm.LE.5)return
      fxy=OneLoopW_HelAmppmpm_HE_6_0(x)
      res=res+(-1d0/xs)**6*fxy
      fxy=OneLoopW_HelAmppmpm_HE_6_1(x)
      res=res+(-1d0/xs)**6*logm2oms*fxy
      fxy=OneLoopW_HelAmppmpm_HE_6_2(x)
      res=res+(-1d0/xs)**6*logm2oms**2*fxy
      IF(iterm.LE.6)return
      fxy=OneLoopW_HelAmppmpm_HE_7_0(x)
      res=res+(-1d0/xs)**7*fxy
      fxy=OneLoopW_HelAmppmpm_HE_7_1(x)
      res=res+(-1d0/xs)**7*logm2oms*fxy
      fxy=OneLoopW_HelAmppmpm_HE_7_2(x)
      res=res+(-1d0/xs)**7*logm2oms**2*fxy
      IF(iterm.LE.7)return
      WRITE(*,*)"ERROR: cannot reach here (+-+-)"
      STOP
      return
      end

      ! +--+ (4 -> 0)
      ! +-+-,-+-+ (2 -> 2)
      function OneLoopW_HelAmp_HE_pmmp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm  ! iterm from 0 to 7
                     ! iterm means add expansion terms up to (-1/xs)^iterm
      double precision xs,xt,xu
      double complex OneLoopW_HelAmp_HE_pmpm
      external OneLoop_HelAmp_HE_pmpm
      res=OneLoopW_HelAmp_HE_pmpm(iterm,xs,xu,xt)
      return
      end

      ! --++: (-m2/s)^0*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_0_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=(-4d0*lins(1))/(3d0*y)
      return
      end


      ! --++: (-m2/s)^0*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_0_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=-0.5d0-(4 + (2*(2 + 1/y))/x - 3*x*y)*
     -   zeta2 - (4d0/3d0 - x*y)*
     -   lins(1)**2 - 
     -  lins(1)*(x - y + 
     -     (-4d0/3d0 + 2/(3d0*x*y) + 
     -        x*y)*lins(2))
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_1_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx(-4d0/(3d0*x*y),0d0)
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_1_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        -23/(3d0*x*y) - 
     -  (8*(-1 + 2*y**2 + y**3)*lins(1))/
     -   (3d0*x**2*y**2)
      return
      end


      ! --++: (-m2/s)^1*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_1_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=11/(3d0*x*y) - (6 - 4/(x*y))*zeta2 - 
     -  2*lins(1)**2 - 
     -  lins(1)*((2*(4 + 7*y**2))/(3d0*x*y**2) + 
     -     (-2d0+4d0/(3d0*x**2*y**2))*lins(2))
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_2_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((6*(2 + 3*y + 2*y**2 + y**3))/(x**3*y**2),0d0)
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_2_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.573643410852713178294573643411d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (43*(-1 + rat1*x + rat1*x**2))/
     -   (x**2*y**2) + 
     -  (4*(6 - 4*x*y + 3*y**3 - 2*y**4 + y**5)*
     -     lins(1))/(3d0*x**3*y**3)
      return
      end


      ! --++: (-m2/s)^2*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_2_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=0.220183486238532110091743119266d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (109*(1 + rat1*x + rat1*x**2))/
     -   (6d0*x**2*y**2) - 
     -  (18*(-1 + x - y**2)*zeta2)/(x**2*y**2) - 
     -  ((28 - 28*y + 42*y**2 - 32*y**3 - 4*y**4)*
     -     lins(1))/(3d0*x**2*y**3) - 
     -  ((12 - 8*x*y)*lins(1)*lins(2))/
     -   (3d0*x**3*y**3)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_3_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-8*(5 + 10*x**7 - 4*x*y + 12*x**6*y + 
     -      3*x**5*y**2))/(3d0*x**4*y**4),0d0)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_3_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.47651006711409395973154362416d0)
      double precision rat2
      parameter (rat2=2.5380313199105145413870246085d0)
      double precision rat3
      parameter (rat3=1.57046979865771812080536912752d0)
      double precision rat4
      parameter (rat4=0.738255033557046979865771812081d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (1192*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4))/
     -   (3d0*x**2*y**3) + 
     -  (8*(10 - 8*x*y + 3*y**5 - 6*y**6 + y**7)*
     -     lins(1))/(3d0*x**4*y**4)
      return
      end


      ! --++: (-m2/s)^3*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_3_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-1.6684881602914389799635701275d0)
      double precision rat2
      parameter (rat2=-2.71357012750455373406193078324d0)
      double precision rat3
      parameter (rat3=-1.65755919854280510018214936248d0)
      double precision rat4
      parameter (rat4=-0.703096539162112932604735883424d0)
      double precision rat5
      parameter (rat5=-2.64860426929392446633825944171d0)
      double precision rat6
      parameter (rat6=-3.35632183908045977011494252874d0)
      double precision rat7
      parameter (rat7=-2.32019704433497536945812807882d0)
      double precision rat8
      parameter (rat8=-0.901477832512315270935960591133d0)
      double precision rat9
      parameter (rat9=-0.180623973727422003284072249589d0)
      double precision rat10
      parameter (rat10=-0.0131362889983579638752052545156d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (488*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4))/
     -   (3d0*x**2*y**3) - 
     -  (8*(50 + 75*y + 92*y**2 + 56*y**3 + 
     -       31*y**4)*zeta2)/(x**2*y**3) + 
     -  (812*(-1 + rat5*x + rat6*x**2 + 
     -       rat7*x**3 + rat8*x**4 + rat9*x**5 + 
     -       rat10*x**6)*lins(1))/(3d0*x**3*y**4)
     -   + (8*(-5 + 4*x*y)*lins(1)*lins(2))/
     -   (3d0*x**4*y**4)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_4_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((140*(-1 - (6*x)/7d0 - (9*x**2)/10d0 - 
     -      (3*x**3)/35d0 - (3*x**4)/70d0 - 
     -      (18*x**7)/35d0 + (44*x**8)/35d0 - 
     -      (8*x**9)/35d0))/(3d0*x**5*y**5),0d0)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_4_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-2.74960254372019077901430842607d0)
      double precision rat2
      parameter (rat2=-4.91335453100158982511923688394d0)
      double precision rat3
      parameter (rat3=-4.70508744038155802861685214626d0)
      double precision rat4
      parameter (rat4=-3.29650238473767885532591414944d0)
      double precision rat5
      parameter (rat5=-1.13275039745627980922098569157d0)
      double precision rat6
      parameter (rat6=-0.377583465818759936406995230525d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (1258*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6))/(x**4*y**4) + 
     -  (4*(20*x*y*(-3 + 4*y**7) + 
     -       3*x**2*(y**2 + 6*y**7) + 
     -       70*(1 + y**9))*lins(1))/
     -   (3d0*x**5*y**5)
      return
      end


      ! --++: (-m2/s)^4*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_4_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.458992199030149694286316677209d0)
      double precision rat2
      parameter (rat2=-0.25100147585916086864853468269d0)
      double precision rat3
      parameter (rat3=0.911501159603626396795277250685d0)
      double precision rat4
      parameter (rat4=-0.130191861690912924309508749736d0)
      double precision rat5
      parameter (rat5=0.296858528357579590976175416403d0)
      double precision rat6
      parameter (rat6=0.0438541007800969850305713683323d0)
      double precision rat7
      parameter (rat7=-0.438095238095238095238095238095d0)
      double precision rat8
      parameter (rat8=-0.604761904761904761904761904762d0)
      double precision rat9
      parameter (rat9=0.247619047619047619047619047619d0)
      double precision rat10
      parameter (rat10=-0.0380952380952380952380952380952d0)
      double precision rat11
      parameter (rat11=-3.72485557695156273144719300844d0)
      double precision rat12
      parameter (rat12=-7.04206784180121463486890830988d0)
      double precision rat13
      parameter (rat13=-8.07680343652792178936453858688d0)
      double precision rat14
      parameter (rat14=-6.0071100577692193749074211228d0)
      double precision rat15
      parameter (rat15=-2.91008739446007998814990371797d0)
      double precision rat16
      parameter (rat16=-0.878388386905643608354317878833d0)
      double precision rat17
      parameter (rat17=-0.146793067693675011109465264405d0)
      double precision rat18
      parameter (rat18=-0.00992445563620204414160865057029d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (1054*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6))/(3d0*x**3*y**4) + 
     -  (840*(-zeta2 + rat7*x*zeta2 + 
     -       rat8*x**2*zeta2 + 
     -       (29*x**3*zeta2)/42d0 - 
     -       (4*x**4*zeta2)/21d0 + 
     -       rat9*x**5*zeta2 + rat10*x**6*zeta2))/
     -   (x**3*y**4) + 
     -  (13502*(-1 + rat11*x + rat12*x**2 + 
     -       rat13*x**3 + rat14*x**4 + 
     -       rat15*x**5 + rat16*x**6 + 
     -       rat17*x**7 + rat18*x**8)*lins(1))/
     -   (9d0*x**4*y**5) - 
     -  (2*(70 - 60*x*y + 3*x**2*y**2)*lins(1)*
     -     lins(2))/(3d0*x**5*y**5)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_5_2(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=0)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-0.960317460317460317460317460317d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=(168*(-1 - (8*x)/9d0 + rat1*x**2 - 
     -      x**3/7d0 - x**4/14d0 - (10*x**9)/21d0 + 
     -      (80*x**10)/63d0 - (16*x**11)/63d0))/
     -  (x**6*y**6)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_5_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-2.3277027027027027027027027027d0)
      double precision rat2
      parameter (rat2=-3.95884663733500942803268384664d0)
      double precision rat3
      parameter (rat3=-3.7985857950974230043997485858d0)
      double precision rat4
      parameter (rat4=-2.41745757385292269013199245757d0)
      double precision rat5
      parameter (rat5=-0.600172847265870521684475172847d0)
      double precision rat6
      parameter (rat6=-0.0534412319296040226272784412319d0)
      double precision rat7
      parameter (rat7=0.0149434318038969201759899434318d0)
      double precision rat8
      parameter (rat8=-682.666666666666666666666666667d0)
      double precision rat9
      parameter (rat9=-42.6666666666666666666666666667d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (50912*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7))/
     -   (3d0*x**3*y**5) + 
     -  ((-3024*x - 14504*x**2 - 39312*x**3 - 
     -       68936*x**4 - 83552*x**5 - 
     -       71232*x**6 - 42560*x**7 - 
     -       17360*x**8 - 4560*x**9 + 
     -       rat8*x**10 + rat9*x**11)*lins(1))/
     -   (x**6*y**6)
      return
      end


      ! --++: (-m2/s)^5*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_5_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=2.46080787275431971621466987069d0)
      double precision rat2
      parameter (rat2=4.04140748369378647442499141778d0)
      double precision rat3
      parameter (rat3=3.98070116718159972536903535874d0)
      double precision rat4
      parameter (rat4=2.47846721592859594919327154137d0)
      double precision rat5
      parameter (rat5=0.604002746309646412633024373498d0)
      double precision rat6
      parameter (rat6=0.0757878475798146240988671472709d0)
      double precision rat7
      parameter (rat7=0.0106579700194530266620894839226d0)
      double precision rat8
      parameter (rat8=2.3630952380952380952380952381d0)
      double precision rat9
      parameter (rat9=3.98571428571428571428571428571d0)
      double precision rat10
      parameter (rat10=3.85178571428571428571428571429d0)
      double precision rat11
      parameter (rat11=-0.00952380952380952380952380952381d0)
      double precision rat12
      parameter (rat12=-4.7761604484147836748992818357d0)
      double precision rat13
      parameter (rat13=-12.0225608688036433701173585567d0)
      double precision rat14
      parameter (rat14=-19.3665382145150931278098908157d0)
      double precision rat15
      parameter (rat15=-21.5185613359023763648041104689d0)
      double precision rat16
      parameter (rat16=-16.9107023997197407602031879489d0)
      double precision rat17
      parameter (rat17=-9.38144450283178606878028843347d0)
      double precision rat18
      parameter (rat18=-3.58060372511239563262684649968d0)
      double precision rat19
      parameter (rat19=-0.887545980031529164477141355754d0)
      double precision rat20
      parameter (rat20=-0.126828983476382320312956151106d0)
      double precision rat21
      parameter (rat21=-0.00771880656273719857534886436621d0)
      double precision rat22
      parameter (rat22=-0.960317460317460317460317460317d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (15536*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7))/
     -   (3d0*x**3*y**5) + 
     -  (13440*(zeta2 + rat8*x*zeta2 + 
     -       rat9*x**2*zeta2 + rat10*x**3*zeta2 + 
     -       (39*x**4*zeta2)/16d0 + 
     -       (3*x**5*zeta2)/5d0 + 
     -       (2*x**6*zeta2)/35d0 + rat11*x**7*zeta2
     -       ))/(x**3*y**5) + 
     -  (7612*(-1 + rat12*x + rat13*x**2 + 
     -       rat14*x**3 + rat15*x**4 + 
     -       rat16*x**5 + rat17*x**6 + 
     -       rat18*x**7 + rat19*x**8 + 
     -       rat20*x**9 + rat21*x**10)*lins(1))/
     -   (x**5*y**6) + 
     -  (168*(-1 - (8*x)/9d0 + rat22*x**2 - 
     -       x**3/7d0 - x**4/14d0)*lins(1)*lins(2))/
     -   (x**6*y**6)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_6_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-56*(11 + 22*x**13 - 10*x*y + 
     -      24*x**12*y + x**2*y**2 + 5*x**11*y**2)
     -    )/(x**7*y**7),0d0)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_6_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-1.31951114279282659263149996248d0)
      double precision rat2
      parameter (rat2=-1.24529338935994597433781045997d0)
      double precision rat3
      parameter (rat3=-0.0686838748405492608989269903204d0)
      double precision rat4
      parameter (rat4=0.133131987694154723493659488257d0)
      double precision rat5
      parameter (rat5=0.187949275906055376303744278532d0)
      double precision rat6
      parameter (rat6=-0.103466646657162152022210550011d0)
      double precision rat7
      parameter (rat7=0.0920424701733323328581076011105d0)
      double precision rat8
      parameter (rat8=-0.0700523373602461169055301268102d0)
      double precision rat9
      parameter (rat9=0.0209283784797778944998874465371d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (426464*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9))/(9d0*x**4*y**6) + 
     -  (56*(x**2*y**2*(2 + 5*y**9) + 
     -       4*x*y*(-5 + 6*y**11) + 22*(1 + y**13)
     -       )*lins(1))/(x**7*y**7)
      return
      end


      ! --++: (-m2/s)^6*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_6_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.38318240039549673041055257185d0)
      double precision rat2
      parameter (rat2=1.02533291386710411001999955057d0)
      double precision rat3
      parameter (rat3=-0.0764435630659985168872609604279d0)
      double precision rat4
      parameter (rat4=-0.439617536684568886092447360733d0)
      double precision rat5
      parameter (rat5=-0.181694793375429765623244421474d0)
      double precision rat6
      parameter (rat6=0.0966874901687602525785937394665d0)
      double precision rat7
      parameter (rat7=-0.108008584076762319947866340082d0)
      double precision rat8
      parameter (rat8=0.114874721916361430080222916339d0)
      double precision rat9
      parameter (rat9=0.0160140221568054650457293094537d0)
      double precision rat10
      parameter (rat10=1.21363636363636363636363636364d0)
      double precision rat11
      parameter (rat11=0.0590909090909090909090909090909d0)
      double precision rat12
      parameter (rat12=-0.186363636363636363636363636364d0)
      double precision rat13
      parameter (rat13=0.0772727272727272727272727272727d0)
      double precision rat14
      parameter (rat14=-0.0136363636363636363636363636364d0)
      double precision rat15
      parameter (rat15=5.81216121413087542389956038247d0)
      double precision rat16
      parameter (rat16=18.2852658945646184357772442705d0)
      double precision rat17
      parameter (rat17=38.0134871064136580013545366053d0)
      double precision rat18
      parameter (rat18=56.4163622195951440590946937606d0)
      double precision rat19
      parameter (rat19=61.7628966206981932229475007829d0)
      double precision rat20
      parameter (rat20=50.4720038160063503006440117587d0)
      double precision rat21
      parameter (rat21=30.7110625344399428084544446036d0)
      double precision rat22
      parameter (rat22=13.6935943526989991479527805626d0)
      double precision rat23
      parameter (rat23=4.32882347086653930462265960107d0)
      double precision rat24
      parameter (rat24=0.91290305353332572721380559244d0)
      double precision rat25
      parameter (rat25=0.114008044685143063825861939221d0)
      double precision rat26
      parameter (rat26=0.0062641249280857581885136266868d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (356008*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9))/(27d0*x**4*y**6) + 
     -  (36960*(zeta2 + (59*x*zeta2)/44d0 + 
     -       rat10*x**2*zeta2 + 
     -       rat11*x**3*zeta2 + 
     -       rat12*x**4*zeta2 - 
     -       (2*x**5*zeta2)/11d0 + 
     -       (x**6*zeta2)/10d0 - 
     -       (x**7*zeta2)/10d0 + 
     -       rat13*x**8*zeta2 + rat14*x**9*zeta2))
     -    /(x**4*y**6) - 
     -  (1647796*(1 + rat15*x + rat16*x**2 + 
     -       rat17*x**3 + rat18*x**4 + 
     -       rat19*x**5 + rat20*x**6 + 
     -       rat21*x**7 + rat22*x**8 + 
     -       rat23*x**9 + rat24*x**10 + 
     -       rat25*x**11 + rat26*x**12)*lins(1))/
     -   (45d0*x**6*y**7) - 
     -  (56*(11 - 10*x*y + x**2*y**2)*lins(1)*
     -     lins(2))/(x**7*y**7)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^2
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_7_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-1.02797202797202797202797202797d0)
      double precision rat2
      parameter (rat2=-0.20979020979020979020979020979d0)
      double precision rat3
      parameter (rat3=-0.104895104895104895104895104895d0)
      double precision rat4
      parameter (rat4=-0.440559440559440559440559440559d0)
      double precision rat5
      parameter (rat5=-0.286713286713286713286713286713d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((2288*(-1 - (12*x)/13d0 + rat1*x**2 + 
     -      rat2*x**3 + rat3*x**4 + rat4*x**13 + 
     -      (14*x**14)/11d0 + rat5*x**15))/
     -  (x**8*y**8),0d0)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^1
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_7_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=1)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=3.23645632757347285876214092694d0)
      double precision rat2
      parameter (rat2=6.14895154102359619083360698034d0)
      double precision rat3
      parameter (rat3=7.52521452393134673136113192144d0)
      double precision rat4
      parameter (rat4=6.09613614576484040859563834458d0)
      double precision rat5
      parameter (rat5=2.90672978392769671921260728534d0)
      double precision rat6
      parameter (rat6=0.677442504126524195668704200491d0)
      double precision rat7
      parameter (rat7=-0.0215802058999594321877770724689d0)
      double precision rat8
      parameter (rat8=0.0208078122104595134636667161751d0)
      double precision rat9
      parameter (rat9=-0.0149353291720285636048903928781d0)
      double precision rat10
      parameter (rat10=0.00475827864678994522955724355197d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(1)=DLOG(-x)+IPi
      res=        (7333048*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10))/
     -   (9d0*x**4*y**7) + 
     -  ((-59488*x - 407792*x**2 - 1645776*x**3 - 
     -       4530592*x**4 - 9097088*x**5 - 
     -       13757744*x**6 - 15945072*x**7 - 
     -       14263392*x**8 - 9833824*x**9 - 
     -       5173168*x**10 - 2034032*x**11 - 
     -       576576*x**12 - 110656*x**13 - 
     -       12752*x**14 - 656*x**15)*lins(1))/
     -   (x**8*y**8)
      return
      end


      ! --++: (-m2/s)^7*(log(-m2/s))^0
      ! --++: F(x)+F(-1-x)
      function OneLoopW_HelAmpmmpp_HE_7_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-3.33162067122009157500221305094d0)
      double precision rat2
      parameter (rat2=-6.2754793119816320257396992063d0)
      double precision rat3
      parameter (rat3=-7.74100119491236837503482808026d0)
      double precision rat4
      parameter (rat4=-6.20678132947183979989211492263d0)
      double precision rat5
      parameter (rat5=-2.98993846601735996387652212776d0)
      double precision rat6
      parameter (rat6=-0.672198405325922764714356255951d0)
      double precision rat7
      parameter (rat7=0.0265533673730416099522724964078d0)
      double precision rat8
      parameter (rat8=-0.0198883649339557747428660989032d0)
      double precision rat9
      parameter (rat9=0.0281615170563388357871254248592d0)
      double precision rat10
      parameter (rat10=0.00394503360430541147035929264249d0)
      double precision rat11
      parameter (rat11=-3.25775401069518716577540106952d0)
      double precision rat12
      parameter (rat12=-6.1796791443850267379679144385d0)
      double precision rat13
      parameter (rat13=-7.57708174178762414056531703591d0)
      double precision rat14
      parameter (rat14=-6.12574484339190221543162719633d0)
      double precision rat15
      parameter (rat15=-2.92773109243697478991596638655d0)
      double precision rat16
      parameter (rat16=-0.677310924369747899159663865546d0)
      double precision rat17
      parameter (rat17=0.021848739495798319327731092437d0)
      double precision rat18
      parameter (rat18=-0.021848739495798319327731092437d0)
      double precision rat19
      parameter (rat19=0.0170359052711993888464476699771d0)
      double precision rat20
      parameter (rat20=-0.00313216195569136745607333842628d0)
      double precision rat21
      parameter (rat21=6.83854251029068910780587564647d0)
      double precision rat22
      parameter (rat22=25.8256477427662871157870285291d0)
      double precision rat23
      parameter (rat23=65.8405878841196674315607407057d0)
      double precision rat24
      parameter (rat24=122.474193073078261487481359719d0)
      double precision rat25
      parameter (rat25=172.378425135302053539798502089d0)
      double precision rat26
      parameter (rat26=186.892466646791662231614662976d0)
      double precision rat27
      parameter (rat27=157.138274496303741820408086489d0)
      double precision rat28
      parameter (rat28=102.254675092195587360870614766d0)
      double precision rat29
      parameter (rat29=50.9592594256860198153893741777d0)
      double precision rat30
      parameter (rat30=19.047322017319916859383049806d0)
      double precision rat31
      parameter (rat31=5.15080261627512061972990794549d0)
      double precision rat32
      parameter (rat32=0.946830133353118765890610941414d0)
      double precision rat33
      parameter (rat33=0.105049823610579692771444963082d0)
      double precision rat34
      parameter (rat34=0.00524346308168164034771205806169d0)
      double precision rat35
      parameter (rat35=-1.02797202797202797202797202797d0)
      double precision rat36
      parameter (rat36=-0.20979020979020979020979020979d0)
      double precision rat37
      parameter (rat37=-0.104895104895104895104895104895d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (5738684*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat7*x**7 + rat8*x**8 + 
     -       rat9*x**9 + rat10*x**10))/
     -   (27d0*x**4*y**7) + 
     -  (628320*(-zeta2 + rat11*x*zeta2 + 
     -       rat12*x**2*zeta2 + 
     -       rat13*x**3*zeta2 + 
     -       rat14*x**4*zeta2 + 
     -       rat15*x**5*zeta2 + 
     -       rat16*x**6*zeta2 + 
     -       rat17*x**7*zeta2 + 
     -       rat18*x**8*zeta2 + 
     -       rat19*x**9*zeta2 + rat20*x**10*zeta2)
     -     )/(x**4*y**7) - 
     -  (7655464*(1 + rat21*x + rat22*x**2 + 
     -       rat23*x**3 + rat24*x**4 + 
     -       rat25*x**5 + rat26*x**6 + 
     -       rat27*x**7 + rat28*x**8 + 
     -       rat29*x**9 + rat30*x**10 + 
     -       rat31*x**11 + rat32*x**12 + 
     -       rat33*x**13 + rat34*x**14)*lins(1))/
     -   (45d0*x**7*y**8) + 
     -  (2288*(-1 - (12*x)/13d0 + rat35*x**2 + 
     -       rat36*x**3 + rat37*x**4)*lins(1)*
     -     lins(2))/(x**8*y**8)
      return
      end


      ! +-+-: (-m2/s)^0*(log(-m2/s))^1
      function OneLoopW_HelAmppmpm_HE_0_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=(-4*y*lins(1))/3d0 - (4*y**2*lins(2))/(3d0*x)
      return
      end


      ! +-+-: (-m2/s)^0*(log(-m2/s))^0
      function OneLoopW_HelAmppmpm_HE_0_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        -1 - (8 - (6*x)/y**2)*zeta2 - 
     -  (4d0/3d0 + y**(-2) + 1/y)*
     -   lins(1)**2 - 
     -  lins(1)*((-1 + x)/y + 
     -     (4*y*lins(2))/(3d0*x))
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^2
      function OneLoopW_HelAmppmpm_HE_1_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-8d0*y)/(3d0*x),0d0)
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^1
      function OneLoopW_HelAmppmpm_HE_1_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-46*y)/(3d0*x) + 
     -  (8*(x - y + y**3)*lins(1))/(3d0*x**2) - 
     -  (8*y*(1 + y + y**2)*lins(2))/(3d0*x**2)
      return
      end


      ! +-+-: (-m2/s)^1*(log(-m2/s))^0
      function OneLoopW_HelAmppmpm_HE_1_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (22*y)/(3d0*x) - (12/y - (8*y)/x)*zeta2 - 
     -  (2*lins(1)**2)/y - 
     -  (8*y*(2 + y)**2*lins(2))/(3d0*x**2) - 
     -  lins(1)*((2*(7 + 4*y**2))/(3d0*x) - 
     -     (8*(1 + 2*y)*lins(2))/(3d0*x**2))
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^2
      function OneLoopW_HelAmppmpm_HE_2_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((12*(1 + 2*y + 3*y**2 + 2*y**3))/x**3,0d0)
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^1
      function OneLoopW_HelAmppmpm_HE_2_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-1.42635658914728682170542635659d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (86*(-1 + rat1*x - x**2))/x**2 + 
     -  (4*(6 + 8*x + 3*x**2 - 4*x*y**3 + 6*y**5)*
     -     lins(1))/(3d0*x**3*y) - 
     -  (4*(9 + 18*y + 31*y**2 + 22*y**3 + 
     -       6*y**4)*lins(2))/(3d0*x**3)
      return
      end


      ! +-+-: (-m2/s)^2*(log(-m2/s))^0
      function OneLoopW_HelAmppmpm_HE_2_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=1.77981651376146788990825688073d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (109*(1 + rat1*x + x**2))/(3d0*x**2) - 
     -  (36*(x - 2*y**2)*zeta2)/x**2 + 
     -  ((4 + 32*y - 42*y**2 + 28*y**3 - 28*y**4)*
     -     lins(1))/(3d0*x**2*y) + 
     -  (4*(12*x + 7*y + x**3*(12 + 7*y))*
     -     lins(2))/(3d0*x**3) - 
     -  (4*(6 + 8*x + 3*x**2)*lins(1)*lins(2))/
     -   (3d0*x**3*y)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^2
      function OneLoopW_HelAmppmpm_HE_3_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-133.333333333333333333333333333d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((rat1*x - 312*x**2 - 408*x**3 - 
     -    312*x**4 + rat1*x**5)/(x**4*y),0d0)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^1
      function OneLoopW_HelAmppmpm_HE_3_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-2.31574158325750682438580527753d0)
      double precision rat2
      parameter (rat2=-3.07916287534121929026387625114d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (4396*(-1 + rat1*x + rat2*x**2 + 
     -       rat1*x**3 - x**4))/(9d0*x**3*y) + 
     -  (8*(3*x**2 + x*(12 - 8*y**5) + 
     -       10*(1 + y**7))*lins(1))/
     -   (3d0*x**4*y**2) + 
     -  (8*(10 + 12*x + 3*x**2 + 3*x**5 + 
     -       12*x**6 + 10*x**7)*lins(2))/
     -   (3d0*x**4*y**2)
      return
      end


      ! +-+-: (-m2/s)^3*(log(-m2/s))^0
      function OneLoopW_HelAmppmpm_HE_3_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=2.45135699373695198329853862213d0)
      double precision rat2
      parameter (rat2=3.08643006263048016701461377871d0)
      double precision rat3
      parameter (rat3=2.34d0)
      double precision rat4
      parameter (rat4=3.06d0)
      double precision rat5
      parameter (rat5=-3.35139573070607553366174055829d0)
      double precision rat6
      parameter (rat6=-5.1133004926108374384236453202d0)
      double precision rat7
      parameter (rat7=-2.59277504105090311986863711002d0)
      double precision rat8
      parameter (rat8=-0.844006568144499178981937602627d0)
      double precision rat9
      parameter (rat9=-0.121510673234811165845648604269d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (4790*(1 + rat1*x + rat2*x**2 + 
     -       rat1*x**3 + x**4))/(27d0*x**3*y) + 
     -  (400*(zeta2 + rat3*x*zeta2 + 
     -       rat4*x**2*zeta2 + rat3*x**3*zeta2 + 
     -       x**4*zeta2))/(x**3*y) + 
     -  (812*(-1 + rat5*x + rat6*x**2 - 
     -       (97*x**3)/21d0 + rat7*x**4 + 
     -       rat8*x**5 + rat9*x**6)*lins(1))/
     -   (3d0*x**3*y**2) + 
     -  (296*(-1 + (25*x)/37d0 - (13*x**2)/37d0 + 
     -       (14*x**3)/37d0 - (13*x**4)/37d0 + 
     -       (25*x**5)/37d0 - x**6)*lins(2))/
     -   (9d0*x**4*y) - 
     -  (8*(1 - 6*y + 3*y**2)*lins(1)*lins(2))/
     -   (3d0*x**4*y**2)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^2
      function OneLoopW_HelAmppmpm_HE_4_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-4*(18*x**7 + 80*x**8 + 70*x**9 + 
     -      3*x**2*(6 + y**5) + 
     -      x*(80 - 60*y**7) + 70*(1 + y**9)))/
     -  (3d0*x**5*y**3),0d0)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^1
      function OneLoopW_HelAmppmpm_HE_4_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-3.25039745627980922098569157393d0)
      double precision rat2
      parameter (rat2=-6.16534181240063593004769475358d0)
      double precision rat3
      parameter (rat3=-7.45230524642289348171701112878d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2516*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat2*x**4 + rat1*x**5 - 
     -       x**6))/(x**4*y**2) + 
     -  (4*(3*x**2*(6 + y**5) + 
     -       x*(80 - 60*y**7) + 70*(1 + y**9))*
     -     lins(1))/(3d0*x**5*y**3) + 
     -  (8*(35 + 40*x + 9*x**2 + 9*x**7 + 
     -       40*x**8 + 35*x**9)*lins(2))/
     -   (3d0*x**5*y**3)
      return
      end


      ! +-+-: (-m2/s)^4*(log(-m2/s))^0
      function OneLoopW_HelAmppmpm_HE_4_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=3.31339513501225087776907726894d0)
      double precision rat2
      parameter (rat2=6.42529490515042057137083533305d0)
      double precision rat3
      parameter (rat3=8.08638763292833867993634595468d0)
      double precision rat4
      parameter (rat4=3.25510204081632653061224489796d0)
      double precision rat5
      parameter (rat5=6.20408163265306122448979591837d0)
      double precision rat6
      parameter (rat6=-4.27514442304843726855280699156d0)
      double precision rat7
      parameter (rat7=-8.96807880314027551473855725078d0)
      double precision rat8
      parameter (rat8=-11.9536364982965486594578580951d0)
      double precision rat9
      parameter (rat9=-10.8841653088431343504665975411d0)
      double precision rat10
      parameter (rat10=-6.82173011405717671456080580655d0)
      double precision rat11
      parameter (rat11=-2.83180269589690416234631906384d0)
      double precision rat12
      parameter (rat12=-0.702562583320989483039549696341d0)
      double precision rat13
      parameter (rat13=-0.0789512664790401422011553843875d0)
      double precision rat14
      parameter (rat14=0.677298311444652908067542213884d0)
      double precision rat15
      parameter (rat15=-0.407129455909943714821763602251d0)
      double precision rat16
      parameter (rat16=0.422138836772983114446529080675d0)
      double precision rat17
      parameter (rat17=-0.420262664165103189493433395872d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (39589*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat2*x**4 + rat1*x**5 + 
     -       x**6))/(54d0*x**4*y**2) + 
     -  (1960*(zeta2 + rat4*x*zeta2 + 
     -       rat5*x**2*zeta2 + 
     -       (53*x**3*zeta2)/7d0 + 
     -       rat5*x**4*zeta2 + rat4*x**5*zeta2 + 
     -       x**6*zeta2))/(x**4*y**2) + 
     -  (13502*(-1 + rat6*x + rat7*x**2 + 
     -       rat8*x**3 + rat9*x**4 + rat10*x**5 + 
     -       rat11*x**6 + rat12*x**7 + rat13*x**8)
     -      *lins(1))/(9d0*x**4*y**3) + 
     -  (1066*(-1 + rat14*x + rat15*x**2 + 
     -       rat16*x**3 + rat17*x**4 + 
     -       rat16*x**5 + rat15*x**6 + 
     -       rat14*x**7 - x**8)*lins(2))/
     -   (9d0*x**5*y**2) - 
     -  (8*(4 - 22*y + 9*y**2)*lins(1)*lins(2))/
     -   (3d0*x**5*y**3)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^2
      function OneLoopW_HelAmppmpm_HE_5_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((3024*x + 15736*x**2 + 44856*x**3 + 
     -    86296*x**4 + 118440*x**5 + 
     -    118440*x**6 + 86296*x**7 + 44856*x**8 + 
     -    15736*x**9 + 3024*x**10)/(x**6*y**4),0d0)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^1
      function OneLoopW_HelAmppmpm_HE_5_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-4.20606895948192876520521571716d0)
      double precision rat2
      parameter (rat2=-10.4600507569790846241358186751d0)
      double precision rat3
      parameter (rat3=-17.2885709285026691170035879933d0)
      double precision rat4
      parameter (rat4=-20.4052463463726262361074647764d0)
      double precision rat5
      parameter (rat5=-3397.33333333333333333333333333d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (60944*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat3*x**5 + 
     -       rat2*x**6 + rat1*x**7 - x**8))/
     -   (5d0*x**5*y**3) + 
     -  ((-3024*x - 15736*x**2 - 44856*x**3 - 
     -       86296*x**4 - 118440*x**5 - 
     -       118440*x**6 - 86296*x**7 - 
     -       44856*x**8 - 15816*x**9 + 
     -       rat5*x**10 - 336*x**11)*lins(1))/
     -   (x**6*y**4) + 
     -  (16*(63 + 70*x + 15*x**2 + 15*x**9 + 
     -       70*x**10 + 63*x**11)*lins(2))/
     -   (3d0*x**6*y**4)
      return
      end


      ! +-+-: (-m2/s)^5*(log(-m2/s))^0
      function OneLoopW_HelAmppmpm_HE_5_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=4.23189477853764692968850667418d0)
      double precision rat2
      parameter (rat2=11.4293870512569656881694258984d0)
      double precision rat3
      parameter (rat3=20.6243722330131065497751413023d0)
      double precision rat4
      parameter (rat4=24.9569197142298092479204348594d0)
      double precision rat5
      parameter (rat5=4.2037037037037037037037037037d0)
      double precision rat6
      parameter (rat6=10.6296296296296296296296296296d0)
      double precision rat7
      parameter (rat7=17.9074074074074074074074074074d0)
      double precision rat8
      parameter (rat8=21.2592592592592592592592592593d0)
      double precision rat9
      parameter (rat9=-5.2238395515852163251007181643d0)
      double precision rat10
      parameter (rat10=-14.0371168330705902960238220354d0)
      double precision rat11
      parameter (rat11=-24.8721725929818415367548315525d0)
      double precision rat12
      parameter (rat12=-31.3870204939569101418812401471d0)
      double precision rat13
      parameter (rat13=-28.9705552636188474338763356104d0)
      double precision rat14
      parameter (rat14=-19.5605593507327611373854148421d0)
      double precision rat15
      parameter (rat15=-9.44647048519880889823086354878d0)
      double precision rat16
      parameter (rat16=-3.09842354177614293221229637415d0)
      double precision rat17
      parameter (rat17=-0.619361242482629765866760086413d0)
      double precision rat18
      parameter (rat18=-0.056997722893676650902084428096d0)
      double precision rat19
      parameter (rat19=0.682442122515877893874206105306d0)
      double precision rat20
      parameter (rat20=-0.436590862528170456873591477156d0)
      double precision rat21
      parameter (rat21=0.448883425527555828723622208564d0)
      double precision rat22
      parameter (rat22=-0.447859045277607047736119647613d0)
      double precision rat23
      parameter (rat23=0.449088301577545584921122720754d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (209539*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat3*x**5 + 
     -       rat2*x**6 + rat1*x**7 + x**8))/
     -   (75d0*x**5*y**3) + 
     -  (9072*(zeta2 + rat5*x*zeta2 + 
     -       rat6*x**2*zeta2 + rat7*x**3*zeta2 + 
     -       rat8*x**4*zeta2 + rat7*x**5*zeta2 + 
     -       rat6*x**6*zeta2 + rat5*x**7*zeta2 + 
     -       x**8*zeta2))/(x**5*y**3) + 
     -  (7612*(-1 + rat9*x + rat10*x**2 + 
     -       rat11*x**3 + rat12*x**4 + 
     -       rat13*x**5 + rat14*x**6 + 
     -       rat15*x**7 + rat16*x**8 + 
     -       rat17*x**9 + rat18*x**10)*lins(1))/
     -   (x**5*y**4) + 
     -  (6508*(-1 + rat19*x + rat20*x**2 + 
     -       rat21*x**3 + rat22*x**4 + 
     -       rat23*x**5 + rat22*x**6 + 
     -       rat21*x**7 + rat20*x**8 + 
     -       rat19*x**9 - x**10)*lins(2))/
     -   (15d0*x**6*y**3) - 
     -  (16*(8 - 40*y + 15*y**2)*lins(1)*lins(2))/
     -   (3d0*x**6*y**4)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^2
      function OneLoopW_HelAmppmpm_HE_6_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((-56*(5*x**11 + 24*x**12 + 22*x**13 + 
     -      x**2*(5 + 2*y**9) - 
     -      4*x*(-6 + 5*y**11) + 22*(1 + y**13)))/
     -  (x**7*y**5),0d0)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^1
      function OneLoopW_HelAmppmpm_HE_6_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=-5.17448414797932522078873468723d0)
      double precision rat2
      parameter (rat2=-15.9981568327891228872059933067d0)
      double precision rat3
      parameter (rat3=-33.6706741530822764797775989832d0)
      double precision rat4
      parameter (rat4=-51.7768014513669961148696242913d0)
      double precision rat5
      parameter (rat5=-59.5505920049589415915572767146d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2555384*(-1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat4*x**6 + rat3*x**7 + rat2*x**8 + 
     -       rat1*x**9 - x**10))/(45d0*x**6*y**4)
     -   + (56*(x**2*(5 + 2*y**9) - 
     -       4*x*(-6 + 5*y**11) + 22*(1 + y**13))*
     -     lins(1))/(x**7*y**5) + 
     -  (56*(22 + 24*x + 5*x**2 + 5*x**11 + 
     -       24*x**12 + 22*x**13)*lins(2))/
     -   (x**7*y**5)
      return
      end


      ! +-+-: (-m2/s)^6*(log(-m2/s))^0
      function OneLoopW_HelAmppmpm_HE_6_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=5.17639992337303321790771625616d0)
      double precision rat2
      parameter (rat2=18.4312136979377319152933870428d0)
      double precision rat3
      parameter (rat3=44.3791138294179629076670043722d0)
      double precision rat4
      parameter (rat4=73.5883063308547185348312346924d0)
      double precision rat5
      parameter (rat5=86.8578827051671598461756277729d0)
      double precision rat6
      parameter (rat6=5.16942148760330578512396694215d0)
      double precision rat7
      parameter (rat7=16.3595041322314049586776859504d0)
      double precision rat8
      parameter (rat8=35.3016528925619834710743801653d0)
      double precision rat9
      parameter (rat9=55.1198347107438016528925619835d0)
      double precision rat10
      parameter (rat10=63.7396694214876033057851239669d0)
      double precision rat11
      parameter (rat11=6.18783878586912457610043961753d0)
      double precision rat12
      parameter (rat12=20.3514925391249887728820800633d0)
      double precision rat13
      parameter (rat13=45.1703050620343780419420850639d0)
      double precision rat14
      parameter (rat14=73.1253431856856067134523933788d0)
      double precision rat15
      parameter (rat15=89.3011719897365936074611177597d0)
      double precision rat16
      parameter (rat16=83.3443096111411849525062568425d0)
      double precision rat17
      parameter (rat17=59.3855647179626604264120073116d0)
      double precision rat18
      parameter (rat18=31.8555743550779344045015281018d0)
      double precision rat19
      parameter (rat19=12.4822975659608349577253494971d0)
      double precision rat20
      parameter (rat20=3.37730641414349834566900271636d0)
      double precision rat21
      parameter (rat21=0.564538328773707424948233883321d0)
      double precision rat22
      parameter (rat22=0.0439544700921716037664856572051d0)
      double precision rat23
      parameter (rat23=-0.687192798365273098801568454189d0)
      double precision rat24
      parameter (rat24=0.455652510078974982051140442923d0)
      double precision rat25
      parameter (rat25=-0.467250234715855746396421273541d0)
      double precision rat26
      parameter (rat26=0.467250234715855746396421273541d0)
      double precision rat27
      parameter (rat27=-0.469569779643231899265477439664d0)
      double precision rat28
      parameter (rat28=0.473628983266140166786325730381d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (2267261*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat4*x**6 + rat3*x**7 + rat2*x**8 + 
     -       rat1*x**9 + x**10))/(225d0*x**6*y**4)
     -   + (40656*(zeta2 + rat6*x*zeta2 + 
     -       rat7*x**2*zeta2 + rat8*x**3*zeta2 + 
     -       rat9*x**4*zeta2 + rat10*x**5*zeta2 + 
     -       rat9*x**6*zeta2 + rat8*x**7*zeta2 + 
     -       rat7*x**8*zeta2 + rat6*x**9*zeta2 + 
     -       x**10*zeta2))/(x**6*y**4) - 
     -  (1647796*(1 + rat11*x + rat12*x**2 + 
     -       rat13*x**3 + rat14*x**4 + 
     -       rat15*x**5 + rat16*x**6 + 
     -       rat17*x**7 + rat18*x**8 + 
     -       rat19*x**9 + rat20*x**10 + 
     -       rat21*x**11 + rat22*x**12)*lins(1))/
     -   (45d0*x**6*y**5) - 
     -  (72428*(1 + rat23*x + rat24*x**2 + 
     -       rat25*x**3 + rat26*x**4 + 
     -       rat27*x**5 + rat28*x**6 + 
     -       rat27*x**7 + rat26*x**8 + 
     -       rat25*x**9 + rat24*x**10 + 
     -       rat23*x**11 + x**12)*lins(2))/
     -   (45d0*x**7*y**4) - 
     -  (56*(3 - 14*y + 5*y**2)*lins(1)*lins(2))/
     -   (x**7*y**5)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^2
      function OneLoopW_HelAmppmpm_HE_7_2(x) RESULT(res)
      implicit none
      double complex res
      !integer NDIM
      !parameter (NDIM=0)
      double precision x,y
      !double complex lins(NDIM)
      !double complex IPi
      !parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      !double precision zeta2
      !parameter (zeta2=1.64493406684822643647241516665d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      res=dcmplx((59488*x + 425040*x**2 + 1757888*x**3 + 
     -    5064576*x**4 + 10800768*x**5 + 
     -    17624992*x**6 + 22419936*x**7 + 
     -    22419936*x**8 + 17624992*x**9 + 
     -    10800768*x**10 + 5064576*x**11 + 
     -    1757888*x**12 + 425040*x**13 + 
     -    59488*x**14)/(x**8*y**6),0d0)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^1
      function OneLoopW_HelAmppmpm_HE_7_1(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=6.15102244380669578265094511289d0)
      double precision rat2
      parameter (rat2=22.7969044272711877338286717595d0)
      double precision rat3
      parameter (rat3=58.4248336128549311493650782072d0)
      double precision rat4
      parameter (rat4=111.082080924114765581365822634d0)
      double precision rat5
      parameter (rat5=161.5496942052777436603929889d0)
      double precision rat6
      parameter (rat6=182.783215327706626327760447461d0)
      double precision rat7
      parameter (rat7=0.22027972027972027972027972028d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (-81157712*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat5*x**7 + rat4*x**8 + 
     -       rat3*x**9 + rat2*x**10 + 
     -       rat1*x**11 + x**12))/(315d0*x**7*y**5)
     -    + ((-59488*x - 425040*x**2 - 
     -       1757888*x**3 - 5064576*x**4 - 
     -       10800768*x**5 - 17624992*x**6 - 
     -       22419936*x**7 - 22419936*x**8 - 
     -       17624992*x**9 - 10800768*x**10 - 
     -       5064576*x**11 - 1757888*x**12 - 
     -       426048*x**13 - 64416*x**14 - 
     -       4576*x**15)*lins(1))/(x**8*y**6) + 
     -  (4576*(1 + (14*x)/13d0 + rat7*x**2 + 
     -       rat7*x**13 + (14*x**14)/13d0 + x**15)*
     -     lins(2))/(x**8*y**6)
      return
      end


      ! +-+-: (-m2/s)^7*(log(-m2/s))^0
      function OneLoopW_HelAmppmpm_HE_7_0(x) RESULT(res)
      implicit none
      double complex res
      integer NDIM
      parameter (NDIM=2)
      double precision x,y
      double complex lins(NDIM)
      double complex IPi
      parameter (IPi=(0d0,3.14159265358979323846264338328d0))
      double precision zeta2
      parameter (zeta2=1.64493406684822643647241516665d0)
      double precision rat1
      parameter (rat1=6.1336712916808582956496583187d0)
      double precision rat2
      parameter (rat2=28.0028348914123210646048478629d0)
      double precision rat3
      parameter (rat3=86.3401964395583600705283101666d0)
      double precision rat4
      parameter (rat4=184.684303616051887874765830137d0)
      double precision rat5
      parameter (rat5=286.678279343164433634034941171d0)
      double precision rat6
      parameter (rat6=330.961006875409219764848029397d0)
      double precision rat7
      parameter (rat7=6.14497041420118343195266272189d0)
      double precision rat8
      parameter (rat8=23.4053254437869822485207100592d0)
      double precision rat9
      parameter (rat9=61.7307692307692307692307692308d0)
      double precision rat10
      parameter (rat10=119.831360946745562130177514793d0)
      double precision rat11
      parameter (rat11=176.446745562130177514792899408d0)
      double precision rat12
      parameter (rat12=200.434911242603550295857988166d0)
      double precision rat13
      parameter (rat13=7.16145748970931089219412435353d0)
      double precision rat14
      parameter (rat14=27.924595108987328714310645125d0)
      double precision rat15
      parameter (rat15=74.660869226402027549025301219d0)
      double precision rat16
      parameter (rat16=147.897319427199784549776659987d0)
      double precision rat17
      parameter (rat17=225.215780519639305991119545465d0)
      double precision rat18
      parameter (rat18=268.619749763045061670984279986d0)
      double precision rat19
      parameter (rat19=252.861385429133641390925861207d0)
      double precision rat20
      parameter (rat20=187.676617903238784742505483665d0)
      double precision rat21
      parameter (rat21=108.815445356601320498479442732d0)
      double precision rat22
      parameter (rat22=48.3469235269643448690466611261d0)
      double precision rat23
      parameter (rat23=15.9157198794780527252915601943d0)
      double precision rat24
      parameter (rat24=3.66067321327616457996536852632d0)
      double precision rat25
      parameter (rat25=0.525413855664771873116657965754d0)
      double precision rat26
      parameter (rat26=0.0354362769986434339111067784862d0)
      double precision rat27
      parameter (rat27=-0.308875136389870708721789940641d0)
      double precision rat28
      parameter (rat28=0.222013219812024215257971698312d0)
      double precision rat29
      parameter (rat29=0.0115009836921949185031027379082d0)
      double precision rat30
      parameter (rat30=-0.000884691053245762961777133685244d0)
      double precision rat31
      parameter (rat31=0.00318488779168474666239768126688d0)
      double precision rat32
      parameter (rat32=-0.00482156624018940814168537858458d0)
      double precision rat33
      parameter (rat33=0.00563674585353728972789430890884d0)
      double precision rat34
      parameter (rat34=-0.22027972027972027972027972028d0)
      IF(x.GE.0d0.OR.x.LE.-1d0)THEN
        WRITE(*,*)"ERROR: x>=0 or x<=-1"
        STOP
      ENDIF
      y=-1d0-x
      lins(2)=DLOG(-y)+IPi
      lins(1)=DLOG(-x)+IPi
      res=        (1142756998*(1 + rat1*x + rat2*x**2 + 
     -       rat3*x**3 + rat4*x**4 + rat5*x**5 + 
     -       rat6*x**6 + rat5*x**7 + rat4*x**8 + 
     -       rat3*x**9 + rat2*x**10 + 
     -       rat1*x**11 + x**12))/
     -   (33075d0*x**7*y**5) + 
     -  (178464*(1 + rat7*x + rat8*x**2 + 
     -       rat9*x**3 + rat10*x**4 + 
     -       rat11*x**5 + rat12*x**6 + 
     -       rat11*x**7 + rat10*x**8 + 
     -       rat9*x**9 + rat8*x**10 + 
     -       rat7*x**11 + x**12)*zeta2)/
     -   (x**7*y**5) - 
     -  (7655464*(1 + rat13*x + rat14*x**2 + 
     -       rat15*x**3 + rat16*x**4 + 
     -       rat17*x**5 + rat18*x**6 + 
     -       rat19*x**7 + rat20*x**8 + 
     -       rat21*x**9 + rat22*x**10 + 
     -       rat23*x**11 + rat24*x**12 + 
     -       rat25*x**13 + rat26*x**14)*lins(1))/
     -   (45d0*x**7*y**6) - 
     -  (1898968*(-1 + rat27*x + rat28*x**2 + 
     -       rat29*x**3 + rat30*x**4 + 
     -       rat31*x**5 + rat32*x**6 + 
     -       rat33*x**7 + rat33*x**8 + 
     -       rat32*x**9 + rat31*x**10 + 
     -       rat30*x**11 + rat29*x**12 + 
     -       rat28*x**13 + rat27*x**14 - x**15)*
     -     lins(2))/(315d0*x**8*y**6) + 
     -  (4576*(-1 - (14*x)/13d0 + rat34*x**2)*
     -     lins(1)*lins(2))/(x**8*y**6)
      return
      end
