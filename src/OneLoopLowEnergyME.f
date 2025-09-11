      ! We need to multiply I*Nc*Qf^4*a^2

      ! get the form factors from helicity amplitudes
      ! for low-energy expansion one-loop case of fermion loop
      subroutine Get_OneLoop_FormFactors_LE(iterm,m2,shat,that,uhat,FFs)
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
      double complex OneLoop_HelAmp_LE
      external OneLoop_HelAmp_LE
      xs=shat/m2
      xt=that/m2
      xu=uhat/m2
      do ihel=1,5
         amp(ihel)=OneLoop_HelAmp_LE(ihel,iterm,xs,xt,xu)
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

      double complex function OneLoop_HelAmp_LE(ihel,iterm,xs,xt,xu)
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
      double complex OneLoop_HelAmp_LE_pppp
      double complex OneLoop_HelAmp_LE_mppp
      double complex OneLoop_HelAmp_LE_mmpp
      double complex OneLoop_HelAmp_LE_pmpm
      double complex OneLoop_HelAmp_LE_pmmp
      external OneLoop_HelAmp_LE_pppp
      external OneLoop_HelAmp_LE_mppp
      external OneLoop_HelAmp_LE_mmpp
      external OneLoop_HelAmp_LE_pmpm
      external OneLoop_HelAmp_LE_pmmp
      integer iterm_used
      SELECT CASE(ihel)
      CASE(1)
         ! --++,++--
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_LE=OneLoop_HelAmp_LE_pppp(iterm_used,xs,xt,xu)
      CASE(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_LE=OneLoop_HelAmp_LE_mppp(iterm_used,xs,xt,xu)
      CASE(3)
         ! ++++,----
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_LE=OneLoop_HelAmp_LE_mmpp(iterm_used,xs,xt,xu)
      CASE(4)
         ! +--+,-++-
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_LE=OneLoop_HelAmp_LE_pmpm(iterm_used,xs,xt,xu)
      CASE(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoop_HelAmp_LE=OneLoop_HelAmp_LE_pmmp(iterm_used,xs,xt,xu)
      CASE DEFAULT
         write(*,*)"ERROR: unknown helicity configuration #7 =",ihel
         stop
      end select
      return
      end

      ! ++++ (4 -> 0)
      ! --++,++-- (2 -> 2)
      function OneLoop_HelAmp_LE_pppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 5
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double precision squaresum,allproduct
      IF(iterm.LT.0.OR.iterm.GT.5)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>5"
         STOP
      ENDIF
      squaresum=xs**2+xt**2+xu**2
      res=dcmplx(-1d0/15d0*squaresum,0d0)
      IF(iterm.LE.0)return
      allproduct=xs*xt*xu
      res=res-2d0/63d0*allproduct
      IF(iterm.LE.1)return
      res=res-1d0/945d0*squaresum**2
      IF(iterm.LE.2)return
      res=res-1d0/990d0*allproduct*squaresum
      IF(iterm.LE.3)return
      res=res-(109d0/450450d0*allproduct**2+1d0/48048d0*squaresum**3)
      IF(iterm.LE.4)return
      res=res-3d0/100100d0*allproduct*squaresum**2
      IF(iterm.LE.5)return
      WRITE(*,*)"ERROR: cannot reach here (++++)"
      STOP
      return
      end

      ! -+++ (4 -> 0)
      ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+ (2 -> 2)
      function OneLoop_HelAmp_LE_mppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 5
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double precision squaresum,allproduct
      IF(iterm.LT.0.OR.iterm.GT.5)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>5"
         STOP
      ENDIF
      res=dcmplx(0d0,0d0)
      IF(iterm.LE.0)return
      allproduct=xs*xt*xu
      res=res-1d0/315d0*allproduct
      IF(iterm.LE.2)return
      squaresum=xs**2+xt**2+xu**2
      res=res-1d0/41580d0*allproduct*squaresum
      IF(iterm.LE.3)return
      res=res-2d0/225225d0*allproduct**2
      IF(iterm.LE.4)return
      res=res-1d0/3603600d0*allproduct*squaresum**2
      IF(iterm.LE.5)return
      WRITE(*,*)"ERROR: cannot reach here (-+++)"
      STOP
      return
      end

      ! --++ (4 -> 0)
      ! ++++,---- (2 -> 2)
      function OneLoop_HelAmp_LE_mmpp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 5
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double precision squaresum
      IF(iterm.LT.0.OR.iterm.GT.5)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>5"
         STOP
      ENDIF
      res=dcmplx(11d0/45d0*xs**2,0d0)
      IF(iterm.LE.0)return
      res=res+4d0/315d0*xs**3
      IF(iterm.LE.1)return
      res=res+xs**2/18900d0*(91d0*xs**2-50d0*xt*xu)
      IF(iterm.LE.2)return
      res=res+1d0/103950d0*xs**3*(48d0*xs**2+49d0*xt*xu)
      IF(iterm.LE.3)return
      res=res+xs**2*(19d0/210210d0*xs**4
     $     +73d0/772200d0*xs**2*(xt**2+xu**2)
     $     +19d0/300300d0*xs*(xt**3+xu**3)
     $     +19d0/600600d0*(xt**4+xu**4))
      IF(iterm.LE.4)return
      res=res+xs**3*(19d0/600600d0*xs**4
     $     -967d0/37837800d0*xs**2*(xt**2+xu**2)
     $     -1d0/38610d0*xs*(xt**3+xu**3)
     $     -1d0/77220d0*(xt**4+xu**4))
      IF(iterm.LE.5)return
      WRITE(*,*)"ERROR: cannot reach here (--++)"
      STOP
      return
      end

      ! +-+- (4 -> 0)
      ! +--+,-++- (2 -> 2)
      function OneLoop_HelAmp_LE_pmpm(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 5
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double complex OneLoop_HelAmp_LE_mmpp
      external OneLoop_HelAmp_LE_mmpp
      res=OneLoop_HelAmp_LE_mmpp(iterm,xu,xt,xs)
      return
      end

      ! +--+ (4 -> 0)
      ! +-+-,-+-+ (2 -> 2)
      function OneLoop_HelAmp_LE_pmmp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm  ! iterm from 0 to 5
                     ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double complex OneLoop_HelAmp_LE_mmpp
      external OneLoop_HelAmp_LE_mmpp
      res=OneLoop_HelAmp_LE_mmpp(iterm,xt,xs,xu)
      return
      end


      ! The following is for W-boson loop
      ! We need to multiply I*(-12)*a^2

      ! get the form factors from helicity amplitudes
      ! for low-energy expansion one-loop case of W-boson loop
      subroutine Get_OneLoopW_FormFactors_LE(iterm,m2,shat,that,uhat,FFs)
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
      double complex OneLoopW_HelAmp_LE
      external OneLoopW_HelAmp_LE
      xs=shat/m2
      xt=that/m2
      xu=uhat/m2
      do ihel=1,5
         amp(ihel)=OneLoopW_HelAmp_LE(ihel,iterm,xs,xt,xu)
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

      double complex function OneLoopW_HelAmp_LE(ihel,iterm,xs,xt,xu)
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
      double complex OneLoopW_HelAmp_LE_pppp
      double complex OneLoopW_HelAmp_LE_mppp
      double complex OneLoopW_HelAmp_LE_mmpp
      double complex OneLoopW_HelAmp_LE_pmpm
      double complex OneLoopW_HelAmp_LE_pmmp
      external OneLoopW_HelAmp_LE_pppp
      external OneLoopW_HelAmp_LE_mppp
      external OneLoopW_HelAmp_LE_mmpp
      external OneLoopW_HelAmp_LE_pmpm
      external OneLoopW_HelAmp_LE_pmmp
      integer iterm_used
      SELECT CASE(ihel)
      CASE(1)
         ! --++,++--
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_LE=OneLoopW_HelAmp_LE_pppp(iterm_used,xs,xt,xu)
      CASE(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_LE=OneLoopW_HelAmp_LE_mppp(iterm_used,xs,xt,xu)
      CASE(3)
         ! ++++,----
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_LE=OneLoopW_HelAmp_LE_mmpp(iterm_used,xs,xt,xu)
      CASE(4)
         ! +--+,-++-
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_LE=OneLoopW_HelAmp_LE_pmpm(iterm_used,xs,xt,xu)
      CASE(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4
         if(iterm.EQ.-1)then
            iterm_used=5
         else
            iterm_used=iterm
         endif
         OneLoopW_HelAmp_LE=OneLoopW_HelAmp_LE_pmmp(iterm_used,xs,xt,xu)
      CASE DEFAULT
         write(*,*)"ERROR: unknown helicity configuration #8 =",ihel
         stop
      end select
      return
      end

      ! ++++ (4 -> 0)
      ! --++,++-- (2 -> 2)
      function OneLoopW_HelAmp_LE_pppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 5
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double complex OneLoop_HelAmp_LE_pppp
      external OneLoop_HelAmp_LE_pppp
      res=OneLoop_HelAmp_LE_pppp(iterm,xs,xt,xu)
      res=res/8d0
      return
      end

      ! -+++ (4 -> 0)
      ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+ (2 -> 2)
      function OneLoopW_HelAmp_LE_mppp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 5
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double complex OneLoop_HelAmp_LE_mppp
      external OneLoop_HelAmp_LE_mppp
      res=OneLoop_HelAmp_LE_mppp(iterm,xs,xt,xu)
      res=res/8d0
      return
      end

      ! --++ (4 -> 0)
      ! ++++,---- (2 -> 2)
      function OneLoopW_HelAmp_LE_mmpp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 5
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double precision squaresum
      IF(iterm.LT.0.OR.iterm.GT.5)THEN
         WRITE(*,*)"ERROR: iterm<0 or iterm>5"
         STOP
      ENDIF
      res=dcmplx(-7d0/30d0*xs**2,0d0)
      IF(iterm.LE.0)return
      res=res+47d0/7560d0*xs**3
      IF(iterm.LE.1)return
      res=res-xs**2/75600d0*(262d0*xs**2-345d0*xt*xu)
      IF(iterm.LE.2)return
      res=res+1d0/1247400d0*xs**3*(292d0*xs**2-1439d0*xt*xu)
      IF(iterm.LE.3)return
      res=res-xs**2/12d0*(-4211d0/7567560d0*xs**4
     $     +79d0/28600d0*xs**2*(xt**2+xu**2)
     $     +1244d0/675675d0*xs*(xt**3+xu**3)
     $     +622d0/675675d0*(xt**4+xu**4))
      IF(iterm.LE.4)return
      res=res+xs**3/12d0*(-1403d0/4204200d0*xs**4
     $     +947d0/1051050d0*xs**2*(xt**2+xu**2)
     $     +9d0/10010d0*xs*(xt**3+xu**3)
     $     +9d0/20020d0*(xt**4+xu**4))
      IF(iterm.LE.5)return
      WRITE(*,*)"ERROR: cannot reach here (--++)"
      STOP
      return
      end

      ! +-+- (4 -> 0)
      ! +--+,-++- (2 -> 2)
      function OneLoopW_HelAmp_LE_pmpm(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm ! iterm from 0 to 5
                    ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double complex OneLoopW_HelAmp_LE_mmpp
      external OneLoopW_HelAmp_LE_mmpp
      res=OneLoopW_HelAmp_LE_mmpp(iterm,xu,xt,xs)
      return
      end

      ! +--+ (4 -> 0)
      ! +-+-,-+-+ (2 -> 2)
      function OneLoopW_HelAmp_LE_pmmp(iterm,xs,xt,xu) RESULT(res)
      implicit none
      double complex res
      integer iterm  ! iterm from 0 to 5
                     ! iterm means add expansion terms up to (xs)^(2+iterm)
      double precision xs,xt,xu
      double complex OneLoopW_HelAmp_LE_mmpp
      external OneLoopW_HelAmp_LE_mmpp
      res=OneLoopW_HelAmp_LE_mmpp(iterm,xt,xs,xu)
      return
      end
