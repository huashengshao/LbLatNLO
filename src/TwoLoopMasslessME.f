      ! get the form factors from helicity amplitudes
      ! for massless two-loop case
      subroutine Get_TwoLoop_FormFactors_Massless(shat,that
     $     ,uhat,FFs)
      implicit none
      double precision shat,that,uhat
      double complex FFs(5)
      double complex amp(5)
      integer ihel
      double complex TwoLoop_HelAmp_Massless
      external TwoLoop_HelAmp_Massless
      do ihel=1,5
         amp(ihel)=TwoLoop_HelAmp_Massless(ihel,shat,that,uhat)
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

      ! helicity amplitude from hep-ph/0109079
      ! eqs.(2.6,A.1-A.4)
      double complex function TwoLoop_HelAmp_Massless(ihel,shat,that,
     $     uhat)
      use nielsen_generalized_polylog_wrapper
      implicit none
      ! 1: --++,++--
      ! 2: -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
      ! 3: ++++,----
      ! 4: +--+,-++-
      ! 5: +-+-,-+-+
      integer ihel
      double precision shat,that,uhat
      double precision x,y,XX,YY
      double precision pipi,pi2,pi4,zeta3,zeta4
      parameter (pipi=3.14159265358979323846264338328d0)
      parameter (pi2=9.86960440108935861883449099988d0)
      parameter (pi4=97.4090910340024372364403326887d0)
      parameter (zeta3=1.20205690315959428539973816151d0)
      parameter (zeta4=1.08232323371113819151600369654d0)
      double precision li4mx,li4my,li3mx,li3my,li2mx,li2my
      double precision li4mxoy,li4myox,li3mxoy,li3myox
      double precision li2mxoy,li2myox
      x=that/shat
      y=uhat/shat
      XX=dlog(-x)
      YY=dlog(-y)
      select case(ihel)
      case(1)
         ! --++,++--
         TwoLoop_HelAmp_Massless=dcmplx(-1.5d0,0d0)
      case(2)
         ! -+++,+-++,++-+,+++-,+---,-+--,--+-,---+
         TwoLoop_HelAmp_Massless=(x**2+1d0)/y**2*(dcmplx(XX,pipi)**2
     $        +pi2)+0.5d0*(x**2+y**2)*((XX-YY)**2+pi2)
     $        -4d0*(1d0/y-x)*dcmplx(XX,pipi)
         TwoLoop_HelAmp_Massless=TwoLoop_HelAmp_Massless+
     $        (y**2+1d0)/x**2*(dcmplx(YY,pipi)**2
     $        +pi2)+0.5d0*(y**2+x**2)*((YY-XX)**2+pi2)
     $        -4d0*(1d0/x-y)*dcmplx(YY,pipi)
         TwoLoop_HelAmp_Massless=1d0/8d0*TwoLoop_HelAmp_Massless
      case(3)
         ! ++++,----
         li4mx=Li4(-x)
         li4my=Li4(-y)
         li4mxoy=Li4(-x/y)
         li4myox=Li4(-y/x)
         li3mx=Li3(-x)
         li3my=Li3(-y)
         li3mxoy=Li3(-x/y)
         li3myox=Li3(-y/x)
         li2mx=Li2_S11(-x)
         li2my=Li2_S11(-y)
         li2mxoy=Li2_S11(-x/y)
         li2myox=Li2_S11(-y/x)
         TwoLoop_HelAmp_Massless=-0.25d0+pi2/12d0-
     -        (li4mxoy-(li2mx*pi2)/6d0)*(x-y)-((3d0+2d0*x*y)*
     -        (pi2+(XX-YY)**2))/8d0+(x+1d0/(2d0*y))*
     -        dcmplx(XX,pipi)-2d0*x**2*(li4mx+li4my-
     -        (2d0*pi4)/45d0+XX**4/12d0+(pi2*XX*YY)/12d0-
     -      (XX**3*YY)/3d0+dcmplx(0d0,1d0/6d0)*pipi*XX*(pi2+XX**2
     -      -3d0*XX*YY)-(li3mx+li3my)*dcmplx(XX,pipi))+((1d0-2d0*x**2)*
     -        (pi2+dcmplx(XX,pipi)**2))/(4d0*y**2)-x*(2d0*li3mx-li3mxoy- 
     -      (2d0*XX*(pi2+XX**2))/3d0+(li2mxoy+XX**2)*(XX-YY)-3d0*zeta3+ 
     -        dcmplx(0d0,-(pipi*(pi2+YY**2)))-2d0*li2mx*
     -        dcmplx(XX,pipi)+((pi2+(XX-YY)**2)*dcmplx(5d0*(XX-YY),
     -        18d0*pipi))/12d0)
         TwoLoop_HelAmp_Massless=TwoLoop_HelAmp_Massless+
     $        -0.25d0+pi2/12d0-
     -        (li4myox-(li2my*pi2)/6d0)*(y-x)-((3d0+2d0*x*y)*
     -        (pi2+(YY-XX)**2))/8d0+(y+1d0/(2d0*x))*
     -        dcmplx(YY,pipi)-2d0*y**2*(li4my+li4mx-
     -        (2d0*pi4)/45d0+YY**4/12d0+(pi2*XX*YY)/12d0-
     -      (YY**3*XX)/3d0+dcmplx(0d0,1d0/6d0)*pipi*YY*(pi2+YY**2
     -      -3d0*YY*XX)-(li3my+li3mx)*dcmplx(YY,pipi))+((1d0-2d0*y**2)*
     -        (pi2+dcmplx(YY,pipi)**2))/(4d0*x**2)-y*(2d0*li3my-li3myox-
     -      (2d0*YY*(pi2+YY**2))/3d0+(li2myox+YY**2)*(YY-XX)-3d0*zeta3+
     -        dcmplx(0d0,-(pipi*(pi2+XX**2)))-2d0*li2my*
     -        dcmplx(YY,pipi)+((pi2+(YY-XX)**2)*dcmplx(5d0*(YY-XX),
     -        18d0*pipi))/12d0)
      case(4)
         ! +--+,-++-
         li4mx=Li4(-x)
         li4my=Li4(-y)
         li4mxoy=Li4(-x/y)
         li3mx=Li3(-x)
         li3my=Li3(-y)
         li2mx=Li2_S11(-x)
         TwoLoop_HelAmp_Massless=-0.5d0+pi2/6d0-((2d0*x**2-y**2)
     -        *(pi2+(XX-YY)**2))/4d0-(2d0*(1d0+x**2)*(li4mxoy-li4my+
     -        (7d0*pi4)/360d0+(dcmplx(0d0,2d0)*pipi*XX**3+XX**4
     -        +2d0*pi2*YY**2-4d0*XX*YY**3+YY**4)/24d0+ 
     -        ((dcmplx(0d0,-pipi)+XX-2d0*YY)*(li3mx-zeta3))/2d0))/y**2+
     -        2d0*(1d0+(2d0*x)/y)*(li3my-(XX**2*(dcmplx(XX,3d0*pipi)))
     -        /8d0+(XX*(pi2+2d0*YY**2))/4d0-zeta3+li2mx*(YY
     -        +dcmplx(0d0,pipi)))-((2d0-y**2)*(pi2+(YY
     -        +dcmplx(0d0,pipi))**2))/(4d0*x**2)+ 
     -        (-1d0+(2d0*x)/y)*(li3mx-XX**3/6d0-(pi2*(XX+YY))/3d0
     -        +zeta3-li2mx*dcmplx(XX,pipi))+((2d0*x+y**2)*
     -        ((YY+dcmplx(0d0,pipi))/x+dcmplx(XX,pipi)/y))/2d0- 
     -        (2d0*(-1d0+x)*(li4mx-XX**4/48d0+(pi2*(li2mx-pi2/6d0
     -        -XX**2/2d0))/6d0-zeta4-((li3mx-zeta3)
     -        *dcmplx(XX,pipi))/2d0))/y- 
     -        ((3d0+(2d0*x)/y**2)*(pi2+dcmplx(XX,pipi)**2))/4d0
      case(5)
         ! +-+-,-+-+
         ! t<->u from ihel=4 (see also eq.(2.12) in hep-ph/0109079)
         li4mx=Li4(-x)
         li4my=Li4(-y)
         li4myox=Li4(-y/x)
         li3mx=Li3(-x)
         li3my=Li3(-y)
         li2my=Li2_S11(-y)
         TwoLoop_HelAmp_Massless=-0.5d0+pi2/6d0-((2d0*y**2-x**2)
     -        *(pi2+(YY-XX)**2))/4d0-(2d0*(1d0+y**2)*(li4myox-li4mx+
     -        (7d0*pi4)/360d0+(dcmplx(0d0,2d0)*pipi*YY**3+YY**4
     -        +2d0*pi2*XX**2-4d0*YY*XX**3+XX**4)/24d0+
     -        ((dcmplx(0d0,-pipi)+YY-2d0*XX)*(li3my-zeta3))/2d0))/x**2+
     -        2d0*(1d0+(2d0*y)/x)*(li3mx-(YY**2*(dcmplx(YY,3d0*pipi)))
     -        /8d0+(YY*(pi2+2d0*XX**2))/4d0-zeta3+li2my*(XX
     -        +dcmplx(0d0,pipi)))-((2d0-x**2)*(pi2+(XX
     -        +dcmplx(0d0,pipi))**2))/(4d0*y**2)+
     -        (-1d0+(2d0*y)/x)*(li3my-YY**3/6d0-(pi2*(YY+XX))/3d0
     -        +zeta3-li2my*dcmplx(YY,pipi))+((2d0*y+x**2)*
     -        ((XX+dcmplx(0d0,pipi))/y+dcmplx(YY,pipi)/x))/2d0-
     -        (2d0*(-1d0+y)*(li4my-YY**4/48d0+(pi2*(li2my-pi2/6d0
     -        -YY**2/2d0))/6d0-zeta4-((li3my-zeta3)
     -        *dcmplx(YY,pipi))/2d0))/x-
     -        ((3d0+(2d0*y)/x**2)*(pi2+dcmplx(YY,pipi)**2))/4d0
      case default
         write(*,*)"ERROR: unknown helicity configuration #2 =",ihel
         stop
      end select
      return
      end
