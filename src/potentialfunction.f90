MODULE potentialfunction
  IMPLICIT NONE
CONTAINS
  ! Leading power potential function J=2*Im(G(0,0)) for ffbar
  ! aS: as value
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  SUBROUTINE Get_potentialJ_LP(aS,DC,E,mf,gf,res)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::aS,DC,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    IF(DC.LT.0d0)THEN
       CALL Get_ImG00CS(aS,DC,E,mf,gf,res)
    ELSE
       CALL Get_ImG00CO(aS,DC,E,mf,gf,res)
    ENDIF
    res=2d0*res
    RETURN
  END SUBROUTINE Get_potentialJ_LP

  ! Next-to-Leading power potential function J=2*Im(G(0,0)) for ffbar
  ! nq: number of massless quark flavors
  ! aS: as value in the Green function
  ! muC: scale for evaluating the Green function
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  SUBROUTINE Get_potentialJ_NLP(nq,aS,muC,DC,E,mf,gf,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nq
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    CALL Get_ImG00NLP(nq,aS,muC,DC,E,mf,gf,res)
    res=2d0*res
    RETURN
  END SUBROUTINE Get_potentialJ_NLP

  ! Next-to-Leading power potential function J=2*Im(G(0,0)) for ffbar
  ! expanded up to O(aS^2)
  ! nq: number of massless quark flavors
  ! aS: as value in the Green function
  ! muC: scale for evaluating the Green function
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  SUBROUTINE Get_potentialJ_NLP_aS2(nq,aS,muC,DC,E,mf,gf,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nq
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    CALL Get_ImG00NLP_aS2(nq,aS,muC,DC,E,mf,gf,res)
    res=2d0*res
    RETURN
  END SUBROUTINE Get_potentialJ_NLP_aS2
  
  ! Im(G(0,0)) for color-singlet ffbar
  ! aS: as value in the Green function
  ! D1: -CF=-4/3 for quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  ! state: this is for expression representation.
  ! 0 undefined sign of E (default); 1, E>0 (scattering state); -1, E<0 (bound state); 2, expression from eq.(3.27) of arXiv:1007.5414
  ! nmin to nmax Breit-Wigner terms when E<0. nmin >= 1 and nmax=-1 means infinite
  SUBROUTINE Get_ImG00CS(aS,D1,E,mf,gf,res,state,nmin,nmax)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::aS,D1,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    INTEGER,INTENT(IN),OPTIONAL::state,nmin,nmax
    REAL(KIND(1d0))::En,p1,Ep,Em,bwall,gf2
    INTEGER::state_used,nmin_used,nmax_used
    INTEGER::n
    COMPLEX(KIND(1d0)),EXTERNAL::polygamma
    COMPLEX(KIND(1d0))::z
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0))::arctanval
    REAL(KIND(1d0)),PARAMETER::tiny=1d-10
    IF(PRESENT(state))THEN
       state_used=state
    ELSE
       state_used=0
    ENDIF
    IF(state_used.NE.0.and.abs(state_used).NE.1.and.state_used.ne.2)THEN
       WRITE(*,*)"ERROR: state can only be 0,-1,1,2 !"
       STOP
    ENDIF
    IF(PRESENT(nmin))THEN
       nmin_used=nmin
    ELSE
       nmin_used=1
    ENDIF
    nmin_used=MAX(1,nmin_used)
    IF(PRESENT(nmax))THEN
       nmax_used=nmax
    ELSE
       nmax_used=-1 ! infinite
    ENDIF
    IF(nmax_used.NE.-1.and.nmax_used.LT.nmin_used)THEN
       WRITE(*,*)"ERROR: nmax < nmin !"
       STOP
    ENDIF
    IF(state_used.eq.2.and.(nmin_used.ne.1.or.nmax_used.ne.-1))THEN
       WRITE(*,*)"ERROR: state = 2 only nmin = 1 and nmax=-1 (inf) is available"
       STOP
    ENDIF
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    IF(D1.GE.0d0)THEN
       WRITE(*,*)"ERROR: D1 >=0 (not an attractive potential)"
       STOP
    ENDIF
    IF(state_used.eq.2)then
       ! use eq.(3.27) in arXiv:1007.5414
       if(gf.eq.0d0)then
          gf2=mf*tiny
       else
          gf2=gf
       endif
       z=1d0+D1*aS/(2d0*SQRT(dcmplx(-E/mf,-gf2/mf)))
       res=DIMAG(-mf**2/(4d0*pipi)*(SQRT(-dcmplx(E,gf2)/mf)&
            -D1*aS*(0.5d0*LOG(dcmplx(-E,-gf2))+polygamma(0,z))))
       return
    endif
    bwall=-99d99
    gf2=gf
    IF(state_used.eq.0.or.gf.ne.0d0)then
       Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
       Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
       if(nmax_used.eq.-1)then
          z=1d0+aS*D1*mf/2d0/dcmplx(Em,Ep)
          bwall=-2d0/(aS*D1)*DIMAG(polygamma(0,z))
       endif
    ELSEIF(gf.eq.0d0.and.state_used.eq.1)then
       IF(E.LT.0d0)THEN
          WRITE(*,*)"ERROR: E < 0 when state = 1"
          STOP
       ENDIF
       Ep=DSQRT(mf*E)
       Em=0d0
       p1=-0.5d0*D1*mf*aS
       IF(nmax_used.eq.-1)then
          bwall=mf/p1*(-Ep/(2d0*p1)+pipi/2d0+pipi/(DEXP(2d0*pipi*p1/Ep)-1))
       endif
    ELSEIF(gf.eq.0d0.and.state_used.eq.-1)then
       IF(E.GT.0d0)THEN
          WRITE(*,*)"ERROR: E > 0 when state = -1"
          STOP
       ENDIF
       ! use a very small gf to represent a dirac delta function
       gf2=mf*tiny
       Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf2**2)+E))
       Em=DSQRT(mf/2d0*(DSQRT(E**2+gf2**2)-E))
       if(nmax_used.eq.-1)then
          z=1d0+aS*D1*mf/2d0/dcmplx(Em,Ep)
          bwall=-2d0/(aS*D1)*DIMAG(polygamma(0,z))
       endif
    endif
    IF(Em.eq.0d0)THEN
       arctanval=pipi/2d0
    ELSE
       arctanval=DATAN(Ep/Em)
    ENDIF
    IF(bwall.NE.-99d99)THEN
       IF(nmin_used.GE.2)THEN
          DO n=1,nmin_used-1
             bwall=bwall-1d0/DBLE(n**4)*ImCSBS_bw_exp(n,aS,D1,E,gf2,mf)
          ENDDO
       ENDIF
       res=mf**2/(4d0*pipi)*(Ep/mf-D1*aS*arctanval+D1**2*aS**2/2d0*bwall)
    ELSE
       bwall=0d0
       DO n=nmin_used,nmax_used
          bwall=bwall+1d0/DBLE(n**4)*ImCSBS_bw_exp(n,aS,D1,E,gf2,mf)
       ENDDO
       res=mf**2/(4d0*pipi)*(Ep/mf-D1*aS*arctanval+D1**2*aS**2/2d0*bwall)
    ENDIF
    RETURN
  END SUBROUTINE Get_ImG00CS

  ! Re(G(0,0)) for color-singlet ffbar
  ! aS: as value in the Green function
  ! D1: -CF=-4/3 for quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  ! state: this is for expression representation.
  ! 0 undefined sign of E (default); 1, E>0 (scattering state); -1, E<0 (bound state); 2, expression from eq.(3.27) of arXiv:1007.5414
  ! nmin to nmax Breit-Wigner terms when E<0. nmin >= 1 and nmax=-1 means infinite
  SUBROUTINE Get_ReG00CS(aS,muC,D1,E,mf,gf,res,state,nmin,nmax)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,D1,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    INTEGER,INTENT(IN),OPTIONAL::state,nmin,nmax
    REAL(KIND(1d0))::En,p1,Ep,Em,bwall,gf2
    INTEGER::state_used,nmin_used,nmax_used
    INTEGER::n
    COMPLEX(KIND(1d0)),EXTERNAL::polygamma
    COMPLEX(KIND(1d0))::z
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::gammaE=0.577215664901532860606512090082d0
    REAL(KIND(1d0)),PARAMETER::tiny=1d-10
    REAL(KIND(1d0))::logv
    IF(PRESENT(state))THEN
       state_used=state
    ELSE
       state_used=0
    ENDIF
    IF(state_used.NE.0.and.abs(state_used).NE.1.and.state_used.ne.2)THEN
       WRITE(*,*)"ERROR: state can only be 0,-1,1,2 !"
       STOP
    ENDIF
    IF(PRESENT(nmin))THEN
       nmin_used=nmin
    ELSE
       nmin_used=1
    ENDIF
    nmin_used=MAX(1,nmin_used)
    IF(PRESENT(nmax))THEN
       nmax_used=nmax
    ELSE
       nmax_used=-1 ! infinite
    ENDIF
    IF(nmax_used.NE.-1.and.nmax_used.LT.nmin_used)THEN
       WRITE(*,*)"ERROR: nmax < nmin !"
       STOP
    ENDIF
    IF(state_used.eq.2.and.(nmin_used.ne.1.or.nmax_used.ne.-1))THEN
       WRITE(*,*)"ERROR: state = 2 only nmin = 1 and nmax=-1 (inf) is available"
       STOP
    ENDIF
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    IF(D1.GE.0d0)THEN
       WRITE(*,*)"ERROR: D1 >=0 (not an attractive potential)"
       STOP
    ENDIF
    IF(state_used.eq.2)then
       ! use eq.(3.27) in arXiv:1007.5414
       if(gf.eq.0d0)then
          gf2=mf*tiny
       else
          gf2=gf
       endif
       z=1d0+D1*aS/(2d0*SQRT(dcmplx(-E/mf,-gf2/mf)))
       res=DREAL(-mf**2/(4d0*pipi)*(SQRT(-dcmplx(E,gf2)/mf)&
            -D1*aS*(0.5d0*LOG(4d0*mf/muC**2*dcmplx(-E,-gf2))&
            -0.5d0+gammaE+polygamma(0,z))))
       return
    endif
    bwall=-99d99
    gf2=gf
    IF(state_used.eq.0.or.gf.ne.0d0)then
       Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
       Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
       logv=DLOG(4d0*mf*DSQRT(E**2+gf**2)/muC**2)
       if(nmax_used.eq.-1)then
          z=1d0+aS*D1*mf/2d0/dcmplx(Em,Ep)
          bwall=2d0/(aS*D1)*(gammaE+DREAL(polygamma(0,z)))
       endif
    ELSEIF(gf.eq.0d0.and.state_used.eq.1)then
       IF(E.LT.0d0)THEN
          WRITE(*,*)"ERROR: E < 0 when state = 1"
          STOP
       ENDIF
       Ep=DSQRT(mf*E)
       Em=0d0
       p1=-0.5d0*D1*mf*aS
       logv=DLOG(4d0*mf*DABS(E)/muC**2)
       IF(nmax_used.eq.-1)then
          z=1d0+aS*D1*mf/2d0/dcmplx(Em,Ep)
          bwall=2d0/(aS*D1)*(gammaE+DREAL(polygamma(0,z)))
       endif
    ELSEIF(gf.eq.0d0.and.state_used.eq.-1)then
       IF(E.GT.0d0)THEN
          WRITE(*,*)"ERROR: E > 0 when state = -1"
          STOP
       ENDIF
       Ep=0d0
       Em=DSQRT(-mf*E)
       logv=DLOG(4d0*mf*DABS(E)/muC**2)
       if(nmax_used.eq.-1)then
          z=1d0+aS*D1*mf/2d0/dcmplx(Em,Ep)
          bwall=2d0/(aS*D1)*(gammaE+DREAL(polygamma(0,z)))
       endif
    endif
    IF(bwall.NE.-99d99)THEN
       IF(nmin_used.GE.2)THEN
          DO n=1,nmin_used-1
             bwall=bwall-1d0/DBLE(n**4)*ReCSBS_bw_exp(n,aS,D1,E,gf2,mf)
          ENDDO
       ENDIF
       res=mf**2/(4d0*pipi)*(-Em/mf+D1*aS/2d0*(logv-1d0)+D1**2*aS**2/2d0*bwall)
    ELSE
       bwall=0d0
       DO n=nmin_used,nmax_used
          bwall=bwall+1d0/DBLE(n**4)*ReCSBS_bw_exp(n,aS,D1,E,gf2,mf)
       ENDDO
       res=mf**2/(4d0*pipi)*(-Em/mf+D1*aS/2d0*(logv-1d0)+D1**2*aS**2/2d0*bwall)
    ENDIF
    RETURN
  END SUBROUTINE Get_ReG00CS

  ! Im(G(0,0)) for color-octet ffbar
  ! aS: as value in the Green function
  ! D8: CA/2-CF=1/(2*Nc)=1/6 for quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  ! state: this is for expression representation.
  ! 0 undefined sign of E (default); 1, E>0 (scattering state); -1, E<0 (bound state); 2, expression from eq.(3.27) of arXiv:1007.5414
  ! nmin to nmax Breit-Wigner terms when E<0. nmin >= 1 and nmax=-1 means infinite
  SUBROUTINE Get_ImG00CO(aS,D8,E,mf,gf,res,state,nmin,nmax)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::aS,D8,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    INTEGER,INTENT(IN),OPTIONAL::state,nmin,nmax
    REAL(KIND(1d0))::p8,bwall,Ep,Em,gf2
    INTEGER::state_used,nmin_used,nmax_used
    INTEGER::n
    COMPLEX(KIND(1d0)),EXTERNAL::polygamma
    COMPLEX(KIND(1d0))::z
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0))::arctanval
    REAL(KIND(1d0)),PARAMETER::tiny=1d-10
    IF(PRESENT(state))THEN
       state_used=state
    ELSE
       state_used=0
    ENDIF
    IF(state_used.NE.0.and.abs(state_used).NE.1.and.state_used.ne.2)THEN
       WRITE(*,*)"ERROR: state can only be 0,-1,1,2 !"
       STOP
    ENDIF
    IF(PRESENT(nmin))THEN
       nmin_used=nmin
    ELSE
       nmin_used=1
    ENDIF
    nmin_used=MAX(1,nmin_used)
    IF(PRESENT(nmax))THEN
       nmax_used=nmax
    ELSE
       nmax_used=-1 ! infinite
    ENDIF
    IF(nmax_used.NE.-1.and.nmax_used.LT.nmin_used)THEN
       WRITE(*,*)"ERROR: nmax < nmin !"
       STOP
    ENDIF
    IF(state_used.eq.2.and.(nmin_used.ne.1.or.nmax_used.ne.-1))THEN
       WRITE(*,*)"ERROR: state = 2 only nmin = 1 and nmax=-1 (inf) is available"
       STOP
    ENDIF
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    IF(D8.LE.0d0)THEN
       WRITE(*,*)"ERROR: D8 <= 0 (not a repulsive potential)"
       STOP
    ENDIF
    IF(state_used.eq.2)then
       ! use eq.(3.27) in arXiv:1007.5414
       if(gf.eq.0d0)then
          gf2=mf*tiny
       else
          gf2=gf
       endif
       z=1d0+D8*aS/(2d0*SQRT(dcmplx(-E/mf,-gf2/mf)))
       res=DIMAG(-mf**2/(4d0*pipi)*(SQRT(-dcmplx(E,gf2)/mf)&
            -D8*aS*(0.5d0*LOG(dcmplx(-E,-gf2))+polygamma(0,z))))
       return
    endif
    bwall=-99d99
    p8=-0.5d0*D8*mf*aS
    IF(state_used.eq.0.or.gf.ne.0d0)then
       Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
       Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
       IF(nmax_used.EQ.-1)then
          z=1d0-p8/dcmplx(Em,Ep)
          bwall=mf/p8*DIMAG(polygamma(0,z))
       endif
    ELSEIF(gf.eq.0d0.and.state_used.eq.1)then
       IF(E.LT.0d0)THEN
          WRITE(*,*)"ERROR: E < 0 when state = 1"
          STOP
       ENDIF
       Ep=DSQRT(mf*E)
       Em=0d0
       IF(nmax_used.EQ.-1)then
          bwall=mf/p8*(-Ep/(2d0*p8)+pipi/2d0+pipi/(DEXP(2d0*pipi*p8/Ep)-1d0))
       endif
    ELSEIF(gf.eq.0d0.and.state_used.eq.-1)then
       IF(E.GT.0d0)THEN
          WRITE(*,*)"ERROR: E > 0 when state = -1"
          STOP
       ENDIF
       Ep=0d0
       Em=DSQRT(-mf*E)
       IF(nmax_used.EQ.-1)THEN
          bwall=0d0
       ENDIF
    endif
    IF(Em.eq.0d0)THEN
       arctanval=pipi/2d0
    ELSE
       arctanval=DATAN(Ep/Em)
    ENDIF
    IF(bwall.NE.-99d99)THEN
       IF(nmin_used.GE.2)THEN
          DO n=1,nmin_used-1
             bwall=bwall-ImCOBS_bw_exp(n,aS,D8,E,gf,mf)
          ENDDO
       ENDIF
       res=mf**2/(4d0*pipi)*(Ep/mf-D8*aS*arctanval+D8**2*aS**2/2d0*bwall)
    ELSE
       bwall=0d0
       DO n=nmin_used,nmax_used
          bwall=bwall+ImCOBS_bw_exp(n,aS,D8,E,gf,mf)
       ENDDO
       res=mf**2/(4d0*pipi)*(Ep/mf-D8*aS*arctanval+D8**2*aS**2/2d0*bwall)
    ENDIF
    RETURN
  END SUBROUTINE Get_ImG00CO

  ! Re(G(0,0)) for color-octet ffbar
  ! aS: as value in the Green function
  ! D8: CA/2-CF=1/(2*Nc)=1/6 for quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  ! state: this is for expression representation.
  ! 0 undefined sign of E (default); 1, E>0 (scattering state); -1, E<0 (bound state); 2, expression from eq.(3.27) of arXiv:1007.5414
  ! nmin to nmax Breit-Wigner terms when E<0. nmin >= 1 and nmax=-1 means infinite
  SUBROUTINE Get_ReG00CO(aS,muC,D8,E,mf,gf,res,state,nmin,nmax)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,D8,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    INTEGER,INTENT(IN),OPTIONAL::state,nmin,nmax
    REAL(KIND(1d0))::p8,bwall,Ep,Em,gf2
    INTEGER::state_used,nmin_used,nmax_used
    INTEGER::n
    COMPLEX(KIND(1d0)),EXTERNAL::polygamma
    COMPLEX(KIND(1d0))::z
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::gammaE=0.577215664901532860606512090082d0
    REAL(KIND(1d0))::logv
    REAL(KIND(1d0)),PARAMETER::tiny=1d-10
    IF(PRESENT(state))THEN
       state_used=state
    ELSE
       state_used=0
    ENDIF
    IF(state_used.NE.0.and.abs(state_used).NE.1.and.state_used.ne.2)THEN
       WRITE(*,*)"ERROR: state can only be 0,-1,1,2 !"
       STOP
    ENDIF
    IF(PRESENT(nmin))THEN
       nmin_used=nmin
    ELSE
       nmin_used=1
    ENDIF
    nmin_used=MAX(1,nmin_used)
    IF(PRESENT(nmax))THEN
       nmax_used=nmax
    ELSE
       nmax_used=-1 ! infinite
    ENDIF
    IF(nmax_used.NE.-1.and.nmax_used.LT.nmin_used)THEN
       WRITE(*,*)"ERROR: nmax < nmin !"
       STOP
    ENDIF
    IF(state_used.eq.2.and.(nmin_used.ne.1.or.nmax_used.ne.-1))THEN
       WRITE(*,*)"ERROR: state = 2 only nmin = 1 and nmax=-1 (inf) is available"
       STOP
    ENDIF
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    IF(D8.LE.0d0)THEN
       WRITE(*,*)"ERROR: D8 <= 0 (not a repulsive potential)"
       STOP
    ENDIF
    IF(state_used.eq.2)then
       ! use eq.(3.27) in arXiv:1007.5414
       if(gf.eq.0d0)then
          gf2=mf*tiny
       else
          gf2=gf
       endif
       z=1d0+D8*aS/(2d0*SQRT(dcmplx(-E/mf,-gf2/mf)))
       res=DREAL(-mf**2/(4d0*pipi)*(SQRT(-dcmplx(E,gf2)/mf)&
            -D8*aS*(0.5d0*LOG(4d0*mf/muC**2*dcmplx(-E,-gf2))&
            -0.5d0+gammaE+polygamma(0,z))))
       return
    endif
    bwall=-99d99
    p8=-0.5d0*D8*mf*aS
    IF(state_used.eq.0.or.gf.ne.0d0)then
       Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
       Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
       logv=DLOG(4d0*mf*DSQRT(E**2+gf**2)/muC**2)
       IF(nmax_used.EQ.-1)then
          z=1d0-p8/dcmplx(Em,Ep)
          bwall=2d0/(D8*aS)*(gammaE+DREAL(polygamma(0,z)))
       endif
    ELSEIF(gf.eq.0d0.and.state_used.eq.1)then
       IF(E.LT.0d0)THEN
          WRITE(*,*)"ERROR: E < 0 when state = 1"
          STOP
       ENDIF
       Ep=DSQRT(mf*E)
       Em=0d0
       logv=DLOG(4d0*mf*DABS(E)/muC**2)
       IF(nmax_used.EQ.-1)then
          z=1d0-p8/dcmplx(Em,Ep)
          bwall=2d0/(D8*aS)*(gammaE+DREAL(polygamma(0,z)))
       endif
    ELSEIF(gf.eq.0d0.and.state_used.eq.-1)then
       IF(E.GT.0d0)THEN
          WRITE(*,*)"ERROR: E > 0 when state = -1"
          STOP
       ENDIF
       Ep=0d0
       Em=DSQRT(-mf*E)
       logv=DLOG(4d0*mf*DABS(E)/muC**2)
       IF(nmax_used.EQ.-1)THEN
          z=1d0-p8/dcmplx(Em,Ep)
          bwall=2d0/(D8*aS)*(gammaE+DREAL(polygamma(0,z)))
       ENDIF
    endif
    IF(bwall.NE.-99d99)THEN
       IF(nmin_used.GE.2)THEN
          DO n=1,nmin_used-1
             bwall=bwall-ReCOBS_bw_exp(n,aS,D8,E,gf,mf)
          ENDDO
       ENDIF
       res=mf**2/(4d0*pipi)*(-Em/mf+D8*aS/2d0*(logv-1d0)+D8**2*aS**2/2d0*bwall)
    ELSE
       bwall=0d0
       DO n=nmin_used,nmax_used
          bwall=bwall+ReCOBS_bw_exp(n,aS,D8,E,gf,mf)
       ENDDO
       res=mf**2/(4d0*pipi)*(-Em/mf+D8*aS/2d0*(logv-1d0)+D8**2*aS**2/2d0*bwall)
    ENDIF
    RETURN
  END SUBROUTINE Get_ReG00CO

  ! G(0,0) for color-singlet/color-octet ffbar at LP
  ! aS: as value in the Green function
  ! DC: D1=-CF=-4/3 or D8=1/6 for quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  ! state: this is for expression representation.
  ! 0 undefined sign of E (default); 1, E>0 (scattering state); -1, E<0 (bound state); 2, expression from eq.(3.27) of arXiv:1007.5414
  ! nmin to nmax Breit-Wigner terms when E<0. nmin >= 1 and nmax=-1 means infinite
  SUBROUTINE Get_G00LP(aS,muC,DC,E,mf,gf,res,state,nmin,nmax)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    COMPLEX(KIND(1d0)),INTENT(OUT)::res
    REAL(KIND(1d0))::resR,resI
    INTEGER,INTENT(IN),OPTIONAL::state,nmin,nmax
    REAL(KIND(1d0))::En,p1,Ep,Em,gf2
    INTEGER::state_used,nmin_used,nmax_used
    COMPLEX(KIND(1d0)),EXTERNAL::polygamma
    COMPLEX(KIND(1d0))::z
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    REAL(KIND(1d0)),PARAMETER::gammaE=0.577215664901532860606512090082d0
    REAL(KIND(1d0)),PARAMETER::tiny=1d-10
    IF(PRESENT(state))THEN
       state_used=state
    ELSE
       state_used=0
    ENDIF
    IF(state_used.NE.0.and.abs(state_used).NE.1.and.state_used.ne.2)THEN
       WRITE(*,*)"ERROR: state can only be 0,-1,1,2 !"
       STOP
    ENDIF
    IF(PRESENT(nmin))THEN
       nmin_used=nmin
    ELSE
       nmin_used=1
    ENDIF
    nmin_used=MAX(1,nmin_used)
    IF(PRESENT(nmax))THEN
       nmax_used=nmax
    ELSE
       nmax_used=-1 ! infinite
    ENDIF
    IF(nmax_used.NE.-1.and.nmax_used.LT.nmin_used)THEN
       WRITE(*,*)"ERROR: nmax < nmin !"
       STOP
    ENDIF
    IF(state_used.eq.2.and.(nmin_used.ne.1.or.nmax_used.ne.-1))THEN
       WRITE(*,*)"ERROR: state = 2 only nmin = 1 and nmax=-1 (inf) is available"
       STOP
    ENDIF
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    IF(DC.EQ.0d0)THEN
       WRITE(*,*)"ERROR: DC = 0"
       STOP
    ENDIF
    IF(state_used.eq.2)then
       ! use eq.(3.27) in arXiv:1007.5414
       if(gf.eq.0d0)then
          gf2=mf*tiny
       else
          gf2=gf
       endif
       z=1d0+DC*aS/(2d0*SQRT(dcmplx(-E/mf,-gf2/mf)))
       res=-mf**2/(4d0*pipi)*(SQRT(-dcmplx(E,gf2)/mf)&
            -DC*aS*(0.5d0*LOG(4d0*mf/muC**2*dcmplx(-E,-gf2))&
            -0.5d0+gammaE+polygamma(0,z)))
    else
       IF(DC.LT.0d0)THEN
          ! color-singlet (attractive force)
          CALL Get_ReG00CS(aS,muC,DC,E,mf,gf,resR,state_used,nmin_used,nmax_used)
          CALL Get_ImG00CS(aS,DC,E,mf,gf,resI,state_used,nmin_used,nmax_used)
       ELSE
          ! color-octet (repulsive force)
          CALL Get_ReG00CO(aS,muC,DC,E,mf,gf,resR,state_used,nmin_used,nmax_used)
          CALL Get_ImG00CO(aS,DC,E,mf,gf,resI,state_used,nmin_used,nmax_used)
       ENDIF
       res=DCMPLX(resR,resI)
    endif
    return
  END SUBROUTINE Get_G00LP

  ! Im(G(0,0)) for ffbar at next-to-leading power (NLP)
  ! nq: number of massless quark flavors
  ! aS: as value in the Green function
  ! muC: scale for evaluating the Green function
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  ! using eq.(A.1) in arXiv:1109.15361
  SUBROUTINE Get_ImG00NLP(nq,aS,muC,DC,E,mf,gf,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nq
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    REAL(KIND(1d0))::a1,beta0,gf2
    REAL(KIND(1d0)),PARAMETER::tiny=1d-10
    COMPLEX(KIND(1d0))::cres,lam,omlam,CE,LE,j0,j1
    COMPLEX(KIND(1d0))::psihat,psi1,psi2
    ! EulerGamma
    REAL(KIND(1d0)),PARAMETER::gammaE=0.577215664901532860606512090082d0
    ! zeta(2)
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    ! pi
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    COMPLEX(KIND(1d0)),EXTERNAL::polygamma
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    if(gf.eq.0d0)then
       gf2=mf*tiny
    else
       gf2=gf
    endif
    CE=dcmplx(E,gf2)
    lam=-DC*aS/2d0/SQRT(-CE/mf)
    omlam=1d0-lam
    LE=-0.5d0*LOG(-4d0*mf*CE/muC**2)
    psihat=polygamma(0,omlam)+gammaE
    psi1=polygamma(1,omlam)
    psi2=polygamma(2,omlam)
    j0=lam*psi1-psihat
    j1=4d0*Hypergeometric_4F3(omlam)+lam*psi2&
         -2d0*lam*psi1*psihat-3d0*psi1+psihat**2-zeta2
    beta0=11d0-2d0/3d0*nq
    a1=31d0/3d0-10d0/9d0*nq
    cres=-mf**2/(16d0*pipi**2)*DC*aS**2*(a1*(LE+j0)&
         +beta0*(LE**2+2d0*j0*LE+j1))
    res=DIMAG(cres)
    RETURN
  END SUBROUTINE Get_ImG00NLP

  ! Re(G(0,0)) for ffbar at next-to-leading power (NLP)
  ! nq: number of massless quark flavors
  ! aS: as value in the Green function
  ! muC: scale for evaluating the Green function
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  ! using eq.(A.1) in arXiv:1109.15361
  SUBROUTINE Get_ReG00NLP(nq,aS,muC,DC,E,mf,gf,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nq
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    REAL(KIND(1d0))::a1,beta0,gf2
    REAL(KIND(1d0)),PARAMETER::tiny=1d-10
    COMPLEX(KIND(1d0))::cres,lam,omlam,CE,LE,j0,j1
    COMPLEX(KIND(1d0))::psihat,psi1,psi2
    ! EulerGamma
    REAL(KIND(1d0)),PARAMETER::gammaE=0.577215664901532860606512090082d0
    ! zeta(2)
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    ! pi
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    COMPLEX(KIND(1d0)),EXTERNAL::polygamma
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    if(gf.eq.0d0)then
       gf2=mf*tiny
    else
       gf2=gf
    endif
    CE=dcmplx(E,gf2)
    lam=-DC*aS/2d0/SQRT(-CE/mf)
    omlam=1d0-lam
    LE=-0.5d0*LOG(-4d0*mf*CE/muC**2)
    psihat=polygamma(0,omlam)+gammaE
    psi1=polygamma(1,omlam)
    psi2=polygamma(2,omlam)
    j0=lam*psi1-psihat
    j1=4d0*Hypergeometric_4F3(omlam)+lam*psi2&
         -2d0*lam*psi1*psihat-3d0*psi1+psihat**2-zeta2
    beta0=11d0-2d0/3d0*nq
    a1=31d0/3d0-10d0/9d0*nq
    cres=-mf**2/(16d0*pipi**2)*DC*aS**2*(a1*(LE+j0)&
         +beta0*(LE**2+2d0*j0*LE+j1))
    res=DREAL(cres)
    RETURN
  END SUBROUTINE Get_ReG00NLP

  ! G(0,0) for ffbar at next-to-leading power (NLP)
  ! nq: number of massless quark flavors
  ! aS: as value in the Green function
  ! muC: scale for evaluating the Green function
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  ! using eq.(A.1) in arXiv:1109.15361
  SUBROUTINE Get_G00NLP(nq,aS,muC,DC,E,mf,gf,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nq
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    COMPLEX(KIND(1d0)),INTENT(OUT)::res
    REAL(KIND(1d0))::a1,beta0,gf2
    REAL(KIND(1d0)),PARAMETER::tiny=1d-10
    COMPLEX(KIND(1d0))::cres,lam,omlam,CE,LE,j0,j1
    COMPLEX(KIND(1d0))::psihat,psi1,psi2
    ! EulerGamma
    REAL(KIND(1d0)),PARAMETER::gammaE=0.577215664901532860606512090082d0
    ! zeta(2)
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    ! pi
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    COMPLEX(KIND(1d0)),EXTERNAL::polygamma
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    if(gf.eq.0d0)then
       gf2=mf*tiny
    else
       gf2=gf
    endif
    CE=dcmplx(E,gf2)
    lam=-DC*aS/2d0/SQRT(-CE/mf)
    omlam=1d0-lam
    LE=-0.5d0*LOG(-4d0*mf*CE/muC**2)
    psihat=polygamma(0,omlam)+gammaE
    psi1=polygamma(1,omlam)
    psi2=polygamma(2,omlam)
    j0=lam*psi1-psihat
    j1=4d0*Hypergeometric_4F3(omlam)+lam*psi2&
         -2d0*lam*psi1*psihat-3d0*psi1+psihat**2-zeta2
    beta0=11d0-2d0/3d0*nq
    a1=31d0/3d0-10d0/9d0*nq
    cres=-mf**2/(16d0*pipi**2)*DC*aS**2*(a1*(LE+j0)&
         +beta0*(LE**2+2d0*j0*LE+j1))
    res=cres
    RETURN
  END SUBROUTINE Get_G00NLP

  ! Im(G(0,0)) for ffbar at next-to-leading power (NLP)
  ! expanded up to O(aS^2)
  ! nq: number of massless quark flavors
  ! aS: as value in the Green function
  ! muC: scale for evaluating the Green function
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  SUBROUTINE Get_ImG00NLP_aS2(nq,aS,muC,DC,E,mf,gf,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nq
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    REAL(KIND(1d0))::a1,beta0
    REAL(KIND(1d0))::Ep,Em,pref,logEv,arctanval
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    IF(gf.EQ.0d0.AND.E.LT.0d0)THEN
       res=0d0
       RETURN
    ENDIF
    beta0=11d0-2d0/3d0*nq
    a1=31d0/3d0-10d0/9d0*nq
    pref=-mf**2*DC*aS**2/(16d0*pipi**2)
    IF(gf.EQ.0d0)THEN
       logEv=DLOG(4d0*mf*E/muC**2)
       res=pref*pipi/2d0*(a1-beta0*LogEv)
    ELSE
       Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
       Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
       IF(Em.eq.0d0)THEN
          arctanval=pipi/2d0
       ELSE
          arctanval=DATAN(Ep/Em)
       ENDIF
       logEv=DLOG(4d0*(Ep**2+Em**2)/muC**2)
       res=pref*arctanval*(a1-beta0*logEv)
    ENDIF
    RETURN
  END SUBROUTINE Get_ImG00NLP_aS2

  ! Re(G(0,0)) for ffbar at next-to-leading power (NLP)
  ! expanded up to O(aS^2)
  ! nq: number of massless quark flavors
  ! aS: as value in the Green function
  ! muC: scale for evaluating the Green function
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  SUBROUTINE Get_ReG00NLP_aS2(nq,aS,muC,DC,E,mf,gf,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nq
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    REAL(KIND(1d0)),INTENT(OUT)::res
    REAL(KIND(1d0))::a1,beta0
    REAL(KIND(1d0))::Ep,Em,pref,logEv,arctanval
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    beta0=11d0-2d0/3d0*nq
    a1=31d0/3d0-10d0/9d0*nq
    pref=-mf**2*DC*aS**2/(16d0*pipi**2)
    IF(gf.EQ.0d0)THEN
       logEv=DLOG(4d0*mf*DABS(E)/muC**2)
       res=pref*(-a1/2d0*logEv+beta0/4d0*logEv**2)
       if(E.GT.0d0)THEN
          res=res-pref*pipi**2/4d0*beta0
       ENDIF
    ELSE
       Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
       Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
       IF(Em.eq.0d0)THEN
          arctanval=pipi/2d0
       ELSE
          arctanval=DATAN(Ep/Em)
       ENDIF
       logEv=DLOG(4d0*(Ep**2+Em**2)/muC**2)
       res=pref*(a1*(-0.5d0*logEv)+beta0*(0.25d0*logEv**2-arctanval**2))
    ENDIF
    RETURN
  END SUBROUTINE Get_ReG00NLP_aS2

  ! G(0,0) for ffbar at next-to-leading power (NLP)
  ! expanded up to O(aS^2)
  ! nq: number of massless quark flavors
  ! aS: as value in the Green function
  ! muC: scale for evaluating the Green function
  ! DC: -CF=-4/3 for color-singlet quark-antiquark
  !     CA/2-CF=1/(2*Nc)=1/6 for color-octet quark-antiquark
  ! E: binding energy E=Mff-2*mf
  ! mf: mass of the constitute fermion
  ! gf: width of the constitute fermion
  ! taking Gamma_ff=2*gf
  SUBROUTINE Get_G00NLP_aS2(nq,aS,muC,DC,E,mf,gf,res)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nq
    REAL(KIND(1d0)),INTENT(IN)::aS,muC,DC,E,mf,gf
    COMPLEX(KIND(1d0)),INTENT(OUT)::res
    COMPLEX(KIND(1d0))::CE
    REAL(KIND(1d0))::a1,beta0
    REAL(KIND(1d0))::pref
    COMPLEX(KIND(1d0))::logEv
    REAL(KIND(1d0)),PARAMETER::pipi=3.14159265358979323846264338328d0
    IF(mf.LE.0d0)THEN
       WRITE(*,*)"ERROR: mf <= 0"
       STOP
    ENDIF
    IF(gf.LT.0d0)THEN
       WRITE(*,*)"ERROR: gf < 0"
       STOP
    ENDIF
    beta0=11d0-2d0/3d0*nq
    a1=31d0/3d0-10d0/9d0*nq
    pref=-mf**2*DC*aS**2/(16d0*pipi**2)
    CE=DCMPLX(E,gf)
    IF(gf.EQ.0d0.and.E.GT.0d0)THEN
       logEv=DCMPLX(DLOG(4d0*mf*E/muC**2),-pipi)
    ELSE
       logEv=LOG(-4d0*mf*CE/muC**2)
    ENDIF
    res=pref*(-a1/2d0*logEv+beta0/4d0*logEv**2)
    RETURN
  END SUBROUTINE Get_G00NLP_aS2

  FUNCTION ImCSBS_bw_exp(n,aS,D1,E,gf,mf)
    IMPLICIT NONE
    REAL(KIND(1d0))::ImCSBS_bw_exp
    INTEGER,INTENT(IN)::n
    REAL(KIND(1d0)),INTENT(IN)::aS,D1,E,gf,mf
    REAL(KIND(1d0))::Ep,Em,En
    Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
    Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
    En=-mf*D1**2*aS**2/(4d0*DBLE(n**2))
    ImCSBS_bw_exp=(-n*gf*mf*D1*aS/2d0&
         +Ep*n**2*(DSQRT(E**2+gf**2)-En))&
         /((E-En)**2+gf**2)
    RETURN
  END FUNCTION ImCSBS_bw_exp

  FUNCTION ReCSBS_bw_exp(n,aS,D1,E,gf,mf)
    IMPLICIT NONE
    REAL(KIND(1d0))::ReCSBS_bw_exp
    INTEGER,INTENT(IN)::n
    REAL(KIND(1d0)),INTENT(IN)::aS,D1,E,gf,mf
    REAL(KIND(1d0))::Ep,Em,En,pn
    Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
    Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
    En=-mf*D1**2*aS**2/(4d0*DBLE(n**2))
    pn=-D1*mf*aS/(2d0*DBLE(n))
    ReCSBS_bw_exp=((Em-pn)*(-n*Em*D1*aS+n**2*(DSQRT(E**2+gf**2)-En)))&
         /((E-En)**2+gf**2)
    RETURN
  END FUNCTION ReCSBS_bw_exp

  FUNCTION CSBS_bw_exp(n,aS,D1,E,gf,mf)
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::CSBS_bw_exp
    INTEGER,INTENT(IN)::n
    REAL(KIND(1d0)),INTENT(IN)::aS,D1,E,gf,mf
    COMPLEX(KIND(1d0))::CE,sqrtv
    if(gf.EQ.0d0.and.E.GT.0d0)THEN
       sqrtv=DCMPLX(0d0,-DSQRT(E/mf))
    ELSE
       CE=DCMPLX(E,gf)
       sqrtv=SQRT(-CE/mf)
    ENDIF
    CSBS_bw_exp=1d0/(n*sqrtv+D1*aS/2d0)
    CSBS_bw_exp=CSBS_bw_exp*n**3
    RETURN
  END FUNCTION CSBS_bw_exp

  FUNCTION ImCOBS_bw_exp(n,aS,D8,E,gf,mf)
    IMPLICIT NONE
    REAL(KIND(1d0))::ImCOBS_bw_exp
    INTEGER,INTENT(IN)::n
    REAL(KIND(1d0)),INTENT(IN)::aS,D8,E,gf,mf
    REAL(KIND(1d0))::Ep,Em,p8
    Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
    Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
    p8=-0.5d0*D8*mf*aS
    ImCOBS_bw_exp=mf*Ep/((n*Em-p8)**2+n**2*Ep**2)
    RETURN
  END FUNCTION ImCOBS_bw_exp

  FUNCTION ReCOBS_bw_exp(n,aS,D8,E,gf,mf)
    IMPLICIT NONE
    REAL(KIND(1d0))::ReCOBS_bw_exp
    INTEGER,INTENT(IN)::n
    REAL(KIND(1d0)),INTENT(IN)::aS,D8,E,gf,mf
    REAL(KIND(1d0))::Ep,Em,p8,pn
    Ep=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)+E))
    Em=DSQRT(mf/2d0*(DSQRT(E**2+gf**2)-E))
    p8=-0.5d0*D8*mf*aS
    pn=p8/(DBLE(n))
    ReCOBS_bw_exp=mf*(Em-pn)/((n*Em-p8)**2+n**2*Ep**2)
    RETURN
  END FUNCTION ReCOBS_bw_exp

  FUNCTION COBS_bw_exp(n,aS,D8,E,gf,mf)
    IMPLICIT NONE
    COMPLEX(KIND(1d0))::COBS_bw_exp
    INTEGER,INTENT(IN)::n
    REAL(KIND(1d0)),INTENT(IN)::aS,D8,E,gf,mf
    COMPLEX(KIND(1d0))::CE,sqrtv
    if(gf.EQ.0d0.and.E.GT.0d0)THEN
       sqrtv=DCMPLX(0d0,-DSQRT(E/mf))
    ELSE
       CE=DCMPLX(E,gf)
       sqrtv=SQRT(-CE/mf)
    ENDIF
    COBS_bw_exp=1d0/(n*sqrtv+D8*aS/2d0)
    COBS_bw_exp=COBS_bw_exp/DBLE(n)
    RETURN
  END FUNCTION COBS_bw_exp

  ! 4F3(1,1,1,1;2,2,N;1)
  FUNCTION Hypergeometric_4F3(N)
    USE HarmonicSums
    IMPLICIT NONE
    COMPLEX(KIND(1d0)),INTENT(IN)::N
    COMPLEX(KIND(1d0))::Hypergeometric_4F3
    COMPLEX(KIND(1d0))::lam
    REAL(KIND(1d0)),PARAMETER::zeta2=1.64493406684822643647241516665d0
    REAL(KIND(1d0)),PARAMETER::zeta3=1.20205690315959428539973816151d0
    COMPLEX(KIND(1d0))::SS1,SS2,SS3,SS21
    lam=1d0-N
    ! eq.(A.10) in arXiv:1109.1536
    SS1=Harmonic_Sum_S(1,-lam)
    SS2=Harmonic_Sum_S(2,-lam)
    SS3=Harmonic_Sum_S(3,-lam)
    SS21=Harmonic_Sum_S21(-lam)
    Hypergeometric_4F3=zeta2-SS2-lam*(zeta3+SS3-SS1*(zeta2-SS2)-SS21)
    RETURN
  END FUNCTION Hypergeometric_4F3
END MODULE potentialfunction
