MODULE LbL_cuts
  IMPLICIT NONE
CONTAINS
  SUBROUTINE Cuts_LbL(icut)
    USE LbL_Global
    USE kinetics
    IMPLICIT NONE
    LOGICAL,INTENT(OUT)::icut ! True (pass cuts); False (not pass cuts)
    INTEGER::i
    REAL(KIND(1d0))::pt,yrap,maa,dphi
    REAL(KIND(1d0)),DIMENSION(0:3)::paa
    REAL(KIND(1d0)),PARAMETER::PI=3.14159265358979312D0
    icut=.TRUE.
    do i=3,4
       pt=DSQRT(LbL_pmom(i,1)**2+LbL_pmom(i,2)**2)
       if(pt.LT.minptp)then
          icut=.FALSE.
          return
       endif
       if(maxptp.GE.0d0.and.pt.GT.maxptp)then
          icut=.FALSE.
          return
       endif
       yrap=rapidity(LbL_pmom(i,0:3))
       if(yrap.LT.minyrapp)then
          icut=.FALSE.
          return
       endif
       if(yrap.GT.maxyrapp)then
          icut=.FALSE.
          return
       endif
    enddo
    paa(0:3)=LbL_pmom(3,0:3)+LbL_pmom(4,0:3)
    maa=paa(0)**2-paa(1)**2-paa(2)**2-paa(3)**2
    if(maa.LT.0d0)then
       maa=0d0
    else
       maa=dsqrt(maa)
    endif
    if(maa.LT.minmpp)then
       icut=.FALSE.
       return
    endif
    if(maxmpp.GE.0d0.and.maa.GT.maxmpp)then
       icut=.FALSE.
       return
    endif
    ! cuts on pt of the pair
    if(maxptpp.GT.0d0)then
       pt=dsqrt(paa(1)**2+paa(2)**2)
       if(pt.gt.maxptpp)then
          icut=.FALSE.
          return
       endif
    endif
    ! cuts on acoplanarity of the photon pair
    if(maxaco.GT.0d0)then
       dphi=ph4(LbL_pmom(3,1),LbL_pmom(3,2),LbL_pmom(3,3))
       dphi=dphi-ph4(LbL_pmom(4,1),LbL_pmom(4,2),LbL_pmom(4,3))
       dphi=DABS(dphi)
       IF(dphi.GT.pi)dphi=2d0*pi-dphi
       dphi=1d0-dphi/pi ! acoplanarity
       if(dphi.gt.maxaco)then
          icut=.FALSE.
          return
       endif
    endif
    ! start the user's cuts
    RETURN
  END SUBROUTINE Cuts_LbL
END MODULE LbL_cuts
