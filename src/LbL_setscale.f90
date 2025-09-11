MODULE LbL_setscale
  IMPLICIT NONE
CONTAINS
  SUBROUTINE LbL_scale(scale0)
    USE LbL_Global
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(OUT)::scale0
    REAL(KIND(1d0))::px,py,ptw2,mas
    REAL(KIND(1d0)),DIMENSION(0:3)::pmom
    INTEGER::ij
    SELECT CASE(scale)
    CASE(0)
       scale0=fscalevalue
    CASE(1)
       ! pT of photon
       ptw2=LbL_pmom(3,1)**2+LbL_pmom(3,2)**2
       scale0=DSQRT(ptw2)
    CASE(2)
       ! invariant mass of the photon
       DO ij=0,3
          pmom(ij)=LbL_pmom(3,ij)+LbL_pmom(4,ij)
       ENDDO
       ptw2=pmom(0)**2-pmom(1)**2-pmom(2)**2-pmom(3)**2
       scale0=DSQRT(MAX(ptw2,0d0))
    CASE DEFAULT
       WRITE(*,*)"ERROR: Do not understand the scale scheme=",scale
       STOP
    END SELECT
    !scale0=MAX(scale0,1d0)
    scale0=scale0*muR_over_ref
    RETURN
  END SUBROUTINE LbL_scale
END MODULE LbL_setscale
