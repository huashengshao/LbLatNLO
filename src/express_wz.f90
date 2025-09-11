MODULE express_wz
  IMPLICIT NONE
CONTAINS
  subroutine evaluate_wz(xsb,xtb,w,z)
    implicit none
    REAL(KIND(1d0)),INTENT(IN)::xsb,xtb
    COMPLEX(KIND(1d0)),INTENT(OUT)::w,z
    ! above xinterp, we use the interpolation if it is possible
    REAL(KIND(1d0)),PARAMETER::xinterp=50d0
    REAL(KIND(1d0)),PARAMETER::xmax=6.3095734448019d7
    IF(xsb.EQ.0d0.OR.xtb.EQ.0d0.OR.xsb+xtb.EQ.0d0)THEN
       ! xsb or xtb or xsb+xtb either is zero
       ! we randomly choose a point for w=z
       w=0.5d0
       z=0.5d0
       RETURN
    ENDIF
    if(xsb.LT.0d0.and.xtb.lt.0d0.and.-xsb.GE.1d-2&
         .and.-xtb.GE.1d-2.and.-xsb.LE.xmax.and.-xtb.LE.xmax.and.&
         (abs(xsb).GT.1000d0.or.abs(xtb).GT.1000d0))then
       call evaluate_wz_interp(xsb,xtb,w,z)
    elseif(xsb.GT.10d0.and.xtb.lt.0d0.and.xsb.LT.1d8.and.-xtb/xsb.GE.1d-6&
         .and.-xtb/xsb.LE.0.9999539493585d0.and.(xsb.GT.500d0.or.&
         (xsb.GT.200d0.and.-xtb/xsb.GT.0.9995d0)))then
       call evaluate_wz_interp(xsb,xtb,w,z)
    elseif(xtb.GT.10d0.and.xsb.lt.0d0.and.xtb.LT.1d8.and.-xsb/xtb.GE.1d-6&
         .and.-xsb/xtb.LE.0.9999539493585d0.and.(xtb.GT.100d0.or.-xsb/xtb.LT.5d-4))then
       call evaluate_wz_interp(xsb,xtb,w,z)
    else
       call evaluate_wz_direct(xsb,xtb,w,z)
    endif
    return
  end subroutine evaluate_wz

  subroutine evaluate_wz_interp(xsb,xtb,w,z)
    ! use interpolation for w,z from xsb and xtb
    ! which is necessary for obtaining stable result when |xsb| or |xtb| is numerically large
    ! xsb=-4*(w-z)**2/((1-w**2)*(1-z**2))
    ! xtb=-(w-z)**2/(w*z)
    use interpolation
    use LbL_Global
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::xsb,xtb
    COMPLEX(KIND(1d0)),INTENT(OUT)::w,z
    INTEGER,SAVE::init=0
    INTEGER::I,J,K,L
    INTEGER,PARAMETER::n_interp=5
    LOGICAL::file_exists
    INTEGER::ixx,iyy
    REAL(KIND(1d0)),DIMENSION(1)::XI,YI,ZI
    REAL(KIND(1d0)),DIMENSION(n_interp)::XD2_1D,YD2_1D
    REAL(KIND(1d0)),DIMENSION(n_interp,n_interp)::ZD2
    ! region 1
    ! xsb<0, xtb<0
    INTEGER,PARAMETER::NXSEG1=5
    INTEGER,PARAMETER::LOG10XMIN1=-2
    INTEGER,PARAMETER::LOG10XMAX1=8
    INTEGER::NXA1,NYA1
    SAVE NXA1,NYA1
    ! XA1 is log10(-xsb)
    ! YA1 is log10(-xtb)
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::XA1,YA1
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::WA1,ZA1
    SAVE XA1,YA1,WA1,ZA1
    ! region (2,1)
    ! xsb>4, xtb<0, -xsb-xtb<0
    INTEGER,PARAMETER::LOG10XMIN2=1
    INTEGER,PARAMETER::LOG10XMAX2=8
    INTEGER,PARAMETER::LOG10YMIN2=-6
    INTEGER,PARAMETER::LOG10YMAX2=0
    INTEGER::NXA2,NYA2
    SAVE NXA2,NYA2
    ! XA2 is log10(xsb)
    ! YA2 is log10(-xtb/xsb)
    ! For YA2, above -1/5, there are extra 13 points added
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::XA2,YA2
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::WA2,ZA2
    SAVE XA2,YA2,WA2,ZA2
    ! region (3,1)
    ! xsb<0, xtb>4, -xsb-xtb<0
    INTEGER,PARAMETER::LOG10XMIN3=1
    INTEGER,PARAMETER::LOG10XMAX3=8
    INTEGER,PARAMETER::LOG10YMIN3=-6
    INTEGER,PARAMETER::LOG10YMAX3=0
    INTEGER::NXA3,NYA3
    SAVE NXA3,NYA3
    ! XA3 is log10(xtb)
    ! YA3 is log10(-xsb/xtb)
    ! For YA3, above -1/5, there are extra 13 points added
    REAL(KIND(1d0)),DIMENSION(:),ALLOCATABLE::XA3,YA3
    REAL(KIND(1d0)),DIMENSION(:,:),ALLOCATABLE::WA3,ZA3
    SAVE XA3,YA3,WA3,ZA3
    IF(init.EQ.0)THEN
       NXA1=(LOG10XMAX1-LOG10XMIN1)*NXSEG1
       IF(ALLOCATED(XA1))THEN
          DEALLOCATE(XA1)
       ENDIF
       ALLOCATE(XA1(NXA1))
       NYA1=NXA1
       IF(ALLOCATED(YA1))THEN
          DEALLOCATE(YA1)
       ENDIF
       ALLOCATE(YA1(NYA1))
       IF(ALLOCATED(WA1))THEN
          DEALLOCATE(WA1)
       ENDIF
       ALLOCATE(WA1(NXA1,NYA1))
       IF(ALLOCATED(ZA1))THEN
          DEALLOCATE(ZA1)
       ENDIF
       ALLOCATE(ZA1(NXA1,NYA1))
       INQUIRE(FILE=TRIM(grid_dir)//"wz_region1_grid.dat",EXIST=file_exists)
       IF(.NOT.file_exists)THEN
          WRITE(*,*)"ERROR: wz_region1_grid.dat does not exist"
          STOP
       ENDIF
       OPEN(UNIT=30765,FILE=TRIM(grid_dir)//"wz_region1_grid.dat",STATUS="OLD",ACTION='READ')
       DO I=1,NXA1
          DO J=1,NYA1
             READ(30765,*)XA1(I),YA1(J),WA1(I,J),ZA1(I,J)
          ENDDO
       ENDDO
       CLOSE(UNIT=30765)
       NXA2=(LOG10XMAX2-LOG10XMIN2)*NXSEG1+1
       IF(ALLOCATED(XA2))THEN
          DEALLOCATE(XA2)
       ENDIF
       ALLOCATE(XA2(NXA2))
       NYA2=(LOG10YMAX2-LOG10YMIN2)*NXSEG1+13
       IF(ALLOCATED(YA2))THEN
          DEALLOCATE(YA2)
       ENDIF
       ALLOCATE(YA2(NYA2))
       IF(ALLOCATED(WA2))THEN
          DEALLOCATE(WA2)
       ENDIF
       ALLOCATE(WA2(NXA2,NYA2))
       IF(ALLOCATED(ZA2))THEN
          DEALLOCATE(ZA2)
       ENDIF
       ALLOCATE(ZA2(NXA2,NYA2))
       INQUIRE(FILE=TRIM(grid_dir)//"wz_region21_grid.dat",EXIST=file_exists)
       IF(.NOT.file_exists)THEN
          WRITE(*,*)"ERROR: wz_region21_grid.dat does not exist"
          STOP
       ENDIF
       OPEN(UNIT=30765,FILE=TRIM(grid_dir)//"wz_region21_grid.dat",STATUS="OLD",ACTION='READ')
       DO I=1,NXA2
          DO J=1,NYA2
             READ(30765,*)XA2(I),YA2(J),WA2(I,J),ZA2(I,J)
          ENDDO
       ENDDO
       CLOSE(UNIT=30765)
       NXA3=(LOG10XMAX3-LOG10XMIN3)*NXSEG1+1
       IF(ALLOCATED(XA3))THEN
          DEALLOCATE(XA3)
       ENDIF
       ALLOCATE(XA3(NXA3))
       NYA3=(LOG10YMAX3-LOG10YMIN3)*NXSEG1+13
       IF(ALLOCATED(YA3))THEN
          DEALLOCATE(YA3)
       ENDIF
       ALLOCATE(YA3(NYA3))
       IF(ALLOCATED(WA3))THEN
          DEALLOCATE(WA3)
       ENDIF
       ALLOCATE(WA3(NXA3,NYA3))
       IF(ALLOCATED(ZA3))THEN
          DEALLOCATE(ZA3)
       ENDIF
       ALLOCATE(ZA3(NXA3,NYA3))
       INQUIRE(FILE=TRIM(grid_dir)//"wz_region31_grid.dat",EXIST=file_exists)
       IF(.NOT.file_exists)THEN
          WRITE(*,*)"ERROR: wz_region31_grid.dat does not exist"
          STOP
       ENDIF
       OPEN(UNIT=30765,FILE=TRIM(grid_dir)//"wz_region31_grid.dat",STATUS="OLD",ACTION='READ')
       DO I=1,NXA3
          DO J=1,NYA3
             READ(30765,*)XA3(I),YA3(J),WA3(I,J),ZA3(I,J)
          ENDDO
       ENDDO
       CLOSE(UNIT=30765)
       init=1
    ENDIF
    IF(xsb.LT.0d0.AND.xtb.LT.0d0)THEN
       ! region 1
       XI(1)=DLOG10(-xsb)
       YI(1)=DLOG10(-xtb)
       IF(XI(1).LT.-2d0.or.YI(1).LT.-2d0.or.XI(1).GT.7.8d0.or.YI(1).GT.7.8d0)THEN
          WRITE(*,*)"WARNING: outside the grid range #1"
          CALL evaluate_wz_direct(xsb,xtb,w,z)
       ELSE
          ixx=FLOOR(XI(1))+2
          iyy=FLOOR(YI(1))+2
          DO i=ixx*NXSEG1+1,MIN((ixx+1)*NXSEG1+1,NXA1)
             IF(XI(1).LE.XA1(i))EXIT
          ENDDO
          IF(i-n_interp/2.GE.1.AND.i-n_interp/2+n_interp-1.LE.NXA1)THEN
             K=i-n_interp/2
          ELSEIF(i-n_interp/2.LT.1)THEN
             K=1
          ELSEIF(i-n_interp/2+n_interp-1.GT.NXA1)THEN
             K=NXA1+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #1 !"
             STOP
          ENDIF
          DO j=iyy*NXSEG1+1,MIN((iyy+1)*NXSEG1+1,NYA1)
             IF(YI(1).LE.YA1(j))EXIT
          ENDDO
          IF(j-n_interp/2.GE.1.AND.j-n_interp/2+n_interp-1.LE.NYA1)THEN
             L=j-n_interp/2
          ELSEIF(j-n_interp/2.LT.1)THEN
             L=1
          ELSEIF(j-n_interp/2+n_interp-1.GT.NYA1)THEN
             L=NYA1+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #2 !"
             STOP
          ENDIF
          DO I=1,n_interp
             XD2_1D(I)=XA1(K+I-1)
             YD2_1D(I)=YA1(L+I-1)
          ENDDO
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=WA1(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          w=ZI(1)
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=ZA1(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          z=ZI(1)
       ENDIF
    ELSEIF(xsb.GT.4d0.AND.xtb.LT.0d0)THEN
       ! region (2,1)
       XI(1)=DLOG10(xsb)
       YI(1)=DLOG10(-xtb/xsb)
       IF(XI(1).LT.1d0.or.YI(1).LT.-6d0.or.XI(1).GT.8d0.or.YI(1).GT.-2d-5)THEN
          WRITE(*,*)"WARNING: outside the grid range #2"
          CALL evaluate_wz_direct(xsb,xtb,w,z)
       ELSE
          ixx=FLOOR(XI(1))-1
          iyy=FLOOR(YI(1))+6
          DO i=ixx*NXSEG1+1,(ixx+1)*NXSEG1+1
             IF(XI(1).LE.XA2(i))EXIT
          ENDDO
          IF(i-n_interp/2.GE.1.AND.i-n_interp/2+n_interp-1.LE.NXA2)THEN
             K=i-n_interp/2
          ELSEIF(i-n_interp/2.LT.1)THEN
             K=1
          ELSEIF(i-n_interp/2+n_interp-1.GT.NXA2)THEN
             K=NXA2+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #3 !"
             STOP
          ENDIF
          IF(iyy.LT.5)THEN
             DO j=iyy*NXSEG1+1,(iyy+1)*NXSEG1+1
                IF(YI(1).LE.YA2(j))EXIT
             ENDDO
          ELSE
             DO j=iyy*NXSEG1+1,NYA2
                IF(YI(1).LE.YA2(j))EXIT
             ENDDO
          ENDIF
          IF(j-n_interp/2.GE.1.AND.j-n_interp/2+n_interp-1.LE.NYA2)THEN
             L=j-n_interp/2
          ELSEIF(j-n_interp/2.LT.1)THEN
             L=1
          ELSEIF(j-n_interp/2+n_interp-1.GT.NYA2)THEN
             L=NYA2+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #4 !"
             STOP
          ENDIF
          DO I=1,n_interp
             XD2_1D(I)=XA2(K+I-1)
             YD2_1D(I)=YA2(L+I-1)
          ENDDO
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=WA2(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          w=ZI(1)
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=ZA2(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          z=ZI(1)
       ENDIF
    ELSEIF(xtb.GT.4d0.AND.xsb.LT.0d0)THEN
       ! region (3,1)
       XI(1)=DLOG10(xtb)
       YI(1)=DLOG10(-xsb/xtb)
       IF(XI(1).LT.1d0.or.YI(1).LT.-6d0.or.XI(1).GT.8d0.or.YI(1).GT.-2d-5)THEN
          WRITE(*,*)"WARNING: outside the grid range #3"
          CALL evaluate_wz_direct(xsb,xtb,w,z)
       ELSE
          ixx=FLOOR(XI(1))-1
          iyy=FLOOR(YI(1))+6
          DO i=ixx*NXSEG1+1,(ixx+1)*NXSEG1+1
             IF(XI(1).LE.XA3(i))EXIT
          ENDDO
          IF(i-n_interp/2.GE.1.AND.i-n_interp/2+n_interp-1.LE.NXA3)THEN
             K=i-n_interp/2
          ELSEIF(i-n_interp/2.LT.1)THEN
             K=1
          ELSEIF(i-n_interp/2+n_interp-1.GT.NXA3)THEN
             K=NXA3+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #5 !"
             STOP
          ENDIF
          IF(iyy.LT.5)THEN
             DO j=iyy*NXSEG1+1,(iyy+1)*NXSEG1+1
                IF(YI(1).LE.YA3(j))EXIT
             ENDDO
          ELSE
             DO j=iyy*NXSEG1+1,NYA3
                IF(YI(1).LE.YA3(j))EXIT
             ENDDO
          ENDIF
          IF(j-n_interp/2.GE.1.AND.j-n_interp/2+n_interp-1.LE.NYA3)THEN
             L=j-n_interp/2
          ELSEIF(j-n_interp/2.LT.1)THEN
             L=1
          ELSEIF(j-n_interp/2+n_interp-1.GT.NYA3)THEN
             L=NYA3+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #6 !"
             STOP
          ENDIF
          DO I=1,n_interp
             XD2_1D(I)=XA3(K+I-1)
             YD2_1D(I)=YA3(L+I-1)
          ENDDO
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=WA3(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          w=ZI(1)
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=ZA3(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          z=ZI(1)
       ENDIF
    ELSE
       WRITE(*,*)"ERROR: Do not understand the region (xsb,xtb)=",xsb,xtb
       STOP
    ENDIF
    return
  end subroutine evaluate_wz_interp

  subroutine evaluate_wz_direct(xsb,xtb,w,z)
    ! calculate w and z from xsb and xtb directly
    ! xsb=-4*(w-z)**2/((1-w**2)*(1-z**2))
    ! xtb=-(w-z)**2/(w*z)
    IMPLICIT NONE
    REAL(KIND(1d0)),INTENT(IN)::xsb,xtb
    COMPLEX(KIND(1d0)),INTENT(OUT)::w,z
    REAL(KIND(1d0))::r1
    COMPLEX(KIND(1d0))::sq1,sq2,r2
    REAL(KIND(1d0))::xstb,xtb2,xsb2,fourmxsb,fourmxtb,temp
    REAL(KIND(1d0))::exp1,exp2,exp3,zden
    COMPLEX(KIND(1d0))::w2,znum,z12,sqrtz12
    REAL(KIND(1d0))::wr,wi,zr,zi
    ! 1-sqrt(2)
    REAL(KIND(1d0)),PARAMETER::onemroot2=-0.414213562373096d0
    IF(xsb.EQ.0d0.OR.xtb.EQ.0d0.OR.xsb+xtb.EQ.0d0)THEN
       ! xsb or xtb or xsb+xtb either is zero
       ! we randomly choose a point for w=z
       w=0.5d0
       z=0.5d0
       RETURN
    ENDIF
    xstb=xsb*xtb
    xtb2=xtb**2
    xsb2=xsb**2
    fourmxsb=4d0-xsb
    fourmxtb=4d0-xtb
    r1=-2d0+(2d0-4d0*xtb+xtb2)*(2d0*xsb2+4d0*fourmxsb*xstb+fourmxsb**2*xtb2)&
         /(2d0*xsb2)
    exp1=xstb-4d0*xsb-4d0*xtb
    exp2=exp1+2d0*xsb
    exp3=exp2+2d0*xtb
    temp=fourmxsb*fourmxtb*exp1
    IF((xsb.LT.0d0.AND.xtb.LT.0d0).or.&
         (xsb.GT.4d0.AND.xtb.LT.0d0.AND.xtb.GT.-xsb).or.&
         (xsb.LT.0d0.AND.xtb.GT.4d0.AND.xsb.GT.-xtb))THEN
       ! region 1 or regions (2,1)+(2,2) or regions (3,1)+(3,2)
       sq1=sqrt(dcmplx(temp,0d0))
       r2=temp*exp2*(2d0-xtb)*xtb/(2d0*xsb2*sq1)
       sq2=sqrt(dcmplx(r1,0d0)-r2)
       w2=-exp2*(2d0-xtb)/(4d0*xsb)+xtb/(4d0*xsb)*sq1-0.5d0*sq2
       zden=8d0*xstb*exp3
       z12=((8d0+sq1-4d0*xtb)*xtb+xsb*(2d0-xtb)**2)/(4d0*xsb)-0.5d0*sq2
       sqrtz12=sqrt(z12)
       znum=-xsb*(2d0-xtb)*(xsb2+8d0*xstb*(2d0-xtb)+16d0*xtb2)*sqrtz12&
            -(exp1+xsb)*exp2*(exp2+xsb)*z12*sqrtz12&
            -xsb*(exp1+xsb)*(exp2+xsb)*(2d0-xtb)*z12**2*sqrtz12&
            -xsb2*exp2*z12**3*sqrtz12
       w=sqrt(w2)
       z=znum/zden
       wr=dreal(w)
       wi=0d0
       zr=dreal(z)
       zi=0d0
       ! check
       IF(xsb.LT.0d0.AND.xtb.LT.0d0)THEN
          ! region 1
          IF(wr.LT.0d0.or.zr.GT.1d0.or.wr.GT.zr)THEN
             WRITE(*,*)"ERROR: evaluate_wz region 1:",xsb,xtb
             WRITE(*,*)"w=",wr
             WRITE(*,*)"z=",zr
             STOP
          ENDIF
       ELSEIF(xsb.GT.4d0.AND.xtb.LT.0d0)THEN
          ! regions (2,1)+(2,2)
          IF(wr.LT.0d0.or.wr.gt.1d0.or.zr.lt.1d0)THEN
             WRITE(*,*)"ERROR: evaluate_wz region (2,1):",xsb,xtb
             WRITE(*,*)"w=",wr
             WRITE(*,*)"z=",zr
             STOP
          ENDIF
          IF(zr.gt.1d0/wr.or.zr.gt.(1d0+wr)/(1d0-wr))THEN
             ! we should pick region (2,2)
             ! sq1 -> -sq1
             sq1=-sq1
             r2=temp*exp2*(2d0-xtb)*xtb/(2d0*xsb2*sq1)
             sq2=sqrt(dcmplx(r1,0d0)-r2)
             w2=-exp2*(2d0-xtb)/(4d0*xsb)+xtb/(4d0*xsb)*sq1-0.5d0*sq2
             zden=8d0*xstb*exp3
             z12=((8d0+sq1-4d0*xtb)*xtb+xsb*(2d0-xtb)**2)/(4d0*xsb)-0.5d0*sq2
             sqrtz12=sqrt(z12)
             znum=-xsb*(2d0-xtb)*(xsb2+8d0*xstb*(2d0-xtb)+16d0*xtb2)*sqrtz12&
                  -(exp1+xsb)*exp2*(exp2+xsb)*z12*sqrtz12&
                  -xsb*(exp1+xsb)*(exp2+xsb)*(2d0-xtb)*z12**2*sqrtz12&
                  -xsb2*exp2*z12**3*sqrtz12
             w=sqrt(w2)
             z=znum/zden
             wr=dreal(w)
             zr=dreal(z)
             IF(wr.LT.0d0.or.wr.gt.1d0.or.zr.lt.1d0.or.zr.gt.1d0/wr&
                  .or.zr.gt.(1d0+wr)/(1d0-wr))THEN
                WRITE(*,*)"ERROR: evaluate_wz region (2,2):",xsb,xtb
                WRITE(*,*)"w=",wr
                WRITE(*,*)"z=",zr
                STOP
             ENDIF
          ENDIF
       ELSEIF(xtb.GT.4d0.AND.xsb.LT.0d0)THEN
          ! regions (3,1)+(3,2)
          wr=-wr
          zr=-zr
          if(zr.gt.-wr.and.zr+wr.LT.1d-4)then
             zr=-2d0*wr-zr
          endif
          if(zr.gt.(1d0+wr)/(1d0-wr).and.(zr-(1d0+wr)/(1d0-wr)).LT.1d-4)then
             zr=2d0*(1d0+wr)/(1d0-wr)-zr
          endif
          IF(wr.gt.0d0.or.wr.lt.-1d0.or.zr.lt.0d0.or.zr.gt.-wr&
               .or.zr.gt.(1d0+wr)/(1d0-wr))THEN
             WRITE(*,*)"ERROR: evaluate_wz region (3,1 or 2):",xsb,xtb
             WRITE(*,*)"w=",wr
             WRITE(*,*)"z=",zr
             STOP
          ENDIF
       ELSE
          WRITE(*,*)"ERROR: invalid region #1:",xsb,xtb
          STOP
       ENDIF
    ELSEIF(xsb.EQ.4d0.AND.xtb.LT.0d0.AND.xtb.GE.-4d0)THEN
       ! the special one
       wr=dsqrt(1d0-xtb/2d0-0.5d0*dsqrt(-xtb*fourmxtb))
       wi=0d0
       zr=1d0/wr
       zi=0d0
    ELSEIF(xsb.LT.0d0.AND.xtb.EQ.4d0.AND.xsb.GE.-4d0)THEN
       ! the special one
       wr=-dsqrt(1d0-4d0*dsqrt(fourmxsb/xsb2)-8d0/xsb)
       wi=0d0
       zr=-wr
       zi=0d0
    ELSEIF(xsb.GT.0d0.AND.xsb.LT.4d0.AND.xtb.LT.0d0.AND.xtb.GT.-xsb)THEN
       ! regions (2,3)+(2,4)
       sq1=sqrt(dcmplx(temp,0d0))
       r2=temp*exp2*(2d0-xtb)*xtb/(2d0*xsb2*sq1)
       sq2=sqrt(dcmplx(r1,0d0)-r2)
       w2=-exp2*(2d0-xtb)/(4d0*xsb)+xtb/(4d0*xsb)*sq1-0.5d0*sq2
       w=sqrt(w2)
       z=w/(abs(w)**2)
       wr=dreal(w)
       wi=dimag(w)
       zr=dreal(z)
       zi=dimag(z)
       ! check
       IF(wr.lt.0d0.or.wr.gt.1d0.or.wi.lt.0d0&
            .or.wi**2.lt.1d0-2d0*wr-wr**2&
            .or.wi**2.gt.1d0-wr**2)then
          ! sq2 -> -sq2
          sq2=-sq2
          w2=-exp2*(2d0-xtb)/(4d0*xsb)+xtb/(4d0*xsb)*sq1-0.5d0*sq2
          w=sqrt(w2)
          z=w/(abs(w)**2)
          wr=dreal(w)
          wi=dimag(w)
          zr=dreal(z)
          zi=dimag(z)
          IF(wr.lt.0d0.or.wr.gt.1d0.or.wi.lt.0d0&
               .or.wi**2.lt.1d0-2d0*wr-wr**2&
               .or.wi**2.gt.1d0-wr**2)then
             WRITE(*,*)"ERROR: evaluate_wz region (2,3 or 4):",xsb,xtb
             WRITE(*,*)"w=",w
             WRITE(*,*)"z=",z
             STOP
          ENDIF
       endif
    ELSEIF(xtb.GT.0d0.AND.xtb.LT.4d0.AND.xsb.LT.0d0.AND.xsb.GT.-xtb)THEN
       ! regions (3,3)+(3,4)
       sq1=sqrt(dcmplx(temp,0d0))
       r2=temp*exp2*(2d0-xtb)*xtb/(2d0*xsb2*sq1)
       sq2=sqrt(dcmplx(r1,0d0)-r2)
       w2=-exp2*(2d0-xtb)/(4d0*xsb)+xtb/(4d0*xsb)*sq1-0.5d0*sq2
       z=sqrt(w2)
       w=-dconjg(z)
       wr=dreal(w)
       wi=dimag(w)
       zr=dreal(z)
       zi=dimag(z)
       ! check
       IF(wr.lt.onemroot2.or.wr.gt.0d0.or.wi.lt.0d0&
            .or.wi**2.gt.1d0+2d0*wr-wr**2)then
          ! sq2 -> -sq2
          sq2=-sq2
          w2=-exp2*(2d0-xtb)/(4d0*xsb)+xtb/(4d0*xsb)*sq1-0.5d0*sq2
          z=sqrt(w2)
          w=-dconjg(z)
          wr=dreal(w)
          wi=dimag(w)
          zr=dreal(z)
          zi=dimag(z)
          IF(wr.lt.onemroot2.or.wr.gt.0d0.or.wi.lt.0d0&
               .or.wi**2.gt.1d0+2d0*wr-wr**2)then
             WRITE(*,*)"ERROR: evaluate_wz region (3,3 or 4):",xsb,xtb
             WRITE(*,*)"w=",w
             WRITE(*,*)"z=",z
             STOP
          ENDIF
       endif
    ELSE
       WRITE(*,*)"ERROR: invalid region #0:",xsb,xtb
       STOP
    ENDIF
    w=dcmplx(wr,wi)
    z=dcmplx(zr,zi)
    RETURN
  END subroutine evaluate_wz_direct

  subroutine evaluate_wz_qp(xsb,xtb,w,z)
    implicit none
    REAL(KIND(1E0_16)),INTENT(IN)::xsb,xtb
    COMPLEX(KIND(1E0_16)),INTENT(OUT)::w,z
    ! above xinterp, we use the interpolation if it is possible
    REAL(KIND(1E0_16)),PARAMETER::xinterp=50E0_16
    REAL(KIND(1E0_16)),PARAMETER::xmax=6.3095734448019E7_16
    IF(xsb.EQ.0E0_16.OR.xtb.EQ.0E0_16.OR.xsb+xtb.EQ.0E0_16)THEN
       ! xsb or xtb or xsb+xtb either is zero
       ! we randomly choose a point for w=z
       w=0.5E0_16
       z=0.5E0_16
       RETURN
    ENDIF
    if(xsb.LT.0E0_16.and.xtb.lt.0E0_16.and.-xsb.GE.1E-2_16&
         .and.-xtb.GE.1E-2_16.and.-xsb.LE.xmax.and.-xtb.LE.xmax.and.&
         (abs(xsb).GT.8E6_16.or.abs(xtb).GT.8E6_16))then
       call evaluate_wz_interp_qp(xsb,xtb,w,z)
    elseif(xsb.GT.10E0_16.and.xtb.lt.0E0_16.and.xsb.LT.1E8_16.and.-xtb/xsb.GE.1E-6_16&
         .and.-xtb/xsb.LE.0.9999539493585E0_16.and.&
         (xsb.GT.9.5E5_16))then
       call evaluate_wz_interp_qp(xsb,xtb,w,z)
    elseif(xtb.GT.10E0_16.and.xsb.lt.0E0_16.and.xtb.LT.1E8_16.and.-xsb/xtb.GE.1E-6_16&
         .and.-xsb/xtb.LE.0.9999539493585E0_16.and.(xtb.GT.9E3_16))then
       call evaluate_wz_interp_qp(xsb,xtb,w,z)
    else
       call evaluate_wz_direct_qp(xsb,xtb,w,z)
    endif
    return
  end subroutine evaluate_wz_qp

  subroutine evaluate_wz_interp_qp(xsb,xtb,w,z)
    ! use interpolation for w,z from xsb and xtb
    ! which is necessary for obtaining stable result when |xsb| or |xtb| is numerically large
    ! xsb=-4*(w-z)**2/((1-w**2)*(1-z**2))
    ! xtb=-(w-z)**2/(w*z)
    use interpolation
    use LbL_Global
    IMPLICIT NONE
    REAL(KIND(1E0_16)),INTENT(IN)::xsb,xtb
    COMPLEX(KIND(1E0_16)),INTENT(OUT)::w,z
    INTEGER,SAVE::init=0
    INTEGER::I,J,K,L
    INTEGER,PARAMETER::n_interp=5
    LOGICAL::file_exists
    INTEGER::ixx,iyy
    REAL(KIND(1E0_16)),DIMENSION(1)::XI,YI,ZI
    REAL(KIND(1E0_16)),DIMENSION(n_interp)::XD2_1D,YD2_1D
    REAL(KIND(1E0_16)),DIMENSION(n_interp,n_interp)::ZD2
    ! region 1
    ! xsb<0, xtb<0
    INTEGER,PARAMETER::NXSEG1=5
    INTEGER,PARAMETER::LOG10XMIN1=-2
    INTEGER,PARAMETER::LOG10XMAX1=8
    INTEGER::NXA1,NYA1
    SAVE NXA1,NYA1
    ! XA1 is log10(-xsb)
    ! YA1 is log10(-xtb)
    REAL(KIND(1E0_16)),DIMENSION(:),ALLOCATABLE::XA1,YA1
    REAL(KIND(1E0_16)),DIMENSION(:,:),ALLOCATABLE::WA1,ZA1
    SAVE XA1,YA1,WA1,ZA1
    ! region (2,1)
    ! xsb>4, xtb<0, -xsb-xtb<0
    INTEGER,PARAMETER::LOG10XMIN2=1
    INTEGER,PARAMETER::LOG10XMAX2=8
    INTEGER,PARAMETER::LOG10YMIN2=-6
    INTEGER,PARAMETER::LOG10YMAX2=0
    INTEGER::NXA2,NYA2
    SAVE NXA2,NYA2
    ! XA2 is log10(xsb)
    ! YA2 is log10(-xtb/xsb)
    ! For YA2, above -1/5, there are extra 13 points added
    REAL(KIND(1E0_16)),DIMENSION(:),ALLOCATABLE::XA2,YA2
    REAL(KIND(1E0_16)),DIMENSION(:,:),ALLOCATABLE::WA2,ZA2
    SAVE XA2,YA2,WA2,ZA2
    ! region (3,1)
    ! xsb<0, xtb>4, -xsb-xtb<0
    INTEGER,PARAMETER::LOG10XMIN3=1
    INTEGER,PARAMETER::LOG10XMAX3=8
    INTEGER,PARAMETER::LOG10YMIN3=-6
    INTEGER,PARAMETER::LOG10YMAX3=0
    INTEGER::NXA3,NYA3
    SAVE NXA3,NYA3
    ! XA3 is log10(xtb)
    ! YA3 is log10(-xsb/xtb)
    ! For YA3, above -1/5, there are extra 13 points added
    REAL(KIND(1E0_16)),DIMENSION(:),ALLOCATABLE::XA3,YA3
    REAL(KIND(1E0_16)),DIMENSION(:,:),ALLOCATABLE::WA3,ZA3
    SAVE XA3,YA3,WA3,ZA3
    IF(init.EQ.0)THEN
       NXA1=(LOG10XMAX1-LOG10XMIN1)*NXSEG1
       IF(ALLOCATED(XA1))THEN
          DEALLOCATE(XA1)
       ENDIF
       ALLOCATE(XA1(NXA1))
       NYA1=NXA1
       IF(ALLOCATED(YA1))THEN
          DEALLOCATE(YA1)
       ENDIF
       ALLOCATE(YA1(NYA1))
       IF(ALLOCATED(WA1))THEN
          DEALLOCATE(WA1)
       ENDIF
       ALLOCATE(WA1(NXA1,NYA1))
       IF(ALLOCATED(ZA1))THEN
          DEALLOCATE(ZA1)
       ENDIF
       ALLOCATE(ZA1(NXA1,NYA1))
       INQUIRE(FILE=TRIM(grid_dir)//"wz_region1_grid.dat",EXIST=file_exists)
       IF(.NOT.file_exists)THEN
          WRITE(*,*)"ERROR: wz_region1_grid.dat does not exist"
          STOP
       ENDIF
       OPEN(UNIT=30765,FILE=TRIM(grid_dir)//"wz_region1_grid.dat",STATUS="OLD",ACTION='READ')
       DO I=1,NXA1
          DO J=1,NYA1
             READ(30765,*)XA1(I),YA1(J),WA1(I,J),ZA1(I,J)
          ENDDO
       ENDDO
       CLOSE(UNIT=30765)
       NXA2=(LOG10XMAX2-LOG10XMIN2)*NXSEG1+1
       IF(ALLOCATED(XA2))THEN
          DEALLOCATE(XA2)
       ENDIF
       ALLOCATE(XA2(NXA2))
       NYA2=(LOG10YMAX2-LOG10YMIN2)*NXSEG1+13
       IF(ALLOCATED(YA2))THEN
          DEALLOCATE(YA2)
       ENDIF
       ALLOCATE(YA2(NYA2))
       IF(ALLOCATED(WA2))THEN
          DEALLOCATE(WA2)
       ENDIF
       ALLOCATE(WA2(NXA2,NYA2))
       IF(ALLOCATED(ZA2))THEN
          DEALLOCATE(ZA2)
       ENDIF
       ALLOCATE(ZA2(NXA2,NYA2))
       INQUIRE(FILE=TRIM(grid_dir)//"wz_region21_grid.dat",EXIST=file_exists)
       IF(.NOT.file_exists)THEN
          WRITE(*,*)"ERROR: wz_region21_grid.dat does not exist"
          STOP
       ENDIF
       OPEN(UNIT=30765,FILE=TRIM(grid_dir)//"wz_region21_grid.dat",STATUS="OLD",ACTION='READ')
       DO I=1,NXA2
          DO J=1,NYA2
             READ(30765,*)XA2(I),YA2(J),WA2(I,J),ZA2(I,J)
          ENDDO
       ENDDO
       CLOSE(UNIT=30765)
       NXA3=(LOG10XMAX3-LOG10XMIN3)*NXSEG1+1
       IF(ALLOCATED(XA3))THEN
          DEALLOCATE(XA3)
       ENDIF
       ALLOCATE(XA3(NXA3))
       NYA3=(LOG10YMAX3-LOG10YMIN3)*NXSEG1+13
       IF(ALLOCATED(YA3))THEN
          DEALLOCATE(YA3)
       ENDIF
       ALLOCATE(YA3(NYA3))
       IF(ALLOCATED(WA3))THEN
          DEALLOCATE(WA3)
       ENDIF
       ALLOCATE(WA3(NXA3,NYA3))
       IF(ALLOCATED(ZA3))THEN
          DEALLOCATE(ZA3)
       ENDIF
       ALLOCATE(ZA3(NXA3,NYA3))
       INQUIRE(FILE=TRIM(grid_dir)//"wz_region31_grid.dat",EXIST=file_exists)
       IF(.NOT.file_exists)THEN
          WRITE(*,*)"ERROR: wz_region31_grid.dat does not exist"
          STOP
       ENDIF
       OPEN(UNIT=30765,FILE=TRIM(grid_dir)//"wz_region31_grid.dat",STATUS="OLD",ACTION='READ')
       DO I=1,NXA3
          DO J=1,NYA3
             READ(30765,*)XA3(I),YA3(J),WA3(I,J),ZA3(I,J)
          ENDDO
       ENDDO
       CLOSE(UNIT=30765)
       init=1
    ENDIF
    IF(xsb.LT.0E0_16.AND.xtb.LT.0E0_16)THEN
       ! region 1
       XI(1)=LOG10(-xsb)
       YI(1)=LOG10(-xtb)
       IF(XI(1).LT.-2E0_16.or.YI(1).LT.-2E0_16.or.&
            XI(1).GT.7.8E0_16.or.YI(1).GT.7.8E0_16)THEN
          WRITE(*,*)"WARNING: outside the grid range #1"
          CALL evaluate_wz_direct_qp(xsb,xtb,w,z)
       ELSE
          ixx=FLOOR(XI(1))+2
          iyy=FLOOR(YI(1))+2
          DO i=ixx*NXSEG1+1,MIN((ixx+1)*NXSEG1+1,NXA1)
             IF(XI(1).LE.XA1(i))EXIT
          ENDDO
          IF(i-n_interp/2.GE.1.AND.i-n_interp/2+n_interp-1.LE.NXA1)THEN
             K=i-n_interp/2
          ELSEIF(i-n_interp/2.LT.1)THEN
             K=1
          ELSEIF(i-n_interp/2+n_interp-1.GT.NXA1)THEN
             K=NXA1+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #1 !"
             STOP
          ENDIF
          DO j=iyy*NXSEG1+1,MIN((iyy+1)*NXSEG1+1,NYA1)
             IF(YI(1).LE.YA1(j))EXIT
          ENDDO
          IF(j-n_interp/2.GE.1.AND.j-n_interp/2+n_interp-1.LE.NYA1)THEN
             L=j-n_interp/2
          ELSEIF(j-n_interp/2.LT.1)THEN
             L=1
          ELSEIF(j-n_interp/2+n_interp-1.GT.NYA1)THEN
             L=NYA1+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #2 !"
             STOP
          ENDIF
          DO I=1,n_interp
             XD2_1D(I)=XA1(K+I-1)
             YD2_1D(I)=YA1(L+I-1)
          ENDDO
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=WA1(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d_qp(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          w=ZI(1)
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=ZA1(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d_qp(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          z=ZI(1)
       ENDIF
    ELSEIF(xsb.GT.4E0_16.AND.xtb.LT.0E0_16)THEN
       ! region (2,1)
       XI(1)=LOG10(xsb)
       YI(1)=LOG10(-xtb/xsb)
       IF(XI(1).LT.1E0_16.or.YI(1).LT.-6E0_16.or.XI(1).GT.8E0_16.or.YI(1).GT.-2E-5_16)THEN
          WRITE(*,*)"WARNING: outside the grid range #2"
          CALL evaluate_wz_direct_qp(xsb,xtb,w,z)
       ELSE
          ixx=FLOOR(XI(1))-1
          iyy=FLOOR(YI(1))+6
          DO i=ixx*NXSEG1+1,(ixx+1)*NXSEG1+1
             IF(XI(1).LE.XA2(i))EXIT
          ENDDO
          IF(i-n_interp/2.GE.1.AND.i-n_interp/2+n_interp-1.LE.NXA2)THEN
             K=i-n_interp/2
          ELSEIF(i-n_interp/2.LT.1)THEN
             K=1
          ELSEIF(i-n_interp/2+n_interp-1.GT.NXA2)THEN
             K=NXA2+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #3 !"
             STOP
          ENDIF
          IF(iyy.LT.5)THEN
             DO j=iyy*NXSEG1+1,(iyy+1)*NXSEG1+1
                IF(YI(1).LE.YA2(j))EXIT
             ENDDO
          ELSE
             DO j=iyy*NXSEG1+1,NYA2
                IF(YI(1).LE.YA2(j))EXIT
             ENDDO
          ENDIF
          IF(j-n_interp/2.GE.1.AND.j-n_interp/2+n_interp-1.LE.NYA2)THEN
             L=j-n_interp/2
          ELSEIF(j-n_interp/2.LT.1)THEN
             L=1
          ELSEIF(j-n_interp/2+n_interp-1.GT.NYA2)THEN
             L=NYA2+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #4 !"
             STOP
          ENDIF
          DO I=1,n_interp
             XD2_1D(I)=XA2(K+I-1)
             YD2_1D(I)=YA2(L+I-1)
          ENDDO
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=WA2(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d_qp(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          w=ZI(1)
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=ZA2(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d_qp(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          z=ZI(1)
       ENDIF
    ELSEIF(xtb.GT.4E0_16.AND.xsb.LT.0E0_16)THEN
       ! region (3,1)
       XI(1)=LOG10(xtb)
       YI(1)=LOG10(-xsb/xtb)
       IF(XI(1).LT.1E0_16.or.YI(1).LT.-6E0_16.or.XI(1).GT.8E0_16.or.YI(1).GT.-2E-5_16)THEN
          WRITE(*,*)"WARNING: outside the grid range #3"
          CALL evaluate_wz_direct_qp(xsb,xtb,w,z)
       ELSE
          ixx=FLOOR(XI(1))-1
          iyy=FLOOR(YI(1))+6
          DO i=ixx*NXSEG1+1,(ixx+1)*NXSEG1+1
             IF(XI(1).LE.XA3(i))EXIT
          ENDDO
          IF(i-n_interp/2.GE.1.AND.i-n_interp/2+n_interp-1.LE.NXA3)THEN
             K=i-n_interp/2
          ELSEIF(i-n_interp/2.LT.1)THEN
             K=1
          ELSEIF(i-n_interp/2+n_interp-1.GT.NXA3)THEN
             K=NXA3+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #5 !"
             STOP
          ENDIF
          IF(iyy.LT.5)THEN
             DO j=iyy*NXSEG1+1,(iyy+1)*NXSEG1+1
                IF(YI(1).LE.YA3(j))EXIT
             ENDDO
          ELSE
             DO j=iyy*NXSEG1+1,NYA3
                IF(YI(1).LE.YA3(j))EXIT
             ENDDO
          ENDIF
          IF(j-n_interp/2.GE.1.AND.j-n_interp/2+n_interp-1.LE.NYA3)THEN
             L=j-n_interp/2
          ELSEIF(j-n_interp/2.LT.1)THEN
             L=1
          ELSEIF(j-n_interp/2+n_interp-1.GT.NYA3)THEN
             L=NYA3+1-n_interp
          ELSE
             WRITE(*,*)"Error: you cannot reach here #6 !"
             STOP
          ENDIF
          DO I=1,n_interp
             XD2_1D(I)=XA3(K+I-1)
             YD2_1D(I)=YA3(L+I-1)
          ENDDO
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=WA3(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d_qp(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          w=ZI(1)
          DO I=1,n_interp
             DO J=1,n_interp
                ZD2(I,J)=ZA3(K+I-1,L+J-1)
             ENDDO
          ENDDO
          CALL lagrange_interp_2d_qp(n_interp-1,n_interp-1,XD2_1D,YD2_1D,ZD2,1,XI,YI,ZI)
          z=ZI(1)
       ENDIF
    ELSE
       WRITE(*,*)"ERROR: Do not understand the region (xsb,xtb)=",xsb,xtb
       STOP
    ENDIF
    return
  end subroutine evaluate_wz_interp_qp

  subroutine evaluate_wz_direct_qp(xsb,xtb,w,z)
    ! calculate w and z from xsb and xtb directly
    ! xsb=-4*(w-z)**2/((1-w**2)*(1-z**2))
    ! xtb=-(w-z)**2/(w*z)
    IMPLICIT NONE
    REAL(KIND(1E0_16)),INTENT(IN)::xsb,xtb
    COMPLEX(KIND(1E0_16)),INTENT(OUT)::w,z
    REAL(KIND(1E0_16))::r1
    COMPLEX(KIND(1E0_16))::sq1,sq2,r2
    REAL(KIND(1E0_16))::xstb,xtb2,xsb2,fourmxsb,fourmxtb,temp
    REAL(KIND(1E0_16))::exp1,exp2,exp3,zden
    COMPLEX(KIND(1E0_16))::w2,znum,z12,sqrtz12
    REAL(KIND(1E0_16))::wr,wi,zr,zi
    ! 1-sqrt(2)
    REAL(KIND(1E0_16)),PARAMETER::onemroot2=-0.41421356237309504880168872420969807856967187537695E0_16
    IF(xsb.EQ.0E0_16.OR.xtb.EQ.0E0_16.OR.xsb+xtb.EQ.0E0_16)THEN
       ! xsb or xtb or xsb+xtb either is zero
       ! we randomly choose a point for w=z
       w=0.5E0_16
       z=0.5E0_16
       RETURN
    ENDIF
    xstb=xsb*xtb
    xtb2=xtb**2
    xsb2=xsb**2
    fourmxsb=4E0_16-xsb
    fourmxtb=4E0_16-xtb
    r1=-2E0_16+(2E0_16-4E0_16*xtb+xtb2)*(2E0_16*xsb2+4E0_16*fourmxsb*xstb+fourmxsb**2*xtb2)&
         /(2E0_16*xsb2)
    exp1=xstb-4E0_16*xsb-4E0_16*xtb
    exp2=exp1+2E0_16*xsb
    exp3=exp2+2E0_16*xtb
    temp=fourmxsb*fourmxtb*exp1
    IF((xsb.LT.0E0_16.AND.xtb.LT.0E0_16).or.&
         (xsb.GT.4E0_16.AND.xtb.LT.0E0_16.AND.xtb.GT.-xsb).or.&
         (xsb.LT.0E0_16.AND.xtb.GT.4E0_16.AND.xsb.GT.-xtb))THEN
       ! region 1 or regions (2,1)+(2,2) or regions (3,1)+(3,2)
       sq1=sqrt(cmplx(temp,0E0_16,kind=16))
       r2=temp*exp2*(2E0_16-xtb)*xtb/(2E0_16*xsb2*sq1)
       sq2=sqrt(cmplx(r1,0E0_16,kind=16)-r2)
       w2=-exp2*(2E0_16-xtb)/(4E0_16*xsb)+xtb/(4E0_16*xsb)*sq1-0.5E0_16*sq2
       zden=8E0_16*xstb*exp3
       z12=((8E0_16+sq1-4E0_16*xtb)*xtb+xsb*(2E0_16-xtb)**2)/(4E0_16*xsb)-0.5E0_16*sq2
       sqrtz12=sqrt(z12)
       znum=-xsb*(2E0_16-xtb)*(xsb2+8E0_16*xstb*(2E0_16-xtb)+16E0_16*xtb2)*sqrtz12&
            -(exp1+xsb)*exp2*(exp2+xsb)*z12*sqrtz12&
            -xsb*(exp1+xsb)*(exp2+xsb)*(2E0_16-xtb)*z12**2*sqrtz12&
            -xsb2*exp2*z12**3*sqrtz12
       w=sqrt(w2)
       z=znum/zden
       wr=real(w,kind=16)
       wi=0E0_16
       zr=real(z,kind=16)
       zi=0E0_16
       ! check
       IF(xsb.LT.0E0_16.AND.xtb.LT.0E0_16)THEN
          ! region 1
          IF(wr.LT.0E0_16.or.zr.GT.1E0_16.or.wr.GT.zr)THEN
             WRITE(*,*)"ERROR: evaluate_wz region 1:",xsb,xtb
             WRITE(*,*)"w=",wr
             WRITE(*,*)"z=",zr
             STOP
          ENDIF
       ELSEIF(xsb.GT.4E0_16.AND.xtb.LT.0E0_16)THEN
          ! regions (2,1)+(2,2)
          IF(wr.LT.0E0_16.or.wr.gt.1E0_16.or.zr.lt.1E0_16)THEN
             WRITE(*,*)"ERROR: evaluate_wz region (2,1):",xsb,xtb
             WRITE(*,*)"w=",wr
             WRITE(*,*)"z=",zr
             STOP
          ENDIF
          IF(zr.gt.1E0_16/wr.or.zr.gt.(1E0_16+wr)/(1E0_16-wr))THEN
             ! we should pick region (2,2)
             ! sq1 -> -sq1
             sq1=-sq1
             r2=temp*exp2*(2E0_16-xtb)*xtb/(2E0_16*xsb2*sq1)
             sq2=sqrt(cmplx(r1,0E0_16,kind=16)-r2)
             w2=-exp2*(2E0_16-xtb)/(4E0_16*xsb)+xtb/(4E0_16*xsb)*sq1-0.5E0_16*sq2
             zden=8E0_16*xstb*exp3
             z12=((8E0_16+sq1-4E0_16*xtb)*xtb+xsb*(2E0_16-xtb)**2)/(4E0_16*xsb)-0.5E0_16*sq2
             sqrtz12=sqrt(z12)
             znum=-xsb*(2E0_16-xtb)*(xsb2+8E0_16*xstb*(2E0_16-xtb)+16E0_16*xtb2)*sqrtz12&
                  -(exp1+xsb)*exp2*(exp2+xsb)*z12*sqrtz12&
                  -xsb*(exp1+xsb)*(exp2+xsb)*(2E0_16-xtb)*z12**2*sqrtz12&
                  -xsb2*exp2*z12**3*sqrtz12
             w=sqrt(w2)
             z=znum/zden
             wr=real(w,kind=16)
             zr=real(z,kind=16)
             IF(wr.LT.0E0_16.or.wr.gt.1E0_16.or.zr.lt.1E0_16.or.zr.gt.1E0_16/wr&
                  .or.zr.gt.(1E0_16+wr)/(1E0_16-wr))THEN
                WRITE(*,*)"ERROR: evaluate_wz region (2,2):",xsb,xtb
                WRITE(*,*)"w=",wr
                WRITE(*,*)"z=",zr
                STOP
             ENDIF
          ENDIF
       ELSEIF(xtb.GT.4E0_16.AND.xsb.LT.0E0_16)THEN
          ! regions (3,1)+(3,2)
          wr=-wr
          zr=-zr
          IF(wr.gt.0E0_16.or.wr.lt.-1E0_16.or.zr.lt.0E0_16.or.zr.gt.-wr&
               .or.zr.gt.(1E0_16+wr)/(1E0_16-wr))THEN
             WRITE(*,*)"ERROR: evaluate_wz region (3,1 or 2):",xsb,xtb
             WRITE(*,*)"w=",wr
             WRITE(*,*)"z=",zr
             STOP
          ENDIF
       ELSE
          WRITE(*,*)"ERROR: invalid region #1:",xsb,xtb
          STOP
       ENDIF
    ELSEIF(xsb.EQ.4E0_16.AND.xtb.LT.0E0_16.AND.xtb.GE.-4E0_16)THEN
       ! the special one
       wr=sqrt(1E0_16-xtb/2E0_16-0.5E0_16*sqrt(-xtb*fourmxtb))
       wi=0E0_16
       zr=1E0_16/wr
       zi=0E0_16
    ELSEIF(xsb.LT.0E0_16.AND.xtb.EQ.4E0_16.AND.xsb.GE.-4E0_16)THEN
       ! the special one
       wr=-sqrt(1E0_16-4E0_16*sqrt(fourmxsb/xsb2)-8E0_16/xsb)
       wi=0E0_16
       zr=-wr
       zi=0E0_16
    ELSEIF(xsb.GT.0E0_16.AND.xsb.LT.4E0_16.AND.xtb.LT.0E0_16.AND.xtb.GT.-xsb)THEN
       ! regions (2,3)+(2,4)
       sq1=sqrt(cmplx(temp,0E0_16,kind=16))
       r2=temp*exp2*(2E0_16-xtb)*xtb/(2E0_16*xsb2*sq1)
       sq2=sqrt(cmplx(r1,0E0_16,kind=16)-r2)
       w2=-exp2*(2E0_16-xtb)/(4E0_16*xsb)+xtb/(4E0_16*xsb)*sq1-0.5E0_16*sq2
       w=sqrt(w2)
       z=w/(abs(w)**2)
       wr=real(w,kind=16)
       wi=imagpart(w)
       zr=real(z,kind=16)
       zi=imagpart(z)
       ! check
       IF(wr.lt.0E0_16.or.wr.gt.1E0_16.or.wi.lt.0E0_16&
            .or.wi**2.lt.1E0_16-2E0_16*wr-wr**2&
            .or.wi**2.gt.1E0_16-wr**2)then
          ! sq2 -> -sq2
          sq2=-sq2
          w2=-exp2*(2E0_16-xtb)/(4E0_16*xsb)+xtb/(4E0_16*xsb)*sq1-0.5E0_16*sq2
          w=sqrt(w2)
          z=w/(abs(w)**2)
          wr=real(w,kind=16)
          wi=imagpart(w)
          zr=real(z,kind=16)
          zi=imagpart(z)
          IF(wr.lt.0E0_16.or.wr.gt.1E0_16.or.wi.lt.0E0_16&
               .or.wi**2.lt.1E0_16-2E0_16*wr-wr**2&
               .or.wi**2.gt.1E0_16-wr**2)then
             WRITE(*,*)"ERROR: evaluate_wz region (2,3 or 4):",xsb,xtb
             WRITE(*,*)"w=",w
             WRITE(*,*)"z=",z
             STOP
          ENDIF
       endif
    ELSEIF(xtb.GT.0E0_16.AND.xtb.LT.4E0_16.AND.xsb.LT.0E0_16.AND.xsb.GT.-xtb)THEN
       ! regions (3,3)+(3,4)
       sq1=sqrt(cmplx(temp,0E0_16,kind=16))
       r2=temp*exp2*(2E0_16-xtb)*xtb/(2E0_16*xsb2*sq1)
       sq2=sqrt(cmplx(r1,0E0_16,kind=16)-r2)
       w2=-exp2*(2E0_16-xtb)/(4E0_16*xsb)+xtb/(4E0_16*xsb)*sq1-0.5E0_16*sq2
       z=sqrt(w2)
       w=-conjg(z)
       wr=real(w,kind=16)
       wi=imagpart(w)
       zr=real(z,kind=16)
       zi=imagpart(z)
       ! check
       IF(wr.lt.onemroot2.or.wr.gt.0E0_16.or.wi.lt.0E0_16&
            .or.wi**2.gt.1E0_16+2E0_16*wr-wr**2)then
          ! sq2 -> -sq2
          sq2=-sq2
          w2=-exp2*(2E0_16-xtb)/(4E0_16*xsb)+xtb/(4E0_16*xsb)*sq1-0.5E0_16*sq2
          z=sqrt(w2)
          w=-conjg(z)
          wr=real(w,kind=16)
          wi=imagpart(w)
          zr=real(z,kind=16)
          zi=imagpart(z)
          IF(wr.lt.onemroot2.or.wr.gt.0E0_16.or.wi.lt.0E0_16&
               .or.wi**2.gt.1E0_16+2E0_16*wr-wr**2)then
             WRITE(*,*)"ERROR: evaluate_wz region (3,3 or 4):",xsb,xtb
             WRITE(*,*)"w=",w
             WRITE(*,*)"z=",z
             STOP
          ENDIF
       endif
    ELSE
       WRITE(*,*)"ERROR: invalid region #0:",xsb,xtb
       STOP
    ENDIF
    w=cmplx(wr,wi,kind=16)
    z=cmplx(zr,zi,kind=16)
    RETURN
  END subroutine evaluate_wz_direct_qp
END MODULE express_wz
