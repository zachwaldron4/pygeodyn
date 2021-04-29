!$TESTS
      SUBROUTINE TESTS(XS,SUNV,XAXIS,YAXIS,ZAXIS,DXAXIS,DYAXIS,DZAXIS, &
     &                 DBDX,LACC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z),LOGICAL(L)
      DIMENSION XS(3),SUNV(3),SUNT(3),XAXIS(3),YAXIS(3),ZAXIS(3)
      DIMENSION XS2(3),XAXIS2(3),YAXIS2(3),ZAXIS2(3)
      DIMENSION DXAXIS(3,3),DYAXIS(3,3),DZAXIS(3,3)
      DIMENSION DBDX(3)
      DATA THOU/1000.D0/
!
!***********************************************************************
! START OF EXECUTABLE CODE *********************************************
!***********************************************************************
!
      ZMAG=SQRT(XS(1)*XS(1)+XS(2)*XS(2)+XS(3)*XS(3))
      ZAXIS(1)=-XS(1)/ZMAG
      ZAXIS(2)=-XS(2)/ZMAG
      ZAXIS(3)=-XS(3)/ZMAG
      SUNT(1)=SUNV(1)-XS(1)
      SUNT(2)=SUNV(2)-XS(2)
      SUNT(3)=SUNV(3)-XS(3)
      SMAG=SQRT(SUNT(1)*SUNT(1)+SUNT(2)*SUNT(2)+SUNT(3)*SUNT(3))
      SUNT(1)=SUNT(1)/SMAG
      SUNT(2)=SUNT(2)/SMAG
      SUNT(3)=SUNT(3)/SMAG
      BANG=ACOS(SUNT(1)*ZAXIS(1)+SUNT(2)*ZAXIS(2)+SUNT(3)*ZAXIS(3))
      CALL CROSPR(ZAXIS,SUNT,YAXIS)
      YMAG=SQRT(YAXIS(1)*YAXIS(1)+YAXIS(2)*YAXIS(2)+YAXIS(3)*YAXIS(3))
      YAXIS(1)=YAXIS(1)/YMAG
      YAXIS(2)=YAXIS(2)/YMAG
      YAXIS(3)=YAXIS(3)/YMAG
      IF(LACC) THEN
         CALL CROSPR(YAXIS,SUNT,XAXIS)
      ELSE
         CALL CROSPR(YAXIS,ZAXIS,XAXIS)
      ENDIF
!
!
      DO 100 K=1,3
      XS2(1)=XS(1)
      XS2(2)=XS(2)
      XS2(3)=XS(3)
      IF(K.EQ.1) XS2(1)=XS2(1)+THOU
      IF(K.EQ.2) XS2(2)=XS2(2)+THOU
      IF(K.EQ.3) XS2(3)=XS2(3)+THOU
! *****
      ZMAG=SQRT(XS2(1)*XS2(1)+XS2(2)*XS2(2)+XS2(3)*XS2(3))
      ZAXIS2(1)=-XS2(1)/ZMAG
      ZAXIS2(2)=-XS2(2)/ZMAG
      ZAXIS2(3)=-XS2(3)/ZMAG
      SUNT(1)=SUNV(1)-XS2(1)
      SUNT(2)=SUNV(2)-XS2(2)
      SUNT(3)=SUNV(3)-XS2(3)
      SMAG=SQRT(SUNT(1)*SUNT(1)+SUNT(2)*SUNT(2)                        &
     &          +SUNT(3)*SUNT(3))
      SUNT(1)=SUNT(1)/SMAG
      SUNT(2)=SUNT(2)/SMAG
      SUNT(3)=SUNT(3)/SMAG
      BANG2=ACOS(SUNT(1)*ZAXIS2(1)+SUNT(2)*ZAXIS2(2)                    &
     &          +SUNT(3)*ZAXIS2(3))
      CALL CROSPR(ZAXIS2,SUNT,YAXIS2)
      YMAG=SQRT(YAXIS2(1)*YAXIS2(1)+YAXIS2(2)*YAXIS2(2)                &
     &          +YAXIS2(3)*YAXIS2(3))
      YAXIS2(1)=YAXIS2(1)/YMAG
      YAXIS2(2)=YAXIS2(2)/YMAG
      YAXIS2(3)=YAXIS2(3)/YMAG
      IF(LACC) THEN
         CALL CROSPR(YAXIS2,SUNT,XAXIS2)
      ELSE
         CALL CROSPR(YAXIS2,ZAXIS2,XAXIS2)
      ENDIF
! *****
      DO 50 J=1,3
      DXAXIS(J,K)=(XAXIS2(J)-XAXIS(J))/THOU
      DYAXIS(J,K)=(YAXIS2(J)-YAXIS(J))/THOU
      DZAXIS(J,K)=(ZAXIS2(J)-ZAXIS(J))/THOU
   50 END DO
      DBDX(K)=(BANG2-BANG)/THOU
  100 END DO
      RETURN
      END
