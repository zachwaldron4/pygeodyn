!
        FUNCTION  FSPL8 ( XC, N, X, Y, IXC, COEF )
! ************************************************************************
! *                                                                      *
! *     Function  FSPL8  computes the interploated value of the function *
! *   at the point XC using the coefficients of the cubic spline COEF    *
! *   computed by the program MAKE_SPLINE before the call of FSPL8.      *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of FSPL8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the fucntino Y(X) which is       *
! *   equal or greater than XC.                                          *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -12345 means that XC < X(1).            *
! *                        IXC =  12345 means that XC > X(N).            *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). DimensionL N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <FSPL8> ( REAL*8  ) -- Value of the cubic spline interpolating the   *
! *                        the function Y(X) in the point XC.            *
! *                                                                      *
! *  Comment: IF XC is out of range [ X(1), X(N) ], then extrapolation   *
! *  is applied and the precision of the result is not guaranteed.       *
! *                                                                      *
! *  ###  05-JUL-1995     FSPL8    v1.0  (c) L. Petrov 06-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER N, IXC
        DOUBLE PRECISION FSPL8, XC, X(N), Y(N), COEF(N)
        DOUBLE PRECISION H, DH, B, C, D
!
! ----- Check of the number of actual arguments
!
!!        CALL  VER$ARG ( 6 )
!
        IF ( IXC.LE.-12345 ) THEN
!
! ---------- Extrapolation beyond the left end of the interval
!
             DH = X(2)-X(1)
             D  = ( COEF(2) - COEF(1) ) / 3.D0
             C  = COEF(1)
             B  = ( Y(2) - Y(1) ) / DH  - D*DH**2 -C*DH
             H  = XC-X(1)
             FSPL8 = Y(1) + B*H
          ELSE IF ( IXC.EQ.N .OR. IXC.GE.12345 ) THEN
!
! ---------- Extrapolation beyond the right end edge of the interval
!
             DH = X(N)-X(N-1)
             D  = ( COEF(N) - COEF(N-1) ) / 3.D0
             C  = COEF(N-1)
             B  = ( Y(N) - Y(N-1) ) / DH  - D*DH**2 -C*DH
             H  = XC - X(N)
             FSPL8 = Y(N) + ( B + 2.D0*C*DH + 3.D0*D*DH**2 ) *H
          ELSE
!
! ---------- Point XC turned out inside the interploation range.
!
             DH = X(IXC+1)-X(IXC)
             D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
             C  = COEF(IXC)
             B  = ( Y(IXC+1) - Y(IXC) ) / DH  - D*DH**2 - C*DH
             H  = XC-X(IXC)
             FSPL8 = Y(IXC) + B*H + C*H**2 + D*H**3
        END IF
!
        RETURN
        END  !#!  FSPL8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  DSPL8 ( XC, N, X, Y, IXC, COEF )
! ************************************************************************
! *                                                                      *
! *     Function  DSPL8  computes the interploated value of the first    *
! *   derivative of the function Y(X) at the point XC using the          *
! *   coefficients of the cubic spline COEF computed by the program      *
! *   MAKE_SPLINE before the call of DSPL8.                              *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of DSPL8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the fucntino Y(X) which is       *
! *   equal or greater than XC.                                          *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -12345 means that XC < X(1).            *
! *                        IXC =  12345 means that XC > X(N).            *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). DimensionL N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <DSPL8> ( REAL*8  ) -- Value of the first derivative of the cubic    *
! *                        spline interpolating the the function Y(X)    *
! *                        in the point XC.                              *
! *                                                                      *
! *  Comment: IF XC is out of range [ X(1), X(N) ], then extrapolation   *
! *  is applied and the precision of the result is not guaranteed.       *
! *                                                                      *
! *  ###  06-JUL-1995    DSPL8    v1.0  (c) L. Petrov  06-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER N, IXC
        DOUBLE PRECISION  DSPL8, XC, X(N), Y(N), COEF(N)
        DOUBLE PRECISION H, DH, B, C, D
!
! ----- Check of the number of actual arguments
!
!!        CALL  VER$ARG ( 6 )
!
        IF ( IXC.LE.-12345 ) THEN
!
! ---------- Extrapolation beyond the left end of the interval
!
             DH = X(2)-X(1)
             D  = ( COEF(2) - COEF(1) ) / 3.D0
             C  = COEF(1)
             B  = ( Y(2) - Y(1) ) / DH  - D*DH**2 -C*DH
             DSPL8 = B
          ELSE IF ( IXC.EQ.N .OR. IXC.GE.12345 ) THEN
!
! ---------- Extrapolation beyond the right end edge of the interval
!
!
             DH = X(N)-X(N-1)
             D  = ( COEF(N) - COEF(N-1) ) / 3.D0
             C  = COEF(N-1)
             B  = ( Y(N) - Y(N-1) ) / DH  - D*DH**2 -C*DH
             DSPL8 = B + 2.D0*C*DH + 3.D0*D*DH**2
          ELSE
!
! ---------- Point XC turned out inside the interploation range.
!
             DH = X(IXC+1)-X(IXC)
             D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
             C  = COEF(IXC)
             B  = ( Y(IXC+1) - Y(IXC) ) / DH  - D*DH**2 - C*DH
             H  = XC-X(IXC)
             DSPL8 = B + 2.0D0*C*H + 3.D0*D*H**2
        END IF
!
        RETURN
        END  !#!  DSPL8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  D2SPL8 ( XC, N, X, Y, IXC, COEF )
! ************************************************************************
! *                                                                      *
! *     Function  D2SPL8  computes the interploated value of the second  *
! *   derivative of the function Y(X) at the point XC using the          *
! *   coefficients of the cubic spline COEF computed by the program      *
! *   MAKE_SPLINE before the call of D2SPL8.                              *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of DSPL8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the fucntino Y(X) which is       *
! *   equal or greater than XC.                                          *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -12345 means that XC < X(1).            *
! *                        IXC =  12345 means that XC > X(N).            *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). DimensionL N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <D2SPL8> ( REAL*8 ) -- Value of the second derivative of the cubic   *
! *                        spline interpolating the the function Y(X)    *
! *                        in the point XC.                              *
! *                                                                      *
! *  Comment: IF XC is out of range [ X(1), X(N) ], then extrapolation   *
! *  is applied and the precision of the result is not guaranteed.       *
! *                                                                      *
! *  ###  06-JUL-1995    D2SPL8   v1.0  (c) L. Petrov  04-JUN-2004  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER N, IXC
        DOUBLE PRECISION H, DH, C, D, D2SPL8, XC, X(N), Y(N), COEF(N)
!
! ----- Check of the number of actual arguments
!
!!        CALL  VER$ARG ( 6 )
!
        IF ( IXC.LE.-12345 ) THEN
             D2SPL8 = 0.0D0
          ELSE IF ( IXC.EQ.N .OR. IXC.GE.12345 ) THEN
             D2SPL8 = 0.0D0
          ELSE
!
! ---------- Point XC turned out inside the interploation range.
!
             DH = X(IXC+1)-X(IXC)
             D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
             C  = COEF(IXC)
             H  = XC-X(IXC)
             D2SPL8 = 2.0D0*C + 6.D0*D*H
        END IF
!
        RETURN
        END  !#!  D2SPL8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION  ISPL8 ( XC, N, X, Y, IBG, IXC, COEF, IUER )
! ************************************************************************
! *                                                                      *
! *     Function  DSPL8  computes the interploated value of the integral *
! *   of the function Y(X) in limints [ X(IBG), XC ] using the           *
! *   coefficients of the cubic spline COEF computed by the program      *
! *   MAKE_SPLINE before the call of ISPL8.                              *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of DSPL8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the fucntino Y(X) which is       *
! *   equal or greater than XC.                                          *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! *     The follosing expression should be satisifed:                    *
! *        X(1) =< XC =< X(N)                                            *
! *        IBG < IXC                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points for function X. Should be    *
! *                        same as the number of points used for         *
! *                        computation of spline coefficients.           *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -12345 means that XC < X(1).            *
! *                        IXC =  12345 means that XC > X(N).            *
! *  COEF ( REAL*8    ) -- Array of spline coeffients computed by        *
! *                        program MAKE_SPLINE ( coeffients at the       *
! *                        quadratic term). DimensionL N.                *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * <ISPL8> ( REAL*8  ) -- Value of the integral in limits [X(IBG), XC]  *
! *                        of the cubic spline interpolating the         *
! *                        function Y(X) in the point XC.                *
! *                                                                      *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER N, IBG, IXC, IUER
        DOUBLE PRECISION ISPL8, XC, X(N), Y(N), COEF(N)
        DOUBLE PRECISION H, DH, B, C, D
        INTEGER J1
!C
!C .....................\\\
!C                       \\\
!        LOGICAL PRESENT
!        INTEGER*4 NUM$ARG, NA, N_ARG
!C
!        PARAMETER ( N_ARG=8 )  !  The number of formal parameters
!C
!C ----- Check whehter the number of formal arguments is equal to the number
!C ----- of actual arguments
!C
!        NA=NUM$ARG()  !  Number of actual arguiments
!        IF ( .NOT. ( NA.EQ.N_ARG .OR. ( .NOT. PRESENT ( IUER, N_ARG )
!     $       .AND.   NA.EQ.(N_ARG-1) ) ) )    CALL  VER$ARG ( N_ARG )
!                        ///
! ......................///  ...   end of check ...
!
!
! ----- Additional tests
!
        ISPL8=-1.11111111111111111D11
!
        ISPL8 = 0.D0
        IF ( IXC.GT.IBG ) THEN
!
! ---------- find the integral for complete nodes
!
             DO 410 J1=IBG,IXC-1
                DH = X(J1+1)-X(J1)
                D  = ( COEF(J1+1) - COEF(J1) ) / (3.D0*DH)
                C  = COEF(J1)
                B  = ( Y(J1+1) - Y(J1) ) / DH  - D*DH**2 - C*DH
                ISPL8 = ISPL8 + Y(J1)*DH + B*DH**2/2.D0 + &
     &                  C*DH**3/3.D0 + D*DH**4/4.D0
  410        CONTINUE
        END IF
!
! ----- Find the integral in the incomplete node
!
        DH = X(IXC+1)-X(IXC)
        D  = ( COEF(IXC+1) - COEF(IXC) ) / (3.D0*DH)
        C  = COEF(IXC)
        B  = ( Y(IXC+1) - Y(IXC) ) / DH  - D*DH**2 - C*DH
        H  = XC-X(IXC)
        ISPL8 = ISPL8 + Y(IXC)*H + B*H**2/2.D0 + C*H**3/3.D0 + &
     &          D*H**4/4.D0
!
        RETURN
        END  !#!  ISPL8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION IXMN8 ( N, X, X0 )
! ************************************************************************
! *                                                                      *
! *     Finction  IXMN8  finds the index of the element X in the array   *
! *   X of N elements, which is minimal among those elements which       *
! *   exceed X0. Binary search method is used.                           *
! *   If X(I) > X for every I then IXMN8_S = -12345                      *
! *   If X(I) < X for every I then IXMN8_S =  12345                      *
! *   If X0 = X(1) then IXMN8 = 1                                        *
! *   If X0 = X(N) then IXMN8 = N                                        *
! *                                                                      *
! *   NB: array X should be ordered in the order of increasing its       *
! *   elements.                                                          *
! *                        ______  Example: ______                       *
! *                                                                      *
! *       X(1)  X(2)  X(3)  X(4)   ...        X(N-1)  X(N)               *
! *        !-----!-----!-----!------!---- ...  --!-----!                 *
! *      !    !           !                        !   !        !        *
! *     C1   C2          C3                       C4   C5      C6        *
! *                                                                      *
! *     Value of IXMN8_S ( 1, N, X, CK ):                                *
! *                                                                      *
! *         for C1  -12345                                               *
! *         for C2   2                                                   *
! *         for C3   4                                                   *
! *         for C4   N                                                   *
! *         for C5   N                                                   *
! *         for C6   12345                                               *
! *                                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- The number of elements in array X.          *
! *       X ( REAL*8    ) -- Array ordered in increasing the elements.   *
! *                          dimension: N.                               *
! *      X0 ( REAL*8    ) -- The point under investigation.              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <IXMN8> ( INTEGER*4 ) -- Index of the pivot element.                 *
! *                                                                      *
! * ###  20-OCT-1989     IXMN8    v1.0 (c) L. Petrov  04-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER  N, IXMN8
        DOUBLE PRECISION X(N), X0
        INTEGER    IL, IR, IM
!
! ----- Check of the number of actual arguments
!
!!        CALL VER$ARG ( 3 )
!
        IF ( X0.LT.X(1) ) THEN
             IXMN8=-12345
             RETURN
        END IF
!
        IF ( X0.GT.X(N) ) THEN
             IXMN8=12345
             RETURN
        END IF
!
        IF ( X0.EQ.X(N) ) THEN
             IXMN8=N
             RETURN
        END IF
!
        IL=0
        IR=N
!
! ----- Binary search
!
  910   CONTINUE
           IF(IL.GE.IR) GOTO 810
           IM=(IL+IR)/2
           IF( X0.GE.X(IM)  ) IL=IM
           IF( X0.LT.X(IM)  ) IR=IM
           IF( (IR-IL).LE.1 ) GOTO 810
        GOTO 910
  810   CONTINUE
        IXMN8=IL
        RETURN
        END  !#!  IXMN8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION IXMN8_S ( IX_LAST, N, X, X0 )
! ************************************************************************
! *                                                                      *
! *     Finction  IXMN8_S  find the index of the element X in the array  *
! *   X of N elements, which is minimal among those elements which       *
! *   exceed X0. Sequential search method is used. The serach starts     *
! *   from the element IX_LAST.                                          *
! *   If X(I) > X for every I then IXMN8_S = -12345                      *
! *   If X(I) < X for every I then IXMN8_S =  12345                      *
! *   If X0 = X(IX_LAST) then IXMN8_S = N                                *
! *   If X0 = X(N) then IXMN8_S = N                                      *
! *                                                                      *
! *   NB: array X should be ordered in the order of increasing its       *
! *   elements.                                                          *
! *                        ______  Example: ______                       *
! *                                                                      *
! *       X(1)  X(2)  X(3)  X(4)   ...        X(N-1)  X(N)               *
! *        !-----!-----!-----!------!---- ...  --!-----!                 *
! *      !    !           !                        !   !        !        *
! *     C1   C2          C3                       C4   C5      C6        *
! *                                                                      *
! *     Value of IXMN8_S ( 1, N, X, CK ):                                *
! *                                                                      *
! *         for C1  -12345                                               *
! *         for C2   2                                                   *
! *         for C3   4                                                   *
! *         for C4   N                                                   *
! *         for C5   N                                                   *
! *         for C6   12345                                               *
! *                                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * IX_LAST ( INTEGER*4 ) -- The element from whcih the search starts.   *
! *       N ( INTEGER*4 ) -- The number of elements in array X.          *
! *       X ( REAL*8    ) -- Array ordered in increasing the elements.   *
! *                          dimension: N.                               *
! *      X0 ( REAL*8    ) -- The poinst under investigation.             *
! *                                                                      *
! * _______________________ Modified parameters ________________________ *
! *                                                                      *
! *   <IXMN8_S> ( INTEGER*4 ) -- Index of the pivot element.             *
! *                                                                      *
! *  ###  20-OCT-1989    IXMN8_S    v1.0 (c) L. Petrov 04-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER IXMN8_S, IX_LAST, N
        DOUBLE PRECISION X(N), X0
        INTEGER   J1, IXB, IXMN8
!
! ----- Check of the number of actual arguments
!
!!        CALL VER$ARG ( 4 )
!
        IXB=IX_LAST-1
        IF ( IXB.LE.0 ) IXB=1
!
        IF ( IXB.GE.N ) THEN
             IXMN8_S=12345
             IF ( IXB.EQ.N .AND. X(N).EQ.X0 ) IXMN8_S=N
             RETURN
        END IF
!
        IF ( X(IXB).GE.X0 ) THEN
             IF ( IXB.EQ.1 ) IXMN8_S=-12345
             IF ( IXB.EQ.1 .AND. X(1).EQ.X0 ) IXMN8_S=1
             IF ( IXB.NE.1 ) IXMN8_S=IXMN8 ( N, X, X0 )
             RETURN
        END IF
!
        DO 410 J1=IXB+1,N
           IF ( X(J1).GT.X0 ) GOTO 810
  410   CONTINUE
        IXMN8_S=12345
        IF ( X(N).EQ.X0 ) IXMN8_S=N
        RETURN
!
  810   CONTINUE
        IXMN8_S=J1-1
        RETURN
        END  !#!  IXMN8_S  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION FLIN8 ( XC, N, X, Y, IXC )
! ************************************************************************
! *                                                                      *
! *     Function  FLIN8  computes at the point XC the value of           *
! *   function Y(X), defined as a table of N paris argument/value, using *
! *   linear interpolation method.                                       *
! *   Argument IXC -- the index of pivot element should be computed      *
! *   before call of FLIN8 using function IXMN8 or IXMN8_S. The pivot    *
! *   element is the minimal element of the argument X which is equal    *
! *   or greater than XC.                                                *
! *                                                                      *
! *     ----------X(IXC-1)------X(IXC)-------X(IXC+1)------X(IXC+2)---   *
! *                                     ^                                *
! *                                     |                                *
! *                                     XC                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    XC ( REAL*8    ) -- Value of the argument for which the value of  *
! *                        cubic spline is computed.                     *
! *     N ( INTEGER*4 ) -- Number of points used for computation of      *
! *                        spline coefficients.                          *
! *     X ( REAL*8    ) -- Array of arguments of the function under      *
! *                        consideration.                                *
! *     Y ( REAL*8    ) -- Array of values of the function under         *
! *                        consideration.                                *
! *   IXC ( INTEGER*4 ) -- Index opf the pivot element.                  *
! *                        IXC = -12345 means that XC < X(1).            *
! *                        IXC =  12345 means that XC > X(N).            *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *  <FLIN8> ( REAL*8    ) -- Interpolated value in the point XC.        *
! *                                                                      *
! *  ###  20-OCT-1989    FLIN8    v 1.0 (c) L. Petrov  04-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER N, IXC, JXC, JXL
        DOUBLE PRECISION FLIN8, XC, X(N), Y(N), A, B
!
! ----- Check of the number of actual arguments
!
!!        CALL VER$ARG ( 5 )
!
        JXC=IXC+1
        JXL=IXC
!
        IF ( JXL.LT.1 ) THEN
             JXC=2
             JXL=1
        END IF
!
        IF ( JXC.GT.N ) THEN
             JXL=N-1
             JXC=N
        END IF
!
! ----- Y(X)=A*X+B
!
        A=( Y(JXC)-Y(JXL) )/( X(JXC)-X(JXL) )
        B=Y(JXL)-A*X(JXL)
        FLIN8=A*XC+B
!
        RETURN
        END
