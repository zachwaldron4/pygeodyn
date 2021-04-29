!
        FUNCTION   IFIND_PL ( L, LIS, NEL )
! ************************************************************************
! *                                                                      *
! *     Finction  IFIND_PL  scans the list LIS which has L elements, and *
! *   search the element NEL. If it finds the element NEL, IFIND_PL      *
! *   returns its index, if not IFIND_PL returns -1.                     *
! *                                                                      *
! *                                                                      *
! * _________________________ Input paramters: _________________________ *
! *                                                                      *
! *       M ( INTEGER*4 )  --  Maximal length of the list LIS.           *
! *    IPAR ( INTEGER*4 )  --  The element under investigation.          *
! *     NEL ( INTEGER*4 )  --  Element under consideration.              *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! *  <IFIND_PL> ( INTEGER*4 )  --  Index of NEL in the list LIS. If not  *
! *                                found, then -1.                       *
! *                                                                      *
! *  ###  31-JAN-1992   IFIND_PL   v2.1 (c)  L. Petrov 04-JAN-1994  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER  IFIND_PL, L, LIS(*), NEL
        IF ( L.LT.1 ) THEN
             IFIND_PL=-2
             RETURN
        END IF
        DO 410 J1=1,L
           IF ( LIS(J1).EQ.NEL ) THEN
                IFIND_PL=J1
                RETURN
           END IF
  410   CONTINUE
        IFIND_PL=-1
        RETURN
        END
