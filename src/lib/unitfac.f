
        REAL FUNCTION UNITFAC( UNIT1, UNIT2, LNUM )

C***********************************************************************
C  function body starts at line
C
C  DESCRIPTION:
C      This function returns a units conversion factor based on the numerator
C      (if any) of argument 1, and the numerator (if any) of argument two.
C      If there are no units quotients, it treats the units field like a 
C      numerator.
C
C  PRECONDITIONS REQUIRED:

C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C     Created by M. Houyoux 10/99
C
C**************************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id$
C
C COPYRIGHT (C) 1999, MCNC--North Carolina Supercomputing Center
C All Rights Reserved
C
C See file COPYRIGHT for conditions of use.
C
C Environmental Programs Group
C MCNC--North Carolina Supercomputing Center
C P.O. Box 12889
C Research Triangle Park, NC  27709-2889
C
C env_progs@mcnc.org
C
C Pathname: $Source$
C Last updated: %G 
C
C***************************************************************************

        IMPLICIT NONE

C.........  INCLUDES:
        INCLUDE 'IOCNST3.EXT'   !  I/O API constants
        INCLUDE 'IOSTRG3.EXT'   !  I/O string lengths

C...........   EXTERNAL FUNCTIONS and their descriptions:
        
        CHARACTER*2  CRLF
        EXTERNAL     CRLF

C...........   SUBROUTINE ARGUMENTS
        CHARACTER(*), INTENT (IN) :: UNIT1    ! first unit
        CHARACTER(*), INTENT (IN) :: UNIT2    ! second unit
        LOGICAL     , INTENT (IN) :: LNUM     ! true: for numerators

C...........   Other local variables
        INTEGER         K1, K2, L1, L2, LB1, LB2
        INTEGER         EXP1       ! exponent on first unit
        INTEGER         EXP2       ! exponent on second unit

        REAL            PRECNV     ! Conversion for exponential prefixes

        CHARACTER(LEN=IOULEN3 ) :: BUF1 = ' '
        CHARACTER(LEN=IOULEN3 ) :: BUF2 = ' '

        CHARACTER*16 :: PROGNAME = 'UNITFAC' ! program name

C***********************************************************************
C   begin body of function UNITFAC

C.........  Retrieve length of unit strings
        L1 = LEN_TRIM( UNIT1 )
        L2 = LEN_TRIM( UNIT2 )

C.........  Cases where one or more of the provided units are blank
        IF( L1 .LE. 0 .OR. L2 .LE. 0 ) THEN

            UNITFAC = 1.0
            RETURN

        END IF

C.........  Determine positions of the divide-by symbol in the units
        K1 = INDEX( UNIT1, '/' )
        K2 = INDEX( UNIT2, '/' )

C.........  Define buffers as numerators or denominators, depending on
C           value of LNUM
        IF( LNUM ) THEN
            IF( K1 .LE. 0 ) K1 = L1 + 1
            IF( K2 .LE. 0 ) K2 = L2 + 1
            BUF1 = UNIT1( 1:K1-1 )
            BUF2 = UNIT2( 1:K2-1 )

        ELSE
            IF( K1 .LE. 0 ) K1 = 0
            IF( K2 .LE. 0 ) K2 = 0
            BUF1 = UNIT1( K1+1:L1 )
            BUF2 = UNIT2( K2+1:L2 )

        END IF

        LB1  = LEN_TRIM( BUF1 )
        LB2  = LEN_TRIM( BUF2 )

        CALL UNITMATCH( BUF1 )
        CALL UNITMATCH( BUF2 )

C.........  Check if units are the same and return a 1.
        IF( BUF1 .EQ. BUF2 ) THEN
            UNITFAC = 1.
            RETURN
        END IF

C.........  Separate out any leading adjustments (e.g., 10E6)
        EXP1 = 0
        EXP2 = 0
        K1 = INDEX( BUF1, '10E' )
        K2 = INDEX( BUF2, '10E' )

        IF( K1 .GT. 0 ) THEN
            I = INDEX( BUF1, ' ' ) 
            IF( I .GT. 0 ) THEN
                EXP1 = STR2INT( BUF1( K1+3:I ) )
                BUF1 = BUF1( I+1:LB1 )
            END IF
        END IF

        IF( K2 .GT. 0 ) THEN
            I = INDEX( BUF2, ' ' ) 
            IF( I .GT. 0 ) THEN
                EXP2 = STR2INT( BUF2( K2+3:I ) )
                BUF1 = BUF2( I+1:LB2 )
            END IF
        END IF

C.........  Set conversion factor by comparing units prefixes
        PRECNV = 10**( EXP1 - EXP2 )

C.........  Set conversion factor by comparing numerators of main units
        SELECT CASE( BUF1 )

            CASE( 'g' )
                
                SELECT CASE( BUF2 )
                    CASE( 'kg' )
                        UNITFAC = 1. / 1000.
                    CASE( 'tons' )
                        UNITFAC = GM2TON
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'kg' )
                
                SELECT CASE( BUF2 )
                    CASE( 'g' )
                        UNITFAC = 1000.
                    CASE( 'tons' )
                        UNITFAC = GM2TON / 1000.
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'tons' )

                SELECT CASE( BUF2 )
                    CASE( 'g' )
                        UNITFAC = TON2GM
                    CASE( 'kg' )
                        UNITFAC = TON2GM / 1000.
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'yr' )

                SELECT CASE( BUF2 )
                    CASE( 'day' )
                        UNITFAC = 365.
                    CASE( 'hr' )
                        UNITFAC = 365. * 24.
                    CASE( 'min' )
                        UNITFAC = 365. * 24. * 60.
                    CASE( 's' )
                        UNITFAC = 365. * 24. * 3600.
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'day' )

                 SELECT CASE( BUF2 )
                    CASE( 'yr' )
                        UNITFAC = 1. / 365.
                    CASE( 'hr' )
                        UNITFAC = 24.
                    CASE( 'min' )
                        UNITFAC = 24. * 60.
                    CASE( 's' )
                        UNITFAC = 24. * 3600.
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'hr' )
 
                SELECT CASE( BUF2 )
                    CASE( 'yr' )
                        UNITFAC = 1. / ( 365. * 24. )
                    CASE( 'day' )
                        UNITFAC = 1. / 24.
                    CASE( 'min' )
                        UNITFAC = 60.
                    CASE( 's' )
                        UNITFAC = 3600.
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'min' )

                SELECT CASE( BUF2 )
                    CASE( 'yr' )
                        UNITFAC = 1. / ( 365. * 24. * 60. )
                    CASE( 'day' )
                        UNITFAC = 1. / ( 24. * 60. )
                    CASE( 'hr' )
                        UNITFAC = 1. / 60.
                    CASE( 's' )
                        UNITFAC = 60.
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 's' )

                SELECT CASE( BUF2 )
                    CASE( 'yr' )
                        UNITFAC = 1. / ( 365. * 24. * 3600. )
                    CASE( 'day' )
                        UNITFAC = 1. / ( 24. * 3600. )
                    CASE( 'hr' )
                        UNITFAC = 1. / 3600.
                    CASE( 'min' )
                        UNITFAC = 1. / 60.
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'km' )

                SELECT CASE( BUF2 )
                    CASE( 'mile' )
                        UNITFAC = 0.6213712
                    CASE( 'm' )
                        UNITFAC = 1000.
                    CASE( 'ft' )
                        UNITFAC = 3280.84
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'mile' )
                SELECT CASE( BUF2 )
                    CASE( 'km' )
                        UNITFAC = 1.609344
                    CASE( 'm' )
                        UNITFAC = 1609.344
                    CASE( 'ft' )
                        UNITFAC = 5280.
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'm' )

                SELECT CASE( BUF2 )
                    CASE( 'km' )
                        UNITFAC = 0.001
                    CASE( 'mile' )
                        UNITFAC = 6.213712E-4
                    CASE( 'ft' )
                        UNITFAC = 3.2808399
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE( 'ft' )

                SELECT CASE( BUF2 )
                    CASE( 'km' )
                        UNITFAC = 0.0003048
                    CASE( 'mile' )
                        UNITFAC = 1.893939E-4
                    CASE( 'm' )
                        UNITFAC = 0.3048
                    CASE DEFAULT
                        CALL CASE_NOT_FOUND( UNITFAC )
                END SELECT

            CASE DEFAULT
                CALL CASE_NOT_FOUND( UNITFAC )

        END SELECT

C.........  Inckude any prefix adjustments
        UNITFAC = UNITFAC * PRECNV
        
        RETURN

C******************  INTERNAL SUBPROGRAMS  *****************************

        CONTAINS

C.............  This subprogram checks for an internal error of the
C               units conversion case not being programmed in the code
            SUBROUTINE CASE_NOT_FOUND( FACTOR )

C.............  Subprogram arguments
            REAL, INTENT (OUT) :: FACTOR

C.............   Local variables
            CHARACTER*300  MESG

C----------------------------------------------------------------------

            MESG = 'NOTE: Units conversion case not found for: "' //
     &             BUF1( 1:LB1 ) // '" with "' // BUF2( 1:LB2 ) // '"'//
     &             CRLF() // BLANK10// 'No conversion will be made.'
            CALL M3MSG2( MESG )
            FACTOR = 1.

            RETURN

            END SUBROUTINE CASE_NOT_FOUND

        END FUNCTION UNITFAC

