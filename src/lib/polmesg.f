
        SUBROUTINE POLMESG( NLIST, NAMES )

C***********************************************************************
C  subroutine body starts at line 
C
C  DESCRIPTION:
C      This subroutine writes out a message stating that the pollutants or
C      emission types in the argument list are being processed.
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C      Subroutines: I/O API subroutines
C
C  REVISION  HISTORY:
C      Created 3/99 by M. Houyoux
C
C************************************************************************
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
C Last updated: $Date$ 
C
C***************************************************************************

        IMPLICIT NONE

C...........   INCLUDES

        INCLUDE 'IOCNST3.EXT'   !  emissions constant parameters

C...........   EXTERNAL FUNCTIONS and their descriptions:
        CHARACTER*2  CRLF
        EXTERNAL     CRLF

C.........  SUBROUTINE ARGUMENTS
        INTEGER     , INTENT (IN) :: NLIST          !  no. of pols or emis types
        CHARACTER(*), INTENT (IN) :: NAMES( NLIST ) !  pollutant names

C...........   Other local variables
        INTEGER       I, L0, L1, L2
        INTEGER       LCNT              ! length count

        CHARACTER*1000 :: MESG           !  message buffer
        CHARACTER*20   :: SPACE = ' '

        CHARACTER*16 :: PROGNAME = 'POLMESG' !  program name

C***********************************************************************
C   begin body of subroutine POLMESG

C.........  Set up initial message. 
        MESG = 'Processing data for:' 

        L0 = LEN_TRIM( MESG )

        L1 = LEN_TRIM( NAMES( 1 ) )
        IF( NAMES( 1 ) .NE. ' ' ) THEN
            MESG = MESG( 1:L0 ) // ' "' // NAMES( 1 )( 1:L1 ) // '"'
        END IF

C.........  Initialize length of initial message
        LCNT = L0 + L1 + 4
        DO I = 2, NLIST

            L1 = LEN_TRIM( MESG )
            IF( NAMES( I ) .EQ. ' ' ) CYCLE
            L2 = LEN_TRIM( NAMES( I ) )

            LCNT = LCNT + L2 + 4

            IF( LCNT .GT. 74 ) THEN
                LCNT = L0 + L2 + 4
                MESG = MESG( 1:L1 ) // ',' // CRLF() // BLANK5 // 
     &                 SPACE // '"' // NAMES( I )( 1:L2 ) // '"'

            ELSE

                MESG = MESG( 1:L1 )// ', "'// NAMES( I )( 1:L2 )// '"'

            END IF

        ENDDO

        CALL M3MSG2( MESG )

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Formatted file I/O formats............ 93xxx

93030   FORMAT( I8.8 )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I8, :, 1X ) )

        END SUBROUTINE POLMESG
