
        SUBROUTINE OPENPHYS( CALLER, FNAME, STATUS, PHYSNAME, EFLAG )

C***********************************************************************
C  subroutine body starts at line
C
C  DESCRIPTION:
C      This subroutine sets the physical file name to a logical file
C      name and opens the i/o api FILESET file.
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C
C****************************************************************************/
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id$
C
C COPYRIGHT (C) 2003, UNC-CH Carolina Environemental Program
C All Rights Reserved
C
C See file COPYRIGHT for conditions of use.
C
C Environmental Modeling Center
C UNC-CH Carolina Environmental Program
C
C Pathname: $Source$
C Last updated: $Date$
C
C***************************************************************************

        IMPLICIT NONE

C...........   INCLUDES
        INCLUDE 'IOCNST3.EXT'   !  emissions constant parameters
        INCLUDE 'IODECL3.EXT'   !  I/O API function declarations
        INCLUDE 'SETDECL.EXT'   !  FileSetAPI variables and functions

C...........   EXTERNAL FUNCTIONS and their descriptions:
        CHARACTER*2     CRLF
        LOGICAL         SETENVVAR

        EXTERNAL        CRLF, SETENVVAR

C.........  SUBROUTINE ARGUMENTS
        CHARACTER(*), INTENT (IN) :: CALLER    ! calling routine
        CHARACTER(*), INTENT (IN) :: FNAME     ! logical file name
        LOGICAL     , INTENT (IN) :: STATUS    ! OPENSET status to use
        CHARACTER(*), INTENT (IN) :: PHYSNAME  ! physical file name
        LOGICAL     , INTENT(OUT) :: EFLAG     ! true: error occurred

C.........  Other local variables
        CHARACTER*256           MESG    !  message buffer

        CHARACTER*16 :: PROGNAME = 'OPENPHYS' ! program name

C***********************************************************************
C   begin body of subroutine OPENPHYS

C..........  Set environment variable
        IF( .NOT. SETENVVAR( FNAME, PHYSNAME ) ) THEN
            MESG = 'ERROR: Could not set logical file name '
     &             // CRLF() // BLANK10 // 'for file ' //
     &             TRIM( PHYSNAME )
            CALL M3MSG2( MESG )
            CALL M3EXIT( CALLER, 0, 0, ' ', 2 )

C..........  Open file for current pollutant
        ELSE IF( .NOT. OPENSET( FNAME, STATUS, CALLER ) ) THEN
            EFLAG = .TRUE.
            MESG = 'ERROR: Could not open file: ' //
     &                 CRLF() // BLANK10 // TRIM( PHYSNAME )
            CALL M3MSG2( MESG )

C.........  Get description for file
        ELSE IF( .NOT. DESCSET( FNAME, ALLFILES ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Could not get description of file: ' // CRLF()//
     &             BLANK10// TRIM( PHYSNAME )
            CALL M3MSG2( MESG )

        END IF

        RETURN

        END SUBROUTINE OPENPHYS
