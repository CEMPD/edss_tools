
        LOGICAL FUNCTION  ISOPEN ( FNAME )

C***********************************************************************
C  function body starts at line  50
C
C  FUNCTION:  Checks if file with logical name FNAME is open
C
C  RETURN VALUE:  TRUE if open, FALSE otherwise
C
C  PRECONDITIONS REQUIRED:  FNAME exists
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:  
C       new 11/7 by cas
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   FNAME   !  logical name of file to be opened


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INDEX1  !  look up names in tables
        EXTERNAL        INDEX1


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FILE            !  file index
        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*80    MSGBUF


C***********************************************************************
C   begin body of function  CLOSE3

        IF ( .NOT. FINIT3 ) THEN
            ISOPEN = .FALSE.
            RETURN
        END IF

        IF ( LEN( FNAME ) .GT. 16 ) THEN
            MSGBUF = 'File name length bad for "' // FNAME // '"'
            WRITE( LOGDEV,91010 )
     &          'Max length 16; actual:', LEN( FNAME ),
     &          MSGBUF
            ISOPEN = .FALSE.
            RETURN
        END IF
        

C.......   Find netCDF index for the file, and check time step availability:

        FIL16 = FNAME   !  fixed-length-16 scratch copy of name
        FILE  = INDEX1( FIL16, COUNT3, FLIST3 )

        IF ( FILE /= 0 ) THEN !  file open
            ISOPEN = .TRUE.
        ELSE
            ISOPEN = .FALSE.
        END IF

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine ISOPEN <<<',
     &            3 ( /5X , A , : ) , I5, // )

C...........   Log message formats................... 92xxx

92010   FORMAT ( /5X , A, / )

        END

