C***********************************************************************
C  Copied from I/O API v2.2 m3msg2.F
C  I/O API m3mesg.f doesn't only write to log file like it should
C***********************************************************************

C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  M3MESG( MESSAGE )

C***********************************************************************
C  Subroutine body starts at line   79
C
C  FUNCTION:
C       Generate simple (1-line text) messages and prompts for Models-3;
C       wrapper around FLUSH() -- needed for IBM XLF.
C
C  PRECONDITIONS REQUIRED:  
C       message fits on one line
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       INIT3, TRIMLEN
C
C  REVISION  HISTORY:   
C       Adapted   1/1997 by CJC from M3MESG()
C       Modified  5/1998 by CJC for OpenMP thread-safety
C       Modified 10/1999 by CJC for AIX "flush"
C       Modified  5/2003 by CJC for control of optional messages via
C       STATE3 variable STDOUT; changes to critical sections.
C       Modified  6/2003 by CJC to deal with IRIX F90v7.4.x failure to
C       properly ANSWER and flush output on program exit:  new entries
C       M3PROMPT used by the GET*() and PROMPT*() routines, and 
C       M3FLUSH for use by user-code.
C***********************************************************************

      IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

C...........   ARGUMENT and its description:

        CHARACTER(*)   MESSAGE

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INITLOG3 !  get log-device unit number
        INTEGER         TRIMLEN  !  string length, exclusive of trailing blanks
        EXTERNAL        TRIMLEN


C.......   Scratch LOCAL VARIABLES

        INTEGER     L
        INTEGER, SAVE :: LOGDEV = -1


C***********************************************************************
C   begin body of subroutine  M3MESG

        L = TRIMLEN( MESSAGE )

!$OMP CRITICAL( S_LOGDEV )
        IF ( LOGDEV .LT. 0 ) THEN
            LOGDEV = INITLOG3()
        END IF          !!  if logdev < 0 (initialization required)

        WRITE( LOGDEV,'( 5X, A )' ) MESSAGE( 1:L )

        CALL FLUSH( LOGDEV )

!$OMP END CRITICAL( S_LOGDEV )

        RETURN

        END

