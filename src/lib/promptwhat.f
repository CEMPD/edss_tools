
        SUBROUTINE PROMPTWHAT( PROMPT, FMODE, RDONLY, FMTTED, DEFAULT, 
     &                         CALLER, FNAME, FDEV )

C***********************************************************************
C  subroutine body starts at line 
C
C       If environment variable PROMPTFLAG is 'Y', returns DEFAULT.
C
C       Prompts user for logical file name, then determines the
C       type of file (ASCII or I/O API) and opens it, using the 
C       indicated file mode (FSREAD3, FSRDWR3, FSNEW3, FSUNKN3)
C
C       Provided that '"NONE"'occurs within the prompt, if name entered 
C       is 'NONE', does not attempt to open the file (but still returns 
C       'NONE' as the subroutine argument for logical name).
C
C  RETURNS:
C       logical name or unit number of file opened
C
C  PRECONDITIONS REQUIRED:
C       "setenv <lname> <pathname>" for the file before program launch
C       file description set in FDESC3.EXT structures if appropriate
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETYN, LEN_TRIM, OPENSET
C
C  REVISION  HISTORY:
C       prototype 6/95 by CJC
C	Revised  10/95 by CJC:  more robust treatment of 'NONE'
C       Modified  8/96 by CJC:  ! is a comment-designator for input
C       Modified  8/97 by MH:   environment variable PROMPTFLAG
C***********************************************************************

        IMPLICIT NONE

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'IODECL3.EXT'
        INCLUDE 'SETDECL.EXT'   !  FileSetAPI variables and functions

C...........   ARGUMENTS and their descriptions:
        
        CHARACTER*(*), INTENT (IN) :: PROMPT   !  prompt for user
        INTEGER      , INTENT (IN) :: FMODE    !  file opening-mode
        LOGICAL      , INTENT (IN) :: RDONLY   !  TRUE iff ASCII file is input-only
        LOGICAL      , INTENT (IN) :: FMTTED   !  TRUE iff ASCII file should be formatted
        CHARACTER*(*), INTENT (IN) :: DEFAULT  !  default logical file name
        CHARACTER*(*), INTENT (IN) :: CALLER   !  caller-name for logging messages
        CHARACTER*(*), INTENT(OUT) :: FNAME    !  Logical file name
        INTEGER      , INTENT(OUT) :: FDEV     !  ASCII unit number (if ASCII)

C...........   PARAMETER

        CHARACTER*16, PARAMETER ::  BLANK16 = ' '
        CHARACTER*16, PARAMETER ::  NONE16 = 'NONE'

C...........   EXTERNAL FUNCTIONS and their descriptions:
        LOGICAL         ENVYN
        INTEGER         GETEFILE
        LOGICAL         GETYN

        EXTERNAL        ENVYN, GETEFILE, GETYN

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         DLEN, PLEN   !  string lengths
        INTEGER         I            !  position at which "!" found
        INTEGER         IDEV         !  tmp ASCII unit number
        INTEGER         IOS          !  I/O status
        INTEGER         LUNIT        !  log-file unit number
 
        LOGICAL, SAVE  :: FIRSTIME  = .TRUE. !  true if first call this execution
        LOGICAL, SAVE  :: PROMPTON     !  Actually prompt or open default
        LOGICAL           NFLAG        !  "NONE" is in the prompt

        CHARACTER*16    LNAME        !  logical file name
        CHARACTER*32    BUF          !  string buffer
        CHARACTER*256   MESG         !  messages
        CHARACTER*512   BUFFER       !  prompt/environment buffer

C***********************************************************************
C   begin body of subroutine  PROMPTWHAT

        IF( FIRSTIME ) THEN
 
            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                      .TRUE., IOS )
            FIRSTIME = .FALSE.
 
        ENDIF

C.......   Initialize
        IDEV = 0
        FDEV = 0

C.......   Decide whether 'NONE' is a valid response

        NFLAG = ( INDEX( PROMPT, '"NONE"' ) .GT. 0 )

        PLEN  = LEN_TRIM( PROMPT  )
        DLEN  = LEN_TRIM( DEFAULT )

        IF ( DLEN .GT. 16 ) THEN
            WRITE( MESG, '( A, A, A, I6, 2X, A )' )
     &      'Length of DEFAULT "',  DEFAULT( 1:DLEN ) , 
     &      '" exceeds 16; truncating'
            BUF = TRIM( CALLER ) // '/PROMPTWHAT'
            CALL M3WARN( BUF, 0, 0, MESG )
            DLEN = 16
        END IF

        IF( PROMPTON ) THEN

C.......   Construct actual prompt; Loop:  get file name until file opens
        
            IF ( DLEN + PLEN .GT. 250 ) THEN
                WRITE( MESG, '( A, A, A, I6, 2X, A )' )
     &          'Prompt too long; truncating'
                BUF = TRIM( CALLER ) // '/PROMPTWHAT'
                CALL M3WARN( BUF, 0, 0, MESG )
                PLEN = 250 - DLEN
            END IF

            WRITE( BUFFER,94000 ) 
     &              PROMPT ( 1: LEN_TRIM( PROMPT  ) ) , ' [',
     &              DEFAULT( 1: LEN_TRIM( DEFAULT ) ) , '] >> '

11          CONTINUE
        
                LNAME = ' '
                WRITE( *,95000 ) BUFFER( 1:LEN_TRIM( BUFFER) + 1 )
                READ ( *,93010, IOSTAT=IOS ) LNAME
            
                IF ( IOS .GT. 0 ) THEN

                    MESG = 'Could not read your response'
                    CALL M3MSG2( MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Could not read logical name for file'
                        CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
                    END IF

                ELSE IF ( IOS .LT. 0 ) THEN

                    MESG = 'Ending program ' // TRIM( CALLER ) // '".'
                    CALL M3EXIT( CALLER, 0, 0, MESG, 2 )

                END IF      !  if could not read response

                I = INDEX( LNAME, '!' )
                IF ( I .GT. 0 ) LNAME( I : LEN( LNAME ) ) = ' '

                IF ( LNAME .EQ. BLANK16 ) THEN
                    LNAME = DEFAULT
                END IF

                IF ( NFLAG .AND. ( LNAME .EQ. NONE16 ) ) THEN
                    FDEV = -2
                    FNAME = NONE16
                    RETURN
                END IF

                LUNIT = INIT3()

C...............  Try to open as I/O API file
                IF ( .NOT. OPENSET( LNAME, FMODE, CALLER ) ) THEN !  failure to open

C...................  Try to open as ASCII file
                    IDEV = GETEFILE( LNAME, RDONLY, FMTTED, CALLER )
                    IF ( IDEV .LT. 0 ) THEN     !  failure to open

                        MESG = 'Could not open file "'// TRIM( LNAME )
     &                         // '" as I/O API nor ASCII.'
                        CALL M3MSG2( MESG )
                        IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                            GO TO  11
                        ELSE
                            MESG = 'Ending program "' //
     &                              TRIM( CALLER ) // '".'
                            CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
                        END IF

                    END IF  !  if ASCII open failed

                END IF      !  if openset() failed

        ELSE  ! Do not prompt for output

            LNAME = DEFAULT

            IF ( NFLAG )  THEN
 
                IF( LNAME .EQ. NONE16      ) THEN
                    FDEV = -2
                    FNAME = NONE16
                    RETURN
                END IF

C           ..  Check if logical name is set in order to permit
C           ..  Study Planner to skip file without having to input "NONE"
 
                CALL ENVSTR( LNAME, 'Input file name', ' ',
     &                       BUFFER, IOS )
 
                IF( IOS .LT. 0 ) THEN   ! either not set (-2) or empty (-1)
                    FDEV = -2
                    FNAME = NONE16
                    RETURN
                END IF

            END IF              !  if nflag (checking for "none"

            LUNIT = INIT3()
 
C............  Try to open as I/O API file
            IF ( .NOT. OPENSET( LNAME, FMODE, CALLER ) ) THEN !  failure to open
 
                MESG = 'Could not open file "' // TRIM( LNAME ) // '".'
                CALL M3MSG2( MESG )

C...............  Try to open as ASCII file
                IDEV = GETEFILE( LNAME, RDONLY, FMTTED, CALLER )
                IF ( IDEV .LT. 0 ) THEN     !  failure to open

                    MESG = 'Could not open file "' // TRIM( LNAME ) //
     &                     '" as I/O API nor ASCII.'
                    CALL M3MSG2( MESG )
                    CALL M3EXIT( CALLER, 0, 0, MESG, 2 )

                END IF  !  if ASCII open failed
            END IF      !  if openset() failed

        END IF          !  if prompting or not

        FNAME = LNAME
        FDEV  = IDEV 

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx
 
92000   FORMAT( /5X, A )


C...........   Formatted file I/O formats............ 93xxx
 
93010   FORMAT( A16 )


C...........   Internal buffering formats ............94xxx
 
94000   FORMAT( 12( A, : ) )
 

C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


        END

