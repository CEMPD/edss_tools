
C.........................................................................
C @(#)$Header$ 
C EDSS/Models-3 I/O API.  Portions copyright (C) 1992-1997 MCNC
C See file "COPYRIGHT.txt" for conditions of use.
C.........................................................................

        LOGICAL   FUNCTION  CHECK3 ( FNAME, VNAME, JDATE, JTIME )

C***********************************************************************
C  function body starts at line  74
C
C  FUNCTION:  returns TRUE iff time step for (JDATE,JTIME) and variable
C       VNAME is available in file with logical name FNAME.  If FNAME 
C       is time idndependent or is a dictionary file, only VNAME is 
C       significant and the routine returns the availability of the
C       variable or file description for VNAME.
C
C  PRECONDITIONS REQUIRED:  
C       FNAME already opened by OPEN3().
C       VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:  
C       prototype 3/92 by CJC
C
C       Modified  7/94 by CJC to handle restart (circular-buffer) files
C
C       modified 10/94 by CJC to work with variable-level-granularity
C       WRITE3() 
C
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   FNAME   !  file name for query
        CHARACTER*(*)   VNAME   !  vble name for query
        INTEGER         JDATE   !  date (YYYYDDD) for query
        INTEGER         JTIME   !  time (HHMMSS) for query


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INDEX1     !  look up names in name tables
        INTEGER         INIT3      !  initialize I/O system files.
        INTEGER         JSTEP3     !  compute time step record numbers

        EXTERNAL        INDEX1, INIT3, JSTEP3
        EXTERNAL        INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         IDUM            !  holds return value for INIT3()
        INTEGER         FID             !  subscript for STATE3 arrays
        INTEGER         STEP            !  time step record number
        INTEGER         STEP2           !  1 or 2, according to step mod 2
        INTEGER         IERR            !  netCDF error status return
        INTEGER         DIMT( 5 )       !  corner   for NCVGT()
        INTEGER         DELT( 5 )       !  diagonal for NCVGT()
        INTEGER         VID             !  subscript for vble in state arrays
        INTEGER         V               !  "; loop counter over variables
        INTEGER         F1, F2          !  test-values for FLAGS
        INTEGER         FLAGS( 2,MXVARS3 )!  flags from NCVGT()            
        CHARACTER*16    FIL16           !  scratch file-name-buffer
        CHARACTER*16    VAR16           !  scratch vble-name-buffer


C***********************************************************************
C   begin body of function  CHECK3

        IF ( .NOT. FINIT3 ) THEN
            IDUM = INIT3()
            WRITE( LOGDEV,91010 )
     &      'CHECK3():  I/O API not yet initialized.'
            CHECK3 = .FALSE.
            RETURN
        END IF

        IF ( LEN( FNAME ) .GT. 16 ) THEN
            WRITE( LOGDEV,91001 )
     &          'File name length bad for "', FNAME, '"',
     &          'Max file name length 16; actual:', LEN( FNAME )
            CHECK3 = .FALSE.
            RETURN
        END IF
        
        IF ( LEN( VNAME ) .GT. 16 ) THEN
            WRITE( LOGDEV,91001 )
     &          'File name length bad for "', VNAME, '"',
     &          'Max file name length 16; actual:', LEN( VNAME )
            CHECK3 = .FALSE.
            RETURN
        END IF
        

C.......   Find netCDF index for the file, and check time step availability:

        FIL16 = FNAME   !  fixed-length-16 scratch copy of name
        VAR16 = VNAME   !  "
        FID   = INDEX1( FIL16, COUNT3, FLIST3 )

        IF ( FID .EQ. 0 ) THEN
            
            CHECK3 = .FALSE.
            RETURN
            
        ELSE IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN
            
            STEP  = INDEX1( VAR16, NVARS3( FID ), VLIST3( 1,FID ) )
            STEP2 = STEP
            VID   = STEP
            
        ELSE
            
            STEP = JSTEP3( JDATE, JTIME,
     &                     SDATE3( FID ),
     &                     STIME3( FID ),
     &                     TSTEP3( FID ) )
            
            IF ( VAR16 .NE. ALLVAR3 ) THEN
                VID = INDEX1( VAR16, NVARS3( FID ), VLIST3( 1,FID ) )
                IF ( VID .EQ. 0 ) THEN
                    CHECK3 = .FALSE.
                    RETURN
                END IF
            ELSE
                VID = ALLAYS3
            END IF              !  if "all variables", or not
         
            IF ( STEP .LT. 0 ) THEN
                   
                CHECK3 = .FALSE.
                RETURN
                
            END IF
                   
            IF ( TSTEP3( FID ) .LT. 0 ) THEN
                STEP2 = 1 + MOD( STEP-1, 2 )
                F1    = JDATE
                F2    = JTIME
            ELSE IF ( TSTEP3( FID ) .GT. 0 ) THEN
                F1    = JDATE
                F2    = JTIME
                STEP2 = STEP
            ELSE
                F1    = 0
                F2    = 0
                STEP2 = STEP
            END IF
             
        END IF          !  if file not open, or if dictionary, or ...
        
        IF ( VOLAT3( FID ) ) THEN      !  volatile file:  synch with disk
           
            CALL NCSNC( CDFID3( FID ), IERR )
            IF ( IERR .NE. 0 ) THEN
           
                WRITE( LOGDEV,91010 )
     &              'Error with disk synchronization for file:  '
     &              // FIL16 ,
     &              'netCDF error number', IERR
           
                CHECK3 = .FALSE.
                RETURN
           
            END IF              !  if synch failed
           
        END IF                  !  if file is volatile
        
        DIMT( 1 ) = 1           !  field:  date and time
        DELT( 1 ) = 2           !  extent:  both date and time
        DIMT( 3 ) = STEP2       !  timestep record number
        DELT( 3 ) = 1           !  extent:  1 time step
        
        IF ( VID .GT. 0 ) THEN      !  checking just one variable
            
            DIMT( 2 ) = VID     !  initial variable number
            DELT( 2 ) = 1       !  extent:  one variable
            
            CALL NCVGT( CDFID3( FID ), TINDX3( FID ), 
     &                  DIMT, DELT, FLAGS( 1,VID ), IERR )
            
            IF ( IERR .EQ. 0 ) THEN
                CHECK3 = ( FLAGS( 1,VID ) .EQ. F1  .AND.
     &                     FLAGS( 2,VID ) .EQ. F2  )
            ELSE IF ( IERR .EQ. -57 ) THEN
                CHECK3 = .FALSE.
            ELSE                !  ierr nonzero:
                WRITE( LOGDEV,91010 )
     &              'Error reading time step flag for file:  '
     &              // FIL16 // ' variable ' // VAR16,
     &              'netCDF error number', IERR
                CHECK3 = .FALSE.
            END IF              !  if ierr = 0  or  not.
        
        ELSE                    !  checking all variables
                
            DIMT( 2 ) = 1               !  initial variable number
            DELT( 2 ) = NVARS3( FID )   !  extent:  all variables
            
            CALL NCVGT( CDFID3( FID ), TINDX3( FID ), 
     &                  DIMT, DELT, FLAGS, IERR )
            
            IF ( IERR .EQ. -57 ) THEN
                CHECK3 = .FALSE.
                RETURN
            ELSE IF ( IERR .NE. 0 ) THEN
                WRITE( LOGDEV,91010 )
     &              'Error reading time step flag for file:  '
     &              // FIL16 ,
     &              'netCDF error number', IERR
                CHECK3 = .FALSE.
                RETURN
            END IF              !  if ncvgt() failed
        
            DO  11  V = 1, NVARS3( FID )
            
                IF ( FLAGS( 1,V ) .NE. F1  .OR. 
     &               FLAGS( 2,V ) .NE. F2       ) THEN
                    CHECK3 = .FALSE.
                    RETURN
                END IF  !  if ierr = 0  or  not.
            
11          CONTINUE
                
            CHECK3 = .TRUE.             ! (if you get to here)
        
        END IF      !  if checking just one vble, or if checking all vbles

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91001   FORMAT ( //5X , '>>> WARNING in subroutine CHECK3 <<<',
     &            /5X , 3A,
     &            /5X , A , I5, // )

91010   FORMAT ( //5X , '>>> WARNING in subroutine CHECK3 <<<',
     &            2 ( /5X , A , : ) , I5, // )

        END

