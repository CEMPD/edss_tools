
        SUBROUTINE CHKGRID( DATDESC, FTYPE, CHKLEVEL, EFLAG )

C***********************************************************************
C  subroutine body starts at line 90
C
C  DESCRIPTION:
C      This subroutine updates the grid information and compares to 
C      the existing information, if it has been previously set.
C
C  PRECONDITIONS REQUIRED:
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C
C**************************************************************************
C
C Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
C                System
C File: @(#)$Id$
C
C COPYRIGHT (C) 2000, MCNC--North Carolina Supercomputing Center
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

C.........  MODULES for public variables
C.........  This module contains the global variables for the 3-d grid
        USE MODGRID

        IMPLICIT NONE

C.........  INCLUDES:
        INCLUDE 'IOCNST3.EXT'   !  emissions constant parameters
        INCLUDE 'PARMS3.EXT'    !  I/O API parameters
        INCLUDE 'IODECL3.EXT'   !  I/O API function declarations
        INCLUDE 'FDESC3.EXT'    !  I/O API file desc. data structures
        INCLUDE 'FLTERR.EXT'    !  error filter statement function

C.........  EXTERNAL FUNCTIONS and their descriptions:
        CHARACTER*2  CRLF
        INTEGER      GETIFDSC  

        EXTERNAL     CRLF, GETIFDSC

C...........   SUBROUTINE ARGUMENTS
        CHARACTER(*), INTENT(IN) :: DATDESC  ! data descriptions
        CHARACTER(*), INTENT(IN) :: FTYPE    ! GMAT|GRID file type of interest
        INTEGER     , INTENT(IN) :: CHKLEVEL ! strigency of check
        LOGICAL     , INTENT(OUT):: EFLAG    ! true: comparison failed

C...........   Local parameters
        INTEGER, PARAMETER :: CHK_ALL = 0     ! check all of the grid settings
        INTEGER, PARAMETER :: CHK_SUBGRID = 1 ! check all but allow subgrids

C...........   Local variables
        INTEGER       L       ! length of file description
        INTEGER       NC      ! tmp number of columns
        INTEGER       NR      ! tmp number of rows
        INTEGER       XO      ! tmp x-offset  
        INTEGER       YO      ! tmp y-offset  

        REAL          CHK_X   ! tmp val for checking subgrid even with grid
        REAL          CHK_Y   ! tmp val for checking subgrid even with grid

        LOGICAL, SAVE :: GFLAG  = .FALSE. ! true: grid settings have been init

        CHARACTER*25    FILDESC  ! description of input file
        CHARACTER*300   MESG     ! message buffer

        CHARACTER*16 :: PROGNAME = 'CHKGRID' ! program name

C***********************************************************************
C   begin body of function CHKGRID

C.............  Set tmp rows, columns, and total cells depending on file type
        IF( FTYPE .EQ. 'GMAT' ) THEN
            NC = GETIFDSC( FDESC3D, '/NCOLS3D/', .TRUE. )
            NR = GETIFDSC( FDESC3D, '/NROWS3D/', .TRUE. )
            FILDESC = 'gridding matrix'

        ELSEIF( FTYPE .EQ. 'GROUPS' ) THEN
            NC = GETIFDSC( FDESC3D, '/NCOLS3D/', .TRUE. )
            NR = GETIFDSC( FDESC3D, '/NROWS3D/', .TRUE. )
            FILDESC = 'stack groups file'

        ELSEIF( FTYPE .EQ. 'GRID' ) THEN
            NC = NCOLS3D
            NR = NROWS3D
            FILDESC = 'gridded file'

        ELSEIF( FTYPE .EQ. 'GRIDDESC' ) THEN
            NC = NCOLS3D
            NR = NROWS3D
            FILDESC = 'grid description file'

        ELSEIF( FTYPE .EQ. 'SURROGATES' ) THEN
            NC = NCOLS3D
            NR = NROWS3D
            FILDESC = 'surrogates file'

        ELSEIF( FTYPE .EQ. 'LANDUSE' ) THEN
            NC = NCOLS3D
            NR = NROWS3D
            FILDESC = 'landuse file'

        ELSE
            MESG = 'INTERNAL ERROR: File type "' // FTYPE // 
     &              '" not known in call to ' // PROGNAME
            CALL M3MSG2( MESG )
            CALL M3EXIT( PROGNAME, 0, 0, MESG, 2 )

        ENDIF

        L = LEN_TRIM( FILDESC )

C.............  If grid information has already been initialized, then compare
C               existing to this file.
        IF( GFLAG ) THEN

C.............  Check settings that must be consistent for exact grid match
            IF( CHKLEVEL .EQ. CHK_ALL ) THEN

                IF ( NCOLS .NE. NC      .OR.
     &               NROWS .NE. NR      .OR.
     &               FLTERR( XORIG, SNGL( XORIG3D ) ) .OR.
     &               FLTERR( YORIG, SNGL( YORIG3D ) )      ) THEN

                    EFLAG = .TRUE.
                    MESG = 'ERROR: Columns, rows, x-origin, or ' //
     &                     'y-origin for ' // DATDESC // ' in ' //
     &                     CRLF() // BLANK10 // FILDESC( 1:L ) // 
     &                     'are inconsistent with initialized values.'
                    CALL M3MSG2( MESG ) 

                END IF

                XOFF = 0
                YOFF = 0

            END IF

C.............  Check settings that must be consistent for grids and subgrids
            IF( CHKLEVEL .LE. CHK_SUBGRID ) THEN

                IF ( GDTYP .NE. GDTYP3D .OR.
     &               FLTERR( XCELL, SNGL( XCELL3D ) ) .OR.
     &               FLTERR( YCELL, SNGL( YCELL3D ) ) .OR.
     &               FLTERR( XCENT, SNGL( XCENT3D ) ) .OR.
     &               FLTERR( YCENT, SNGL( YCENT3D ) ) .OR.
     &               FLTERR( P_ALP, SNGL( P_ALP3D ) ) .OR.
     &               FLTERR( P_BET, SNGL( P_BET3D ) ) .OR.
     &               FLTERR( P_GAM, SNGL( P_GAM3D ) )      ) THEN

                    EFLAG = .TRUE.
                    MESG = 'ERROR: Grid type, cell sizes, or ' //
     &                     'grid projection for ' // DATDESC // ' in '//
     &                     CRLF() // BLANK10 // FILDESC( 1:L ) // 
     &                     'are inconsistent with initialized values.'
                    CALL M3MSG2( MESG ) 

                END IF

C.................  Ensure that origins are compatible with each other by
C                   making sure they line up based on the cell sizes
                CHK_X  = ( XORIG3D - XORIG ) / XCELL
                CHK_X  = CHK_X - INT( CHK_X )
                CHK_Y  = ( YORIG3D - YORIG ) / YCELL
                CHK_Y  = CHK_Y - INT( CHK_Y )
                IF( FLTERR( CHK_X, 0. ) .OR.
     &              FLTERR( CHK_Y, 0. )      ) THEN

                    EFLAG = .TRUE.
                    MESG = 'ERROR: Grid origins not compatible ' //
     &                     'between ' // DATDESC // ' in ' // 
     &                     CRLF() // BLANK10 // FILDESC( 1:L ) // 
     &                     'and initialized values.'
                    CALL M3MSG2( MESG ) 

                END IF

C.................  If offset has been set, then check to ensure its the same
                IF( OFFLAG ) THEN

                    XO = INT( ( XORIG3D - XORIG ) / XCELL )
                    YO = INT( ( YORIG3D - YORIG ) / YCELL )
                    IF( XOFF .NE. XO .OR.
     &                  YOFF .NE. YO      ) THEN

                        EFLAG = .TRUE.
                        MESG = 'ERROR: Subgrid offset for ' //
     &                          DATDESC // ' in ' // CRLF() // BLANK10// 
     &                          FILDESC( 1:L ) // 'is ' //
     &                         'inconsistent with initialized values.'
                        CALL M3MSG2( MESG ) 


                    END IF

C.....................  Check that current subgrid is the same as the previous
C                       subgrid
                    IF ( NCOLS .NE. NC      .OR.
     &                   NROWS .NE. NR      .OR.
     &                   FLTERR( XORIG, SNGL( XORIG3D ) ) .OR.
     &                   FLTERR( YORIG, SNGL( YORIG3D ) )      ) THEN

                         EFLAG = .TRUE.
                         MESG = 'ERROR: Columns, rows, x-origin, or ' //
     &                     'y-origin for ' // DATDESC // ' in ' //
     &                     CRLF() // BLANK10 // FILDESC( 1:L ) // 
     &                     'are inconsistent with values from ' // GRDNM
                         CALL M3MSG2( MESG ) 

                    END IF

C.................  If no offset yet...
                ELSE

C.....................  Compute possible offset from upper right hand corner,
C                       and if there is one, set flag
                    XOFF = INT( ( XORIG  + NCOLS * XCELL   ) - 
     &                          ( XORIG3D+ NC    * XCELL3D ) ) / XCELL
                    YOFF = INT( ( YORIG  + NROWS * YCELL   ) - 
     &                          ( YORIG3D+ NR    * YCELL3D ) ) / YCELL

                    IF( XOFF .NE. 0 .OR. YOFF .NE. 0 ) OFFLAG = .TRUE.

C.....................  Compute possible offset from origin, and if there is 
C                       one, set flag
                    XOFF = INT( ( XORIG3D - XORIG ) / XCELL )
                    YOFF = INT( ( YORIG3D - YORIG ) / YCELL )
                    
                    IF( XOFF .NE. 0 .OR. YOFF .NE. 0 ) OFFLAG = .TRUE.

C.....................  Reset origin and number of cells to latest grid
                    GRDNM = GDNAM3D
                    XORIG = SNGL( XORIG3D )
                    YORIG = SNGL( YORIG3D )
                    NCOLS = NC
                    NROWS = NR
                    NGRID = NCOLS * NROWS

                END IF

            END IF

C.........  Initialize grid information
        ELSE

            GFLAG = .TRUE.
            GRDNM = GDNAM3D
            GDTYP = GDTYP3D
            P_ALP = SNGL( P_ALP3D )
            P_BET = SNGL( P_BET3D )
            P_GAM = SNGL( P_GAM3D )
            XCENT = SNGL( XCENT3D )
            YCENT = SNGL( YCENT3D )
            XORIG = SNGL( XORIG3D )
            YORIG = SNGL( YORIG3D )
            XCELL = SNGL( XCELL3D )
            YCELL = SNGL( YCELL3D )
            NCOLS = NC
            NROWS = NR
            NGRID = NCOLS * NROWS

            MESG = 'NOTE: Grid settings initialized using ' // 
     &             DATDESC // ' in ' // CRLF()// BLANK10 // 
     &             FILDESC( 1:L ) // '.'

            CALL M3MSG2( MESG )

        ENDIF

        IF( EFLAG ) THEN

            MESG = 'ERROR: Grid parameters for ' // DATDESC // ' in ' //
     &             CRLF() // BLANK10 // FILDESC( 1:L ) //
     &             ' are inconsistent with initialized values.'
            CALL M3MSG2( MESG )

        END IF

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Formatted file I/O formats............ 93xxx

93000   FORMAT( A )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 10( A, :, I8, :, 1X ) )

        END

