
        MODULE MODGRID

!***********************************************************************
!  Module body starts at line
!
!  DESCRIPTION:
!     This module contains the public variables and allocatable arrays 
!     used for gridding.
!
!  PRECONDITIONS REQUIRED:
!
!  SUBROUTINES AND FUNCTIONS CALLED:
!
!  REVISION HISTORY:
!     Created 10/2000 by M. Houyoux
!
!***************************************************************************
!
! Project Title: Sparse Matrix Operator Kernel Emissions (SMOKE) Modeling
!                System
! File: @(#)$Id$ 
!
! COPYRIGHT (C) 2000, MCNC--North Carolina Supercomputing Center
! All Rights Reserved
!
! See file COPYRIGHT for conditions of use.
!
! Environmental Programs Group
! MCNC--North Carolina Supercomputing Center
! P.O. Box 12889
! Research Triangle Park, NC  27709-2889
!
! env_progs@mcnc.org
!
! Pathname: $Source$ 
! Last updated: $Date$ 
!
!****************************************************************************

C...........   INCLUDES:
        INCLUDE 'IOPRVT3.EXT'   !  I/O API tools private string lengths

!.........  Horiztonal grid information
        CHARACTER(LEN=IOVLEN3), PUBLIC :: GRDNM = ' '  ! grid name
        CHARACTER(LEN=IOVLEN3), PUBLIC :: COORD = ' '  ! coord system name
        REAL   , PUBLIC :: GDTYP = -1     ! i/o api grid type code
        REAL   , PUBLIC :: P_ALP = 0.D0   ! projection alpha
        REAL   , PUBLIC :: P_BET = 0.D0   ! projection beta
        REAL   , PUBLIC :: P_GAM = 0.D0   ! projection gamma
        REAL   , PUBLIC :: XCENT = 0.D0   ! x-center of projection
        REAL   , PUBLIC :: YCENT = 0.D0   ! y-center of projection
        REAL   , PUBLIC :: XORIG = 0.D0   ! x-origin of grid
        REAL   , PUBLIC :: YORIG = 0.D0   ! y-origin of grid
        REAL   , PUBLIC :: XCELL = 0.D0   ! x-dim of cells
        REAL   , PUBLIC :: YCELL = 0.D0   ! y-dim of cells
        INTEGER, PUBLIC :: NCOLS = 0      ! number of columns in grid
        INTEGER, PUBLIC :: NROWS = 0      ! number of rows in grid
        INTEGER, PUBLIC :: NGRID = 0      ! number of cells in grid
        INTEGER, PUBLIC :: XOFF  = 0      ! subgrid offset
        INTEGER, PUBLIC :: YOFF  = 0      ! subgrid offset
        LOGICAL, PUBLIC :: OFFLAG = .FALSE. ! true: subgrid offset has been set

!.........  Vertical structure information
        INTEGER, PUBLIC :: NLAYS = 1       ! number of layers
        INTEGER, PUBLIC :: VGTYP  = -1     ! type of vertical coordinates
        REAL   , PUBLIC :: VGTOP  = 0.0    ! model-top, for sigma coord types
        REAL   , ALLOCATABLE, PUBLIC :: VGLVS( : ) ! vertical coordinate values

        END MODULE MODGRID
