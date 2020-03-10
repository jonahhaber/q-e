!
! Copyright (C) 2001-2008 PWSCF group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!
!----------------------------------------------------------------------------
SUBROUTINE allocate_wfc()
  !----------------------------------------------------------------------------
  !
  ! ... dynamical allocation of arrays: wavefunctions
  ! ... must be called after allocate_nlpot 
  !
  USE io_global, ONLY : stdout
  USE wvfct,     ONLY : npwx, nbnd
  USE basis,     ONLY : natomwfc, swfcatom
  USE fixed_occ, ONLY : one_atom_occupations
  ! debug Dahvyd Wing 3/9/2020
  !  USE ldaU,      ONLY : wfcU, nwfcU, lda_plus_u, U_projection
  USE ldaU,      ONLY : wfcU, nwfcU, lda_plus_u, U_projection, wannier_constraint
  ! end debug
  USE noncollin_module,     ONLY : noncolin, npol
  USE wavefunctions_module, ONLY : evc
  USE wannier_new, ONLY : use_wannier
  !
  IMPLICIT NONE
  !
  !
  ALLOCATE( evc( npwx*npol, nbnd ) )    
  IF ( one_atom_occupations .OR. use_wannier ) &
     ALLOCATE( swfcatom( npwx*npol, natomwfc) )
  IF ( lda_plus_u .AND. (U_projection.NE.'pseudo') ) &
       ALLOCATE( wfcU(npwx*npol, nwfcU) )
 
  !debug Dahvyd Wing 3/9/2020 trying to find why wfcU has the wrong dimensions
  IF (wannier_constraint) ALLOCATE(wfcU(npwx,1))
  WRITE( stdout, '(5X,"allocated wfcU ")' )
  WRITE( stdout, '(5X,"wfcU dim 1 ", I4)' ) SIZE(wfcU,1)
  WRITE( stdout, '(5X,"wfcU dim 2 ", I4)' ) SIZE(wfcU,2)
  !end debug

  !
  RETURN
  !
END SUBROUTINE allocate_wfc
