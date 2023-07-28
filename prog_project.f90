PROGRAM prog_project
	USE mod_prec
	USE mod_proc
	USE mod_diag
	implicit none

!	.. Local tensors ..
	REAL(dp),ALLOCATABLE :: full_tens(:,:),eigen(:,:),projected(:,:)
	REAL(dp)             :: final_eigenvec(3,3) = 0.d0, &
                            ref_eigenvec(3,3)
!
!	.. Local scalars ..
	INTEGER              :: i
	INTEGER              :: cursor,nb_rows
!
!	.. Local char ..
	CHARACTER(LEN=50)    :: infile,outfile_vec,outfile_proj


! -- Read infile name
	CALL GETARG(1,infile)

! -- Read concatenated tensors into one
	CALL COUNT_ROWS(infile,nb_rows)
	ALLOCATE(full_tens(nb_rows,3),eigen(nb_rows,4))
	CALL READ_NXN_MAT(infile,nb_rows,full_tens)
	eigen(:,1:3) = full_tens(:,:)

! -- Iterate through the concatenated tensor
	DO i=1,(nb_rows/3)

		cursor = 3*(i-1) + 1
!		WRITE(*,'(/,A)') 'CURRENT SLICE:'
!		CALL PRINTMAT(eigen(cursor:cursor+2,1:3))

! -- Symmetrise tensor in-place (by averaging ij and ji elements), 
! -- otherwise imaginary eigenvectors would complicate everything.
! -- Note: a tensor slice loses its ALLOCATABLE nature.
		CALL SYMMETRISE(eigen(cursor:cursor+2,1:3))
!		WRITE(*,'(/,A)') 'SYMMETRISED SLICE:'
!		CALL PRINTMAT(eigen(cursor:cursor+2,1:3))

! -- Diag, save eigenvectors into eigen (mod in-place)
! -- And save eigenvalues in the 4th column of eigen
		CALL DIAG_SYM_NXN(eigen(cursor:cursor+2,1:3),eigen(cursor:cursor+2,4))
!		WRITE(*,'(/,A)') 'EIGENVECTORS:'
!		CALL PRINTMAT(eigen(cursor:cursor+2,1:3))	

!		WRITE(*,'(/,A)') 'EIGENVALUES:'
!		CALL PRINTVEC(eigen(cursor:cursor+2,4),3)

! -- Flip negative eigenvectors to harmonise them, 
! -- (1: FLIP_IN_FINAL): sum and store in final_eigenvec, negate the eigenvalues of flipped eigenvec in eigen.; OR
! -- (2: FLIP_IN_PLACE): sum and store in final_eigenvec AND eigen, negate the eigenvalues of flipped eigenvec in eigen.
! -- -> Use EITHER one or the other.
! -- (1) plots eigenvecs before regrouping, (2) plots them after.
		IF (i .EQ. 1) ref_eigenvec = eigen(1:3,1:3)
		CALL FLIP_IN_FINAL(ref_eigenvec,eigen(cursor:cursor+2,:),final_eigenvec)
!		CALL FLIP_IN_PLACE(ref_eigenvec,eigen(cursor:cursor+2,:),final_eigenvec)

	END DO

! -- Make scatter plot
	CALL EXPORT_EIGEN(eigen,nb_rows)
	CALL PLOT_EIGEN

! -- Renormalise eigenvectors.
	final_eigenvec = final_eigenvec / (nb_rows/3)

!	WRITE(*,'(/,A)') 'FINAL EIGENVECTORS:'
!	CALL PRINTMAT(final_eigenvec(:,1:3))

! -- Project eigenval from eigen onto averaged eigenvec
	ALLOCATE(projected((nb_rows/3),3))
	DO i=1,(nb_rows/3)
		cursor = 3*(i-1) + 1
		CALL PROJECTION(final_eigenvec,eigen(cursor:cursor+2,4),projected(i,:))

!		CALL PRINTVEC(projected(i,:),3)
	END DO

!	WRITE(*,'(/,A)') 'PROJECTIONS OF EIGENVALUES ONTO AVERAGE EIGENVECS:'
!	WRITE(*,'(A8,A8,A8)') 'XXX','YYY','ZZZ'
!	WRITE(*,'(A)') '  ------------------------'
!	CALL PRINTMAT(projected)

	CALL MODIFY_STRING(infile,'.csv',outfile_vec,'_eigenvec.dat')
	CALL PRINT_TOFILE_MAT(final_eigenvec,outfile_vec)

	CALL MODIFY_STRING(infile,'.csv',outfile_proj,'_projected.dat')
	CALL PRINT_TOFILE_MAT(projected,outfile_proj)

	DEALLOCATE(full_tens,eigen,projected)
END PROGRAM prog_project
