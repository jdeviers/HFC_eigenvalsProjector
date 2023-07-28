MODULE mod_diag
	use mod_prec
	use mod_proc
	implicit none

	contains

	SUBROUTINE DIAG_SYM_NXN(A,W)
		implicit none

		REAL(dp),INTENT(INOUT) :: A(3,3)
		INTEGER,PARAMETER      :: N = 3,LDA = 3,LWMAX = 1000
		INTEGER                :: INFO,LWORK
		REAL(dp)               :: W(3),WORK(LWMAX)

! Find optimal internal settings for DSYEV  
		LWORK = -1
		CALL DSYEV('Vectors','Upper',N,A,LDA,W,WORK,LWORK,INFO)
		LWORK = MIN(LWMAX,INT(WORK(1)))
! Solve eigenproblem
		CALL DSYEV('Vectors','Upper',N,A,LDA,W,WORK,LWORK,INFO)
! Check for convergence.
		IF (INFO.GT.0) THEN
			WRITE(*,*)'The algorithm failed to compute eigenvalues.'
			STOP
		END IF
! ! Eigenvalues print
! 		WRITE(*,'(/,A)') 'Eigenvalues (in ascending order):'
! 		CALL PRINTVEC(W,N)
! !		CALL PRINT_TOFILE_VEC(W,N,'eigenvalues.dat')
! ! Eigenvectors print
! 		WRITE(*,'(/,A)') 'Orthonormal eigenvectors:'
! 		CALL PRINTSQMAT(A,N)
! !		CALL PRINT_TOFILE_SQMAT(A,N,'eigenvectors.dat')

	END SUBROUTINE DIAG_SYM_NXN

END MODULE mod_diag