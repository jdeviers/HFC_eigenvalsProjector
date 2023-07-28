MODULE mod_proc
	USE mod_prec
	implicit  none

	contains
! ----------
	SUBROUTINE COUNT_ROWS(INFILE,LEN)
		implicit none

		INTEGER                      :: io
		INTEGER,         INTENT(OUT) :: LEN
		CHARACTER(LEN=*),INTENT(IN)  :: INFILE

		LEN=-1
		OPEN(10,file=INFILE,status='old',action='read',iostat=io)
		IF (io .NE. 0) ERROR STOP 'Error opening the file'
		DO WHILE (io .EQ. 0)
			READ(10,*,iostat=io)
			LEN=LEN+1
		END DO
		CLOSE(10)

	END SUBROUTINE COUNT_ROWS
! ----------
	SUBROUTINE READ_NxN_MAT(INFILE,LEN,MAT)
		implicit none

		INTEGER                            :: i,io
		INTEGER,             INTENT(IN)    :: LEN
		CHARACTER(LEN=*),    INTENT(IN)    :: INFILE
		REAL(dp),ALLOCATABLE,INTENT(INOUT) :: MAT(:,:)

		OPEN(10,file=INFILE,status='old',action='read',iostat=io)
		IF (io .NE. 0) ERROR STOP 'Error opening the file'
		DO i=1,LEN
			READ(10,*) MAT(i,:)
		END DO
		CLOSE(10)

	END SUBROUTINE READ_NxN_MAT
! ----------
	SUBROUTINE MODIFY_STRING(IN_STRING,OLD_SUBSTR,OUT_STRING,NEW_SUBSTR)
		implicit none

		CHARACTER(LEN=*),INTENT(IN)  :: IN_STRING,OLD_SUBSTR,NEW_SUBSTR
		CHARACTER(LEN=*),INTENT(OUT) :: OUT_STRING

		OUT_STRING = IN_STRING(1:INDEX(IN_STRING,OLD_SUBSTR)-1)//NEW_SUBSTR

	END SUBROUTINE MODIFY_STRING
! ----------
	SUBROUTINE SYMMETRISE(MAT)
		implicit none

		REAL(dp),INTENT(INOUT) :: MAT(3,3)
		REAL(dp)               :: avg
		INTEGER                :: i,j

		DO i=1,UBOUND(MAT,1)
			DO j=i+1,UBOUND(MAT,2)
				avg = (MAT(i,j)+MAT(j,i))/2.
				MAT(i,j)=avg;MAT(j,i)=avg
			END DO
		END DO

	END SUBROUTINE SYMMETRISE
! ----------
	LOGICAL FUNCTION CHECKSYMM(MAT,LEN)
		implicit none

		INTEGER              :: i,j
		INTEGER,INTENT(IN)   :: LEN
		REAL(dp),ALLOCATABLE :: MAT(:,:)

		DO i=1,LEN
			DO j=i+1,LEN

				! -- Equivalent to: IF (MAT(i,j) .NE. MAT(j,i)) THEN
				IF ( ABS(MAT(i,j)-MAT(j,i)) .GT. EPSILON(MAT(i,j)) ) THEN
					CHECKSYMM=.FALSE.
					WRITE(*,'(A)') 'A is NOT SYMMETRIC; using DGEEV.'
					RETURN
				END IF

			END DO
		END DO
		CHECKSYMM=.TRUE.
		WRITE(*,'(A)') 'A is SYMMETRIC; using DSYEV.'
		RETURN

	END FUNCTION CHECKSYMM
! ----------
	SUBROUTINE PRINTVEC(VEC,LEN)
		implicit none

		INTEGER              :: i
		INTEGER, INTENT(IN)  :: LEN
		REAL(dp),INTENT(OUT) :: VEC(3)

		WRITE(*,'(*(F8.2))') (VEC(i), i=1,LEN)

	END SUBROUTINE PRINTVEC
! ----------
	SUBROUTINE PRINTMAT(MAT)
		implicit none

		INTEGER              :: i,j
		REAL(dp),INTENT(OUT) :: MAT(:,:)

		DO i=1,UBOUND(MAT,1)
			WRITE(*,'(*(F8.2))') (MAT(i,j), j=1,UBOUND(MAT,2))
		END DO

	END SUBROUTINE PRINTMAT
! ----------
	SUBROUTINE PRINT_TOFILE_VEC(VEC,LEN,OUTFILE)
		implicit none

		INTEGER              :: i
		INTEGER,INTENT(IN)   :: LEN
		CHARACTER(LEN=*)     :: OUTFILE
		REAL(dp),ALLOCATABLE :: VEC(:)

		OPEN(10,file=OUTFILE,status='unknown',action='write')
		WRITE(10,'(*(F12.6))') (VEC(i), i=1,LEN)
		CLOSE(10)

	END SUBROUTINE PRINT_TOFILE_VEC
! ----------
	SUBROUTINE PRINT_TOFILE_MAT(MAT,OUTFILE)
		implicit none

		INTEGER              :: i,j
		CHARACTER(LEN=*)     :: OUTFILE
		REAL(dp)             :: MAT(:,:)

		OPEN(10,file=OUTFILE,status='unknown',action='write')
		DO i=1,UBOUND(MAT,1)
			WRITE(10,'(*(F12.6))') (MAT(i,j), j=1,UBOUND(MAT,2))
		END DO
		CLOSE(10)

	END SUBROUTINE PRINT_TOFILE_MAT
! ----------
	SUBROUTINE EXPORT_EIGEN(MAT,LEN)
		implicit none

		REAL(dp),ALLOCATABLE,INTENT(INOUT) :: MAT(:,:)
		INTEGER,             INTENT(IN)    :: LEN
		INTEGER                            :: i,cursor

		OPEN(11,file='eigen_1.csv',status='unknown',action='write')
		OPEN(12,file='eigen_2.csv',status='unknown',action='write')
		OPEN(13,file='eigen_3.csv',status='unknown',action='write')

		DO i=1,(LEN/3)
			cursor = 3*(i-1) + 1
			WRITE(11,'(3F8.3)') RESHAPE(MAT(cursor:cursor+2,1),(/1,3/))
			WRITE(12,'(3F8.3)') RESHAPE(MAT(cursor:cursor+2,2),(/1,3/))
			WRITE(13,'(3F8.3)') RESHAPE(MAT(cursor:cursor+2,3),(/1,3/))
		END DO

		CLOSE(11)
		CLOSE(12)
		CLOSE(13)

	END SUBROUTINE EXPORT_EIGEN
! ----------
	SUBROUTINE PLOT_EIGEN
		implicit none

		CHARACTER(LEN=50) :: csv_name,atom_name,command

		CALL GETARG(1,csv_name)
		CALL MODIFY_STRING(csv_name,'.csv',atom_name,'')

		OPEN(14,file='plot_eigen.plt',status='unknown',action='write')
		WRITE(14,'(A)') "set term pdfcairo enhanced dashed size 3.2,3 font 'Courier New,10';"
		WRITE(14,'(A)') "set encoding iso_8859_1"
		WRITE(14,'(A)') ""
		WRITE(14,'(A)') "set loadpath '~/.plotcfg'"
		WRITE(14,'(A)') "load 'greens.pal'"
		WRITE(14,'(A)') ""
		WRITE(14,'(A)') "set tics nomirror"
		WRITE(14,'(A)') "set key box Left top right samplen 2 spacing 1 font 'Courier New,6'"
		WRITE(14,'(A)') ""
		WRITE(14,'(A)') "set output '"//TRIM(atom_name)//".pdf'"
		WRITE(14,'(A)') ""
		WRITE(14,'(A)') "set title 'Eigenvectors of "//TRIM(atom_name)//"'"
		WRITE(14,'(A)') "splot 'eigen_1.csv' u 1:2:3 ls 3 pt 7 ps 0.5 t ' eigen 1', \"
		WRITE(14,'(A)') "      'eigen_2.csv' u 1:2:3 ls 5 pt 7 ps 0.5 t ' eigen 2', \"
		WRITE(14,'(A)') "      'eigen_3.csv' u 1:2:3 ls 7 pt 7 ps 0.5 t ' eigen 3'"
		CLOSE(14)

		CALL SYSTEM('gnuplot plot_eigen.plt')
		command = 'evince '//TRIM(atom_name)//'.pdf &'
!		CALL SYSTEM(command)

	END SUBROUTINE PLOT_EIGEN
! ----------
	SUBROUTINE FLIP_IN_FINAL(REF_MAT,MAT,SUMMED_MAT)
		implicit none

		REAL(dp),INTENT(IN)    :: REF_MAT(:,:)
		REAL(dp),INTENT(INOUT) :: MAT(:,:),SUMMED_MAT(:,:)
		INTEGER                :: i

		DO i=1,3
			IF (DOT_PRODUCT(REF_MAT(:,i),MAT(:,i)) .GE. 0.d0) THEN
				SUMMED_MAT(:,i) = SUMMED_MAT(:,i) + MAT(:,i)
			ELSE
				SUMMED_MAT(:,i) = SUMMED_MAT(:,i) - MAT(:,i)
				MAT(i,4) = - MAT(i,4)
			END IF
		END DO

	END SUBROUTINE FLIP_IN_FINAL
! ----------
	SUBROUTINE FLIP_IN_PLACE(REF_MAT,MAT,SUMMED_MAT)
!
! -- Also flip eigenvec in-place, so the scatter plot shows eigenvecs after regrouping
! -- Use EITHER this one or FLIP_AND_SUM. 
!
		implicit none

		REAL(dp),INTENT(IN)    :: REF_MAT(:,:)
		REAL(dp),INTENT(INOUT) :: MAT(:,:),SUMMED_MAT(:,:)
		INTEGER                :: i

		DO i=1,3
			IF (DOT_PRODUCT(REF_MAT(:,i),MAT(:,i)) .GE. 0.d0) THEN
				SUMMED_MAT(:,i) = SUMMED_MAT(:,i) + MAT(:,i)
			ELSE
				SUMMED_MAT(:,i) = SUMMED_MAT(:,i) - MAT(:,i)
				MAT(:,i)        = - MAT(:,i)
				MAT(i,4)        = - MAT(i,4)
			END IF
		END DO

	END SUBROUTINE FLIP_IN_PLACE
! ----------
	SUBROUTINE PROJECTION(EVECS,EVALS,PROJ)
		implicit none

		REAL(dp),INTENT(IN)    :: EVECS(:,:)
		REAL(dp),INTENT(IN)    :: EVALS(:)
		REAL(dp),INTENT(INOUT) :: PROJ(:)
		REAL(dp)               :: tmp
		INTEGER                :: i,j

		DO i=1,3        ! i runs over columns
			tmp = 0.d0
			DO j=1,3    ! j runs over the rows of column i
				tmp = tmp + ( EVECS(j,i)*EVALS(j) )**2.d0
			END DO
			PROJ(i) = SQRT(tmp)
		END DO

	END SUBROUTINE PROJECTION
! ----------


END MODULE mod_proc
