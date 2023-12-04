program matrix_multiplcation
    implicit none
    
    real(kind=8), dimension(:,:), allocatable :: m1
    real(kind=8), dimension(:,:), allocatable :: m2

    integer :: m1_row, m1_col, m2_row, m2_col

    print*, "Please enter integers for the rows and columns of the first matrix"
    read*, m1_row, m1_col

    print*, "Please enter integers for the rows and columns of the second matrix"
    read*, m2_row, m2_col

    do while(m1_col == m2_row)
        print*, "The number of columns of the 1st matrix must equal the number of rows of the second matrix"
        print*, "Please enter integers for the rows and columns of the first matrix"
        read*, m1_row, m1_col

        print*, "Please enter integers for the rows and columns of the second matrix"
        read*, m2_row, m2_col
    end do

    allocate(m1(m1_row,m1_col))
    allocate(m2(m2_row,m2_col))


    contains

    subroutine fill_matrix(matrix1, matrix2, row1, row2, col1, col2)

        real(kind=8), dimension(:,:), intent(inout) :: matrix1, matrix2
        integer, intent(in) :: row1, row2, col1, col2
        integer :: i, j

        do i = 0, row1

            do j = 0, col1
                
            end do
            
        end do

    end subroutine fill_matrix

    subroutine mult_matrix(matrix1, matrix2)

    end subroutine mult_matrix

end program matrix_multiplcation