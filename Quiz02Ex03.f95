program randomMatrix
    implicit none
    
    integer :: rows, cols, i, j
    real :: rand_num
    real, dimension(:,:), allocatable :: matrix

    print*, "This program calculates the total production of the ABC Co. during your desired startYrs."
    print*, "Please enter the number of rows for the matrix (a positive number less than 10)"
    read*, rows

    print*, "Please enter the number of columns for the matrix (a positive number less than 10)"
    read*, cols

    do while ((rows<0 .or. rows>10) .or. (cols<0 .or. cols>10))
        print*, "The number of rows or columns should be an integer value between one and 10"
        print*, "Please enter the number of rows for the matrix (a positive number less than 10)"
        read*, rows

        print*, "Please enter the number of columns for the matrix (a positive number less than 10)"
        read*, cols
    end do

    allocate(matrix(rows,cols))

    do i=1, rows
        do j=1, cols
            call random_number(rand_num)
            rand_num = 200.0 * rand_num - 100.0

            matrix(i,j) = rand_num
        end do
    end do
    

    open(1, file='output.txt')

    do i = 1, rows
        write(1,*) matrix(i,:)
    end do

end program randomMatrix
