program example
    implicit none
    
    integer :: i, M(3,4)

    data M /3,-5,7,4, &
            0,6,-2,-3, &
            1,9,-6,0/

    call ConvMAT(M)

    do i = 1, 3
        print*, M(i, :)
    end do
    
    contains

    subroutine ConvMAT(matrix)

        integer, intent(inout) :: matrix(3,4)
        integer :: i,j 

        do i = 1, size(matrix, 1)
            do j = 1, size(matrix, 2)
                if(matrix(i,j) < 0) then
                    matrix(i, j) = -1
                end if
            end do
        end do
    
        
    end subroutine ConvMAT

end program example