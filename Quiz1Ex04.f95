program summation
    implicit none
    
    real, parameter :: check = 10E+8
    real :: sum
    integer :: m, n

    print*, "Please enter an integer value"
    read*, m

    do n = 1, m
        sum = sum + ((n**2) + (2*n) + 2)
    end do

    if ( sum <= check ) then
        print*, "result is ", sum
    else 
        print*, "Out of Range!"
    end if

end program summation