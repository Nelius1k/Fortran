program myprog
    implicit none

    integer :: i
    real :: n(42)
    n(1) = 1.0

    do i=2, 42
        n(i) = 4 - n(i-1)
        print*, n(i)
    end do
end program myprog