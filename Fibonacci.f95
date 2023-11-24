program fibonacci
    implicit none
    
    integer (kind=8) :: i, cnt, fib_num
    integer, dimension(:), allocatable :: sequence

    print*, "Enter a positive integer to get it's fibonacci number"
    read*, cnt

    do while(cnt < 1)
        print*, "The number entered is too small. Please enter a number greater than 0"
        read*, cnt
    end do

    allocate(sequence(cnt))

    fib_num = get_fibonacci(cnt)

    print"(a,i5)", "Your fibonacci number is", fib_num 

    contains

    recursive integer(kind=8) function get_fibonacci(n) result(num)

        integer (kind=8), intent(in) :: n

        if (n <= 2 ) then
            num = 1
        else
            num = get_fibonacci(n-1) + get_fibonacci(n-2)
        end if
        
        end function get_fibonacci

end program fibonacci