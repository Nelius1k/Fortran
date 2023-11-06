program average
    implicit none

    !character(len=3) :: num
    real :: num
    integer :: counter, io
    real :: avg, sum

    counter = 0
    avg = 0

    open(1, file='input.txt', status='old', action='read', iostat=io, ERR=10)
    do
        read(1, *, iostat=io) num
        if (io /= 0) exit
        counter = counter + 1
        sum = sum + int(num)
        
    end do

    avg = sum / counter

    print "(f5.3)", avg

    10 print*, "problem reading the file"
end program average