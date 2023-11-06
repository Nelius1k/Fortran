program example2
    implicit none

    real :: a(2), avg
    real, dimension(10,3) :: table
    real, dimension(:,:), allocatable :: table2
    integer :: i

    open(1, file='input2.txt', action='read', status='old', ERR=10)
    a = shape(table)
    do i=1, 10
        read(1, *) table(i, 1:3)
    end do

    do i=1, 10
        print "(3e10.3)", table(i,1:)
    end do

    allocate(table2(a(2),a(1)))

    table2 = transpose(table)

    open(2, file='output.dat', action='write', ERR=10)
    do i = 1, int(a(2))
        write(2,"(10es10.3)") table2(i, 1:a(1))
    end do

    write(2,*) "The average of the above matrix is"
    avg = real(sum(table2) / size(table2))

    write(2, "(f5.2)") avg

    deallocate(table2)
    close(1)
    10 stop

end program example2