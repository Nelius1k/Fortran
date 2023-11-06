program density
    implicit none

    integer :: i, counter, io
    character(len=100) :: line
    character, allocatable :: str(:,:)

    counter = 0

    open(1, file='cities.txt', status='old', action='read')
    do while(.true.)
        read(1, '(a)', iostat=io) line
        if ( io .ne. 0 ) exit

        counter = counter + 1
    end do

    allocate(str(counter,1))
    rewind(1)

    do i = 1, counter
        read(1,*) str(i,1:)
    end do

    do i = 1, counter
        print*, str(i,1:)
    end do
end program density