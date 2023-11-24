program ABC
    implicit none
    

    integer  :: i, startYr, endYr, cnt, io, sum
    integer, dimension(:), allocatable :: x, y
    character(len=100) :: line

    print*, "This program calculates the total production of the ABC Co. during your desired startYrs."
    print*, "Please enter the starting and ending startYrs (comma or space separated)."
    print*, "Note: the years should be integer numbers between 2000 and 2022"
    read*, startYr, endYr

    do while ((startYr<2000 .or. startYr>2022) .or. (endYr<2000 .or. endYr>2022))
        print*, "The one or both entries are not between 2000 and 2022"
        print*, "Note: the years should be integer numbers between 2000 nad 2022"
        read*, startYr, endYr
    end do

    cnt = 0
    sum = 0

    open(1, file='ABCinput.txt', ERR=10)

    do 
        read(1, '(a)', iostat=io) line
        if(io .ne. 0) exit
        cnt = cnt + 1 
    end do

    allocate(x(23))
    allocate(y(23))

    rewind(1)

    do i=1, 2
        read(1, '(a)') line
    end do

    do i=2, 23
        read(1,100,iostat=io) x(i), y(i)
    end do

    100 format (i4,1x,i4)

    print*, y
    
    do i=1, 23
        if(x(i)>=startYr .and. x(i) <= endYr) then
            sum = sum + y(i)
        end if
    end do

    print*, "Total production", sum

    10 stop

end program ABC