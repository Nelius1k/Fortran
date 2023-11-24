program printColumn
    implicit none
    
    real :: line
    integer :: cnt, i, io
    integer, dimension(:,:), allocatable :: array

    cnt = 0

    open(1, file='data.dat', status='old')

    do 
        read(1, '(a)', iostat=io) line
        if(io .ne. 0) exit
        cnt = cnt + 1 
    end do

    rewind(1)
    allocate(array(cnt,2))
    

    call grab_col_items(1,cnt, array)
    !print*, array

    do i=1, cnt
        print*, array(i, 2)
    end do 

    close(1)

    contains

    subroutine grab_col_items(unit, length, arr) 
        implicit none

        integer, intent(in) :: length
        integer :: i, unit
        integer, dimension(length,2) :: arr

        do i=1, length
            read(unit, *) arr(i, :)
        end do

        rewind(unit)
        
    end subroutine grab_col_items

end program printColumn






