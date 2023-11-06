program printColumn
    implicit none
    
    integer :: cnt
    real, dimension(:), allocatable :: array

    open(1, file='data.dat')

    cnt = length_of_file(1)

    print*, cnt

    close(1)

    contains 

    function length_of_file(unit) result(counter)
        implicit none
    
        integer :: unit
        integer :: io, counter
        character(len=15) :: line
    
        counter = 0
    
        do 
           read(unit, '(a)', iostat=io) line
           if(io .ne. 0) exit
           counter = counter + 1 
        end do
    
    end function length_of_file

end program printColumn






