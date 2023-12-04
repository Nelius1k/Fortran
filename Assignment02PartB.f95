program VehicleEntrySearch
    implicit none

    character(len=100) :: line
    character(len=10) :: make, color, plate
    integer :: year, month, day, hr, min, sec, iost, i

    open(10, file='logfile.txt')
    do i = 0, 2
        read(10,'(a)') line
    end do

    open(11, file='output_suspects.txt', action='write')
    do 
        read(10,100, iostat=iost) make, color, plate, year, month, day, hr, min, sec
        if (iost /= 0) then
            if (iost == -1) then
                print*, "End of file reached."
            else
                print*, "Error reading line:", iost
            end if
            exit
        end if

        !write(*,100) make
        
        if ((make == 'Nissan') .and. (color == 'White') .and. (hr == 15)) then
            !write(*,100) year, month, day, hr, min, sec
            write(11,100) make, color, plate, year, month, day, hr, min, sec
        end if
    end do
    100 format(a8,1x,a7,3x,a9,1x,i4,2x,i1,1x,i2,1x,i2,1x,i2,1x,i2)

    close(10)
    close(11)
end program VehicleEntrySearch
