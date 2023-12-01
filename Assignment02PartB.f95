program VehicleEntrySearch
    implicit none

    character(len=100) :: line
    character(len=10) :: make, color, plate
    integer :: year, month, day, hr, min, sec, iost, i

    open(10, file='logfile.txt')
    do i = 0, 2
        read(10,'(a)') line
    end do

    do 
        read(10,100) make, color, plate, year, month, day, hr, min, sec
        if (iost /= 0) then
            print*, "Error reading line:", iost
            exit
        end if
        print*, make, color, plate
    end do
    100 format(a8,1x,a7,3x,a9,1x,i5,1x,i3,1x,i2,1x,i3,1x,i3,1x,i3)
end program VehicleEntrySearch
