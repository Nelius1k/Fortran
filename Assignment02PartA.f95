program conversion
    implicit none

    integer :: i, j, cols
    real :: rows, startTemp, endTemp, increment, tempF, tempK
    real, dimension(:,:), allocatable :: myTable


    cols = 3;

    print*, "Please enter the starting and ending temperatures of your table (separate with a comma)"
    read*, startTemp, endTemp

    print*, "Please enter the increment of your table, a value between 1 and 210.0000"
    read*, increment

    do while ((startTemp < -10 .or. startTemp > 210) .or. (endTemp < -10 .or. endTemp > 210))
        print*, "Please enter the starting and ending temperatures of your table (values between -10 and 210)"
        read*, startTemp, endTemp
    end do
    
    if ( startTemp < 0.0 ) then
        rows = floor(((abs(startTemp) + endTemp)+1)/increment)
    else
        rows = floor(((endTemp+1) - startTemp)/increment)
    end if
    
    allocate(myTable(int(rows), cols))

    myTable(1, :) = [startTemp, 32 + (1.8*startTemp), startTemp + 273.15]

    do i = 2, int(rows)
        startTemp = startTemp + increment
        myTable(i, :) = [startTemp, 32 + (1.8*startTemp), startTemp + 273.15]
    end do
    
    !do i = 1, rows
     !   print*, myTable(i, 1), myTable(i, 2), myTable(i, 3)
    !end do

    write(*, '("   ", A, 6X, A, 6X, A)') "Celsius", "Fahrenheit", "Kelvin"
    
    do i = 1, int(rows)

        write(*, '(F10.5, A, F10.5, A, F10.5)') myTable(i, 1), "     ", myTable(i, 2), "     ", myTable(i, 3)

    end do


    deallocate(myTable)
end program conversion