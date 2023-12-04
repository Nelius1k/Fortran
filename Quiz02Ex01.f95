program XYZ
    implicit none

    character(len=60) :: name
    character(len=3) :: id
    character :: lastCharacter, charCheck
    integer :: idNum, i

    charCheck = 'n'

    open(10, file='XYZinput.txt')

    do i=1, 10
        read(10,100) name, id, idNum
        
        ! Get the last character of a string
        lastCharacter = name(len_trim(name):len(name))
        
        if ( (mod(idNum,10) .eq. 4) .and. (lastCharacter .eq. charCheck)) then
            print*, "Name: ", trim(name), " ID: ", idNum
        end if
    end do
    100 format(a16, 1x, a3, i6)

    close(10)
end program XYZ