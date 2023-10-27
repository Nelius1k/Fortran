program  evaluate
    implicit none
    
    real:: y
    integer:: x

    do x=1,30
        y = ((x**2) + (3 * TAN(1/sqrt(REAL(x))))) / ((6 * log10(REAL(x))) + 2)
        print*, "for x= ",x, " , y= ",y
    end do
end program  evaluate