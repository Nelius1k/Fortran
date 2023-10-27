program whereClause
    implicit none

    real :: a(5), b(5)
    
    data a /5.5, 2.1, 2, 3, 8/, b /1, 2, 2, 4, 5.5/

    where (a >= 1.0 .and. a <= b) a = 100
        
    print*, a

end program whereClause