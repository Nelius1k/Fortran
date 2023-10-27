PROGRAM myprog
    REAL :: u(2), v(2)
    REAL :: dot_product

    ! Define the vectors u and v
    u = [3.0, 4.0]  ! u = 3i + 4j
    v = [-1.0, 2.0] ! v = -i + 2j

    ! Calculate the dot product
    dot_product = u(1) * v(1) + u(2) * v(2)

    ! Print the result
    PRINT *, "The dot product of u and v is:", dot_product

END PROGRAM myprog