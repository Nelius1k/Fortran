! Q1 f(x) = x^2-1, EPSILON = 1.E-3
!   When the subroutine is called with the above parameters we get
!   the correct zero of the function which is 1.0000305180437934

! Q2 EPSILON = 1.0E-6
!   When the subroutine is called with the above EPSILON we get
!   the correct zero of the function which is 1.0000000004656613

! Q3 f(x) = x^2+1
!   When the subroutine is called with the above function we get
!   stuck in an infinite loop


module Newton
    implicit none
    
contains

subroutine solve(f,fp,x0,n,x,iteration, EPSILON)
    real(kind=8), intent(inout) :: x0, x
    real(kind=8), intent(in) :: EPSILON
    
    real(kind=8) :: fx, fpx, f, fp, xnew

    integer, intent(out) ::  iteration
    integer, intent(in), optional :: n  ! optional maximum iteration parameter
    integer :: i

    iteration = 0  ! Initialize iteration counter

    x = x0

    do i = 1, n


        fx = f(x)
        fpx = fp(x)

        ! Check for division by zero to avoid potential issues
        if (fpx == 0.0d0) then
            print *, "Error: Division by zero. Cannot proceed."
            return
        end if

        if ( abs(fx) < EPSILON ) then
            print*, "The zero is: ", x
            exit
        end if

        print*, iteration, x, fx

        xnew = x - (fx / fpx)

        if ( iteration > 1000 ) then
            print*, "No convergence"
            exit
        end if

        x = xnew
        iteration = iteration + 1

    end do
    
end subroutine solve
    
end module Newton


Program Assignment03A
    use Newton
    implicit none
        integer, parameter :: n = 1000  ! maximum iteration
        real(kind=8), parameter :: EPSILON = 1.0E-6    !1.E-3  1.0E-6
        real(kind=8):: x0,xn
        integer:: iteration
        x0=3.0d0
        call solve(f,fp,x0,n,xn,iteration, EPSILON)  ! f and fp arguments are f(x) and fp(x) functions
   
        contains

    real (kind=8) function f(x)  ! this is f(x)
        implicit none
        real (kind=8), intent(in)::x
        f=x**2.0d0+1.0d0
    end function f

    real (kind=8) function fp(x)  ! This is f'(x)
        implicit none
        real (kind=8), intent(in)::x
        fp=2.0d0*x
    end function fp

end program Assignment03A
