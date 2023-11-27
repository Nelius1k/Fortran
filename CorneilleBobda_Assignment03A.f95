module Newton
    implicit none
    
contains

subroutine solve(f,fp,x0,n,x,iteration, EPSILON)
    real(kind=8), intent(inout) :: x0, x
    real(kind=8), intent(in) :: EPSILON
    
    real(kind=8) :: fx, fpx
    real(kind=8) :: f, fp

    integer, intent(out) ::  iteration
    integer, intent(in), optional :: n  ! optional maximum iteration parameter
    integer :: i

    iteration = 0  ! Initialize iteration counter

    !x = x0

    do i = 1, n

        fx = f(x0)
        fpx = fp(x0)

        ! Check for division by zero to avoid potential issues
        if (fpx == 0.0d0) then
            print *, "Error: Division by zero. Cannot proceed."
            return
        end if

        if ( abs(fx) < EPSILON ) then
            print*, "x0 is the result"
            exit
        end if

        print*, iteration, x0, fx

        x = x0 - (fx / fpx)

        if ( iteration > 1000 ) then
            print*, "No convergence"
            exit
        end if

        x0 = x
        iteration = iteration + 1

    end do
    
end subroutine solve
    
end module Newton


Program Assignment03A
    use Newton
    implicit none
        integer, parameter :: n = 1000  ! maximum iteration
        real(kind=8), parameter :: EPSILON = 1.E-3    !1.E-3  1.0E-6
        real(kind=8):: x0,xn
        integer:: iteration
        x0=-1.0d0
        call solve(f,fp,x0,n,xn,iteration, EPSILON)  ! f and fp arguments are f(x) and fp(x) functions
   
        contains

    real (kind=8) function f(x)  ! this is f(x)
        implicit none
        real (kind=8), intent(in)::x
        f=x**2.0d0-1.0d0
    end function f

    real (kind=8) function fp(x)  ! This is f'(x)
        implicit none
        real (kind=8), intent(in)::x
        fp=2.0d0*x
    end function fp

end program Assignment03A


! module Newton
!     implicit none
!   contains
  
!     subroutine solve(f, fp, x0, n, xn, iteration, EPSILON)
!       real(kind=8), intent(inout) :: x0, xn
!       integer, intent(out) :: iteration
!       real(kind=8), intent(in) :: EPSILON
!       integer, intent(in), optional :: n  ! optional maximum iteration parameter
  
!       real(kind=8) :: fx, fpx
!       integer :: i
  
!       ! Declare the functions f and fp
!       real(kind=8) :: f
!       real(kind=8) :: fp
  
!       iteration = 0  ! Initialize iteration counter
  
!       do i = 1, n
!         fx = f(x0)
!         fpx = fp(x0)
  
!         ! Check for division by zero to avoid potential issues
!         if (fpx == 0.0d0) then
!           print *, "Error: Division by zero. Cannot proceed."
!           return
!         end if
  
!         xn = x0 - fx / fpx
  
!         ! Check for convergence
!         if (abs(fx) < EPSILON .or. abs(xn - x0) < EPSILON) then
!           print *, "Converged in ", iteration, " iterations."
!           return
!         end if
  
!         x0 = xn  ! Update x0 for the next iteration
!         iteration = iteration + 1
!       end do
  
!       print *, "Did not converge in ", iteration, " iterations."
!     end subroutine solve
  
!   end module Newton
  
!   program Assignment03A
!     use Newton
!     implicit none
!     integer, parameter :: n = 1000  ! maximum iteration
!         real(kind=8), parameter :: EPSILON = 1.E-3    !1.E-3  1.0E-6
!         real(kind=8):: x0,xn
!         integer:: iteration
!         x0=1.0d0
!         call solve(f,fp,x0,n,xn,iteration, EPSILON) 
  
!     ! Comment to check if the correct zero is obtained
!     ! For f(x) = x^2 - 1, the correct zero is x = 1.0
!     ! The answer should be close to 1.0 if the implementation is correct
!     print *, "Result:", xn
  
!     ! Change EPSILON to 1.0E-6 and rerun the program
!     ! Comment the next line to test with EPSILON = 1.0E-6
!     !EPSILON = 1.0E-6
  
!     ! Comment to check for a function without zeros
!     ! For f(x) = x^2 + 1, there are no real zeros
!     ! The program should print that it did not converge in the maximum allowed iterations
!     x0 = 3.0d0  ! Reset x0
!     call solve(f, fp, x0, n, xn, iteration, EPSILON)

!     contains 

!     real(kind=8) function f(x)  ! this is f(x)
!     implicit none
!     real(kind=8), intent(in) :: x
!     f = x**2.0d0 + 1.0d0
!   end function f
  
!   real(kind=8) function fp(x)  ! This is f'(x)
!     implicit none
!     real(kind=8), intent(in) :: x
!     fp = 2.0d0 * x
!   end function fp
  
!   end program Assignment03A
  
