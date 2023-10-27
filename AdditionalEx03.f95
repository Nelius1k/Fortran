program ProjectileMotion
    implicit none

    real :: x, y, angle, vel, TIME
    real, parameter :: PI_VALUE = 3.141592653589793238
    real, parameter :: GRAVITY = 9.81
    integer :: i

    vel = 25.0
    angle = 35.0 * (PI_VALUE / 180.0)


    do i = 0, 100
        TIME = i * (4.0/100.0)
        x = vel * COS(angle) * TIME
        y = (vel * SIN(angle) * TIME) - ((0.5) * GRAVITY * (TIME**2))
        print*, "x = ",x, " y = ",y, " time = ", TIME
    end do

end program ProjectileMotion