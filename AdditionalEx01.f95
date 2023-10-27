program myprog
implicit none

real :: result, x, a

print*, "please enter 2 values for a and x"
read*, a, x

result = log10(x + sqrt((x*x) + (a*a)))

print*, "result = ", result
end program myprog