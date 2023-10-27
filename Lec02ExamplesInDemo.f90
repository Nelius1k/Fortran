program lec2
implicit none
integer (kind=8) :: i
integer :: r,aaa
complex :: C
logical :: log
log=.true.
C=(5,6)
i=3*10**10
aaa=1:100
! print*, a
print*,'enter a real number'
read*, r
print*, huge(i), kind(i), i
print*, huge(r), kind(r), r
print*, kind(C), C
write(*,*) 'this is',log

end program lec2



program factorial
implicit none
! we want to calculate 1! 2! 3! . up to number 5!
integer :: nfact
integer :: n,m
logical :: check
character (len=40) :: name
print*, 'what is you name (max 40 letters)'
read*, name
print*, 'at what number you want to stop the calculation?'
read*,m
nfact=1
n=1
check=.true.
! construct factorials
do while (check)
   nfact=nfact*n
   print*, n, nfact, check
   n=n+1
   check=(n <= m)
end do
print*, 'this work was done by',name
end program factorial