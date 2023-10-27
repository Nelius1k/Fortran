program ifprog
! this program reads a grade and tells if it is a passing mark or not
implicit none
real (kind=4) :: grade
logical :: check
check=.true.
do while (check)
 print*,"enter your mark (note: your mark should be between 0 and 100)"
 read(*,*) grade
 if (grade > 100.0 .OR. grade < 0.0) then
  print*, "your mark is not between 0 and 100"
  check=.true.
  else if (grade >= 50.0) then
  Print*, "you have passed! :)"
  check=.false.
  else 
  print*, "you have failed! :("
  exit
 end if 
end do
Print*,"thank you for checking your make"
end program ifprog

Program caseprogram
implicit none
real :: age, height
integer :: a,h
print*,"We will tell you if you can use the device"
Print*,"please enter your age in years and your height in cm (comma separated)"
read(*,*) age, height
a=int(age)
h=int(height)
select case (a)
   case (0:25)
     select case (h)
        case (0:150)
        Print*,"you are a short junior, you can not use the device"
        case (151:200)
        Print*,"you are a tall junior, you can not use the device"
     end select  
   case (26:65)
   select case (h)
        case (0:150)
        Print*,"you are a short senior, you can not use the device"
        case (151:200)
        Print*,"you are a tall senior, you can use the device"
     end select
end select 
end program caseprogram

program array
implicit none

integer :: i,j
real,dimension(5) :: n
double precision,dimension(5) :: L
real,dimension (3,3):: matrix

n(1)=2.0
do i=2,5
 n(i)=i*2.0
end do

L=(/1.5D0, 3.2D0, 4.0D0, 5.76D0, 7.0D0/)
L(1:5:2)=10.0

do i=1,3
 do j=1,3
  matrix(i,j)=i+j
  print*, i, j, matrix(i,j)
 end do
end do

do i=1,5
print*, L(i)
end do

end program array