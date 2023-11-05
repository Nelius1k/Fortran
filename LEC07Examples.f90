program tri_in
implicit none
real :: aa,bb,cc
Print *, "Enter sides of your desier triangle in meters (comma separated)"
read (*,*) aa,bb,cc
write(*,100) "The area of a triangle with sides of length ",aa,",",bb, &
             ", and",cc," meters is:"
100 format ((a),f6.2,(a), f6.2,(a),f6.2,(a))
Print "(f6.2,a)",  area_of_triangle(aa,bb,cc)," square meter"

contains
 
real function area_of_triangle(a,b,c)
! this function computes the area of a triangle       
implicit none      
! declaration local variables and dummy arguments           
real :: a,b,c,s 
s=(a+b+c)/2.0
area_of_triangle=sqrt(s*(s-a)*(s-b)*(s-c))
end function area_of_triangle

end program tri_in




program tri_out
implicit none
   real :: aa,bb,cc,area_of_triangle
   Print *, "Enter sides of your desier triangle in meters (comma separated)"
   read (*,*) aa,bb,cc
   write(*,100) "the area of a triangle with sides of length ",aa,",",bb, &
   ", and",cc," meters is:"
   100 format ((a),f6.2,(a), f6.2,(a),f6.2,(a))
   Print "(f6.2,a)",area_of_triangle(aa,bb,cc)," square meter"
end program tri_out
 
real function area_of_triangle(a,b,c)
! this function computes the area of a triangle       
implicit none      
! declaration local variables and dummy arguments        
real :: a,b,c,s !,area_of_triangle
s=(a+b+c)/2.0
area_of_triangle=sqrt(s*(s-a)*(s-b)*(s-c))
end function area_of_triangle




program factorial_Cal
implicit none
   integer :: i,f,myfactorial
   Print *, "Enter a positive integer number to calculate its factorial"
   read*, i
   f = myfactorial(i)
   Print "(a,i30,a,i50)", "The value of factorial",i," is ",f
end program factorial_Cal
    
recursive function myfactorial (n) result (fac) 
! computes the factorial of n (n!)       
implicit none     
! declaration dummy arguments         
integer :: n,fac      
select case (n)         
case (0:1)         
   fac = 1         
case default    
   fac = n * myfactorial (n-1)  
end select 
end function myfactorial_Cal




program tri_sub_in
implicit none
real :: aa,bb,cc,tri_area
Print *, "Enter sides of your desier triangle in meters (comma separated)"
read (*,*) aa,bb,cc
write(*,100) "The area of a triangle with sides of length ",aa,",",bb, &
             ", and",cc," meters is:"
100 format ((a),f6.2,(a), f6.2,(a),f6.2,(a))
call area_of_triangle(aa,bb,cc,tri_area)
Print "(f6.2,a)",tri_area," square meter"

contains 

subroutine area_of_triangle(a,b,c,area)
! this subroutine computes the area of a triangle       
implicit none      
! declaration of dummy arguments and local variables        
real, intent(in) :: a,b,c
real :: s
real, intent(out) :: area
s=(a+b+c)/2.0
area=sqrt(s*(s-a)*(s-b)*(s-c))
end subroutine area_of_triangle

end program tri_sub_in




program tri_sub_out
implicit none
real :: aa,bb,cc,tri_area,s
Print *, "Enter sides of your desier triangle in meters (comma separated)"
read (*,*) aa,bb,cc
write(*,100) "The area of a triangle with sides of length ",aa,",",bb, &
             ", and",cc," meters is:"
100 format ((a),f6.2,(a), f6.2,(a),f6.2,(a))
call area_of_triangle(aa,bb,cc,tri_area)
Print "(f6.2,a)",tri_area," square meter"
print*, s
end program tri_sub_out
 
subroutine area_of_triangle(a,b,c,area)
! this subroutine computes the area of a triangle       
 implicit none      
! declaration of dummy arguments and local variables        
real :: a,b,c,s,area
s=(a+b+c)/2.0
print*, s
area=sqrt(s*(s-a)*(s-b)*(s-c))
end subroutine area_of_triangle




Program unknown_array_read
implicit none
  integer i,n,stat
  real x
  real, allocatable::a(:),b(:)
  open(20,file='data.dat')
  n=-1
  stat=0
  do while(stat == 0)
     n=n+1
     read(20,*,iostat=stat)x
  enddo
  write(*,*)n
  allocate(a(n))
  allocate(b(n))
  rewind(20)
  do i=1,n
  read(20,*)a(i),b(i)
  end do
  !a=transpose(a)
  do i=1,n
     write(*,*) b(i)
  enddo
end program unknown_array_read


