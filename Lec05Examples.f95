program array
implicit none

integer :: i, j, s1, s2, ii
real :: matrix0(3,3)
integer, parameter :: nx=3, ny=3
real, dimension (nx,ny) :: matrix1
real, dimension (-1:1,2:4) :: matrix2
real :: matrix3(-1:1,2:4)
real, dimension (:,:), allocatable :: matrix4
real :: matrix5(nx,ny)

do i=1,3
 do j=1,3
  matrix0(i,j)=i+j*sqrt(real(i*j))
 end do
end do

matrix1=1.0

do i=-1,1
 do j=2,4
  matrix2(i,j)=i+j*(i+j)**2
 end do
end do

data matrix3(:,2) /1.0, 2.0, 3.0/
data matrix3(:,3) /3*4.0/
data matrix3(:,4) /5.0, 6.0, 7.0/

print*, rank(matrix0), size(matrix0), shape(matrix0)
do i=1,3
 print*, matrix0(i,:)
end do

print*, rank(matrix1), size(matrix1), shape(matrix1)
do i=1,nx
 print*, matrix1(i,:)
end do

print*, rank(matrix2), size(matrix2), shape(matrix2)
do i=-1,1
 print*, matrix2(i,:)
end do

print*, rank(matrix3), size(matrix3), shape(matrix3)
do i=-1,1
 print*, matrix3(i,:)
end do

matrix5=transpose(matmul(matrix2,matrix3))
print*, rank(matrix5), size(matrix5), shape(matrix5)
do i=1,nx
 print*, matrix5(i,:)
end do

Stop

print*, 'enter the 1st and 2nd dimension sizes respectively for matrix4'
read*, s1,s2
allocate (matrix4(s1,s2))

do i=1,s1
 do j=1,s2
  matrix4(i,j)=i*j
 end do
end do

where (matrix4>5.0 .and.matrix4<8.0) matrix4=99.0

do j=1,s2
 forall (ii=2:s1, matrix4(ii,j)>=8.0 .and. matrix4(ii,j)<=9.0) 
  matrix4(ii,j)=999.0
 end forall
end do

print*, rank(matrix4), size(matrix4), shape(matrix4)
do i=1,s1
 print*, matrix4(i,:)
end do

deallocate (matrix4)


end program array



program charc_array
implicit none

character (len=15) :: surname, firstname
character (len=40) :: name
character (len=10) :: studentID
character (len=5) :: title
character (len=20) :: Code_name
character (len=3) :: code
integer :: num

title='Mr.'
firstname='John'
surname='Atkinson'
studentID='123456789'

name= title//firstname//surname
print*, 'Hello, I am ', name,' and my student number is: ', studentID

name= trim(title)//' '//trim(firstname)//' '//trim(surname)
print*, 'Hello, I am ', trim(name),' and my student number is: ', studentID

name= adjustl(title)//adjustl(firstname)//adjustl(surname)
print*, 'Hello, I am ', name,' and my student number is: ', studentID

name= adjustr(title)//adjustr(firstname)//adjustr(surname)
print*, 'Hello, I am ', name,' and my student number is: ', studentID

name= trim(title)//trim(surname)//trim(firstname)
studentID=adjustr(studentID)
Code_name(1:17)=name(1:17)
Code_name(18:20)=studentID(8:10)

read(Code_name(18:20),*) num

write(code(1:3),'(I3)') num

Print*, Code_name, num, code

if(index(Code_name, code) ==0) then
  Print*, 'code is not found'
else
  Print*, 'code is found at index: ', index(Code_name, code) 
end if

end program charc_array




program deriveDataType
implicit none
   integer :: i
   !type declaration
   type Books
      character(len = 50) :: title
      character(len = 50) :: author
      character(len = 150) :: subject
      integer :: book_id
   end type Books
   
   !declaring array of books
   type(Books), dimension(2) :: list 
    
   !accessing the components of the structure
   
   list(1)%title = "Fortran Programming"
   list(1)%author = "Donald Duck"
   list(1)%subject = "Fortran Programming Tutorial"
   list(1)%book_id = 123456 
   
   list(2)%title = "Python Programming"
   list(2)%author = "Mickey Mouse"
   list(2)%subject = "Python Programming Tutorial"
   list(2)%book_id = 654321
  
   !display book info
   
   !Print *, list(1)%title 
   !Print *, list(1)%author 
   !Print *, list(1)%subject 
   !Print *, list(1)%book_id  
   
   !Print *, list(2)%title 
   !Print *, list(2)%author 
   !Print *, list(2)%subject 
   !Print *, list(2)%book_id  
   
  do i=1,2
   Print *, list(i)
  enddo 
   
end program deriveDataType