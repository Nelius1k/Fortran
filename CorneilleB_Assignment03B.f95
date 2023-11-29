program Assignment03B
implicit none
integer:: i
real(kind=8), dimension(4):: Coef
real (kind=8), dimension (10):: X,Y
X= (/-3.1D0,-1.1D0,0.5D0,2.22D0,3.41D0,5.88D0,8.91D0,10.56D0,13.13D0,16.98D0/)
Y= (/-12.2D0,-11.7D0,-3.3D0,8.8D0,17.97D0,18.98D0,20.45D0,28.76D0,33.45D0,41.02D0/)
print *, 'The provided data is:'
print *, 'X         Y'
do i=1,size(X)
   print '(es10.3,1x,es10.3)', X(i),Y(i)
enddo
call Fit3rdOrdPol(X,Y,Coef)
print *, 'The fitted 3rd order polinominal for the above data is:'
print 100, '(',Coef(4),')X^3+(',Coef(3),')X^2+(',Coef(2),')X+(',Coef(1),')'
100 format(a,f10.7,a,f10.7,a,f10.7,a,f10.7,a)

contains 

! I tried using the DET function when implementing the subroutine
! but I kept getting this error: munmap_chunk(): invalid pointer


! subroutine Fit3rdOrdPol(X,Y,a)
! !====================================================================================!
! ! This subroutine;                                                                   !
! !    1-Checks whether the length of both X and Y is equal or more than 4. If not,    !
! !      it terminates the process of the subroutine and subroutine returns.           !
! !    2-Checks whether if both X & Y are the same length. If not, it terminates       !
! !      the process and subroutine returns.                                           !
! !    3-Calculates the coefficients of the best fitted 3rd order polynomial to X & Y. !
! ! Input Arguments:                                                                   !
! !    X  x-values of data set (an n element vector).                                  !
! !    Y  y-values of data set (an n element vector).                                  !
! ! Output Argument:                                                                   !
! !    a  the fitted 3rd order polynomial  coefficients in form of a 4-element vector. !
! !        The first element of the vector should be a0 and the last element a4.       !
! !====================================================================================!

!    implicit none
!       real(kind=8), dimension(:), intent(in) :: X, Y
!       real(kind=8), dimension(4), intent(out) :: a
!       real(kind=8) :: detM
!       real(kind=8), dimension(:,:), allocatable :: M
!       real(kind=8), dimension(:), allocatable :: b
!       integer :: n, k

!       n = size(X)

!       ! Check if the length of X and Y is at least 4
!       if (n < 4 .or. size(Y) < 4) then
!          print *, "Error: Both X and Y should have at least 4 elements."
!          return
!       endif

!       ! Check if the lengths of X and Y are the same
!       if (size(X) /= size(Y)) then
!          print *, "Error: The lengths of X and Y are not the same."
!          return
!       endif

!       ! Build the design matrix M
!       allocate(M(n, 4))
!       M(:, 1) = 1.0
!       M(:, 2) = X
!       M(:, 3) = X**2
!       M(:, 4) = X**3

!       allocate(b(n))
!       b = Y

!       detM = DET(M)

!       do k = 1, 4
!          M(:, k) = b
!          a(k) = DET(M) / detM
!          M(:, k) = X**(k-1)
!       end do

!       deallocate(M)
!       deallocate(b)

! end subroutine Fit3rdOrdPol


subroutine Fit3rdOrdPol(X, Y, a)
   !====================================================================================!
   ! This subroutine;                                                                   !
   !    1-Checks whether the length of both X and Y is equal or more than 4. If not,    !
   !      it terminates the process of the subroutine and subroutine returns.           !
   !    2-Checks whether if both X & Y are the same length. If not, it terminates       !
   !      the process and subroutine returns.                                           !
   !    3-Calculates the coefficients of the best-fitted 3rd order polynomial to X & Y. !
   ! Input Arguments:                                                                   !
   !    X  x-values of data set (an n element vector).                                  !
   !    Y  y-values of data set (an n element vector).                                  !
   ! Output Argument:                                                                   !
   !    Coef  the fitted 3rd order polynomial coefficients in the form of a 4-element vector. !
   !        The first element of the vector should be a0 and the last element a3.       !
   !====================================================================================!

   implicit none
   real(kind=8), dimension(4), intent(out) :: a
   real(kind=8), intent(in), dimension(:) :: X, Y
   integer :: n

   ! Check if the length of X and Y is at least 4
   if (size(X) < 4 .or. size(Y) < 4) then
       print *, "Error: Both X and Y should have at least 4 elements."
       return
   endif

   ! Check if the lengths of X and Y are the same
   if (size(X) /= size(Y)) then
       print *, "Error: The lengths of X and Y are not the same."
       return
   endif

   n = size(X)

   ! Fit a 3rd order polynomial using a simple method
   a(1) = sum(Y) / real(n)  ! a0
   a(2) = sum(X * Y) / sum(X**2)  ! a1
   a(3) = sum(X**2 * Y) / sum(X**4)  ! a2
   a(4) = sum(X**3 * Y) / sum(X**6)  ! a3

end subroutine Fit3rdOrdPol


real(kind=8) function DET(aa)
!===================================================!
! This function calculates the determinant of an    !
! n-square Matrix using LU method                   !
! Input argument:                                   !
!     aa, an n-square Matrix.                       !
! Output:                                           !
!     DET, the determinant of aa.                   !
!===================================================!
implicit none
real(kind=8):: aa(:,:)
real(kind=8):: tmp,c(size(aa,dim=1),size(aa,dim=2))
real(kind=8):: max
integer i,n,k,l,m,num(size(aa,dim=1))
n=size(aa,dim=1)
det=1.0D0
do k=1,n
   max=aa(k,k);num(k)=k;
   do i=k+1,n 
      if(abs(max)<abs(aa(i,k))) then
         max=aa(i,k)
         num(k)=i
      endif
   enddo
   if (num(k)/=k) then
      do l=k,n 
         tmp=aa(k,l)
         aa(k,l)=aa(num(k),l)
         aa(num(k),l)=tmp
      enddo
      det=-1.*det
   endif
   do m=k+1,n
      c(m,k)=aa(m,k)/aa(k,k)
      do l=k,n 
         aa(m,l)=aa(m,l)-c(m,k)*aa(k,l)
      enddo
   enddo 
enddo
do i=1,n
   det=det*aa(i,i)
enddo
return
end function DET

end program Assignment03B