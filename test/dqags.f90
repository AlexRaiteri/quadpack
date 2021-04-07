!
! Test dqags
!
! Evaluate
! \int_a^b f(x) dx where a=1, b=3, f(x)=x**2
!
program main
use quadpack, only: dqags
use iso_fortran_env, only: dp=>real64
implicit none

real(dp), parameter :: a = 1.0_dp
real(dp), parameter :: b = 3.0_dp
real(dp), parameter :: epsabs = 0.0_dp
real(dp), parameter :: epsrel = 0.05_dp
real(dp) :: r, abserr
integer :: neval, ier
integer, parameter :: limit = 4
integer, parameter :: lenw  = limit*4
integer :: last
integer :: iwork(limit)
real(dp) :: work(lenw)

call dqags(f, a, b, epsabs, epsrel, r, abserr, neval, ier, limit, lenw, last, iwork, work)

if (ier .eq. 0) then
    print *, 'OK: dqags:', r, '= 8.66666666...'
    print *, 'OK: Number of integrand evaluations:', neval
else
    print *, 'ERR: dqags:', ier
end if

contains

!
! f(x) = The integrand
!
function f(x) result(r)
    real(dp), intent(in) :: x
    real(dp) :: r
    r = x**2_dp
end function f

end program main
