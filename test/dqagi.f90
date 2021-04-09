!
! Test dqagi
!
! Evaluate
! \int_a^b f(x) dx, where a=1, b=infinity, f(x)=1/x^3
!
program main
use quadpack, only: dqagi
use iso_fortran_env, only: dp=>real64
implicit none

real(dp), parameter :: bound = 1.0_dp
integer, parameter  :: inf = 1
real(dp), parameter :: epsabs = 0.00_dp
real(dp), parameter :: epsrel = sqrt(epsilon(1.0_dp))
real(dp) :: r, abserr
integer :: neval, ier
integer, parameter :: limit = 128
integer, parameter :: lenw  = limit*4
integer :: last
integer :: iwork(limit)
real(dp) :: work(lenw)

call dqagi(f, bound, inf, epsabs, epsrel, r, abserr, neval, ier, limit, lenw, last, iwork, work)

if (ier .eq. 0) then
    print *, 'OK: dqagi: ', r, '= 0.5'
    print *, 'OK: Number of integrand evaluations:', neval
else
    print *, 'ERR: dqagi:', ier
end if

contains

!
! f(x) = The integrand
!
function f(x) result(r)
    real(dp), intent(in) :: x
    real(dp) :: r
    r = 1.0_dp/x**3.0_dp
end function f

end program main
