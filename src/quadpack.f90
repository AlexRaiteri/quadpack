module quadpack
use iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

private
public :: qags, dqags, qagi, dqagi, qawo, dqawo, qawc, dqawc, qaws, dqaws, qng, dqng, qag, dqag, qawf, dqawf

abstract interface

function integrand_sp(x) result(r)
    import sp
    real(sp), intent(in) :: x
    real(sp) :: r
end function

function integrand_dp(x) result(r)
    import dp
    real(dp), intent(in) :: x
    real(dp) :: r
end function

end interface

interface

subroutine qawf(f, a, omega, integr, epsabs, result, abserr, neval, ier, limlst, lst, leniw, maxp1, lenw, iwork, work)
    import sp
    procedure(integrand_sp) :: f
    real(sp), intent(in)    :: a
    real(sp), intent(in)    :: omega
    integer, intent(in)     :: integr
    real(sp), intent(in)    :: epsabs
    real(sp), intent(out)   :: result
    real(sp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limlst
    integer, intent(out)    :: lst
    integer, intent(in)     :: leniw
    integer, intent(in)     :: maxp1
    integer, intent(in)     :: lenw
    integer, intent(out)    :: iwork(leniw)
    real(sp), intent(out)   :: work(lenw)
end subroutine

subroutine dqawf(f, a, omega, integr, epsabs, result, abserr, neval, ier, limlst, lst, leniw, maxp1, lenw, iwork, work)
    import dp
    procedure(integrand_dp) :: f
    real(dp), intent(in)    :: a
    real(dp), intent(in)    :: omega
    integer, intent(in)     :: integr
    real(dp), intent(in)    :: epsabs
    real(dp), intent(out)   :: result
    real(dp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limlst
    integer, intent(out)    :: lst
    integer, intent(in)     :: leniw
    integer, intent(in)     :: maxp1
    integer, intent(in)     :: lenw
    integer, intent(out)    :: iwork(leniw)
    real(dp), intent(out)   :: work(lenw)
end subroutine

subroutine qag(f, a, b, epsabs, epsrel, key, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import sp
    procedure(integrand_sp) :: f
    real(sp), intent(in)    :: a
    real(sp), intent(in)    :: b
    real(sp), intent(in)    :: epsabs
    real(sp), intent(in)    :: epsrel
    integer, intent(in)     :: key
    real(sp), intent(out)   :: result
    real(sp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(limit)
    real(sp), intent(out)   :: work(lenw)
end subroutine

subroutine dqag(f, a, b, epsabs, epsrel, key, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import dp
    procedure(integrand_dp) :: f
    real(dp), intent(in)    :: a
    real(dp), intent(in)    :: b
    real(dp), intent(in)    :: epsabs
    real(dp), intent(in)    :: epsrel
    integer, intent(in)     :: key
    real(dp), intent(out)   :: result
    real(dp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(limit)
    real(dp), intent(out)   :: work(lenw)
end subroutine

subroutine qng(f, a, b, epsabs, epsrel, result, abserr, neval, ier)
    import sp
    procedure(integrand_sp) :: f
    real(sp), intent(in)    :: a
    real(sp), intent(in)    :: b
    real(sp), intent(in)    :: epsabs
    real(sp), intent(in)    :: epsrel
    real(sp), intent(out)   :: result
    real(sp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
end subroutine

subroutine dqng(f, a, b, epsabs, epsrel, result, abserr, neval, ier)
    import dp
    procedure(integrand_dp) :: f
    real(dp), intent(in)    :: a
    real(dp), intent(in)    :: b
    real(dp), intent(in)    :: epsabs
    real(dp), intent(in)    :: epsrel
    real(dp), intent(out)   :: result
    real(dp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
end subroutine

subroutine qaws(f, a, b, alfa, beta, integr, epsabs, epsrel, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import sp
    procedure(integrand_sp) :: f
    real(sp), intent(in)    :: a
    real(sp), intent(in)    :: b
    real(sp), intent(in)    :: alfa
    real(sp), intent(in)    :: beta
    integer, intent(in)     :: integr
    real(sp), intent(in)    :: epsabs
    real(sp), intent(in)    :: epsrel
    real(sp), intent(out)   :: result
    real(sp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(limit)
    real(sp), intent(out)   :: work(lenw)
end subroutine

subroutine dqaws(f, a, b, alfa, beta, integr, epsabs, epsrel, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import dp
    procedure(integrand_dp) :: f
    real(dp), intent(in)    :: a
    real(dp), intent(in)    :: b
    real(dp), intent(in)    :: alfa
    real(dp), intent(in)    :: beta
    integer, intent(in)     :: integr
    real(dp), intent(in)    :: epsabs
    real(dp), intent(in)    :: epsrel
    real(dp), intent(out)   :: result
    real(dp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(limit)
    real(dp), intent(out)   :: work(lenw)
end subroutine

subroutine qawc(f, a, b, c, epsabs, epsrel, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import sp
    procedure(integrand_sp) :: f
    real(sp), intent(in)    :: a
    real(sp), intent(in)    :: b
    real(sp), intent(in)    :: c
    real(sp), intent(in)    :: epsabs
    real(sp), intent(in)    :: epsrel
    real(sp), intent(out)   :: result
    real(sp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(limit)
    real(sp), intent(out)   :: work(lenw)
end subroutine

subroutine dqawc(f, a, b, c, epsabs, epsrel, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import dp
    procedure(integrand_dp) :: f
    real(dp), intent(in)    :: a
    real(dp), intent(in)    :: b
    real(dp), intent(in)    :: c
    real(dp), intent(in)    :: epsabs
    real(dp), intent(in)    :: epsrel
    real(dp), intent(out)   :: result
    real(dp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(limit)
    real(dp), intent(out)   :: work(lenw)
end subroutine

subroutine qawo(f, a, b, omega, integr, epsabs, epsrel, result, abserr, &
                                neval, ier, leniw, maxp1, lenw, last, iwork, work)
    import sp
    procedure(integrand_sp) :: f
    real(sp), intent(in)    :: a
    real(sp), intent(in)    :: b
    real(sp), intent(in)    :: omega
    integer, intent(in)     :: integr
    real(sp), intent(in)    :: epsabs
    real(sp), intent(in)    :: epsrel
    real(sp), intent(out)   :: result
    real(sp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: leniw
    integer, intent(in)     :: maxp1
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(leniw)
    real(sp), intent(out)   :: work(lenw)
end subroutine

subroutine dqawo(f, a, b, omega, integr, epsabs, epsrel, result, abserr, &
                                neval, ier, leniw, maxp1, lenw, last, iwork, work)
    import dp
    procedure(integrand_dp) :: f
    real(dp), intent(in)    :: a
    real(dp), intent(in)    :: b
    real(dp), intent(in)    :: omega
    integer, intent(in)     :: integr
    real(dp), intent(in)    :: epsabs
    real(dp), intent(in)    :: epsrel
    real(dp), intent(out)   :: result
    real(dp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: leniw
    integer, intent(in)     :: maxp1
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(leniw)
    real(dp), intent(out)   :: work(lenw)
end subroutine

subroutine qagi(f, bound, inf, epsabs, epsrel, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import sp
    procedure(integrand_sp) :: f
    real(sp), intent(in)    :: bound
    integer, intent(in)     :: inf
    real(sp), intent(in)    :: epsabs
    real(sp), intent(in)    :: epsrel
    real(sp), intent(out)   :: result
    real(sp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(limit)
    real(sp), intent(out)   :: work(lenw)
end subroutine

subroutine dqagi(f, bound, inf, epsabs, epsrel, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import dp
    procedure(integrand_dp) :: f
    real(dp), intent(in)    :: bound
    integer, intent(in)     :: inf
    real(dp), intent(in)    :: epsabs
    real(dp), intent(in)    :: epsrel
    real(dp), intent(out)   :: result
    real(dp), intent(out)   :: abserr
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: last
    integer, intent(out)    :: iwork(limit)
    real(dp), intent(out)   :: work(lenw)
end subroutine

subroutine qags(f, a, b, epsabs, epsrel, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import sp
    procedure(integrand_sp) :: f
    real(sp), intent(in)    :: a
    real(sp), intent(in)    :: b
    real(sp), intent(in)    :: epsabs
    real(sp), intent(in)    :: epsrel
    real(sp), intent(out)   :: abserr
    real(sp), intent(out)   :: result
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(out)    :: last
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: iwork(limit)
    real(sp), intent(out)   :: work(lenw)
end subroutine

subroutine dqags(f, a, b, epsabs, epsrel, result, abserr, neval, ier, limit, lenw, last, iwork, work)
    import dp
    procedure(integrand_dp) :: f
    real(dp), intent(in)    :: a
    real(dp), intent(in)    :: b
    real(dp), intent(in)    :: epsabs
    real(dp), intent(in)    :: epsrel
    real(dp), intent(out)   :: abserr
    real(dp), intent(out)   :: result
    integer, intent(out)    :: neval
    integer, intent(out)    :: ier
    integer, intent(out)    :: last
    integer, intent(in)     :: limit
    integer, intent(in)     :: lenw
    integer, intent(out)    :: iwork(limit)
    real(dp), intent(out)   :: work(lenw)
end subroutine

end interface

end module quadpack
