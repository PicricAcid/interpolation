program spline_interpolation
  use linarg
  use sort
  use plot
  implicit none

!parameters
  double precision, parameter:: pi = 3.1415926535
  integer, parameter:: iunit = 90
  integer, parameter:: N = 5

!variables
  integer:: i, j, k, seedsize
  double precision:: rand, xi, yi, dx
  character:: seedc

!arrays
  double precision, allocatable:: x(:), y(:), b(:), c(:), d(:)
  integer, allocatable:: seed(:)

!init

  allocate(x(N), y(N), b(N), c(N), d(N))

  call random_seed(size=seedsize)
  allocate(seed(seedsize))
  do i = 1, seedsize
    call date_and_time(time=seedc)
    read(seedc, *) seed(i)
  end do
  call random_seed(put=seed(:))

  do i=1, N
    call random_number(rand)
    x(i) = rand
    call random_number(rand)
    y(i) = rand
  end do

  call bsort(x, N)

  open(iunit, file="./random.dat", status="replace")
  do i=1, N
    write(iunit, *) x(i), y(i)
  end do
  close(iunit)

!main

  call compute_a(N, x, y, b, c, d, 1) !open

  k = 100
  open(iunit, file="./open.dat", status="replace")
  do i=1, N-1
    dx = (x(i+1) - x(i))/dble(k)
    do j=1, k
      xi = dble(j - 1)*dx
      yi = y(i) + b(i)*xi + c(i)*xi**2 + d(i)*xi**3
      write(iunit, *) x(i)+xi, yi
    end do
  end do
  close(iunit)

  call compute_a(N, x, y, b, c, d, 2) !close
  open(iunit, file="./close.dat", status="replace")
  do i=1, N-1
    dx = (x(i+1) - x(i))/dble(k)
    do j=1, k
      xi = dble(j - 1)*dx
      yi = y(i) + b(i)*xi + c(i)*xi**2 + d(i)*xi**3
      write(iunit, *) x(i)+xi, yi
    end do
  end do
  close(iunit)

  call plot_options(file_name="plot", title="spline interpolation", xlabel="x", ylabel="y")
  call plot_graph(file_name="plot", dat_file_name="random")
  call plot_graph(file_name="plot", dat_file_name="open", option="with lines")
  call plot_graph(file_name="plot", dat_file_name="close", option="with lines")
  call plot_open("plot")

contains

  subroutine compute_a(n, x, y, b, c, d, flag)
    implicit none

    integer, intent(in):: n
    double precision, intent(in):: x(:), y(:)
    double precision, intent(inout):: b(:), c(:), d(:)
    integer, intent(in):: flag

    double precision:: A(n, n), bb(n)

    integer:: i, j
    double precision:: hi, hii, hn1

  ! make A, bb
  !init
    do i=1, n
      do j=1, n
        A(i, j) = 0
      end do
      bb(i) = 0
      b(i) = 0
      c(i) = 0
      d(i) = 0
    end do

    if(flag == 1) then
      A(1, 1) = 1
      A(n, n) = 1
    else
      A(1, 1) = 2*(x(2) - x(n))
      A(2, 1) = x(2) - x(1)
      A(n, n) = 2*(x(1) - x(n-1))
      A(n-1, n) = x(n) - x(n-1)
      bb(1) = 3*(y(2) - y(1))/(x(2) - x(1)) - 3*(y(1) - y(n))/(x(1) - x(n))
      bb(n) = 3*(y(1) - y(n))/(x(1) - x(n)) - 3*(y(n) - y(n-1))/(x(n) - x(n-1))
    end if

    do i=2, n-1
      hi = x(i) - x(i-1)
      hii = x(i+1) - x(i)
      A(i, i-1) = hi
      A(i, i) = 2*(hi + hii)
      A(i, i+1) = hii
      bb(i) = 3*(y(i+1) - y(i))/hii - 3*(y(i) - y(i-1))/hi
    end do

  !compute c
    call solve(n, A, bb, c)

    do i=1, n-1
      hii = x(i+1) - x(i)
      b(i) = (y(i+1) - y(i))/hii - (2*c(i) + c(i+1))*hii/3
      d(i) = (c(i+1) - c(i))/(3*hii)
    end do

  end subroutine compute_a

end program spline_interpolation
