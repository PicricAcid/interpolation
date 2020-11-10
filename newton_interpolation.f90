program newton_interpolation
  use plot
  implicit none

!parameters
  double precision, parameter:: pi = 3.1415926535
  integer, parameter:: iunit = 90
  double precision, parameter:: minx = -1
  double precision, parameter:: maxx = 1

!variables
  integer:: i, j, N
  double precision:: h, xi, yi, li

!arrays
  double precision, allocatable:: x(:), y(:), a(:), c(:)


!init
  write(*, *) "input N"
  read(*, *) N

  allocate(x(N), y(N), a(N), c(N))

  h = (maxx - minx)/N

  open(iunit, file="./point.dat", status="replace")
  do i=1, N
    if(i == 1) then
      x(1) = minx
      y(1) = exp(-minx**2)*cos(pi*minx)
    else
      x(i) = x(i-1) + h
      y(i) = exp(-x(i)**2)*cos(pi*x(i))
    end if
    write(iunit, *) x(i), y(i)
  end do
  close(iunit)

!main

  do i=1, N
    a(i) = y(i)
    do j=i-1, 1, -1
      a(j) = (a(j+1) - a(j))/(x(i) - x(j))
    end do
    c(i) = a(1)
  end do

  open(iunit, file="./interpolation.dat", status="replace")
  do i=1, 100
    xi = (dble(i)/100)*2 - 1
    yi = c(N)
    do j=N-1, 1, -1
      yi = yi*(xi - x(j)) + c(j)
    end do
    write(iunit, *) xi, yi
  end do
  close(iunit)

  open(iunit, file="./funcplot.dat", status="replace")
  do i=1, 100
    xi = (dble(i)/100)*2 - 1
    li = exp(-xi**2)*cos(pi*xi)
    write(iunit, *) xi, li
  end do
  close(iunit)

  call plot_options(file_name="plot", title="newton interpolation", xlabel="x", ylabel="y")
  call plot_graph(file_name="plot", dat_file_name="point")
  call plot_graph(file_name="plot", dat_file_name="interpolation", option="with lines")
  call plot_graph(file_name="plot", dat_file_name="funcplot", option="with lines")
  call plot_open("plot")

end program newton_interpolation
