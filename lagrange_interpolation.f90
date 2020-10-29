program lagrange_interpolation
  use sort
  use plot
  implicit none

!parameters
  double precision, parameter:: pi = 3.1415926535
  integer, parameter:: iunit = 90

!variables
  integer:: i, j, k, N
  double precision:: rand, xi, li, lki

!arrays
  double precision, allocatable:: x(:), y(:)


!init
  write(*, *) "input N"
  read(*, *) N

  allocate(x(N), y(N))

  do i=1, N
    call random_number(rand)
    x(i) = rand*2 - 1
  end do

  call bsort(x, N)

  open(iunit, file="./random.dat", status="replace")
  do i=1, N
    y(i) = sin(pi*x(i))
    write(iunit, *) x(i), y(i)
  end do
  close(iunit)

!main

  open(iunit, file="./interpolation.dat", status="replace")
  do i=1, 100
    xi = (dble(i)/100)*2 - 1
    li = 0
    do k=1, N
      lki = 1
      do j=1, N
        if(j == k) then
          continue
        else
          lki = lki*((xi - x(j))/(x(k) - x(j)))
        end if
      end do
      li = li + lki*y(k)
    end do
    write(iunit, *) xi, li
  end do
  close(iunit)

  open(iunit, file="./trueplot.dat", status="replace")
  do i=1, 100
    xi = (dble(i)/100)*2 - 1
    li = sin(pi*xi)
    write(iunit, *) xi, li
  end do
  close(iunit)

  call plot_options(file_name="plot", title="lagrange interpolation", xlabel="x", ylabel="y")
  call plot_graph(file_name="plot", dat_file_name="random")
  call plot_graph(file_name="plot", dat_file_name="interpolation", option="with lines")
  call plot_graph(file_name="plot", dat_file_name="trueplot", option="with lines")
  call plot_open("plot")

end program lagrange_interpolation
