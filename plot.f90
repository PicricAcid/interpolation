module plot
  implicit none
  private

  public:: plot_options, plot_graph, plot_open

contains
  subroutine to_gnuplot(file_name)
    implicit none
    character(*):: file_name
    integer:: status, system

    status = system("gnuplot "//file_name//".plt")
  end subroutine to_gnuplot

  subroutine file_open(file_name)
    implicit none
    character(*):: file_name
    integer:: status, system

    status = system("open -a preview "//file_name//".png")
  end subroutine file_open

  subroutine plt_file_delete(file_name)
    implicit none
    character(*):: file_name
    integer:: status, system

    status = system("rm -r "//file_name//".plt")
  end subroutine plt_file_delete

  subroutine plot_options(file_name, title, xlabel, ylabel, xrange, yrange)
    implicit none
    integer, parameter:: iunit = 90
    character(*):: file_name
    character(*), optional:: title, xlabel, ylabel
    double precision, optional:: xrange(2), yrange(2)

    open(iunit, file="./"//file_name//".plt", status="replace")

    write(iunit, *) "set terminal png"
    write(iunit, *) "set out 'plot.png'"
    if(present(title)) then
      write(iunit, *) "set title '", title, "'"
    end if
    if(present(xlabel)) then
      write(iunit, *) "set xlabel '", xlabel, "'"
    end if
    if(present(ylabel)) then
      write(iunit, *) "set ylabel '", ylabel, "'"
    end if
    if(present(xrange)) then
      write(iunit, *) "set xrange [", xrange(1), ":", xrange(2), "]"
    end if
    if(present(yrange)) then
      write(iunit, *) "set yrange [", yrange(1), ":", yrange(2), "]"
    end if

    write(iunit, "(a)", advance="no") "plot \"

    close(iunit)

  end subroutine plot_options

  subroutine plot_graph(file_name, dat_file_name, option)
    integer, parameter:: iunit = 90
    character(*):: file_name, dat_file_name
    character(*), optional:: option

    open(iunit, file="./"//file_name//".plt", status="old", position="append")
    if(present(option)) then
      write(iunit, "(a)", advance="no") "'"//dat_file_name//".dat' title '"//dat_file_name//"' "//option//", \"
    else
      write(iunit, "(a)", advance="no") "'"//dat_file_name//".dat' title '"//dat_file_name//"', \"
    end if
    close(iunit)

  end subroutine plot_graph

  subroutine plot_open(file_name)
    character(*):: file_name

    call to_gnuplot(file_name)
    call file_open(file_name)
    call plt_file_delete(file_name)

  end subroutine plot_open

end module plot
