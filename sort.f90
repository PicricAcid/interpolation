module sort
  implicit none
  private

  public:: bsort

contains
  subroutine bsort(a, n)
    implicit none
    double precision, intent(inout):: a(:)
    integer, intent(in):: n
    integer:: i, j
    double precision:: aa

    do i=1, n-1
      do j=i+1, n
        if(a(i) > a(j)) then
          aa = a(j)
          a(j) = a(i)
          a(i) = aa
        end if
      end do
    end do

  end subroutine bsort

end module sort
