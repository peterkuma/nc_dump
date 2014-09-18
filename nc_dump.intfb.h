interface
    subroutine acraneb_dump(group, name, offset, data)
        USE PARKIND1,ONLY : JPRB
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: name
        integer, dimension(:), intent(in) :: offset
        real(kind=JPRB), dimension(:), intent(in) :: data
    end subroutine

    subroutine acraneb_dataset(group, name, title, units, dtype, dims)
        character(len=*), intent(in) :: group
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: title
        character(len=*), intent(in) :: units
        character(len=*), intent(in) :: dtype
        integer, dimension(:), intent(in) :: dims
    end subroutine
end interface
