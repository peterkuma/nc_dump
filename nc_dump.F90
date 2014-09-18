!OPTIONS XOPT(NOEVAL)

subroutine acraneb_dump(group, name, offset, data)
    USE PARKIND1  ,ONLY : JPIM    ,JPRB
    USE YOMPHY    ,ONLY : LRNUMX
    USE YOMHOOK   ,ONLY : LHOOK   ,DR_HOOK
    use oml_mod
    implicit none

    REAL(KIND=JPRB) :: ZHOOK_HANDLE

    character(len=*), intent(in) :: group
    character(len=*), intent(in) :: name
    integer, dimension(:), intent(in) :: offset
    real(kind=JPRB), dimension(:), intent(in) :: data

    integer, parameter :: UNIT = 700
    character(len=512) :: filename
    integer :: iostat
    integer, dimension(:), allocatable :: dims, block_size
    integer :: i, rank, tmp, recl, off, f
    logical :: exist, opened
    character(len=512) :: errmsg

    #include "abor1.intfb.h"

    IF (LHOOK) CALL DR_HOOK('ACRANEB_DUMP',0,ZHOOK_HANDLE)

    if (size(data) == 0) return ! Nothing to do.

    f = UNIT + oml_my_thread() ! We need this to be reentrant.
    write(filename,'(A)') group//'/'//name

    write(*,'(A,A,A,I3,A)') 'acraneb_dump: ', trim(filename), ' thread ', oml_my_thread(), ' started'

    inquire(file=trim(filename), exist=exist)
    if (.not. exist) then
        write(errmsg,'(A,A,A)') 'acraneb_dump: No such dataset "', trim(filename), '"'
        call abor1(errmsg)
    end if

    inquire(unit=f, opened=opened)
    if (opened) then
        write(errmsg,'(A,I3,A)') 'acraneb_dump: Unit ', f, ' is in use'
        call abor1(errmsg)
    end if

    ! Read dataset dimensions from file.
    open(unit=f, file=trim(filename)//'.dims', action='read')
    rank = -1
    iostat = 0
    do while (iostat == 0)
        rank = rank + 1
        read(f,'(I10)',iostat=iostat) tmp
    end do
    rewind(f)
    allocate(dims(rank))
    do i=1,rank
        read(f,'(I10)') dims(i)
    end do
    close(f)

    ! Calculate block size.
    allocate(block_size(rank))
    block_size(rank) = 1
    do i=rank-1,1,-1
        block_size(i) = block_size(i+1)*dims(i+1)
    end do

    ! Calculate offset.
    off = 0
    do i=1,size(offset)
        off = off + (offset(i)-1)*block_size(i)
    end do

    if (mod(off, size(data)) /= 0) then
        call abor1('acraneb_dump: Data size does not match sub-block size')
    end if

    ! Write data.
    inquire(iolength=recl) data
    open(unit=f, file=trim(filename), access='direct', recl=recl, &
        & status='old', iostat=iostat)

    write(f,rec=(off/size(data)+1)) data
    close(f)

    deallocate(block_size)
    deallocate(dims)

    write(*,'(A,A,A,I3,A)') 'acraneb_dump: ', trim(filename), ' thread ', oml_my_thread(), ' finished'
    IF (LHOOK) CALL DR_HOOK('ACRANEB_DUMP',1,ZHOOK_HANDLE)
end subroutine

subroutine acraneb_dataset(group, name, title, units, dtype, dims)
    USE PARKIND1  ,ONLY : JPIM    ,JPRB
    USE YOMPHY    ,ONLY : LRNUMX
    USE YOMHOOK   ,ONLY : LHOOK   ,DR_HOOK
    use oml_mod
    implicit none

    character(len=*), intent(in) :: group
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: title
    character(len=*), intent(in) :: units
    character(len=*), intent(in) :: dtype
    integer, dimension(:), intent(in) :: dims

    integer, parameter :: UNIT = 800
    character(len=512) :: filename
    integer :: i, f
    integer :: iostat
    logical :: exist, opened
    character(len=512) :: errmsg

    REAL(KIND=JPRB) :: ZHOOK_HANDLE
    IF (LHOOK) CALL DR_HOOK('ACRANEB_DATASET',0,ZHOOK_HANDLE)

    f = UNIT + oml_my_thread() ! We need this to be reentrant.
    write(filename,'(A)') group//'/'//name

    inquire(unit=f, opened=opened)
    if (opened) then
        write(errmsg,'(A,I3,A)') 'acraneb_dataset: Unit ', f, ' is in use'
        call abor1(errmsg)
    end if

    inquire(file=trim(filename), exist=exist)
    if (exist) return ! Dataset already exists (assumed), do nothing.

    write(*,'(A,A,A,I3,A)') 'acraneb_dataset: ', trim(filename), ' thread ', oml_my_thread(), ' started'

    call system('mkdir -p `dirname "    '//trim(filename)//'"`')

    open(unit=f, file=trim(filename), &
        & status='new', action='write', iostat=iostat)
    close(f)
    if (iostat /= 0) return ! Dataset already exists (assumed), do nothing.

    ! Write attributes.
    open(unit=f, file=trim(filename)//'.title', status='replace')
    write(f,'(A)') title
    close(f)

    open(unit=f, file=trim(filename)//'.dtype', status='replace')
    write(f,'(A)') dtype
    close(f)

    open(unit=f, file=trim(filename)//'.dims', status='replace')
    do i=1,size(dims)
        write(f,'(I10)') dims(i)
    end do
    close(f)

    open(unit=f, file=trim(filename)//'.units', status='replace')
    write(f,'(A)') units
    close(f)
    write(*,'(A,A,A,I3,A)')  'acraneb_dataset: ', trim(filename), ' thread ', oml_my_thread(), ' finished'
    IF (LHOOK) CALL DR_HOOK('ACRANEB_DATASET',1,ZHOOK_HANDLE)
end subroutine
