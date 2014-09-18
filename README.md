nc_dump
=======

Dump arrays to [netcdf](http://www.unidata.ucar.edu/software/netcdf/)
in ALADIN/ACRANEB numerical weather prediction models.

Usage
-----

1. Copy `nc_dump.intfb.h` to `Arp/interfaces` and `nc_dump.F90` to
   `Arp/phys_dmn`.

2. Initialize datasets in `Arp/adiab/cpg.F90`, e.g.:

    INTEGER :: N_DUMP_POINTS ! Number of data points to dump per NPROMA block.
    INTEGER :: DUMP_DIMS2(2) ! Dimensions specification for 2D arrays.
    INTEGER :: DUMP_DIMS3_HALF(3) ! Dimensions spec. for 3D arrays (half lev.).
    INTEGER :: DUMP_DIMS3_FULL(3) ! Dimensions spec. for 3D arrays (full lev.).
    ...

    #include "nc_dump.intfb.h"
    ...

    N_DUMP_POINTS=2 ! Dump only limited number of data points per NPROMA block.
    
    ! Dimensions of 1D arrays.
    DUMP_DIMS1=(/ CEILING(1.0*KGPCOMP/NPROMA)*N_DUMP_POINTS /)
    
    ! Dimensions of 2D arrays. -1 allows for arbitrary number of time steps.
    DUMP_DIMS2=(/ -1, CEILING(1.0*KGPCOMP/NPROMA)*N_DUMP_POINTS /)

    ! Dimensions of 3D arrays: half levels and full levels.
    DUMP_DIMS3_HALF=(/ -1, CEILING(1.0*KGPCOMP/NPROMA)*N_DUMP_POINTS, (NFLEVG+1) /)
    DUMP_DIMS3_FULL=(/ -1, CEILING(1.0*KGPCOMP/NPROMA)*N_DUMP_POINTS, NFLEVG /)

    ! 1D array: latitude (lon).
    CALL NC_DATASET('radiation', 'latitude', &
     & TITLE='Latitude', UNITS='degree', &
     & DTYPE='float64', DIMS=DUMP_DIMS1)
    
    ! 1D array: longitude (lon).
    CALL NC_DATASET('radiation', 'longitude', &
     & TITLE='Longitude', UNITS='degree', &
     & DTYPE='float64', DIMS=DUMP_DIMS1)

    ! 2D array: surface temperature (time x lon).
    CALL NC_DATASET('radiation', 'surface_temperature', &
     & TITLE='Surface Temperature', UNITS='K', &
     & DTYPE='float64', DIMS=DUMP_DIMS2)

    ! 3D array: pressure (time x lon x level)
    CALL NC_DATASET('radiation', 'pressure', &
     & TITLE='Pressure', UNITS='Pa', &
     & DTYPE='float64', DIMS=DUMP_DIMS3_FULL)

3. Dump dataset anywhere in the code.

    #include "nc_dump.intfb.h"
    ...

    ! Declaration of variables.
    INTEGER :: DUMP_OFFSET1(1) ! Offset for 1D arrays.
    INTEGER :: DUMP_OFFSET2(2) ! Offset for 2D & 3D arrays.
    ...

    N_DUMP_POINTS=2 ! Dump only limited number of data points per NPROMA block.
    DUMP_POINTS=(/ (JLON, JLON=1,KLON,(KLON/N_DUMP_POINTS)) /)
    
    ! 1D arrays: offset of the NPROMA block.
    DUMP_OFFSET1=(/ (KBL-1)*N_DUMP_POINTS+1 /)

    ! 2D & 3D arrays: time step, offset of the NPROMA block.
    DUMP_OFFSET2=(/ KSTEP+1, (KBL-1)*N_DUMP_POINTS+1 /)

    ! 1D arrays, independed of time.
    IF (KSTEP == 0) THEN
      ! Dump latitude (PGELAT).
      CALL NC_DUMP('radiation', 'latitude', DUMP_OFFSET1, PGELAT(DUMP_POINTS)*180.0/PI)
      ! Dump longitude (PGELAM).
      CALL NC_DUMP('radiation', 'longitude', DUMP_OFFSET1, PGELAM(DUMP_POINTS)*180.0/PI)
    END IF

    ! 2D arrays.
    ! Dump surface temperature (PTS).
    CALL NC_DUMP('radiation', 'surface_temperature', DUMP_OFFSET2, &
     & PTS(DUMP_POINTS))

    ! 3D arrays.
    ! Use PACK to flatten lon & level dimensions.
    ! Dump pressure (PAPRSF).
    CALL NC_DUMP('radiation', 'pressure', DUMP_OFFSET2, &
     & PACK(TRANSPOSE(PAPRSF(DUMP_POINTS,:)), .TRUE.))

4. Copy the resulting datasets to a local directory after the model is run,
   e.g. for dataset group named `radiation` (in Perl task script):

    foreach $file ( <ICMSH$exp+????>, <DHFDL$exp+????>, <radiation/*> ) {
      print "Copying $file to $OUTPUT/$file\n";
      $dir = dirname("$OUTPUT/$file");
      mkpath($dir, 0, 0755);
      copy($file, "$OUTPUT/$file") or
      &abort(
        "Cannot copy file:",
        "$file -> $ICMSH/$file",
        "$file -> $OUTPUT/$file",
      );
    }

5. Use [dump2h5](https://github.com/peterkuma/dump2h5) to convert
   raw dataset files to HDF5.

API
---

### nc_dataset

    subroutine nc_dataset(group, name, title, units, dtype, dims)

    character(len=*), intent(in) :: group
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: title
    character(len=*), intent(in) :: units
    character(len=*), intent(in) :: dtype
    integer, dimension(:), intent(in) :: dims

Initialize dataset `name` in group `group`. `title` is a human-readable
title of the dataset, `units` are the units of data values, `dtype`
is the type of data values, and `dims` is dimensions specification.

`group` and `name` are arbitrary group and dataset names. Dataset files
will be written to a local directory `group`.

Only `dtype` equal to `float64` is supported at the moment.

`dims` is an array specifying the size of dataset in each dimension.
The first element of `dims` is allowed to take the value of -1, in which
case the size along the first dimension is determined automatically.

### nc_dump

    subroutine nc_dump(group, name, offset, data)

    character(len=*), intent(in) :: group
    character(len=*), intent(in) :: name
    integer, dimension(:), intent(in) :: offset
    real(kind=JPRB), dimension(:), intent(in) :: data

Dump data `data` to dataset `name` in group `group`. `offset` is an array
specifying the offset along each dimension where data is to be saved.
`data` is a one-dimensional array. To save multi-dimensional data arrays,
flatten the dimensions with `PACK(data, .TRUE.)` when passing to `nc_dump`.

Bugs
----

Please report bugs to Peter Kuma <peterkuma@waveland.org>.
