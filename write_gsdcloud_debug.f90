SUBROUTINE write_gsdcloud_debug(output_name, nz, nlat, nlon, & 
                                data_type, &
                                data_out_rkind, data_out_rsingle, data_out_ikind)
! ----------------------------------------------------------------------
! this program is used to write the intermediate output from
! gsdcloudanalysis.F90 to a netcdf file
! the purpose of having this subroutine is to help the understanding
! of the gsd cloud analysis process
!
! in: output_name:                      output filename
!     nz, nlat, nlon:                   output variable dimensions
!     data_type:			r_kind, r_single or i_kind
!     data_out_*:                       3d output variable
!					if data_type==r_kind: data_out_rkind would be picked up
!                                       if data_type==r_single: data_out_rsingle would be picked up
!                                       if data_type==i_kind: data_out_ikind would be picked up
!
! author: sijin zhang
! ----------------------------------------------------------------------
use kinds, only: r_kind,i_kind,r_single
use netcdf
implicit none


character(len=*),intent(in)::data_type			! r_kind, r_single and i_kind
character(len=*),intent(in)::output_name		! output filename
integer(i_kind) :: nz, nlat, nlon                       ! output variable dimensions
integer, parameter :: NDIMS = 3                         ! output is a 3d data

integer :: ncid, varid, dimids(NDIMS)
integer :: x_dim, y_dim, z_dim
real(r_kind) :: data_out_rkind(nlon, nlat, nz)
real(r_single) :: data_out_rsingle(nlon, nlat, nz)
integer(i_kind) :: data_out_ikind(nlon, nlat, nz)

integer :: x, y, z

write(*,*) 'write data for ', output_name

call check(nf90_create(output_name, NF90_CLOBBER, ncid))
call check(nf90_def_dim(ncid, "x", nlat, x_dim) )
call check(nf90_def_dim(ncid, "y", nlon, y_dim) )
call check(nf90_def_dim(ncid, "z", nz, z_dim) )


dimids =  (/ y_dim, x_dim, z_dim/)

call check( nf90_def_var(ncid, "data", NF90_FLOAT, dimids, varid) )
call check( nf90_enddef(ncid) )

if (data_type == "r_kind") then
   call check( nf90_put_var(ncid, varid, data_out_rkind) )
else if (data_type == "r_single") then
   call check( nf90_put_var(ncid, varid, data_out_rsingle) )
else if (data_type == "i_kind") then
   call check( nf90_put_var(ncid, varid, data_out_ikind) )
endif

call check( nf90_close(ncid) )

contains
  subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check
end SUBROUTINE write_gsdcloud_debug

