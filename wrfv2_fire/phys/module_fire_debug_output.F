MODULE module_fire_debug

      USE module_domain
      USE module_configure
      TYPE(domain), POINTER, SAVE :: fd_domain_ptr
      TYPE(grid_config_rec_type), SAVE :: fd_config_flags

      CONTAINS

      SUBROUTINE set_fire_debug_vars(lgrid,lconfig)
      IMPLICIT NONE

      TYPE(domain),INTENT(IN),TARGET :: lgrid
      TYPE(grid_config_rec_type),INTENT(IN) :: lconfig

      fd_domain_ptr=>lgrid
      fd_config_flags=lconfig
      END SUBROUTINE set_fire_debug_vars

      SUBROUTINE fire_debug_write
      IMPLICIT NONE
      INTEGER :: stream
      stream=0
      CALL med_hist_out(fd_domain_ptr,stream,fd_config_flags)
      CALL med_restart_out(fd_domain_ptr,fd_config_flags)
      END SUBROUTINE fire_debug_write

END MODULE module_fire_debug


