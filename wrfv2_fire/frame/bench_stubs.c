#include <sys/time.h>

/* from RSL/compat.h */
#ifndef T3D

#  ifndef NOUNDERSCORE
#    ifdef F2CSTYLE
#   define RSL_INTERNAL_MILLICLOCK rsl_internal_milliclock__
#   define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock__
#    else
#   define RSL_INTERNAL_MILLICLOCK rsl_internal_milliclock_
#   define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock_

#    endif

#  else
#   define RSL_INTERNAL_MILLICLOCK rsl_internal_milliclock
#   define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock
#  endif
#endif

int RSL_INTERNAL_MILLICLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;  /* microseconds */
    int msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000 * isec + usec / 1000 ;
    return(msecs) ;
}
int RSL_INTERNAL_MICROCLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;  /* microseconds */
    int msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000000 * isec + usec ;
    return(msecs) ;
}

