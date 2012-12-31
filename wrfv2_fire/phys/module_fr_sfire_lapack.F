!  ********
!  This is a source file obtained from netlib.org as ssysv + dependencies and
!  converted from fortran 77 to fortran 90 style using Michael Metcalfe's 
!  convert.f90 and converted into a module to prevent polution of the WRF namespace.
!  Martin Vejmelka vejmelkam@gmail.com 2012
!  ********

module module_fr_sfire_lapack
implicit none

public::SSYSV, SGEMM

contains

!> \brief \b IEEECK                                                     
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download IEEECK + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       INTEGER          FUNCTION IEEECK( ISPEC, ZERO, ONE )            
!                                                                       
!       .. Scalar Arguments ..                                          
!       INTEGER            ISPEC                                        
!       REAL               ONE, ZERO                                    
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> IEEECK is called from the ILAENV to verify that Infinity and         
!> possibly NaN arithmetic is safe (i.e. will not trap).                
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] ISPEC                                                     
!> \verbatim                                                            
!>          ISPEC is INTEGER                                            
!>          Specifies whether to test just for inifinity arithmetic     
!>          or whether to test for infinity and NaN arithmetic.         
!>          = 0: Verify infinity arithmetic only.                       
!>          = 1: Verify infinity and NaN arithmetic.                    
!> \endverbatim                                                         
!>                                                                      
!> \param[in] ZERO                                                      
!> \verbatim                                                            
!>          ZERO is REAL                                                
!>          Must contain the value 0.0                                  
!>          This is passed to prevent the compiler from optimizing      
!>          away this code.                                             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] ONE                                                       
!> \verbatim                                                            
!>          ONE is REAL                                                 
!>          Must contain the value 1.0                                  
!>          This is passed to prevent the compiler from optimizing      
!>          away this code.                                             
!>                                                                      
!>  RETURN VALUE:  INTEGER                                              
!>          = 0:  Arithmetic failed to produce the correct answers      
!>          = 1:  Arithmetic produced the correct answers               
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup auxOTHERauxiliary                                           
!                                                                       
!  =====================================================================
      INTEGER FUNCTION IEEECK (ISPEC, ZERO, ONE) 
!                                                                       
!  -- LAPACK auxiliary routine (version 3.4.0) --                       
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      INTEGER ISPEC 
      REAL ONE, ZERO 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL NAN1, NAN2, NAN3, NAN4, NAN5, NAN6, NEGINF, NEGZRO, NEWZRO,  &
      POSINF                                                            
!     ..                                                                
!     .. Executable Statements ..                                       
      IEEECK = 1 
!                                                                       
      POSINF = ONE / ZERO 
      IF (POSINF.LE.ONE) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      NEGINF = - ONE / ZERO 
      IF (NEGINF.GE.ZERO) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      NEGZRO = ONE / (NEGINF + ONE) 
      IF (NEGZRO.NE.ZERO) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      NEGINF = ONE / NEGZRO 
      IF (NEGINF.GE.ZERO) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      NEWZRO = NEGZRO + ZERO 
      IF (NEWZRO.NE.ZERO) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      POSINF = ONE / NEWZRO 
      IF (POSINF.LE.ONE) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      NEGINF = NEGINF * POSINF 
      IF (NEGINF.GE.ZERO) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      POSINF = POSINF * POSINF 
      IF (POSINF.LE.ONE) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
!                                                                       
!                                                                       
!                                                                       
!     Return if we were only asked to check infinity arithmetic         
!                                                                       
      IF (ISPEC.EQ.0) RETURN 
!                                                                       
      NAN1 = POSINF + NEGINF 
!                                                                       
      NAN2 = POSINF / NEGINF 
!                                                                       
      NAN3 = POSINF / POSINF 
!                                                                       
      NAN4 = POSINF * ZERO 
!                                                                       
      NAN5 = NEGINF * NEGZRO 
!                                                                       
      NAN6 = NAN5 * ZERO 
!                                                                       
      IF (NAN1.EQ.NAN1) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      IF (NAN2.EQ.NAN2) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      IF (NAN3.EQ.NAN3) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      IF (NAN4.EQ.NAN4) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      IF (NAN5.EQ.NAN5) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      IF (NAN6.EQ.NAN6) THEN 
        IEEECK = 0 
        RETURN 
      ENDIF 
!                                                                       
      RETURN 
      END FUNCTION IEEECK                           
!> \brief \b ILAENV                                                     
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download ILAENV + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       INTEGER FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4 )    
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER*( * )    NAME, OPTS                                   
!       INTEGER            ISPEC, N1, N2, N3, N4                        
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> ILAENV is called from the LAPACK routines to choose problem-dependent
!> parameters for the local environment.  See ISPEC for a description of
!> the parameters.                                                      
!>                                                                      
!> ILAENV returns an INTEGER                                            
!> if ILAENV >= 0: ILAENV returns the value of the parameter specified b
!> if ILAENV < 0:  if ILAENV = -k, the k-th argument had an illegal valu
!>                                                                      
!> This version provides a set of parameters which should give good,    
!> but not optimal, performance on many of the currently available      
!> computers.  Users are encouraged to modify this subroutine to set    
!> the tuning parameters for their particular machine using the option  
!> and problem size information in the arguments.                       
!>                                                                      
!> This routine will not function correctly if it is converted to all   
!> lower case.  Converting it to all upper case is allowed.             
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] ISPEC                                                     
!> \verbatim                                                            
!>          ISPEC is INTEGER                                            
!>          Specifies the parameter to be returned as the value of      
!>          ILAENV.                                                     
!>          = 1: the optimal blocksize; if this value is 1, an unblocked
!>               algorithm will give the best performance.              
!>          = 2: the minimum block size for which the block routine     
!>               should be used; if the usable block size is less than  
!>               this value, an unblocked routine should be used.       
!>          = 3: the crossover point (in a block routine, for N less    
!>               than this value, an unblocked routine should be used)  
!>          = 4: the number of shifts, used in the nonsymmetric         
!>               eigenvalue routines (DEPRECATED)                       
!>          = 5: the minimum column dimension for blocking to be used;  
!>               rectangular blocks must have dimension at least k by m,
!>               where k is given by ILAENV(2,...) and m by ILAENV(5,...
!>          = 6: the crossover point for the SVD (when reducing an m by 
!>               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
!>               this value, a QR factorization is used first to reduce 
!>               the matrix to a triangular form.)                      
!>          = 7: the number of processors                               
!>          = 8: the crossover point for the multishift QR method       
!>               for nonsymmetric eigenvalue problems (DEPRECATED)      
!>          = 9: maximum size of the subproblems at the bottom of the   
!>               computation tree in the divide-and-conquer algorithm   
!>               (used by xGELSD and xGESDD)                            
!>          =10: ieee NaN arithmetic can be trusted not to trap         
!>          =11: infinity arithmetic can be trusted not to trap         
!>          12 <= ISPEC <= 16:                                          
!>               xHSEQR or one of its subroutines,                      
!>               see IPARMQ for detailed explanation                    
!> \endverbatim                                                         
!>                                                                      
!> \param[in] NAME                                                      
!> \verbatim                                                            
!>          NAME is CHARACTER*(*)                                       
!>          The name of the calling subroutine, in either upper case or 
!>          lower case.                                                 
!> \endverbatim                                                         
!>                                                                      
!> \param[in] OPTS                                                      
!> \verbatim                                                            
!>          OPTS is CHARACTER*(*)                                       
!>          The character options to the subroutine NAME, concatenated  
!>          into a single character string.  For example, UPLO = 'U',   
!>          TRANS = 'T', and DIAG = 'N' for a triangular routine would  
!>          be specified as OPTS = 'UTN'.                               
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N1                                                        
!> \verbatim                                                            
!>          N1 is INTEGER                                               
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N2                                                        
!> \verbatim                                                            
!>          N2 is INTEGER                                               
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N3                                                        
!> \verbatim                                                            
!>          N3 is INTEGER                                               
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N4                                                        
!> \verbatim                                                            
!>          N4 is INTEGER                                               
!>          Problem dimensions for the subroutine NAME; these may not al
!>          be required.                                                
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup auxOTHERauxiliary                                           
!                                                                       
!> \par Further Details:                                                
!  =====================                                                
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!>  The following conventions have been used when calling ILAENV from th
!>  LAPACK routines:                                                    
!>  1)  OPTS is a concatenation of all of the character options to      
!>      subroutine NAME, in the same order that they appear in the      
!>      argument list for NAME, even if they are not used in determining
!>      the value of the parameter specified by ISPEC.                  
!>  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
!>      that they appear in the argument list for NAME.  N1 is used     
!>      first, N2 second, and so on, and unused problem dimensions are  
!>      passed a value of -1.                                           
!>  3)  The parameter value returned by ILAENV is checked for validity i
!>      the calling subroutine.  For example, ILAENV is used to retrieve
!>      the optimal blocksize for STRTRI as follows:                    
!>                                                                      
!>      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )         
!>      IF( NB.LE.1 ) NB = MAX( 1, N )                                  
!> \endverbatim                                                         
!>                                                                      
!  =====================================================================
      INTEGER FUNCTION ILAENV (ISPEC, NAME, OPTS, N1, N2, N3, N4) 
!                                                                       
!  -- LAPACK auxiliary routine (version 3.4.0) --                       
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER ( * ) NAME, OPTS 
      INTEGER ISPEC, N1, N2, N3, N4 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      INTEGER I, IC, IZ, NB, NBMIN, NX 
      LOGICAL CNAME, SNAME 
      CHARACTER C1 * 1, C2 * 2, C4 * 2, C3 * 3, SUBNAM * 6 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC CHAR, ICHAR, INT, MIN, REAL 
!     ..                                                                
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
      GOTO (10, 10, 10, 80, 90, 100, 110, 120, 130, 140, 150, 160, 160, &
      160, 160, 160) ISPEC                                              
!                                                                       
!     Invalid value for ISPEC                                           
!                                                                       
      ILAENV = - 1 
      RETURN 
!                                                                       
   10 CONTINUE 
!                                                                       
!     Convert NAME to upper case if the first character is lower case.  
!                                                                       
      ILAENV = 1 
      SUBNAM = NAME 
      IC = ICHAR (SUBNAM (1:1) ) 
      IZ = ICHAR ('Z') 
      IF (IZ.EQ.90.OR.IZ.EQ.122) THEN 
!                                                                       
!        ASCII character set                                            
!                                                                       
        IF (IC.GE.97.AND.IC.LE.122) THEN 
          SUBNAM (1:1) = CHAR (IC - 32) 
          DO 20 I = 2, 6 
            IC = ICHAR (SUBNAM (I:I) ) 
            IF (IC.GE.97.AND.IC.LE.122) SUBNAM (I:I) = CHAR (IC - 32) 
   20     END DO 
        ENDIF 
!                                                                       
      ELSEIF (IZ.EQ.233.OR.IZ.EQ.169) THEN 
!                                                                       
!        EBCDIC character set                                           
!                                                                       
        IF ( (IC.GE.129.AND.IC.LE.137) .OR. (IC.GE.145.AND.IC.LE.153)   &
        .OR. (IC.GE.162.AND.IC.LE.169) ) THEN                           
          SUBNAM (1:1) = CHAR (IC + 64) 
          DO 30 I = 2, 6 
            IC = ICHAR (SUBNAM (I:I) ) 
            IF ( (IC.GE.129.AND.IC.LE.137) .OR. (                       &
            IC.GE.145.AND.IC.LE.153) .OR. (IC.GE.162.AND.IC.LE.169) )   &
            SUBNAM (I:I) = CHAR (IC + 64)                               
   30     END DO 
        ENDIF 
!                                                                       
      ELSEIF (IZ.EQ.218.OR.IZ.EQ.250) THEN 
!                                                                       
!        Prime machines:  ASCII+128                                     
!                                                                       
        IF (IC.GE.225.AND.IC.LE.250) THEN 
          SUBNAM (1:1) = CHAR (IC - 32) 
          DO 40 I = 2, 6 
            IC = ICHAR (SUBNAM (I:I) ) 
            IF (IC.GE.225.AND.IC.LE.250) SUBNAM (I:I) = CHAR (IC - 32) 
   40     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      C1 = SUBNAM (1:1) 
      SNAME = C1.EQ.'S'.OR.C1.EQ.'D' 
      CNAME = C1.EQ.'C'.OR.C1.EQ.'Z' 
      IF (.NOT. (CNAME.OR.SNAME) ) RETURN 
      C2 = SUBNAM (2:3) 
      C3 = SUBNAM (4:6) 
      C4 = C3 (2:3) 
!                                                                       
      GOTO (50, 60, 70) ISPEC 
!                                                                       
   50 CONTINUE 
!                                                                       
!     ISPEC = 1:  block size                                            
!                                                                       
!     In these examples, separate code is provided for setting NB for   
!     real and complex.  We assume that NB will take the same value in  
!     single or double precision.                                       
!                                                                       
      NB = 1 
!                                                                       
      IF (C2.EQ.'GE') THEN 
        IF (C3.EQ.'TRF') THEN 
          IF (SNAME) THEN 
            NB = 64 
          ELSE 
            NB = 64 
          ENDIF 
        ELSEIF (                                                        &
        C3.EQ.'QRF'.OR.C3.EQ.'RQF'.OR.C3.EQ.'LQF'.OR.C3.EQ.'QLF') THEN  
          IF (SNAME) THEN 
            NB = 32 
          ELSE 
            NB = 32 
          ENDIF 
        ELSEIF (C3.EQ.'HRD') THEN 
          IF (SNAME) THEN 
            NB = 32 
          ELSE 
            NB = 32 
          ENDIF 
        ELSEIF (C3.EQ.'BRD') THEN 
          IF (SNAME) THEN 
            NB = 32 
          ELSE 
            NB = 32 
          ENDIF 
        ELSEIF (C3.EQ.'TRI') THEN 
          IF (SNAME) THEN 
            NB = 64 
          ELSE 
            NB = 64 
          ENDIF 
        ENDIF 
      ELSEIF (C2.EQ.'PO') THEN 
        IF (C3.EQ.'TRF') THEN 
          IF (SNAME) THEN 
            NB = 64 
          ELSE 
            NB = 64 
          ENDIF 
        ENDIF 
      ELSEIF (C2.EQ.'SY') THEN 
        IF (C3.EQ.'TRF') THEN 
          IF (SNAME) THEN 
            NB = 64 
          ELSE 
            NB = 64 
          ENDIF 
        ELSEIF (SNAME.AND.C3.EQ.'TRD') THEN 
          NB = 32 
        ELSEIF (SNAME.AND.C3.EQ.'GST') THEN 
          NB = 64 
        ENDIF 
      ELSEIF (CNAME.AND.C2.EQ.'HE') THEN 
        IF (C3.EQ.'TRF') THEN 
          NB = 64 
        ELSEIF (C3.EQ.'TRD') THEN 
          NB = 32 
        ELSEIF (C3.EQ.'GST') THEN 
          NB = 64 
        ENDIF 
      ELSEIF (SNAME.AND.C2.EQ.'OR') THEN 
        IF (C3 (1:1) .EQ.'G') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NB = 32 
          ENDIF 
        ELSEIF (C3 (1:1) .EQ.'M') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NB = 32 
          ENDIF 
        ENDIF 
      ELSEIF (CNAME.AND.C2.EQ.'UN') THEN 
        IF (C3 (1:1) .EQ.'G') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NB = 32 
          ENDIF 
        ELSEIF (C3 (1:1) .EQ.'M') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NB = 32 
          ENDIF 
        ENDIF 
      ELSEIF (C2.EQ.'GB') THEN 
        IF (C3.EQ.'TRF') THEN 
          IF (SNAME) THEN 
            IF (N4.LE.64) THEN 
              NB = 1 
            ELSE 
              NB = 32 
            ENDIF 
          ELSE 
            IF (N4.LE.64) THEN 
              NB = 1 
            ELSE 
              NB = 32 
            ENDIF 
          ENDIF 
        ENDIF 
      ELSEIF (C2.EQ.'PB') THEN 
        IF (C3.EQ.'TRF') THEN 
          IF (SNAME) THEN 
            IF (N2.LE.64) THEN 
              NB = 1 
            ELSE 
              NB = 32 
            ENDIF 
          ELSE 
            IF (N2.LE.64) THEN 
              NB = 1 
            ELSE 
              NB = 32 
            ENDIF 
          ENDIF 
        ENDIF 
      ELSEIF (C2.EQ.'TR') THEN 
        IF (C3.EQ.'TRI') THEN 
          IF (SNAME) THEN 
            NB = 64 
          ELSE 
            NB = 64 
          ENDIF 
        ENDIF 
      ELSEIF (C2.EQ.'LA') THEN 
        IF (C3.EQ.'UUM') THEN 
          IF (SNAME) THEN 
            NB = 64 
          ELSE 
            NB = 64 
          ENDIF 
        ENDIF 
      ELSEIF (SNAME.AND.C2.EQ.'ST') THEN 
        IF (C3.EQ.'EBZ') THEN 
          NB = 1 
        ENDIF 
      ENDIF 
      ILAENV = NB 
      RETURN 
!                                                                       
   60 CONTINUE 
!                                                                       
!     ISPEC = 2:  minimum block size                                    
!                                                                       
      NBMIN = 2 
      IF (C2.EQ.'GE') THEN 
        IF (C3.EQ.'QRF'.OR.C3.EQ.'RQF'.OR.C3.EQ.'LQF'.OR.C3.EQ.'QLF')   &
        THEN                                                            
          IF (SNAME) THEN 
            NBMIN = 2 
          ELSE 
            NBMIN = 2 
          ENDIF 
        ELSEIF (C3.EQ.'HRD') THEN 
          IF (SNAME) THEN 
            NBMIN = 2 
          ELSE 
            NBMIN = 2 
          ENDIF 
        ELSEIF (C3.EQ.'BRD') THEN 
          IF (SNAME) THEN 
            NBMIN = 2 
          ELSE 
            NBMIN = 2 
          ENDIF 
        ELSEIF (C3.EQ.'TRI') THEN 
          IF (SNAME) THEN 
            NBMIN = 2 
          ELSE 
            NBMIN = 2 
          ENDIF 
        ENDIF 
      ELSEIF (C2.EQ.'SY') THEN 
        IF (C3.EQ.'TRF') THEN 
          IF (SNAME) THEN 
            NBMIN = 8 
          ELSE 
            NBMIN = 8 
          ENDIF 
        ELSEIF (SNAME.AND.C3.EQ.'TRD') THEN 
          NBMIN = 2 
        ENDIF 
      ELSEIF (CNAME.AND.C2.EQ.'HE') THEN 
        IF (C3.EQ.'TRD') THEN 
          NBMIN = 2 
        ENDIF 
      ELSEIF (SNAME.AND.C2.EQ.'OR') THEN 
        IF (C3 (1:1) .EQ.'G') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NBMIN = 2 
          ENDIF 
        ELSEIF (C3 (1:1) .EQ.'M') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NBMIN = 2 
          ENDIF 
        ENDIF 
      ELSEIF (CNAME.AND.C2.EQ.'UN') THEN 
        IF (C3 (1:1) .EQ.'G') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NBMIN = 2 
          ENDIF 
        ELSEIF (C3 (1:1) .EQ.'M') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NBMIN = 2 
          ENDIF 
        ENDIF 
      ENDIF 
      ILAENV = NBMIN 
      RETURN 
!                                                                       
   70 CONTINUE 
!                                                                       
!     ISPEC = 3:  crossover point                                       
!                                                                       
      NX = 0 
      IF (C2.EQ.'GE') THEN 
        IF (C3.EQ.'QRF'.OR.C3.EQ.'RQF'.OR.C3.EQ.'LQF'.OR.C3.EQ.'QLF')   &
        THEN                                                            
          IF (SNAME) THEN 
            NX = 128 
          ELSE 
            NX = 128 
          ENDIF 
        ELSEIF (C3.EQ.'HRD') THEN 
          IF (SNAME) THEN 
            NX = 128 
          ELSE 
            NX = 128 
          ENDIF 
        ELSEIF (C3.EQ.'BRD') THEN 
          IF (SNAME) THEN 
            NX = 128 
          ELSE 
            NX = 128 
          ENDIF 
        ENDIF 
      ELSEIF (C2.EQ.'SY') THEN 
        IF (SNAME.AND.C3.EQ.'TRD') THEN 
          NX = 32 
        ENDIF 
      ELSEIF (CNAME.AND.C2.EQ.'HE') THEN 
        IF (C3.EQ.'TRD') THEN 
          NX = 32 
        ENDIF 
      ELSEIF (SNAME.AND.C2.EQ.'OR') THEN 
        IF (C3 (1:1) .EQ.'G') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NX = 128 
          ENDIF 
        ENDIF 
      ELSEIF (CNAME.AND.C2.EQ.'UN') THEN 
        IF (C3 (1:1) .EQ.'G') THEN 
      IF (C4.EQ.'QR'.OR.C4.EQ.'RQ'.OR.C4.EQ.'LQ'.OR.C4.EQ.'QL'.OR.C4.EQ.&
     &'HR'.OR.C4.EQ.'TR'.OR.C4.EQ.'BR') THEN                            
            NX = 128 
          ENDIF 
        ENDIF 
      ENDIF 
      ILAENV = NX 
      RETURN 
!                                                                       
   80 CONTINUE 
!                                                                       
!     ISPEC = 4:  number of shifts (used by xHSEQR)                     
!                                                                       
      ILAENV = 6 
      RETURN 
!                                                                       
   90 CONTINUE 
!                                                                       
!     ISPEC = 5:  minimum column dimension (not used)                   
!                                                                       
      ILAENV = 2 
      RETURN 
!                                                                       
  100 CONTINUE 
!                                                                       
!     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)   
!                                                                       
      ILAENV = INT (REAL (MIN (N1, N2) ) * 1.6E0) 
      RETURN 
!                                                                       
  110 CONTINUE 
!                                                                       
!     ISPEC = 7:  number of processors (not used)                       
!                                                                       
      ILAENV = 1 
      RETURN 
!                                                                       
  120 CONTINUE 
!                                                                       
!     ISPEC = 8:  crossover point for multishift (used by xHSEQR)       
!                                                                       
      ILAENV = 50 
      RETURN 
!                                                                       
  130 CONTINUE 
!                                                                       
!     ISPEC = 9:  maximum size of the subproblems at the bottom of the  
!                 computation tree in the divide-and-conquer algorithm  
!                 (used by xGELSD and xGESDD)                           
!                                                                       
      ILAENV = 25 
      RETURN 
!                                                                       
  140 CONTINUE 
!                                                                       
!     ISPEC = 10: ieee NaN arithmetic can be trusted not to trap        
!                                                                       
!     ILAENV = 0                                                        
      ILAENV = 1 
      IF (ILAENV.EQ.1) THEN 
        ILAENV = IEEECK (1, 0.0, 1.0) 
      ENDIF 
      RETURN 
!                                                                       
  150 CONTINUE 
!                                                                       
!     ISPEC = 11: infinity arithmetic can be trusted not to trap        
!                                                                       
!     ILAENV = 0                                                        
      ILAENV = 1 
      IF (ILAENV.EQ.1) THEN 
        ILAENV = IEEECK (0, 0.0, 1.0) 
      ENDIF 
      RETURN 
!                                                                       
  160 CONTINUE 
!                                                                       
!     12 <= ISPEC <= 16: xHSEQR or one of its subroutines.              
!                                                                       
      ILAENV = IPARMQ (ISPEC, NAME, OPTS, N1, N2, N3, N4) 
      RETURN 
!                                                                       
!     End of ILAENV                                                     
!                                                                       
!> \brief \b IPARMQ                                                     
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download IPARMQ + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       INTEGER FUNCTION IPARMQ( ISPEC, NAME, OPTS, N, ILO, IHI, LWORK )
!                                                                       
!       .. Scalar Arguments ..                                          
!       INTEGER            IHI, ILO, ISPEC, LWORK, N                    
!       CHARACTER          NAME*( * ), OPTS*( * )                       
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!>      This program sets problem and machine dependent parameters      
!>      useful for xHSEQR and its subroutines. It is called whenever    
!>      ILAENV is called with 12 <= ISPEC <= 16                         
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] ISPEC                                                     
!> \verbatim                                                            
!>          ISPEC is integer scalar                                     
!>              ISPEC specifies which tunable parameter IPARMQ should   
!>              return.                                                 
!>                                                                      
!>              ISPEC=12: (INMIN)  Matrices of order nmin or less       
!>                        are sent directly to xLAHQR, the implicit     
!>                        double shift QR algorithm.  NMIN must be      
!>                        at least 11.                                  
!>                                                                      
!>              ISPEC=13: (INWIN)  Size of the deflation window.        
!>                        This is best set greater than or equal to     
!>                        the number of simultaneous shifts NS.         
!>                        Larger matrices benefit from larger deflation 
!>                        windows.                                      
!>                                                                      
!>              ISPEC=14: (INIBL) Determines when to stop nibbling and  
!>                        invest in an (expensive) multi-shift QR sweep.
!>                        If the aggressive early deflation subroutine  
!>                        finds LD converged eigenvalues from an order  
!>                        NW deflation window and LD.GT.(NW*NIBBLE)/100,
!>                        then the next QR sweep is skipped and early   
!>                        deflation is applied immediately to the       
!>                        remaining active diagonal block.  Setting     
!>                        IPARMQ(ISPEC=14) = 0 causes TTQRE to skip a   
!>                        multi-shift QR sweep whenever early deflation 
!>                        finds a converged eigenvalue.  Setting        
!>                        IPARMQ(ISPEC=14) greater than or equal to 100 
!>                        prevents TTQRE from skipping a multi-shift    
!>                        QR sweep.                                     
!>                                                                      
!>              ISPEC=15: (NSHFTS) The number of simultaneous shifts in 
!>                        a multi-shift QR iteration.                   
!>                                                                      
!>              ISPEC=16: (IACC22) IPARMQ is set to 0, 1 or 2 with the  
!>                        following meanings.                           
!>                        0:  During the multi-shift QR sweep,          
!>                            xLAQR5 does not accumulate reflections and
!>                            does not use matrix-matrix multiply to    
!>                            update the far-from-diagonal matrix       
!>                            entries.                                  
!>                        1:  During the multi-shift QR sweep,          
!>                            xLAQR5 and/or xLAQRaccumulates reflections
!>                            matrix-matrix multiply to update the      
!>                            far-from-diagonal matrix entries.         
!>                        2:  During the multi-shift QR sweep.          
!>                            xLAQR5 accumulates reflections and takes  
!>                            advantage of 2-by-2 block structure during
!>                            matrix-matrix multiplies.                 
!>                        (If xTRMM is slower than xGEMM, then          
!>                        IPARMQ(ISPEC=16)=1 may be more efficient than 
!>                        IPARMQ(ISPEC=16)=2 despite the greater level o
!>                        arithmetic work implied by the latter choice.)
!> \endverbatim                                                         
!>                                                                      
!> \param[in] NAME                                                      
!> \verbatim                                                            
!>          NAME is character string                                    
!>               Name of the calling subroutine                         
!> \endverbatim                                                         
!>                                                                      
!> \param[in] OPTS                                                      
!> \verbatim                                                            
!>          OPTS is character string                                    
!>               This is a concatenation of the string arguments to     
!>               TTQRE.                                                 
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N                                                         
!> \verbatim                                                            
!>          N is integer scalar                                         
!>               N is the order of the Hessenberg matrix H.             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] ILO                                                       
!> \verbatim                                                            
!>          ILO is INTEGER                                              
!> \endverbatim                                                         
!>                                                                      
!> \param[in] IHI                                                       
!> \verbatim                                                            
!>          IHI is INTEGER                                              
!>               It is assumed that H is already upper triangular       
!>               in rows and columns 1:ILO-1 and IHI+1:N.               
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LWORK                                                     
!> \verbatim                                                            
!>          LWORK is integer scalar                                     
!>               The amount of workspace available.                     
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup auxOTHERauxiliary                                           
!                                                                       
!> \par Further Details:                                                
!  =====================                                                
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!>       Little is known about how best to choose these parameters.     
!>       It is possible to use different values of the parameters       
!>       for each of CHSEQR, DHSEQR, SHSEQR and ZHSEQR.                 
!>                                                                      
!>       It is probably best to choose different parameters for         
!>       different matrices and different parameters at different       
!>       times during the iteration, but this has not been              
!>       implemented --- yet.                                           
!>                                                                      
!>                                                                      
!>       The best choices of most of the parameters depend              
!>       in an ill-understood way on the relative execution             
!>       rate of xLAQR3 and xLAQR5 and on the nature of each            
!>       particular eigenvalue problem.  Experiment may be the          
!>       only practical way to determine which choices are most         
!>       effective.                                                     
!>                                                                      
!>       Following is a list of default values supplied by IPARMQ.      
!>       These defaults may be adjusted in order to attain better       
!>       performance in any particular computational environment.       
!>                                                                      
!>       IPARMQ(ISPEC=12) The xLAHQR vs xLAQR0 crossover point.         
!>                        Default: 75. (Must be at least 11.)           
!>                                                                      
!>       IPARMQ(ISPEC=13) Recommended deflation window size.            
!>                        This depends on ILO, IHI and NS, the          
!>                        number of simultaneous shifts returned        
!>                        by IPARMQ(ISPEC=15).  The default for         
!>                        (IHI-ILO+1).LE.500 is NS.  The default        
!>                        for (IHI-ILO+1).GT.500 is 3*NS/2.             
!>                                                                      
!>       IPARMQ(ISPEC=14) Nibble crossover point.  Default: 14.         
!>                                                                      
!>       IPARMQ(ISPEC=15) Number of simultaneous shifts, NS.            
!>                        a multi-shift QR iteration.                   
!>                                                                      
!>                        If IHI-ILO+1 is ...                           
!>                                                                      
!>                        greater than      ...but less    ... the      
!>                        or equal to ...      than        default is   
!>                                                                      
!>                                0               30       NS =   2+    
!>                               30               60       NS =   4+    
!>                               60              150       NS =  10     
!>                              150              590       NS =  **     
!>                              590             3000       NS =  64     
!>                             3000             6000       NS = 128     
!>                             6000             infinity   NS = 256     
!>                                                                      
!>                    (+)  By default matrices of this order are        
!>                         passed to the implicit double shift routine  
      END FUNCTION ILAENV                           
!>                         xLAHQR.  See IPARMQ(ISPEC=12) above.   These 
!>                         values of NS are used only in case of a rare 
!>                         xLAHQR failure.                              
!>                                                                      
!>                    (**) The asterisks (**) indicate an ad-hoc        
!>                         function increasing from 10 to 64.           
!>                                                                      
!>       IPARMQ(ISPEC=16) Select structured matrix multiply.            
!>                        (See ISPEC=16 above for details.)             
!>                        Default: 3.                                   
!> \endverbatim                                                         
!>                                                                      
!  =====================================================================
      INTEGER FUNCTION IPARMQ (ISPEC, NAME, OPTS, N, ILO, IHI, LWORK) 
!                                                                       
!  -- LAPACK auxiliary routine (version 3.4.0) --                       
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      INTEGER IHI, ILO, ISPEC, LWORK, N 
      CHARACTER NAME * ( * ), OPTS * ( * ) 
!                                                                       
!  ================================================================     
!     .. Parameters ..                                                  
      INTEGER INMIN, INWIN, INIBL, ISHFTS, IACC22 
      PARAMETER (INMIN = 12, INWIN = 13, INIBL = 14, ISHFTS = 15,       &
      IACC22 = 16)                                                      
      INTEGER NMIN, K22MIN, KACMIN, NIBBLE, KNWSWP 
      PARAMETER (NMIN = 75, K22MIN = 14, KACMIN = 14, NIBBLE = 14,      &
      KNWSWP = 500)                                                     
      REAL TWO 
      PARAMETER (TWO = 2.0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      INTEGER NH, NS 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC LOG, MAX, MOD, NINT, REAL 
!     ..                                                                
!     .. Executable Statements ..                                       
      IF ( (ISPEC.EQ.ISHFTS) .OR. (ISPEC.EQ.INWIN) .OR. (               &
      ISPEC.EQ.IACC22) ) THEN                                           
!                                                                       
!        ==== Set the number simultaneous shifts ====                   
!                                                                       
        NH = IHI - ILO + 1 
        NS = 2 
        IF (NH.GE.30) NS = 4 
        IF (NH.GE.60) NS = 10 
        IF (NH.GE.150) NS = MAX (10, NH / NINT (LOG (REAL (NH) )        &
        / LOG (TWO) ) )                                                 
        IF (NH.GE.590) NS = 64 
        IF (NH.GE.3000) NS = 128 
        IF (NH.GE.6000) NS = 256 
        NS = MAX (2, NS - MOD (NS, 2) ) 
      ENDIF 
!                                                                       
      IF (ISPEC.EQ.INMIN) THEN 
!                                                                       
!                                                                       
!        ===== Matrices of order smaller than NMIN get sent             
!        .     to xLAHQR, the classic double shift algorithm.           
!        .     This must be at least 11. ====                           
!                                                                       
        IPARMQ = NMIN 
!                                                                       
      ELSEIF (ISPEC.EQ.INIBL) THEN 
!                                                                       
!        ==== INIBL: skip a multi-shift qr iteration and                
!        .    whenever aggressive early deflation finds                 
!        .    at least (NIBBLE*(window size)/100) deflations. ====      
!                                                                       
        IPARMQ = NIBBLE 
!                                                                       
      ELSEIF (ISPEC.EQ.ISHFTS) THEN 
!                                                                       
!        ==== NSHFTS: The number of simultaneous shifts =====           
!                                                                       
        IPARMQ = NS 
!                                                                       
      ELSEIF (ISPEC.EQ.INWIN) THEN 
!                                                                       
!        ==== NW: deflation window size.  ====                          
!                                                                       
        IF (NH.LE.KNWSWP) THEN 
          IPARMQ = NS 
        ELSE 
          IPARMQ = 3 * NS / 2 
        ENDIF 
!                                                                       
      ELSEIF (ISPEC.EQ.IACC22) THEN 
!                                                                       
!        ==== IACC22: Whether to accumulate reflections                 
!        .     before updating the far-from-diagonal elements           
!        .     and whether to use 2-by-2 block structure while          
!        .     doing it.  A small amount of work could be saved         
!        .     by making this choice dependent also upon the            
!        .     NH=IHI-ILO+1.                                            
!                                                                       
        IPARMQ = 0 
        IF (NS.GE.KACMIN) IPARMQ = 1 
        IF (NS.GE.K22MIN) IPARMQ = 2 
!                                                                       
      ELSE 
!        ===== invalid value of ispec =====                             
        IPARMQ = - 1 
!                                                                       
      ENDIF 
!                                                                       
!     ==== End of IPARMQ ====                                           
!                                                                       
      END FUNCTION IPARMQ                           
!> \brief \b LSAME                                                      
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!      LOGICAL FUNCTION LSAME( CA, CB )                                 
!                                                                       
!     .. Scalar Arguments ..                                            
!      CHARACTER          CA, CB                                        
!     ..                                                                
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> LSAME returns .TRUE. if CA is the same letter as CB regardless of    
!> case.                                                                
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] CA                                                        
!> \verbatim                                                            
!> \endverbatim                                                         
!>                                                                      
!> \param[in] CB                                                        
!> \verbatim                                                            
!>          CA and CB specify the single characters to be compared.     
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup auxOTHERauxiliary                                           
!                                                                       
!  =====================================================================
      LOGICAL FUNCTION LSAME (CA, CB) 
!                                                                       
!  -- LAPACK auxiliary routine (version 3.4.0) --                       
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER CA, CB 
!     ..                                                                
!                                                                       
! ===================================================================== 
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC ICHAR 
!     ..                                                                
!     .. Local Scalars ..                                               
      INTEGER INTA, INTB, ZCODE 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
!     Test if the characters are equal                                  
!                                                                       
      LSAME = CA.EQ.CB 
      IF (LSAME) RETURN 
!                                                                       
!     Now test for equivalence if both characters are alphabetic.       
!                                                                       
      ZCODE = ICHAR ('Z') 
!                                                                       
!     Use 'Z' rather than 'A' so that ASCII can be detected on Prime    
!     machines, on which ICHAR returns a value with bit 8 set.          
!     ICHAR('A') on Prime machines returns 193 which is the same as     
!     ICHAR('A') on an EBCDIC machine.                                  
!                                                                       
      INTA = ICHAR (CA) 
      INTB = ICHAR (CB) 
!                                                                       
      IF (ZCODE.EQ.90.OR.ZCODE.EQ.122) THEN 
!                                                                       
!        ASCII is assumed - ZCODE is the ASCII code of either lower or  
!        upper case 'Z'.                                                
!                                                                       
        IF (INTA.GE.97.AND.INTA.LE.122) INTA = INTA - 32 
        IF (INTB.GE.97.AND.INTB.LE.122) INTB = INTB - 32 
!                                                                       
      ELSEIF (ZCODE.EQ.233.OR.ZCODE.EQ.169) THEN 
!                                                                       
!        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
!        upper case 'Z'.                                                
!                                                                       
      IF (INTA.GE.129.AND.INTA.LE.137.OR.INTA.GE.145.AND.INTA.LE.153.OR.&
     &INTA.GE.162.AND.INTA.LE.169) INTA = INTA + 64                     
      IF (INTB.GE.129.AND.INTB.LE.137.OR.INTB.GE.145.AND.INTB.LE.153.OR.&
     &INTB.GE.162.AND.INTB.LE.169) INTB = INTB + 64                     
!                                                                       
      ELSEIF (ZCODE.EQ.218.OR.ZCODE.EQ.250) THEN 
!                                                                       
!        ASCII is assumed, on Prime machines - ZCODE is the ASCII code  
!        plus 128 of either lower or upper case 'Z'.                    
!                                                                       
        IF (INTA.GE.225.AND.INTA.LE.250) INTA = INTA - 32 
        IF (INTB.GE.225.AND.INTB.LE.250) INTB = INTB - 32 
      ENDIF 
      LSAME = INTA.EQ.INTB 
!                                                                       
!     RETURN                                                            
!                                                                       
!     End of LSAME                                                      
!                                                                       
      END FUNCTION LSAME                            
!> \brief \b SISNAN tests input for NaN.                                
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SISNAN + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       LOGICAL FUNCTION SISNAN( SIN )                                  
!                                                                       
!       .. Scalar Arguments ..                                          
!       REAL               SIN                                          
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> SISNAN returns .TRUE. if its argument is NaN, and .FALSE.            
!> otherwise.  To be replaced by the Fortran 2003 intrinsic in the      
!> future.                                                              
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] SIN                                                       
!> \verbatim                                                            
!>          SIN is REAL                                                 
!>          Input to test for NaN.                                      
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date September 2012                                                 
!                                                                       
!> \ingroup auxOTHERauxiliary                                           
!                                                                       
!  =====================================================================
      LOGICAL FUNCTION SISNAN (SIN) 
!                                                                       
!  -- LAPACK auxiliary routine (version 3.4.2) --                       
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     September 2012                                                    
!                                                                       
!     .. Scalar Arguments ..                                            
      REAL SIN 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!  ..                                                                   
!  .. Executable Statements ..                                          
      SISNAN = SLAISNAN (SIN, SIN) 
      RETURN 
      END FUNCTION SISNAN                           
!> \brief \b SLAISNAN tests input for NaN by comparing two arguments for
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SLAISNAN + dependencies                                     
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       LOGICAL FUNCTION SLAISNAN( SIN1, SIN2 )                         
!                                                                       
!       .. Scalar Arguments ..                                          
!       REAL               SIN1, SIN2                                   
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> This routine is not for general use.  It exists solely to avoid      
!> over-optimization in SISNAN.                                         
!>                                                                      
!> SLAISNAN checks for NaNs by comparing its two arguments for          
!> inequality.  NaN is the only floating-point value where NaN != NaN   
!> returns .TRUE.  To check for NaNs, pass the same variable as both    
!> arguments.                                                           
!>                                                                      
!> A compiler must assume that the two arguments are                    
!> not the same variable, and the test will not be optimized away.      
!> Interprocedural or whole-program optimization may delete this        
!> test.  The ISNAN functions will be replaced by the correct           
!> Fortran 03 intrinsic once the intrinsic is widely available.         
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] SIN1                                                      
!> \verbatim                                                            
!>          SIN1 is REAL                                                
!> \endverbatim                                                         
!>                                                                      
!> \param[in] SIN2                                                      
!> \verbatim                                                            
!>          SIN2 is REAL                                                
!>          Two numbers to compare for inequality.                      
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date September 2012                                                 
!                                                                       
!> \ingroup auxOTHERauxiliary                                           
!                                                                       
!  =====================================================================
      LOGICAL FUNCTION SLAISNAN (SIN1, SIN2) 
!                                                                       
!  -- LAPACK auxiliary routine (version 3.4.2) --                       
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     September 2012                                                    
!                                                                       
!     .. Scalar Arguments ..                                            
      REAL SIN1, SIN2 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!  .. Executable Statements ..                                          
      SLAISNAN = (SIN1.NE.SIN2) 
      RETURN 
      END FUNCTION SLAISNAN                         
!> \brief \b SLASYF computes a partial factorization of a real symmetric
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SLASYF + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       SUBROUTINE SLASYF( UPLO, N, NB, KB, A, LDA, IPIV, W, LDW, INFO )
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER          UPLO                                         
!       INTEGER            INFO, KB, LDA, LDW, N, NB                    
!       ..                                                              
!       .. Array Arguments ..                                           
!       INTEGER            IPIV( * )                                    
!       REAL               A( LDA, * ), W( LDW, * )                     
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> SLASYF computes a partial factorization of a real symmetric matrix A 
!> using the Bunch-Kaufman diagonal pivoting method. The partial        
!> factorization has the form:                                          
!>                                                                      
!> A  =  ( I  U12 ) ( A11  0  ) (  I       0    )  if UPLO = 'U', or:   
!>       ( 0  U22 ) (  0   D  ) ( U12**T U22**T )                       
!>                                                                      
!> A  =  ( L11  0 ) (  D   0  ) ( L11**T L21**T )  if UPLO = 'L'        
!>       ( L21  I ) (  0  A22 ) (  0       I    )                       
!>                                                                      
!> where the order of D is at most NB. The actual order is returned in  
!> the argument KB, and is either NB or NB-1, or N if N <= NB.          
!>                                                                      
!> SLASYF is an auxiliary routine called by SSYTRF. It uses blocked code
!> (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
!> A22 (if UPLO = 'L').                                                 
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] UPLO                                                      
!> \verbatim                                                            
!>          UPLO is CHARACTER*1                                         
!>          Specifies whether the upper or lower triangular part of the 
!>          symmetric matrix A is stored:                               
!>          = 'U':  Upper triangular                                    
!>          = 'L':  Lower triangular                                    
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N                                                         
!> \verbatim                                                            
!>          N is INTEGER                                                
!>          The order of the matrix A.  N >= 0.                         
!> \endverbatim                                                         
!>                                                                      
!> \param[in] NB                                                        
!> \verbatim                                                            
!>          NB is INTEGER                                               
!>          The maximum number of columns of the matrix A that should be
!>          factored.  NB should be at least 2 to allow for 2-by-2 pivot
!>          blocks.                                                     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] KB                                                       
!> \verbatim                                                            
!>          KB is INTEGER                                               
!>          The number of columns of A that were actually factored.     
!>          KB is either NB-1 or NB, or N if N <= NB.                   
!> \endverbatim                                                         
!>                                                                      
!> \param[in,out] A                                                     
!> \verbatim                                                            
!>          A is REAL array, dimension (LDA,N)                          
!>          On entry, the symmetric matrix A.  If UPLO = 'U', the leadin
!>          n-by-n upper triangular part of A contains the upper        
!>          triangular part of the matrix A, and the strictly lower     
!>          triangular part of A is not referenced.  If UPLO = 'L', the 
!>          leading n-by-n lower triangular part of A contains the lower
!>          triangular part of the matrix A, and the strictly upper     
!>          triangular part of A is not referenced.                     
!>          On exit, A contains details of the partial factorization.   
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDA                                                       
!> \verbatim                                                            
!>          LDA is INTEGER                                              
!>          The leading dimension of the array A.  LDA >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] IPIV                                                     
!> \verbatim                                                            
!>          IPIV is INTEGER array, dimension (N)                        
!>          Details of the interchanges and the block structure of D.   
!>          If UPLO = 'U', only the last KB elements of IPIV are set;   
!>          if UPLO = 'L', only the first KB elements are set.          
!>                                                                      
!>          If IPIV(k) > 0, then rows and columns k and IPIV(k) were    
!>          interchanged and D(k,k) is a 1-by-1 diagonal block.         
!>          If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and    
!>          columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k
!>          is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =    
!>          IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were  
!>          interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block. 
!> \endverbatim                                                         
!>                                                                      
!> \param[out] W                                                        
!> \verbatim                                                            
!>          W is REAL array, dimension (LDW,NB)                         
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDW                                                       
!> \verbatim                                                            
!>          LDW is INTEGER                                              
!>          The leading dimension of the array W.  LDW >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] INFO                                                     
!> \verbatim                                                            
!>          INFO is INTEGER                                             
!>          = 0: successful exit                                        
!>          > 0: if INFO = k, D(k,k) is exactly zero.  The factorization
!>               has been completed, but the block diagonal matrix D is 
!>               exactly singular.                                      
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date September 2012                                                 
!                                                                       
!> \ingroup realSYcomputational                                         
!                                                                       
!  =====================================================================
      SUBROUTINE SLASYF (UPLO, N, NB, KB, A, LDA, IPIV, W, LDW, INFO) 
!                                                                       
!  -- LAPACK computational routine (version 3.4.2) --                   
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     September 2012                                                    
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER UPLO 
      INTEGER INFO, KB, LDA, LDW, N, NB 
!     ..                                                                
!     .. Array Arguments ..                                             
      INTEGER IPIV ( * ) 
      REAL A (LDA, * ), W (LDW, * ) 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO, ONE 
      PARAMETER (ZERO = 0.0E+0, ONE = 1.0E+0) 
      REAL EIGHT, SEVTEN 
      PARAMETER (EIGHT = 8.0E+0, SEVTEN = 17.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      INTEGER IMAX, J, JB, JJ, JMAX, JP, K, KK, KKW, KP, KSTEP, KW 
      REAL ABSAKK, ALPHA, COLMAX, D11, D21, D22, R1, ROWMAX, T 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, MAX, MIN, SQRT 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
      INFO = 0 
!                                                                       
!     Initialize ALPHA for use in choosing pivot block size.            
!                                                                       
      ALPHA = (ONE+SQRT (SEVTEN) ) / EIGHT 
!                                                                       
      IF (LSAME (UPLO, 'U') ) THEN 
!                                                                       
!        Factorize the trailing columns of A using the upper triangle   
!        of A and working backwards, and compute the matrix W = U12*D   
!        for use in updating A11                                        
!                                                                       
!        K is the main loop index, decreasing from N in steps of 1 or 2 
!                                                                       
!        KW is the column of W which corresponds to column K of A       
!                                                                       
        K = N 
   10   CONTINUE 
        KW = NB + K - N 
!                                                                       
!        Exit from loop                                                 
!                                                                       
        IF ( (K.LE.N - NB + 1.AND.NB.LT.N) .OR.K.LT.1) GOTO 30 
!                                                                       
!        Copy column K of A to column KW of W and update it             
!                                                                       
        CALL SCOPY (K, A (1, K), 1, W (1, KW), 1) 
        IF (K.LT.N) CALL SGEMV ('No transpose', K, N - K, - ONE, A (1,  &
        K + 1) , LDA, W (K, KW + 1) , LDW, ONE, W (1, KW) , 1)          
!                                                                       
        KSTEP = 1 
!                                                                       
!        Determine rows and columns to be interchanged and whether      
!        a 1-by-1 or 2-by-2 pivot block will be used                    
!                                                                       
        ABSAKK = ABS (W (K, KW) ) 
!                                                                       
!        IMAX is the row-index of the largest off-diagonal element in   
!        column K, and COLMAX is its absolute value                     
!                                                                       
        IF (K.GT.1) THEN 
          IMAX = ISAMAX (K - 1, W (1, KW), 1) 
          COLMAX = ABS (W (IMAX, KW) ) 
        ELSE 
          COLMAX = ZERO 
        ENDIF 
!                                                                       
        IF (MAX (ABSAKK, COLMAX) .EQ.ZERO) THEN 
!                                                                       
!           Column K is zero: set INFO and continue                     
!                                                                       
          IF (INFO.EQ.0) INFO = K 
          KP = K 
        ELSE 
          IF (ABSAKK.GE.ALPHA * COLMAX) THEN 
!                                                                       
!              no interchange, use 1-by-1 pivot block                   
!                                                                       
            KP = K 
          ELSE 
!                                                                       
!              Copy column IMAX to column KW-1 of W and update it       
!                                                                       
            CALL SCOPY (IMAX, A (1, IMAX), 1, W (1, KW - 1), 1) 
            CALL SCOPY (K - IMAX, A (IMAX, IMAX + 1), LDA, W (IMAX + 1, &
            KW - 1), 1)                                                 
            IF (K.LT.N) CALL SGEMV ('No transpose', K, N - K, - ONE, A (&
            1, K + 1) , LDA, W (IMAX, KW + 1) , LDW, ONE, W (1, KW - 1) &
            , 1)                                                        
!                                                                       
!              JMAX is the column-index of the largest off-diagonal     
!              element in row IMAX, and ROWMAX is its absolute value    
!                                                                       
            JMAX = IMAX + ISAMAX (K - IMAX, W (IMAX + 1, KW - 1),       &
            1)                                                          
            ROWMAX = ABS (W (JMAX, KW - 1) ) 
            IF (IMAX.GT.1) THEN 
              JMAX = ISAMAX (IMAX - 1, W (1, KW - 1), 1) 
              ROWMAX = MAX (ROWMAX, ABS (W (JMAX, KW - 1) ) ) 
            ENDIF 
!                                                                       
            IF (ABSAKK.GE.ALPHA * COLMAX * (COLMAX / ROWMAX) ) THEN 
!                                                                       
!                 no interchange, use 1-by-1 pivot block                
!                                                                       
              KP = K 
            ELSEIF (ABS (W (IMAX, KW - 1) ) .GE.ALPHA * ROWMAX) THEN 
!                                                                       
!                 interchange rows and columns K and IMAX, use 1-by-1   
!                 pivot block                                           
!                                                                       
              KP = IMAX 
!                                                                       
!                 copy column KW-1 of W to column KW                    
!                                                                       
              CALL SCOPY (K, W (1, KW - 1), 1, W (1, KW), 1) 
            ELSE 
!                                                                       
!                 interchange rows and columns K-1 and IMAX, use 2-by-2 
!                 pivot block                                           
!                                                                       
              KP = IMAX 
              KSTEP = 2 
            ENDIF 
          ENDIF 
!                                                                       
          KK = K - KSTEP + 1 
          KKW = NB + KK - N 
!                                                                       
!           Updated column KP is already stored in column KKW of W      
!                                                                       
          IF (KP.NE.KK) THEN 
!                                                                       
!              Copy non-updated column KK to column KP                  
!                                                                       
            A (KP, K) = A (KK, K) 
            CALL SCOPY (K - 1 - KP, A (KP + 1, KK), 1, A (KP, KP + 1),  &
            LDA)                                                        
            CALL SCOPY (KP, A (1, KK), 1, A (1, KP), 1) 
!                                                                       
!              Interchange rows KK and KP in last KK columns of A and W 
!                                                                       
            CALL SSWAP (N - KK + 1, A (KK, KK), LDA, A (KP, KK),        &
            LDA)                                                        
            CALL SSWAP (N - KK + 1, W (KK, KKW), LDW, W (KP, KKW),      &
            LDW)                                                        
          ENDIF 
!                                                                       
          IF (KSTEP.EQ.1) THEN 
!                                                                       
!              1-by-1 pivot block D(k): column KW of W now holds        
!                                                                       
!              W(k) = U(k)*D(k)                                         
!                                                                       
!              where U(k) is the k-th column of U                       
!                                                                       
!              Store U(k) in column k of A                              
!                                                                       
            CALL SCOPY (K, W (1, KW), 1, A (1, K), 1) 
            R1 = ONE / A (K, K) 
            CALL SSCAL (K - 1, R1, A (1, K), 1) 
          ELSE 
!                                                                       
!              2-by-2 pivot block D(k): columns KW and KW-1 of W now    
!              hold                                                     
!                                                                       
!              ( W(k-1) W(k) ) = ( U(k-1) U(k) )*D(k)                   
!                                                                       
!              where U(k) and U(k-1) are the k-th and (k-1)-th columns  
!              of U                                                     
!                                                                       
            IF (K.GT.2) THEN 
!                                                                       
!                 Store U(k) and U(k-1) in columns k and k-1 of A       
!                                                                       
              D21 = W (K - 1, KW) 
              D11 = W (K, KW) / D21 
              D22 = W (K - 1, KW - 1) / D21 
              T = ONE / (D11 * D22 - ONE) 
              D21 = T / D21 
              DO 20 J = 1, K - 2 
                A (J, K - 1) = D21 * (D11 * W (J, KW - 1) - W (J, KW) ) 
                A (J, K) = D21 * (D22 * W (J, KW) - W (J, KW - 1) ) 
   20         END DO 
            ENDIF 
!                                                                       
!              Copy D(k) to A                                           
!                                                                       
            A (K - 1, K - 1) = W (K - 1, KW - 1) 
            A (K - 1, K) = W (K - 1, KW) 
            A (K, K) = W (K, KW) 
          ENDIF 
        ENDIF 
!                                                                       
!        Store details of the interchanges in IPIV                      
!                                                                       
        IF (KSTEP.EQ.1) THEN 
          IPIV (K) = KP 
        ELSE 
          IPIV (K) = - KP 
          IPIV (K - 1) = - KP 
        ENDIF 
!                                                                       
!        Decrease K and return to the start of the main loop            
!                                                                       
        K = K - KSTEP 
        GOTO 10 
!                                                                       
   30   CONTINUE 
!                                                                       
!        Update the upper triangle of A11 (= A(1:k,1:k)) as             
!                                                                       
!        A11 := A11 - U12*D*U12**T = A11 - U12*W**T                     
!                                                                       
!        computing blocks of NB columns at a time                       
!                                                                       
        DO 50 J = ( (K - 1) / NB) * NB + 1, 1, - NB 
          JB = MIN (NB, K - J + 1) 
!                                                                       
!           Update the upper triangle of the diagonal block             
!                                                                       
          DO 40 JJ = J, J + JB - 1 
            CALL SGEMV ('No transpose', JJ - J + 1, N - K, - ONE, A (J, &
            K + 1) , LDA, W (JJ, KW + 1) , LDW, ONE, A (J, JJ) , 1)     
   40     END DO 
!                                                                       
!           Update the rectangular superdiagonal block                  
!                                                                       
          CALL SGEMM ('No transpose', 'Transpose', J - 1, JB, N - K,    &
          - ONE, A (1, K + 1) , LDA, W (J, KW + 1) , LDW, ONE, A (1, J) &
          , LDA)                                                        
   50   END DO 
!                                                                       
!        Put U12 in standard form by partially undoing the interchanges 
!        in columns k+1:n                                               
!                                                                       
        J = K + 1 
   60   CONTINUE 
        JJ = J 
        JP = IPIV (J) 
        IF (JP.LT.0) THEN 
          JP = - JP 
          J = J + 1 
        ENDIF 
        J = J + 1 
        IF (JP.NE.JJ.AND.J.LE.N) CALL SSWAP (N - J + 1, A (JP, J),      &
        LDA, A (JJ, J), LDA)                                            
        IF (J.LE.N) GOTO 60 
!                                                                       
!        Set KB to the number of columns factorized                     
!                                                                       
        KB = N - K 
!                                                                       
      ELSE 
!                                                                       
!        Factorize the leading columns of A using the lower triangle    
!        of A and working forwards, and compute the matrix W = L21*D    
!        for use in updating A22                                        
!                                                                       
!        K is the main loop index, increasing from 1 in steps of 1 or 2 
!                                                                       
        K = 1 
   70   CONTINUE 
!                                                                       
!        Exit from loop                                                 
!                                                                       
        IF ( (K.GE.NB.AND.NB.LT.N) .OR.K.GT.N) GOTO 90 
!                                                                       
!        Copy column K of A to column K of W and update it              
!                                                                       
        CALL SCOPY (N - K + 1, A (K, K), 1, W (K, K), 1) 
        CALL SGEMV ('No transpose', N - K + 1, K - 1, - ONE, A (K, 1) , &
        LDA, W (K, 1) , LDW, ONE, W (K, K) , 1)                         
!                                                                       
        KSTEP = 1 
!                                                                       
!        Determine rows and columns to be interchanged and whether      
!        a 1-by-1 or 2-by-2 pivot block will be used                    
!                                                                       
        ABSAKK = ABS (W (K, K) ) 
!                                                                       
!        IMAX is the row-index of the largest off-diagonal element in   
!        column K, and COLMAX is its absolute value                     
!                                                                       
        IF (K.LT.N) THEN 
          IMAX = K + ISAMAX (N - K, W (K + 1, K), 1) 
          COLMAX = ABS (W (IMAX, K) ) 
        ELSE 
          COLMAX = ZERO 
        ENDIF 
!                                                                       
        IF (MAX (ABSAKK, COLMAX) .EQ.ZERO) THEN 
!                                                                       
!           Column K is zero: set INFO and continue                     
!                                                                       
          IF (INFO.EQ.0) INFO = K 
          KP = K 
        ELSE 
          IF (ABSAKK.GE.ALPHA * COLMAX) THEN 
!                                                                       
!              no interchange, use 1-by-1 pivot block                   
!                                                                       
            KP = K 
          ELSE 
!                                                                       
!              Copy column IMAX to column K+1 of W and update it        
!                                                                       
            CALL SCOPY (IMAX - K, A (IMAX, K), LDA, W (K, K + 1),       &
            1)                                                          
            CALL SCOPY (N - IMAX + 1, A (IMAX, IMAX), 1, W (IMAX, K + 1)&
            , 1)                                                        
            CALL SGEMV ('No transpose', N - K + 1, K - 1, - ONE, A (K,  &
            1) , LDA, W (IMAX, 1) , LDW, ONE, W (K, K + 1) , 1)         
!                                                                       
!              JMAX is the column-index of the largest off-diagonal     
!              element in row IMAX, and ROWMAX is its absolute value    
!                                                                       
            JMAX = K - 1 + ISAMAX (IMAX - K, W (K, K + 1), 1) 
            ROWMAX = ABS (W (JMAX, K + 1) ) 
            IF (IMAX.LT.N) THEN 
              JMAX = IMAX + ISAMAX (N - IMAX, W (IMAX + 1, K + 1),      &
              1)                                                        
              ROWMAX = MAX (ROWMAX, ABS (W (JMAX, K + 1) ) ) 
            ENDIF 
!                                                                       
            IF (ABSAKK.GE.ALPHA * COLMAX * (COLMAX / ROWMAX) ) THEN 
!                                                                       
!                 no interchange, use 1-by-1 pivot block                
!                                                                       
              KP = K 
            ELSEIF (ABS (W (IMAX, K + 1) ) .GE.ALPHA * ROWMAX) THEN 
!                                                                       
!                 interchange rows and columns K and IMAX, use 1-by-1   
!                 pivot block                                           
!                                                                       
              KP = IMAX 
!                                                                       
!                 copy column K+1 of W to column K                      
!                                                                       
              CALL SCOPY (N - K + 1, W (K, K + 1), 1, W (K, K), 1) 
            ELSE 
!                                                                       
!                 interchange rows and columns K+1 and IMAX, use 2-by-2 
!                 pivot block                                           
!                                                                       
              KP = IMAX 
              KSTEP = 2 
            ENDIF 
          ENDIF 
!                                                                       
          KK = K + KSTEP - 1 
!                                                                       
!           Updated column KP is already stored in column KK of W       
!                                                                       
          IF (KP.NE.KK) THEN 
!                                                                       
!              Copy non-updated column KK to column KP                  
!                                                                       
            A (KP, K) = A (KK, K) 
            CALL SCOPY (KP - K - 1, A (K + 1, KK), 1, A (KP, K + 1),    &
            LDA)                                                        
            CALL SCOPY (N - KP + 1, A (KP, KK), 1, A (KP, KP), 1) 
!                                                                       
!              Interchange rows KK and KP in first KK columns of A and W
!                                                                       
            CALL SSWAP (KK, A (KK, 1), LDA, A (KP, 1), LDA) 
            CALL SSWAP (KK, W (KK, 1), LDW, W (KP, 1), LDW) 
          ENDIF 
!                                                                       
          IF (KSTEP.EQ.1) THEN 
!                                                                       
!              1-by-1 pivot block D(k): column k of W now holds         
!                                                                       
!              W(k) = L(k)*D(k)                                         
!                                                                       
!              where L(k) is the k-th column of L                       
!                                                                       
!              Store L(k) in column k of A                              
!                                                                       
            CALL SCOPY (N - K + 1, W (K, K), 1, A (K, K), 1) 
            IF (K.LT.N) THEN 
              R1 = ONE / A (K, K) 
              CALL SSCAL (N - K, R1, A (K + 1, K), 1) 
            ENDIF 
          ELSE 
!                                                                       
!              2-by-2 pivot block D(k): columns k and k+1 of W now hold 
!                                                                       
!              ( W(k) W(k+1) ) = ( L(k) L(k+1) )*D(k)                   
!                                                                       
!              where L(k) and L(k+1) are the k-th and (k+1)-th columns  
!              of L                                                     
!                                                                       
            IF (K.LT.N - 1) THEN 
!                                                                       
!                 Store L(k) and L(k+1) in columns k and k+1 of A       
!                                                                       
              D21 = W (K + 1, K) 
              D11 = W (K + 1, K + 1) / D21 
              D22 = W (K, K) / D21 
              T = ONE / (D11 * D22 - ONE) 
              D21 = T / D21 
              DO 80 J = K + 2, N 
                A (J, K) = D21 * (D11 * W (J, K) - W (J, K + 1) ) 
                A (J, K + 1) = D21 * (D22 * W (J, K + 1) - W (J, K) ) 
   80         END DO 
            ENDIF 
!                                                                       
!              Copy D(k) to A                                           
!                                                                       
            A (K, K) = W (K, K) 
            A (K + 1, K) = W (K + 1, K) 
            A (K + 1, K + 1) = W (K + 1, K + 1) 
          ENDIF 
        ENDIF 
!                                                                       
!        Store details of the interchanges in IPIV                      
!                                                                       
        IF (KSTEP.EQ.1) THEN 
          IPIV (K) = KP 
        ELSE 
          IPIV (K) = - KP 
          IPIV (K + 1) = - KP 
        ENDIF 
!                                                                       
!        Increase K and return to the start of the main loop            
!                                                                       
        K = K + KSTEP 
        GOTO 70 
!                                                                       
   90   CONTINUE 
!                                                                       
!        Update the lower triangle of A22 (= A(k:n,k:n)) as             
!                                                                       
!        A22 := A22 - L21*D*L21**T = A22 - L21*W**T                     
!                                                                       
!        computing blocks of NB columns at a time                       
!                                                                       
        DO 110 J = K, N, NB 
          JB = MIN (NB, N - J + 1) 
!                                                                       
!           Update the lower triangle of the diagonal block             
!                                                                       
          DO 100 JJ = J, J + JB - 1 
            CALL SGEMV ('No transpose', J + JB - JJ, K - 1, - ONE, A (  &
            JJ, 1) , LDA, W (JJ, 1) , LDW, ONE, A (JJ, JJ) , 1)         
  100     END DO 
!                                                                       
!           Update the rectangular subdiagonal block                    
!                                                                       
          IF (J + JB.LE.N) CALL SGEMM ('No transpose', 'Transpose', N - &
          J - JB + 1, JB, K - 1, - ONE, A (J + JB, 1) , LDA, W (J, 1) , &
          LDW, ONE, A (J + JB, J) , LDA)                                
  110   END DO 
!                                                                       
!        Put L21 in standard form by partially undoing the interchanges 
!        in columns 1:k-1                                               
!                                                                       
        J = K - 1 
  120   CONTINUE 
        JJ = J 
        JP = IPIV (J) 
        IF (JP.LT.0) THEN 
          JP = - JP 
          J = J - 1 
        ENDIF 
        J = J - 1 
        IF (JP.NE.JJ.AND.J.GE.1) CALL SSWAP (J, A (JP, 1), LDA, A (JJ,  &
        1), LDA)                                                        
        IF (J.GE.1) GOTO 120 
!                                                                       
!        Set KB to the number of columns factorized                     
!                                                                       
        KB = K - 1 
!                                                                       
      ENDIF 
      RETURN 
!                                                                       
!     End of SLASYF                                                     
!                                                                       
      END SUBROUTINE SLASYF                         
!> \brief \b SSYCONV                                                    
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SSYCONV + dependencies                                      
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       SUBROUTINE SSYCONV( UPLO, WAY, N, A, LDA, IPIV, WORK, INFO )    
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER          UPLO, WAY                                    
!       INTEGER            INFO, LDA, N                                 
!       ..                                                              
!       .. Array Arguments ..                                           
!       INTEGER            IPIV( * )                                    
!       REAL               A( LDA, * ), WORK( * )                       
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> SSYCONV convert A given by TRF into L and D and vice-versa.          
!> Get Non-diag elements of D (returned in workspace) and               
!> apply or reverse permutation done in TRF.                            
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] UPLO                                                      
!> \verbatim                                                            
!>          UPLO is CHARACTER*1                                         
!>          Specifies whether the details of the factorization are store
!>          as an upper or lower triangular matrix.                     
!>          = 'U':  Upper triangular, form is A = U*D*U**T;             
!>          = 'L':  Lower triangular, form is A = L*D*L**T.             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] WAY                                                       
!> \verbatim                                                            
!>          WAY is CHARACTER*1                                          
!>          = 'C': Convert                                              
!>          = 'R': Revert                                               
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N                                                         
!> \verbatim                                                            
!>          N is INTEGER                                                
!>          The order of the matrix A.  N >= 0.                         
!> \endverbatim                                                         
!>                                                                      
!> \param[in] A                                                         
!> \verbatim                                                            
!>          A is REAL array, dimension (LDA,N)                          
!>          The block diagonal matrix D and the multipliers used to     
!>          obtain the factor U or L as computed by SSYTRF.             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDA                                                       
!> \verbatim                                                            
!>          LDA is INTEGER                                              
!>          The leading dimension of the array A.  LDA >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[in] IPIV                                                      
!> \verbatim                                                            
!>          IPIV is INTEGER array, dimension (N)                        
!>          Details of the interchanges and the block structure of D    
!>          as determined by SSYTRF.                                    
!> \endverbatim                                                         
!>                                                                      
!> \param[out] WORK                                                     
!> \verbatim                                                            
!>          WORK is REAL array, dimension (N)                           
!> \endverbatim                                                         
!>                                                                      
!> \param[out] INFO                                                     
!> \verbatim                                                            
!>          INFO is INTEGER                                             
!>          = 0:  successful exit                                       
!>          < 0:  if INFO = -i, the i-th argument had an illegal value  
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup realSYcomputational                                         
!                                                                       
!  =====================================================================
      SUBROUTINE SSYCONV (UPLO, WAY, N, A, LDA, IPIV, WORK, INFO) 
!                                                                       
!  -- LAPACK computational routine (version 3.4.0) --                   
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER UPLO, WAY 
      INTEGER INFO, LDA, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      INTEGER IPIV ( * ) 
      REAL A (LDA, * ), WORK ( * ) 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      LOGICAL UPPER, CONVERT 
      INTEGER I, IP, J 
      REAL TEMP 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
      INFO = 0 
      UPPER = LSAME (UPLO, 'U') 
      CONVERT = LSAME (WAY, 'C') 
      IF (.NOT.UPPER.AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = - 1 
      ELSEIF (.NOT.CONVERT.AND..NOT.LSAME (WAY, 'R') ) THEN 
        INFO = - 2 
      ELSEIF (N.LT.0) THEN 
        INFO = - 3 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = - 5 
                                                                        
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYCONV', - INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible                                          
!                                                                       
      IF (N.EQ.0) RETURN 
!                                                                       
      IF (UPPER) THEN 
!                                                                       
!      A is UPPER                                                       
!                                                                       
!      Convert A (A is upper)                                           
!                                                                       
!        Convert VALUE                                                  
!                                                                       
        IF (CONVERT) THEN 
          I = N 
          WORK (1) = ZERO 
          DO WHILE (I.GT.1) 
          IF (IPIV (I) .LT.0) THEN 
            WORK (I) = A (I - 1, I) 
            A (I - 1, I) = ZERO 
            I = I - 1 
          ELSE 
            WORK (I) = ZERO 
          ENDIF 
          I = I - 1 
          ENDDO 
!                                                                       
!        Convert PERMUTATIONS                                           
!                                                                       
          I = N 
          DO WHILE (I.GE.1) 
          IF (IPIV (I) .GT.0) THEN 
            IP = IPIV (I) 
            IF (I.LT.N) THEN 
              DO 12 J = I + 1, N 
                TEMP = A (IP, J) 
                A (IP, J) = A (I, J) 
                A (I, J) = TEMP 
   12         END DO 
            ENDIF 
          ELSE 
            IP = - IPIV (I) 
            IF (I.LT.N) THEN 
              DO 13 J = I + 1, N 
                TEMP = A (IP, J) 
                A (IP, J) = A (I - 1, J) 
                A (I - 1, J) = TEMP 
   13         END DO 
            ENDIF 
            I = I - 1 
          ENDIF 
          I = I - 1 
          ENDDO 
                                                                        
        ELSE 
!                                                                       
!      Revert A (A is upper)                                            
!                                                                       
!                                                                       
!        Revert PERMUTATIONS                                            
!                                                                       
          I = 1 
          DO WHILE (I.LE.N) 
          IF (IPIV (I) .GT.0) THEN 
            IP = IPIV (I) 
            IF (I.LT.N) THEN 
              DO J = I + 1, N 
              TEMP = A (IP, J) 
              A (IP, J) = A (I, J) 
              A (I, J) = TEMP 
              ENDDO 
            ENDIF 
          ELSE 
            IP = - IPIV (I) 
            I = I + 1 
            IF (I.LT.N) THEN 
              DO J = I + 1, N 
              TEMP = A (IP, J) 
              A (IP, J) = A (I - 1, J) 
              A (I - 1, J) = TEMP 
              ENDDO 
            ENDIF 
          ENDIF 
          I = I + 1 
          ENDDO 
!                                                                       
!        Revert VALUE                                                   
!                                                                       
          I = N 
          DO WHILE (I.GT.1) 
          IF (IPIV (I) .LT.0) THEN 
            A (I - 1, I) = WORK (I) 
            I = I - 1 
          ENDIF 
          I = I - 1 
          ENDDO 
        ENDIF 
      ELSE 
!                                                                       
!      A is LOWER                                                       
!                                                                       
        IF (CONVERT) THEN 
!                                                                       
!      Convert A (A is lower)                                           
!                                                                       
!                                                                       
!        Convert VALUE                                                  
!                                                                       
          I = 1 
          WORK (N) = ZERO 
          DO WHILE (I.LE.N) 
          IF (I.LT.N.AND.IPIV (I) .LT.0) THEN 
            WORK (I) = A (I + 1, I) 
            A (I + 1, I) = ZERO 
            I = I + 1 
          ELSE 
            WORK (I) = ZERO 
          ENDIF 
          I = I + 1 
          ENDDO 
!                                                                       
!        Convert PERMUTATIONS                                           
!                                                                       
          I = 1 
          DO WHILE (I.LE.N) 
          IF (IPIV (I) .GT.0) THEN 
            IP = IPIV (I) 
            IF (I.GT.1) THEN 
              DO 22 J = 1, I - 1 
                TEMP = A (IP, J) 
                A (IP, J) = A (I, J) 
                A (I, J) = TEMP 
   22         END DO 
            ENDIF 
          ELSE 
            IP = - IPIV (I) 
            IF (I.GT.1) THEN 
              DO 23 J = 1, I - 1 
                TEMP = A (IP, J) 
                A (IP, J) = A (I + 1, J) 
                A (I + 1, J) = TEMP 
   23         END DO 
            ENDIF 
            I = I + 1 
          ENDIF 
          I = I + 1 
          ENDDO 
        ELSE 
!                                                                       
!      Revert A (A is lower)                                            
!                                                                       
!                                                                       
!        Revert PERMUTATIONS                                            
!                                                                       
          I = N 
          DO WHILE (I.GE.1) 
          IF (IPIV (I) .GT.0) THEN 
            IP = IPIV (I) 
            IF (I.GT.1) THEN 
              DO J = 1, I - 1 
              TEMP = A (I, J) 
              A (I, J) = A (IP, J) 
              A (IP, J) = TEMP 
              ENDDO 
            ENDIF 
          ELSE 
            IP = - IPIV (I) 
            I = I - 1 
            IF (I.GT.1) THEN 
              DO J = 1, I - 1 
              TEMP = A (I + 1, J) 
              A (I + 1, J) = A (IP, J) 
              A (IP, J) = TEMP 
              ENDDO 
            ENDIF 
          ENDIF 
          I = I - 1 
          ENDDO 
!                                                                       
!        Revert VALUE                                                   
!                                                                       
          I = 1 
          DO WHILE (I.LE.N - 1) 
          IF (IPIV (I) .LT.0) THEN 
            A (I + 1, I) = WORK (I) 
            I = I + 1 
          ENDIF 
          I = I + 1 
          ENDDO 
        ENDIF 
      ENDIF 
                                                                        
      RETURN 
!                                                                       
!     End of SSYCONV                                                    
!                                                                       
      END SUBROUTINE SSYCONV                        
!> \brief <b> SSYSV computes the solution to system of linear equations 
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SSYSV + dependencies                                        
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       SUBROUTINE SSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    
!                         LWORK, INFO )                                 
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER          UPLO                                         
!       INTEGER            INFO, LDA, LDB, LWORK, N, NRHS               
!       ..                                                              
!       .. Array Arguments ..                                           
!       INTEGER            IPIV( * )                                    
!       REAL               A( LDA, * ), B( LDB, * ), WORK( * )          
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> SSYSV computes the solution to a real system of linear equations     
!>    A * X = B,                                                        
!> where A is an N-by-N symmetric matrix and X and B are N-by-NRHS      
!> matrices.                                                            
!>                                                                      
!> The diagonal pivoting method is used to factor A as                  
!>    A = U * D * U**T,  if UPLO = 'U', or                              
!>    A = L * D * L**T,  if UPLO = 'L',                                 
!> where U (or L) is a product of permutation and unit upper (lower)    
!> triangular matrices, and D is symmetric and block diagonal with      
!> 1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then   
!> used to solve the system of equations A * X = B.                     
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] UPLO                                                      
!> \verbatim                                                            
!>          UPLO is CHARACTER*1                                         
!>          = 'U':  Upper triangle of A is stored;                      
!>          = 'L':  Lower triangle of A is stored.                      
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N                                                         
!> \verbatim                                                            
!>          N is INTEGER                                                
!>          The number of linear equations, i.e., the order of the      
!>          matrix A.  N >= 0.                                          
!> \endverbatim                                                         
!>                                                                      
!> \param[in] NRHS                                                      
!> \verbatim                                                            
!>          NRHS is INTEGER                                             
!>          The number of right hand sides, i.e., the number of columns 
!>          of the matrix B.  NRHS >= 0.                                
!> \endverbatim                                                         
!>                                                                      
!> \param[in,out] A                                                     
!> \verbatim                                                            
!>          A is REAL array, dimension (LDA,N)                          
!>          On entry, the symmetric matrix A.  If UPLO = 'U', the leadin
!>          N-by-N upper triangular part of A contains the upper        
!>          triangular part of the matrix A, and the strictly lower     
!>          triangular part of A is not referenced.  If UPLO = 'L', the 
!>          leading N-by-N lower triangular part of A contains the lower
!>          triangular part of the matrix A, and the strictly upper     
!>          triangular part of A is not referenced.                     
!>                                                                      
!>          On exit, if INFO = 0, the block diagonal matrix D and the   
!>          multipliers used to obtain the factor U or L from the       
!>          factorization A = U*D*U**T or A = L*D*L**T as computed by   
!>          SSYTRF.                                                     
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDA                                                       
!> \verbatim                                                            
!>          LDA is INTEGER                                              
!>          The leading dimension of the array A.  LDA >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] IPIV                                                     
!> \verbatim                                                            
!>          IPIV is INTEGER array, dimension (N)                        
!>          Details of the interchanges and the block structure of D, as
!>          determined by SSYTRF.  If IPIV(k) > 0, then rows and columns
!>          k and IPIV(k) were interchanged, and D(k,k) is a 1-by-1     
!>          diagonal block.  If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, 
!>          then rows and columns k-1 and -IPIV(k) were interchanged and
!>          D(k-1:k,k-1:k) is a 2-by-2 diagonal block.  If UPLO = 'L' an
!>          IPIV(k) = IPIV(k+1) < 0, then rows and columns k+1 and      
!>          -IPIV(k) were interchanged and D(k:k+1,k:k+1) is a 2-by-2   
!>          diagonal block.                                             
!> \endverbatim                                                         
!>                                                                      
!> \param[in,out] B                                                     
!> \verbatim                                                            
!>          B is REAL array, dimension (LDB,NRHS)                       
!>          On entry, the N-by-NRHS right hand side matrix B.           
!>          On exit, if INFO = 0, the N-by-NRHS solution matrix X.      
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDB                                                       
!> \verbatim                                                            
!>          LDB is INTEGER                                              
!>          The leading dimension of the array B.  LDB >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] WORK                                                     
!> \verbatim                                                            
!>          WORK is REAL array, dimension (MAX(1,LWORK))                
!>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.    
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LWORK                                                     
!> \verbatim                                                            
!>          LWORK is INTEGER                                            
!>          The length of WORK.  LWORK >= 1, and for best performance   
!>          LWORK >= max(1,N*NB), where NB is the optimal blocksize for 
!>          SSYTRF.                                                     
!>          for LWORK < N, TRS will be done with Level BLAS 2           
!>          for LWORK >= N, TRS will be done with Level BLAS 3          
!>                                                                      
!>          If LWORK = -1, then a workspace query is assumed; the routin
!>          only calculates the optimal size of the WORK array, returns 
!>          this value as the first entry of the WORK array, and no erro
!>          message related to LWORK is issued by XERBLA.               
!> \endverbatim                                                         
!>                                                                      
!> \param[out] INFO                                                     
!> \verbatim                                                            
!>          INFO is INTEGER                                             
!>          = 0: successful exit                                        
!>          < 0: if INFO = -i, the i-th argument had an illegal value   
!>          > 0: if INFO = i, D(i,i) is exactly zero.  The factorization
!>               has been completed, but the block diagonal matrix D is 
!>               exactly singular, so the solution could not be computed
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup realSYsolve                                                 
!                                                                       
!  =====================================================================
      SUBROUTINE SSYSV (UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,      &
      LWORK, INFO)                                                      
!                                                                       
!  -- LAPACK driver routine (version 3.4.0) --                          
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER UPLO 
      INTEGER INFO, LDA, LDB, LWORK, N, NRHS 
!     ..                                                                
!     .. Array Arguments ..                                             
      INTEGER IPIV ( * ) 
      REAL A (LDA, * ), B (LDB, * ), WORK ( * ) 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      LOGICAL LQUERY 
      INTEGER LWKOPT 
!     ..                                                                
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      LQUERY = (LWORK.EQ. - 1) 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = - 1 
      ELSEIF (N.LT.0) THEN 
        INFO = - 2 
      ELSEIF (NRHS.LT.0) THEN 
        INFO = - 3 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = - 5 
      ELSEIF (LDB.LT.MAX (1, N) ) THEN 
        INFO = - 8 
      ELSEIF (LWORK.LT.1.AND..NOT.LQUERY) THEN 
        INFO = - 10 
      ENDIF 
!                                                                       
      IF (INFO.EQ.0) THEN 
        IF (N.EQ.0) THEN 
          LWKOPT = 1 
        ELSE 
          CALL SSYTRF (UPLO, N, A, LDA, IPIV, WORK, - 1, INFO) 
          LWKOPT = WORK (1) 
        ENDIF 
        WORK (1) = LWKOPT 
      ENDIF 
!                                                                       
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYSV ', - INFO) 
        RETURN 
      ELSEIF (LQUERY) THEN 
        RETURN 
      ENDIF 
!                                                                       
!     Compute the factorization A = U*D*U**T or A = L*D*L**T.           
!                                                                       
      CALL SSYTRF (UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO) 
      IF (INFO.EQ.0) THEN 
!                                                                       
!        Solve the system A*X = B, overwriting B with X.                
!                                                                       
        IF (LWORK.LT.N) THEN 
!                                                                       
!        Solve with TRS ( Use Level BLAS 2)                             
!                                                                       
          CALL SSYTRS (UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO) 
!                                                                       
        ELSE 
!                                                                       
!        Solve with TRS2 ( Use Level BLAS 3)                            
!                                                                       
          CALL SSYTRS2 (UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, INFO) 
!                                                                       
        ENDIF 
!                                                                       
      ENDIF 
!                                                                       
      WORK (1) = LWKOPT 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYSV                                                      
!                                                                       
      END SUBROUTINE SSYSV                          
!> \brief \b SSYTF2 computes the factorization of a real symmetric indef
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SSYTF2 + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       SUBROUTINE SSYTF2( UPLO, N, A, LDA, IPIV, INFO )                
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER          UPLO                                         
!       INTEGER            INFO, LDA, N                                 
!       ..                                                              
!       .. Array Arguments ..                                           
!       INTEGER            IPIV( * )                                    
!       REAL               A( LDA, * )                                  
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> SSYTF2 computes the factorization of a real symmetric matrix A using 
!> the Bunch-Kaufman diagonal pivoting method:                          
!>                                                                      
!>    A = U*D*U**T  or  A = L*D*L**T                                    
!>                                                                      
!> where U (or L) is a product of permutation and unit upper (lower)    
!> triangular matrices, U**T is the transpose of U, and D is symmetric a
!> block diagonal with 1-by-1 and 2-by-2 diagonal blocks.               
!>                                                                      
!> This is the unblocked version of the algorithm, calling Level 2 BLAS.
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] UPLO                                                      
!> \verbatim                                                            
!>          UPLO is CHARACTER*1                                         
!>          Specifies whether the upper or lower triangular part of the 
!>          symmetric matrix A is stored:                               
!>          = 'U':  Upper triangular                                    
!>          = 'L':  Lower triangular                                    
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N                                                         
!> \verbatim                                                            
!>          N is INTEGER                                                
!>          The order of the matrix A.  N >= 0.                         
!> \endverbatim                                                         
!>                                                                      
!> \param[in,out] A                                                     
!> \verbatim                                                            
!>          A is REAL array, dimension (LDA,N)                          
!>          On entry, the symmetric matrix A.  If UPLO = 'U', the leadin
!>          n-by-n upper triangular part of A contains the upper        
!>          triangular part of the matrix A, and the strictly lower     
!>          triangular part of A is not referenced.  If UPLO = 'L', the 
!>          leading n-by-n lower triangular part of A contains the lower
!>          triangular part of the matrix A, and the strictly upper     
!>          triangular part of A is not referenced.                     
!>                                                                      
!>          On exit, the block diagonal matrix D and the multipliers use
!>          to obtain the factor U or L (see below for further details).
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDA                                                       
!> \verbatim                                                            
!>          LDA is INTEGER                                              
!>          The leading dimension of the array A.  LDA >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] IPIV                                                     
!> \verbatim                                                            
!>          IPIV is INTEGER array, dimension (N)                        
!>          Details of the interchanges and the block structure of D.   
!>          If IPIV(k) > 0, then rows and columns k and IPIV(k) were    
!>          interchanged and D(k,k) is a 1-by-1 diagonal block.         
!>          If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and    
!>          columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k
!>          is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =    
!>          IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were  
!>          interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block. 
!> \endverbatim                                                         
!>                                                                      
!> \param[out] INFO                                                     
!> \verbatim                                                            
!>          INFO is INTEGER                                             
!>          = 0: successful exit                                        
!>          < 0: if INFO = -k, the k-th argument had an illegal value   
!>          > 0: if INFO = k, D(k,k) is exactly zero.  The factorization
!>               has been completed, but the block diagonal matrix D is 
!>               exactly singular, and division by zero will occur if it
!>               is used to solve a system of equations.                
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date September 2012                                                 
!                                                                       
!> \ingroup realSYcomputational                                         
!                                                                       
!> \par Further Details:                                                
!  =====================                                                
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!>  If UPLO = 'U', then A = U*D*U**T, where                             
!>     U = P(n)*U(n)* ... *P(k)U(k)* ...,                               
!>  i.e., U is a product of terms P(k)*U(k), where k decreases from n to
!>  1 in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1  
!>  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as   
!>  defined by IPIV(k), and U(k) is a unit upper triangular matrix, such
!>  that if the diagonal block D(k) is of order s (s = 1 or 2), then    
!>                                                                      
!>             (   I    v    0   )   k-s                                
!>     U(k) =  (   0    I    0   )   s                                  
!>             (   0    0    I   )   n-k                                
!>                k-s   s   n-k                                         
!>                                                                      
!>  If s = 1, D(k) overwrites A(k,k), and v overwrites A(1:k-1,k).      
!>  If s = 2, the upper triangle of D(k) overwrites A(k-1,k-1), A(k-1,k)
!>  and A(k,k), and v overwrites A(1:k-2,k-1:k).                        
!>                                                                      
!>  If UPLO = 'L', then A = L*D*L**T, where                             
!>     L = P(1)*L(1)* ... *P(k)*L(k)* ...,                              
!>  i.e., L is a product of terms P(k)*L(k), where k increases from 1 to
!>  n in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1  
!>  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as   
!>  defined by IPIV(k), and L(k) is a unit lower triangular matrix, such
!>  that if the diagonal block D(k) is of order s (s = 1 or 2), then    
!>                                                                      
!>             (   I    0     0   )  k-1                                
!>     L(k) =  (   0    I     0   )  s                                  
!>             (   0    v     I   )  n-k-s+1                            
!>                k-1   s  n-k-s+1                                      
!>                                                                      
!>  If s = 1, D(k) overwrites A(k,k), and v overwrites A(k+1:n,k).      
!>  If s = 2, the lower triangle of D(k) overwrites A(k,k), A(k+1,k),   
!>  and A(k+1,k+1), and v overwrites A(k+2:n,k:k+1).                    
!> \endverbatim                                                         
!                                                                       
!> \par Contributors:                                                   
!  ==================                                                   
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!>  09-29-06 - patch from                                               
!>    Bobby Cheng, MathWorks                                            
!>                                                                      
!>    Replace l.204 and l.372                                           
!>         IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN                     
!>    by                                                                
!>         IF( (MAX( ABSAKK, COLMAX ).EQ.ZERO) .OR. SISNAN(ABSAKK) ) THE
!>                                                                      
!>  01-01-96 - Based on modifications by                                
!>    J. Lewis, Boeing Computer Services Company                        
!>    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
!>  1-96 - Based on modifications by J. Lewis, Boeing Computer Services 
!>         Company                                                      
!>                                                                      
!> \endverbatim                                                         
!                                                                       
!  =====================================================================
      SUBROUTINE SSYTF2 (UPLO, N, A, LDA, IPIV, INFO) 
!                                                                       
!  -- LAPACK computational routine (version 3.4.2) --                   
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     September 2012                                                    
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER UPLO 
      INTEGER INFO, LDA, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      INTEGER IPIV ( * ) 
      REAL A (LDA, * ) 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO, ONE 
      PARAMETER (ZERO = 0.0E+0, ONE = 1.0E+0) 
      REAL EIGHT, SEVTEN 
      PARAMETER (EIGHT = 8.0E+0, SEVTEN = 17.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      LOGICAL UPPER 
      INTEGER I, IMAX, J, JMAX, K, KK, KP, KSTEP 
      REAL ABSAKK, ALPHA, COLMAX, D11, D12, D21, D22, R1, ROWMAX, T, WK,&
      WKM1, WKP1                                                        
!     ..                                                                
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, MAX, SQRT 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      UPPER = LSAME (UPLO, 'U') 
      IF (.NOT.UPPER.AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = - 1 
      ELSEIF (N.LT.0) THEN 
        INFO = - 2 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = - 4 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYTF2', - INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Initialize ALPHA for use in choosing pivot block size.            
!                                                                       
      ALPHA = (ONE+SQRT (SEVTEN) ) / EIGHT 
!                                                                       
      IF (UPPER) THEN 
!                                                                       
!        Factorize A as U*D*U**T using the upper triangle of A          
!                                                                       
!        K is the main loop index, decreasing from N to 1 in steps of   
!        1 or 2                                                         
!                                                                       
        K = N 
   10   CONTINUE 
!                                                                       
!        If K < 1, exit from loop                                       
!                                                                       
        IF (K.LT.1) GOTO 70 
        KSTEP = 1 
!                                                                       
!        Determine rows and columns to be interchanged and whether      
!        a 1-by-1 or 2-by-2 pivot block will be used                    
!                                                                       
        ABSAKK = ABS (A (K, K) ) 
!                                                                       
!        IMAX is the row-index of the largest off-diagonal element in   
!        column K, and COLMAX is its absolute value                     
!                                                                       
        IF (K.GT.1) THEN 
          IMAX = ISAMAX (K - 1, A (1, K), 1) 
          COLMAX = ABS (A (IMAX, K) ) 
        ELSE 
          COLMAX = ZERO 
        ENDIF 
!                                                                       
        IF ( (MAX (ABSAKK, COLMAX) .EQ.ZERO) .OR.SISNAN (ABSAKK) ) THEN 
!                                                                       
!           Column K is zero or contains a NaN: set INFO and continue   
!                                                                       
          IF (INFO.EQ.0) INFO = K 
          KP = K 
        ELSE 
          IF (ABSAKK.GE.ALPHA * COLMAX) THEN 
!                                                                       
!              no interchange, use 1-by-1 pivot block                   
!                                                                       
            KP = K 
          ELSE 
!                                                                       
!              JMAX is the column-index of the largest off-diagonal     
!              element in row IMAX, and ROWMAX is its absolute value    
!                                                                       
            JMAX = IMAX + ISAMAX (K - IMAX, A (IMAX, IMAX + 1), LDA) 
            ROWMAX = ABS (A (IMAX, JMAX) ) 
            IF (IMAX.GT.1) THEN 
              JMAX = ISAMAX (IMAX - 1, A (1, IMAX), 1) 
              ROWMAX = MAX (ROWMAX, ABS (A (JMAX, IMAX) ) ) 
            ENDIF 
!                                                                       
            IF (ABSAKK.GE.ALPHA * COLMAX * (COLMAX / ROWMAX) ) THEN 
!                                                                       
!                 no interchange, use 1-by-1 pivot block                
!                                                                       
              KP = K 
            ELSEIF (ABS (A (IMAX, IMAX) ) .GE.ALPHA * ROWMAX) THEN 
!                                                                       
!                 interchange rows and columns K and IMAX, use 1-by-1   
!                 pivot block                                           
!                                                                       
              KP = IMAX 
            ELSE 
!                                                                       
!                 interchange rows and columns K-1 and IMAX, use 2-by-2 
!                 pivot block                                           
!                                                                       
              KP = IMAX 
              KSTEP = 2 
            ENDIF 
          ENDIF 
!                                                                       
          KK = K - KSTEP + 1 
          IF (KP.NE.KK) THEN 
!                                                                       
!              Interchange rows and columns KK and KP in the leading    
!              submatrix A(1:k,1:k)                                     
!                                                                       
            CALL SSWAP (KP - 1, A (1, KK), 1, A (1, KP), 1) 
            CALL SSWAP (KK - KP - 1, A (KP + 1, KK), 1, A (KP, KP + 1), &
            LDA)                                                        
            T = A (KK, KK) 
            A (KK, KK) = A (KP, KP) 
            A (KP, KP) = T 
            IF (KSTEP.EQ.2) THEN 
              T = A (K - 1, K) 
              A (K - 1, K) = A (KP, K) 
              A (KP, K) = T 
            ENDIF 
          ENDIF 
!                                                                       
!           Update the leading submatrix                                
!                                                                       
          IF (KSTEP.EQ.1) THEN 
!                                                                       
!              1-by-1 pivot block D(k): column k now holds              
!                                                                       
!              W(k) = U(k)*D(k)                                         
!                                                                       
!              where U(k) is the k-th column of U                       
!                                                                       
!              Perform a rank-1 update of A(1:k-1,1:k-1) as             
!                                                                       
!              A := A - U(k)*D(k)*U(k)**T = A - W(k)*1/D(k)*W(k)**T     
!                                                                       
            R1 = ONE / A (K, K) 
            CALL SSYR (UPLO, K - 1, - R1, A (1, K), 1, A, LDA) 
!                                                                       
!              Store U(k) in column k                                   
!                                                                       
            CALL SSCAL (K - 1, R1, A (1, K), 1) 
          ELSE 
!                                                                       
!              2-by-2 pivot block D(k): columns k and k-1 now hold      
!                                                                       
!              ( W(k-1) W(k) ) = ( U(k-1) U(k) )*D(k)                   
!                                                                       
!              where U(k) and U(k-1) are the k-th and (k-1)-th columns  
!              of U                                                     
!                                                                       
!              Perform a rank-2 update of A(1:k-2,1:k-2) as             
!                                                                       
!              A := A - ( U(k-1) U(k) )*D(k)*( U(k-1) U(k) )**T         
!                 = A - ( W(k-1) W(k) )*inv(D(k))*( W(k-1) W(k) )**T    
!                                                                       
            IF (K.GT.2) THEN 
!                                                                       
              D12 = A (K - 1, K) 
              D22 = A (K - 1, K - 1) / D12 
              D11 = A (K, K) / D12 
              T = ONE / (D11 * D22 - ONE) 
              D12 = T / D12 
!                                                                       
              DO 30 J = K - 2, 1, - 1 
                WKM1 = D12 * (D11 * A (J, K - 1) - A (J, K) ) 
                WK = D12 * (D22 * A (J, K) - A (J, K - 1) ) 
                DO 20 I = J, 1, - 1 
                  A (I, J) = A (I, J) - A (I, K) * WK - A (I, K - 1)    &
                  * WKM1                                                
   20           END DO 
                A (J, K) = WK 
                A (J, K - 1) = WKM1 
   30         END DO 
!                                                                       
            ENDIF 
!                                                                       
          ENDIF 
        ENDIF 
!                                                                       
!        Store details of the interchanges in IPIV                      
!                                                                       
        IF (KSTEP.EQ.1) THEN 
          IPIV (K) = KP 
        ELSE 
          IPIV (K) = - KP 
          IPIV (K - 1) = - KP 
        ENDIF 
!                                                                       
!        Decrease K and return to the start of the main loop            
!                                                                       
        K = K - KSTEP 
        GOTO 10 
!                                                                       
      ELSE 
!                                                                       
!        Factorize A as L*D*L**T using the lower triangle of A          
!                                                                       
!        K is the main loop index, increasing from 1 to N in steps of   
!        1 or 2                                                         
!                                                                       
        K = 1 
   40   CONTINUE 
!                                                                       
!        If K > N, exit from loop                                       
!                                                                       
        IF (K.GT.N) GOTO 70 
        KSTEP = 1 
!                                                                       
!        Determine rows and columns to be interchanged and whether      
!        a 1-by-1 or 2-by-2 pivot block will be used                    
!                                                                       
        ABSAKK = ABS (A (K, K) ) 
!                                                                       
!        IMAX is the row-index of the largest off-diagonal element in   
!        column K, and COLMAX is its absolute value                     
!                                                                       
        IF (K.LT.N) THEN 
          IMAX = K + ISAMAX (N - K, A (K + 1, K), 1) 
          COLMAX = ABS (A (IMAX, K) ) 
        ELSE 
          COLMAX = ZERO 
        ENDIF 
!                                                                       
        IF ( (MAX (ABSAKK, COLMAX) .EQ.ZERO) .OR.SISNAN (ABSAKK) ) THEN 
!                                                                       
!           Column K is zero or contains a NaN: set INFO and continue   
!                                                                       
          IF (INFO.EQ.0) INFO = K 
          KP = K 
        ELSE 
          IF (ABSAKK.GE.ALPHA * COLMAX) THEN 
!                                                                       
!              no interchange, use 1-by-1 pivot block                   
!                                                                       
            KP = K 
          ELSE 
!                                                                       
!              JMAX is the column-index of the largest off-diagonal     
!              element in row IMAX, and ROWMAX is its absolute value    
!                                                                       
            JMAX = K - 1 + ISAMAX (IMAX - K, A (IMAX, K), LDA) 
            ROWMAX = ABS (A (IMAX, JMAX) ) 
            IF (IMAX.LT.N) THEN 
              JMAX = IMAX + ISAMAX (N - IMAX, A (IMAX + 1, IMAX),       &
              1)                                                        
              ROWMAX = MAX (ROWMAX, ABS (A (JMAX, IMAX) ) ) 
            ENDIF 
!                                                                       
            IF (ABSAKK.GE.ALPHA * COLMAX * (COLMAX / ROWMAX) ) THEN 
!                                                                       
!                 no interchange, use 1-by-1 pivot block                
!                                                                       
              KP = K 
            ELSEIF (ABS (A (IMAX, IMAX) ) .GE.ALPHA * ROWMAX) THEN 
!                                                                       
!                 interchange rows and columns K and IMAX, use 1-by-1   
!                 pivot block                                           
!                                                                       
              KP = IMAX 
            ELSE 
!                                                                       
!                 interchange rows and columns K+1 and IMAX, use 2-by-2 
!                 pivot block                                           
!                                                                       
              KP = IMAX 
              KSTEP = 2 
            ENDIF 
          ENDIF 
!                                                                       
          KK = K + KSTEP - 1 
          IF (KP.NE.KK) THEN 
!                                                                       
!              Interchange rows and columns KK and KP in the trailing   
!              submatrix A(k:n,k:n)                                     
!                                                                       
            IF (KP.LT.N) CALL SSWAP (N - KP, A (KP + 1, KK), 1, A (KP + &
            1, KP), 1)                                                  
            CALL SSWAP (KP - KK - 1, A (KK + 1, KK), 1, A (KP, KK + 1), &
            LDA)                                                        
            T = A (KK, KK) 
            A (KK, KK) = A (KP, KP) 
            A (KP, KP) = T 
            IF (KSTEP.EQ.2) THEN 
              T = A (K + 1, K) 
              A (K + 1, K) = A (KP, K) 
              A (KP, K) = T 
            ENDIF 
          ENDIF 
!                                                                       
!           Update the trailing submatrix                               
!                                                                       
          IF (KSTEP.EQ.1) THEN 
!                                                                       
!              1-by-1 pivot block D(k): column k now holds              
!                                                                       
!              W(k) = L(k)*D(k)                                         
!                                                                       
!              where L(k) is the k-th column of L                       
!                                                                       
            IF (K.LT.N) THEN 
!                                                                       
!                 Perform a rank-1 update of A(k+1:n,k+1:n) as          
!                                                                       
!                 A := A - L(k)*D(k)*L(k)**T = A - W(k)*(1/D(k))*W(k)**T
!                                                                       
              D11 = ONE / A (K, K) 
              CALL SSYR (UPLO, N - K, - D11, A (K + 1, K), 1, A (K + 1, &
              K + 1), LDA)                                              
!                                                                       
!                 Store L(k) in column K                                
!                                                                       
              CALL SSCAL (N - K, D11, A (K + 1, K), 1) 
            ENDIF 
          ELSE 
!                                                                       
!              2-by-2 pivot block D(k)                                  
!                                                                       
            IF (K.LT.N - 1) THEN 
!                                                                       
!                 Perform a rank-2 update of A(k+2:n,k+2:n) as          
!                                                                       
!                 A := A - ( (A(k) A(k+1))*D(k)**(-1) ) * (A(k) A(k+1))*
!                                                                       
!                 where L(k) and L(k+1) are the k-th and (k+1)-th       
!                 columns of L                                          
!                                                                       
              D21 = A (K + 1, K) 
              D11 = A (K + 1, K + 1) / D21 
              D22 = A (K, K) / D21 
              T = ONE / (D11 * D22 - ONE) 
              D21 = T / D21 
!                                                                       
              DO 60 J = K + 2, N 
!                                                                       
                WK = D21 * (D11 * A (J, K) - A (J, K + 1) ) 
                WKP1 = D21 * (D22 * A (J, K + 1) - A (J, K) ) 
!                                                                       
                DO 50 I = J, N 
                  A (I, J) = A (I, J) - A (I, K) * WK - A (I, K + 1)    &
                  * WKP1                                                
   50           END DO 
!                                                                       
                A (J, K) = WK 
                A (J, K + 1) = WKP1 
!                                                                       
   60         END DO 
            ENDIF 
          ENDIF 
        ENDIF 
!                                                                       
!        Store details of the interchanges in IPIV                      
!                                                                       
        IF (KSTEP.EQ.1) THEN 
          IPIV (K) = KP 
        ELSE 
          IPIV (K) = - KP 
          IPIV (K + 1) = - KP 
        ENDIF 
!                                                                       
!        Increase K and return to the start of the main loop            
!                                                                       
        K = K + KSTEP 
        GOTO 40 
!                                                                       
      ENDIF 
!                                                                       
   70 CONTINUE 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYTF2                                                     
!                                                                       
      END SUBROUTINE SSYTF2                         
!> \brief \b SSYTRF                                                     
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SSYTRF + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       SUBROUTINE SSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )   
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER          UPLO                                         
!       INTEGER            INFO, LDA, LWORK, N                          
!       ..                                                              
!       .. Array Arguments ..                                           
!       INTEGER            IPIV( * )                                    
!       REAL               A( LDA, * ), WORK( * )                       
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> SSYTRF computes the factorization of a real symmetric matrix A using 
!> the Bunch-Kaufman diagonal pivoting method.  The form of the         
!> factorization is                                                     
!>                                                                      
!>    A = U*D*U**T  or  A = L*D*L**T                                    
!>                                                                      
!> where U (or L) is a product of permutation and unit upper (lower)    
!> triangular matrices, and D is symmetric and block diagonal with      
!> 1-by-1 and 2-by-2 diagonal blocks.                                   
!>                                                                      
!> This is the blocked version of the algorithm, calling Level 3 BLAS.  
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] UPLO                                                      
!> \verbatim                                                            
!>          UPLO is CHARACTER*1                                         
!>          = 'U':  Upper triangle of A is stored;                      
!>          = 'L':  Lower triangle of A is stored.                      
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N                                                         
!> \verbatim                                                            
!>          N is INTEGER                                                
!>          The order of the matrix A.  N >= 0.                         
!> \endverbatim                                                         
!>                                                                      
!> \param[in,out] A                                                     
!> \verbatim                                                            
!>          A is REAL array, dimension (LDA,N)                          
!>          On entry, the symmetric matrix A.  If UPLO = 'U', the leadin
!>          N-by-N upper triangular part of A contains the upper        
!>          triangular part of the matrix A, and the strictly lower     
!>          triangular part of A is not referenced.  If UPLO = 'L', the 
!>          leading N-by-N lower triangular part of A contains the lower
!>          triangular part of the matrix A, and the strictly upper     
!>          triangular part of A is not referenced.                     
!>                                                                      
!>          On exit, the block diagonal matrix D and the multipliers use
!>          to obtain the factor U or L (see below for further details).
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDA                                                       
!> \verbatim                                                            
!>          LDA is INTEGER                                              
!>          The leading dimension of the array A.  LDA >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] IPIV                                                     
!> \verbatim                                                            
!>          IPIV is INTEGER array, dimension (N)                        
!>          Details of the interchanges and the block structure of D.   
!>          If IPIV(k) > 0, then rows and columns k and IPIV(k) were    
!>          interchanged and D(k,k) is a 1-by-1 diagonal block.         
!>          If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and    
!>          columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k
!>          is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =    
!>          IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were  
!>          interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block. 
!> \endverbatim                                                         
!>                                                                      
!> \param[out] WORK                                                     
!> \verbatim                                                            
!>          WORK is REAL array, dimension (MAX(1,LWORK))                
!>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.    
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LWORK                                                     
!> \verbatim                                                            
!>          LWORK is INTEGER                                            
!>          The length of WORK.  LWORK >=1.  For best performance       
!>          LWORK >= N*NB, where NB is the block size returned by ILAENV
!>                                                                      
!>          If LWORK = -1, then a workspace query is assumed; the routin
!>          only calculates the optimal size of the WORK array, returns 
!>          this value as the first entry of the WORK array, and no erro
!>          message related to LWORK is issued by XERBLA.               
!> \endverbatim                                                         
!>                                                                      
!> \param[out] INFO                                                     
!> \verbatim                                                            
!>          INFO is INTEGER                                             
!>          = 0:  successful exit                                       
!>          < 0:  if INFO = -i, the i-th argument had an illegal value  
!>          > 0:  if INFO = i, D(i,i) is exactly zero.  The factorizatio
!>                has been completed, but the block diagonal matrix D is
!>                exactly singular, and division by zero will occur if i
!>                is used to solve a system of equations.               
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup realSYcomputational                                         
!                                                                       
!> \par Further Details:                                                
!  =====================                                                
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!>  If UPLO = 'U', then A = U*D*U**T, where                             
!>     U = P(n)*U(n)* ... *P(k)U(k)* ...,                               
!>  i.e., U is a product of terms P(k)*U(k), where k decreases from n to
!>  1 in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1  
!>  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as   
!>  defined by IPIV(k), and U(k) is a unit upper triangular matrix, such
!>  that if the diagonal block D(k) is of order s (s = 1 or 2), then    
!>                                                                      
!>             (   I    v    0   )   k-s                                
!>     U(k) =  (   0    I    0   )   s                                  
!>             (   0    0    I   )   n-k                                
!>                k-s   s   n-k                                         
!>                                                                      
!>  If s = 1, D(k) overwrites A(k,k), and v overwrites A(1:k-1,k).      
!>  If s = 2, the upper triangle of D(k) overwrites A(k-1,k-1), A(k-1,k)
!>  and A(k,k), and v overwrites A(1:k-2,k-1:k).                        
!>                                                                      
!>  If UPLO = 'L', then A = L*D*L**T, where                             
!>     L = P(1)*L(1)* ... *P(k)*L(k)* ...,                              
!>  i.e., L is a product of terms P(k)*L(k), where k increases from 1 to
!>  n in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1  
!>  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as   
!>  defined by IPIV(k), and L(k) is a unit lower triangular matrix, such
!>  that if the diagonal block D(k) is of order s (s = 1 or 2), then    
!>                                                                      
!>             (   I    0     0   )  k-1                                
!>     L(k) =  (   0    I     0   )  s                                  
!>             (   0    v     I   )  n-k-s+1                            
!>                k-1   s  n-k-s+1                                      
!>                                                                      
!>  If s = 1, D(k) overwrites A(k,k), and v overwrites A(k+1:n,k).      
!>  If s = 2, the lower triangle of D(k) overwrites A(k,k), A(k+1,k),   
!>  and A(k+1,k+1), and v overwrites A(k+2:n,k:k+1).                    
!> \endverbatim                                                         
!>                                                                      
!  =====================================================================
      SUBROUTINE SSYTRF (UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO) 
!                                                                       
!  -- LAPACK computational routine (version 3.4.0) --                   
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER UPLO 
      INTEGER INFO, LDA, LWORK, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      INTEGER IPIV ( * ) 
      REAL A (LDA, * ), WORK ( * ) 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      LOGICAL LQUERY, UPPER 
      INTEGER IINFO, IWS, J, K, KB, LDWORK, LWKOPT, NB, NBMIN 
!     ..                                                                
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      UPPER = LSAME (UPLO, 'U') 
      LQUERY = (LWORK.EQ. - 1) 
      IF (.NOT.UPPER.AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = - 1 
      ELSEIF (N.LT.0) THEN 
        INFO = - 2 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = - 4 
      ELSEIF (LWORK.LT.1.AND..NOT.LQUERY) THEN 
        INFO = - 7 
      ENDIF 
!                                                                       
      IF (INFO.EQ.0) THEN 
!                                                                       
!        Determine the block size                                       
!                                                                       
        NB = ILAENV (1, 'SSYTRF', UPLO, N, - 1, - 1, - 1) 
        LWKOPT = N * NB 
        WORK (1) = LWKOPT 
      ENDIF 
!                                                                       
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYTRF', - INFO) 
        RETURN 
      ELSEIF (LQUERY) THEN 
        RETURN 
      ENDIF 
!                                                                       
      NBMIN = 2 
      LDWORK = N 
      IF (NB.GT.1.AND.NB.LT.N) THEN 
        IWS = LDWORK * NB 
        IF (LWORK.LT.IWS) THEN 
          NB = MAX (LWORK / LDWORK, 1) 
          NBMIN = MAX (2, ILAENV (2, 'SSYTRF', UPLO, N, - 1, - 1,       &
          - 1) )                                                        
        ENDIF 
      ELSE 
        IWS = 1 
      ENDIF 
      IF (NB.LT.NBMIN) NB = N 
!                                                                       
      IF (UPPER) THEN 
!                                                                       
!        Factorize A as U*D*U**T using the upper triangle of A          
!                                                                       
!        K is the main loop index, decreasing from N to 1 in steps of   
!        KB, where KB is the number of columns factorized by SLASYF;    
!        KB is either NB or NB-1, or K for the last block               
!                                                                       
        K = N 
   10   CONTINUE 
!                                                                       
!        If K < 1, exit from loop                                       
!                                                                       
        IF (K.LT.1) GOTO 40 
!                                                                       
        IF (K.GT.NB) THEN 
!                                                                       
!           Factorize columns k-kb+1:k of A and use blocked code to     
!           update columns 1:k-kb                                       
!                                                                       
          CALL SLASYF (UPLO, K, NB, KB, A, LDA, IPIV, WORK, LDWORK,     &
          IINFO)                                                        
        ELSE 
!                                                                       
!           Use unblocked code to factorize columns 1:k of A            
!                                                                       
          CALL SSYTF2 (UPLO, K, A, LDA, IPIV, IINFO) 
          KB = K 
        ENDIF 
!                                                                       
!        Set INFO on the first occurrence of a zero pivot               
!                                                                       
        IF (INFO.EQ.0.AND.IINFO.GT.0) INFO = IINFO 
!                                                                       
!        Decrease K and return to the start of the main loop            
!                                                                       
        K = K - KB 
        GOTO 10 
!                                                                       
      ELSE 
!                                                                       
!        Factorize A as L*D*L**T using the lower triangle of A          
!                                                                       
!        K is the main loop index, increasing from 1 to N in steps of   
!        KB, where KB is the number of columns factorized by SLASYF;    
!        KB is either NB or NB-1, or N-K+1 for the last block           
!                                                                       
        K = 1 
   20   CONTINUE 
!                                                                       
!        If K > N, exit from loop                                       
!                                                                       
        IF (K.GT.N) GOTO 40 
!                                                                       
        IF (K.LE.N - NB) THEN 
!                                                                       
!           Factorize columns k:k+kb-1 of A and use blocked code to     
!           update columns k+kb:n                                       
!                                                                       
          CALL SLASYF (UPLO, N - K + 1, NB, KB, A (K, K), LDA, IPIV (K),&
          WORK, LDWORK, IINFO)                                          
        ELSE 
!                                                                       
!           Use unblocked code to factorize columns k:n of A            
!                                                                       
          CALL SSYTF2 (UPLO, N - K + 1, A (K, K), LDA, IPIV (K),        &
          IINFO)                                                        
          KB = N - K + 1 
        ENDIF 
!                                                                       
!        Set INFO on the first occurrence of a zero pivot               
!                                                                       
        IF (INFO.EQ.0.AND.IINFO.GT.0) INFO = IINFO + K - 1 
!                                                                       
!        Adjust IPIV                                                    
!                                                                       
        DO 30 J = K, K + KB - 1 
          IF (IPIV (J) .GT.0) THEN 
            IPIV (J) = IPIV (J) + K - 1 
          ELSE 
            IPIV (J) = IPIV (J) - K + 1 
          ENDIF 
   30   END DO 
!                                                                       
!        Increase K and return to the start of the main loop            
!                                                                       
        K = K + KB 
        GOTO 20 
!                                                                       
      ENDIF 
!                                                                       
   40 CONTINUE 
      WORK (1) = LWKOPT 
      RETURN 
!                                                                       
!     End of SSYTRF                                                     
!                                                                       
      END SUBROUTINE SSYTRF                         
!> \brief \b SSYTRS                                                     
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SSYTRS + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       SUBROUTINE SSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )  
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER          UPLO                                         
!       INTEGER            INFO, LDA, LDB, N, NRHS                      
!       ..                                                              
!       .. Array Arguments ..                                           
!       INTEGER            IPIV( * )                                    
!       REAL               A( LDA, * ), B( LDB, * )                     
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> SSYTRS solves a system of linear equations A*X = B with a real       
!> symmetric matrix A using the factorization A = U*D*U**T or           
!> A = L*D*L**T computed by SSYTRF.                                     
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] UPLO                                                      
!> \verbatim                                                            
!>          UPLO is CHARACTER*1                                         
!>          Specifies whether the details of the factorization are store
!>          as an upper or lower triangular matrix.                     
!>          = 'U':  Upper triangular, form is A = U*D*U**T;             
!>          = 'L':  Lower triangular, form is A = L*D*L**T.             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N                                                         
!> \verbatim                                                            
!>          N is INTEGER                                                
!>          The order of the matrix A.  N >= 0.                         
!> \endverbatim                                                         
!>                                                                      
!> \param[in] NRHS                                                      
!> \verbatim                                                            
!>          NRHS is INTEGER                                             
!>          The number of right hand sides, i.e., the number of columns 
!>          of the matrix B.  NRHS >= 0.                                
!> \endverbatim                                                         
!>                                                                      
!> \param[in] A                                                         
!> \verbatim                                                            
!>          A is REAL array, dimension (LDA,N)                          
!>          The block diagonal matrix D and the multipliers used to     
!>          obtain the factor U or L as computed by SSYTRF.             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDA                                                       
!> \verbatim                                                            
!>          LDA is INTEGER                                              
!>          The leading dimension of the array A.  LDA >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[in] IPIV                                                      
!> \verbatim                                                            
!>          IPIV is INTEGER array, dimension (N)                        
!>          Details of the interchanges and the block structure of D    
!>          as determined by SSYTRF.                                    
!> \endverbatim                                                         
!>                                                                      
!> \param[in,out] B                                                     
!> \verbatim                                                            
!>          B is REAL array, dimension (LDB,NRHS)                       
!>          On entry, the right hand side matrix B.                     
!>          On exit, the solution matrix X.                             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDB                                                       
!> \verbatim                                                            
!>          LDB is INTEGER                                              
!>          The leading dimension of the array B.  LDB >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] INFO                                                     
!> \verbatim                                                            
!>          INFO is INTEGER                                             
!>          = 0:  successful exit                                       
!>          < 0:  if INFO = -i, the i-th argument had an illegal value  
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup realSYcomputational                                         
!                                                                       
!  =====================================================================
      SUBROUTINE SSYTRS (UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO) 
!                                                                       
!  -- LAPACK computational routine (version 3.4.0) --                   
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER UPLO 
      INTEGER INFO, LDA, LDB, N, NRHS 
!     ..                                                                
!     .. Array Arguments ..                                             
      INTEGER IPIV ( * ) 
      REAL A (LDA, * ), B (LDB, * ) 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE 
      PARAMETER (ONE = 1.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      LOGICAL UPPER 
      INTEGER J, K, KP 
      REAL AK, AKM1, AKM1K, BK, BKM1, DENOM 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
      INFO = 0 
      UPPER = LSAME (UPLO, 'U') 
      IF (.NOT.UPPER.AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = - 1 
      ELSEIF (N.LT.0) THEN 
        INFO = - 2 
      ELSEIF (NRHS.LT.0) THEN 
        INFO = - 3 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = - 5 
      ELSEIF (LDB.LT.MAX (1, N) ) THEN 
        INFO = - 8 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYTRS', - INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible                                          
!                                                                       
      IF (N.EQ.0.OR.NRHS.EQ.0) RETURN 
!                                                                       
      IF (UPPER) THEN 
!                                                                       
!        Solve A*X = B, where A = U*D*U**T.                             
!                                                                       
!        First solve U*D*X = B, overwriting B with X.                   
!                                                                       
!        K is the main loop index, decreasing from N to 1 in steps of   
!        1 or 2, depending on the size of the diagonal blocks.          
!                                                                       
        K = N 
   10   CONTINUE 
!                                                                       
!        If K < 1, exit from loop.                                      
!                                                                       
        IF (K.LT.1) GOTO 30 
!                                                                       
        IF (IPIV (K) .GT.0) THEN 
!                                                                       
!           1 x 1 diagonal block                                        
!                                                                       
!           Interchange rows K and IPIV(K).                             
!                                                                       
          KP = IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
!                                                                       
!           Multiply by inv(U(K)), where U(K) is the transformation     
!           stored in column K of A.                                    
!                                                                       
          CALL SGER (K - 1, NRHS, - ONE, A (1, K), 1, B (K, 1), LDB, B (&
          1, 1), LDB)                                                   
!                                                                       
!           Multiply by the inverse of the diagonal block.              
!                                                                       
          CALL SSCAL (NRHS, ONE / A (K, K), B (K, 1), LDB) 
          K = K - 1 
        ELSE 
!                                                                       
!           2 x 2 diagonal block                                        
!                                                                       
!           Interchange rows K-1 and -IPIV(K).                          
!                                                                       
          KP = - IPIV (K) 
          IF (KP.NE.K - 1) CALL SSWAP (NRHS, B (K - 1, 1), LDB, B (KP,  &
          1), LDB)                                                      
!                                                                       
!           Multiply by inv(U(K)), where U(K) is the transformation     
!           stored in columns K-1 and K of A.                           
!                                                                       
          CALL SGER (K - 2, NRHS, - ONE, A (1, K), 1, B (K, 1), LDB, B (&
          1, 1), LDB)                                                   
          CALL SGER (K - 2, NRHS, - ONE, A (1, K - 1), 1, B (K - 1, 1), &
          LDB, B (1, 1), LDB)                                           
!                                                                       
!           Multiply by the inverse of the diagonal block.              
!                                                                       
          AKM1K = A (K - 1, K) 
          AKM1 = A (K - 1, K - 1) / AKM1K 
          AK = A (K, K) / AKM1K 
          DENOM = AKM1 * AK - ONE 
          DO 20 J = 1, NRHS 
            BKM1 = B (K - 1, J) / AKM1K 
            BK = B (K, J) / AKM1K 
            B (K - 1, J) = (AK * BKM1 - BK) / DENOM 
            B (K, J) = (AKM1 * BK - BKM1) / DENOM 
   20     END DO 
          K = K - 2 
        ENDIF 
!                                                                       
        GOTO 10 
   30   CONTINUE 
!                                                                       
!        Next solve U**T *X = B, overwriting B with X.                  
!                                                                       
!        K is the main loop index, increasing from 1 to N in steps of   
!        1 or 2, depending on the size of the diagonal blocks.          
!                                                                       
        K = 1 
   40   CONTINUE 
!                                                                       
!        If K > N, exit from loop.                                      
!                                                                       
        IF (K.GT.N) GOTO 50 
!                                                                       
        IF (IPIV (K) .GT.0) THEN 
!                                                                       
!           1 x 1 diagonal block                                        
!                                                                       
!           Multiply by inv(U**T(K)), where U(K) is the transformation  
!           stored in column K of A.                                    
!                                                                       
          CALL SGEMV ('Transpose', K - 1, NRHS, - ONE, B, LDB, A (1, K) &
          , 1, ONE, B (K, 1) , LDB)                                     
!                                                                       
!           Interchange rows K and IPIV(K).                             
!                                                                       
          KP = IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
          K = K + 1 
        ELSE 
!                                                                       
!           2 x 2 diagonal block                                        
!                                                                       
!           Multiply by inv(U**T(K+1)), where U(K+1) is the transformati
!           stored in columns K and K+1 of A.                           
!                                                                       
          CALL SGEMV ('Transpose', K - 1, NRHS, - ONE, B, LDB, A (1, K) &
          , 1, ONE, B (K, 1) , LDB)                                     
          CALL SGEMV ('Transpose', K - 1, NRHS, - ONE, B, LDB, A (1, K +&
          1) , 1, ONE, B (K + 1, 1) , LDB)                              
!                                                                       
!           Interchange rows K and -IPIV(K).                            
!                                                                       
          KP = - IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
          K = K + 2 
        ENDIF 
!                                                                       
        GOTO 40 
   50   CONTINUE 
!                                                                       
      ELSE 
!                                                                       
!        Solve A*X = B, where A = L*D*L**T.                             
!                                                                       
!        First solve L*D*X = B, overwriting B with X.                   
!                                                                       
!        K is the main loop index, increasing from 1 to N in steps of   
!        1 or 2, depending on the size of the diagonal blocks.          
!                                                                       
        K = 1 
   60   CONTINUE 
!                                                                       
!        If K > N, exit from loop.                                      
!                                                                       
        IF (K.GT.N) GOTO 80 
!                                                                       
        IF (IPIV (K) .GT.0) THEN 
!                                                                       
!           1 x 1 diagonal block                                        
!                                                                       
!           Interchange rows K and IPIV(K).                             
!                                                                       
          KP = IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
!                                                                       
!           Multiply by inv(L(K)), where L(K) is the transformation     
!           stored in column K of A.                                    
!                                                                       
          IF (K.LT.N) CALL SGER (N - K, NRHS, - ONE, A (K + 1, K),      &
          1, B (K, 1), LDB, B (K + 1, 1), LDB)                          
!                                                                       
!           Multiply by the inverse of the diagonal block.              
!                                                                       
          CALL SSCAL (NRHS, ONE / A (K, K), B (K, 1), LDB) 
          K = K + 1 
        ELSE 
!                                                                       
!           2 x 2 diagonal block                                        
!                                                                       
!           Interchange rows K+1 and -IPIV(K).                          
!                                                                       
          KP = - IPIV (K) 
          IF (KP.NE.K + 1) CALL SSWAP (NRHS, B (K + 1, 1), LDB, B (KP,  &
          1), LDB)                                                      
!                                                                       
!           Multiply by inv(L(K)), where L(K) is the transformation     
!           stored in columns K and K+1 of A.                           
!                                                                       
          IF (K.LT.N - 1) THEN 
            CALL SGER (N - K - 1, NRHS, - ONE, A (K + 2, K), 1, B (K, 1)&
            , LDB, B (K + 2, 1), LDB)                                   
            CALL SGER (N - K - 1, NRHS, - ONE, A (K + 2, K + 1),        &
            1, B (K + 1, 1), LDB, B (K + 2, 1), LDB)                    
          ENDIF 
!                                                                       
!           Multiply by the inverse of the diagonal block.              
!                                                                       
          AKM1K = A (K + 1, K) 
          AKM1 = A (K, K) / AKM1K 
          AK = A (K + 1, K + 1) / AKM1K 
          DENOM = AKM1 * AK - ONE 
          DO 70 J = 1, NRHS 
            BKM1 = B (K, J) / AKM1K 
            BK = B (K + 1, J) / AKM1K 
            B (K, J) = (AK * BKM1 - BK) / DENOM 
            B (K + 1, J) = (AKM1 * BK - BKM1) / DENOM 
   70     END DO 
          K = K + 2 
        ENDIF 
!                                                                       
        GOTO 60 
   80   CONTINUE 
!                                                                       
!        Next solve L**T *X = B, overwriting B with X.                  
!                                                                       
!        K is the main loop index, decreasing from N to 1 in steps of   
!        1 or 2, depending on the size of the diagonal blocks.          
!                                                                       
        K = N 
   90   CONTINUE 
!                                                                       
!        If K < 1, exit from loop.                                      
!                                                                       
        IF (K.LT.1) GOTO 100 
!                                                                       
        IF (IPIV (K) .GT.0) THEN 
!                                                                       
!           1 x 1 diagonal block                                        
!                                                                       
!           Multiply by inv(L**T(K)), where L(K) is the transformation  
!           stored in column K of A.                                    
!                                                                       
          IF (K.LT.N) CALL SGEMV ('Transpose', N - K, NRHS, - ONE, B (K &
          + 1, 1) , LDB, A (K + 1, K) , 1, ONE, B (K, 1) , LDB)         
!                                                                       
!           Interchange rows K and IPIV(K).                             
!                                                                       
          KP = IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
          K = K - 1 
        ELSE 
!                                                                       
!           2 x 2 diagonal block                                        
!                                                                       
!           Multiply by inv(L**T(K-1)), where L(K-1) is the transformati
!           stored in columns K-1 and K of A.                           
!                                                                       
          IF (K.LT.N) THEN 
            CALL SGEMV ('Transpose', N - K, NRHS, - ONE, B (K + 1, 1) , &
            LDB, A (K + 1, K) , 1, ONE, B (K, 1) , LDB)                 
            CALL SGEMV ('Transpose', N - K, NRHS, - ONE, B (K + 1, 1) , &
            LDB, A (K + 1, K - 1) , 1, ONE, B (K - 1, 1) , LDB)         
          ENDIF 
!                                                                       
!           Interchange rows K and -IPIV(K).                            
!                                                                       
          KP = - IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
          K = K - 2 
        ENDIF 
!                                                                       
        GOTO 90 
  100   CONTINUE 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYTRS                                                     
!                                                                       
      END SUBROUTINE SSYTRS                         
!> \brief \b SSYTRS2                                                    
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download SSYTRS2 + dependencies                                      
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       SUBROUTINE SSYTRS2( UPLO, N, NRHS, A, LDA, IPIV, B, LDB,        
!                           WORK, INFO )                                
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER          UPLO                                         
!       INTEGER            INFO, LDA, LDB, N, NRHS                      
!       ..                                                              
!       .. Array Arguments ..                                           
!       INTEGER            IPIV( * )                                    
!       REAL               A( LDA, * ), B( LDB, * ), WORK( * )          
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> SSYTRS2 solves a system of linear equations A*X = B with a real      
!> symmetric matrix A using the factorization A = U*D*U**T or           
!> A = L*D*L**T computed by SSYTRF and converted by SSYCONV.            
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] UPLO                                                      
!> \verbatim                                                            
!>          UPLO is CHARACTER*1                                         
!>          Specifies whether the details of the factorization are store
!>          as an upper or lower triangular matrix.                     
!>          = 'U':  Upper triangular, form is A = U*D*U**T;             
!>          = 'L':  Lower triangular, form is A = L*D*L**T.             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] N                                                         
!> \verbatim                                                            
!>          N is INTEGER                                                
!>          The order of the matrix A.  N >= 0.                         
!> \endverbatim                                                         
!>                                                                      
!> \param[in] NRHS                                                      
!> \verbatim                                                            
!>          NRHS is INTEGER                                             
!>          The number of right hand sides, i.e., the number of columns 
!>          of the matrix B.  NRHS >= 0.                                
!> \endverbatim                                                         
!>                                                                      
!> \param[in] A                                                         
!> \verbatim                                                            
!>          A is REAL array, dimension (LDA,N)                          
!>          The block diagonal matrix D and the multipliers used to     
!>          obtain the factor U or L as computed by SSYTRF.             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDA                                                       
!> \verbatim                                                            
!>          LDA is INTEGER                                              
!>          The leading dimension of the array A.  LDA >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[in] IPIV                                                      
!> \verbatim                                                            
!>          IPIV is INTEGER array, dimension (N)                        
!>          Details of the interchanges and the block structure of D    
!>          as determined by SSYTRF.                                    
!> \endverbatim                                                         
!>                                                                      
!> \param[in,out] B                                                     
!> \verbatim                                                            
!>          B is REAL array, dimension (LDB,NRHS)                       
!>          On entry, the right hand side matrix B.                     
!>          On exit, the solution matrix X.                             
!> \endverbatim                                                         
!>                                                                      
!> \param[in] LDB                                                       
!> \verbatim                                                            
!>          LDB is INTEGER                                              
!>          The leading dimension of the array B.  LDB >= max(1,N).     
!> \endverbatim                                                         
!>                                                                      
!> \param[out] WORK                                                     
!> \verbatim                                                            
!>          WORK is REAL array, dimension (N)                           
!> \endverbatim                                                         
!>                                                                      
!> \param[out] INFO                                                     
!> \verbatim                                                            
!>          INFO is INTEGER                                             
!>          = 0:  successful exit                                       
!>          < 0:  if INFO = -i, the i-th argument had an illegal value  
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup realSYcomputational                                         
!                                                                       
!  =====================================================================
      SUBROUTINE SSYTRS2 (UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,    &
      INFO)                                                             
!                                                                       
!  -- LAPACK computational routine (version 3.4.0) --                   
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER UPLO 
      INTEGER INFO, LDA, LDB, N, NRHS 
!     ..                                                                
!     .. Array Arguments ..                                             
      INTEGER IPIV ( * ) 
      REAL A (LDA, * ), B (LDB, * ), WORK ( * ) 
!     ..                                                                
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE 
      PARAMETER (ONE = 1.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      LOGICAL UPPER 
      INTEGER I, IINFO, J, K, KP 
      REAL AK, AKM1, AKM1K, BK, BKM1, DENOM 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
      INFO = 0 
      UPPER = LSAME (UPLO, 'U') 
      IF (.NOT.UPPER.AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = - 1 
      ELSEIF (N.LT.0) THEN 
        INFO = - 2 
      ELSEIF (NRHS.LT.0) THEN 
        INFO = - 3 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = - 5 
      ELSEIF (LDB.LT.MAX (1, N) ) THEN 
        INFO = - 8 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYTRS2', - INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible                                          
!                                                                       
      IF (N.EQ.0.OR.NRHS.EQ.0) RETURN 
!                                                                       
!     Convert A                                                         
!                                                                       
      CALL SSYCONV (UPLO, 'C', N, A, LDA, IPIV, WORK, IINFO) 
!                                                                       
      IF (UPPER) THEN 
!                                                                       
!        Solve A*X = B, where A = U*D*U**T.                             
!                                                                       
!       P**T * B                                                        
        K = N 
        DO WHILE (K.GE.1) 
        IF (IPIV (K) .GT.0) THEN 
!           1 x 1 diagonal block                                        
!           Interchange rows K and IPIV(K).                             
          KP = IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
          K = K - 1 
        ELSE 
!           2 x 2 diagonal block                                        
!           Interchange rows K-1 and -IPIV(K).                          
          KP = - IPIV (K) 
          IF (KP.EQ. - IPIV (K - 1) ) CALL SSWAP (NRHS, B (K - 1, 1),   &
          LDB, B (KP, 1), LDB)                                          
          K = K - 2 
        ENDIF 
        ENDDO 
!                                                                       
!  Compute (U \P**T * B) -> B    [ (U \P**T * B) ]                      
!                                                                       
        CALL STRSM ('L', 'U', 'N', 'U', N, NRHS, ONE, A, LDA, B, LDB) 
!                                                                       
!  Compute D \ B -> B   [ D \ (U \P**T * B) ]                           
!                                                                       
        I = N 
        DO WHILE (I.GE.1) 
        IF (IPIV (I) .GT.0) THEN 
          CALL SSCAL (NRHS, ONE / A (I, I), B (I, 1), LDB) 
        ELSEIF (I.GT.1) THEN 
          IF (IPIV (I - 1) .EQ.IPIV (I) ) THEN 
            AKM1K = WORK (I) 
            AKM1 = A (I - 1, I - 1) / AKM1K 
            AK = A (I, I) / AKM1K 
            DENOM = AKM1 * AK - ONE 
            DO 15 J = 1, NRHS 
              BKM1 = B (I - 1, J) / AKM1K 
              BK = B (I, J) / AKM1K 
              B (I - 1, J) = (AK * BKM1 - BK) / DENOM 
              B (I, J) = (AKM1 * BK - BKM1) / DENOM 
   15       END DO 
            I = I - 1 
          ENDIF 
        ENDIF 
        I = I - 1 
        ENDDO 
!                                                                       
!      Compute (U**T \ B) -> B   [ U**T \ (D \ (U \P**T * B) ) ]        
!                                                                       
        CALL STRSM ('L', 'U', 'T', 'U', N, NRHS, ONE, A, LDA, B, LDB) 
!                                                                       
!       P * B  [ P * (U**T \ (D \ (U \P**T * B) )) ]                    
!                                                                       
        K = 1 
        DO WHILE (K.LE.N) 
        IF (IPIV (K) .GT.0) THEN 
!           1 x 1 diagonal block                                        
!           Interchange rows K and IPIV(K).                             
          KP = IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
          K = K + 1 
        ELSE 
!           2 x 2 diagonal block                                        
!           Interchange rows K-1 and -IPIV(K).                          
          KP = - IPIV (K) 
          IF (K.LT.N.AND.KP.EQ. - IPIV (K + 1) ) CALL SSWAP (NRHS, B (K,&
          1), LDB, B (KP, 1), LDB)                                      
          K = K + 2 
        ENDIF 
        ENDDO 
!                                                                       
      ELSE 
!                                                                       
!        Solve A*X = B, where A = L*D*L**T.                             
!                                                                       
!       P**T * B                                                        
        K = 1 
        DO WHILE (K.LE.N) 
        IF (IPIV (K) .GT.0) THEN 
!           1 x 1 diagonal block                                        
!           Interchange rows K and IPIV(K).                             
          KP = IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
          K = K + 1 
        ELSE 
!           2 x 2 diagonal block                                        
!           Interchange rows K and -IPIV(K+1).                          
          KP = - IPIV (K + 1) 
          IF (KP.EQ. - IPIV (K) ) CALL SSWAP (NRHS, B (K + 1, 1),       &
          LDB, B (KP, 1), LDB)                                          
          K = K + 2 
        ENDIF 
        ENDDO 
!                                                                       
!  Compute (L \P**T * B) -> B    [ (L \P**T * B) ]                      
!                                                                       
        CALL STRSM ('L', 'L', 'N', 'U', N, NRHS, ONE, A, LDA, B, LDB) 
!                                                                       
!  Compute D \ B -> B   [ D \ (L \P**T * B) ]                           
!                                                                       
        I = 1 
        DO WHILE (I.LE.N) 
        IF (IPIV (I) .GT.0) THEN 
          CALL SSCAL (NRHS, ONE / A (I, I), B (I, 1), LDB) 
        ELSE 
          AKM1K = WORK (I) 
          AKM1 = A (I, I) / AKM1K 
          AK = A (I + 1, I + 1) / AKM1K 
          DENOM = AKM1 * AK - ONE 
          DO 25 J = 1, NRHS 
            BKM1 = B (I, J) / AKM1K 
            BK = B (I + 1, J) / AKM1K 
            B (I, J) = (AK * BKM1 - BK) / DENOM 
            B (I + 1, J) = (AKM1 * BK - BKM1) / DENOM 
   25     END DO 
          I = I + 1 
        ENDIF 
        I = I + 1 
        ENDDO 
!                                                                       
!  Compute (L**T \ B) -> B   [ L**T \ (D \ (L \P**T * B) ) ]            
!                                                                       
        CALL STRSM ('L', 'L', 'T', 'U', N, NRHS, ONE, A, LDA, B, LDB) 
!                                                                       
!       P * B  [ P * (L**T \ (D \ (L \P**T * B) )) ]                    
!                                                                       
        K = N 
        DO WHILE (K.GE.1) 
        IF (IPIV (K) .GT.0) THEN 
!           1 x 1 diagonal block                                        
!           Interchange rows K and IPIV(K).                             
          KP = IPIV (K) 
          IF (KP.NE.K) CALL SSWAP (NRHS, B (K, 1), LDB, B (KP, 1),      &
          LDB)                                                          
          K = K - 1 
        ELSE 
!           2 x 2 diagonal block                                        
!           Interchange rows K-1 and -IPIV(K).                          
          KP = - IPIV (K) 
          IF (K.GT.1.AND.KP.EQ. - IPIV (K - 1) ) CALL SSWAP (NRHS, B (K,&
          1), LDB, B (KP, 1), LDB)                                      
          K = K - 2 
        ENDIF 
        ENDDO 
!                                                                       
      ENDIF 
!                                                                       
!     Revert A                                                          
!                                                                       
      CALL SSYCONV (UPLO, 'R', N, A, LDA, IPIV, WORK, IINFO) 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYTRS2                                                    
!                                                                       
      END SUBROUTINE SSYTRS2                        
!> \brief \b XERBLA                                                     
!                                                                       
!  =========== DOCUMENTATION ===========                                
!                                                                       
! Online html documentation available at                                
!            http://www.netlib.org/lapack/explore-html/                 
!                                                                       
!> \htmlonly                                                            
!> Download XERBLA + dependencies                                       
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&fil
!> [TGZ]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&fil
!> [ZIP]</a>                                                            
!> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&fil
!> [TXT]</a>                                                            
!> \endhtmlonly                                                         
!                                                                       
!  Definition:                                                          
!  ===========                                                          
!                                                                       
!       SUBROUTINE XERBLA( SRNAME, INFO )                               
!                                                                       
!       .. Scalar Arguments ..                                          
!       CHARACTER*(*)      SRNAME                                       
!       INTEGER            INFO                                         
!       ..                                                              
!                                                                       
!                                                                       
!> \par Purpose:                                                        
!  =============                                                        
!>                                                                      
!> \verbatim                                                            
!>                                                                      
!> XERBLA  is an error handler for the LAPACK routines.                 
!> It is called by an LAPACK routine if an input parameter has an       
!> invalid value.  A message is printed and execution stops.            
!>                                                                      
!> Installers may consider modifying the STOP statement in order to     
!> call system-specific exception-handling facilities.                  
!> \endverbatim                                                         
!                                                                       
!  Arguments:                                                           
!  ==========                                                           
!                                                                       
!> \param[in] SRNAME                                                    
!> \verbatim                                                            
!>          SRNAME is CHARACTER*(*)                                     
!>          The name of the routine which called XERBLA.                
!> \endverbatim                                                         
!>                                                                      
!> \param[in] INFO                                                      
!> \verbatim                                                            
!>          INFO is INTEGER                                             
!>          The position of the invalid parameter in the parameter list 
!>          of the calling routine.                                     
!> \endverbatim                                                         
!                                                                       
!  Authors:                                                             
!  ========                                                             
!                                                                       
!> \author Univ. of Tennessee                                           
!> \author Univ. of California Berkeley                                 
!> \author Univ. of Colorado Denver                                     
!> \author NAG Ltd.                                                     
!                                                                       
!> \date November 2011                                                  
!                                                                       
!> \ingroup auxOTHERauxiliary                                           
!                                                                       
!  =====================================================================
      SUBROUTINE XERBLA (SRNAME, INFO) 
!                                                                       
!  -- LAPACK auxiliary routine (version 3.4.0) --                       
!  -- LAPACK is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd
!     November 2011                                                     
!                                                                       
!     .. Scalar Arguments ..                                            
      CHARACTER ( * ) SRNAME 
      INTEGER INFO 
!     ..                                                                
!                                                                       
! ===================================================================== 
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC LEN_TRIM 
!     ..                                                                
!     .. Executable Statements ..                                       
!                                                                       
      WRITE ( *, FMT = 9999) SRNAME (1:LEN_TRIM (SRNAME) ), INFO 
!                                                                       
      STOP 
!                                                                       
 9999 FORMAT( ' ** On entry to ', A, ' parameter number ', I2, ' had ', &
     &      'an illegal value' )                                        
!                                                                       
!     End of XERBLA                                                     
!                                                                       
      END SUBROUTINE XERBLA

      SUBROUTINE STRMM (SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,  &
      B, LDB)                                                           
!     .. Scalar Arguments ..                                            
      REAL ALPHA 
      INTEGER LDA, LDB, M, N 
      CHARACTER DIAG, SIDE, TRANSA, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), B (LDB, * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  STRMM  performs one of the matrix-matrix operations                  
!                                                                       
!     B := alpha*op( A )*B,   or   B := alpha*B*op( A ),                
!                                                                       
!  where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!                                                                       
!     op( A ) = A   or   op( A ) = A**T.                                
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  SIDE   - CHARACTER*1.                                                
!           On entry,  SIDE specifies whether  op( A ) multiplies B from
!           the left or right as follows:                               
!                                                                       
!              SIDE = 'L' or 'l'   B := alpha*op( A )*B.                
!                                                                       
!              SIDE = 'R' or 'r'   B := alpha*B*op( A ).                
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:                         
!                                                                       
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.     
!                                                                       
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANSA - CHARACTER*1.                                                
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:                       
!                                                                       
!              TRANSA = 'N' or 'n'   op( A ) = A.                       
!                                                                       
!              TRANSA = 'T' or 't'   op( A ) = A**T.                    
!                                                                       
!              TRANSA = 'C' or 'c'   op( A ) = A**T.                    
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  DIAG   - CHARACTER*1.                                                
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:                                                 
!                                                                       
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.  
!                                                                       
!              DIAG = 'N' or 'n'   A is not assumed to be unit          
!                                  triangular.                          
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  M      - INTEGER.                                                    
!           On entry, M specifies the number of rows of B. M must be at 
!           least zero.                                                 
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.                                              
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.                                                      
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.                                        
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.                                        
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).                      
!           Unchanged on exit.                                          
!                                                                       
!  B      - REAL             array of DIMENSION ( LDB, n ).             
!           Before entry,  the leading  m by n part of the array  B must
!           contain the matrix  B,  and  on exit  is overwritten  by the
!           transformed matrix.                                         
!                                                                       
!  LDB    - INTEGER.                                                    
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 3 Blas routine.                                                
!                                                                       
!  -- Written on 8-February-1989.                                       
!     Jack Dongarra, Argonne National Laboratory.                       
!     Iain Duff, AERE Harwell.                                          
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.                   
!     Sven Hammarling, Numerical Algorithms Group Ltd.                  
!                                                                       
!  =====================================================================
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, J, K, NROWA 
      LOGICAL LSIDE, NOUNIT, UPPER 
!     ..                                                                
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      LSIDE = LSAME (SIDE, 'L') 
      IF (LSIDE) THEN 
        NROWA = M 
      ELSE 
        NROWA = N 
      ENDIF 
      NOUNIT = LSAME (DIAG, 'N') 
      UPPER = LSAME (UPLO, 'U') 
!                                                                       
      INFO = 0 
      IF ( (.NOT.LSIDE) .AND. (.NOT.LSAME (SIDE, 'R') ) ) THEN 
        INFO = 1 
      ELSEIF ( (.NOT.UPPER) .AND. (.NOT.LSAME (UPLO, 'L') ) ) THEN 
        INFO = 2 
      ELSEIF ( (.NOT.LSAME (TRANSA, 'N') ) .AND. (.NOT.LSAME (TRANSA,   &
      'T') ) .AND. (.NOT.LSAME (TRANSA, 'C') ) ) THEN                   
        INFO = 3 
      ELSEIF ( (.NOT.LSAME (DIAG, 'U') ) .AND. (.NOT.LSAME (DIAG, 'N') )&
      ) THEN                                                            
        INFO = 4 
      ELSEIF (M.LT.0) THEN 
        INFO = 5 
      ELSEIF (N.LT.0) THEN 
        INFO = 6 
      ELSEIF (LDA.LT.MAX (1, NROWA) ) THEN 
        INFO = 9 
      ELSEIF (LDB.LT.MAX (1, M) ) THEN 
        INFO = 11 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('STRMM ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF (M.EQ.0.OR.N.EQ.0) RETURN 
!                                                                       
!     And when  alpha.eq.zero.                                          
!                                                                       
      IF (ALPHA.EQ.ZERO) THEN 
        DO 20 J = 1, N 
          DO 10 I = 1, M 
            B (I, J) = ZERO 
   10     END DO 
   20   END DO 
        RETURN 
      ENDIF 
!                                                                       
!     Start the operations.                                             
!                                                                       
      IF (LSIDE) THEN 
        IF (LSAME (TRANSA, 'N') ) THEN 
!                                                                       
!           Form  B := alpha*A*B.                                       
!                                                                       
          IF (UPPER) THEN 
            DO 50 J = 1, N 
              DO 40 K = 1, M 
                IF (B (K, J) .NE.ZERO) THEN 
                  TEMP = ALPHA * B (K, J) 
                  DO 30 I = 1, K - 1 
                    B (I, J) = B (I, J) + TEMP * A (I, K) 
   30             END DO 
                  IF (NOUNIT) TEMP = TEMP * A (K, K) 
                  B (K, J) = TEMP 
                ENDIF 
   40         END DO 
   50       END DO 
          ELSE 
            DO 80 J = 1, N 
              DO 70 K = M, 1, - 1 
                IF (B (K, J) .NE.ZERO) THEN 
                  TEMP = ALPHA * B (K, J) 
                  B (K, J) = TEMP 
                  IF (NOUNIT) B (K, J) = B (K, J) * A (K, K) 
                  DO 60 I = K + 1, M 
                    B (I, J) = B (I, J) + TEMP * A (I, K) 
   60             END DO 
                ENDIF 
   70         END DO 
   80       END DO 
          ENDIF 
        ELSE 
!                                                                       
!           Form  B := alpha*A**T*B.                                    
!                                                                       
          IF (UPPER) THEN 
            DO 110 J = 1, N 
              DO 100 I = M, 1, - 1 
                TEMP = B (I, J) 
                IF (NOUNIT) TEMP = TEMP * A (I, I) 
                DO 90 K = 1, I - 1 
                  TEMP = TEMP + A (K, I) * B (K, J) 
   90           END DO 
                B (I, J) = ALPHA * TEMP 
  100         END DO 
  110       END DO 
          ELSE 
            DO 140 J = 1, N 
              DO 130 I = 1, M 
                TEMP = B (I, J) 
                IF (NOUNIT) TEMP = TEMP * A (I, I) 
                DO 120 K = I + 1, M 
                  TEMP = TEMP + A (K, I) * B (K, J) 
  120           END DO 
                B (I, J) = ALPHA * TEMP 
  130         END DO 
  140       END DO 
          ENDIF 
        ENDIF 
      ELSE 
        IF (LSAME (TRANSA, 'N') ) THEN 
!                                                                       
!           Form  B := alpha*B*A.                                       
!                                                                       
          IF (UPPER) THEN 
            DO 180 J = N, 1, - 1 
              TEMP = ALPHA 
              IF (NOUNIT) TEMP = TEMP * A (J, J) 
              DO 150 I = 1, M 
                B (I, J) = TEMP * B (I, J) 
  150         END DO 
              DO 170 K = 1, J - 1 
                IF (A (K, J) .NE.ZERO) THEN 
                  TEMP = ALPHA * A (K, J) 
                  DO 160 I = 1, M 
                    B (I, J) = B (I, J) + TEMP * B (I, K) 
  160             END DO 
                ENDIF 
  170         END DO 
  180       END DO 
          ELSE 
            DO 220 J = 1, N 
              TEMP = ALPHA 
              IF (NOUNIT) TEMP = TEMP * A (J, J) 
              DO 190 I = 1, M 
                B (I, J) = TEMP * B (I, J) 
  190         END DO 
              DO 210 K = J + 1, N 
                IF (A (K, J) .NE.ZERO) THEN 
                  TEMP = ALPHA * A (K, J) 
                  DO 200 I = 1, M 
                    B (I, J) = B (I, J) + TEMP * B (I, K) 
  200             END DO 
                ENDIF 
  210         END DO 
  220       END DO 
          ENDIF 
        ELSE 
!                                                                       
!           Form  B := alpha*B*A**T.                                    
!                                                                       
          IF (UPPER) THEN 
            DO 260 K = 1, N 
              DO 240 J = 1, K - 1 
                IF (A (J, K) .NE.ZERO) THEN 
                  TEMP = ALPHA * A (J, K) 
                  DO 230 I = 1, M 
                    B (I, J) = B (I, J) + TEMP * B (I, K) 
  230             END DO 
                ENDIF 
  240         END DO 
              TEMP = ALPHA 
              IF (NOUNIT) TEMP = TEMP * A (K, K) 
              IF (TEMP.NE.ONE) THEN 
                DO 250 I = 1, M 
                  B (I, K) = TEMP * B (I, K) 
  250           END DO 
              ENDIF 
  260       END DO 
          ELSE 
            DO 300 K = N, 1, - 1 
              DO 280 J = K + 1, N 
                IF (A (J, K) .NE.ZERO) THEN 
                  TEMP = ALPHA * A (J, K) 
                  DO 270 I = 1, M 
                    B (I, J) = B (I, J) + TEMP * B (I, K) 
  270             END DO 
                ENDIF 
  280         END DO 
              TEMP = ALPHA 
              IF (NOUNIT) TEMP = TEMP * A (K, K) 
              IF (TEMP.NE.ONE) THEN 
                DO 290 I = 1, M 
                  B (I, K) = TEMP * B (I, K) 
  290           END DO 
              ENDIF 
  300       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of STRMM .                                                    
!                                                                       
      END SUBROUTINE STRMM                          
      SUBROUTINE SGBMV (TRANS, M, N, KL, KU, ALPHA, A, LDA, X, INCX,    &
      BETA, Y, INCY)                                                    
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER INCX, INCY, KL, KU, LDA, M, N 
      CHARACTER TRANS 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ), Y ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SGBMV  performs one of the matrix-vector operations                  
!                                                                       
!     y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,       
!                                                                       
!  where alpha and beta are scalars, x and y are vectors and A is an    
!  m by n band matrix, with kl sub-diagonals and ku super-diagonals.    
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry, TRANS specifies the operation to be performed as  
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.            
!                                                                       
!              TRANS = 'T' or 't'   y := alpha*A**T*x + beta*y.         
!                                                                       
!              TRANS = 'C' or 'c'   y := alpha*A**T*x + beta*y.         
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  M      - INTEGER.                                                    
!           On entry, M specifies the number of rows of the matrix A.   
!           M must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  KL     - INTEGER.                                                    
!           On entry, KL specifies the number of sub-diagonals of the   
!           matrix A. KL must satisfy  0 .le. KL.                       
!           Unchanged on exit.                                          
!                                                                       
!  KU     - INTEGER.                                                    
!           On entry, KU specifies the number of super-diagonals of the 
!           matrix A. KU must satisfy  0 .le. KU.                       
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry, the leading ( kl + ku + 1 ) by n part of the  
!           array A must contain the matrix of coefficients, supplied   
!           column by column, with the leading diagonal of the matrix in
!           row ( ku + 1 ) of the array, the first super-diagonal       
!           starting at position 2 in row ku, the first sub-diagonal    
!           starting at position 1 in row ( ku + 2 ), and so on.        
!           Elements in the array A that do not correspond to elements  
!           in the band matrix (such as the top left ku by ku triangle) 
!           are not referenced.                                         
!           The following program segment will transfer a band matrix   
!           from conventional full matrix storage to band storage:      
!                                                                       
!                 DO 20, J = 1, N                                       
!                    K = KU + 1 - J                                     
!                    DO 10, I = MAX( 1, J - KU ), MIN( M, J + KL )      
!                       A( K + I, J ) = matrix( I, J )                  
!              10    CONTINUE                                           
!              20 CONTINUE                                              
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           ( kl + ku + 1 ).                                            
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of DIMENSION at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'       
!           and at least                                                
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.                    
!           Before entry, the incremented array X must contain the      
!           vector x.                                                   
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry, BETA specifies the scalar beta. When BETA is      
!           supplied as zero then Y need not be set on input.           
!           Unchanged on exit.                                          
!                                                                       
!  Y      - REAL             array of DIMENSION at least                
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'       
!           and at least                                                
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.                    
!           Before entry, the incremented array Y must contain the      
!           vector y. On exit, Y is overwritten by the updated vector y.
!                                                                       
!  INCY   - INTEGER.                                                    
!           On entry, INCY specifies the increment for the elements of  
!           Y. INCY must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!  The vector and matrix arguments are not referenced when N = 0, or M =
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, IY, J, JX, JY, K, KUP1, KX, KY, LENX, LENY 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX, MIN 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (TRANS, 'N') .AND..NOT.LSAME (TRANS, 'T')          &
      .AND..NOT.LSAME (TRANS, 'C') ) THEN                               
        INFO = 1 
      ELSEIF (M.LT.0) THEN 
        INFO = 2 
      ELSEIF (N.LT.0) THEN 
        INFO = 3 
      ELSEIF (KL.LT.0) THEN 
        INFO = 4 
      ELSEIF (KU.LT.0) THEN 
        INFO = 5 
      ELSEIF (LDA.LT. (KL + KU + 1) ) THEN 
        INFO = 8 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 10 
      ELSEIF (INCY.EQ.0) THEN 
        INFO = 13 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SGBMV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (M.EQ.0) .OR. (N.EQ.0) .OR. ( (ALPHA.EQ.ZERO) .AND. (        &
      BETA.EQ.ONE) ) ) RETURN                                           
!                                                                       
!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set 
!     up the start points in  X  and  Y.                                
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
        LENX = N 
        LENY = M 
      ELSE 
        LENX = M 
        LENY = N 
      ENDIF 
      IF (INCX.GT.0) THEN 
        KX = 1 
      ELSE 
        KX = 1 - (LENX - 1) * INCX 
      ENDIF 
      IF (INCY.GT.0) THEN 
        KY = 1 
      ELSE 
        KY = 1 - (LENY - 1) * INCY 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through the band part of A.   
!                                                                       
!     First form  y := beta*y.                                          
!                                                                       
      IF (BETA.NE.ONE) THEN 
        IF (INCY.EQ.1) THEN 
          IF (BETA.EQ.ZERO) THEN 
            DO 10 I = 1, LENY 
              Y (I) = ZERO 
   10       END DO 
          ELSE 
            DO 20 I = 1, LENY 
              Y (I) = BETA * Y (I) 
   20       END DO 
          ENDIF 
        ELSE 
          IY = KY 
          IF (BETA.EQ.ZERO) THEN 
            DO 30 I = 1, LENY 
              Y (IY) = ZERO 
              IY = IY + INCY 
   30       END DO 
          ELSE 
            DO 40 I = 1, LENY 
              Y (IY) = BETA * Y (IY) 
              IY = IY + INCY 
   40       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
      IF (ALPHA.EQ.ZERO) RETURN 
      KUP1 = KU + 1 
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  y := alpha*A*x + y.                                      
!                                                                       
        JX = KX 
        IF (INCY.EQ.1) THEN 
          DO 60 J = 1, N 
            IF (X (JX) .NE.ZERO) THEN 
              TEMP = ALPHA * X (JX) 
              K = KUP1 - J 
              DO 50 I = MAX (1, J - KU), MIN (M, J + KL) 
                Y (I) = Y (I) + TEMP * A (K + I, J) 
   50         END DO 
            ENDIF 
            JX = JX + INCX 
   60     END DO 
        ELSE 
          DO 80 J = 1, N 
            IF (X (JX) .NE.ZERO) THEN 
              TEMP = ALPHA * X (JX) 
              IY = KY 
              K = KUP1 - J 
              DO 70 I = MAX (1, J - KU), MIN (M, J + KL) 
                Y (IY) = Y (IY) + TEMP * A (K + I, J) 
                IY = IY + INCY 
   70         END DO 
            ENDIF 
            JX = JX + INCX 
            IF (J.GT.KU) KY = KY + INCY 
   80     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  y := alpha*A**T*x + y.                                   
!                                                                       
        JY = KY 
        IF (INCX.EQ.1) THEN 
          DO 100 J = 1, N 
            TEMP = ZERO 
            K = KUP1 - J 
            DO 90 I = MAX (1, J - KU), MIN (M, J + KL) 
              TEMP = TEMP + A (K + I, J) * X (I) 
   90       END DO 
            Y (JY) = Y (JY) + ALPHA * TEMP 
            JY = JY + INCY 
  100     END DO 
        ELSE 
          DO 120 J = 1, N 
            TEMP = ZERO 
            IX = KX 
            K = KUP1 - J 
            DO 110 I = MAX (1, J - KU), MIN (M, J + KL) 
              TEMP = TEMP + A (K + I, J) * X (IX) 
              IX = IX + INCX 
  110       END DO 
            Y (JY) = Y (JY) + ALPHA * TEMP 
            JY = JY + INCY 
            IF (J.GT.KU) KX = KX + INCX 
  120     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SGBMV .                                                    
!                                                                       
      END SUBROUTINE SGBMV                          
      REAL FUNCTION SCASUM (N, CX, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      COMPLEX CX ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     SCASUM takes the sum of the absolute values of a complex vector an
!     returns a single precision result.                                
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 3/93 to return if incx .le. 0.                           
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL STEMP 
      INTEGER I, NINCX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, AIMAG, REAL 
!     ..                                                                
      SCASUM = 0.0e0 
      STEMP = 0.0e0 
      IF (N.LE.0.OR.INCX.LE.0) RETURN 
      IF (INCX.EQ.1) THEN 
!                                                                       
!        code for increment equal to 1                                  
!                                                                       
        DO I = 1, N 
        STEMP = STEMP + ABS (REAL (CX (I) ) ) + ABS (AIMAG (CX (I) ) ) 
        ENDDO 
      ELSE 
!                                                                       
!        code for increment not equal to 1                              
!                                                                       
        NINCX = N * INCX 
        DO I = 1, NINCX, INCX 
        STEMP = STEMP + ABS (REAL (CX (I) ) ) + ABS (AIMAG (CX (I) ) ) 
        ENDDO 
      ENDIF 
      SCASUM = STEMP 
      RETURN 
      END FUNCTION SCASUM                           
      SUBROUTINE SSYR2 (UPLO, N, ALPHA, X, INCX, Y, INCY, A, LDA) 
!     .. Scalar Arguments ..                                            
      REAL ALPHA 
      INTEGER INCX, INCY, LDA, N 
      CHARACTER UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ), Y ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSYR2  performs the symmetric rank 2 operation                       
!                                                                       
!     A := alpha*x*y**T + alpha*y*x**T + A,                             
!                                                                       
!  where alpha is a scalar, x and y are n element vectors and A is an n 
!  by n symmetric matrix.                                               
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the upper or lower         
!           triangular part of the array A is to be referenced as       
!           follows:                                                    
!                                                                       
!              UPLO = 'U' or 'u'   Only the upper triangular part of A  
!                                  is to be referenced.                 
!                                                                       
!              UPLO = 'L' or 'l'   Only the lower triangular part of A  
!                                  is to be referenced.                 
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Y      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCY ) ).                              
!           Before entry, the incremented array Y must contain the n    
!           element vector y.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCY   - INTEGER.                                                    
!           On entry, INCY specifies the increment for the elements of  
!           Y. INCY must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry with  UPLO = 'U' or 'u', the leading n by n    
!           upper triangular part of the array A must contain the upper 
!           triangular part of the symmetric matrix and the strictly    
!           lower triangular part of A is not referenced. On exit, the  
!           upper triangular part of the array A is overwritten by the  
!           upper triangular part of the updated matrix.                
!           Before entry with UPLO = 'L' or 'l', the leading n by n     
!           lower triangular part of the array A must contain the lower 
!           triangular part of the symmetric matrix and the strictly    
!           upper triangular part of A is not referenced. On exit, the  
!           lower triangular part of the array A is overwritten by the  
!           lower triangular part of the updated matrix.                
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           max( 1, n ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP1, TEMP2 
      INTEGER I, INFO, IX, IY, J, JX, JY, KX, KY 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (N.LT.0) THEN 
        INFO = 2 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 5 
      ELSEIF (INCY.EQ.0) THEN 
        INFO = 7 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = 9 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYR2 ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. (ALPHA.EQ.ZERO) ) RETURN 
!                                                                       
!     Set up the start points in X and Y if the increments are not both 
!     unity.                                                            
!                                                                       
      IF ( (INCX.NE.1) .OR. (INCY.NE.1) ) THEN 
        IF (INCX.GT.0) THEN 
          KX = 1 
        ELSE 
          KX = 1 - (N - 1) * INCX 
        ENDIF 
        IF (INCY.GT.0) THEN 
          KY = 1 
        ELSE 
          KY = 1 - (N - 1) * INCY 
        ENDIF 
        JX = KX 
        JY = KY 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through the triangular part   
!     of A.                                                             
!                                                                       
      IF (LSAME (UPLO, 'U') ) THEN 
!                                                                       
!        Form  A  when A is stored in the upper triangle.               
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 20 J = 1, N 
            IF ( (X (J) .NE.ZERO) .OR. (Y (J) .NE.ZERO) ) THEN 
              TEMP1 = ALPHA * Y (J) 
              TEMP2 = ALPHA * X (J) 
              DO 10 I = 1, J 
                A (I, J) = A (I, J) + X (I) * TEMP1 + Y (I) * TEMP2 
   10         END DO 
            ENDIF 
   20     END DO 
        ELSE 
          DO 40 J = 1, N 
            IF ( (X (JX) .NE.ZERO) .OR. (Y (JY) .NE.ZERO) ) THEN 
              TEMP1 = ALPHA * Y (JY) 
              TEMP2 = ALPHA * X (JX) 
              IX = KX 
              IY = KY 
              DO 30 I = 1, J 
                A (I, J) = A (I, J) + X (IX) * TEMP1 + Y (IY) * TEMP2 
                IX = IX + INCX 
                IY = IY + INCY 
   30         END DO 
            ENDIF 
            JX = JX + INCX 
            JY = JY + INCY 
   40     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  A  when A is stored in the lower triangle.               
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 60 J = 1, N 
            IF ( (X (J) .NE.ZERO) .OR. (Y (J) .NE.ZERO) ) THEN 
              TEMP1 = ALPHA * Y (J) 
              TEMP2 = ALPHA * X (J) 
              DO 50 I = J, N 
                A (I, J) = A (I, J) + X (I) * TEMP1 + Y (I) * TEMP2 
   50         END DO 
            ENDIF 
   60     END DO 
        ELSE 
          DO 80 J = 1, N 
            IF ( (X (JX) .NE.ZERO) .OR. (Y (JY) .NE.ZERO) ) THEN 
              TEMP1 = ALPHA * Y (JY) 
              TEMP2 = ALPHA * X (JX) 
              IX = JX 
              IY = JY 
              DO 70 I = J, N 
                A (I, J) = A (I, J) + X (IX) * TEMP1 + Y (IY) * TEMP2 
                IX = IX + INCX 
                IY = IY + INCY 
   70         END DO 
            ENDIF 
            JX = JX + INCX 
            JY = JY + INCY 
   80     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYR2 .                                                    
!                                                                       
      END SUBROUTINE SSYR2                          
      SUBROUTINE SROTM (N, SX, INCX, SY, INCY, SPARAM) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, INCY, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SPARAM (5), SX ( * ), SY ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX 
!                                                                       
!     (SX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF SX ARE IN
!     (SX**T)                                                           
!                                                                       
!     SX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE    
!     LX = (-INCX)*N, AND SIMILARLY FOR SY USING USING LY AND INCY.     
!     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..          
!                                                                       
!     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0      
!                                                                       
!       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)    
!     H=(          )    (          )    (          )    (          )    
!       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).   
!     SEE  SROTMG FOR A DESCRIPTION OF DATA STORAGE IN SPARAM.          
!                                                                       
!                                                                       
!  Arguments                                                            
!  =========                                                            
!                                                                       
!  N      (input) INTEGER                                               
!         number of elements in input vector(s)                         
!                                                                       
!  SX     (input/output) REAL array, dimension N                        
!         double precision vector with N elements                       
!                                                                       
!  INCX   (input) INTEGER                                               
!         storage spacing between elements of SX                        
!                                                                       
!  SY     (input/output) REAL array, dimension N                        
!         double precision vector with N elements                       
!                                                                       
!  INCY   (input) INTEGER                                               
!         storage spacing between elements of SY                        
!                                                                       
!  SPARAM (input/output)  REAL array, dimension 5                       
!     SPARAM(1)=SFLAG                                                   
!     SPARAM(2)=SH11                                                    
!     SPARAM(3)=SH21                                                    
!     SPARAM(4)=SH12                                                    
!     SPARAM(5)=SH22                                                    
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL SFLAG, SH11, SH12, SH21, SH22, TWO, W, Z, ZERO 
      INTEGER I, KX, KY, NSTEPS 
!     ..                                                                
!     .. Data statements ..                                             
      DATA ZERO, TWO / 0.E0, 2.E0 / 
!     ..                                                                
!                                                                       
      SFLAG = SPARAM (1) 
      IF (N.LE.0.OR. (SFLAG + TWO.EQ.ZERO) ) RETURN 
      IF (INCX.EQ.INCY.AND.INCX.GT.0) THEN 
!                                                                       
        NSTEPS = N * INCX 
        IF (SFLAG.LT.ZERO) THEN 
          SH11 = SPARAM (2) 
          SH12 = SPARAM (4) 
          SH21 = SPARAM (3) 
          SH22 = SPARAM (5) 
          DO I = 1, NSTEPS, INCX 
          W = SX (I) 
          Z = SY (I) 
          SX (I) = W * SH11 + Z * SH12 
          SY (I) = W * SH21 + Z * SH22 
          ENDDO 
        ELSEIF (SFLAG.EQ.ZERO) THEN 
          SH12 = SPARAM (4) 
          SH21 = SPARAM (3) 
          DO I = 1, NSTEPS, INCX 
          W = SX (I) 
          Z = SY (I) 
          SX (I) = W + Z * SH12 
          SY (I) = W * SH21 + Z 
          ENDDO 
        ELSE 
          SH11 = SPARAM (2) 
          SH22 = SPARAM (5) 
          DO I = 1, NSTEPS, INCX 
          W = SX (I) 
          Z = SY (I) 
          SX (I) = W * SH11 + Z 
          SY (I) = - W + SH22 * Z 
          ENDDO 
        ENDIF 
      ELSE 
        KX = 1 
        KY = 1 
        IF (INCX.LT.0) KX = 1 + (1 - N) * INCX 
        IF (INCY.LT.0) KY = 1 + (1 - N) * INCY 
!                                                                       
        IF (SFLAG.LT.ZERO) THEN 
          SH11 = SPARAM (2) 
          SH12 = SPARAM (4) 
          SH21 = SPARAM (3) 
          SH22 = SPARAM (5) 
          DO I = 1, N 
          W = SX (KX) 
          Z = SY (KY) 
          SX (KX) = W * SH11 + Z * SH12 
          SY (KY) = W * SH21 + Z * SH22 
          KX = KX + INCX 
          KY = KY + INCY 
          ENDDO 
        ELSEIF (SFLAG.EQ.ZERO) THEN 
          SH12 = SPARAM (4) 
          SH21 = SPARAM (3) 
          DO I = 1, N 
          W = SX (KX) 
          Z = SY (KY) 
          SX (KX) = W + Z * SH12 
          SY (KY) = W * SH21 + Z 
          KX = KX + INCX 
          KY = KY + INCY 
          ENDDO 
        ELSE 
          SH11 = SPARAM (2) 
          SH22 = SPARAM (5) 
          DO I = 1, N 
          W = SX (KX) 
          Z = SY (KY) 
          SX (KX) = W * SH11 + Z 
          SY (KY) = - W + SH22 * Z 
          KX = KX + INCX 
          KY = KY + INCY 
          ENDDO 
        ENDIF 
      ENDIF 
      RETURN 
      END SUBROUTINE SROTM                          
      SUBROUTINE SSPMV (UPLO, N, ALPHA, AP, X, INCX, BETA, Y, INCY) 
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER INCX, INCY, N 
      CHARACTER UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL AP ( * ), X ( * ), Y ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSPMV  performs the matrix-vector operation                          
!                                                                       
!     y := alpha*A*x + beta*y,                                          
!                                                                       
!  where alpha and beta are scalars, x and y are n element vectors and  
!  A is an n by n symmetric matrix, supplied in packed form.            
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the upper or lower         
!           triangular part of the matrix A is supplied in the packed   
!           array AP as follows:                                        
!                                                                       
!              UPLO = 'U' or 'u'   The upper triangular part of A is    
!                                  supplied in AP.                      
!                                                                       
!              UPLO = 'L' or 'l'   The lower triangular part of A is    
!                                  supplied in AP.                      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  AP     - REAL             array of DIMENSION at least                
!           ( ( n*( n + 1 ) )/2 ).                                      
!           Before entry with UPLO = 'U' or 'u', the array AP must      
!           contain the upper triangular part of the symmetric matrix   
!           packed sequentially, column by column, so that AP( 1 )      
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )   
!           and a( 2, 2 ) respectively, and so on.                      
!           Before entry with UPLO = 'L' or 'l', the array AP must      
!           contain the lower triangular part of the symmetric matrix   
!           packed sequentially, column by column, so that AP( 1 )      
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )   
!           and a( 3, 1 ) respectively, and so on.                      
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry, BETA specifies the scalar beta. When BETA is      
!           supplied as zero then Y need not be set on input.           
!           Unchanged on exit.                                          
!                                                                       
!  Y      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCY ) ).                              
!           Before entry, the incremented array Y must contain the n    
!           element vector y. On exit, Y is overwritten by the updated  
!           vector y.                                                   
!                                                                       
!  INCY   - INTEGER.                                                    
!           On entry, INCY specifies the increment for the elements of  
!           Y. INCY must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!  The vector and matrix arguments are not referenced when N = 0, or M =
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP1, TEMP2 
      INTEGER I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY 
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (N.LT.0) THEN 
        INFO = 2 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 6 
      ELSEIF (INCY.EQ.0) THEN 
        INFO = 9 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSPMV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. ( (ALPHA.EQ.ZERO) .AND. (BETA.EQ.ONE) ) )      &
      RETURN                                                            
!                                                                       
!     Set up the start points in  X  and  Y.                            
!                                                                       
      IF (INCX.GT.0) THEN 
        KX = 1 
      ELSE 
        KX = 1 - (N - 1) * INCX 
      ENDIF 
      IF (INCY.GT.0) THEN 
        KY = 1 
      ELSE 
        KY = 1 - (N - 1) * INCY 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of the array AP
!     are accessed sequentially with one pass through AP.               
!                                                                       
!     First form  y := beta*y.                                          
!                                                                       
      IF (BETA.NE.ONE) THEN 
        IF (INCY.EQ.1) THEN 
          IF (BETA.EQ.ZERO) THEN 
            DO 10 I = 1, N 
              Y (I) = ZERO 
   10       END DO 
          ELSE 
            DO 20 I = 1, N 
              Y (I) = BETA * Y (I) 
   20       END DO 
          ENDIF 
        ELSE 
          IY = KY 
          IF (BETA.EQ.ZERO) THEN 
            DO 30 I = 1, N 
              Y (IY) = ZERO 
              IY = IY + INCY 
   30       END DO 
          ELSE 
            DO 40 I = 1, N 
              Y (IY) = BETA * Y (IY) 
              IY = IY + INCY 
   40       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
      IF (ALPHA.EQ.ZERO) RETURN 
      KK = 1 
      IF (LSAME (UPLO, 'U') ) THEN 
!                                                                       
!        Form  y  when AP contains the upper triangle.                  
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 60 J = 1, N 
            TEMP1 = ALPHA * X (J) 
            TEMP2 = ZERO 
            K = KK 
            DO 50 I = 1, J - 1 
              Y (I) = Y (I) + TEMP1 * AP (K) 
              TEMP2 = TEMP2 + AP (K) * X (I) 
              K = K + 1 
   50       END DO 
            Y (J) = Y (J) + TEMP1 * AP (KK + J - 1) + ALPHA * TEMP2 
            KK = KK + J 
   60     END DO 
        ELSE 
          JX = KX 
          JY = KY 
          DO 80 J = 1, N 
            TEMP1 = ALPHA * X (JX) 
            TEMP2 = ZERO 
            IX = KX 
            IY = KY 
            DO 70 K = KK, KK + J - 2 
              Y (IY) = Y (IY) + TEMP1 * AP (K) 
              TEMP2 = TEMP2 + AP (K) * X (IX) 
              IX = IX + INCX 
              IY = IY + INCY 
   70       END DO 
            Y (JY) = Y (JY) + TEMP1 * AP (KK + J - 1) + ALPHA * TEMP2 
            JX = JX + INCX 
            JY = JY + INCY 
            KK = KK + J 
   80     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  y  when AP contains the lower triangle.                  
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 100 J = 1, N 
            TEMP1 = ALPHA * X (J) 
            TEMP2 = ZERO 
            Y (J) = Y (J) + TEMP1 * AP (KK) 
            K = KK + 1 
            DO 90 I = J + 1, N 
              Y (I) = Y (I) + TEMP1 * AP (K) 
              TEMP2 = TEMP2 + AP (K) * X (I) 
              K = K + 1 
   90       END DO 
            Y (J) = Y (J) + ALPHA * TEMP2 
            KK = KK + (N - J + 1) 
  100     END DO 
        ELSE 
          JX = KX 
          JY = KY 
          DO 120 J = 1, N 
            TEMP1 = ALPHA * X (JX) 
            TEMP2 = ZERO 
            Y (JY) = Y (JY) + TEMP1 * AP (KK) 
            IX = JX 
            IY = JY 
            DO 110 K = KK + 1, KK + N - J 
              IX = IX + INCX 
              IY = IY + INCY 
              Y (IY) = Y (IY) + TEMP1 * AP (K) 
              TEMP2 = TEMP2 + AP (K) * X (IX) 
  110       END DO 
            Y (JY) = Y (JY) + ALPHA * TEMP2 
            JX = JX + INCX 
            JY = JY + INCY 
            KK = KK + (N - J + 1) 
  120     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSPMV .                                                    
!                                                                       
      END SUBROUTINE SSPMV                          
      SUBROUTINE SAXPY (N, SA, SX, INCX, SY, INCY) 
!     .. Scalar Arguments ..                                            
      REAL SA 
      INTEGER INCX, INCY, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ), SY ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     SAXPY constant times a vector plus a vector.                      
!     uses unrolled loops for increments equal to one.                  
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      INTEGER I, IX, IY, M, MP1 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MOD 
!     ..                                                                
      IF (N.LE.0) RETURN 
      IF (SA.EQ.0.0) RETURN 
      IF (INCX.EQ.1.AND.INCY.EQ.1) THEN 
!                                                                       
!        code for both increments equal to 1                            
!                                                                       
!                                                                       
!        clean-up loop                                                  
!                                                                       
        M = MOD (N, 4) 
        IF (M.NE.0) THEN 
          DO I = 1, M 
          SY (I) = SY (I) + SA * SX (I) 
          ENDDO 
        ENDIF 
        IF (N.LT.4) RETURN 
        MP1 = M + 1 
        DO I = MP1, N, 4 
        SY (I) = SY (I) + SA * SX (I) 
        SY (I + 1) = SY (I + 1) + SA * SX (I + 1) 
        SY (I + 2) = SY (I + 2) + SA * SX (I + 2) 
        SY (I + 3) = SY (I + 3) + SA * SX (I + 3) 
        ENDDO 
      ELSE 
!                                                                       
!        code for unequal increments or equal increments                
!          not equal to 1                                               
!                                                                       
        IX = 1 
        IY = 1 
        IF (INCX.LT.0) IX = ( - N + 1) * INCX + 1 
        IF (INCY.LT.0) IY = ( - N + 1) * INCY + 1 
        DO I = 1, N 
        SY (IY) = SY (IY) + SA * SX (IX) 
        IX = IX + INCX 
        IY = IY + INCY 
        ENDDO 
      ENDIF 
      RETURN 
      END SUBROUTINE SAXPY                          
      SUBROUTINE SGEMM (TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, &
      BETA, C, LDC)                                                     
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER K, LDA, LDB, LDC, M, N 
      CHARACTER TRANSA, TRANSB 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), B (LDB, * ), C (LDC, * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SGEMM  performs one of the matrix-matrix operations                  
!                                                                       
!     C := alpha*op( A )*op( B ) + beta*C,                              
!                                                                       
!  where  op( X ) is one of                                             
!                                                                       
!     op( X ) = X   or   op( X ) = X**T,                                
!                                                                       
!  alpha and beta are scalars, and A, B and C are matrices, with op( A )
!  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  TRANSA - CHARACTER*1.                                                
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:                       
!                                                                       
!              TRANSA = 'N' or 'n',  op( A ) = A.                       
!                                                                       
!              TRANSA = 'T' or 't',  op( A ) = A**T.                    
!                                                                       
!              TRANSA = 'C' or 'c',  op( A ) = A**T.                    
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANSB - CHARACTER*1.                                                
!           On entry, TRANSB specifies the form of op( B ) to be used in
!           the matrix multiplication as follows:                       
!                                                                       
!              TRANSB = 'N' or 'n',  op( B ) = B.                       
!                                                                       
!              TRANSB = 'T' or 't',  op( B ) = B**T.                    
!                                                                       
!              TRANSB = 'C' or 'c',  op( B ) = B**T.                    
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  M      - INTEGER.                                                    
!           On entry,  M  specifies  the number  of rows  of the  matrix
!           op( A )  and of the  matrix  C.  M  must  be at least  zero.
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry,  N  specifies the number  of columns of the matrix
!           op( B ) and the number of columns of the matrix C. N must be
!           at least zero.                                              
!           Unchanged on exit.                                          
!                                                                       
!  K      - INTEGER.                                                    
!           On entry,  K  specifies  the number of columns of the matrix
!           op( A ) and the number of rows of the matrix op( B ). K must
!           be at least  zero.                                          
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.        
!           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by m  part of the array  A  must contain  the
!           matrix A.                                                   
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, k ).                                         
!           Unchanged on exit.                                          
!                                                                       
!  B      - REAL             array of DIMENSION ( LDB, kb ), where kb is
!           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.        
!           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  n by k  part of the array  B  must contain  the
!           matrix B.                                                   
!           Unchanged on exit.                                          
!                                                                       
!  LDB    - INTEGER.                                                    
!           On entry, LDB specifies the first dimension of B as declared
!           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
!           LDB must be at least  max( 1, k ), otherwise  LDB must be at
!           least  max( 1, n ).                                         
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.           
!           Unchanged on exit.                                          
!                                                                       
!  C      - REAL             array of DIMENSION ( LDC, n ).             
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.                            
!           On exit, the array  C  is overwritten by the  m by n  matrix
!           ( alpha*op( A )*op( B ) + beta*C ).                         
!                                                                       
!  LDC    - INTEGER.                                                    
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 3 Blas routine.                                                
!                                                                       
!  -- Written on 8-February-1989.                                       
!     Jack Dongarra, Argonne National Laboratory.                       
!     Iain Duff, AERE Harwell.                                          
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.                   
!     Sven Hammarling, Numerical Algorithms Group Ltd.                  
!                                                                       
!  =====================================================================
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, J, L, NCOLA, NROWA, NROWB 
      LOGICAL NOTA, NOTB 
!     ..                                                                
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!                                                                       
!     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
!     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
!     and  columns of  A  and the  number of  rows  of  B  respectively.
!                                                                       
      NOTA = LSAME (TRANSA, 'N') 
      NOTB = LSAME (TRANSB, 'N') 
      IF (NOTA) THEN 
        NROWA = M 
        NCOLA = K 
      ELSE 
        NROWA = K 
        NCOLA = M 
      ENDIF 
      IF (NOTB) THEN 
        NROWB = K 
      ELSE 
        NROWB = N 
      ENDIF 
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF ( (.NOT.NOTA) .AND. (.NOT.LSAME (TRANSA, 'C') ) .AND. (        &
      .NOT.LSAME (TRANSA, 'T') ) ) THEN                                 
        INFO = 1 
      ELSEIF ( (.NOT.NOTB) .AND. (.NOT.LSAME (TRANSB, 'C') ) .AND. (    &
      .NOT.LSAME (TRANSB, 'T') ) ) THEN                                 
        INFO = 2 
      ELSEIF (M.LT.0) THEN 
        INFO = 3 
      ELSEIF (N.LT.0) THEN 
        INFO = 4 
      ELSEIF (K.LT.0) THEN 
        INFO = 5 
      ELSEIF (LDA.LT.MAX (1, NROWA) ) THEN 
        INFO = 8 
      ELSEIF (LDB.LT.MAX (1, NROWB) ) THEN 
        INFO = 10 
      ELSEIF (LDC.LT.MAX (1, M) ) THEN 
        INFO = 13 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SGEMM ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (M.EQ.0) .OR. (N.EQ.0) .OR. ( ( (ALPHA.EQ.ZERO) .OR. (K.EQ.0)&
      ) .AND. (BETA.EQ.ONE) ) ) RETURN                                  
!                                                                       
!     And if  alpha.eq.zero.                                            
!                                                                       
      IF (ALPHA.EQ.ZERO) THEN 
        IF (BETA.EQ.ZERO) THEN 
          DO 20 J = 1, N 
            DO 10 I = 1, M 
              C (I, J) = ZERO 
   10       END DO 
   20     END DO 
        ELSE 
          DO 40 J = 1, N 
            DO 30 I = 1, M 
              C (I, J) = BETA * C (I, J) 
   30       END DO 
   40     END DO 
        ENDIF 
        RETURN 
      ENDIF 
!                                                                       
!     Start the operations.                                             
!                                                                       
      IF (NOTB) THEN 
        IF (NOTA) THEN 
!                                                                       
!           Form  C := alpha*A*B + beta*C.                              
!                                                                       
          DO 90 J = 1, N 
            IF (BETA.EQ.ZERO) THEN 
              DO 50 I = 1, M 
                C (I, J) = ZERO 
   50         END DO 
            ELSEIF (BETA.NE.ONE) THEN 
              DO 60 I = 1, M 
                C (I, J) = BETA * C (I, J) 
   60         END DO 
            ENDIF 
            DO 80 L = 1, K 
              IF (B (L, J) .NE.ZERO) THEN 
                TEMP = ALPHA * B (L, J) 
                DO 70 I = 1, M 
                  C (I, J) = C (I, J) + TEMP * A (I, L) 
   70           END DO 
              ENDIF 
   80       END DO 
   90     END DO 
        ELSE 
!                                                                       
!           Form  C := alpha*A**T*B + beta*C                            
!                                                                       
          DO 120 J = 1, N 
            DO 110 I = 1, M 
              TEMP = ZERO 
              DO 100 L = 1, K 
                TEMP = TEMP + A (L, I) * B (L, J) 
  100         END DO 
              IF (BETA.EQ.ZERO) THEN 
                C (I, J) = ALPHA * TEMP 
              ELSE 
                C (I, J) = ALPHA * TEMP + BETA * C (I, J) 
              ENDIF 
  110       END DO 
  120     END DO 
        ENDIF 
      ELSE 
        IF (NOTA) THEN 
!                                                                       
!           Form  C := alpha*A*B**T + beta*C                            
!                                                                       
          DO 170 J = 1, N 
            IF (BETA.EQ.ZERO) THEN 
              DO 130 I = 1, M 
                C (I, J) = ZERO 
  130         END DO 
            ELSEIF (BETA.NE.ONE) THEN 
              DO 140 I = 1, M 
                C (I, J) = BETA * C (I, J) 
  140         END DO 
            ENDIF 
            DO 160 L = 1, K 
              IF (B (J, L) .NE.ZERO) THEN 
                TEMP = ALPHA * B (J, L) 
                DO 150 I = 1, M 
                  C (I, J) = C (I, J) + TEMP * A (I, L) 
  150           END DO 
              ENDIF 
  160       END DO 
  170     END DO 
        ELSE 
!                                                                       
!           Form  C := alpha*A**T*B**T + beta*C                         
!                                                                       
          DO 200 J = 1, N 
            DO 190 I = 1, M 
              TEMP = ZERO 
              DO 180 L = 1, K 
                TEMP = TEMP + A (L, I) * B (J, L) 
  180         END DO 
              IF (BETA.EQ.ZERO) THEN 
                C (I, J) = ALPHA * TEMP 
              ELSE 
                C (I, J) = ALPHA * TEMP + BETA * C (I, J) 
              ENDIF 
  190       END DO 
  200     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SGEMM .                                                    
!                                                                       
      END SUBROUTINE SGEMM                          
      SUBROUTINE SSYR (UPLO, N, ALPHA, X, INCX, A, LDA) 
!     .. Scalar Arguments ..                                            
      REAL ALPHA 
      INTEGER INCX, LDA, N 
      CHARACTER UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSYR   performs the symmetric rank 1 operation                       
!                                                                       
!     A := alpha*x*x**T + A,                                            
!                                                                       
!  where alpha is a real scalar, x is an n element vector and A is an   
!  n by n symmetric matrix.                                             
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the upper or lower         
!           triangular part of the array A is to be referenced as       
!           follows:                                                    
!                                                                       
!              UPLO = 'U' or 'u'   Only the upper triangular part of A  
!                                  is to be referenced.                 
!                                                                       
!              UPLO = 'L' or 'l'   Only the lower triangular part of A  
!                                  is to be referenced.                 
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry with  UPLO = 'U' or 'u', the leading n by n    
!           upper triangular part of the array A must contain the upper 
!           triangular part of the symmetric matrix and the strictly    
!           lower triangular part of A is not referenced. On exit, the  
!           upper triangular part of the array A is overwritten by the  
!           upper triangular part of the updated matrix.                
!           Before entry with UPLO = 'L' or 'l', the leading n by n     
!           lower triangular part of the array A must contain the lower 
!           triangular part of the symmetric matrix and the strictly    
!           upper triangular part of A is not referenced. On exit, the  
!           lower triangular part of the array A is overwritten by the  
!           lower triangular part of the updated matrix.                
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           max( 1, n ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JX, KX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (N.LT.0) THEN 
        INFO = 2 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 5 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = 7 
      ENDIF 
      IF (INFO.NE.0) THEN 
      CALL XERBLA ('SSYR  ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. (ALPHA.EQ.ZERO) ) RETURN 
!                                                                       
!     Set the start point in X if the increment is not unity.           
!                                                                       
      IF (INCX.LE.0) THEN 
        KX = 1 - (N - 1) * INCX 
      ELSEIF (INCX.NE.1) THEN 
        KX = 1 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through the triangular part   
!     of A.                                                             
!                                                                       
      IF (LSAME (UPLO, 'U') ) THEN 
!                                                                       
!        Form  A  when A is stored in upper triangle.                   
!                                                                       
        IF (INCX.EQ.1) THEN 
          DO 20 J = 1, N 
            IF (X (J) .NE.ZERO) THEN 
              TEMP = ALPHA * X (J) 
              DO 10 I = 1, J 
                A (I, J) = A (I, J) + X (I) * TEMP 
   10         END DO 
            ENDIF 
   20     END DO 
        ELSE 
          JX = KX 
          DO 40 J = 1, N 
            IF (X (JX) .NE.ZERO) THEN 
              TEMP = ALPHA * X (JX) 
              IX = KX 
              DO 30 I = 1, J 
                A (I, J) = A (I, J) + X (IX) * TEMP 
                IX = IX + INCX 
   30         END DO 
            ENDIF 
            JX = JX + INCX 
   40     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  A  when A is stored in lower triangle.                   
!                                                                       
        IF (INCX.EQ.1) THEN 
          DO 60 J = 1, N 
            IF (X (J) .NE.ZERO) THEN 
              TEMP = ALPHA * X (J) 
              DO 50 I = J, N 
                A (I, J) = A (I, J) + X (I) * TEMP 
   50         END DO 
            ENDIF 
   60     END DO 
        ELSE 
          JX = KX 
          DO 80 J = 1, N 
            IF (X (JX) .NE.ZERO) THEN 
              TEMP = ALPHA * X (JX) 
              IX = JX 
              DO 70 I = J, N 
                A (I, J) = A (I, J) + X (IX) * TEMP 
                IX = IX + INCX 
   70         END DO 
            ENDIF 
            JX = JX + INCX 
   80     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYR  .                                                    
!                                                                       
      END SUBROUTINE SSYR                           
      SUBROUTINE SSPR2 (UPLO, N, ALPHA, X, INCX, Y, INCY, AP) 
!     .. Scalar Arguments ..                                            
      REAL ALPHA 
      INTEGER INCX, INCY, N 
      CHARACTER UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL AP ( * ), X ( * ), Y ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSPR2  performs the symmetric rank 2 operation                       
!                                                                       
!     A := alpha*x*y**T + alpha*y*x**T + A,                             
!                                                                       
!  where alpha is a scalar, x and y are n element vectors and A is an   
!  n by n symmetric matrix, supplied in packed form.                    
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the upper or lower         
!           triangular part of the matrix A is supplied in the packed   
!           array AP as follows:                                        
!                                                                       
!              UPLO = 'U' or 'u'   The upper triangular part of A is    
!                                  supplied in AP.                      
!                                                                       
!              UPLO = 'L' or 'l'   The lower triangular part of A is    
!                                  supplied in AP.                      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Y      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCY ) ).                              
!           Before entry, the incremented array Y must contain the n    
!           element vector y.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCY   - INTEGER.                                                    
!           On entry, INCY specifies the increment for the elements of  
!           Y. INCY must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  AP     - REAL             array of DIMENSION at least                
!           ( ( n*( n + 1 ) )/2 ).                                      
!           Before entry with  UPLO = 'U' or 'u', the array AP must     
!           contain the upper triangular part of the symmetric matrix   
!           packed sequentially, column by column, so that AP( 1 )      
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )   
!           and a( 2, 2 ) respectively, and so on. On exit, the array   
!           AP is overwritten by the upper triangular part of the       
!           updated matrix.                                             
!           Before entry with UPLO = 'L' or 'l', the array AP must      
!           contain the lower triangular part of the symmetric matrix   
!           packed sequentially, column by column, so that AP( 1 )      
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )   
!           and a( 3, 1 ) respectively, and so on. On exit, the array   
!           AP is overwritten by the lower triangular part of the       
!           updated matrix.                                             
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP1, TEMP2 
      INTEGER I, INFO, IX, IY, J, JX, JY, K, KK, KX, KY 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (N.LT.0) THEN 
        INFO = 2 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 5 
      ELSEIF (INCY.EQ.0) THEN 
        INFO = 7 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSPR2 ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. (ALPHA.EQ.ZERO) ) RETURN 
!                                                                       
!     Set up the start points in X and Y if the increments are not both 
!     unity.                                                            
!                                                                       
      IF ( (INCX.NE.1) .OR. (INCY.NE.1) ) THEN 
        IF (INCX.GT.0) THEN 
          KX = 1 
        ELSE 
          KX = 1 - (N - 1) * INCX 
        ENDIF 
        IF (INCY.GT.0) THEN 
          KY = 1 
        ELSE 
          KY = 1 - (N - 1) * INCY 
        ENDIF 
        JX = KX 
        JY = KY 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of the array AP
!     are accessed sequentially with one pass through AP.               
!                                                                       
      KK = 1 
      IF (LSAME (UPLO, 'U') ) THEN 
!                                                                       
!        Form  A  when upper triangle is stored in AP.                  
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 20 J = 1, N 
            IF ( (X (J) .NE.ZERO) .OR. (Y (J) .NE.ZERO) ) THEN 
              TEMP1 = ALPHA * Y (J) 
              TEMP2 = ALPHA * X (J) 
              K = KK 
              DO 10 I = 1, J 
                AP (K) = AP (K) + X (I) * TEMP1 + Y (I) * TEMP2 
                K = K + 1 
   10         END DO 
            ENDIF 
            KK = KK + J 
   20     END DO 
        ELSE 
          DO 40 J = 1, N 
            IF ( (X (JX) .NE.ZERO) .OR. (Y (JY) .NE.ZERO) ) THEN 
              TEMP1 = ALPHA * Y (JY) 
              TEMP2 = ALPHA * X (JX) 
              IX = KX 
              IY = KY 
              DO 30 K = KK, KK + J - 1 
                AP (K) = AP (K) + X (IX) * TEMP1 + Y (IY) * TEMP2 
                IX = IX + INCX 
                IY = IY + INCY 
   30         END DO 
            ENDIF 
            JX = JX + INCX 
            JY = JY + INCY 
            KK = KK + J 
   40     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  A  when lower triangle is stored in AP.                  
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 60 J = 1, N 
            IF ( (X (J) .NE.ZERO) .OR. (Y (J) .NE.ZERO) ) THEN 
              TEMP1 = ALPHA * Y (J) 
              TEMP2 = ALPHA * X (J) 
              K = KK 
              DO 50 I = J, N 
                AP (K) = AP (K) + X (I) * TEMP1 + Y (I) * TEMP2 
                K = K + 1 
   50         END DO 
            ENDIF 
            KK = KK + N - J + 1 
   60     END DO 
        ELSE 
          DO 80 J = 1, N 
            IF ( (X (JX) .NE.ZERO) .OR. (Y (JY) .NE.ZERO) ) THEN 
              TEMP1 = ALPHA * Y (JY) 
              TEMP2 = ALPHA * X (JX) 
              IX = JX 
              IY = JY 
              DO 70 K = KK, KK + N - J 
                AP (K) = AP (K) + X (IX) * TEMP1 + Y (IY) * TEMP2 
                IX = IX + INCX 
                IY = IY + INCY 
   70         END DO 
            ENDIF 
            JX = JX + INCX 
            JY = JY + INCY 
            KK = KK + N - J + 1 
   80     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSPR2 .                                                    
!                                                                       
      END SUBROUTINE SSPR2                          
      SUBROUTINE STRSV (UPLO, TRANS, DIAG, N, A, LDA, X, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, LDA, N 
      CHARACTER DIAG, TRANS, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  STRSV  solves one of the systems of equations                        
!                                                                       
!     A*x = b,   or   A**T*x = b,                                       
!                                                                       
!  where b and x are n element vectors and A is an n by n unit, or      
!  non-unit, upper or lower triangular matrix.                          
!                                                                       
!  No test for singularity or near-singularity is included in this      
!  routine. Such tests must be performed before calling this routine.   
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the matrix is an upper or  
!           lower triangular matrix as follows:                         
!                                                                       
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.     
!                                                                       
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry, TRANS specifies the equations to be solved as     
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   A*x = b.                            
!                                                                       
!              TRANS = 'T' or 't'   A**T*x = b.                         
!                                                                       
!              TRANS = 'C' or 'c'   A**T*x = b.                         
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  DIAG   - CHARACTER*1.                                                
!           On entry, DIAG specifies whether or not A is unit           
!           triangular as follows:                                      
!                                                                       
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.  
!                                                                       
!              DIAG = 'N' or 'n'   A is not assumed to be unit          
!                                  triangular.                          
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry with  UPLO = 'U' or 'u', the leading n by n    
!           upper triangular part of the array A must contain the upper 
!           triangular matrix and the strictly lower triangular part of 
!           A is not referenced.                                        
!           Before entry with UPLO = 'L' or 'l', the leading n by n     
!           lower triangular part of the array A must contain the lower 
!           triangular matrix and the strictly upper triangular part of 
!           A is not referenced.                                        
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of 
!           A are not referenced either, but are assumed to be unity.   
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           max( 1, n ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element right-hand side vector b. On exit, X is overwritten 
!           with the solution vector x.                                 
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JX, KX 
      LOGICAL NOUNIT 
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (.NOT.LSAME (TRANS, 'N') .AND..NOT.LSAME (TRANS, 'T')      &
      .AND..NOT.LSAME (TRANS, 'C') ) THEN                               
        INFO = 2 
      ELSEIF (.NOT.LSAME (DIAG, 'U') .AND..NOT.LSAME (DIAG, 'N') ) THEN 
        INFO = 3 
      ELSEIF (N.LT.0) THEN 
        INFO = 4 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = 6 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 8 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('STRSV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF (N.EQ.0) RETURN 
!                                                                       
      NOUNIT = LSAME (DIAG, 'N') 
!                                                                       
!     Set up the start point in X if the increment is not unity. This   
!     will be  ( N - 1 )*INCX  too small for descending loops.          
!                                                                       
      IF (INCX.LE.0) THEN 
        KX = 1 - (N - 1) * INCX 
      ELSEIF (INCX.NE.1) THEN 
        KX = 1 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through A.                    
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  x := inv( A )*x.                                         
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          IF (INCX.EQ.1) THEN 
            DO 20 J = N, 1, - 1 
              IF (X (J) .NE.ZERO) THEN 
                IF (NOUNIT) X (J) = X (J) / A (J, J) 
                TEMP = X (J) 
                DO 10 I = J - 1, 1, - 1 
                  X (I) = X (I) - TEMP * A (I, J) 
   10           END DO 
              ENDIF 
   20       END DO 
          ELSE 
            JX = KX + (N - 1) * INCX 
            DO 40 J = N, 1, - 1 
              IF (X (JX) .NE.ZERO) THEN 
                IF (NOUNIT) X (JX) = X (JX) / A (J, J) 
                TEMP = X (JX) 
                IX = JX 
                DO 30 I = J - 1, 1, - 1 
                  IX = IX - INCX 
                  X (IX) = X (IX) - TEMP * A (I, J) 
   30           END DO 
              ENDIF 
              JX = JX - INCX 
   40       END DO 
          ENDIF 
        ELSE 
          IF (INCX.EQ.1) THEN 
            DO 60 J = 1, N 
              IF (X (J) .NE.ZERO) THEN 
                IF (NOUNIT) X (J) = X (J) / A (J, J) 
                TEMP = X (J) 
                DO 50 I = J + 1, N 
                  X (I) = X (I) - TEMP * A (I, J) 
   50           END DO 
              ENDIF 
   60       END DO 
          ELSE 
            JX = KX 
            DO 80 J = 1, N 
              IF (X (JX) .NE.ZERO) THEN 
                IF (NOUNIT) X (JX) = X (JX) / A (J, J) 
                TEMP = X (JX) 
                IX = JX 
                DO 70 I = J + 1, N 
                  IX = IX + INCX 
                  X (IX) = X (IX) - TEMP * A (I, J) 
   70           END DO 
              ENDIF 
              JX = JX + INCX 
   80       END DO 
          ENDIF 
        ENDIF 
      ELSE 
!                                                                       
!        Form  x := inv( A**T )*x.                                      
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          IF (INCX.EQ.1) THEN 
            DO 100 J = 1, N 
              TEMP = X (J) 
              DO 90 I = 1, J - 1 
                TEMP = TEMP - A (I, J) * X (I) 
   90         END DO 
              IF (NOUNIT) TEMP = TEMP / A (J, J) 
              X (J) = TEMP 
  100       END DO 
          ELSE 
            JX = KX 
            DO 120 J = 1, N 
              TEMP = X (JX) 
              IX = KX 
              DO 110 I = 1, J - 1 
                TEMP = TEMP - A (I, J) * X (IX) 
                IX = IX + INCX 
  110         END DO 
              IF (NOUNIT) TEMP = TEMP / A (J, J) 
              X (JX) = TEMP 
              JX = JX + INCX 
  120       END DO 
          ENDIF 
        ELSE 
          IF (INCX.EQ.1) THEN 
            DO 140 J = N, 1, - 1 
              TEMP = X (J) 
              DO 130 I = N, J + 1, - 1 
                TEMP = TEMP - A (I, J) * X (I) 
  130         END DO 
              IF (NOUNIT) TEMP = TEMP / A (J, J) 
              X (J) = TEMP 
  140       END DO 
          ELSE 
            KX = KX + (N - 1) * INCX 
            JX = KX 
            DO 160 J = N, 1, - 1 
              TEMP = X (JX) 
              IX = KX 
              DO 150 I = N, J + 1, - 1 
                TEMP = TEMP - A (I, J) * X (IX) 
                IX = IX - INCX 
  150         END DO 
              IF (NOUNIT) TEMP = TEMP / A (J, J) 
              X (JX) = TEMP 
              JX = JX - INCX 
  160       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of STRSV .                                                    
!                                                                       
      END SUBROUTINE STRSV                          
      SUBROUTINE SSPR (UPLO, N, ALPHA, X, INCX, AP) 
!     .. Scalar Arguments ..                                            
      REAL ALPHA 
      INTEGER INCX, N 
      CHARACTER UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL AP ( * ), X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSPR    performs the symmetric rank 1 operation                      
!                                                                       
!     A := alpha*x*x**T + A,                                            
!                                                                       
!  where alpha is a real scalar, x is an n element vector and A is an   
!  n by n symmetric matrix, supplied in packed form.                    
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the upper or lower         
!           triangular part of the matrix A is supplied in the packed   
!           array AP as follows:                                        
!                                                                       
!              UPLO = 'U' or 'u'   The upper triangular part of A is    
!                                  supplied in AP.                      
!                                                                       
!              UPLO = 'L' or 'l'   The lower triangular part of A is    
!                                  supplied in AP.                      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  AP     - REAL             array of DIMENSION at least                
!           ( ( n*( n + 1 ) )/2 ).                                      
!           Before entry with  UPLO = 'U' or 'u', the array AP must     
!           contain the upper triangular part of the symmetric matrix   
!           packed sequentially, column by column, so that AP( 1 )      
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 1, 2 )   
!           and a( 2, 2 ) respectively, and so on. On exit, the array   
!           AP is overwritten by the upper triangular part of the       
!           updated matrix.                                             
!           Before entry with UPLO = 'L' or 'l', the array AP must      
!           contain the lower triangular part of the symmetric matrix   
!           packed sequentially, column by column, so that AP( 1 )      
!           contains a( 1, 1 ), AP( 2 ) and AP( 3 ) contain a( 2, 1 )   
!           and a( 3, 1 ) respectively, and so on. On exit, the array   
!           AP is overwritten by the lower triangular part of the       
!           updated matrix.                                             
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JX, K, KK, KX 
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (N.LT.0) THEN 
        INFO = 2 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 5 
      ENDIF 
      IF (INFO.NE.0) THEN 
      CALL XERBLA ('SSPR  ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. (ALPHA.EQ.ZERO) ) RETURN 
!                                                                       
!     Set the start point in X if the increment is not unity.           
!                                                                       
      IF (INCX.LE.0) THEN 
        KX = 1 - (N - 1) * INCX 
      ELSEIF (INCX.NE.1) THEN 
        KX = 1 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of the array AP
!     are accessed sequentially with one pass through AP.               
!                                                                       
      KK = 1 
      IF (LSAME (UPLO, 'U') ) THEN 
!                                                                       
!        Form  A  when upper triangle is stored in AP.                  
!                                                                       
        IF (INCX.EQ.1) THEN 
          DO 20 J = 1, N 
            IF (X (J) .NE.ZERO) THEN 
              TEMP = ALPHA * X (J) 
              K = KK 
              DO 10 I = 1, J 
                AP (K) = AP (K) + X (I) * TEMP 
                K = K + 1 
   10         END DO 
            ENDIF 
            KK = KK + J 
   20     END DO 
        ELSE 
          JX = KX 
          DO 40 J = 1, N 
            IF (X (JX) .NE.ZERO) THEN 
              TEMP = ALPHA * X (JX) 
              IX = KX 
              DO 30 K = KK, KK + J - 1 
                AP (K) = AP (K) + X (IX) * TEMP 
                IX = IX + INCX 
   30         END DO 
            ENDIF 
            JX = JX + INCX 
            KK = KK + J 
   40     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  A  when lower triangle is stored in AP.                  
!                                                                       
        IF (INCX.EQ.1) THEN 
          DO 60 J = 1, N 
            IF (X (J) .NE.ZERO) THEN 
              TEMP = ALPHA * X (J) 
              K = KK 
              DO 50 I = J, N 
                AP (K) = AP (K) + X (I) * TEMP 
                K = K + 1 
   50         END DO 
            ENDIF 
            KK = KK + N - J + 1 
   60     END DO 
        ELSE 
          JX = KX 
          DO 80 J = 1, N 
            IF (X (JX) .NE.ZERO) THEN 
              TEMP = ALPHA * X (JX) 
              IX = JX 
              DO 70 K = KK, KK + N - J 
                AP (K) = AP (K) + X (IX) * TEMP 
                IX = IX + INCX 
   70         END DO 
            ENDIF 
            JX = JX + INCX 
            KK = KK + N - J + 1 
   80     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSPR  .                                                    
!                                                                       
      END SUBROUTINE SSPR                           
      SUBROUTINE STRSM (SIDE, UPLO, TRANSA, DIAG, M, N, ALPHA, A, LDA,  &
      B, LDB)                                                           
!     .. Scalar Arguments ..                                            
      REAL ALPHA 
      INTEGER LDA, LDB, M, N 
      CHARACTER DIAG, SIDE, TRANSA, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), B (LDB, * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  STRSM  solves one of the matrix equations                            
!                                                                       
!     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,                  
!                                                                       
!  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
!                                                                       
!     op( A ) = A   or   op( A ) = A**T.                                
!                                                                       
!  The matrix X is overwritten on B.                                    
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  SIDE   - CHARACTER*1.                                                
!           On entry, SIDE specifies whether op( A ) appears on the left
!           or right of X as follows:                                   
!                                                                       
!              SIDE = 'L' or 'l'   op( A )*X = alpha*B.                 
!                                                                       
!              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.                 
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the matrix A is an upper or
!           lower triangular matrix as follows:                         
!                                                                       
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.     
!                                                                       
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANSA - CHARACTER*1.                                                
!           On entry, TRANSA specifies the form of op( A ) to be used in
!           the matrix multiplication as follows:                       
!                                                                       
!              TRANSA = 'N' or 'n'   op( A ) = A.                       
!                                                                       
!              TRANSA = 'T' or 't'   op( A ) = A**T.                    
!                                                                       
!              TRANSA = 'C' or 'c'   op( A ) = A**T.                    
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  DIAG   - CHARACTER*1.                                                
!           On entry, DIAG specifies whether or not A is unit triangular
!           as follows:                                                 
!                                                                       
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.  
!                                                                       
!              DIAG = 'N' or 'n'   A is not assumed to be unit          
!                                  triangular.                          
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  M      - INTEGER.                                                    
!           On entry, M specifies the number of rows of B. M must be at 
!           least zero.                                                 
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the number of columns of B.  N must be
!           at least zero.                                              
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
!           zero then  A is not referenced and  B need not be set before
!           entry.                                                      
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, k ), where k is m
!           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
!           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
!           upper triangular part of the array  A must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           A is not referenced.                                        
!           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
!           lower triangular part of the array  A must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           A is not referenced.                                        
!           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
!           A  are not referenced either,  but are assumed to be  unity.
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
!           then LDA must be at least max( 1, n ).                      
!           Unchanged on exit.                                          
!                                                                       
!  B      - REAL             array of DIMENSION ( LDB, n ).             
!           Before entry,  the leading  m by n part of the array  B must
!           contain  the  right-hand  side  matrix  B,  and  on exit  is
!           overwritten by the solution matrix  X.                      
!                                                                       
!  LDB    - INTEGER.                                                    
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 3 Blas routine.                                                
!                                                                       
!                                                                       
!  -- Written on 8-February-1989.                                       
!     Jack Dongarra, Argonne National Laboratory.                       
!     Iain Duff, AERE Harwell.                                          
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.                   
!     Sven Hammarling, Numerical Algorithms Group Ltd.                  
!                                                                       
!  =====================================================================
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, J, K, NROWA 
      LOGICAL LSIDE, NOUNIT, UPPER 
!     ..                                                                
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      LSIDE = LSAME (SIDE, 'L') 
      IF (LSIDE) THEN 
        NROWA = M 
      ELSE 
        NROWA = N 
      ENDIF 
      NOUNIT = LSAME (DIAG, 'N') 
      UPPER = LSAME (UPLO, 'U') 
!                                                                       
      INFO = 0 
      IF ( (.NOT.LSIDE) .AND. (.NOT.LSAME (SIDE, 'R') ) ) THEN 
        INFO = 1 
      ELSEIF ( (.NOT.UPPER) .AND. (.NOT.LSAME (UPLO, 'L') ) ) THEN 
        INFO = 2 
      ELSEIF ( (.NOT.LSAME (TRANSA, 'N') ) .AND. (.NOT.LSAME (TRANSA,   &
      'T') ) .AND. (.NOT.LSAME (TRANSA, 'C') ) ) THEN                   
        INFO = 3 
      ELSEIF ( (.NOT.LSAME (DIAG, 'U') ) .AND. (.NOT.LSAME (DIAG, 'N') )&
      ) THEN                                                            
        INFO = 4 
      ELSEIF (M.LT.0) THEN 
        INFO = 5 
      ELSEIF (N.LT.0) THEN 
        INFO = 6 
      ELSEIF (LDA.LT.MAX (1, NROWA) ) THEN 
        INFO = 9 
      ELSEIF (LDB.LT.MAX (1, M) ) THEN 
        INFO = 11 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('STRSM ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF (M.EQ.0.OR.N.EQ.0) RETURN 
!                                                                       
!     And when  alpha.eq.zero.                                          
!                                                                       
      IF (ALPHA.EQ.ZERO) THEN 
        DO 20 J = 1, N 
          DO 10 I = 1, M 
            B (I, J) = ZERO 
   10     END DO 
   20   END DO 
        RETURN 
      ENDIF 
!                                                                       
!     Start the operations.                                             
!                                                                       
      IF (LSIDE) THEN 
        IF (LSAME (TRANSA, 'N') ) THEN 
!                                                                       
!           Form  B := alpha*inv( A )*B.                                
!                                                                       
          IF (UPPER) THEN 
            DO 60 J = 1, N 
              IF (ALPHA.NE.ONE) THEN 
                DO 30 I = 1, M 
                  B (I, J) = ALPHA * B (I, J) 
   30           END DO 
              ENDIF 
              DO 50 K = M, 1, - 1 
                IF (B (K, J) .NE.ZERO) THEN 
                  IF (NOUNIT) B (K, J) = B (K, J) / A (K, K) 
                  DO 40 I = 1, K - 1 
                    B (I, J) = B (I, J) - B (K, J) * A (I, K) 
   40             END DO 
                ENDIF 
   50         END DO 
   60       END DO 
          ELSE 
            DO 100 J = 1, N 
              IF (ALPHA.NE.ONE) THEN 
                DO 70 I = 1, M 
                  B (I, J) = ALPHA * B (I, J) 
   70           END DO 
              ENDIF 
              DO 90 K = 1, M 
                IF (B (K, J) .NE.ZERO) THEN 
                  IF (NOUNIT) B (K, J) = B (K, J) / A (K, K) 
                  DO 80 I = K + 1, M 
                    B (I, J) = B (I, J) - B (K, J) * A (I, K) 
   80             END DO 
                ENDIF 
   90         END DO 
  100       END DO 
          ENDIF 
        ELSE 
!                                                                       
!           Form  B := alpha*inv( A**T )*B.                             
!                                                                       
          IF (UPPER) THEN 
            DO 130 J = 1, N 
              DO 120 I = 1, M 
                TEMP = ALPHA * B (I, J) 
                DO 110 K = 1, I - 1 
                  TEMP = TEMP - A (K, I) * B (K, J) 
  110           END DO 
                IF (NOUNIT) TEMP = TEMP / A (I, I) 
                B (I, J) = TEMP 
  120         END DO 
  130       END DO 
          ELSE 
            DO 160 J = 1, N 
              DO 150 I = M, 1, - 1 
                TEMP = ALPHA * B (I, J) 
                DO 140 K = I + 1, M 
                  TEMP = TEMP - A (K, I) * B (K, J) 
  140           END DO 
                IF (NOUNIT) TEMP = TEMP / A (I, I) 
                B (I, J) = TEMP 
  150         END DO 
  160       END DO 
          ENDIF 
        ENDIF 
      ELSE 
        IF (LSAME (TRANSA, 'N') ) THEN 
!                                                                       
!           Form  B := alpha*B*inv( A ).                                
!                                                                       
          IF (UPPER) THEN 
            DO 210 J = 1, N 
              IF (ALPHA.NE.ONE) THEN 
                DO 170 I = 1, M 
                  B (I, J) = ALPHA * B (I, J) 
  170           END DO 
              ENDIF 
              DO 190 K = 1, J - 1 
                IF (A (K, J) .NE.ZERO) THEN 
                  DO 180 I = 1, M 
                    B (I, J) = B (I, J) - A (K, J) * B (I, K) 
  180             END DO 
                ENDIF 
  190         END DO 
              IF (NOUNIT) THEN 
                TEMP = ONE / A (J, J) 
                DO 200 I = 1, M 
                  B (I, J) = TEMP * B (I, J) 
  200           END DO 
              ENDIF 
  210       END DO 
          ELSE 
            DO 260 J = N, 1, - 1 
              IF (ALPHA.NE.ONE) THEN 
                DO 220 I = 1, M 
                  B (I, J) = ALPHA * B (I, J) 
  220           END DO 
              ENDIF 
              DO 240 K = J + 1, N 
                IF (A (K, J) .NE.ZERO) THEN 
                  DO 230 I = 1, M 
                    B (I, J) = B (I, J) - A (K, J) * B (I, K) 
  230             END DO 
                ENDIF 
  240         END DO 
              IF (NOUNIT) THEN 
                TEMP = ONE / A (J, J) 
                DO 250 I = 1, M 
                  B (I, J) = TEMP * B (I, J) 
  250           END DO 
              ENDIF 
  260       END DO 
          ENDIF 
        ELSE 
!                                                                       
!           Form  B := alpha*B*inv( A**T ).                             
!                                                                       
          IF (UPPER) THEN 
            DO 310 K = N, 1, - 1 
              IF (NOUNIT) THEN 
                TEMP = ONE / A (K, K) 
                DO 270 I = 1, M 
                  B (I, K) = TEMP * B (I, K) 
  270           END DO 
              ENDIF 
              DO 290 J = 1, K - 1 
                IF (A (J, K) .NE.ZERO) THEN 
                  TEMP = A (J, K) 
                  DO 280 I = 1, M 
                    B (I, J) = B (I, J) - TEMP * B (I, K) 
  280             END DO 
                ENDIF 
  290         END DO 
              IF (ALPHA.NE.ONE) THEN 
                DO 300 I = 1, M 
                  B (I, K) = ALPHA * B (I, K) 
  300           END DO 
              ENDIF 
  310       END DO 
          ELSE 
            DO 360 K = 1, N 
              IF (NOUNIT) THEN 
                TEMP = ONE / A (K, K) 
                DO 320 I = 1, M 
                  B (I, K) = TEMP * B (I, K) 
  320           END DO 
              ENDIF 
              DO 340 J = K + 1, N 
                IF (A (J, K) .NE.ZERO) THEN 
                  TEMP = A (J, K) 
                  DO 330 I = 1, M 
                    B (I, J) = B (I, J) - TEMP * B (I, K) 
  330             END DO 
                ENDIF 
  340         END DO 
              IF (ALPHA.NE.ONE) THEN 
                DO 350 I = 1, M 
                  B (I, K) = ALPHA * B (I, K) 
  350           END DO 
              ENDIF 
  360       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of STRSM .                                                    
!                                                                       
      END SUBROUTINE STRSM                          
      REAL FUNCTION SNRM2 (N, X, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SNRM2 returns the euclidean norm of a vector via the function        
!  name, so that                                                        
!                                                                       
!     SNRM2 := sqrt( x'*x ).                                            
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  -- This version written on 25-October-1982.                          
!     Modified on 14-October-1993 to inline the call to SLASSQ.         
!     Sven Hammarling, Nag Ltd.                                         
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL ABSXI, NORM, SCALE, SSQ 
      INTEGER IX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, SQRT 
!     ..                                                                
      IF (N.LT.1.OR.INCX.LT.1) THEN 
        NORM = ZERO 
      ELSEIF (N.EQ.1) THEN 
        NORM = ABS (X (1) ) 
      ELSE 
        SCALE = ZERO 
        SSQ = ONE 
!        The following loop is equivalent to this call to the LAPACK    
!        auxiliary routine:                                             
!        CALL SLASSQ( N, X, INCX, SCALE, SSQ )                          
!                                                                       
        DO 10 IX = 1, 1 + (N - 1) * INCX, INCX 
          IF (X (IX) .NE.ZERO) THEN 
            ABSXI = ABS (X (IX) ) 
            IF (SCALE.LT.ABSXI) THEN 
              SSQ = ONE+SSQ * (SCALE / ABSXI) **2 
              SCALE = ABSXI 
            ELSE 
              SSQ = SSQ + (ABSXI / SCALE) **2 
            ENDIF 
          ENDIF 
   10   END DO 
        NORM = SCALE * SQRT (SSQ) 
      ENDIF 
!                                                                       
      SNRM2 = NORM 
      RETURN 
!                                                                       
!     End of SNRM2.                                                     
!                                                                       
      END FUNCTION SNRM2                            
      REAL FUNCTION SDSDOT (N, SB, SX, INCX, SY, INCY) 
!     .. Scalar Arguments ..                                            
      REAL SB 
      INTEGER INCX, INCY, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ), SY ( * ) 
!     ..                                                                
!                                                                       
!  PURPOSE                                                              
!  =======                                                              
!                                                                       
!  Compute the inner product of two vectors with extended               
!  precision accumulation.                                              
!                                                                       
!  Returns S.P. result with dot product accumulated in D.P.             
!  SDSDOT = SB + sum for I = 0 to N-1 of SX(LX+I*INCX)*SY(LY+I*INCY),   
!  where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is       
!  defined in a similar way using INCY.                                 
!                                                                       
!  AUTHOR                                                               
!  ======                                                               
!  Lawson, C. L., (JPL), Hanson, R. J., (SNLA),                         
!  Kincaid, D. R., (U. of Texas), Krogh, F. T., (JPL)                   
!                                                                       
!  ARGUMENTS                                                            
!  =========                                                            
!                                                                       
!  N      (input) INTEGER                                               
!         number of elements in input vector(s)                         
!                                                                       
!  SB     (input) REAL                                                  
!         single precision scalar to be added to inner product          
!                                                                       
!  SX     (input) REAL array, dimension (N)                             
!         single precision vector with N elements                       
!                                                                       
!  INCX   (input) INTEGER                                               
!         storage spacing between elements of SX                        
!                                                                       
!  SY     (input) REAL array, dimension (N)                             
!         single precision vector with N elements                       
!                                                                       
!  INCY   (input) INTEGER                                               
!         storage spacing between elements of SY                        
!                                                                       
!  SDSDOT (output) REAL                                                 
!         single precision dot product (SB if N .LE. 0)                 
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  REFERENCES                                                           
!                                                                       
!  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.                  
!  Krogh, Basic linear algebra subprograms for Fortran                  
!  usage, Algorithm No. 539, Transactions on Mathematical               
!  Software 5, 3 (September 1979), pp. 308-323.                         
!                                                                       
!  REVISION HISTORY  (YYMMDD)                                           
!                                                                       
!  791001  DATE WRITTEN                                                 
!  890531  Changed all specific intrinsics to generic.  (WRB)           
!  890831  Modified array declarations.  (WRB)                          
!  890831  REVISION DATE from Version 3.2                               
!  891214  Prologue converted to Version 4.0 format.  (BAB)             
!  920310  Corrected definition of LX in DESCRIPTION.  (WRB)            
!  920501  Reformatted the REFERENCES section.  (WRB)                   
!  070118  Reformat to LAPACK coding style                              
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      DOUBLE PRECISION DSDOT 
      INTEGER I, KX, KY, NS 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC DBLE 
!     ..                                                                
      DSDOT = SB 
      IF (N.LE.0) THEN 
        SDSDOT = DSDOT 
        RETURN 
      ENDIF 
      IF (INCX.EQ.INCY.AND.INCX.GT.0) THEN 
!                                                                       
!     Code for equal and positive increments.                           
!                                                                       
        NS = N * INCX 
        DO I = 1, NS, INCX 
        DSDOT = DSDOT + DBLE (SX (I) ) * DBLE (SY (I) ) 
        ENDDO 
      ELSE 
!                                                                       
!     Code for unequal or nonpositive increments.                       
!                                                                       
        KX = 1 
        KY = 1 
        IF (INCX.LT.0) KX = 1 + (1 - N) * INCX 
        IF (INCY.LT.0) KY = 1 + (1 - N) * INCY 
        DO I = 1, N 
        DSDOT = DSDOT + DBLE (SX (KX) ) * DBLE (SY (KY) ) 
        KX = KX + INCX 
        KY = KY + INCY 
        ENDDO 
      ENDIF 
      SDSDOT = DSDOT 
      RETURN 
      END FUNCTION SDSDOT                           
      SUBROUTINE SSYRK (UPLO, TRANS, N, K, ALPHA, A, LDA, BETA, C, LDC) 
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER K, LDA, LDC, N 
      CHARACTER TRANS, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), C (LDC, * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSYRK  performs one of the symmetric rank k operations               
!                                                                       
!     C := alpha*A*A**T + beta*C,                                       
!                                                                       
!  or                                                                   
!                                                                       
!     C := alpha*A**T*A + beta*C,                                       
!                                                                       
!  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
!  and  A  is an  n by k  matrix in the first case and a  k by n  matrix
!  in the second case.                                                  
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:                                                    
!                                                                       
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.                 
!                                                                       
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.                 
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   C := alpha*A*A**T + beta*C.         
!                                                                       
!              TRANS = 'T' or 't'   C := alpha*A**T*A + beta*C.         
!                                                                       
!              TRANS = 'C' or 'c'   C := alpha*A**T*A + beta*C.         
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.                                              
!           Unchanged on exit.                                          
!                                                                       
!  K      - INTEGER.                                                    
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns   of  the   matrix   A,   and  on   entry   with
!           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
!           of rows of the matrix  A.  K must be at least zero.         
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.         
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.                                                   
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).                                   
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry, BETA specifies the scalar beta.                   
!           Unchanged on exit.                                          
!                                                                       
!  C      - REAL             array of DIMENSION ( LDC, n ).             
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n 
!           upper triangular part of the array C must contain the upper 
!           triangular part  of the  symmetric matrix  and the strictly 
!           lower triangular part of C is not referenced.  On exit, the 
!           upper triangular part of the array  C is overwritten by the 
!           upper triangular part of the updated matrix.                
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n 
!           lower triangular part of the array C must contain the lower 
!           triangular part  of the  symmetric matrix  and the strictly 
!           upper triangular part of C is not referenced.  On exit, the 
!           lower triangular part of the array  C is overwritten by the 
!           lower triangular part of the updated matrix.                
!                                                                       
!  LDC    - INTEGER.                                                    
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 3 Blas routine.                                                
!                                                                       
!  -- Written on 8-February-1989.                                       
!     Jack Dongarra, Argonne National Laboratory.                       
!     Iain Duff, AERE Harwell.                                          
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.                   
!     Sven Hammarling, Numerical Algorithms Group Ltd.                  
!                                                                       
!  =====================================================================
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, J, L, NROWA 
      LOGICAL UPPER 
!     ..                                                                
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
        NROWA = N 
      ELSE 
        NROWA = K 
      ENDIF 
      UPPER = LSAME (UPLO, 'U') 
!                                                                       
      INFO = 0 
      IF ( (.NOT.UPPER) .AND. (.NOT.LSAME (UPLO, 'L') ) ) THEN 
        INFO = 1 
      ELSEIF ( (.NOT.LSAME (TRANS, 'N') ) .AND. (.NOT.LSAME (TRANS, 'T')&
      ) .AND. (.NOT.LSAME (TRANS, 'C') ) ) THEN                         
        INFO = 2 
      ELSEIF (N.LT.0) THEN 
        INFO = 3 
      ELSEIF (K.LT.0) THEN 
        INFO = 4 
      ELSEIF (LDA.LT.MAX (1, NROWA) ) THEN 
        INFO = 7 
      ELSEIF (LDC.LT.MAX (1, N) ) THEN 
        INFO = 10 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYRK ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. ( ( (ALPHA.EQ.ZERO) .OR. (K.EQ.0) ) .AND. (    &
      BETA.EQ.ONE) ) ) RETURN                                           
!                                                                       
!     And when  alpha.eq.zero.                                          
!                                                                       
      IF (ALPHA.EQ.ZERO) THEN 
        IF (UPPER) THEN 
          IF (BETA.EQ.ZERO) THEN 
            DO 20 J = 1, N 
              DO 10 I = 1, J 
                C (I, J) = ZERO 
   10         END DO 
   20       END DO 
          ELSE 
            DO 40 J = 1, N 
              DO 30 I = 1, J 
                C (I, J) = BETA * C (I, J) 
   30         END DO 
   40       END DO 
          ENDIF 
        ELSE 
          IF (BETA.EQ.ZERO) THEN 
            DO 60 J = 1, N 
              DO 50 I = J, N 
                C (I, J) = ZERO 
   50         END DO 
   60       END DO 
          ELSE 
            DO 80 J = 1, N 
              DO 70 I = J, N 
                C (I, J) = BETA * C (I, J) 
   70         END DO 
   80       END DO 
          ENDIF 
        ENDIF 
        RETURN 
      ENDIF 
!                                                                       
!     Start the operations.                                             
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  C := alpha*A*A**T + beta*C.                              
!                                                                       
        IF (UPPER) THEN 
          DO 130 J = 1, N 
            IF (BETA.EQ.ZERO) THEN 
              DO 90 I = 1, J 
                C (I, J) = ZERO 
   90         END DO 
            ELSEIF (BETA.NE.ONE) THEN 
              DO 100 I = 1, J 
                C (I, J) = BETA * C (I, J) 
  100         END DO 
            ENDIF 
            DO 120 L = 1, K 
              IF (A (J, L) .NE.ZERO) THEN 
                TEMP = ALPHA * A (J, L) 
                DO 110 I = 1, J 
                  C (I, J) = C (I, J) + TEMP * A (I, L) 
  110           END DO 
              ENDIF 
  120       END DO 
  130     END DO 
        ELSE 
          DO 180 J = 1, N 
            IF (BETA.EQ.ZERO) THEN 
              DO 140 I = J, N 
                C (I, J) = ZERO 
  140         END DO 
            ELSEIF (BETA.NE.ONE) THEN 
              DO 150 I = J, N 
                C (I, J) = BETA * C (I, J) 
  150         END DO 
            ENDIF 
            DO 170 L = 1, K 
              IF (A (J, L) .NE.ZERO) THEN 
                TEMP = ALPHA * A (J, L) 
                DO 160 I = J, N 
                  C (I, J) = C (I, J) + TEMP * A (I, L) 
  160           END DO 
              ENDIF 
  170       END DO 
  180     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  C := alpha*A**T*A + beta*C.                              
!                                                                       
        IF (UPPER) THEN 
          DO 210 J = 1, N 
            DO 200 I = 1, J 
              TEMP = ZERO 
              DO 190 L = 1, K 
                TEMP = TEMP + A (L, I) * A (L, J) 
  190         END DO 
              IF (BETA.EQ.ZERO) THEN 
                C (I, J) = ALPHA * TEMP 
              ELSE 
                C (I, J) = ALPHA * TEMP + BETA * C (I, J) 
              ENDIF 
  200       END DO 
  210     END DO 
        ELSE 
          DO 240 J = 1, N 
            DO 230 I = J, N 
              TEMP = ZERO 
              DO 220 L = 1, K 
                TEMP = TEMP + A (L, I) * A (L, J) 
  220         END DO 
              IF (BETA.EQ.ZERO) THEN 
                C (I, J) = ALPHA * TEMP 
              ELSE 
                C (I, J) = ALPHA * TEMP + BETA * C (I, J) 
              ENDIF 
  230       END DO 
  240     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYRK .                                                    
!                                                                       
      END SUBROUTINE SSYRK                          
      SUBROUTINE STRMV (UPLO, TRANS, DIAG, N, A, LDA, X, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, LDA, N 
      CHARACTER DIAG, TRANS, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  STRMV  performs one of the matrix-vector operations                  
!                                                                       
!     x := A*x,   or   x := A**T*x,                                     
!                                                                       
!  where x is an n element vector and  A is an n by n unit, or non-unit,
!  upper or lower triangular matrix.                                    
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the matrix is an upper or  
!           lower triangular matrix as follows:                         
!                                                                       
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.     
!                                                                       
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry, TRANS specifies the operation to be performed as  
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   x := A*x.                           
!                                                                       
!              TRANS = 'T' or 't'   x := A**T*x.                        
!                                                                       
!              TRANS = 'C' or 'c'   x := A**T*x.                        
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  DIAG   - CHARACTER*1.                                                
!           On entry, DIAG specifies whether or not A is unit           
!           triangular as follows:                                      
!                                                                       
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.  
!                                                                       
!              DIAG = 'N' or 'n'   A is not assumed to be unit          
!                                  triangular.                          
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry with  UPLO = 'U' or 'u', the leading n by n    
!           upper triangular part of the array A must contain the upper 
!           triangular matrix and the strictly lower triangular part of 
!           A is not referenced.                                        
!           Before entry with UPLO = 'L' or 'l', the leading n by n     
!           lower triangular part of the array A must contain the lower 
!           triangular matrix and the strictly upper triangular part of 
!           A is not referenced.                                        
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of 
!           A are not referenced either, but are assumed to be unity.   
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           max( 1, n ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x. On exit, X is overwritten with the        
!           tranformed vector x.                                        
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!  The vector and matrix arguments are not referenced when N = 0, or M =
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JX, KX 
      LOGICAL NOUNIT 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (.NOT.LSAME (TRANS, 'N') .AND..NOT.LSAME (TRANS, 'T')      &
      .AND..NOT.LSAME (TRANS, 'C') ) THEN                               
        INFO = 2 
      ELSEIF (.NOT.LSAME (DIAG, 'U') .AND..NOT.LSAME (DIAG, 'N') ) THEN 
        INFO = 3 
      ELSEIF (N.LT.0) THEN 
        INFO = 4 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = 6 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 8 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('STRMV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF (N.EQ.0) RETURN 
!                                                                       
      NOUNIT = LSAME (DIAG, 'N') 
!                                                                       
!     Set up the start point in X if the increment is not unity. This   
!     will be  ( N - 1 )*INCX  too small for descending loops.          
!                                                                       
      IF (INCX.LE.0) THEN 
        KX = 1 - (N - 1) * INCX 
      ELSEIF (INCX.NE.1) THEN 
        KX = 1 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through A.                    
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  x := A*x.                                                
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          IF (INCX.EQ.1) THEN 
            DO 20 J = 1, N 
              IF (X (J) .NE.ZERO) THEN 
                TEMP = X (J) 
                DO 10 I = 1, J - 1 
                  X (I) = X (I) + TEMP * A (I, J) 
   10           END DO 
                IF (NOUNIT) X (J) = X (J) * A (J, J) 
              ENDIF 
   20       END DO 
          ELSE 
            JX = KX 
            DO 40 J = 1, N 
              IF (X (JX) .NE.ZERO) THEN 
                TEMP = X (JX) 
                IX = KX 
                DO 30 I = 1, J - 1 
                  X (IX) = X (IX) + TEMP * A (I, J) 
                  IX = IX + INCX 
   30           END DO 
                IF (NOUNIT) X (JX) = X (JX) * A (J, J) 
              ENDIF 
              JX = JX + INCX 
   40       END DO 
          ENDIF 
        ELSE 
          IF (INCX.EQ.1) THEN 
            DO 60 J = N, 1, - 1 
              IF (X (J) .NE.ZERO) THEN 
                TEMP = X (J) 
                DO 50 I = N, J + 1, - 1 
                  X (I) = X (I) + TEMP * A (I, J) 
   50           END DO 
                IF (NOUNIT) X (J) = X (J) * A (J, J) 
              ENDIF 
   60       END DO 
          ELSE 
            KX = KX + (N - 1) * INCX 
            JX = KX 
            DO 80 J = N, 1, - 1 
              IF (X (JX) .NE.ZERO) THEN 
                TEMP = X (JX) 
                IX = KX 
                DO 70 I = N, J + 1, - 1 
                  X (IX) = X (IX) + TEMP * A (I, J) 
                  IX = IX - INCX 
   70           END DO 
                IF (NOUNIT) X (JX) = X (JX) * A (J, J) 
              ENDIF 
              JX = JX - INCX 
   80       END DO 
          ENDIF 
        ENDIF 
      ELSE 
!                                                                       
!        Form  x := A**T*x.                                             
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          IF (INCX.EQ.1) THEN 
            DO 100 J = N, 1, - 1 
              TEMP = X (J) 
              IF (NOUNIT) TEMP = TEMP * A (J, J) 
              DO 90 I = J - 1, 1, - 1 
                TEMP = TEMP + A (I, J) * X (I) 
   90         END DO 
              X (J) = TEMP 
  100       END DO 
          ELSE 
            JX = KX + (N - 1) * INCX 
            DO 120 J = N, 1, - 1 
              TEMP = X (JX) 
              IX = JX 
              IF (NOUNIT) TEMP = TEMP * A (J, J) 
              DO 110 I = J - 1, 1, - 1 
                IX = IX - INCX 
                TEMP = TEMP + A (I, J) * X (IX) 
  110         END DO 
              X (JX) = TEMP 
              JX = JX - INCX 
  120       END DO 
          ENDIF 
        ELSE 
          IF (INCX.EQ.1) THEN 
            DO 140 J = 1, N 
              TEMP = X (J) 
              IF (NOUNIT) TEMP = TEMP * A (J, J) 
              DO 130 I = J + 1, N 
                TEMP = TEMP + A (I, J) * X (I) 
  130         END DO 
              X (J) = TEMP 
  140       END DO 
          ELSE 
            JX = KX 
            DO 160 J = 1, N 
              TEMP = X (JX) 
              IX = JX 
              IF (NOUNIT) TEMP = TEMP * A (J, J) 
              DO 150 I = J + 1, N 
                IX = IX + INCX 
                TEMP = TEMP + A (I, J) * X (IX) 
  150         END DO 
              X (JX) = TEMP 
              JX = JX + INCX 
  160       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of STRMV .                                                    
!                                                                       
      END SUBROUTINE STRMV                          
      SUBROUTINE STBMV (UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, K, LDA, N 
      CHARACTER DIAG, TRANS, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  STBMV  performs one of the matrix-vector operations                  
!                                                                       
!     x := A*x,   or   x := A**T*x,                                     
!                                                                       
!  where x is an n element vector and  A is an n by n unit, or non-unit,
!  upper or lower triangular band matrix, with ( k + 1 ) diagonals.     
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the matrix is an upper or  
!           lower triangular matrix as follows:                         
!                                                                       
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.     
!                                                                       
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry, TRANS specifies the operation to be performed as  
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   x := A*x.                           
!                                                                       
!              TRANS = 'T' or 't'   x := A**T*x.                        
!                                                                       
!              TRANS = 'C' or 'c'   x := A**T*x.                        
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  DIAG   - CHARACTER*1.                                                
!           On entry, DIAG specifies whether or not A is unit           
!           triangular as follows:                                      
!                                                                       
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.  
!                                                                       
!              DIAG = 'N' or 'n'   A is not assumed to be unit          
!                                  triangular.                          
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  K      - INTEGER.                                                    
!           On entry with UPLO = 'U' or 'u', K specifies the number of  
!           super-diagonals of the matrix A.                            
!           On entry with UPLO = 'L' or 'l', K specifies the number of  
!           sub-diagonals of the matrix A.                              
!           K must satisfy  0 .le. K.                                   
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )  
!           by n part of the array A must contain the upper triangular  
!           band part of the matrix of coefficients, supplied column by 
!           column, with the leading diagonal of the matrix in row      
!           ( k + 1 ) of the array, the first super-diagonal starting at
!           position 2 in row k, and so on. The top left k by k triangle
!           of the array A is not referenced.                           
!           The following program segment will transfer an upper        
!           triangular band matrix from conventional full matrix storage
!           to band storage:                                            
!                                                                       
!                 DO 20, J = 1, N                                       
!                    M = K + 1 - J                                      
!                    DO 10, I = MAX( 1, J - K ), J                      
!                       A( M + I, J ) = matrix( I, J )                  
!              10    CONTINUE                                           
!              20 CONTINUE                                              
!                                                                       
!           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )  
!           by n part of the array A must contain the lower triangular  
!           band part of the matrix of coefficients, supplied column by 
!           column, with the leading diagonal of the matrix in row 1 of 
!           the array, the first sub-diagonal starting at position 1 in 
!           row 2, and so on. The bottom right k by k triangle of the   
!           array A is not referenced.                                  
!           The following program segment will transfer a lower         
!           triangular band matrix from conventional full matrix storage
!           to band storage:                                            
!                                                                       
!                 DO 20, J = 1, N                                       
!                    M = 1 - J                                          
!                    DO 10, I = J, MIN( N, J + K )                      
!                       A( M + I, J ) = matrix( I, J )                  
!              10    CONTINUE                                           
!              20 CONTINUE                                              
!                                                                       
!           Note that when DIAG = 'U' or 'u' the elements of the array A
!           corresponding to the diagonal elements of the matrix are not
!           referenced, but are assumed to be unity.                    
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           ( k + 1 ).                                                  
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x. On exit, X is overwritten with the        
!           tranformed vector x.                                        
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!  The vector and matrix arguments are not referenced when N = 0, or M =
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JX, KPLUS1, KX, L 
      LOGICAL NOUNIT 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX, MIN 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (.NOT.LSAME (TRANS, 'N') .AND..NOT.LSAME (TRANS, 'T')      &
      .AND..NOT.LSAME (TRANS, 'C') ) THEN                               
        INFO = 2 
      ELSEIF (.NOT.LSAME (DIAG, 'U') .AND..NOT.LSAME (DIAG, 'N') ) THEN 
        INFO = 3 
      ELSEIF (N.LT.0) THEN 
        INFO = 4 
      ELSEIF (K.LT.0) THEN 
        INFO = 5 
      ELSEIF (LDA.LT. (K + 1) ) THEN 
        INFO = 7 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 9 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('STBMV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF (N.EQ.0) RETURN 
!                                                                       
      NOUNIT = LSAME (DIAG, 'N') 
!                                                                       
!     Set up the start point in X if the increment is not unity. This   
!     will be  ( N - 1 )*INCX   too small for descending loops.         
!                                                                       
      IF (INCX.LE.0) THEN 
        KX = 1 - (N - 1) * INCX 
      ELSEIF (INCX.NE.1) THEN 
        KX = 1 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through A.                    
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!         Form  x := A*x.                                               
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          KPLUS1 = K + 1 
          IF (INCX.EQ.1) THEN 
            DO 20 J = 1, N 
              IF (X (J) .NE.ZERO) THEN 
                TEMP = X (J) 
                L = KPLUS1 - J 
                DO 10 I = MAX (1, J - K), J - 1 
                  X (I) = X (I) + TEMP * A (L + I, J) 
   10           END DO 
                IF (NOUNIT) X (J) = X (J) * A (KPLUS1, J) 
              ENDIF 
   20       END DO 
          ELSE 
            JX = KX 
            DO 40 J = 1, N 
              IF (X (JX) .NE.ZERO) THEN 
                TEMP = X (JX) 
                IX = KX 
                L = KPLUS1 - J 
                DO 30 I = MAX (1, J - K), J - 1 
                  X (IX) = X (IX) + TEMP * A (L + I, J) 
                  IX = IX + INCX 
   30           END DO 
                IF (NOUNIT) X (JX) = X (JX) * A (KPLUS1, J) 
              ENDIF 
              JX = JX + INCX 
              IF (J.GT.K) KX = KX + INCX 
   40       END DO 
          ENDIF 
        ELSE 
          IF (INCX.EQ.1) THEN 
            DO 60 J = N, 1, - 1 
              IF (X (J) .NE.ZERO) THEN 
                TEMP = X (J) 
                L = 1 - J 
                DO 50 I = MIN (N, J + K), J + 1, - 1 
                  X (I) = X (I) + TEMP * A (L + I, J) 
   50           END DO 
                IF (NOUNIT) X (J) = X (J) * A (1, J) 
              ENDIF 
   60       END DO 
          ELSE 
            KX = KX + (N - 1) * INCX 
            JX = KX 
            DO 80 J = N, 1, - 1 
              IF (X (JX) .NE.ZERO) THEN 
                TEMP = X (JX) 
                IX = KX 
                L = 1 - J 
                DO 70 I = MIN (N, J + K), J + 1, - 1 
                  X (IX) = X (IX) + TEMP * A (L + I, J) 
                  IX = IX - INCX 
   70           END DO 
                IF (NOUNIT) X (JX) = X (JX) * A (1, J) 
              ENDIF 
              JX = JX - INCX 
              IF ( (N - J) .GE.K) KX = KX - INCX 
   80       END DO 
          ENDIF 
        ENDIF 
      ELSE 
!                                                                       
!        Form  x := A**T*x.                                             
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          KPLUS1 = K + 1 
          IF (INCX.EQ.1) THEN 
            DO 100 J = N, 1, - 1 
              TEMP = X (J) 
              L = KPLUS1 - J 
              IF (NOUNIT) TEMP = TEMP * A (KPLUS1, J) 
              DO 90 I = J - 1, MAX (1, J - K), - 1 
                TEMP = TEMP + A (L + I, J) * X (I) 
   90         END DO 
              X (J) = TEMP 
  100       END DO 
          ELSE 
            KX = KX + (N - 1) * INCX 
            JX = KX 
            DO 120 J = N, 1, - 1 
              TEMP = X (JX) 
              KX = KX - INCX 
              IX = KX 
              L = KPLUS1 - J 
              IF (NOUNIT) TEMP = TEMP * A (KPLUS1, J) 
              DO 110 I = J - 1, MAX (1, J - K), - 1 
                TEMP = TEMP + A (L + I, J) * X (IX) 
                IX = IX - INCX 
  110         END DO 
              X (JX) = TEMP 
              JX = JX - INCX 
  120       END DO 
          ENDIF 
        ELSE 
          IF (INCX.EQ.1) THEN 
            DO 140 J = 1, N 
              TEMP = X (J) 
              L = 1 - J 
              IF (NOUNIT) TEMP = TEMP * A (1, J) 
              DO 130 I = J + 1, MIN (N, J + K) 
                TEMP = TEMP + A (L + I, J) * X (I) 
  130         END DO 
              X (J) = TEMP 
  140       END DO 
          ELSE 
            JX = KX 
            DO 160 J = 1, N 
              TEMP = X (JX) 
              KX = KX + INCX 
              IX = KX 
              L = 1 - J 
              IF (NOUNIT) TEMP = TEMP * A (1, J) 
              DO 150 I = J + 1, MIN (N, J + K) 
                TEMP = TEMP + A (L + I, J) * X (IX) 
                IX = IX + INCX 
  150         END DO 
              X (JX) = TEMP 
              JX = JX + INCX 
  160       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of STBMV .                                                    
!                                                                       
      END SUBROUTINE STBMV                          
      SUBROUTINE STPSV (UPLO, TRANS, DIAG, N, AP, X, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
      CHARACTER DIAG, TRANS, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL AP ( * ), X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  STPSV  solves one of the systems of equations                        
!                                                                       
!     A*x = b,   or   A**T*x = b,                                       
!                                                                       
!  where b and x are n element vectors and A is an n by n unit, or      
!  non-unit, upper or lower triangular matrix, supplied in packed form. 
!                                                                       
!  No test for singularity or near-singularity is included in this      
!  routine. Such tests must be performed before calling this routine.   
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the matrix is an upper or  
!           lower triangular matrix as follows:                         
!                                                                       
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.     
!                                                                       
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry, TRANS specifies the equations to be solved as     
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   A*x = b.                            
!                                                                       
!              TRANS = 'T' or 't'   A**T*x = b.                         
!                                                                       
!              TRANS = 'C' or 'c'   A**T*x = b.                         
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  DIAG   - CHARACTER*1.                                                
!           On entry, DIAG specifies whether or not A is unit           
!           triangular as follows:                                      
!                                                                       
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.  
!                                                                       
!              DIAG = 'N' or 'n'   A is not assumed to be unit          
!                                  triangular.                          
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  AP     - REAL             array of DIMENSION at least                
!           ( ( n*( n + 1 ) )/2 ).                                      
!           Before entry with  UPLO = 'U' or 'u', the array AP must     
!           contain the upper triangular matrix packed sequentially,    
!           column by column, so that AP( 1 ) contains a( 1, 1 ),       
!           AP( 2 ) and AP( 3 ) contain a( 1, 2 ) and a( 2, 2 )         
!           respectively, and so on.                                    
!           Before entry with UPLO = 'L' or 'l', the array AP must      
!           contain the lower triangular matrix packed sequentially,    
!           column by column, so that AP( 1 ) contains a( 1, 1 ),       
!           AP( 2 ) and AP( 3 ) contain a( 2, 1 ) and a( 3, 1 )         
!           respectively, and so on.                                    
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of 
!           A are not referenced, but are assumed to be unity.          
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element right-hand side vector b. On exit, X is overwritten 
!           with the solution vector x.                                 
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JX, K, KK, KX 
      LOGICAL NOUNIT 
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (.NOT.LSAME (TRANS, 'N') .AND..NOT.LSAME (TRANS, 'T')      &
      .AND..NOT.LSAME (TRANS, 'C') ) THEN                               
        INFO = 2 
      ELSEIF (.NOT.LSAME (DIAG, 'U') .AND..NOT.LSAME (DIAG, 'N') ) THEN 
        INFO = 3 
      ELSEIF (N.LT.0) THEN 
        INFO = 4 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 7 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('STPSV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF (N.EQ.0) RETURN 
!                                                                       
      NOUNIT = LSAME (DIAG, 'N') 
!                                                                       
!     Set up the start point in X if the increment is not unity. This   
!     will be  ( N - 1 )*INCX  too small for descending loops.          
!                                                                       
      IF (INCX.LE.0) THEN 
        KX = 1 - (N - 1) * INCX 
      ELSEIF (INCX.NE.1) THEN 
        KX = 1 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of AP are      
!     accessed sequentially with one pass through AP.                   
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  x := inv( A )*x.                                         
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          KK = (N * (N + 1) ) / 2 
          IF (INCX.EQ.1) THEN 
            DO 20 J = N, 1, - 1 
              IF (X (J) .NE.ZERO) THEN 
                IF (NOUNIT) X (J) = X (J) / AP (KK) 
                TEMP = X (J) 
                K = KK - 1 
                DO 10 I = J - 1, 1, - 1 
                  X (I) = X (I) - TEMP * AP (K) 
                  K = K - 1 
   10           END DO 
              ENDIF 
              KK = KK - J 
   20       END DO 
          ELSE 
            JX = KX + (N - 1) * INCX 
            DO 40 J = N, 1, - 1 
              IF (X (JX) .NE.ZERO) THEN 
                IF (NOUNIT) X (JX) = X (JX) / AP (KK) 
                TEMP = X (JX) 
                IX = JX 
                DO 30 K = KK - 1, KK - J + 1, - 1 
                  IX = IX - INCX 
                  X (IX) = X (IX) - TEMP * AP (K) 
   30           END DO 
              ENDIF 
              JX = JX - INCX 
              KK = KK - J 
   40       END DO 
          ENDIF 
        ELSE 
          KK = 1 
          IF (INCX.EQ.1) THEN 
            DO 60 J = 1, N 
              IF (X (J) .NE.ZERO) THEN 
                IF (NOUNIT) X (J) = X (J) / AP (KK) 
                TEMP = X (J) 
                K = KK + 1 
                DO 50 I = J + 1, N 
                  X (I) = X (I) - TEMP * AP (K) 
                  K = K + 1 
   50           END DO 
              ENDIF 
              KK = KK + (N - J + 1) 
   60       END DO 
          ELSE 
            JX = KX 
            DO 80 J = 1, N 
              IF (X (JX) .NE.ZERO) THEN 
                IF (NOUNIT) X (JX) = X (JX) / AP (KK) 
                TEMP = X (JX) 
                IX = JX 
                DO 70 K = KK + 1, KK + N - J 
                  IX = IX + INCX 
                  X (IX) = X (IX) - TEMP * AP (K) 
   70           END DO 
              ENDIF 
              JX = JX + INCX 
              KK = KK + (N - J + 1) 
   80       END DO 
          ENDIF 
        ENDIF 
      ELSE 
!                                                                       
!        Form  x := inv( A**T )*x.                                      
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          KK = 1 
          IF (INCX.EQ.1) THEN 
            DO 100 J = 1, N 
              TEMP = X (J) 
              K = KK 
              DO 90 I = 1, J - 1 
                TEMP = TEMP - AP (K) * X (I) 
                K = K + 1 
   90         END DO 
              IF (NOUNIT) TEMP = TEMP / AP (KK + J - 1) 
              X (J) = TEMP 
              KK = KK + J 
  100       END DO 
          ELSE 
            JX = KX 
            DO 120 J = 1, N 
              TEMP = X (JX) 
              IX = KX 
              DO 110 K = KK, KK + J - 2 
                TEMP = TEMP - AP (K) * X (IX) 
                IX = IX + INCX 
  110         END DO 
              IF (NOUNIT) TEMP = TEMP / AP (KK + J - 1) 
              X (JX) = TEMP 
              JX = JX + INCX 
              KK = KK + J 
  120       END DO 
          ENDIF 
        ELSE 
          KK = (N * (N + 1) ) / 2 
          IF (INCX.EQ.1) THEN 
            DO 140 J = N, 1, - 1 
              TEMP = X (J) 
              K = KK 
              DO 130 I = N, J + 1, - 1 
                TEMP = TEMP - AP (K) * X (I) 
                K = K - 1 
  130         END DO 
              IF (NOUNIT) TEMP = TEMP / AP (KK - N + J) 
              X (J) = TEMP 
              KK = KK - (N - J + 1) 
  140       END DO 
          ELSE 
            KX = KX + (N - 1) * INCX 
            JX = KX 
            DO 160 J = N, 1, - 1 
              TEMP = X (JX) 
              IX = KX 
              DO 150 K = KK, KK - (N - (J + 1) ), - 1 
                TEMP = TEMP - AP (K) * X (IX) 
                IX = IX - INCX 
  150         END DO 
              IF (NOUNIT) TEMP = TEMP / AP (KK - N + J) 
              X (JX) = TEMP 
              JX = JX - INCX 
              KK = KK - (N - J + 1) 
  160       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of STPSV .                                                    
!                                                                       
      END SUBROUTINE STPSV                          
      REAL FUNCTION SCNRM2 (N, X, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      COMPLEX X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SCNRM2 returns the euclidean norm of a vector via the function       
!  name, so that                                                        
!                                                                       
!     SCNRM2 := sqrt( x**H*x )                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  -- This version written on 25-October-1982.                          
!     Modified on 14-October-1993 to inline the call to CLASSQ.         
!     Sven Hammarling, Nag Ltd.                                         
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL NORM, SCALE, SSQ, TEMP 
      INTEGER IX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, AIMAG, REAL, SQRT 
!     ..                                                                
      IF (N.LT.1.OR.INCX.LT.1) THEN 
        NORM = ZERO 
      ELSE 
        SCALE = ZERO 
        SSQ = ONE 
!        The following loop is equivalent to this call to the LAPACK    
!        auxiliary routine:                                             
!        CALL CLASSQ( N, X, INCX, SCALE, SSQ )                          
!                                                                       
        DO 10 IX = 1, 1 + (N - 1) * INCX, INCX 
          IF (REAL (X (IX) ) .NE.ZERO) THEN 
            TEMP = ABS (REAL (X (IX) ) ) 
            IF (SCALE.LT.TEMP) THEN 
              SSQ = ONE+SSQ * (SCALE / TEMP) **2 
              SCALE = TEMP 
            ELSE 
              SSQ = SSQ + (TEMP / SCALE) **2 
            ENDIF 
          ENDIF 
          IF (AIMAG (X (IX) ) .NE.ZERO) THEN 
            TEMP = ABS (AIMAG (X (IX) ) ) 
            IF (SCALE.LT.TEMP) THEN 
              SSQ = ONE+SSQ * (SCALE / TEMP) **2 
              SCALE = TEMP 
            ELSE 
              SSQ = SSQ + (TEMP / SCALE) **2 
            ENDIF 
          ENDIF 
   10   END DO 
        NORM = SCALE * SQRT (SSQ) 
      ENDIF 
!                                                                       
      SCNRM2 = NORM 
      RETURN 
!                                                                       
!     End of SCNRM2.                                                    
!                                                                       
      END FUNCTION SCNRM2                           
      SUBROUTINE STBSV (UPLO, TRANS, DIAG, N, K, A, LDA, X, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, K, LDA, N 
      CHARACTER DIAG, TRANS, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  STBSV  solves one of the systems of equations                        
!                                                                       
!     A*x = b,   or   A**T*x = b,                                       
!                                                                       
!  where b and x are n element vectors and A is an n by n unit, or      
!  non-unit, upper or lower triangular band matrix, with ( k + 1 )      
!  diagonals.                                                           
!                                                                       
!  No test for singularity or near-singularity is included in this      
!  routine. Such tests must be performed before calling this routine.   
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the matrix is an upper or  
!           lower triangular matrix as follows:                         
!                                                                       
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.     
!                                                                       
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry, TRANS specifies the equations to be solved as     
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   A*x = b.                            
!                                                                       
!              TRANS = 'T' or 't'   A**T*x = b.                         
!                                                                       
!              TRANS = 'C' or 'c'   A**T*x = b.                         
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  DIAG   - CHARACTER*1.                                                
!           On entry, DIAG specifies whether or not A is unit           
!           triangular as follows:                                      
!                                                                       
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.  
!                                                                       
!              DIAG = 'N' or 'n'   A is not assumed to be unit          
!                                  triangular.                          
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  K      - INTEGER.                                                    
!           On entry with UPLO = 'U' or 'u', K specifies the number of  
!           super-diagonals of the matrix A.                            
!           On entry with UPLO = 'L' or 'l', K specifies the number of  
!           sub-diagonals of the matrix A.                              
!           K must satisfy  0 .le. K.                                   
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )  
!           by n part of the array A must contain the upper triangular  
!           band part of the matrix of coefficients, supplied column by 
!           column, with the leading diagonal of the matrix in row      
!           ( k + 1 ) of the array, the first super-diagonal starting at
!           position 2 in row k, and so on. The top left k by k triangle
!           of the array A is not referenced.                           
!           The following program segment will transfer an upper        
!           triangular band matrix from conventional full matrix storage
!           to band storage:                                            
!                                                                       
!                 DO 20, J = 1, N                                       
!                    M = K + 1 - J                                      
!                    DO 10, I = MAX( 1, J - K ), J                      
!                       A( M + I, J ) = matrix( I, J )                  
!              10    CONTINUE                                           
!              20 CONTINUE                                              
!                                                                       
!           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )  
!           by n part of the array A must contain the lower triangular  
!           band part of the matrix of coefficients, supplied column by 
!           column, with the leading diagonal of the matrix in row 1 of 
!           the array, the first sub-diagonal starting at position 1 in 
!           row 2, and so on. The bottom right k by k triangle of the   
!           array A is not referenced.                                  
!           The following program segment will transfer a lower         
!           triangular band matrix from conventional full matrix storage
!           to band storage:                                            
!                                                                       
!                 DO 20, J = 1, N                                       
!                    M = 1 - J                                          
!                    DO 10, I = J, MIN( N, J + K )                      
!                       A( M + I, J ) = matrix( I, J )                  
!              10    CONTINUE                                           
!              20 CONTINUE                                              
!                                                                       
!           Note that when DIAG = 'U' or 'u' the elements of the array A
!           corresponding to the diagonal elements of the matrix are not
!           referenced, but are assumed to be unity.                    
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           ( k + 1 ).                                                  
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element right-hand side vector b. On exit, X is overwritten 
!           with the solution vector x.                                 
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JX, KPLUS1, KX, L 
      LOGICAL NOUNIT 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX, MIN 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (.NOT.LSAME (TRANS, 'N') .AND..NOT.LSAME (TRANS, 'T')      &
      .AND..NOT.LSAME (TRANS, 'C') ) THEN                               
        INFO = 2 
      ELSEIF (.NOT.LSAME (DIAG, 'U') .AND..NOT.LSAME (DIAG, 'N') ) THEN 
        INFO = 3 
      ELSEIF (N.LT.0) THEN 
        INFO = 4 
      ELSEIF (K.LT.0) THEN 
        INFO = 5 
      ELSEIF (LDA.LT. (K + 1) ) THEN 
        INFO = 7 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 9 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('STBSV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF (N.EQ.0) RETURN 
!                                                                       
      NOUNIT = LSAME (DIAG, 'N') 
!                                                                       
!     Set up the start point in X if the increment is not unity. This   
!     will be  ( N - 1 )*INCX  too small for descending loops.          
!                                                                       
      IF (INCX.LE.0) THEN 
        KX = 1 - (N - 1) * INCX 
      ELSEIF (INCX.NE.1) THEN 
        KX = 1 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed by sequentially with one pass through A.                 
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  x := inv( A )*x.                                         
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          KPLUS1 = K + 1 
          IF (INCX.EQ.1) THEN 
            DO 20 J = N, 1, - 1 
              IF (X (J) .NE.ZERO) THEN 
                L = KPLUS1 - J 
                IF (NOUNIT) X (J) = X (J) / A (KPLUS1, J) 
                TEMP = X (J) 
                DO 10 I = J - 1, MAX (1, J - K), - 1 
                  X (I) = X (I) - TEMP * A (L + I, J) 
   10           END DO 
              ENDIF 
   20       END DO 
          ELSE 
            KX = KX + (N - 1) * INCX 
            JX = KX 
            DO 40 J = N, 1, - 1 
              KX = KX - INCX 
              IF (X (JX) .NE.ZERO) THEN 
                IX = KX 
                L = KPLUS1 - J 
                IF (NOUNIT) X (JX) = X (JX) / A (KPLUS1, J) 
                TEMP = X (JX) 
                DO 30 I = J - 1, MAX (1, J - K), - 1 
                  X (IX) = X (IX) - TEMP * A (L + I, J) 
                  IX = IX - INCX 
   30           END DO 
              ENDIF 
              JX = JX - INCX 
   40       END DO 
          ENDIF 
        ELSE 
          IF (INCX.EQ.1) THEN 
            DO 60 J = 1, N 
              IF (X (J) .NE.ZERO) THEN 
                L = 1 - J 
                IF (NOUNIT) X (J) = X (J) / A (1, J) 
                TEMP = X (J) 
                DO 50 I = J + 1, MIN (N, J + K) 
                  X (I) = X (I) - TEMP * A (L + I, J) 
   50           END DO 
              ENDIF 
   60       END DO 
          ELSE 
            JX = KX 
            DO 80 J = 1, N 
              KX = KX + INCX 
              IF (X (JX) .NE.ZERO) THEN 
                IX = KX 
                L = 1 - J 
                IF (NOUNIT) X (JX) = X (JX) / A (1, J) 
                TEMP = X (JX) 
                DO 70 I = J + 1, MIN (N, J + K) 
                  X (IX) = X (IX) - TEMP * A (L + I, J) 
                  IX = IX + INCX 
   70           END DO 
              ENDIF 
              JX = JX + INCX 
   80       END DO 
          ENDIF 
        ENDIF 
      ELSE 
!                                                                       
!        Form  x := inv( A**T)*x.                                       
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          KPLUS1 = K + 1 
          IF (INCX.EQ.1) THEN 
            DO 100 J = 1, N 
              TEMP = X (J) 
              L = KPLUS1 - J 
              DO 90 I = MAX (1, J - K), J - 1 
                TEMP = TEMP - A (L + I, J) * X (I) 
   90         END DO 
              IF (NOUNIT) TEMP = TEMP / A (KPLUS1, J) 
              X (J) = TEMP 
  100       END DO 
          ELSE 
            JX = KX 
            DO 120 J = 1, N 
              TEMP = X (JX) 
              IX = KX 
              L = KPLUS1 - J 
              DO 110 I = MAX (1, J - K), J - 1 
                TEMP = TEMP - A (L + I, J) * X (IX) 
                IX = IX + INCX 
  110         END DO 
              IF (NOUNIT) TEMP = TEMP / A (KPLUS1, J) 
              X (JX) = TEMP 
              JX = JX + INCX 
              IF (J.GT.K) KX = KX + INCX 
  120       END DO 
          ENDIF 
        ELSE 
          IF (INCX.EQ.1) THEN 
            DO 140 J = N, 1, - 1 
              TEMP = X (J) 
              L = 1 - J 
              DO 130 I = MIN (N, J + K), J + 1, - 1 
                TEMP = TEMP - A (L + I, J) * X (I) 
  130         END DO 
              IF (NOUNIT) TEMP = TEMP / A (1, J) 
              X (J) = TEMP 
  140       END DO 
          ELSE 
            KX = KX + (N - 1) * INCX 
            JX = KX 
            DO 160 J = N, 1, - 1 
              TEMP = X (JX) 
              IX = KX 
              L = 1 - J 
              DO 150 I = MIN (N, J + K), J + 1, - 1 
                TEMP = TEMP - A (L + I, J) * X (IX) 
                IX = IX - INCX 
  150         END DO 
              IF (NOUNIT) TEMP = TEMP / A (1, J) 
              X (JX) = TEMP 
              JX = JX - INCX 
              IF ( (N - J) .GE.K) KX = KX - INCX 
  160       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of STBSV .                                                    
!                                                                       
      END SUBROUTINE STBSV                          
      REAL FUNCTION SASUM (N, SX, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     SASUM takes the sum of the absolute values.                       
!     uses unrolled loops for increment equal to one.                   
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 3/93 to return if incx .le. 0.                           
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL STEMP 
      INTEGER I, M, MP1, NINCX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, MOD 
!     ..                                                                
      SASUM = 0.0e0 
      STEMP = 0.0e0 
      IF (N.LE.0.OR.INCX.LE.0) RETURN 
      IF (INCX.EQ.1) THEN 
!        code for increment equal to 1                                  
!                                                                       
!                                                                       
!        clean-up loop                                                  
!                                                                       
        M = MOD (N, 6) 
        IF (M.NE.0) THEN 
          DO I = 1, M 
          STEMP = STEMP + ABS (SX (I) ) 
          ENDDO 
          IF (N.LT.6) THEN 
            SASUM = STEMP 
            RETURN 
          ENDIF 
        ENDIF 
        MP1 = M + 1 
        DO I = MP1, N, 6 
        STEMP = STEMP + ABS (SX (I) ) + ABS (SX (I + 1) ) + ABS (SX (I +&
        2) ) + ABS (SX (I + 3) ) + ABS (SX (I + 4) ) + ABS (SX (I + 5) )
        ENDDO 
      ELSE 
!                                                                       
!        code for increment not equal to 1                              
!                                                                       
        NINCX = N * INCX 
        DO I = 1, NINCX, INCX 
        STEMP = STEMP + ABS (SX (I) ) 
        ENDDO 
      ENDIF 
      SASUM = STEMP 
      RETURN 
      END FUNCTION SASUM                            
      REAL FUNCTION SDOT (N, SX, INCX, SY, INCY) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, INCY, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ), SY ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     SDOT forms the dot product of two vectors.                        
!     uses unrolled loops for increments equal to one.                  
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL STEMP 
      INTEGER I, IX, IY, M, MP1 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MOD 
!     ..                                                                
      STEMP = 0.0e0 
      SDOT = 0.0e0 
      IF (N.LE.0) RETURN 
      IF (INCX.EQ.1.AND.INCY.EQ.1) THEN 
!                                                                       
!        code for both increments equal to 1                            
!                                                                       
!                                                                       
!        clean-up loop                                                  
!                                                                       
        M = MOD (N, 5) 
        IF (M.NE.0) THEN 
          DO I = 1, M 
          STEMP = STEMP + SX (I) * SY (I) 
          ENDDO 
          IF (N.LT.5) THEN 
            SDOT = STEMP 
            RETURN 
          ENDIF 
        ENDIF 
        MP1 = M + 1 
        DO I = MP1, N, 5 
        STEMP = STEMP + SX (I) * SY (I) + SX (I + 1) * SY (I + 1)       &
        + SX (I + 2) * SY (I + 2) + SX (I + 3) * SY (I + 3) + SX (I + 4)&
        * SY (I + 4)                                                    
        ENDDO 
      ELSE 
!                                                                       
!        code for unequal increments or equal increments                
!          not equal to 1                                               
!                                                                       
        IX = 1 
        IY = 1 
        IF (INCX.LT.0) IX = ( - N + 1) * INCX + 1 
        IF (INCY.LT.0) IY = ( - N + 1) * INCY + 1 
        DO I = 1, N 
        STEMP = STEMP + SX (IX) * SY (IY) 
        IX = IX + INCX 
        IY = IY + INCY 
        ENDDO 
      ENDIF 
      SDOT = STEMP 
      RETURN 
      END FUNCTION SDOT                             
      SUBROUTINE STPMV (UPLO, TRANS, DIAG, N, AP, X, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
      CHARACTER DIAG, TRANS, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL AP ( * ), X ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  STPMV  performs one of the matrix-vector operations                  
!                                                                       
!     x := A*x,   or   x := A**T*x,                                     
!                                                                       
!  where x is an n element vector and  A is an n by n unit, or non-unit,
!  upper or lower triangular matrix, supplied in packed form.           
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the matrix is an upper or  
!           lower triangular matrix as follows:                         
!                                                                       
!              UPLO = 'U' or 'u'   A is an upper triangular matrix.     
!                                                                       
!              UPLO = 'L' or 'l'   A is a lower triangular matrix.      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry, TRANS specifies the operation to be performed as  
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   x := A*x.                           
!                                                                       
!              TRANS = 'T' or 't'   x := A**T*x.                        
!                                                                       
!              TRANS = 'C' or 'c'   x := A**T*x.                        
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  DIAG   - CHARACTER*1.                                                
!           On entry, DIAG specifies whether or not A is unit           
!           triangular as follows:                                      
!                                                                       
!              DIAG = 'U' or 'u'   A is assumed to be unit triangular.  
!                                                                       
!              DIAG = 'N' or 'n'   A is not assumed to be unit          
!                                  triangular.                          
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  AP     - REAL             array of DIMENSION at least                
!           ( ( n*( n + 1 ) )/2 ).                                      
!           Before entry with  UPLO = 'U' or 'u', the array AP must     
!           contain the upper triangular matrix packed sequentially,    
!           column by column, so that AP( 1 ) contains a( 1, 1 ),       
!           AP( 2 ) and AP( 3 ) contain a( 1, 2 ) and a( 2, 2 )         
!           respectively, and so on.                                    
!           Before entry with UPLO = 'L' or 'l', the array AP must      
!           contain the lower triangular matrix packed sequentially,    
!           column by column, so that AP( 1 ) contains a( 1, 1 ),       
!           AP( 2 ) and AP( 3 ) contain a( 2, 1 ) and a( 3, 1 )         
!           respectively, and so on.                                    
!           Note that when  DIAG = 'U' or 'u', the diagonal elements of 
!           A are not referenced, but are assumed to be unity.          
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x. On exit, X is overwritten with the        
!           tranformed vector x.                                        
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!  The vector and matrix arguments are not referenced when N = 0, or M =
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JX, K, KK, KX 
      LOGICAL NOUNIT 
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (.NOT.LSAME (TRANS, 'N') .AND..NOT.LSAME (TRANS, 'T')      &
      .AND..NOT.LSAME (TRANS, 'C') ) THEN                               
        INFO = 2 
      ELSEIF (.NOT.LSAME (DIAG, 'U') .AND..NOT.LSAME (DIAG, 'N') ) THEN 
        INFO = 3 
      ELSEIF (N.LT.0) THEN 
        INFO = 4 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 7 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('STPMV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF (N.EQ.0) RETURN 
!                                                                       
      NOUNIT = LSAME (DIAG, 'N') 
!                                                                       
!     Set up the start point in X if the increment is not unity. This   
!     will be  ( N - 1 )*INCX  too small for descending loops.          
!                                                                       
      IF (INCX.LE.0) THEN 
        KX = 1 - (N - 1) * INCX 
      ELSEIF (INCX.NE.1) THEN 
        KX = 1 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of AP are      
!     accessed sequentially with one pass through AP.                   
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  x:= A*x.                                                 
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          KK = 1 
          IF (INCX.EQ.1) THEN 
            DO 20 J = 1, N 
              IF (X (J) .NE.ZERO) THEN 
                TEMP = X (J) 
                K = KK 
                DO 10 I = 1, J - 1 
                  X (I) = X (I) + TEMP * AP (K) 
                  K = K + 1 
   10           END DO 
                IF (NOUNIT) X (J) = X (J) * AP (KK + J - 1) 
              ENDIF 
              KK = KK + J 
   20       END DO 
          ELSE 
            JX = KX 
            DO 40 J = 1, N 
              IF (X (JX) .NE.ZERO) THEN 
                TEMP = X (JX) 
                IX = KX 
                DO 30 K = KK, KK + J - 2 
                  X (IX) = X (IX) + TEMP * AP (K) 
                  IX = IX + INCX 
   30           END DO 
                IF (NOUNIT) X (JX) = X (JX) * AP (KK + J - 1) 
              ENDIF 
              JX = JX + INCX 
              KK = KK + J 
   40       END DO 
          ENDIF 
        ELSE 
          KK = (N * (N + 1) ) / 2 
          IF (INCX.EQ.1) THEN 
            DO 60 J = N, 1, - 1 
              IF (X (J) .NE.ZERO) THEN 
                TEMP = X (J) 
                K = KK 
                DO 50 I = N, J + 1, - 1 
                  X (I) = X (I) + TEMP * AP (K) 
                  K = K - 1 
   50           END DO 
                IF (NOUNIT) X (J) = X (J) * AP (KK - N + J) 
              ENDIF 
              KK = KK - (N - J + 1) 
   60       END DO 
          ELSE 
            KX = KX + (N - 1) * INCX 
            JX = KX 
            DO 80 J = N, 1, - 1 
              IF (X (JX) .NE.ZERO) THEN 
                TEMP = X (JX) 
                IX = KX 
                DO 70 K = KK, KK - (N - (J + 1) ), - 1 
                  X (IX) = X (IX) + TEMP * AP (K) 
                  IX = IX - INCX 
   70           END DO 
                IF (NOUNIT) X (JX) = X (JX) * AP (KK - N + J) 
              ENDIF 
              JX = JX - INCX 
              KK = KK - (N - J + 1) 
   80       END DO 
          ENDIF 
        ENDIF 
      ELSE 
!                                                                       
!        Form  x := A**T*x.                                             
!                                                                       
        IF (LSAME (UPLO, 'U') ) THEN 
          KK = (N * (N + 1) ) / 2 
          IF (INCX.EQ.1) THEN 
            DO 100 J = N, 1, - 1 
              TEMP = X (J) 
              IF (NOUNIT) TEMP = TEMP * AP (KK) 
              K = KK - 1 
              DO 90 I = J - 1, 1, - 1 
                TEMP = TEMP + AP (K) * X (I) 
                K = K - 1 
   90         END DO 
              X (J) = TEMP 
              KK = KK - J 
  100       END DO 
          ELSE 
            JX = KX + (N - 1) * INCX 
            DO 120 J = N, 1, - 1 
              TEMP = X (JX) 
              IX = JX 
              IF (NOUNIT) TEMP = TEMP * AP (KK) 
              DO 110 K = KK - 1, KK - J + 1, - 1 
                IX = IX - INCX 
                TEMP = TEMP + AP (K) * X (IX) 
  110         END DO 
              X (JX) = TEMP 
              JX = JX - INCX 
              KK = KK - J 
  120       END DO 
          ENDIF 
        ELSE 
          KK = 1 
          IF (INCX.EQ.1) THEN 
            DO 140 J = 1, N 
              TEMP = X (J) 
              IF (NOUNIT) TEMP = TEMP * AP (KK) 
              K = KK + 1 
              DO 130 I = J + 1, N 
                TEMP = TEMP + AP (K) * X (I) 
                K = K + 1 
  130         END DO 
              X (J) = TEMP 
              KK = KK + (N - J + 1) 
  140       END DO 
          ELSE 
            JX = KX 
            DO 160 J = 1, N 
              TEMP = X (JX) 
              IX = JX 
              IF (NOUNIT) TEMP = TEMP * AP (KK) 
              DO 150 K = KK + 1, KK + N - J 
                IX = IX + INCX 
                TEMP = TEMP + AP (K) * X (IX) 
  150         END DO 
              X (JX) = TEMP 
              JX = JX + INCX 
              KK = KK + (N - J + 1) 
  160       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of STPMV .                                                    
!                                                                       
      END SUBROUTINE STPMV                          
      SUBROUTINE SGER (M, N, ALPHA, X, INCX, Y, INCY, A, LDA) 
!     .. Scalar Arguments ..                                            
      REAL ALPHA 
      INTEGER INCX, INCY, LDA, M, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ), Y ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SGER   performs the rank 1 operation                                 
!                                                                       
!     A := alpha*x*y**T + A,                                            
!                                                                       
!  where alpha is a scalar, x is an m element vector, y is an n element 
!  vector and A is an m by n matrix.                                    
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  M      - INTEGER.                                                    
!           On entry, M specifies the number of rows of the matrix A.   
!           M must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( m - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the m    
!           element vector x.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Y      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCY ) ).                              
!           Before entry, the incremented array Y must contain the n    
!           element vector y.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCY   - INTEGER.                                                    
!           On entry, INCY specifies the increment for the elements of  
!           Y. INCY must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry, the leading m by n part of the array A must   
!           contain the matrix of coefficients. On exit, A is           
!           overwritten by the updated matrix.                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           max( 1, m ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ZERO 
      PARAMETER (ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, J, JY, KX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (M.LT.0) THEN 
        INFO = 1 
      ELSEIF (N.LT.0) THEN 
        INFO = 2 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 5 
      ELSEIF (INCY.EQ.0) THEN 
        INFO = 7 
      ELSEIF (LDA.LT.MAX (1, M) ) THEN 
        INFO = 9 
      ENDIF 
      IF (INFO.NE.0) THEN 
      CALL XERBLA ('SGER  ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (M.EQ.0) .OR. (N.EQ.0) .OR. (ALPHA.EQ.ZERO) ) RETURN 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through A.                    
!                                                                       
      IF (INCY.GT.0) THEN 
        JY = 1 
      ELSE 
        JY = 1 - (N - 1) * INCY 
      ENDIF 
      IF (INCX.EQ.1) THEN 
        DO 20 J = 1, N 
          IF (Y (JY) .NE.ZERO) THEN 
            TEMP = ALPHA * Y (JY) 
            DO 10 I = 1, M 
              A (I, J) = A (I, J) + X (I) * TEMP 
   10       END DO 
          ENDIF 
          JY = JY + INCY 
   20   END DO 
      ELSE 
        IF (INCX.GT.0) THEN 
          KX = 1 
        ELSE 
          KX = 1 - (M - 1) * INCX 
        ENDIF 
        DO 40 J = 1, N 
          IF (Y (JY) .NE.ZERO) THEN 
            TEMP = ALPHA * Y (JY) 
            IX = KX 
            DO 30 I = 1, M 
              A (I, J) = A (I, J) + X (IX) * TEMP 
              IX = IX + INCX 
   30       END DO 
          ENDIF 
          JY = JY + INCY 
   40   END DO 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SGER  .                                                    
!                                                                       
      END SUBROUTINE SGER                           
      SUBROUTINE SSYMV (UPLO, N, ALPHA, A, LDA, X, INCX, BETA, Y, INCY) 
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER INCX, INCY, LDA, N 
      CHARACTER UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ), Y ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSYMV  performs the matrix-vector  operation                         
!                                                                       
!     y := alpha*A*x + beta*y,                                          
!                                                                       
!  where alpha and beta are scalars, x and y are n element vectors and  
!  A is an n by n symmetric matrix.                                     
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the upper or lower         
!           triangular part of the array A is to be referenced as       
!           follows:                                                    
!                                                                       
!              UPLO = 'U' or 'u'   Only the upper triangular part of A  
!                                  is to be referenced.                 
!                                                                       
!              UPLO = 'L' or 'l'   Only the lower triangular part of A  
!                                  is to be referenced.                 
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry with  UPLO = 'U' or 'u', the leading n by n    
!           upper triangular part of the array A must contain the upper 
!           triangular part of the symmetric matrix and the strictly    
!           lower triangular part of A is not referenced.               
!           Before entry with UPLO = 'L' or 'l', the leading n by n     
!           lower triangular part of the array A must contain the lower 
!           triangular part of the symmetric matrix and the strictly    
!           upper triangular part of A is not referenced.               
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           max( 1, n ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the n    
!           element vector x.                                           
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry, BETA specifies the scalar beta. When BETA is      
!           supplied as zero then Y need not be set on input.           
!           Unchanged on exit.                                          
!                                                                       
!  Y      - REAL             array of dimension at least                
!           ( 1 + ( n - 1 )*abs( INCY ) ).                              
!           Before entry, the incremented array Y must contain the n    
!           element vector y. On exit, Y is overwritten by the updated  
!           vector y.                                                   
!                                                                       
!  INCY   - INTEGER.                                                    
!           On entry, INCY specifies the increment for the elements of  
!           Y. INCY must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!  The vector and matrix arguments are not referenced when N = 0, or M =
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP1, TEMP2 
      INTEGER I, INFO, IX, IY, J, JX, JY, KX, KY 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (N.LT.0) THEN 
        INFO = 2 
      ELSEIF (LDA.LT.MAX (1, N) ) THEN 
        INFO = 5 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 7 
      ELSEIF (INCY.EQ.0) THEN 
        INFO = 10 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYMV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. ( (ALPHA.EQ.ZERO) .AND. (BETA.EQ.ONE) ) )      &
      RETURN                                                            
!                                                                       
!     Set up the start points in  X  and  Y.                            
!                                                                       
      IF (INCX.GT.0) THEN 
        KX = 1 
      ELSE 
        KX = 1 - (N - 1) * INCX 
      ENDIF 
      IF (INCY.GT.0) THEN 
        KY = 1 
      ELSE 
        KY = 1 - (N - 1) * INCY 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through the triangular part   
!     of A.                                                             
!                                                                       
!     First form  y := beta*y.                                          
!                                                                       
      IF (BETA.NE.ONE) THEN 
        IF (INCY.EQ.1) THEN 
          IF (BETA.EQ.ZERO) THEN 
            DO 10 I = 1, N 
              Y (I) = ZERO 
   10       END DO 
          ELSE 
            DO 20 I = 1, N 
              Y (I) = BETA * Y (I) 
   20       END DO 
          ENDIF 
        ELSE 
          IY = KY 
          IF (BETA.EQ.ZERO) THEN 
            DO 30 I = 1, N 
              Y (IY) = ZERO 
              IY = IY + INCY 
   30       END DO 
          ELSE 
            DO 40 I = 1, N 
              Y (IY) = BETA * Y (IY) 
              IY = IY + INCY 
   40       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
      IF (ALPHA.EQ.ZERO) RETURN 
      IF (LSAME (UPLO, 'U') ) THEN 
!                                                                       
!        Form  y  when A is stored in upper triangle.                   
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 60 J = 1, N 
            TEMP1 = ALPHA * X (J) 
            TEMP2 = ZERO 
            DO 50 I = 1, J - 1 
              Y (I) = Y (I) + TEMP1 * A (I, J) 
              TEMP2 = TEMP2 + A (I, J) * X (I) 
   50       END DO 
            Y (J) = Y (J) + TEMP1 * A (J, J) + ALPHA * TEMP2 
   60     END DO 
        ELSE 
          JX = KX 
          JY = KY 
          DO 80 J = 1, N 
            TEMP1 = ALPHA * X (JX) 
            TEMP2 = ZERO 
            IX = KX 
            IY = KY 
            DO 70 I = 1, J - 1 
              Y (IY) = Y (IY) + TEMP1 * A (I, J) 
              TEMP2 = TEMP2 + A (I, J) * X (IX) 
              IX = IX + INCX 
              IY = IY + INCY 
   70       END DO 
            Y (JY) = Y (JY) + TEMP1 * A (J, J) + ALPHA * TEMP2 
            JX = JX + INCX 
            JY = JY + INCY 
   80     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  y  when A is stored in lower triangle.                   
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 100 J = 1, N 
            TEMP1 = ALPHA * X (J) 
            TEMP2 = ZERO 
            Y (J) = Y (J) + TEMP1 * A (J, J) 
            DO 90 I = J + 1, N 
              Y (I) = Y (I) + TEMP1 * A (I, J) 
              TEMP2 = TEMP2 + A (I, J) * X (I) 
   90       END DO 
            Y (J) = Y (J) + ALPHA * TEMP2 
  100     END DO 
        ELSE 
          JX = KX 
          JY = KY 
          DO 120 J = 1, N 
            TEMP1 = ALPHA * X (JX) 
            TEMP2 = ZERO 
            Y (JY) = Y (JY) + TEMP1 * A (J, J) 
            IX = JX 
            IY = JY 
            DO 110 I = J + 1, N 
              IX = IX + INCX 
              IY = IY + INCY 
              Y (IY) = Y (IY) + TEMP1 * A (I, J) 
              TEMP2 = TEMP2 + A (I, J) * X (IX) 
  110       END DO 
            Y (JY) = Y (JY) + ALPHA * TEMP2 
            JX = JX + INCX 
            JY = JY + INCY 
  120     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYMV .                                                    
!                                                                       
      END SUBROUTINE SSYMV                          
      SUBROUTINE SSWAP (N, SX, INCX, SY, INCY) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, INCY, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ), SY ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     interchanges two vectors.                                         
!     uses unrolled loops for increments equal to 1.                    
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL STEMP 
      INTEGER I, IX, IY, M, MP1 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MOD 
!     ..                                                                
      IF (N.LE.0) RETURN 
      IF (INCX.EQ.1.AND.INCY.EQ.1) THEN 
!                                                                       
!       code for both increments equal to 1                             
!                                                                       
!                                                                       
!       clean-up loop                                                   
!                                                                       
        M = MOD (N, 3) 
        IF (M.NE.0) THEN 
          DO I = 1, M 
          STEMP = SX (I) 
          SX (I) = SY (I) 
          SY (I) = STEMP 
          ENDDO 
          IF (N.LT.3) RETURN 
        ENDIF 
        MP1 = M + 1 
        DO I = MP1, N, 3 
        STEMP = SX (I) 
        SX (I) = SY (I) 
        SY (I) = STEMP 
        STEMP = SX (I + 1) 
        SX (I + 1) = SY (I + 1) 
        SY (I + 1) = STEMP 
        STEMP = SX (I + 2) 
        SX (I + 2) = SY (I + 2) 
        SY (I + 2) = STEMP 
        ENDDO 
      ELSE 
!                                                                       
!       code for unequal increments or equal increments not equal       
!         to 1                                                          
!                                                                       
        IX = 1 
        IY = 1 
        IF (INCX.LT.0) IX = ( - N + 1) * INCX + 1 
        IF (INCY.LT.0) IY = ( - N + 1) * INCY + 1 
        DO I = 1, N 
        STEMP = SX (IX) 
        SX (IX) = SY (IY) 
        SY (IY) = STEMP 
        IX = IX + INCX 
        IY = IY + INCY 
        ENDDO 
      ENDIF 
      RETURN 
      END SUBROUTINE SSWAP                          
      SUBROUTINE SSBMV (UPLO, N, K, ALPHA, A, LDA, X, INCX, BETA, Y,    &
      INCY)                                                             
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER INCX, INCY, K, LDA, N 
      CHARACTER UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ), Y ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSBMV  performs the matrix-vector  operation                         
!                                                                       
!     y := alpha*A*x + beta*y,                                          
!                                                                       
!  where alpha and beta are scalars, x and y are n element vectors and  
!  A is an n by n symmetric band matrix, with k super-diagonals.        
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On entry, UPLO specifies whether the upper or lower         
!           triangular part of the band matrix A is being supplied as   
!           follows:                                                    
!                                                                       
!              UPLO = 'U' or 'u'   The upper triangular part of A is    
!                                  being supplied.                      
!                                                                       
!              UPLO = 'L' or 'l'   The lower triangular part of A is    
!                                  being supplied.                      
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the order of the matrix A.            
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  K      - INTEGER.                                                    
!           On entry, K specifies the number of super-diagonals of the  
!           matrix A. K must satisfy  0 .le. K.                         
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry with UPLO = 'U' or 'u', the leading ( k + 1 )  
!           by n part of the array A must contain the upper triangular  
!           band part of the symmetric matrix, supplied column by       
!           column, with the leading diagonal of the matrix in row      
!           ( k + 1 ) of the array, the first super-diagonal starting at
!           position 2 in row k, and so on. The top left k by k triangle
!           of the array A is not referenced.                           
!           The following program segment will transfer the upper       
!           triangular part of a symmetric band matrix from conventional
!           full matrix storage to band storage:                        
!                                                                       
!                 DO 20, J = 1, N                                       
!                    M = K + 1 - J                                      
!                    DO 10, I = MAX( 1, J - K ), J                      
!                       A( M + I, J ) = matrix( I, J )                  
!              10    CONTINUE                                           
!              20 CONTINUE                                              
!                                                                       
!           Before entry with UPLO = 'L' or 'l', the leading ( k + 1 )  
!           by n part of the array A must contain the lower triangular  
!           band part of the symmetric matrix, supplied column by       
!           column, with the leading diagonal of the matrix in row 1 of 
!           the array, the first sub-diagonal starting at position 1 in 
!           row 2, and so on. The bottom right k by k triangle of the   
!           array A is not referenced.                                  
!           The following program segment will transfer the lower       
!           triangular part of a symmetric band matrix from conventional
!           full matrix storage to band storage:                        
!                                                                       
!                 DO 20, J = 1, N                                       
!                    M = 1 - J                                          
!                    DO 10, I = J, MIN( N, J + K )                      
!                       A( M + I, J ) = matrix( I, J )                  
!              10    CONTINUE                                           
!              20 CONTINUE                                              
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           ( k + 1 ).                                                  
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of DIMENSION at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ).                              
!           Before entry, the incremented array X must contain the      
!           vector x.                                                   
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry, BETA specifies the scalar beta.                   
!           Unchanged on exit.                                          
!                                                                       
!  Y      - REAL             array of DIMENSION at least                
!           ( 1 + ( n - 1 )*abs( INCY ) ).                              
!           Before entry, the incremented array Y must contain the      
!           vector y. On exit, Y is overwritten by the updated vector y.
!                                                                       
!  INCY   - INTEGER.                                                    
!           On entry, INCY specifies the increment for the elements of  
!           Y. INCY must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!  The vector and matrix arguments are not referenced when N = 0, or M =
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP1, TEMP2 
      INTEGER I, INFO, IX, IY, J, JX, JY, KPLUS1, KX, KY, L 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX, MIN 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (UPLO, 'U') .AND..NOT.LSAME (UPLO, 'L') ) THEN 
        INFO = 1 
      ELSEIF (N.LT.0) THEN 
        INFO = 2 
      ELSEIF (K.LT.0) THEN 
        INFO = 3 
      ELSEIF (LDA.LT. (K + 1) ) THEN 
        INFO = 6 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 8 
      ELSEIF (INCY.EQ.0) THEN 
        INFO = 11 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSBMV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. ( (ALPHA.EQ.ZERO) .AND. (BETA.EQ.ONE) ) )      &
      RETURN                                                            
!                                                                       
!     Set up the start points in  X  and  Y.                            
!                                                                       
      IF (INCX.GT.0) THEN 
        KX = 1 
      ELSE 
        KX = 1 - (N - 1) * INCX 
      ENDIF 
      IF (INCY.GT.0) THEN 
        KY = 1 
      ELSE 
        KY = 1 - (N - 1) * INCY 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of the array A 
!     are accessed sequentially with one pass through A.                
!                                                                       
!     First form  y := beta*y.                                          
!                                                                       
      IF (BETA.NE.ONE) THEN 
        IF (INCY.EQ.1) THEN 
          IF (BETA.EQ.ZERO) THEN 
            DO 10 I = 1, N 
              Y (I) = ZERO 
   10       END DO 
          ELSE 
            DO 20 I = 1, N 
              Y (I) = BETA * Y (I) 
   20       END DO 
          ENDIF 
        ELSE 
          IY = KY 
          IF (BETA.EQ.ZERO) THEN 
            DO 30 I = 1, N 
              Y (IY) = ZERO 
              IY = IY + INCY 
   30       END DO 
          ELSE 
            DO 40 I = 1, N 
              Y (IY) = BETA * Y (IY) 
              IY = IY + INCY 
   40       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
      IF (ALPHA.EQ.ZERO) RETURN 
      IF (LSAME (UPLO, 'U') ) THEN 
!                                                                       
!        Form  y  when upper triangle of A is stored.                   
!                                                                       
        KPLUS1 = K + 1 
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 60 J = 1, N 
            TEMP1 = ALPHA * X (J) 
            TEMP2 = ZERO 
            L = KPLUS1 - J 
            DO 50 I = MAX (1, J - K), J - 1 
              Y (I) = Y (I) + TEMP1 * A (L + I, J) 
              TEMP2 = TEMP2 + A (L + I, J) * X (I) 
   50       END DO 
            Y (J) = Y (J) + TEMP1 * A (KPLUS1, J) + ALPHA * TEMP2 
   60     END DO 
        ELSE 
          JX = KX 
          JY = KY 
          DO 80 J = 1, N 
            TEMP1 = ALPHA * X (JX) 
            TEMP2 = ZERO 
            IX = KX 
            IY = KY 
            L = KPLUS1 - J 
            DO 70 I = MAX (1, J - K), J - 1 
              Y (IY) = Y (IY) + TEMP1 * A (L + I, J) 
              TEMP2 = TEMP2 + A (L + I, J) * X (IX) 
              IX = IX + INCX 
              IY = IY + INCY 
   70       END DO 
            Y (JY) = Y (JY) + TEMP1 * A (KPLUS1, J) + ALPHA * TEMP2 
            JX = JX + INCX 
            JY = JY + INCY 
            IF (J.GT.K) THEN 
              KX = KX + INCX 
              KY = KY + INCY 
            ENDIF 
   80     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  y  when lower triangle of A is stored.                   
!                                                                       
        IF ( (INCX.EQ.1) .AND. (INCY.EQ.1) ) THEN 
          DO 100 J = 1, N 
            TEMP1 = ALPHA * X (J) 
            TEMP2 = ZERO 
            Y (J) = Y (J) + TEMP1 * A (1, J) 
            L = 1 - J 
            DO 90 I = J + 1, MIN (N, J + K) 
              Y (I) = Y (I) + TEMP1 * A (L + I, J) 
              TEMP2 = TEMP2 + A (L + I, J) * X (I) 
   90       END DO 
            Y (J) = Y (J) + ALPHA * TEMP2 
  100     END DO 
        ELSE 
          JX = KX 
          JY = KY 
          DO 120 J = 1, N 
            TEMP1 = ALPHA * X (JX) 
            TEMP2 = ZERO 
            Y (JY) = Y (JY) + TEMP1 * A (1, J) 
            L = 1 - J 
            IX = JX 
            IY = JY 
            DO 110 I = J + 1, MIN (N, J + K) 
              IX = IX + INCX 
              IY = IY + INCY 
              Y (IY) = Y (IY) + TEMP1 * A (L + I, J) 
              TEMP2 = TEMP2 + A (L + I, J) * X (IX) 
  110       END DO 
            Y (JY) = Y (JY) + ALPHA * TEMP2 
            JX = JX + INCX 
            JY = JY + INCY 
  120     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSBMV .                                                    
!                                                                       
      END SUBROUTINE SSBMV                          
      SUBROUTINE SSYR2K (UPLO, TRANS, N, K, ALPHA, A, LDA, B, LDB, BETA,&
      C, LDC)                                                           
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER K, LDA, LDB, LDC, N 
      CHARACTER TRANS, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), B (LDB, * ), C (LDC, * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSYR2K  performs one of the symmetric rank 2k operations             
!                                                                       
!     C := alpha*A*B**T + alpha*B*A**T + beta*C,                        
!                                                                       
!  or                                                                   
!                                                                       
!     C := alpha*A**T*B + alpha*B**T*A + beta*C,                        
!                                                                       
!  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
!  and  A and B  are  n by k  matrices  in the  first  case  and  k by n
!  matrices in the second case.                                         
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of the  array  C  is to be  referenced  as
!           follows:                                                    
!                                                                       
!              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
!                                  is to be referenced.                 
!                                                                       
!              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
!                                  is to be referenced.                 
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry,  TRANS  specifies the operation to be performed as
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   C := alpha*A*B**T + alpha*B*A**T +  
!                                        beta*C.                        
!                                                                       
!              TRANS = 'T' or 't'   C := alpha*A**T*B + alpha*B**T*A +  
!                                        beta*C.                        
!                                                                       
!              TRANS = 'C' or 'c'   C := alpha*A**T*B + alpha*B**T*A +  
!                                        beta*C.                        
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry,  N specifies the order of the matrix C.  N must be
!           at least zero.                                              
!           Unchanged on exit.                                          
!                                                                       
!  K      - INTEGER.                                                    
!           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
!           of  columns  of the  matrices  A and B,  and on  entry  with
!           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
!           of rows of the matrices  A and B.  K must be at least  zero.
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, ka ), where ka is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.         
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  A  must contain the matrix  A,  otherwise
!           the leading  k by n  part of the array  A  must contain  the
!           matrix A.                                                   
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDA must be at least  max( 1, n ), otherwise  LDA must
!           be at least  max( 1, k ).                                   
!           Unchanged on exit.                                          
!                                                                       
!  B      - REAL             array of DIMENSION ( LDB, kb ), where kb is
!           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.         
!           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
!           part of the array  B  must contain the matrix  B,  otherwise
!           the leading  k by n  part of the array  B  must contain  the
!           matrix B.                                                   
!           Unchanged on exit.                                          
!                                                                       
!  LDB    - INTEGER.                                                    
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
!           then  LDB must be at least  max( 1, n ), otherwise  LDB must
!           be at least  max( 1, k ).                                   
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry, BETA specifies the scalar beta.                   
!           Unchanged on exit.                                          
!                                                                       
!  C      - REAL             array of DIMENSION ( LDC, n ).             
!           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n 
!           upper triangular part of the array C must contain the upper 
!           triangular part  of the  symmetric matrix  and the strictly 
!           lower triangular part of C is not referenced.  On exit, the 
!           upper triangular part of the array  C is overwritten by the 
!           upper triangular part of the updated matrix.                
!           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n 
!           lower triangular part of the array C must contain the lower 
!           triangular part  of the  symmetric matrix  and the strictly 
!           upper triangular part of C is not referenced.  On exit, the 
!           lower triangular part of the array  C is overwritten by the 
!           lower triangular part of the updated matrix.                
!                                                                       
!  LDC    - INTEGER.                                                    
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, n ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 3 Blas routine.                                                
!                                                                       
!                                                                       
!  -- Written on 8-February-1989.                                       
!     Jack Dongarra, Argonne National Laboratory.                       
!     Iain Duff, AERE Harwell.                                          
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.                   
!     Sven Hammarling, Numerical Algorithms Group Ltd.                  
!                                                                       
!  =====================================================================
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP1, TEMP2 
      INTEGER I, INFO, J, L, NROWA 
      LOGICAL UPPER 
!     ..                                                                
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
        NROWA = N 
      ELSE 
        NROWA = K 
      ENDIF 
      UPPER = LSAME (UPLO, 'U') 
!                                                                       
      INFO = 0 
      IF ( (.NOT.UPPER) .AND. (.NOT.LSAME (UPLO, 'L') ) ) THEN 
        INFO = 1 
      ELSEIF ( (.NOT.LSAME (TRANS, 'N') ) .AND. (.NOT.LSAME (TRANS, 'T')&
      ) .AND. (.NOT.LSAME (TRANS, 'C') ) ) THEN                         
        INFO = 2 
      ELSEIF (N.LT.0) THEN 
        INFO = 3 
      ELSEIF (K.LT.0) THEN 
        INFO = 4 
      ELSEIF (LDA.LT.MAX (1, NROWA) ) THEN 
        INFO = 7 
      ELSEIF (LDB.LT.MAX (1, NROWA) ) THEN 
        INFO = 9 
      ELSEIF (LDC.LT.MAX (1, N) ) THEN 
        INFO = 12 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYR2K', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (N.EQ.0) .OR. ( ( (ALPHA.EQ.ZERO) .OR. (K.EQ.0) ) .AND. (    &
      BETA.EQ.ONE) ) ) RETURN                                           
!                                                                       
!     And when  alpha.eq.zero.                                          
!                                                                       
      IF (ALPHA.EQ.ZERO) THEN 
        IF (UPPER) THEN 
          IF (BETA.EQ.ZERO) THEN 
            DO 20 J = 1, N 
              DO 10 I = 1, J 
                C (I, J) = ZERO 
   10         END DO 
   20       END DO 
          ELSE 
            DO 40 J = 1, N 
              DO 30 I = 1, J 
                C (I, J) = BETA * C (I, J) 
   30         END DO 
   40       END DO 
          ENDIF 
        ELSE 
          IF (BETA.EQ.ZERO) THEN 
            DO 60 J = 1, N 
              DO 50 I = J, N 
                C (I, J) = ZERO 
   50         END DO 
   60       END DO 
          ELSE 
            DO 80 J = 1, N 
              DO 70 I = J, N 
                C (I, J) = BETA * C (I, J) 
   70         END DO 
   80       END DO 
          ENDIF 
        ENDIF 
        RETURN 
      ENDIF 
!                                                                       
!     Start the operations.                                             
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  C := alpha*A*B**T + alpha*B*A**T + C.                    
!                                                                       
        IF (UPPER) THEN 
          DO 130 J = 1, N 
            IF (BETA.EQ.ZERO) THEN 
              DO 90 I = 1, J 
                C (I, J) = ZERO 
   90         END DO 
            ELSEIF (BETA.NE.ONE) THEN 
              DO 100 I = 1, J 
                C (I, J) = BETA * C (I, J) 
  100         END DO 
            ENDIF 
            DO 120 L = 1, K 
              IF ( (A (J, L) .NE.ZERO) .OR. (B (J, L) .NE.ZERO) ) THEN 
                TEMP1 = ALPHA * B (J, L) 
                TEMP2 = ALPHA * A (J, L) 
                DO 110 I = 1, J 
                  C (I, J) = C (I, J) + A (I, L) * TEMP1 + B (I, L)     &
                  * TEMP2                                               
  110           END DO 
              ENDIF 
  120       END DO 
  130     END DO 
        ELSE 
          DO 180 J = 1, N 
            IF (BETA.EQ.ZERO) THEN 
              DO 140 I = J, N 
                C (I, J) = ZERO 
  140         END DO 
            ELSEIF (BETA.NE.ONE) THEN 
              DO 150 I = J, N 
                C (I, J) = BETA * C (I, J) 
  150         END DO 
            ENDIF 
            DO 170 L = 1, K 
              IF ( (A (J, L) .NE.ZERO) .OR. (B (J, L) .NE.ZERO) ) THEN 
                TEMP1 = ALPHA * B (J, L) 
                TEMP2 = ALPHA * A (J, L) 
                DO 160 I = J, N 
                  C (I, J) = C (I, J) + A (I, L) * TEMP1 + B (I, L)     &
                  * TEMP2                                               
  160           END DO 
              ENDIF 
  170       END DO 
  180     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  C := alpha*A**T*B + alpha*B**T*A + C.                    
!                                                                       
        IF (UPPER) THEN 
          DO 210 J = 1, N 
            DO 200 I = 1, J 
              TEMP1 = ZERO 
              TEMP2 = ZERO 
              DO 190 L = 1, K 
                TEMP1 = TEMP1 + A (L, I) * B (L, J) 
                TEMP2 = TEMP2 + B (L, I) * A (L, J) 
  190         END DO 
              IF (BETA.EQ.ZERO) THEN 
                C (I, J) = ALPHA * TEMP1 + ALPHA * TEMP2 
              ELSE 
                C (I, J) = BETA * C (I, J) + ALPHA * TEMP1 + ALPHA *    &
                TEMP2                                                   
              ENDIF 
  200       END DO 
  210     END DO 
        ELSE 
          DO 240 J = 1, N 
            DO 230 I = J, N 
              TEMP1 = ZERO 
              TEMP2 = ZERO 
              DO 220 L = 1, K 
                TEMP1 = TEMP1 + A (L, I) * B (L, J) 
                TEMP2 = TEMP2 + B (L, I) * A (L, J) 
  220         END DO 
              IF (BETA.EQ.ZERO) THEN 
                C (I, J) = ALPHA * TEMP1 + ALPHA * TEMP2 
              ELSE 
                C (I, J) = BETA * C (I, J) + ALPHA * TEMP1 + ALPHA *    &
                TEMP2                                                   
              ENDIF 
  230       END DO 
  240     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYR2K.                                                    
!                                                                       
      END SUBROUTINE SSYR2K                         
      SUBROUTINE SROTMG (SD1, SD2, SX1, SY1, SPARAM) 
!     .. Scalar Arguments ..                                            
      REAL SD1, SD2, SX1, SY1 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SPARAM (5) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS 
!     THE SECOND COMPONENT OF THE 2-VECTOR  (SQRT(SD1)*SX1,SQRT(SD2)*   
!     SY2)**T.                                                          
!     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..          
!                                                                       
!     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0      
!                                                                       
!       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)    
!     H=(          )    (          )    (          )    (          )    
!       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).   
!     LOCATIONS 2-4 OF SPARAM CONTAIN SH11,SH21,SH12, AND SH22          
!     RESPECTIVELY. (VALUES OF 1.E0, -1.E0, OR 0.E0 IMPLIED BY THE      
!     VALUE OF SPARAM(1) ARE NOT STORED IN SPARAM.)                     
!                                                                       
!     THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE   
!     INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE   
!     OF SD1 AND SD2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.    
!                                                                       
!                                                                       
!  Arguments                                                            
!  =========                                                            
!                                                                       
!                                                                       
!  SD1    (input/output) REAL                                           
!                                                                       
!  SD2    (input/output) REAL                                           
!                                                                       
!  SX1    (input/output) REAL                                           
!                                                                       
!  SY1    (input) REAL                                                  
!                                                                       
!                                                                       
!  SPARAM (input/output)  REAL array, dimension 5                       
!     SPARAM(1)=SFLAG                                                   
!     SPARAM(2)=SH11                                                    
!     SPARAM(3)=SH21                                                    
!     SPARAM(4)=SH12                                                    
!     SPARAM(5)=SH22                                                    
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL GAM, GAMSQ, ONE, RGAMSQ, SFLAG, SH11, SH12, SH21, SH22, SP1, &
      SP2, SQ1, SQ2, STEMP, SU, TWO, ZERO                               
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS 
!     ..                                                                
!     .. Data statements ..                                             
!                                                                       
      DATA ZERO, ONE, TWO / 0.E0, 1.E0, 2.E0 / 
      DATA GAM, GAMSQ, RGAMSQ / 4096.E0, 1.67772E7, 5.96046E-8 / 
!     ..                                                                
                                                                        
      IF (SD1.LT.ZERO) THEN 
!        GO ZERO-H-D-AND-SX1..                                          
        SFLAG = - ONE 
        SH11 = ZERO 
        SH12 = ZERO 
        SH21 = ZERO 
        SH22 = ZERO 
!                                                                       
        SD1 = ZERO 
        SD2 = ZERO 
        SX1 = ZERO 
      ELSE 
!        CASE-SD1-NONNEGATIVE                                           
        SP2 = SD2 * SY1 
        IF (SP2.EQ.ZERO) THEN 
          SFLAG = - TWO 
          SPARAM (1) = SFLAG 
          RETURN 
        ENDIF 
!        REGULAR-CASE..                                                 
        SP1 = SD1 * SX1 
        SQ2 = SP2 * SY1 
        SQ1 = SP1 * SX1 
!                                                                       
        IF (ABS (SQ1) .GT.ABS (SQ2) ) THEN 
          SH21 = - SY1 / SX1 
          SH12 = SP2 / SP1 
!                                                                       
          SU = ONE-SH12 * SH21 
!                                                                       
          IF (SU.GT.ZERO) THEN 
            SFLAG = ZERO 
            SD1 = SD1 / SU 
            SD2 = SD2 / SU 
            SX1 = SX1 * SU 
          ENDIF 
        ELSE 
                                                                        
          IF (SQ2.LT.ZERO) THEN 
!              GO ZERO-H-D-AND-SX1..                                    
            SFLAG = - ONE 
            SH11 = ZERO 
            SH12 = ZERO 
            SH21 = ZERO 
            SH22 = ZERO 
!                                                                       
            SD1 = ZERO 
            SD2 = ZERO 
            SX1 = ZERO 
          ELSE 
            SFLAG = ONE 
            SH11 = SP1 / SP2 
            SH22 = SX1 / SY1 
            SU = ONE+SH11 * SH22 
            STEMP = SD2 / SU 
            SD2 = SD1 / SU 
            SD1 = STEMP 
            SX1 = SY1 * SU 
          ENDIF 
        ENDIF 
                                                                        
!     PROCESURE..SCALE-CHECK                                            
        IF (SD1.NE.ZERO) THEN 
          DO WHILE ( (SD1.LE.RGAMSQ) .OR. (SD1.GE.GAMSQ) ) 
          IF (SFLAG.EQ.ZERO) THEN 
            SH11 = ONE 
            SH22 = ONE 
            SFLAG = - ONE 
          ELSE 
            SH21 = - ONE 
            SH12 = ONE 
            SFLAG = - ONE 
          ENDIF 
          IF (SD1.LE.RGAMSQ) THEN 
            SD1 = SD1 * GAM**2 
            SX1 = SX1 / GAM 
            SH11 = SH11 / GAM 
            SH12 = SH12 / GAM 
          ELSE 
            SD1 = SD1 / GAM**2 
            SX1 = SX1 * GAM 
            SH11 = SH11 * GAM 
            SH12 = SH12 * GAM 
          ENDIF 
          ENDDO 
        ENDIF 
                                                                        
        IF (SD2.NE.ZERO) THEN 
          DO WHILE ( (ABS (SD2) .LE.RGAMSQ) .OR. (ABS (SD2) .GE.GAMSQ) ) 
          IF (SFLAG.EQ.ZERO) THEN 
            SH11 = ONE 
            SH22 = ONE 
            SFLAG = - ONE 
          ELSE 
            SH21 = - ONE 
            SH12 = ONE 
            SFLAG = - ONE 
          ENDIF 
          IF (ABS (SD2) .LE.RGAMSQ) THEN 
            SD2 = SD2 * GAM**2 
            SH21 = SH21 / GAM 
            SH22 = SH22 / GAM 
          ELSE 
            SD2 = SD2 / GAM**2 
            SH21 = SH21 * GAM 
            SH22 = SH22 * GAM 
          ENDIF 
          ENDDO 
        ENDIF 
                                                                        
      ENDIF 
                                                                        
      IF (SFLAG.LT.ZERO) THEN 
        SPARAM (2) = SH11 
        SPARAM (3) = SH21 
        SPARAM (4) = SH12 
        SPARAM (5) = SH22 
      ELSEIF (SFLAG.EQ.ZERO) THEN 
        SPARAM (3) = SH21 
        SPARAM (4) = SH12 
      ELSE 
        SPARAM (2) = SH11 
        SPARAM (5) = SH22 
      ENDIF 
                                                                        
  260 CONTINUE 
      SPARAM (1) = SFLAG 
      RETURN 
      END SUBROUTINE SROTMG                         
                                                                        
                                                                        
                                                                        
                                                                        
      SUBROUTINE SGEMV (TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y,   &
      INCY)                                                             
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER INCX, INCY, LDA, M, N 
      CHARACTER TRANS 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), X ( * ), Y ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SGEMV  performs one of the matrix-vector operations                  
!                                                                       
!     y := alpha*A*x + beta*y,   or   y := alpha*A**T*x + beta*y,       
!                                                                       
!  where alpha and beta are scalars, x and y are vectors and A is an    
!  m by n matrix.                                                       
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  TRANS  - CHARACTER*1.                                                
!           On entry, TRANS specifies the operation to be performed as  
!           follows:                                                    
!                                                                       
!              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.            
!                                                                       
!              TRANS = 'T' or 't'   y := alpha*A**T*x + beta*y.         
!                                                                       
!              TRANS = 'C' or 'c'   y := alpha*A**T*x + beta*y.         
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  M      - INTEGER.                                                    
!           On entry, M specifies the number of rows of the matrix A.   
!           M must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the number of columns of the matrix A.
!           N must be at least zero.                                    
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, n ).             
!           Before entry, the leading m by n part of the array A must   
!           contain the matrix of coefficients.                         
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program. LDA must be at least          
!           max( 1, m ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  X      - REAL             array of DIMENSION at least                
!           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'       
!           and at least                                                
!           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.                    
!           Before entry, the incremented array X must contain the      
!           vector x.                                                   
!           Unchanged on exit.                                          
!                                                                       
!  INCX   - INTEGER.                                                    
!           On entry, INCX specifies the increment for the elements of  
!           X. INCX must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry, BETA specifies the scalar beta. When BETA is      
!           supplied as zero then Y need not be set on input.           
!           Unchanged on exit.                                          
!                                                                       
!  Y      - REAL             array of DIMENSION at least                
!           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'       
!           and at least                                                
!           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.                    
!           Before entry with BETA non-zero, the incremented array Y    
!           must contain the vector y. On exit, Y is overwritten by the 
!           updated vector y.                                           
!                                                                       
!  INCY   - INTEGER.                                                    
!           On entry, INCY specifies the increment for the elements of  
!           Y. INCY must not be zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 2 Blas routine.                                                
!  The vector and matrix arguments are not referenced when N = 0, or M =
!                                                                       
!  -- Written on 22-October-1986.                                       
!     Jack Dongarra, Argonne National Lab.                              
!     Jeremy Du Croz, Nag Central Office.                               
!     Sven Hammarling, Nag Central Office.                              
!     Richard Hanson, Sandia National Labs.                             
!                                                                       
!  =====================================================================
!                                                                       
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP 
      INTEGER I, INFO, IX, IY, J, JX, JY, KX, KY, LENX, LENY 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF (.NOT.LSAME (TRANS, 'N') .AND..NOT.LSAME (TRANS, 'T')          &
      .AND..NOT.LSAME (TRANS, 'C') ) THEN                               
        INFO = 1 
      ELSEIF (M.LT.0) THEN 
        INFO = 2 
      ELSEIF (N.LT.0) THEN 
        INFO = 3 
      ELSEIF (LDA.LT.MAX (1, M) ) THEN 
        INFO = 6 
      ELSEIF (INCX.EQ.0) THEN 
        INFO = 8 
      ELSEIF (INCY.EQ.0) THEN 
        INFO = 11 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SGEMV ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (M.EQ.0) .OR. (N.EQ.0) .OR. ( (ALPHA.EQ.ZERO) .AND. (        &
      BETA.EQ.ONE) ) ) RETURN                                           
!                                                                       
!     Set  LENX  and  LENY, the lengths of the vectors x and y, and set 
!     up the start points in  X  and  Y.                                
!                                                                       
      IF (LSAME (TRANS, 'N') ) THEN 
        LENX = N 
        LENY = M 
      ELSE 
        LENX = M 
        LENY = N 
      ENDIF 
      IF (INCX.GT.0) THEN 
        KX = 1 
      ELSE 
        KX = 1 - (LENX - 1) * INCX 
      ENDIF 
      IF (INCY.GT.0) THEN 
        KY = 1 
      ELSE 
        KY = 1 - (LENY - 1) * INCY 
      ENDIF 
!                                                                       
!     Start the operations. In this version the elements of A are       
!     accessed sequentially with one pass through A.                    
!                                                                       
!     First form  y := beta*y.                                          
!                                                                       
      IF (BETA.NE.ONE) THEN 
        IF (INCY.EQ.1) THEN 
          IF (BETA.EQ.ZERO) THEN 
            DO 10 I = 1, LENY 
              Y (I) = ZERO 
   10       END DO 
          ELSE 
            DO 20 I = 1, LENY 
              Y (I) = BETA * Y (I) 
   20       END DO 
          ENDIF 
        ELSE 
          IY = KY 
          IF (BETA.EQ.ZERO) THEN 
            DO 30 I = 1, LENY 
              Y (IY) = ZERO 
              IY = IY + INCY 
   30       END DO 
          ELSE 
            DO 40 I = 1, LENY 
              Y (IY) = BETA * Y (IY) 
              IY = IY + INCY 
   40       END DO 
          ENDIF 
        ENDIF 
      ENDIF 
      IF (ALPHA.EQ.ZERO) RETURN 
      IF (LSAME (TRANS, 'N') ) THEN 
!                                                                       
!        Form  y := alpha*A*x + y.                                      
!                                                                       
        JX = KX 
        IF (INCY.EQ.1) THEN 
          DO 60 J = 1, N 
            IF (X (JX) .NE.ZERO) THEN 
              TEMP = ALPHA * X (JX) 
              DO 50 I = 1, M 
                Y (I) = Y (I) + TEMP * A (I, J) 
   50         END DO 
            ENDIF 
            JX = JX + INCX 
   60     END DO 
        ELSE 
          DO 80 J = 1, N 
            IF (X (JX) .NE.ZERO) THEN 
              TEMP = ALPHA * X (JX) 
              IY = KY 
              DO 70 I = 1, M 
                Y (IY) = Y (IY) + TEMP * A (I, J) 
                IY = IY + INCY 
   70         END DO 
            ENDIF 
            JX = JX + INCX 
   80     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  y := alpha*A**T*x + y.                                   
!                                                                       
        JY = KY 
        IF (INCX.EQ.1) THEN 
          DO 100 J = 1, N 
            TEMP = ZERO 
            DO 90 I = 1, M 
              TEMP = TEMP + A (I, J) * X (I) 
   90       END DO 
            Y (JY) = Y (JY) + ALPHA * TEMP 
            JY = JY + INCY 
  100     END DO 
        ELSE 
          DO 120 J = 1, N 
            TEMP = ZERO 
            IX = KX 
            DO 110 I = 1, M 
              TEMP = TEMP + A (I, J) * X (IX) 
              IX = IX + INCX 
  110       END DO 
            Y (JY) = Y (JY) + ALPHA * TEMP 
            JY = JY + INCY 
  120     END DO 
        ENDIF 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SGEMV .                                                    
!                                                                       
      END SUBROUTINE SGEMV                          
      SUBROUTINE SROT (N, SX, INCX, SY, INCY, C, S) 
!     .. Scalar Arguments ..                                            
      REAL C, S 
      INTEGER INCX, INCY, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ), SY ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     applies a plane rotation.                                         
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL STEMP 
      INTEGER I, IX, IY 
!     ..                                                                
      IF (N.LE.0) RETURN 
      IF (INCX.EQ.1.AND.INCY.EQ.1) THEN 
!                                                                       
!       code for both increments equal to 1                             
!                                                                       
        DO I = 1, N 
        STEMP = C * SX (I) + S * SY (I) 
        SY (I) = C * SY (I) - S * SX (I) 
        SX (I) = STEMP 
        ENDDO 
      ELSE 
!                                                                       
!       code for unequal increments or equal increments not equal       
!         to 1                                                          
!                                                                       
        IX = 1 
        IY = 1 
        IF (INCX.LT.0) IX = ( - N + 1) * INCX + 1 
        IF (INCY.LT.0) IY = ( - N + 1) * INCY + 1 
        DO I = 1, N 
        STEMP = C * SX (IX) + S * SY (IY) 
        SY (IY) = C * SY (IY) - S * SX (IX) 
        SX (IX) = STEMP 
        IX = IX + INCX 
        IY = IY + INCY 
        ENDDO 
      ENDIF 
      RETURN 
      END SUBROUTINE SROT                           
      SUBROUTINE SSYMM (SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB, BETA,  &
      C, LDC)                                                           
!     .. Scalar Arguments ..                                            
      REAL ALPHA, BETA 
      INTEGER LDA, LDB, LDC, M, N 
      CHARACTER SIDE, UPLO 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL A (LDA, * ), B (LDB, * ), C (LDC, * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SSYMM  performs one of the matrix-matrix operations                  
!                                                                       
!     C := alpha*A*B + beta*C,                                          
!                                                                       
!  or                                                                   
!                                                                       
!     C := alpha*B*A + beta*C,                                          
!                                                                       
!  where alpha and beta are scalars,  A is a symmetric matrix and  B and
!  C are  m by n matrices.                                              
!                                                                       
!  Arguments                                                            
!  ==========                                                           
!                                                                       
!  SIDE   - CHARACTER*1.                                                
!           On entry,  SIDE  specifies whether  the  symmetric matrix  A
!           appears on the  left or right  in the  operation as follows:
!                                                                       
!              SIDE = 'L' or 'l'   C := alpha*A*B + beta*C,             
!                                                                       
!              SIDE = 'R' or 'r'   C := alpha*B*A + beta*C,             
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  UPLO   - CHARACTER*1.                                                
!           On  entry,   UPLO  specifies  whether  the  upper  or  lower
!           triangular  part  of  the  symmetric  matrix   A  is  to  be
!           referenced as follows:                                      
!                                                                       
!              UPLO = 'U' or 'u'   Only the upper triangular part of the
!                                  symmetric matrix is to be referenced.
!                                                                       
!              UPLO = 'L' or 'l'   Only the lower triangular part of the
!                                  symmetric matrix is to be referenced.
!                                                                       
!           Unchanged on exit.                                          
!                                                                       
!  M      - INTEGER.                                                    
!           On entry,  M  specifies the number of rows of the matrix  C.
!           M  must be at least zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  N      - INTEGER.                                                    
!           On entry, N specifies the number of columns of the matrix C.
!           N  must be at least zero.                                   
!           Unchanged on exit.                                          
!                                                                       
!  ALPHA  - REAL            .                                           
!           On entry, ALPHA specifies the scalar alpha.                 
!           Unchanged on exit.                                          
!                                                                       
!  A      - REAL             array of DIMENSION ( LDA, ka ), where ka is
!           m  when  SIDE = 'L' or 'l'  and is  n otherwise.            
!           Before entry  with  SIDE = 'L' or 'l',  the  m by m  part of
!           the array  A  must contain the  symmetric matrix,  such that
!           when  UPLO = 'U' or 'u', the leading m by m upper triangular
!           part of the array  A  must contain the upper triangular part
!           of the  symmetric matrix and the  strictly  lower triangular
!           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
!           the leading  m by m  lower triangular part  of the  array  A
!           must  contain  the  lower triangular part  of the  symmetric
!           matrix and the  strictly upper triangular part of  A  is not
!           referenced.                                                 
!           Before entry  with  SIDE = 'R' or 'r',  the  n by n  part of
!           the array  A  must contain the  symmetric matrix,  such that
!           when  UPLO = 'U' or 'u', the leading n by n upper triangular
!           part of the array  A  must contain the upper triangular part
!           of the  symmetric matrix and the  strictly  lower triangular
!           part of  A  is not referenced,  and when  UPLO = 'L' or 'l',
!           the leading  n by n  lower triangular part  of the  array  A
!           must  contain  the  lower triangular part  of the  symmetric
!           matrix and the  strictly upper triangular part of  A  is not
!           referenced.                                                 
!           Unchanged on exit.                                          
!                                                                       
!  LDA    - INTEGER.                                                    
!           On entry, LDA specifies the first dimension of A as declared
!           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
!           LDA must be at least  max( 1, m ), otherwise  LDA must be at
!           least  max( 1, n ).                                         
!           Unchanged on exit.                                          
!                                                                       
!  B      - REAL             array of DIMENSION ( LDB, n ).             
!           Before entry, the leading  m by n part of the array  B  must
!           contain the matrix B.                                       
!           Unchanged on exit.                                          
!                                                                       
!  LDB    - INTEGER.                                                    
!           On entry, LDB specifies the first dimension of B as declared
!           in  the  calling  (sub)  program.   LDB  must  be  at  least
!           max( 1, m ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  BETA   - REAL            .                                           
!           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
!           supplied as zero then C need not be set on input.           
!           Unchanged on exit.                                          
!                                                                       
!  C      - REAL             array of DIMENSION ( LDC, n ).             
!           Before entry, the leading  m by n  part of the array  C must
!           contain the matrix  C,  except when  beta  is zero, in which
!           case C need not be set on entry.                            
!           On exit, the array  C  is overwritten by the  m by n updated
!           matrix.                                                     
!                                                                       
!  LDC    - INTEGER.                                                    
!           On entry, LDC specifies the first dimension of C as declared
!           in  the  calling  (sub)  program.   LDC  must  be  at  least
!           max( 1, m ).                                                
!           Unchanged on exit.                                          
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!  Level 3 Blas routine.                                                
!                                                                       
!  -- Written on 8-February-1989.                                       
!     Jack Dongarra, Argonne National Laboratory.                       
!     Iain Duff, AERE Harwell.                                          
!     Jeremy Du Croz, Numerical Algorithms Group Ltd.                   
!     Sven Hammarling, Numerical Algorithms Group Ltd.                  
!                                                                       
!  =====================================================================
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC MAX 
!     ..                                                                
!     .. Local Scalars ..                                               
      REAL TEMP1, TEMP2 
      INTEGER I, INFO, J, K, NROWA 
      LOGICAL UPPER 
!     ..                                                                
!     .. Parameters ..                                                  
      REAL ONE, ZERO 
      PARAMETER (ONE = 1.0E+0, ZERO = 0.0E+0) 
!     ..                                                                
!                                                                       
!     Set NROWA as the number of rows of A.                             
!                                                                       
      IF (LSAME (SIDE, 'L') ) THEN 
        NROWA = M 
      ELSE 
        NROWA = N 
      ENDIF 
      UPPER = LSAME (UPLO, 'U') 
!                                                                       
!     Test the input parameters.                                        
!                                                                       
      INFO = 0 
      IF ( (.NOT.LSAME (SIDE, 'L') ) .AND. (.NOT.LSAME (SIDE, 'R') ) )  &
      THEN                                                              
        INFO = 1 
      ELSEIF ( (.NOT.UPPER) .AND. (.NOT.LSAME (UPLO, 'L') ) ) THEN 
        INFO = 2 
      ELSEIF (M.LT.0) THEN 
        INFO = 3 
      ELSEIF (N.LT.0) THEN 
        INFO = 4 
      ELSEIF (LDA.LT.MAX (1, NROWA) ) THEN 
        INFO = 7 
      ELSEIF (LDB.LT.MAX (1, M) ) THEN 
        INFO = 9 
      ELSEIF (LDC.LT.MAX (1, M) ) THEN 
        INFO = 12 
      ENDIF 
      IF (INFO.NE.0) THEN 
        CALL XERBLA ('SSYMM ', INFO) 
        RETURN 
      ENDIF 
!                                                                       
!     Quick return if possible.                                         
!                                                                       
      IF ( (M.EQ.0) .OR. (N.EQ.0) .OR. ( (ALPHA.EQ.ZERO) .AND. (        &
      BETA.EQ.ONE) ) ) RETURN                                           
!                                                                       
!     And when  alpha.eq.zero.                                          
!                                                                       
      IF (ALPHA.EQ.ZERO) THEN 
        IF (BETA.EQ.ZERO) THEN 
          DO 20 J = 1, N 
            DO 10 I = 1, M 
              C (I, J) = ZERO 
   10       END DO 
   20     END DO 
        ELSE 
          DO 40 J = 1, N 
            DO 30 I = 1, M 
              C (I, J) = BETA * C (I, J) 
   30       END DO 
   40     END DO 
        ENDIF 
        RETURN 
      ENDIF 
!                                                                       
!     Start the operations.                                             
!                                                                       
      IF (LSAME (SIDE, 'L') ) THEN 
!                                                                       
!        Form  C := alpha*A*B + beta*C.                                 
!                                                                       
        IF (UPPER) THEN 
          DO 70 J = 1, N 
            DO 60 I = 1, M 
              TEMP1 = ALPHA * B (I, J) 
              TEMP2 = ZERO 
              DO 50 K = 1, I - 1 
                C (K, J) = C (K, J) + TEMP1 * A (K, I) 
                TEMP2 = TEMP2 + B (K, J) * A (K, I) 
   50         END DO 
              IF (BETA.EQ.ZERO) THEN 
                C (I, J) = TEMP1 * A (I, I) + ALPHA * TEMP2 
              ELSE 
                C (I, J) = BETA * C (I, J) + TEMP1 * A (I, I) + ALPHA * &
                TEMP2                                                   
              ENDIF 
   60       END DO 
   70     END DO 
        ELSE 
          DO 100 J = 1, N 
            DO 90 I = M, 1, - 1 
              TEMP1 = ALPHA * B (I, J) 
              TEMP2 = ZERO 
              DO 80 K = I + 1, M 
                C (K, J) = C (K, J) + TEMP1 * A (K, I) 
                TEMP2 = TEMP2 + B (K, J) * A (K, I) 
   80         END DO 
              IF (BETA.EQ.ZERO) THEN 
                C (I, J) = TEMP1 * A (I, I) + ALPHA * TEMP2 
              ELSE 
                C (I, J) = BETA * C (I, J) + TEMP1 * A (I, I) + ALPHA * &
                TEMP2                                                   
              ENDIF 
   90       END DO 
  100     END DO 
        ENDIF 
      ELSE 
!                                                                       
!        Form  C := alpha*B*A + beta*C.                                 
!                                                                       
        DO 170 J = 1, N 
          TEMP1 = ALPHA * A (J, J) 
          IF (BETA.EQ.ZERO) THEN 
            DO 110 I = 1, M 
              C (I, J) = TEMP1 * B (I, J) 
  110       END DO 
          ELSE 
            DO 120 I = 1, M 
              C (I, J) = BETA * C (I, J) + TEMP1 * B (I, J) 
  120       END DO 
          ENDIF 
          DO 140 K = 1, J - 1 
            IF (UPPER) THEN 
              TEMP1 = ALPHA * A (K, J) 
            ELSE 
              TEMP1 = ALPHA * A (J, K) 
            ENDIF 
            DO 130 I = 1, M 
              C (I, J) = C (I, J) + TEMP1 * B (I, K) 
  130       END DO 
  140     END DO 
          DO 160 K = J + 1, N 
            IF (UPPER) THEN 
              TEMP1 = ALPHA * A (J, K) 
            ELSE 
              TEMP1 = ALPHA * A (K, J) 
            ENDIF 
            DO 150 I = 1, M 
              C (I, J) = C (I, J) + TEMP1 * B (I, K) 
  150       END DO 
  160     END DO 
  170   END DO 
      ENDIF 
!                                                                       
      RETURN 
!                                                                       
!     End of SSYMM .                                                    
!                                                                       
      END SUBROUTINE SSYMM                          
      SUBROUTINE SROTG (SA, SB, C, S) 
!     .. Scalar Arguments ..                                            
      REAL C, S, SA, SB 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     SROTG construct givens plane rotation.                            
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL R, ROE, SCALE, Z 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, SIGN, SQRT 
!     ..                                                                
      ROE = SB 
      IF (ABS (SA) .GT.ABS (SB) ) ROE = SA 
      SCALE = ABS (SA) + ABS (SB) 
      IF (SCALE.EQ.0.0) THEN 
        C = 1.0 
        S = 0.0 
        R = 0.0 
        Z = 0.0 
      ELSE 
        R = SCALE * SQRT ( (SA / SCALE) **2 + (SB / SCALE) **2) 
        R = SIGN (1.0, ROE) * R 
        C = SA / R 
        S = SB / R 
        Z = 1.0 
        IF (ABS (SA) .GT.ABS (SB) ) Z = S 
        IF (ABS (SB) .GE.ABS (SA) .AND.C.NE.0.0) Z = 1.0 / C 
      ENDIF 
      SA = R 
      SB = Z 
      RETURN 
      END SUBROUTINE SROTG                          
      SUBROUTINE SCOPY (N, SX, INCX, SY, INCY) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, INCY, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ), SY ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     SCOPY copies a vector, x, to a vector, y.                         
!     uses unrolled loops for increments equal to 1.                    
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      INTEGER I, IX, IY, M, MP1 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MOD 
!     ..                                                                
      IF (N.LE.0) RETURN 
      IF (INCX.EQ.1.AND.INCY.EQ.1) THEN 
!                                                                       
!        code for both increments equal to 1                            
!                                                                       
!                                                                       
!        clean-up loop                                                  
!                                                                       
        M = MOD (N, 7) 
        IF (M.NE.0) THEN 
          DO I = 1, M 
          SY (I) = SX (I) 
          ENDDO 
          IF (N.LT.7) RETURN 
        ENDIF 
        MP1 = M + 1 
        DO I = MP1, N, 7 
        SY (I) = SX (I) 
        SY (I + 1) = SX (I + 1) 
        SY (I + 2) = SX (I + 2) 
        SY (I + 3) = SX (I + 3) 
        SY (I + 4) = SX (I + 4) 
        SY (I + 5) = SX (I + 5) 
        SY (I + 6) = SX (I + 6) 
        ENDDO 
      ELSE 
!                                                                       
!        code for unequal increments or equal increments                
!          not equal to 1                                               
!                                                                       
        IX = 1 
        IY = 1 
        IF (INCX.LT.0) IX = ( - N + 1) * INCX + 1 
        IF (INCY.LT.0) IY = ( - N + 1) * INCY + 1 
        DO I = 1, N 
        SY (IY) = SX (IX) 
        IX = IX + INCX 
        IY = IY + INCY 
        ENDDO 
      ENDIF 
      RETURN 
      END SUBROUTINE SCOPY                          
      SUBROUTINE SSCAL (N, SA, SX, INCX) 
!     .. Scalar Arguments ..                                            
      REAL SA 
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     scales a vector by a constant.                                    
!     uses unrolled loops for increment equal to 1.                     
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 3/93 to return if incx .le. 0.                           
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      INTEGER I, M, MP1, NINCX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC MOD 
!     ..                                                                
      IF (N.LE.0.OR.INCX.LE.0) RETURN 
      IF (INCX.EQ.1) THEN 
!                                                                       
!        code for increment equal to 1                                  
!                                                                       
!                                                                       
!        clean-up loop                                                  
!                                                                       
        M = MOD (N, 5) 
        IF (M.NE.0) THEN 
          DO I = 1, M 
          SX (I) = SA * SX (I) 
          ENDDO 
          IF (N.LT.5) RETURN 
        ENDIF 
        MP1 = M + 1 
        DO I = MP1, N, 5 
        SX (I) = SA * SX (I) 
        SX (I + 1) = SA * SX (I + 1) 
        SX (I + 2) = SA * SX (I + 2) 
        SX (I + 3) = SA * SX (I + 3) 
        SX (I + 4) = SA * SX (I + 4) 
        ENDDO 
      ELSE 
!                                                                       
!        code for increment not equal to 1                              
!                                                                       
        NINCX = N * INCX 
        DO I = 1, NINCX, INCX 
        SX (I) = SA * SX (I) 
        ENDDO 
      ENDIF 
      RETURN 
      END SUBROUTINE SSCAL                          
      REAL FUNCTION SCABS1 (Z) 
!     .. Scalar Arguments ..                                            
      COMPLEX Z 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!  SCABS1 computes absolute value of a complex number                   
!                                                                       
!  =====================================================================
!                                                                       
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, AIMAG, REAL 
!     ..                                                                
      SCABS1 = ABS (REAL (Z) ) + ABS (AIMAG (Z) ) 
      RETURN 
      END FUNCTION SCABS1                           
      INTEGER FUNCTION ICAMAX (N, CX, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      COMPLEX CX ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     ICAMAX finds the index of element having max. absolute value.     
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 3/93 to return if incx .le. 0.                           
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL SMAX 
      INTEGER I, IX 
!     ..                                                                
      ICAMAX = 0 
      IF (N.LT.1.OR.INCX.LE.0) RETURN 
      ICAMAX = 1 
      IF (N.EQ.1) RETURN 
      IF (INCX.EQ.1) THEN 
!                                                                       
!        code for increment equal to 1                                  
!                                                                       
        SMAX = SCABS1 (CX (1) ) 
        DO I = 2, N 
        IF (SCABS1 (CX (I) ) .GT.SMAX) THEN 
          ICAMAX = I 
          SMAX = SCABS1 (CX (I) ) 
        ENDIF 
        ENDDO 
      ELSE 
!                                                                       
!        code for increment not equal to 1                              
!                                                                       
        IX = 1 
        SMAX = SCABS1 (CX (1) ) 
        IX = IX + INCX 
        DO I = 2, N 
        IF (SCABS1 (CX (IX) ) .GT.SMAX) THEN 
          ICAMAX = I 
          SMAX = SCABS1 (CX (IX) ) 
        ENDIF 
        IX = IX + INCX 
        ENDDO 
      ENDIF 
      RETURN 
      END FUNCTION ICAMAX                           
      INTEGER FUNCTION ISAMAX (N, SX, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      REAL SX ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     ISAMAX finds the index of element having max. absolute value.     
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 3/93 to return if incx .le. 0.                           
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      REAL SMAX 
      INTEGER I, IX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC ABS 
!     ..                                                                
      ISAMAX = 0 
      IF (N.LT.1.OR.INCX.LE.0) RETURN 
      ISAMAX = 1 
      IF (N.EQ.1) RETURN 
      IF (INCX.EQ.1) THEN 
!                                                                       
!        code for increment equal to 1                                  
!                                                                       
        SMAX = ABS (SX (1) ) 
        DO I = 2, N 
        IF (ABS (SX (I) ) .GT.SMAX) THEN 
          ISAMAX = I 
          SMAX = ABS (SX (I) ) 
        ENDIF 
        ENDDO 
      ELSE 
!                                                                       
!        code for increment not equal to 1                              
!                                                                       
        IX = 1 
        SMAX = ABS (SX (1) ) 
        IX = IX + INCX 
        DO I = 2, N 
        IF (ABS (SX (IX) ) .GT.SMAX) THEN 
          ISAMAX = I 
          SMAX = ABS (SX (IX) ) 
        ENDIF 
        IX = IX + INCX 
        ENDDO 
      ENDIF 
      RETURN 
      END FUNCTION ISAMAX                           

      INTEGER FUNCTION IZAMAX (N, ZX, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      DOUBLE COMPLEX ZX ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     IZAMAX finds the index of element having max. absolute value.     
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, 1/15/85.                                           
!     modified 3/93 to return if incx .le. 0.                           
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      DOUBLE PRECISION DMAX 
      INTEGER I, IX 
!     ..                                                                
      IZAMAX = 0 
      IF (N.LT.1.OR.INCX.LE.0) RETURN 
      IZAMAX = 1 
      IF (N.EQ.1) RETURN 
      IF (INCX.EQ.1) THEN 
!                                                                       
!        code for increment equal to 1                                  
!                                                                       
        DMAX = DCABS1 (ZX (1) ) 
        DO I = 2, N 
        IF (DCABS1 (ZX (I) ) .GT.DMAX) THEN 
          IZAMAX = I 
          DMAX = DCABS1 (ZX (I) ) 
        ENDIF 
        ENDDO 
      ELSE 
!                                                                       
!        code for increment not equal to 1                              
!                                                                       
        IX = 1 
        DMAX = DCABS1 (ZX (1) ) 
        IX = IX + INCX 
        DO I = 2, N 
        IF (DCABS1 (ZX (IX) ) .GT.DMAX) THEN 
          IZAMAX = I 
          DMAX = DCABS1 (ZX (IX) ) 
        ENDIF 
        IX = IX + INCX 
        ENDDO 
      ENDIF 
      RETURN 
      END FUNCTION IZAMAX                           
      INTEGER FUNCTION IDAMAX (N, DX, INCX) 
!     .. Scalar Arguments ..                                            
      INTEGER INCX, N 
!     ..                                                                
!     .. Array Arguments ..                                             
      DOUBLE PRECISION DX ( * ) 
!     ..                                                                
!                                                                       
!  Purpose                                                              
!  =======                                                              
!                                                                       
!     IDAMAX finds the index of element having max. absolute value.     
!                                                                       
!  Further Details                                                      
!  ===============                                                      
!                                                                       
!     jack dongarra, linpack, 3/11/78.                                  
!     modified 3/93 to return if incx .le. 0.                           
!     modified 12/3/93, array(1) declarations changed to array(*)       
!                                                                       
!  =====================================================================
!                                                                       
!     .. Local Scalars ..                                               
      DOUBLE PRECISION DMAX 
      INTEGER I, IX 
!     ..                                                                
!     .. Intrinsic Functions ..                                         
      INTRINSIC DABS 
!     ..                                                                
      IDAMAX = 0 
      IF (N.LT.1.OR.INCX.LE.0) RETURN 
      IDAMAX = 1 
      IF (N.EQ.1) RETURN 
      IF (INCX.EQ.1) THEN 
!                                                                       
!        code for increment equal to 1                                  
!                                                                       
        DMAX = DABS (DX (1) ) 
        DO I = 2, N 
        IF (DABS (DX (I) ) .GT.DMAX) THEN 
          IDAMAX = I 
          DMAX = DABS (DX (I) ) 
        ENDIF 
        ENDDO 
      ELSE 
!                                                                       
!        code for increment not equal to 1                              
!                                                                       
        IX = 1 
        DMAX = DABS (DX (1) ) 
        IX = IX + INCX 
        DO I = 2, N 
        IF (DABS (DX (IX) ) .GT.DMAX) THEN 
          IDAMAX = I 
          DMAX = DABS (DX (IX) ) 
        ENDIF 
        IX = IX + INCX 
        ENDDO 
      ENDIF 
      RETURN 
      END FUNCTION IDAMAX

      DOUBLE PRECISION FUNCTION DCABS1 (Z) 
      !     .. Scalar Arguments ..                                            
      DOUBLE COMPLEX Z 
      !     ..                                                                
      !     ..                                                                
      !  Purpose                                                              
      !  =======                                                              
      !                                                                       
      !  DCABS1 computes absolute value of a double complex number            
      !                                                                       
      !  =====================================================================
      !                                                                       
      !     .. Intrinsic Functions ..                                         
      INTRINSIC ABS, DBLE, DIMAG 
      !                                                                       
        DCABS1 = ABS (DBLE (Z) ) + ABS (DIMAG (Z) ) 
        RETURN 
      END FUNCTION DCABS1    

end module module_fr_sfire_lapack

