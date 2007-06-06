      FUNCTION TIMEF()
      REAL*8 TIMEF
      INTEGER IC, IR
      CALL SYSTEM_CLOCK(COUNT=IC, COUNT_RATE=IR)
      print*,ic,ir
      TIMEF=REAL(IC)/REAL(IR) * 1000.0
      END
      real*8 timef
      print*,timef()
      end
