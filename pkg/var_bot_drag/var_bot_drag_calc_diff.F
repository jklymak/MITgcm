#include "VAR_BOT_DRAG_OPTIONS.h"
      SUBROUTINE VAR_BOT_DRAG_CALC_DIFF(
     I        bi,bj,iMin,iMax,jMin,jMax,kArg,kSize,
     U        KappaRx,
     I        myThid)

C     *==========================================================*
C     | SUBROUTINE VAR_BOT_DRAG_CALC_DIFF                        |
C     | o Add contrubution to net diffusivity from bottom drag   |
C     *==========================================================*
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_3D_DIFFKR
# include "DYNVARS.h"
#endif
#ifdef ALLOW_VAR_BOT_DRAG
#include "VAR_BOT_DRAG.h"
#endif

C     == Routine arguments ==
C     bi, bj,   :: tile indices
C     iMin,iMax :: Range of points for which calculation is done
C     jMin,jMax :: Range of points for which calculation is done
C     kArg      :: = 0 -> do the k-loop here and treat all levels
C                  > 0 -> k-loop is done outside and treat only level k=kArg
C     kSize     :: 3rd Dimension of the vertical diffusivity array KappaRx
C     KappaRx   :: vertical diffusivity array
C     myThid    :: Instance number for this innvocation of KL10_CALC_DIFF

      INTEGER bi,bj,iMin,iMax,jMin,jMax,kArg,kSize
      _RL KappaRx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,kSize)
      INTEGER myThid

#ifdef ALLOW_VAR_BOT_DRAG

C     == Local variables ==
C     i,j,k     :: Loop counters
      INTEGER i,j,k
      _RL KC

C--   Add Vertical diffusivity contribution from KL10
      IF ( kArg .EQ. 0 ) THEN
C-    do all levels :
        DO k=1,MIN(Nr,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
            IF ((var_bot_nsq(i,j,bi,bj).GT.0).AND.
     &          (rC(k).LT.
     &           (R_low(i,j,bi,bj)+var_bot_vert_scale(i,j,bi,bj))))
     &         THEN
              KC = var_bot_worku(i,j,bi,bj) + 
     &          var_bot_workv(i,j,bi,bj)
              KC = KC*0.2 * drC(k)/var_bot_vert_scale(i,j,bi,bj) 
              KC = KC/var_bot_nsq(i,j,bi,bj)
            ELSE
              KC = 0.0
            ENDIF
            KappaRx(i,j,k) = KappaRx(i,j,k)
     &                  +(KC
#ifdef ALLOW_3D_DIFFKR
     &                     - diffKr(i,j,k,bi,bj) )
#else
     &                     - diffKrNrS(k) )
#endif
          ENDDO
         ENDDO
        ENDDO
      ELSE
C-    do level k=kArg only :
         k = MIN(kArg,kSize)
         DO j=jMin,jMax
          DO i=iMin,iMax
            IF ((var_bot_nsq(i,j,bi,bj).GT.0).AND.
     &          (rC(k).LT.
     &           (R_low(i,j,bi,bj)+var_bot_vert_scale(i,j,bi,bj))))
     &         THEN
              KC = var_bot_worku(i,j,bi,bj) + 
     &          var_bot_workv(i,j,bi,bj)
              KC = KC*0.2 * drC(k)/var_bot_vert_scale(i,j,bi,bj) 
              KC = KC/var_bot_nsq(i,j,bi,bj)
            ELSE
              KC = 0.0
            ENDIF
            KappaRx(i,j,k) = KappaRx(i,j,k)
     &                  +(KC
#ifdef ALLOW_3D_DIFFKR
     &                     - diffKr(i,j,k,bi,bj) )
#else
     &                     - diffKrNrS(k) )
#endif
          ENDDO
         ENDDO
      ENDIF

#endif /* ALLOW_VAR_BOT_DRAG */

      RETURN
      END
