#include "VAR_BOT_DRAG_OPTIONS.h"
      SUBROUTINE VAR_BOT_DRAG_CALC_VISC(
     I        bi,bj,iMin,iMax,jMin,jMax,k,
     U        KappaRU,KappaRV,
     I        myThid)

CBOP
C     *==========================================================*
C     | SUBROUTINE KL10_CALC_VISC                                |
C     | o Add contrubution to net viscosity from KL10 mixing     |
C     *==========================================================*
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "VAR_BOT_DRAG.h"

C     == Routine arguments ==
C     bi, bj :: tile numbers
C     iMin, iMax, jMin, jMax :: Range of points for which calculation
C     maskUp :: land/water mask for Wvel points (above tracer level)
C     myThid :: Instance number for this innvocation of KL10_CALC_VISC
C
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      _RL KappaRU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL KappaRV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

#ifdef ALLOW_VAR_BOT_DRAG

C     == Local variables ==
C     i, j    :: Loop counters
      INTEGER i,j
      _RL KU, KC, KV
CEOP
      DO j=jMin,jMax
       DO i=iMin,iMax
         KC = var_bot_worku(i,j,bi,bj)+var_bot_workv(i,j,bi,bj)
         KC = KC*0.2*var_bot_drag_fac(i,j,k,bi,bj) 
         IF (var_bot_nsq(i,j,bi,bj).GT.0) THEN
            KC= KC/var_bot_nsq(i,j,bi,bj)
         ELSE
            KC = 0.0
         ENDIF
         KU = var_bot_worku(i-1,j,bi,bj)+var_bot_workv(i-1,j,bi,bj)
         KU = KU*0.2*var_bot_drag_fac(i-1,j,k,bi,bj) 
         IF (var_bot_nsq(i,j,bi,bj).GT.0) THEN
           KU = KU/var_bot_nsq(i,j,bi,bj)
         ELSE
            KU = 0.0
         ENDIF
         KV = var_bot_worku(i,j-1,bi,bj)+var_bot_workv(i,j-1,bi,bj)
         KV = KV*0.2*var_bot_drag_fac(i,j-1,k,bi,bj) 
         IF (var_bot_nsq(i,j,bi,bj).GT.0) THEN
            KV = KV/var_bot_nsq(i,j,bi,bj)
         ELSE
            KV = 0.0
         ENDIF
         KappaRU(i,j,k) = max ( KappaRU(i,j,k),
     &         KappaRU(i,j,k) - viscArNr(k) + _maskW(i,j,k,bi,bj) *
     &         0.5*(KC+KU) )
         KappaRV(i,j,k) = max ( KappaRV(i,j,k),
     &         KappaRV(i,j,k) - viscArNr(k) + _maskS(i,j,k,bi,bj) *
     &         0.5*(KC+KV) )
       ENDDO
      ENDDO

#endif /* ALLOW_VAR_BOT_DRAG */

      RETURN
      END
