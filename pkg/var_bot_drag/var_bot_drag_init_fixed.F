#include "VAR_BOT_DRAG_OPTIONS.h"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_INIT_FIXED

C !INTERFACE:
      SUBROUTINE VAR_BOT_DRAG_INIT_FIXED( myThid )

C     !DESCRIPTION:
C     Initialize VAR_BOT_DRAG variables that are kept fixed during the run.

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "VAR_BOT_DRAG.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===

      IF ( debugLevel.GE.debLevZero ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A)')
     &         ' VAR_BOT_DRAG_INIT: Reading drag files'
        _END_MASTER(myThid)
      ENDIF

      CALL READ_FLD_XY_RL(Var_Bot_Drag_uLinFile ,' ',
     &                       var_bot_dragu_linear, 0, myThid )
      CALL EXCH_XY_RL( var_bot_dragu_linear, myThid )

      CALL READ_FLD_XY_RL(Var_Bot_Drag_vLinFile ,' ',
     &                       var_bot_dragv_linear, 0, myThid )
      CALL EXCH_XY_RL( var_bot_dragv_linear, myThid )

      CALL READ_FLD_XY_RL(Var_Bot_Drag_uQuadFile ,' ',
     &                       var_bot_dragu_quadratic, 0, myThid )
      CALL EXCH_XY_RL( var_bot_dragu_quadratic, myThid )

      CALL READ_FLD_XY_RL(Var_Bot_Drag_vQuadFile ,' ',
     &                       var_bot_dragv_quadratic, 0, myThid )
      CALL EXCH_XY_RL( var_bot_dragv_quadratic, myThid )

      IF ( debugLevel.GE.debLevZero ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A)')
     &         ' VAR_BOT_DRAG_INIT: Done Reading drag files'
        _END_MASTER(myThid)
      ENDIF

#ifdef VAR_BOT_DRAG_SPREAD
      IF ( debugLevel.GE.debLevZero ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A)')
     &         ' VAR_BOT_DRAG_INIT: Reading drag factor file'
        _END_MASTER(myThid)
      ENDIF
      CALL READ_FLD_XYZ_RL(Var_Bot_Drag_FacFile, ' ',
     &                var_bot_drag_fac, 0, myThid )
      CALL EXCH_XYZ_RL(var_bot_drag_fac, myThid )
      IF ( debugLevel.GE.debLevZero ) THEN
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A)')
     &         ' VAR_BOT_DRAG_INIT: Done drag factor file'
        _END_MASTER(myThid)
      ENDIF
#endif

      RETURN
      END
