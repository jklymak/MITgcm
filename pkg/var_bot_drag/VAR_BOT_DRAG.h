#ifdef ALLOW_VAR_BOT_DRAG

#include "VAR_BOT_DRAG_OPTIONS.h"

      COMMON /VAR_BOT_DRAG_FILES/
     &       Var_Bot_Drag_uLinFile, Var_Bot_Drag_vLinFile,
     &       Var_Bot_Drag_uQuadFile, Var_Bot_Drag_vQuadFile,
     &       Var_Bot_Drag_FacFile
      CHARACTER*(MAX_LEN_FNAM)
     &       Var_Bot_Drag_uLinFile, Var_Bot_Drag_vLinFile,
     &       Var_Bot_Drag_uQuadFile, Var_Bot_Drag_vQuadFile,
     &       Var_Bot_Drag_FacFile

C     2-dim. fields
      COMMON /VAR_BOT_DRAG_FIELDS/
     &  var_bot_dragu_linear, var_bot_dragv_linear,
     &  var_bot_dragu_quadratic, var_bot_dragv_quadratic,
     &  var_bot_stressu, var_bot_stressv,
     &  var_bot_worku, var_bot_workv,
     &  var_bot_drag_fac, var_bot_Nsq
      _RL var_bot_dragu_linear(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var_bot_dragv_linear(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var_bot_dragu_quadratic(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                            nSx,nSy)
      _RL var_bot_dragv_quadratic(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                            nSx,nSy)
      _RL var_bot_stressu(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var_bot_stressv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var_bot_worku(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var_bot_workv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var_bot_nsq(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL var_bot_drag_fac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

C     Scalars:
      COMMON /VAR_BOT_DRAG_SCALARS/
     &  Var_Bot_Drag_LinNPow, Var_Bot_Drag_QuadNPow,
     &  Var_Bot_Drag_NScale, var_bot_drag_cfac
       _RL Var_Bot_Drag_LinNPow, Var_Bot_Drag_QuadNPow,
     &     Var_Bot_Drag_NScale, Var_Bot_Drag_cfac

#endif /* ALLOW_VAR_BOT_DRAG */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
