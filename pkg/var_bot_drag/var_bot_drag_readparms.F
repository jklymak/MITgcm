#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: VAR_BOT_DRAG_READPARMS

C !INTERFACE:
      SUBROUTINE VAR_BOT_DRAG_READPARMS( myThid )

C     !DESCRIPTION:
C     Initialize MYPACKAGE variables and constants.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "VAR_BOT_DRAG.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

#ifdef ALLOW_VAR_BOT_DRAG

C     !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
C     iUnit      :: Work variable for IO unit number
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER iUnit

      NAMELIST /VAR_BOT_DRAG_PARM01/
     &       Var_Bot_Drag_uLinFile, Var_Bot_Drag_vLinFile,
     &       Var_Bot_Drag_uQuadFile, Var_Bot_Drag_vQuadFile,
     &       Var_Bot_Drag_FacFile,
     &       Var_Bot_Drag_LinNPow, Var_Bot_Drag_QuadNPow,
     &       Var_Bot_Drag_NScale

      IF ( .NOT.useVar_Bot_Drag ) THEN
C-    pkg MYPACKAGE is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.mypackage is found
         CALL PACKAGES_UNUSED_MSG( 'useVar_Bot_Drag', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C--   Default values for MYPACKAGE
      Var_Bot_Drag_uLinFile = ' '
      Var_Bot_Drag_vLinFile = ' '
      Var_Bot_Drag_uQuadFile = ' '
      Var_Bot_Drag_vQuadFile = ' '
      Var_Bot_Drag_FacFile = ' '


      WRITE(msgBuf,'(AA)')
     &      'VAR_BOT_DRAG_READPARMS: opening data.var_bot_drag'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                     'data.var_bot_drag',
     I                     'VAR_BOT_DRAG_READPARMS',
     O                     iUnit,
     I                     myThid )

C     Read parameters from open data file
      READ(UNIT=iUnit,NML=VAR_BOT_DRAG_PARM01)
      WRITE(msgBuf,'(A)')
     &    'VAR_BOT_DRAG_READPARMS: finished reading data.var_bot_drag'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &    SQUEEZE_RIGHT , 1)
C     Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_VAR_BOT_DRAG */

      RETURN
      END
