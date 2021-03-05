#include "VAR_BOT_DRAG_OPTIONS.h"
C--  File mypackage_tendency_apply.F: Routines to apply MYPACKAGE tendencies
C--   Contents
C--   o MYPACKAGE_TENDENCY_APPLY_U
C--   o MYPACKAGE_TENDENCY_APPLY_V
C--   o MYPACKAGE_TENDENCY_APPLY_T
C--   o MYPACKAGE_TENDENCY_APPLY_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: VAR_BOT_DRAG_TENDENCY_APPLY_U

C !INTERFACE:
      SUBROUTINE VAR_BOT_DRAG_TENDENCY_APPLY_U(
     U                     gU_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add mypackage tendency terms to U tendency.  Routine works for one
C     level at a time. Assumes that U and V tendencies are on C-Grid or
C     on A-grid.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "VAR_BOT_DRAG.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gU_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gU_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j, kl
      _RL     UU, NfacL, NfacQ

      IF (k.EQ.1) THEN
C Get the bottom stress...
        DO j=jMin,jMax
          DO i=iMin,iMax
            IF (kLowC(i,j,bi,bj).GT.0) THEN
              kl = kLowC(i,j,bi,bj)
C var_bot_drag_cfac allows some smoothing...
              var_bot_stressu(i,j,bi,bj) = 
     &           var_bot_stressu(i,j,bi,bj)*(1.0-Var_Bot_Drag_cfac)
C.............If needed, figure out Nsq
              CALL GetNfac(i, j, bi, bj, kl, myThid, NfacL, NfacQ)
              var_bot_stressu(i,j,bi,bj) = var_bot_stressu(i,j,bi,bj) 
     &            +Var_Bot_Drag_cfac*
     &            (-var_bot_dragu_linear(i, j, bi, bj)*NFacL*
     &             uVel(i, j, kl, bi, bj))
C.............Quadratic
              IF (var_bot_dragu_quadratic(i, j, bi, bj).GT.0) THEN
                UU = vVel(i,j,kl,bi,bj)*vVel(i,j,kl,bi,bj)
     &             +uVel(i,j,kl,bi,bj)*uVel(i,j,kl,bi,bj)
                var_bot_stressu(i,j,bi,bj) = var_bot_stressu(i,j,bi,bj)  
     &            +Var_Bot_Drag_cfac * 
     &            (-var_bot_dragu_quadratic(i, j, bi, bj)
     &             * SQRT(UU) * NFacQ * uVel(i,j,kl,bi,bj))
              ENDIF
              var_bot_worku(i,j,bi,bj) = -var_bot_stressu(i,j,bi,bj)*
     &            uVel(i,j,kl,bi,bj)
            ELSE
              var_bot_stressu(i,j,bi,bj) = 0.0
            ENDIF
          ENDDO
        ENDDO
C.....Done getting the bottom stress
      ENDIF

C.....Apply the bottom stress, weighted by the cell thickness.
C.....Note that var_bot_drag_fac integrates to one in the vertical...
      DO j=jMin,jMax
       DO i=iMin,iMax
          gU_arr(i,j) = gU_arr(i,j)
     &     + maskW(i,j,k,bi,bj) * var_bot_stressu(i,j,bi,bj)
     &     * var_bot_drag_fac(i,j,k,bi,bj) * drF(k)
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_V

C !INTERFACE:
      SUBROUTINE VAR_BOT_DRAG_TENDENCY_APPLY_V(
     U                     gV_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add mypackage tendency terms to V tendency.  Routine works for one
C     level at a time. Assumes that U and V tendencies are on C-Grid or
C     on A-grid.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "VAR_BOT_DRAG.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gV_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gV_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      CHARACTER*(MAX_LEN_MBUF) msgBuf

CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j, kl
      _RL     UU, NfacL, NfacQ, Nsq

      IF (k.EQ.1) THEN
C Get the bottom stress...
        DO j=jMin,jMax
          DO i=iMin,iMax
            IF (kLowC(i,j,bi,bj).GT.0) THEN
              kl = kLowC(i,j,bi,bj)
              var_bot_stressv(i,j,bi,bj) = 
     &          var_bot_stressv(i,j,bi,bj)*(1.0-Var_Bot_Drag_cfac) 
C.............If needed, figure out Nsq
              CALL GetNfac(i,j,bi,bj,kl,myThid,NfacL,NfacQ,Nsq)
              var_bot_nsq(i,j,bi,bj) = Nsq 
C             WRITE(msgBuf,'(A, I5, I5, F10.2, F10.2)')
C     &                  'VAR_BOT_DRAG_Nsq',
C     &                   0, kl, NfacL, NfacQ
C             CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT , 1)

              var_bot_stressv(i,j,bi,bj) = var_bot_stressv(i,j,bi,bj)
     &               +Var_Bot_Drag_cfac *
     &           (-var_bot_dragv_linear(i, j, bi, bj) * NfacL *
     &          vVel(i,j,kl,bi,bj))
C.............  To do: Quadratic....
              IF (var_bot_dragv_quadratic(i, j, bi, bj).GT.0) THEN
                UU = vVel(i,j,kl,bi,bj)*vVel(i,j,kl,bi,bj)
     &              +uVel(i,j,kl,bi,bj)*uVel(i,j,kl,bi,bj)
                var_bot_stressv(i,j,bi,bj)
     &             = var_bot_stressv(i,j,bi,bj)
     &               +Var_Bot_Drag_cfac *
     &              (- var_bot_dragv_quadratic(i, j, bi, bj)
     &               * SQRT(UU) * NfacQ * vVel(i,j,kl,bi,bj))
              ENDIF
              var_bot_workv(i,j,bi,bj) = -var_bot_stressv(i,j,bi,bj) *
     &            vVel(i,j,kl,bi,bj)
            ELSE
              var_bot_stressv(i,j,bi,bj) = 0.0
            ENDIF
          ENDDO
        ENDDO
C.....Done getting the bottom stress
      ENDIF

C.....Apply the bottom stress, weighted by teh cell thickness.
C.....Note that var_bot_drag_fac integrates to one in the vertical...
      DO j=jMin,jMax
       DO i=iMin,iMax
          gv_arr(i,j) = gv_arr(i,j)
     &     + maskS(i,j,k,bi,bj) * var_bot_stressv(i,j,bi,bj)
     &     * var_bot_drag_fac(i,j,k,bi,bj) * drF(k)
       ENDDO
      ENDDO

      RETURN
      END

C........................................................
      SUBROUTINE GetNfac(i, j, bi, bj, kl, myThid, NfacL, 
     &                   NfacQ, Nsq)
C.......Get Nsq for the drag coefficients if they have an N dependence.
C.......NfacL = Nsq ** (LinNpow/2)  where
C.......Nsq = (max(rho) - min(rho)) / g / (ztop-zbottom)/rho_nil
C.......where the max and min are taken over the grid cells spanning
C.......Var_Bot_Drag_NScale.  (Max and min avoid any issues with
C.......overturns).

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "VAR_BOT_DRAG.h"
#include "PARAMS.h"


      INTEGER i, j, kl, bi, bj, myThid
      _RL     NfacL, NfacQ, Nsq
C     Local
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER kup
      _RL     rhoup, rhodown, rhotest

      IF ((Var_Bot_Drag_LinNPow.NE.0).OR.
     &            (Var_Bot_Drag_QuadNPow.NE.0)) THEN
        kup = kl
        CALL FIND_RHO_SCALAR(theta(i,j,kl,bi,bj),
     &               salt(i,j,kl,bi,bj),totPhiHyd(i,j,kl,bi,bj),
     &               rhodown,myThid )
        rhoup = rhodown
C.......keep moving upper up until it is big enough (or we hit the top)
        DO WHILE (((rC(kup) - rC(kl)).LT.Var_Bot_Drag_NScale)
     &                       .AND.(kup.GT.1))
C          WRITE(msgBuf,'(A, I5, I5, F10.2, F10.2)') 'VAR_BOT_DRAG_Nsq',
C     &                    kup, kl, rC(kup), rC(kL)
C          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT , 1)
          kup = kup - 1
          CALL FIND_RHO_SCALAR(theta(i,j,kup,bi,bj),
     &                salt(i,j,kup,bi,bj),totPhiHyd(i,j,kup,bi,bj),
     &                rhotest, myThid )

          if (rhotest.LT.rhoup) THEN
            rhoup = rhotest
          endif
          if (rhotest.GT.rhodown) THEN
            rhodown = rhotest
          endif
        ENDDO
        IF (kup.LT.kl) THEN
C.......calc Nsq

          Nsq = gravity*(rhodown - rhoup)/(rC(kup) - rC(kl))
          Nsq = Nsq / rhoNil
C          WRITE(msgBuf,
C     &           '(A, I5, I5, F10.2, F10.2, F10.4, F10.4, F10.6)')
C     &                  'VAR_BOT_DRAG_Nsq',
C     &                    kup, kl, rC(kup), rC(kL), rhodown,
C     &                    rhoup, SQRT(Nsq)
C          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT , 1)
        ELSE
          Nsq = 1.0
        ENDIF
      ELSE
        Nsq = 1.0
      ENDIF

C.....Calculate the factors, or return 1 if they aren't being used.
      IF (Var_Bot_Drag_LinNPow.NE.0) THEN
        IF (Nsq.LE.0) THEN
          NfacL = 0.0
        ELSE IF (Var_Bot_Drag_LinNPow.EQ.1.0) THEN
          NfacL = SQRT(Nsq)
        ELSE
          NfacL = Nsq**(Var_Bot_Drag_LinNPow/2.0)
        ENDIF
      ELSE
        NfacL = 1.0
      ENDIF
      IF (Var_Bot_Drag_QuadNPow.NE.0) THEN
        IF (Nsq.LE.0) THEN
          NfacQ = 0.0
        ELSE IF (Var_Bot_Drag_QuadNPow.EQ.1.0) THEN
          NfacQ = SQRT(Nsq)
        ELSE
          NfacQ = Nsq**(Var_Bot_Drag_QuadNPow/2.0)
        ENDIF
      ELSE
        NfacQ = 1.0
      ENDIF
C      WRITE(msgBuf,
C     &           '(A, F10.6, F10.6, F10.6)')
C     &                  'VAR_BOT_DRAG_Nsq2',
C     &                    sqrt(Nsq), NfacL, NfacQ
C          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
C     &                    SQUEEZE_RIGHT , 1)


      RETURN
      END
