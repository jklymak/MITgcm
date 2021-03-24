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
      INTEGER i, j, kl, kk
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     UU, NfacL, NfacQ, Nsq, VV, Usq, ddz, ZBL, fac, amp


      IF (k.EQ.1) THEN
C Get the bottom stress...
        DO j=jMin,jMax
          DO i=iMin,iMax
            kl = MIN(kLowC(i,j,bi,bj), kLowC(i-1,j,bi,bj))
            kl = MIN(kl, kLowC(i,j-1,bi,bj))
            IF (kl.GT.0) THEN
C calculate the velocity scale of the free-stream
              kk = kl
              ddz = 0
              UU = 0.0
              VV = 0.0
              DO WHILE ((ddz.LT.var_bot_vert_scale(i,j,bi,bj)*0.8)
     &                  .AND.(kk.GT.0))
                ddz = ddz + drF(kk)
                kk = kk-1
              ENDDO 
              ddz = 0
              DO WHILE ((ddz.LT.
     &                  (var_bot_vert_scale(i,j,bi,bj) * 1.2))
     &                  .AND.(kk.GT.0))
                UU = UU + uVel(i,j,kk,bi,bj) * drF(kk)
                VV = VV + vVel(i,j,kk,bi,bj) * drF(kk)
                ddz = ddz + drF(kk)
                kk = kk-1
              ENDDO
              var_bot_vscale(i,j,bi,bj) = VV / ddz
              var_bot_uscale(i,j,bi,bj) = UU / ddz
C.............If needed, figure out Nsq
              CALL GetNfac(i,j,bi,bj,kl,myThid,NfacL,NfacQ,Nsq)
C DEBUG::::              
              var_bot_nsq(i,j,bi,bj) = Nsq
              IF ((i.EQ.40).AND.(j.EQ.40)) THEN
                WRITE(msgBuf, '(A, I5, I5, E14.6, E14.6, E14.6)')
     &                  'VAR_BOT_DRAGU Nsq',
     &                   k, kl, Nsq  , NfacL, NfacQ 
                CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
              ENDIF

              var_bot_stressu(i,j,bi,bj) = 
     &            -var_bot_dragu_linear(i, j, bi, bj)*NFacL*
     &             var_bot_uscale(i,j,bi,bj)
              var_bot_stressv(i,j,bi,bj) = 
     &            -var_bot_dragv_linear(i, j, bi, bj)*NFacL*
     &             var_bot_vscale(i,j,bi,bj)
C.............Quadratic
              IF (var_bot_dragu_quadratic(i, j, bi, bj).GT.0) THEN
                Usq = var_bot_uscale(i,j,bi,bj)*
     &                var_bot_uscale(i,j,bi,bj) +
     &                var_bot_vscale(i,j,bi,bj)*
     &                var_bot_vscale(i,j,bi,bj)                
                var_bot_stressu(i,j,bi,bj) = 
     &            var_bot_stressu(i,j,bi,bj)  
     &            -(var_bot_dragu_quadratic(i, j, bi, bj)
     &             * SQRT(Usq) * NFacQ * var_bot_uscale(i,j,bi,bj))
                var_bot_stressv(i,j,bi,bj) = 
     &            var_bot_stressv(i,j,bi,bj)  
     &            -(var_bot_dragv_quadratic(i, j, bi, bj)
     &             * SQRT(Usq) * NFacQ * var_bot_vscale(i,j,bi,bj))
              ENDIF
              var_bot_worku(i,j,bi,bj) = -var_bot_stressu(i,j,bi,bj)*
     &            var_bot_uscale(i,j,bi,bj)
              var_bot_workv(i,j,bi,bj) = -var_bot_stressv(i,j,bi,bj)*
     &            var_bot_vscale(i,j,bi,bj)
            ELSE
              var_bot_stressu(i,j,bi,bj) = 0.0
              var_bot_stressv(i,j,bi,bj) = 0.0
            ENDIF
            IF ((i.EQ.40).AND.(j.EQ.40)) THEN
              WRITE(msgBuf,
     & '(A, I5, I5, E14.6, E14.6, F10.4, F10.4, F10.4, F10.4, F10.4)')
     &                  'VAR_BOT_DRAGU ij',
     &                   k, kl, var_bot_stressu(i,j,bi,bj), 
     &                   var_bot_stressv(i,j,bi,bj), 
     &                   var_bot_dragu_quadratic(i, j, bi, bj),
     &                   var_bot_dragu_linear(i, j, bi, bj),
     &                   var_bot_uscale(i,j,bi,bj),
     &                   var_bot_vscale(i,j,bi,bj),
     &                   var_bot_vert_scale(i,j,bi,bj)     
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
            ENDIF

          ENDDO
        ENDDO
C.....Done getting the bottom stress
      ENDIF

      DO j=jMin,jMax
       DO i=iMin,iMax
         ZBL = rC(k) - R_low(i,j,bi,bj) 
         IF (ZBL.LT.var_bot_vert_scale(i,j,bi,bj)) THEN
C         fac = ZBL*ZBL*4.0/var_bot_vert_scale(i,j,bi,bj)/
C     &           var_bot_vert_scale(i,j,bi,bj)
C         fac = EXP(-fac)
C         amp = var_bot_stressu(i,j,bi,bj)*4.0/
C     &         var_bot_vert_scale(i,j,bi,bj)/1.772
          amp = var_bot_stressu(i,j,bi,bj) 
          amp = amp / var_bot_vert_scale(i,j,bi,bj)
          gU_arr(i,j) = gU_arr(i,j) + maskW(i,j,k,bi,bj)*amp
        ENDIF
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
      INTEGER i, j, kl, kk
      _RL     ZBL, fac, amp

C bottom stress is calculated above in ...APPLY_U

C.....Apply the bottom stress, weighted by teh cell thickness.
      DO j=jMin,jMax
       DO i=iMin,iMax
         ZBL = rC(k) - R_low(i,j,bi,bj)
         IF (ZBL.LT.var_bot_vert_scale(i,j,bi,bj)) THEN 
C         fac = ZBL*ZBL*4.0/var_bot_vert_scale(i,j,bi,bj)/
C     &           var_bot_vert_scale(i,j,bi,bj)
C         fac = EXP(-fac)
C         amp = var_bot_stressv(i,j,bi,bj) * 4.0/
C     &         var_bot_vert_scale(i,j,bi,bj)/1.772
          amp = var_bot_stressv(i,j,bi,bj) 
          amp = amp / var_bot_vert_scale(i,j,bi,bj)
          gV_arr(i,j) = gV_arr(i,j) + maskW(i,j,k,bi,bj)*amp
C          gv_arr(i,j) = gv_arr(i,j) + maskS(i,j,k,bi,bj)*amp*fac
         endif
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
          Nsq = 1.0E-6
        ENDIF
      ELSE
        Nsq = 1.E-6
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
