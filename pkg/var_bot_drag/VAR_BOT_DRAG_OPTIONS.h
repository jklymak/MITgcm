C CPP options file for VAR_BOT_DRAG
C Use this file for selecting options within package "var_bot_drag"

#ifndef VAR_BOT_DRAG_OPTIONS_H
#define VAR_BOT_DRAG_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_VAR_BOT_DRAG
C Place CPP define/undef flag here

C if bottom drag will be spread over more than just
C the bottom-most cell.
#define VAR_BOT_DRAG_SPREAD

#endif /* ALLOW_MYPACKAGE */
#endif /* MYPACKAGE_OPTIONS_H */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
