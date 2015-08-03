#ifndef YMIR_YMIR_H
#define YMIR_YMIR_H

#include "ffi_stub.h"

#define YMIR_RETURN(value) return ymir_return(value)

typedef HsStablePtr YmirValue;
typedef HsPtr* YmirArray;

#endif

