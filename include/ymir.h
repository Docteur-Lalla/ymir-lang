#ifndef YMIR_YMIR_H
#define YMIR_YMIR_H

#include "ffi_stub.h"

#define YMIR_RETURN(value) return ymir_return(value)
#define YMIR_THROW_NUMBER_ARGS(num, args, size) return ymir_throwNumberArguments(num, args, size)
#define YMIR_THROW_TYPE_MISMATCH(type, value) return ymir_throwTypeMismatch(type, value)

typedef HsStablePtr YmirValue;
typedef HsPtr* YmirArray;

#endif
