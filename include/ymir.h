#ifndef YMIR_YMIR_H
#define YMIR_YMIR_H

#include "extern_stub.h"

#define YMIR_RETURN(value) return ymir_return(value)
#define YMIR_THROW_NUMBER_ARGS(num, args, size) return ymir_throwNumberArguments(num, args, size)
#define YMIR_THROW_TYPE_MISMATCH(type, value) return ymir_throwTypeMismatch(type, value)

typedef HsStablePtr YmirValue;
typedef HsPtr* YmirArray;

#define ymir_newNumber ymir_newInteger
#define ymir_isNumber(val) (ymir_isInteger(val) || ymir_isFloat(val))
#define ymir_getNumber ymir_getInteger

#endif
