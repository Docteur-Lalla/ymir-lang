#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern HsStablePtr ymir_newInteger(HsInt a1);
extern HsStablePtr ymir_newFloat(HsDouble a1);
extern HsStablePtr ymir_newChar(HsChar a1);
extern HsStablePtr ymir_newBool(HsBool a1);
extern HsStablePtr ymir_newString(HsPtr a1);
extern HsStablePtr ymir_newSymbol(HsPtr a1);
extern HsStablePtr ymir_newList(HsPtr a1, HsInt a2);
extern HsStablePtr ymir_newPointer(HsPtr a1);
extern HsStablePtr ymir_return(HsStablePtr a1);
extern HsBool ymir_isInteger(HsStablePtr a1);
extern HsBool ymir_isFloat(HsStablePtr a1);
extern HsBool ymir_isChar(HsStablePtr a1);
extern HsBool ymir_isBool(HsStablePtr a1);
extern HsBool ymir_isString(HsStablePtr a1);
extern HsBool ymir_isSymbol(HsStablePtr a1);
extern HsBool ymir_isList(HsStablePtr a1);
extern HsBool ymir_isPointer(HsStablePtr a1);
extern HsBool ymir_isFunction(HsStablePtr a1);
extern HsInt ymir_getInteger(HsStablePtr a1);
extern HsDouble ymir_getFloat(HsStablePtr a1);
extern HsChar ymir_getChar(HsStablePtr a1);
extern HsBool ymir_getBool(HsStablePtr a1);
extern HsPtr ymir_getString(HsStablePtr a1);
extern HsPtr ymir_getSymbol(HsStablePtr a1);
extern HsPtr ymir_getPointer(HsStablePtr a1);
extern HsStablePtr ymir_functionCall(HsStablePtr a1, HsPtr a2, HsInt a3);
extern HsStablePtr ymir_throwNumberArguments(HsInt a1, HsPtr a2, HsInt a3);
extern HsStablePtr ymir_throwTypeMismatch(HsPtr a1, HsStablePtr a2);
#ifdef __cplusplus
}
#endif

