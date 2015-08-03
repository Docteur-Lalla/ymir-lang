#include "../include/ymir.h"
#include <math.h>

YmirValue c_cos(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isNumber(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    int res = (int)cos((double)ymir_getNumber(args[0]));
    YMIR_RETURN(ymir_newNumber(res));
  }
}

YmirValue c_sin(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isNumber(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    int res = (int)sin((double)ymir_getNumber(args[0]));
    YMIR_RETURN(ymir_newNumber(res));
  }
}

YmirValue c_tan(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isNumber(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    int res = (int)tan((double)ymir_getNumber(args[0]));
    YMIR_RETURN(ymir_newNumber(res));
  }
}

YmirValue c_sqrt(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isNumber(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    int res = (int)sqrt((double)ymir_getNumber(args[0]));
    YMIR_RETURN(ymir_newNumber(res));
  }
}
