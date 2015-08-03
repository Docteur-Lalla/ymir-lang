#include "../include/ymir.h"
#include <math.h>

YmirValue c_cos(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isFloat(args[0]) && !ymir_isInteger(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    double arg = ymir_isFloat(args[0]) ?
      ymir_getFloat(args[0]) :
      (double)ymir_getInteger(args[0]);
    double res = cos(arg);
    YMIR_RETURN(ymir_newFloat(res));
  }
}

YmirValue c_sin(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isFloat(args[0]) && !ymir_isInteger(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    double arg = ymir_isFloat(args[0]) ?
      ymir_getFloat(args[0]) :
      (double)ymir_getInteger(args[0]);
    double res = sin(arg);
    YMIR_RETURN(ymir_newFloat(res));
  }
}

YmirValue c_tan(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isFloat(args[0]) && !ymir_isInteger(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    double arg = ymir_isFloat(args[0]) ?
      ymir_getFloat(args[0]) :
      (double)ymir_getInteger(args[0]);
    double res = tan(arg);
    YMIR_RETURN(ymir_newFloat(res));
  }
}

YmirValue c_sqrt(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isFloat(args[0]) && !ymir_isInteger(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    double arg = ymir_isFloat(args[0]) ?
      ymir_getFloat(args[0]) :
      (double)ymir_getInteger(args[0]);
    double res = sqrt(arg);
    YMIR_RETURN(ymir_newFloat(res));
  }
}
