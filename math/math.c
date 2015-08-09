#include "../include/ymir.h"
#include <math.h>

#define YMIR_C_FUNCTION(c_name, cy_name) \
YmirValue cy_name(YmirArray args, int size)\
{\
  if(size != 1)\
    YMIR_THROW_NUMBER_ARGS(1, args, size);\
  else if(!ymir_isNumber(args[0]))\
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);\
  else\
  {\
    double arg = ymir_isFloat(args[0]) ?\
      ymir_getFloat(args[0]) :\
      (double)ymir_getInteger(args[0]);\
    double res = c_name(arg);\
    YMIR_RETURN(ymir_newFloat(res));\
  }\
}

YMIR_C_FUNCTION(cos, c_cos);
YMIR_C_FUNCTION(sin, c_sin);
YMIR_C_FUNCTION(tan, c_tan);
YMIR_C_FUNCTION(acos, c_acos);
YMIR_C_FUNCTION(asin, c_asin);
YMIR_C_FUNCTION(atan, c_atan);
YMIR_C_FUNCTION(cosh, c_cosh);
YMIR_C_FUNCTION(sinh, c_sinh);
YMIR_C_FUNCTION(tanh, c_tanh);
YMIR_C_FUNCTION(acosh, c_acosh);
YMIR_C_FUNCTION(asinh, c_asinh);
YMIR_C_FUNCTION(atanh, c_atanh);
YMIR_C_FUNCTION(log, c_log);
YMIR_C_FUNCTION(log10, c_log10);
YMIR_C_FUNCTION(log2, c_log2);

YMIR_C_FUNCTION(exp, c_exp);

YMIR_C_FUNCTION(sqrt, c_sqrt);

YmirValue c_ceil(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isNumber(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    if(ymir_isInteger(args[0]))
      YMIR_RETURN(args[0]);
    else
    {
      double arg = ymir_getFloat(args[0]);
      YMIR_RETURN(ymir_newFloat(ceil(arg)));
    }
  }
}

YmirValue c_floor(YmirArray args, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, args, size);
  else if(!ymir_isNumber(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else
  {
    if(ymir_isInteger(args[0]))
      YMIR_RETURN(args[0]);
    else
    {
      double arg = ymir_getFloat(args[0]);
      YMIR_RETURN(ymir_newFloat(floor(arg)));
    }
  }
}

YmirValue c_pow(YmirArray args, int size)
{
  if(size != 2)
    YMIR_THROW_NUMBER_ARGS(2, args, size);
  else if(!ymir_isNumber(args[0]))
    YMIR_THROW_TYPE_MISMATCH("number", args[0]);
  else if(!ymir_isNumber(args[1]))
    YMIR_THROW_TYPE_MISMATCH("number", args[1]);
  else
  {
    double x = ymir_isFloat(args[0]) ?
      ymir_getFloat(args[0]) :
      (double)ymir_getInteger(args[0]);
    double n = ymir_isFloat(args[1]) ?
      ymir_getFloat(args[1]) :
      (double)ymir_getInteger(args[1]);
    
    YMIR_RETURN(ymir_newFloat(pow(x, n)));
  }
}
