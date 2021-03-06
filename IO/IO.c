#include "../include/ymir.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

YmirValue print_integer(YmirArray ptr, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, ptr, size);

  if(!ymir_isInteger(ptr[0]))
    YMIR_THROW_TYPE_MISMATCH("integer", ptr[0]);

  printf("%d", ymir_getInteger(ptr[0]));
  YMIR_RETURN(ymir_newList(NULL, 0));
}

YmirValue print_float(YmirArray ptr, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, ptr, size);

  if(!ymir_isFloat(ptr[0]))
    YMIR_THROW_TYPE_MISMATCH("float", ptr[0]);

  printf("%lf", ymir_getFloat(ptr[0]));
  YMIR_RETURN(ymir_newList(NULL, 0));
}

YmirValue print_char(YmirArray ptr, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, ptr, size);

  if(!ymir_isChar(ptr[0]))
    YMIR_THROW_TYPE_MISMATCH("char", ptr[0]);

  printf("%c", ymir_getChar(ptr[0]));
  YMIR_RETURN(ymir_newList(NULL, 0));
}

YmirValue print_bool(YmirArray ptr, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, ptr, size);

  if(!ymir_isBool(ptr[0]))
    YMIR_THROW_TYPE_MISMATCH("bool", ptr[0]);

  int b = ymir_getBool(ptr[0]);

  if(b == 0)
    printf("false");
  else
    printf("true");

  YMIR_RETURN(ymir_newList(NULL, 0));
}

YmirValue print_string(YmirArray ptr, int size)
{
  if(size != 1)
    YMIR_THROW_NUMBER_ARGS(1, ptr, size);

  if(!ymir_isString(ptr[0]))
    YMIR_THROW_TYPE_MISMATCH("string", ptr[0]);

  printf("%s", ymir_getString(ptr[0]));
  YMIR_RETURN(ymir_newList(NULL, 0));
}

YmirValue flush_stdout(YmirArray ptr, int size)
{
  if(size > 0)
    YMIR_THROW_NUMBER_ARGS(0, ptr, size);
  
  fflush(stdout);
  YMIR_RETURN(ymir_newList(NULL, 0));
}

YmirValue input_integer(YmirArray ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  int n;
  scanf("%d", &n);
  YMIR_RETURN(ymir_newInteger(n));
}

YmirValue input_float(YmirArray ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  double n;
  scanf("%lf", &n);
  YMIR_RETURN(ymir_newFloat(n));
}

YmirValue input_char(YmirArray ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  char c;
  scanf("%c", &c);
  YMIR_RETURN(ymir_newChar(c));
}

YmirValue input_bool(YmirArray ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  char s[6];
  scanf("%s", s);

  if(strcmp(s, "true"))
    YMIR_RETURN(ymir_newBool(1));
  else
    YMIR_RETURN(ymir_newBool(0));
}

char* readline(FILE* file)
{
  char buffer[1024];
  char* result = 0;
  int length = 0;

  while(!feof(file))
  {
    fgets(buffer, sizeof(buffer), file);
    int len = strlen(buffer);
    buffer[len] = 0;

    length += len;
    char* tmp = (char*)malloc(length+1);
    tmp[0] = 0;

    if(result)
    {
      strcpy(tmp, result);
      free(result);
      result = tmp;
    }

    else
    {
      result = (char*)malloc(strlen(tmp));
    }

    strcat(result, buffer);

    if(strstr(buffer, "\n"))
      break;
  }

  // result[strlen(result)-1] == '\n' by definition
  result[strlen(result)-1] = '\0';
  return result;
}

YmirValue input_string(YmirArray ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  char* line = readline(stdin);
  YmirValue ret = ymir_newString(line);
  free(line);

  YMIR_RETURN(ret);
}

