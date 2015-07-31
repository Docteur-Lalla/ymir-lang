#include "../src/ffi_stub.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

HsStablePtr print_number(HsPtr* ptr, int size)
{
  if(size != 1)
    return ymir_throwNumberArguments(1, ptr, size);

  if(!ymir_isNumber(ptr[0]))
    ymir_throwTypeMismatch("number", ptr[0]);

  printf("%d", ymir_getNumber(ptr[0]));
  return ymir_return(ymir_newList(NULL, 0));
}

HsStablePtr print_char(HsPtr* ptr, int size)
{
  if(size != 1)
    return ymir_throwNumberArguments(1, ptr, size);

  if(!ymir_isChar(ptr[0]))
    ymir_throwTypeMismatch("char", ptr[0]);

  printf("%c", ymir_getChar(ptr[0]));
  return ymir_return(ymir_newList(NULL, 0));
}

HsStablePtr print_bool(HsPtr* ptr, int size)
{
  if(size != 1)
    return ymir_throwNumberArguments(1, ptr, size);

  if(!ymir_isBool(ptr[0]))
    return ymir_throwTypeMismatch("bool", ptr[0]);

  int b = ymir_getBool(ptr[0]);

  if(b == 0)
    printf("false");
  else
    printf("true");

  return ymir_return(ymir_newList(NULL, 0));
}

HsStablePtr print_string(HsPtr* ptr, int size)
{
  if(size != 1)
    return ymir_throwNumberArguments(1, ptr, size);

  if(!ymir_isString(ptr[0]))
    return ymir_throwTypeMismatch("string", ptr[0]);

  printf("%s", ymir_getString(ptr[0]));
  return ymir_return(ymir_newList(NULL, 0));
}

HsStablePtr flush_stdout(HsPtr* ptr, int size)
{
  if(size > 0)
    return ymir_throwNumberArguments(0, ptr, size);
  
  fflush(stdout);
  return ymir_return(ymir_newList(NULL, 0));
}

HsStablePtr input_number(HsPtr* ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  int n;
  scanf("%d", &n);
  return ymir_return(ymir_newNumber(n));
}

HsStablePtr input_char(HsPtr* ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  char c;
  scanf("%c", &c);
  return ymir_return(ymir_newChar(c));
}

HsStablePtr input_bool(HsPtr* ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  char s[6];
  scanf("%s", s);

  if(strcmp(s, "true"))
    return ymir_return(ymir_newBool(1));
  else
    return ymir_return(ymir_newBool(0));
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

HsStablePtr input_string(HsPtr* ptr, int size)
{
  if(size == 1 && ymir_isString(ptr[0]))
  {
    print_string(ptr, size);
    flush_stdout(NULL, 0);
  }

  char* line = readline(stdin);
  HsStablePtr ret = ymir_newString(line);
  // free(line);

  return ymir_return(ret);
}

