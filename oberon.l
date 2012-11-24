/* oberon.l - Lex file for processing the tokens of Oberon-2.
 * Copyright (C) 1997  Paulo Pinto
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
 
%{
  #include <iostream.h>
  #include <string.h>
  #include <math.h>
  #include <ctype.h>

  #include "y.tab.h"
  

  // Prototypes of functions used by the lexer, but not declared
  // This is needed in order to satisfy the C++ compiler
  extern "C" int yyinput ();
  extern "C" int wrap  ();

  // Prototypes of functions local to the lexer

  static int  IsKeyword_or_Id (const char *text);
  static long HexToLong       (const char *str);
  static char HexToChar       (const char *str);
  static int  AsciiToDouble (const char *str, double &value);

  // Local variables
  static int lineNum = 1;
%}

digit     [0-9]
digits    [0-9]+
hexDigits [0-9A-F]+
letter    [A-Za-z]

%%

\n                             { lineNum++; } 
                  
(" "|\t)+                      { }


{digits}                       {
                                 return INTCONST;
			       }

{digit}{hexDigits}"H"          {
                                 return INTCONST;
                               }
			       
{digit}{hexDigits}"X"          { 
                                 return CHARCONST;
                               }

{digits}"."{digit}*([ED][+-]?{digits})? {
                                           return AsciiToDouble (yytext, yylval.rvalue);
                                        }


{letter}({digit}|{letter})*    { 
                                 return IsKeyword_or_Id (yytext);
                               }



"(*"                           {
                                 int ch;                 // Current char   
                                 int lastCh = '\0';      // Last char read
                                 int commentCounter = 1; // Nesting level of the comments
 
                                 while ((ch = yyinput ()) != EOF) 
                                  if (lastCh == '(' && ch == '*') {
                                    commentCounter++;
                                    lastCh = '\0';
                                  }
                                  else if (lastCh == '*' && ch == ')') {
                                    lastCh = '\0';
                                    commentCounter--;
                                    if (commentCounter == 0)
                                      break;
                                  }
                                  else
                                     lastCh = ch;

                                 if (commentCounter != 0)
                                   yyerror ("Unexpected End of Comment"); 
                               }


(\'[^\'\n]*\')|(\"[^\"\n]*\")  { 
                                 return STRCONST;
                               } 


"<="                           return LE;
">="                           return GE;
":="                           return BECOMES;
".."                           return DOTDOT;

"<"|">"|"="|"."|"("|")"|"["    return yytext [0];
"]"|";"|"^"|"#"|"&"|"~"|"+"    return yytext [0];
"-"|"*"|"/"|","|"{"|"}"|"|"    return yytext [0];
":"                            return yytext [0];

.                            yyerror ("Unknown token");


%%

// Table to search for reserved keywords

static struct {
   char *keyName;        // Ascii representation 
   int   token;          // Token value 
} keyTbl[]= {{"ARRAY", ARRAY_TK},
             {"BEGIN", BEGIN_TK},
	     {"BY", BY_TK}, 
             {"CASE", CASE_TK},
	     {"CONST", CONST_TK},
	     {"DIV", DIV_TK},
	     {"DO", DO_TK}, 
             {"ELSE", ELSE_TK},
	     {"ELSIF", ELSIF_TK},
	     {"END", END_TK}, 
             {"EXIT", EXIT_TK},
	     {"FOR", FOR_TK},
	     {"IF", IF_TK}, 
             {"IMPORT", IMPORT_TK},
	     {"IN", IN_TK},
	     {"IS", IS_TK}, 
             {"LOOP", LOOP_TK},
	     {"MOD", MOD_TK},
	     {"MODULE", MODULE_TK}, 
             {"NIL", NIL_TK},
	     {"OF", OF_TK},
	     {"OR", OR_TK}, 
             {"POINTER", POINTER_TK},
	     {"PROCEDURE", PROCEDURE_TK},
             {"RECORD", RECORD_TK},
	     {"REPEAT", REPEAT_TK},
	     {"RETURN", RETURN_TK}, 
             {"THEN", THEN_TK},
	     {"TO", TO_TK},
	     {"TYPE", TYPE_TK},
             {"UNTIL", UNTIL_TK},
	     {"VAR", VAR_TK},
	     {"WHILE", WHILE_TK},
             {"WITH", WITH_TK}
            };

//---------------------------------------
// Returns the token that represents text
//---------------------------------------
static int IsKeyword_or_Id (const char *text)
{
  unsigned i;

  for (i=0; i < (sizeof keyTbl/ sizeof keyTbl[0]); i++)
   if (!strcmp (keyTbl[i].keyName, text))
     return keyTbl[i].token; 
  
  return ID;
}

//--------------------------------------------------------------
//  Converts a string that represents an hexadecimal number to
// a long int.
//--------------------------------------------------------------
static long HexToLong (const char *str)
{
  long temp = 0;

  while (*str) {
   if (isdigit (*str))
    temp = (temp << 4) + *str - '0';
   else
    temp = (temp << 4) + *str - 'A' + 10;

   str++;
  }

  return temp;
}

//--------------------------------------------------------------
//  Converts a string that represents an hexadecimal character 
// constant to char
//--------------------------------------------------------------
static char HexToChar (const char *str)
{
  char temp = 0;

  while (*str) {
   if (isdigit (*str))
    temp = (temp << 4) + *str - '0';
   else
    temp = (temp << 4) + *str - 'A' + 10;

   str++;
  }

  return temp;
}


//--------------------------------------------------------------
//  Converts a string that represents a real number to a double
// and returns if it represents a REAL or a LONGREAL.
//--------------------------------------------------------------
static int AsciiToDouble (const char *str, double &value)
{
  double temp = 0.0;
  int token = REALCONST;

  while (isdigit (*str)) {
    temp = temp * 10 + *str - '0';
    str++;
  }
  
  if (*str == '.') {
    str++;
    
    int count = 10;
    while (isdigit (*str)) {
      temp = temp + (static_cast <double> (*str - '0') / count);
      count *= 10;
      str++;
    }

    if (*str == 'E')
      str++;
    else if (*str == 'D') {
      str++;
      token = LREALCONST;
    }

    int sign;
    if (*str == '+') {
      str++;
      sign = 1;
    }
    else if (*str == '-') {
      str++;
      sign  = -1;
    }
    else
      sign = 1;

    int exp = 0;
    while (isdigit (*str)) {
      exp = exp * 10 + *str - '0';
      str++;
    }
    value = temp * pow (10, sign * exp);
  }
  else
    value = temp;

  return token;
}

//--------------------------------
// Returns the current line number
//--------------------------------
int LineNumber ()
{
  return lineNum;
}
