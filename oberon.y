/* oberon.y - This is an yacc grammar for the Oberon-2 language.
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

%token ARRAY_TK  BEGIN_TK  BY_TK     CASE_TK CONST_TK   DIV_TK
%token DO_TK     ELSE_TK   ELSIF_TK  END_TK  EXIT_TK    FOR_TK
%token IF_TK     IMPORT_TK IN_TK     IS_TK   LOOP_TK    MOD_TK
%token MODULE_TK NIL_TK    OF_TK     OR_TK   POINTER_TK PROCEDURE_TK
%token RECORD_TK REPEAT_TK RETURN_TK THEN_TK TO_TK      TYPE_TK
%token UNTIL_TK  VAR_TK    WHILE_TK  WITH_TK

/* General token values */

%token '#'       '~'        LE  GE
%token DOTDOT    BECOMES    ID
%token INTCONST
%token STRCONST  CHARCONST
%token REALCONST LREALCONST MOD_ID

/* Priorities for the operators */
%left  '=' '#' '<'     LE     '>' GE
%left  '+' '-'  OR_TK
%left  '*' '/'  DIV_TK MOD_TK '&'
%right '~'


%start module

%%

/* Start of the program */
module : MODULE_TK ident ';' importList declSeq beginBlock END_TK ident '.'

importList : IMPORT_TK modList ';'  
           | /* empty */            { }
           ;

modName : ident                     
        | ident BECOMES ident       
        ;

modList : modName                   
        | modList ',' modName        
        ;
beginBlock : BEGIN_TK statementSeq  
           | /* empty */            
           ;

ident : ID                          { /* NotImplemented (); */ } 
      ;
      
identList : identDef                
          | identList ',' identDef  
          ;

qualident : ident                    
          | MOD_ID '.' ident        
          ;

designator : qualident                       
           | designator '.' ident            
	   | designator '[' exprList ']'     
	   | designator '^'                  
	   | designator '(' qualident ')'    
	   ;

identDef : ident      
         | ident '*' 
         | ident '-' 
         ;

/* declarations */
declSeq : declSeqList procDecls      
        ;

declSeqList : declSeqList CONST_TK constDeclList   
            | declSeqList TYPE_TK  typeDeclList    
	    | declSeqList VAR_TK   varDeclList      
	    | /* empty */
	    ;
	   	
       
constDeclList : identDef '=' constExpr                    
              | constDeclList ';' identDef '=' constExpr  
	      | /* empty */                                
	      ;

typeDeclList : identDef '=' type                           
             | typeDeclList ';' identDef '=' type         
	     | /* empty */                                 
	     ;

varDeclList : identList  ':' type                         
              | varDeclList ';' identList '=' type         
	      | /* empty */                               
	      ;
	      
procDecls : procDecls procDecl ';'                        
          | procDecls forwardDecl ';'                      
	  | /* empty */                                   
	  ;
	  
procDecl : PROCEDURE_TK receiver identDef formalPars ';' declSeq beginBlock END_TK ident
            
         ;
	 
forwardDecl : PROCEDURE_TK '^' receiver identDef formalPars ';' 
            ;

receiver : '(' byReference ident ':' ident ')' 
         | /* empty */                         
	 ;
	 
formalPars : '(' fpSectionList ')' retType     
           ;
	   
retType : ':' qualident                         
        | /* empty */                          
	;
	
fpSectionList : byReference identList ':' type 
              ;
	      
byReference : VAR_TK                            
            | /* empty */                       
	    ;
	    
/* type declarations */
type : qualident                                
     | ARRAY_TK constExprList OF_TK type        
     | RECORD_TK  baseRec fieldList END_TK      
     | POINTER_TK TO_TK type                   
     | PROCEDURE_TK formalPars                  
     ;

constExprList : constExpr                      
              | constExprList ',' constExpr     
	      ;
	      
baseRec : '(' qualident ')'                     
        | /* empty */                            
	;
	
fieldList : identList ':' type                  
          | fieldList ';' identList ':' type   
	  | /* empty */                         
	  ;
	  
/* statements */
statementSeq : statement                        
             | statementSeq ';' statement       
	     ;
	     
statement : designator BECOMES expr            
          | designator params                  
	  | IF_TK expr THEN_TK statementSeq elsifList elsePart END_TK     
	  | CASE_TK expr OF_TK caseList elsePart END_TK                 
	  | WHILE_TK expr DO_TK statementSeq END_TK                     
	  | REPEAT_TK statementSeq UNTIL_TK expr                        
	  | FOR_TK ident BECOMES expr TO_TK expr byStep DO_TK statementSeq END_TK
                                                                       
	  | LOOP_TK statementSeq END_TK                                
	  | WITH_TK guardList elsePart END_TK                          
	  | EXIT_TK                                                     
	  | RETURN_TK exprOpt                                          
	  ;
	  
params : '(' exprList ')'           
       | /* empty */                
       ;
       
elsifList : elsifList ELSIF_TK expr THEN_TK statementSeq     
	  | /* empty */                                      
	  ;
	  
elsePart : ELSE_TK statementSeq               
         | /* empty */                      
	 ;

caseList : caseLabelsList ':' statementSeq   
         | /* empty */                       
	 ;
	 
caseLabelsList : caseLabels                       
               | caseLabelsList ',' caseLabels     
	       ;
	       
caseLabels : constExpr                            
           | constExpr DOTDOT constExpr             
	   ;
	   
byStep : BY_TK constExpr                           
       | /* empty */                               
       ;
       
guardList : guard DO_TK statementSeq                
          | guardList '|' guard DO_TK statementSeq  
	  ;

guard : qualident ':' qualident   
      ;
      
exprOpt : expr                     
        | /* empty */             
	;
	
/* expression processing */
constExpr : expr
          ;
	  
expr : simpleExpr                      
     | simpleExpr '=' simpleExpr    
     | simpleExpr '#' simpleExpr    
     | simpleExpr '<' simpleExpr    
     | simpleExpr LE  simpleExpr    
     | simpleExpr '>' simpleExpr    
     | simpleExpr GE simpleExpr     
     | simpleExpr IN_TK simpleExpr     
     | simpleExpr IS_TK simpleExpr     
     ;

simpleExpr : simpleExpr '+' simpleExpr    
           | simpleExpr '-' simpleExpr    
           | simpleExpr '*' simpleExpr    
           | simpleExpr '/' simpleExpr    
           | simpleExpr DIV_TK simpleExpr 
           | simpleExpr MOD_TK simpleExpr 
           | simpleExpr OR_TK  simpleExpr 
           | simpleExpr '&' simpleExpr    
           | '(' simpleExpr ')'           
	   | designator params             
	   | number                       
	   | CHARCONST                     
	   | STRCONST                     
	   | NIL_TK                       
	   | Set                          
	   | '~' simpleExpr                
           ;


number : INTCONST          
       | REALCONST              
       | LREALCONST        
       ;                
       
exprList : expr                 
         | exprList ',' expr   
	 ;

Set : '{' elementList '}'        
    ;
    
elementList : element                     
            | elementList ',' element     
	    | /* empty */                  
	    ;

element : expr                        
        | expr DOTDOT expr           
	;





