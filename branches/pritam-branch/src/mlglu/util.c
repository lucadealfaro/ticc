#include "mdd.h"
#include "stdio.h"
#include "string.h"
#include "stdlib.h"

static char* utilMddToStringRec( mdd_manager* mgr, mdd_t *mdd, int level);


int UtilGetTopMddId(mdd_manager *mgr, mdd_t * mdd)
{
  array_t * varidArray = mdd_get_support(mgr , mdd);
  int id;

  if (array_n(varidArray) == 0){
    printf("Error: mdd has empty support\n");
    exit(1);
  }

  id = (int)array_fetch( int, varidArray, 0);
  array_free(varidArray);

 return id;
}


char * getNameFromId(mdd_manager *mgr, int id)
{
  mvar_type mvar;
  mvar = mdd_get_var_by_id(mgr,id);
  return mvar.name;
}


int getNVals(mdd_manager * mgr, int id)
{
  mvar_type x = array_fetch(mvar_type, mdd_ret_mvar_list(mgr), id);
  return x.values;
}


mdd_t * UtilGetLiteral(	mdd_manager * mgr, int id, int pos)
{ 
  array_t * values = array_alloc(int ,0);
  mdd_t * literal;
  
  array_insert(int, values, 0, pos);
  literal = mdd_literal(mgr, id, values);
  array_free(values);
  assert(literal);
  
  return literal;
}


void utilMddPrintRec( mdd_manager* mgr, mdd_t *mdd, int level)
{
  mdd_t *s;
  int  id;
  char *varName;
  boolean ife = FALSE;
  int nvals, i;
  mdd_t *literal;
    
  char * indentation;
  
  /* bypass printing 1s unnecessarily */
  if(mdd_is_tautology(mdd,1) && level > 0){
    return;
  }

  level++;

  printf("(");

  if(mdd_is_tautology(mdd,0)) printf("0");
  else if(mdd_is_tautology(mdd,1)) printf("1");
  else{ 

    id = UtilGetTopMddId(mgr,mdd);
   
   
    varName = getNameFromId(mgr,id);
    if(!varName){ 
      printf("Error: No var for id : %d\n", id);
      exit (1);
    }
    
    nvals = getNVals(mgr,id);
    
    indentation = ALLOC(char, level*2);
    for( i=0; i<level*2; i++) {
      *(indentation+i) = ' ';
    }
    *(indentation + 2*level -1) = 0;
    
    for ( i =0; i < nvals; i++){

      literal = UtilGetLiteral(mgr, id, i);
      
      s = mdd_cofactor(mgr, mdd, literal);

      if( !mdd_is_tautology(s, 0)){
        if(ife)
          printf( "\n%s + ", indentation);

	printf( "\n%s(%s = %d)", indentation, varName, i);
        
        ife = TRUE;
        utilMddPrintRec(mgr, s , level);
      }
    }
    
    FREE(indentation);
  }
  
  level--;

  printf(") ");
  
  if(level == 0) {
    printf("\n");
    fflush(stdout);
  }
}


char* utilAccumulateString(int init, char* add)
{
  static int size = 1024;
  static char* buffer = 0;

  int len;

  if (!buffer) {
    buffer = (char*)malloc(1024);
    buffer[0] = '\0';
  }
  if (init) {
    buffer[0] = '\0';
  }

  len = strlen(buffer);
  if ((len + strlen(add)) < (size - 1)) {
    strcat(buffer, add);
  } else {
    buffer = (char*)realloc(buffer, size * 2);
    size = size * 2;
    strcat(buffer, add);
  }
  return buffer;
}


char* utilMddToString(mdd_manager* mgr, mdd_t *mdd, int level)
{
  utilAccumulateString(1, "");
  return utilMddToStringRec(mgr, mdd, level);
}


char* utilMddToStringRec( mdd_manager* mgr, mdd_t *mdd, int level)
{
  mdd_t *s;
  int  id;
  char *varName;
  int nvals, i;
  mdd_t *literal;
  char* buffer = NULL;

  static char local[1024] = "";
  
  /* bypass printing 1s unnecessarily */
  if(mdd_is_tautology(mdd,1) && level > 0){
    local[0] = '\0';
    return local;
  }

  level++;

  if(mdd_is_tautology(mdd,0)) {
    buffer = utilAccumulateString(0, "0");
  } else if(mdd_is_tautology(mdd,1)) {
    buffer = utilAccumulateString(0, "1");
  } else { 

    id = UtilGetTopMddId(mgr,mdd);
   
   
    varName = getNameFromId(mgr,id);
    if(!varName){ 
      printf("Error: No var for id : %d\n", id);
      exit (1);
    }
    
    nvals = getNVals(mgr,id);
    for ( i =0; i < nvals; i++){
      literal = UtilGetLiteral(mgr, id, i);
      
      s = mdd_cofactor(mgr, mdd, literal);

      if( !mdd_is_tautology(s, 0)){
	sprintf(local, "%s=%d ", varName, i);
	buffer = utilAccumulateString(0, local);
        
        utilMddToStringRec(mgr, s , level);
      }
    }
  }
  
  level--;
  
  return buffer;
}

void utilMddPrintRec2( mdd_manager* mgr, mdd_t *mdd, int level)
{
    mdd_t *s;
    int  id;
    char *varName;
    int nvals, i, j;
    mdd_t *literal;
  
  
    /* we only need to print a 1 or 0 if it's the only thing in the expr */
	if (mdd_is_tautology(mdd,1)) {
	    printf("1");
	    return;
	}
	if (mdd_is_tautology(mdd,0)) {
	    printf("0");
	    return;
	}

#define doIndent(level) for (j = 0; j < level; j++) printf("  ");

    printf("(");

    id = UtilGetTopMddId(mgr,mdd);
   
    varName = getNameFromId(mgr,id);
    if(!varName){ 
	printf("Error: No var for id : %d\n", id);
	exit (1);
    }
    nvals = getNVals(mgr,id);
    
    for ( i =0; i < nvals; i++){
	literal = UtilGetLiteral(mgr, id, i);
	s = mdd_cofactor(mgr, mdd, literal);
	if (!mdd_is_tautology(s, 0)){
	    if (i > 0) {
		printf("\n");
		doIndent(level);
		printf("+ ");
	    }
	    printf( "(%s = %d)", varName, i);
        
	    utilMddPrintRec2(mgr, s , level+1);
	}
    
    }
  
    printf(") ");
  
    if(level == 0) {
	printf("\n");
	fflush(stdout);
    }
}


