/**CHeaderFile*****************************************************************

  FileName    [epd.h]

  PackageName [epd]

  Synopsis    [The University of Colorado extended double precision package.]

  Description [arithmetic functions with extended double precision.]

  SeeAlso     []

  Author      [In-Ho Moon]

  Copyright [This file was created at the University of Colorado at
  Boulder.  The University of Colorado at Boulder makes no warranty
  about the suitability of this software for any purpose.  It is
  presented on an AS IS basis.]

  Revision    [$Id: epd.h,v 1.1 2005/04/21 05:58:01 luca Exp $]

******************************************************************************/

#ifndef _EPD
#define _EPD


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

#define	EPD_MAX_BIN	1023
#define	EPD_MAX_DEC	308
#define	EPD_EXP_INF	0x7ff

/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/

/**Struct**********************************************************************

  Synopsis    [IEEE double struct.]

  Description [IEEE double struct.]

  SeeAlso     []

******************************************************************************/
#ifdef	EPD_BIG_ENDIAN
struct IeeeDoubleStruct {	/* BIG_ENDIAN */
  unsigned int sign: 1;
  unsigned int exponent: 11;
  unsigned int mantissa0: 20;
  unsigned int mantissa1: 32;
};
#else
struct IeeeDoubleStruct {	/* LITTLE_ENDIAN */
  unsigned int mantissa1: 32;
  unsigned int mantissa0: 20;
  unsigned int exponent: 11;
  unsigned int sign: 1;
};
#endif

/**Struct**********************************************************************

  Synopsis    [IEEE double NaN struct.]

  Description [IEEE double NaN struct.]

  SeeAlso     []

******************************************************************************/
#ifdef	EPD_BIG_ENDIAN
struct IeeeNanStruct {	/* BIG_ENDIAN */
  unsigned int sign: 1;
  unsigned int exponent: 11;
  unsigned int quiet_bit: 1;
  unsigned int mantissa0: 19;
  unsigned int mantissa1: 32;
};
#else
struct IeeeNanStruct {	/* LITTLE_ENDIAN */
  unsigned int mantissa1: 32;
  unsigned int mantissa0: 19;
  unsigned int quiet_bit: 1;
  unsigned int exponent: 11;
  unsigned int sign: 1;
};
#endif

/**Struct**********************************************************************

  Synopsis    [Extended precision double to keep very large value.]

  Description [Extended precision double to keep very large value.]

  SeeAlso     []

******************************************************************************/
struct EpDoubleStruct {
  union {
    double			value;
    struct IeeeDoubleStruct	bits;
    struct IeeeNanStruct	nan;
  } type;
  int		exponent;
};

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
typedef struct EpDoubleStruct EpDouble;
typedef struct IeeeDoubleStruct IeeeDouble;
typedef struct IeeeNanStruct IeeeNan;


/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

EXTERN EpDouble *EpdAlloc(void);
EXTERN int EpdCmp(const char *key1, const char *key2);
EXTERN void EpdFree(EpDouble *epd);
EXTERN void EpdGetString(EpDouble *epd, char *str);
EXTERN void EpdConvert(double value, EpDouble *epd);
EXTERN void EpdMultiply(EpDouble *epd1, double value);
EXTERN void EpdMultiply2(EpDouble *epd1, EpDouble *epd2);
EXTERN void EpdMultiply2Decimal(EpDouble *epd1, EpDouble *epd2);
EXTERN void EpdMultiply3(EpDouble *epd1, EpDouble *epd2, EpDouble *epd3);
EXTERN void EpdMultiply3Decimal(EpDouble *epd1, EpDouble *epd2, EpDouble *epd3);
EXTERN void EpdDivide(EpDouble *epd1, double value);
EXTERN void EpdDivide2(EpDouble *epd1, EpDouble *epd2);
EXTERN void EpdDivide3(EpDouble *epd1, EpDouble *epd2, EpDouble *epd3);
EXTERN void EpdAdd(EpDouble *epd1, double value);
EXTERN void EpdAdd2(EpDouble *epd1, EpDouble *epd2);
EXTERN void EpdAdd3(EpDouble *epd1, EpDouble *epd2, EpDouble *epd3);
EXTERN void EpdSubtract(EpDouble *epd1, double value);
EXTERN void EpdSubtract2(EpDouble *epd1, EpDouble *epd2);
EXTERN void EpdSubtract3(EpDouble *epd1, EpDouble *epd2, EpDouble *epd3);
EXTERN void EpdPow2(int n, EpDouble *epd);
EXTERN void EpdPow2Decimal(int n, EpDouble *epd);
EXTERN void EpdNormalize(EpDouble *epd);
EXTERN void EpdNormalizeDecimal(EpDouble *epd);
EXTERN void EpdGetValueAndDecimalExponent(EpDouble *epd, double *value, int *exponent);
EXTERN int EpdGetExponent(double value);
EXTERN int EpdGetExponentDecimal(double value);
EXTERN void EpdMakeInf(EpDouble *epd, int sign);
EXTERN void EpdMakeZero(EpDouble *epd, int sign);
EXTERN void EpdMakeNan(EpDouble *epd);
EXTERN void EpdCopy(EpDouble *from, EpDouble *to);
EXTERN int EpdIsInf(EpDouble *epd);
EXTERN int EpdIsZero(EpDouble *epd);
EXTERN int EpdIsNan(EpDouble *epd);
EXTERN int EpdIsNanOrInf(EpDouble *epd);
EXTERN int IsInfDouble(double value);
EXTERN int IsNanDouble(double value);
EXTERN int IsNanOrInfDouble(double value);

#endif /* _EPD */
