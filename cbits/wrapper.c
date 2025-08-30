#include "binaryninjacore.h"

void BNGetLowLevelILByIndexPtr(BNLowLevelILInstruction* out, BNLowLevelILFunction* func, size_t i)
{
  *out = BNGetLowLevelILByIndex(func, i);
}


void BNGetMediumLevelILByIndexPtr(BNMediumLevelILInstruction* out, BNMediumLevelILFunction* func, size_t i)
{
  *out = BNGetMediumLevelILByIndex(func, i);
}


void BNGetMediumLevelSSAILByIndexPtr(BNMediumLevelILInstruction* out,
                                                            BNMediumLevelILFunction* func, size_t i)
{
  return BNGetMediumLevelILByIndexPtr(out, func, i);
}


BNVariable* BNFromVariableIdentifierPtr(uint64_t index)
{
  BNVariable tmp = BNFromVariableIdentifier(index);
  BNVariable* out = (BNVariable*)malloc(sizeof (struct BNVariable));
  if (!out) return 0;
  *out = tmp;
  return out;
}


void freeBNVariable(BNVariable* var)
{
  free(var);
}



BNPossibleValueSet* BNGetCachedMediumLevelILPossibleValueSetPtr(BNMediumLevelILFunction* func, size_t idx)
{
  BNPossibleValueSet tmp = BNGetCachedMediumLevelILPossibleValueSet(func, idx);
  BNPossibleValueSet* out = (BNPossibleValueSet*)malloc(sizeof (struct BNPossibleValueSet));
  if (!out) return 0;
  *out = tmp;
  return out;
}


void freeBNPossibleValueSet(BNPossibleValueSet* vs)
{
  free(vs);
}


