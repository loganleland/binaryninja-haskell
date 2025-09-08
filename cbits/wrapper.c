#include "binaryninjacore.h"

void BNGetLowLevelILByIndexPtr(BNLowLevelILInstruction* out,
                               BNLowLevelILFunction* func, size_t i)
{
  *out = BNGetLowLevelILByIndex(func, i);
}


void BNGetMediumLevelILByIndexPtr(BNMediumLevelILInstruction* out,
                                  BNMediumLevelILFunction* func, size_t i)
{
  *out = BNGetMediumLevelILByIndex(func, i);
}


void BNGetMediumLevelSSAILByIndexPtr(BNMediumLevelILInstruction* out,
                                     BNMediumLevelILFunction* func, size_t i)
{
  return BNGetMediumLevelILByIndexPtr(out, func, i);
}


void BNFromVariableIdentifierPtr(BNVariable* out, uint64_t index)
{
  *out = BNFromVariableIdentifier(index);
}


BNPossibleValueSet* BNGetCachedMediumLevelILPossibleValueSetPtr(BNPossibleValueSet* out,
                                                                BNMediumLevelILFunction* func, size_t idx)
{
  *out = BNGetCachedMediumLevelILPossibleValueSet(func, idx);
}

