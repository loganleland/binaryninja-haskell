#include "binaryninjacore.h"

// BINARYNINJACOREAPI BNLowLevelILInstruction BNGetLowLevelILByIndex(BNLowLevelILFunction* func, size_t i);
BNLowLevelILInstruction* BNGetLowLevelILByIndexPtr(BNLowLevelILFunction* func, size_t i)
{
  BNLowLevelILInstruction tmp = BNGetLowLevelILByIndex(func, i);
  BNLowLevelILInstruction* out = (BNLowLevelILInstruction*)malloc(sizeof (struct BNLowLevelILInstruction));
  if (!out) return 0;
  *out = tmp;
  return out;
}


void freeBNLowLevelInstruction(BNLowLevelILInstruction* inst)
{
  free(inst);
}


// BINARYNINJACOREAPI BNMediumLevelILInstruction BNGetMediumLevelILByIndex(BNMediumLevelILFunction* func, size_t i);
BNMediumLevelILInstruction* BNGetMediumLevelILByIndexPtr(BNMediumLevelILFunction* func, size_t i)
{
  BNMediumLevelILInstruction tmp = BNGetMediumLevelILByIndex(func, i);
  BNMediumLevelILInstruction* out = (BNMediumLevelILInstruction*)malloc(sizeof (struct BNMediumLevelILInstruction));
  if (!out) return 0;
  *out = tmp;
  return out;
}


void freeBNMediumLevelInstruction(BNMediumLevelILInstruction* inst)
{
  free(inst);
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

