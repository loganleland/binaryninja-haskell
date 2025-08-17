#include "binaryninjacore.h"

// BINARYNINJACOREAPI BNLowLevelILInstruction BNGetLowLevelILByIndex(BNLowLevelILFunction* func, size_t i);
BNLowLevelILInstruction* BNGetLowLevelILByIndexPtr(BNLowLevelILFunction* func, size_t i) {
  BNLowLevelILInstruction tmp = BNGetLowLevelILByIndex(func, i);
  BNLowLevelILInstruction* out = (BNLowLevelILInstruction*)malloc(sizeof (struct BNLowLevelILInstruction));
  if (!out) return 0;
  *out = tmp;
  return out;
}


// BINARYNINJACOREAPI BNMediumLevelILInstruction BNGetMediumLevelILByIndex(BNMediumLevelILFunction* func, size_t i);
BNMediumLevelILInstruction* BNGetMediumLevelILByIndexPtr(BNMediumLevelILFunction* func, size_t i) {
  BNMediumLevelILInstruction tmp = BNGetMediumLevelILByIndex(func, i);
  BNMediumLevelILInstruction* out = (BNMediumLevelILInstruction*)malloc(sizeof (struct BNMediumLevelILInstruction));
  if (!out) return 0;
  *out = tmp;
  return out;
}

