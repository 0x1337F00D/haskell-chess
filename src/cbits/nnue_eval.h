#ifndef NNUE_EVAL_H
#define NNUE_EVAL_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Performs the dot product between the accumulator (Int32) and the weights (Int16) for a row in the HalfKP architecture.
// Applies clipped ReLU (min(max(x, 0), 127)) to the accumulator values before multiplication.
int32_t dotAccRowHalfKPC(const int32_t* accBA, const int16_t* wBA, int accN, int row, int32_t z0, int usOffset, int themOffset);

// Performs the dot product between the H1 activations (Int32) and the weights (Int16) for the output layer.
// Also applies clipped ReLU to the activations before multiplication.
int32_t dotH2RowC(const int32_t* actBA, const int16_t* wBA, int hidN, int row, int32_t z0);

#ifdef __cplusplus
}
#endif

#endif // NNUE_EVAL_H
