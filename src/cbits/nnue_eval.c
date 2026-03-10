#include "nnue_eval.h"
#include <immintrin.h>

static inline int16_t clippedRelu16(int32_t x) {
    if (x <= 0) return 0;
    if (x >= 127) return 127;
    return (int16_t)x;
}

int32_t dotAccRowHalfKPC(const int32_t* accBA, const int16_t* wBA, int accN, int row, int32_t z0, int usOffset, int themOffset) {
    int32_t z = z0;
    int baseUs = row * (accN * 2);
    int baseThem = baseUs + accN;

    int j = 0;
#if defined(__AVX512F__) && defined(__AVX512BW__)
    // We can process 16 elements at a time
    __m512i v_zero = _mm512_setzero_si512();
    __m512i v_127 = _mm512_set1_epi32(127);
    __m512i v_sum_us = _mm512_setzero_si512();
    __m512i v_sum_them = _mm512_setzero_si512();

    // We will multiply 16 Int16 by 16 Int16. But weights are Int16, while activations are Int32.
    // So after clamping activations to Int32 [0, 127], we pack them to Int16.

    for (; j <= accN - 16; j += 16) {
        // Load 16 int32s from us
        __m512i a_us = _mm512_loadu_si512((__m512i const*)&accBA[usOffset + j]);
        // clamp to 0
        a_us = _mm512_max_epi32(a_us, v_zero);
        // clamp to 127
        a_us = _mm512_min_epi32(a_us, v_127);

        // Load 16 int16s from weights
        __m256i w_us_16 = _mm256_loadu_si256((__m256i const*)&wBA[baseUs + j]);
        // Sign extend to int32
        __m512i w_us = _mm512_cvtepi16_epi32(w_us_16);

        // Multiply and add (a_us * w_us)
        // Note: _mm512_mullo_epi32 multiplies 32-bit integers and keeps lower 32-bits
        __m512i prod_us = _mm512_mullo_epi32(a_us, w_us);
        v_sum_us = _mm512_add_epi32(v_sum_us, prod_us);


        // Load 16 int32s from them
        __m512i a_them = _mm512_loadu_si512((__m512i const*)&accBA[themOffset + j]);
        a_them = _mm512_max_epi32(a_them, v_zero);
        a_them = _mm512_min_epi32(a_them, v_127);

        // Load weights
        __m256i w_them_16 = _mm256_loadu_si256((__m256i const*)&wBA[baseThem + j]);
        __m512i w_them = _mm512_cvtepi16_epi32(w_them_16);

        __m512i prod_them = _mm512_mullo_epi32(a_them, w_them);
        v_sum_them = _mm512_add_epi32(v_sum_them, prod_them);
    }

    int32_t sum_us_reduce = _mm512_reduce_add_epi32(v_sum_us);
    int32_t sum_them_reduce = _mm512_reduce_add_epi32(v_sum_them);
    z += sum_us_reduce + sum_them_reduce;
#elif defined(__AVX2__)
    __m256i v_zero = _mm256_setzero_si256();
    __m256i v_127 = _mm256_set1_epi32(127);
    __m256i v_sum_us = _mm256_setzero_si256();
    __m256i v_sum_them = _mm256_setzero_si256();

    for (; j <= accN - 8; j += 8) {
        __m256i a_us = _mm256_loadu_si256((__m256i const*)&accBA[usOffset + j]);
        a_us = _mm256_max_epi32(a_us, v_zero);
        a_us = _mm256_min_epi32(a_us, v_127);

        __m128i w_us_16 = _mm_loadu_si128((__m128i const*)&wBA[baseUs + j]);
        __m256i w_us = _mm256_cvtepi16_epi32(w_us_16);

        __m256i prod_us = _mm256_mullo_epi32(a_us, w_us);
        v_sum_us = _mm256_add_epi32(v_sum_us, prod_us);

        __m256i a_them = _mm256_loadu_si256((__m256i const*)&accBA[themOffset + j]);
        a_them = _mm256_max_epi32(a_them, v_zero);
        a_them = _mm256_min_epi32(a_them, v_127);

        __m128i w_them_16 = _mm_loadu_si128((__m128i const*)&wBA[baseThem + j]);
        __m256i w_them = _mm256_cvtepi16_epi32(w_them_16);

        __m256i prod_them = _mm256_mullo_epi32(a_them, w_them);
        v_sum_them = _mm256_add_epi32(v_sum_them, prod_them);
    }

    int32_t us_arr[8];
    int32_t them_arr[8];
    _mm256_storeu_si256((__m256i*)us_arr, v_sum_us);
    _mm256_storeu_si256((__m256i*)them_arr, v_sum_them);
    for(int k=0; k<8; k++) {
        z += us_arr[k] + them_arr[k];
    }
#endif

    // Tail processing
    int32_t zUs = 0;
    int32_t zThem = 0;
    for (; j < accN; ++j) {
        int32_t a_us = clippedRelu16(accBA[usOffset + j]);
        int32_t w_us = wBA[baseUs + j];
        zUs += a_us * w_us;

        int32_t a_them = clippedRelu16(accBA[themOffset + j]);
        int32_t w_them = wBA[baseThem + j];
        zThem += a_them * w_them;
    }
    z += zUs + zThem;

    return z;
}

int32_t dotH2RowC(const int32_t* actBA, const int16_t* wBA, int hidN, int row, int32_t z0) {
    int32_t z = z0;
    int base = row * hidN;
    int j = 0;

#if defined(__AVX512F__) && defined(__AVX512BW__)
    __m512i v_sum = _mm512_setzero_si512();

    for (; j <= hidN - 16; j += 16) {
        __m512i a = _mm512_loadu_si512((__m512i const*)&actBA[j]);

        __m256i w_16 = _mm256_loadu_si256((__m256i const*)&wBA[base + j]);
        __m512i w = _mm512_cvtepi16_epi32(w_16);

        __m512i prod = _mm512_mullo_epi32(a, w);
        v_sum = _mm512_add_epi32(v_sum, prod);
    }

    z += _mm512_reduce_add_epi32(v_sum);
#elif defined(__AVX2__)
    __m256i v_sum = _mm256_setzero_si256();
    for (; j <= hidN - 8; j += 8) {
        __m256i a = _mm256_loadu_si256((__m256i const*)&actBA[j]);
        __m128i w_16 = _mm_loadu_si128((__m128i const*)&wBA[base + j]);
        __m256i w = _mm256_cvtepi16_epi32(w_16);

        __m256i prod = _mm256_mullo_epi32(a, w);
        v_sum = _mm256_add_epi32(v_sum, prod);
    }
    int32_t arr[8];
    _mm256_storeu_si256((__m256i*)arr, v_sum);
    for(int k=0; k<8; k++) z += arr[k];
#endif

    for (; j < hidN; ++j) {
        int32_t a = actBA[j];
        int32_t w = wBA[base + j];
        z += a * w;
    }

    return z;
}
