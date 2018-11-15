/*
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  * Additional permission under GNU GPL version 3 section 7
  *
  * If you modify this Program, or any covered work, by linking or combining
  * it with OpenSSL (or a modified version of that library), containing parts
  * covered by the terms of OpenSSL License and SSLeay License, the licensors
  * of this Program grant you additional permission to convey the resulting work.
  *
  */

/*
 * Parts of this file are originally copyright (c) 2014-2017, The Monero Project
 */
#pragma once

#ifdef __x86_64__
#ifdef __GNUC__
#include <x86intrin.h>
#else
#include <intrin.h>
#endif // __GNUC__
#endif

#include <inttypes.h>


#define saes_data(w) {\
	w(0x63), w(0x7c), w(0x77), w(0x7b), w(0xf2), w(0x6b), w(0x6f), w(0xc5),\
	w(0x30), w(0x01), w(0x67), w(0x2b), w(0xfe), w(0xd7), w(0xab), w(0x76),\
	w(0xca), w(0x82), w(0xc9), w(0x7d), w(0xfa), w(0x59), w(0x47), w(0xf0),\
	w(0xad), w(0xd4), w(0xa2), w(0xaf), w(0x9c), w(0xa4), w(0x72), w(0xc0),\
	w(0xb7), w(0xfd), w(0x93), w(0x26), w(0x36), w(0x3f), w(0xf7), w(0xcc),\
	w(0x34), w(0xa5), w(0xe5), w(0xf1), w(0x71), w(0xd8), w(0x31), w(0x15),\
	w(0x04), w(0xc7), w(0x23), w(0xc3), w(0x18), w(0x96), w(0x05), w(0x9a),\
	w(0x07), w(0x12), w(0x80), w(0xe2), w(0xeb), w(0x27), w(0xb2), w(0x75),\
	w(0x09), w(0x83), w(0x2c), w(0x1a), w(0x1b), w(0x6e), w(0x5a), w(0xa0),\
	w(0x52), w(0x3b), w(0xd6), w(0xb3), w(0x29), w(0xe3), w(0x2f), w(0x84),\
	w(0x53), w(0xd1), w(0x00), w(0xed), w(0x20), w(0xfc), w(0xb1), w(0x5b),\
	w(0x6a), w(0xcb), w(0xbe), w(0x39), w(0x4a), w(0x4c), w(0x58), w(0xcf),\
	w(0xd0), w(0xef), w(0xaa), w(0xfb), w(0x43), w(0x4d), w(0x33), w(0x85),\
	w(0x45), w(0xf9), w(0x02), w(0x7f), w(0x50), w(0x3c), w(0x9f), w(0xa8),\
	w(0x51), w(0xa3), w(0x40), w(0x8f), w(0x92), w(0x9d), w(0x38), w(0xf5),\
	w(0xbc), w(0xb6), w(0xda), w(0x21), w(0x10), w(0xff), w(0xf3), w(0xd2),\
	w(0xcd), w(0x0c), w(0x13), w(0xec), w(0x5f), w(0x97), w(0x44), w(0x17),\
	w(0xc4), w(0xa7), w(0x7e), w(0x3d), w(0x64), w(0x5d), w(0x19), w(0x73),\
	w(0x60), w(0x81), w(0x4f), w(0xdc), w(0x22), w(0x2a), w(0x90), w(0x88),\
	w(0x46), w(0xee), w(0xb8), w(0x14), w(0xde), w(0x5e), w(0x0b), w(0xdb),\
	w(0xe0), w(0x32), w(0x3a), w(0x0a), w(0x49), w(0x06), w(0x24), w(0x5c),\
	w(0xc2), w(0xd3), w(0xac), w(0x62), w(0x91), w(0x95), w(0xe4), w(0x79),\
	w(0xe7), w(0xc8), w(0x37), w(0x6d), w(0x8d), w(0xd5), w(0x4e), w(0xa9),\
	w(0x6c), w(0x56), w(0xf4), w(0xea), w(0x65), w(0x7a), w(0xae), w(0x08),\
	w(0xba), w(0x78), w(0x25), w(0x2e), w(0x1c), w(0xa6), w(0xb4), w(0xc6),\
	w(0xe8), w(0xdd), w(0x74), w(0x1f), w(0x4b), w(0xbd), w(0x8b), w(0x8a),\
	w(0x70), w(0x3e), w(0xb5), w(0x66), w(0x48), w(0x03), w(0xf6), w(0x0e),\
	w(0x61), w(0x35), w(0x57), w(0xb9), w(0x86), w(0xc1), w(0x1d), w(0x9e),\
	w(0xe1), w(0xf8), w(0x98), w(0x11), w(0x69), w(0xd9), w(0x8e), w(0x94),\
	w(0x9b), w(0x1e), w(0x87), w(0xe9), w(0xce), w(0x55), w(0x28), w(0xdf),\
	w(0x8c), w(0xa1), w(0x89), w(0x0d), w(0xbf), w(0xe6), w(0x42), w(0x68),\
	w(0x41), w(0x99), w(0x2d), w(0x0f), w(0xb0), w(0x54), w(0xbb), w(0x16) }

#define SAES_WPOLY           0x011b

#define saes_b2w(b0, b1, b2, b3) (((uint32_t)(b3) << 24) | \
	((uint32_t)(b2) << 16) | ((uint32_t)(b1) << 8) | (b0))

#define saes_f2(x)   ((x<<1) ^ (((x>>7) & 1) * SAES_WPOLY))
#define saes_f3(x)   (saes_f2(x) ^ x)
#define saes_h0(x)   (x)

#define saes_u0(p)   saes_b2w(saes_f2(p),          p,          p, saes_f3(p))
#define saes_u1(p)   saes_b2w(saes_f3(p), saes_f2(p),          p,          p)
#define saes_u2(p)   saes_b2w(         p, saes_f3(p), saes_f2(p),          p)
#define saes_u3(p)   saes_b2w(         p,          p, saes_f3(p), saes_f2(p))

alignas(16) const uint32_t saes_table[4][256] = { saes_data(saes_u0), saes_data(saes_u1), saes_data(saes_u2), saes_data(saes_u3) };
alignas(16) const uint8_t  saes_sbox[256] = saes_data(saes_h0);


#ifdef __x86_64__

#define _mm_aesenc_si128(...) soft_aesenc(__VA_ARGS__)
#define _mm_aeskeygenassist_si128(...) soft_aeskeygenassist(__VA_ARGS__)

#else

#include <string.h>
#include <stdlib.h>

#if 0
/* math lib: long double sqrtl(long double) */
typedef union {int64_t i[2]; long double x; double d[2]; } mynumber;

static const double
  t512 = 0x1p512,   /* 0x5ff0000000000000 */
  tm256 = 0x1p-256, /* 0x2ff0000000000000 */
  two54 = 0x1p54,	/* 0x4350000000000000 */
  twom54 = 0x1p-54;	/* 0x3C90000000000000 */

/*********************************************************************/
/* An ultimate sqrt routine. Given an IEEE double machine number x   */
/* it computes the correctly rounded (to nearest) value of square    */
/* root of x.                                                        */
/*********************************************************************/
static long double __ieee754_sqrt(long double x)
{
  static const long double big = 134217728.0, big1 = 134217729.0;
  long double t,s,i;
  mynumber a,c;
  uint64_t k, l;
  int64_t m, n;
  double d;

  a.x=x;
  k=a.i[0] & INT64_C(0x7fffffffffffffff);
  /*----------------- 2^-1022  <= | x |< 2^1024  -----------------*/
  if (k>INT64_C(0x000fffff00000000) && k<INT64_C(0x7ff0000000000000)) {
    if (x < 0) return (big1-big1)/(big-big);
    l = (k&INT64_C(0x001fffffffffffff))|INT64_C(0x3fe0000000000000);
    if ((a.i[1] & INT64_C(0x7fffffffffffffff)) != 0) {
      n = (int64_t) ((l - k) * 2) >> 53;
      m = (a.i[1] >> 52) & 0x7ff;
      if (m == 0) {
	a.d[1] *= two54;
	m = ((a.i[1] >> 52) & 0x7ff) - 54;
      }
      m += n;
      if (m > 0)
	a.i[1] = (a.i[1] & INT64_C(0x800fffffffffffff)) | (m << 52);
      else if (m <= -54) {
	a.i[1] &= INT64_C(0x8000000000000000);
      } else {
	m += 54;
	a.i[1] = (a.i[1] & INT64_C(0x800fffffffffffff)) | (m << 52);
	a.d[1] *= twom54;
      }
    }
    a.i[0] = l;
    s = a.x;
    d = __ieee754_sqrt (a.d[0]);
    c.i[0] = INT64_C(0x2000000000000000)+((k&INT64_C(0x7fe0000000000000))>>1);
    c.i[1] = 0;
    i = d;
    t = 0.5L * (i + s / i);
    i = 0.5L * (t + s / t);
    return c.x * i;
  }
  else {
    if (k>=INT64_C(0x7ff0000000000000))
      /* sqrt (-Inf) = NaN, sqrt (NaN) = NaN, sqrt (+Inf) = +Inf.  */
      return x * x + x;
    if (x == 0) return x;
    if (x < 0) return (big1-big1)/(big-big);
    return tm256*__ieee754_sqrt(x*t512);
  }
}
#endif

/* emu x86 code */
struct mm128int {
    uint32_t x[4];
};

typedef struct mm128int __m128i;

static inline __m128i _mm_set_epi32(int e3, int e2, int e1, int e0)
{
    __m128i a;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    a.x[0] = e0;
    a.x[1] = e1;
    a.x[2] = e2;
    a.x[3] = e3;
#else
    a.x[0] = e3;
    a.x[1] = e2;
    a.x[2] = e1;
    a.x[3] = e0;
#endif
    return a;
}

static inline __m128i _mm_set_epi64x(int64_t e1, int64_t e0)
{
    __m128i a;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(&(a.x[0]), &e0, 8);
    memcpy(&(a.x[2]), &e1, 8);
#else
    memcpy(&(a.x[2]), &e0, 8);
    memcpy(&(a.x[0]), &e1, 8);
#endif

    return a;
}

static inline uint32_t _mm_cvtsi128_si32(__m128i a)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return a.x[0];
#else
    return a.x[3];
#endif
}

static inline uint64_t _mm_cvtsi128_si64(__m128i a)
{
    uint64_t tmp;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(&tmp, &(a.x[0]), 8);
#else
    memcpy(&tmp, &(a.x[2]), 8);
#endif

    return tmp;
}

static inline uint32_t _mm_select4(__m128i a, int imm8)
{
    int control;
    control = imm8 & 0x00000003;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return a.x[control];
#else
    return a.x[3 - control];
#endif
}

static inline __m128i _mm_shuffle_epi32(__m128i a, int imm8)
{
    __m128i b;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    b.x[0] = _mm_select4(a, (imm8 & 0x00000003));
    b.x[1] = _mm_select4(a, (imm8 & 0x0000000f) >> 2);
    b.x[2] = _mm_select4(a, (imm8 & 0x0000003f) >> 4);
    b.x[3] = _mm_select4(a, (imm8 & 0x000000ff) >> 6);
#else
    b.x[3] = _mm_select4(a, (imm8 & 0x00000003));
    b.x[2] = _mm_select4(a, (imm8 & 0x0000000f) >> 2);
    b.x[1] = _mm_select4(a, (imm8 & 0x0000003f) >> 4);
    b.x[0] = _mm_select4(a, (imm8 & 0x000000ff) >> 6);
#endif

    return b;
}

static inline __m128i _mm_slli_si128(__m128i a, int imm8)
{
    __m128i b = {
        .x = {0, 0, 0, 0},
    };
    int tmp;
    int o1;

    tmp = imm8 & 0x000000ff;
    if (tmp > 15) {
        tmp = 16;
    }
    tmp = tmp * 8;

    uint64_t *p, *q, tmp64;
    p = (uint64_t *)&a;
    q = (uint64_t *)&b;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    if (tmp <= 64) {
        q[0] = p[0] << tmp;
        q[1] = p[1] << tmp;
        o1 = 64 - tmp;
        tmp64 = p[0] >> o1;
        q[1] = q[1] + tmp64;
    } else {
        o1 = tmp - 64;
        q[1] = p[0] << o1;
        q[0] = 0;
    }
#else
    if (tmp <= 64) {
        q[0] = p[0] << tmp;
        q[1] = p[1] << tmp;
        o1 = 64 - tmp;
        tmp64 = p[1] >> o1;
        q[0] = q[0] + tmp64;
    } else {
        o1 = tmp - 64;
        q[0] = p[1] << o1;
        q[1] = 0;
    }
#endif

    return b;
}

static inline __m128i _mm_xor_si128(__m128i a, __m128i b)
{
    __m128i c;

    c.x[0] = a.x[0] ^ b.x[0];
    c.x[1] = a.x[1] ^ b.x[1];
    c.x[2] = a.x[2] ^ b.x[2];
    c.x[3] = a.x[3] ^ b.x[3];

    return c;
}

static inline __m128i _mm_load_si128(const void* mem_addr)
{
    __m128i a;

    memcpy(&a, mem_addr, 16);

    return a;
}

static inline void _mm_store_si128(void* mem_addr, __m128i a)
{
    memcpy(mem_addr, &a, 16);
}

static inline __m128i _mm_setzero_si128()
{
    __m128i a;

    memset(&a, 0, 16);

    return a;
}

static inline __m128i _mm_cmpeq_epi32(__m128i a, __m128i b)
{
    __m128i c;

    c.x[0] = (a.x[0] == b.x[0]) ? 0xFFFFFFFF : 0;
    c.x[1] = (a.x[1] == b.x[1]) ? 0xFFFFFFFF : 0;
    c.x[2] = (a.x[2] == b.x[2]) ? 0xFFFFFFFF : 0;
    c.x[3] = (a.x[3] == b.x[3]) ? 0xFFFFFFFF : 0;

    return c;
}

static inline __m128i _mm_add_epi64(__m128i a, __m128i b)
{
    __m128i c = {
        .x = {0, 0, 0, 0},
    };

    uint64_t *x1, *x2, *x3;

    x1 = (uint64_t *)&a;
    x2 = (uint64_t *)&b;
    x3 = (uint64_t *)&c;

    x3[0] = x1[0] + x2[0];
    x3[1] = x1[1] + x2[1];

    return c;
}

static inline __m128i _mm_movehl_ps(__m128i a, __m128i b)
{
    __m128i c;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    c.x[0] = b.x[2];
    c.x[1] = b.x[3];
    c.x[2] = a.x[2];
    c.x[3] = a.x[3];
#else
    c.x[3] = b.x[1];
    c.x[2] = b.x[0];
    c.x[1] = a.x[1];
    c.x[0] = a.x[0];
#endif
    return c;
}

static inline void* _mm_malloc(size_t size, size_t align)
{
    void *p = malloc(size);
    return p;
}

static inline void _mm_free(void * mem_addr)
{
    free(mem_addr);
}

static inline __m128i _mm_castsi128_pd(__m128i a)
{
    return a;
}

static inline __m128i _mm_cvtsi64_si128(uint64_t a)
{
    __m128i c = {
        .x = {0, 0, 0, 0},
    };

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(&(c.x[0]), &a, 8);
#else
    memcpy(&(c.x[2]), &a, 8);
#endif

    return c;
}

#include <math.h>
static inline __m128i _mm_sqrt_sd(__m128i a, __m128i b)
{
    __m128i c;
    long double x, y;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(&x, &(b.x[0]), 8);
#else
    memcpy(&x, &(b.x[2]), 8);
#endif
    y = sqrtl(x);
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    memcpy(&(c.x[0]), &y, 8);
    memcpy(&(c.x[2]), &(a.x[2]), 8);
#else
    memcpy(&(c.x[2]), &y, 8);
    memcpy(&(c.x[0]), &(a.x[0]), 8);
#endif

    return c;
}

static inline __m128i _mm_castpd_si128(__m128i a)
{
    return a;
}

static inline __m128i _mm_setzero_pd(void)
{
    __m128i c = {
        .x = {0, 0, 0, 0},
    };

    return c;
}

static inline __m128i _mm_castsi128_ps(__m128i a)
{
    return a;
}

static inline __m128i _mm_castps_si128(__m128i a)
{
    return a;
}

static inline __m128i _mm_srli_si128(__m128i a, int imm8)
{
    __m128i b = {
        .x = {0, 0, 0, 0},
    };
    int tmp;
    int o1;

    tmp = imm8 & 0x000000ff;
    if (tmp > 15) {
        tmp = 16;
    }
    tmp = tmp * 8;

    uint64_t *p, *q, tmp64;
    p = (uint64_t *)&a;
    q = (uint64_t *)&b;

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    if (tmp <= 64) {
        q[0] = p[0] >> tmp;
        q[1] = p[1] >> tmp;
        o1 = 64 - tmp;
        tmp64 = p[1] << o1;
        q[0] = q[0] + tmp64;
    } else {
        o1 = tmp - 64;
        q[0] = p[1] >> o1;
        q[1] = 0;
    }
#else
    if (tmp <= 64) {
        q[0] = p[0] >> tmp;
        q[1] = p[1] >> tmp;
        o1 = 64 - tmp;
        tmp64 = p[0] << o1;
        q[1] = q[1] + tmp64;
    } else {
        o1 = tmp - 64;
        q[1] = p[0] >> o1;
        q[0] = 0;
    }
#endif

    return b;
}

#define _mm_prefetch(...) do {} while(0);

#endif


static inline __m128i soft_aesenc(__m128i in, __m128i key)
{
	uint32_t x0, x1, x2, x3;
	x0 = _mm_cvtsi128_si32(in);
	x1 = _mm_cvtsi128_si32(_mm_shuffle_epi32(in, 0x55));
	x2 = _mm_cvtsi128_si32(_mm_shuffle_epi32(in, 0xAA));
	x3 = _mm_cvtsi128_si32(_mm_shuffle_epi32(in, 0xFF));

	__m128i out = _mm_set_epi32(
		(saes_table[0][x3 & 0xff] ^ saes_table[1][(x0 >> 8) & 0xff] ^ saes_table[2][(x1 >> 16) & 0xff] ^ saes_table[3][x2 >> 24]),
		(saes_table[0][x2 & 0xff] ^ saes_table[1][(x3 >> 8) & 0xff] ^ saes_table[2][(x0 >> 16) & 0xff] ^ saes_table[3][x1 >> 24]),
		(saes_table[0][x1 & 0xff] ^ saes_table[1][(x2 >> 8) & 0xff] ^ saes_table[2][(x3 >> 16) & 0xff] ^ saes_table[3][x0 >> 24]),
		(saes_table[0][x0 & 0xff] ^ saes_table[1][(x1 >> 8) & 0xff] ^ saes_table[2][(x2 >> 16) & 0xff] ^ saes_table[3][x3 >> 24]));

	return _mm_xor_si128(out, key);
}

static inline uint32_t sub_word(uint32_t key)
{
	return (saes_sbox[key >> 24 ] << 24)   |
		(saes_sbox[(key >> 16) & 0xff] << 16 ) |
		(saes_sbox[(key >> 8)  & 0xff] << 8  ) |
		 saes_sbox[key & 0xff];
}

#if (!defined __x86_64__) || defined(__clang__)
static inline uint32_t _rotr(uint32_t value, uint32_t amount)
{
	return (value >> amount) | (value << ((32 - amount) & 31));
}
#endif

static inline __m128i soft_aeskeygenassist(__m128i key, uint8_t rcon)
{
	uint32_t X1 = sub_word(_mm_cvtsi128_si32(_mm_shuffle_epi32(key, 0x55)));
	uint32_t X3 = sub_word(_mm_cvtsi128_si32(_mm_shuffle_epi32(key, 0xFF)));
	return _mm_set_epi32(_rotr(X3, 8) ^ rcon, X3,_rotr(X1, 8) ^ rcon, X1);
}
