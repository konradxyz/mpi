/*
 * An implementation of the matrix generation library
 * for the MPI assignment.
 * Concurrent and Distributed Programming Course, spring 2014.
 * Faculty of Mathematics, Informatics and Mechanics.
 * University of Warsaw, Warsaw, Poland.
 *
 * The implementation is based on the Mersenne Twister generator.
 * See below for the license.
 * 
 * The adaptation is copyright (C) Konrad Iwanicki, 2014.
 */

/* http://www.math.keio.ac.jp/~matumoto/ver980409.html
 *
 * This is the ``Mersenne Twister'' random number generator MT19937, which
 * generates pseudorandom integers uniformly distributed in 0..(2^32 - 1)
 * starting from any odd seed in 0..(2^32 - 1).  This version is a recode
 * by Shawn Cokus (Cokus@math.washington.edu) on March 8, 1998 of a version by
 * Takuji Nishimura (who had suggestions from Topher Cooper and Marc Rieffel in
 * July-August 1997).
 *
 * Effectiveness of the recoding (on Goedel2.math.washington.edu, a DEC Alpha
 * running OSF/1) using GCC -O3 as a compiler: before recoding: 51.6 sec. to
 * generate 300 million random numbers; after recoding: 24.0 sec. for the same
 * (i.e., 46.5% of original time), so speed is now about 12.5 million random
 * number generations per second on this machine.
 *
 * According to the URL <http: *www.math.keio.ac.jp/~matumoto/emt.html>
 * (and paraphrasing a bit in places), the Mersenne Twister is ``designed
 * with consideration of the flaws of various existing generators,'' has
 * a period of 2^19937 - 1, gives a sequence that is 623-dimensionally
 * equidistributed, and ``has passed many stringent tests, including the
 * die-hard test of G. Marsaglia and the load test of P. Hellekalek and
 * S. Wegenkittl.''  It is efficient in memory usage (typically using 2506
 * to 5012 bytes of static data, depending on data type sizes, and the code
 * is quite short as well).  It generates random numbers in batches of 624
 * at a time, so the caching and pipelining of modern systems is exploited.
 * It is also divide- and mod-free.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation (either version 2 of the License or, at your
 * option, any later version).  This library is distributed in the hope that
 * it will be useful, but WITHOUT ANY WARRANTY, without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
 * the GNU Library General Public License for more details.  You should have
 * received a copy of the GNU Library General Public License along with this
 * library; if not, write to the Free Software Foundation, Inc., 59 Temple
 * Place, Suite 330, Boston, MA 02111-1307, USA.
 *
 * The code as Shawn received it included the following notice:
 *
 *   Copyright (C) 1997 Makoto Matsumoto and Takuji Nishimura.  When
 *   you use this, send an e-mail to <matumoto@math.keio.ac.jp> with
 *   an appropriate reference to your work.
 *
 * It would be nice to CC: <Cokus@math.washington.edu> when you write.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "matgen.h"

typedef unsigned long matgen_uint32_t;


#define N              (624)                  /* length of state vector */
#define M              (397)                  /* a period parameter */
#define K              (0x9908B0DFUL)         /* a magic constant */
#define hiBit(u)       ((u) & 0x80000000UL)   /* mask all but highest   bit of u */
#define loBit(u)       ((u) & 0x00000001UL)   /* mask all but lowest    bit of u */
#define loBits(u)      ((u) & 0x7FFFFFFFUL)   /* mask     the highest   bit of u */
#define mixBits(u, v)  (hiBit(u)|loBits(v))   /* move hi bit of u to hi bit of v */
#define NUMBER_RANGE   200001UL

struct matgen_s
{
    unsigned          numLeft;
    unsigned          next;
    matgen_uint32_t   state[N+1];
};



static inline void seedMT(matgen_t * matgenPtr, matgen_uint32_t seed)
{
    matgen_uint32_t     x = (seed | 1U) & 0xFFFFFFFFU;
    matgen_uint32_t *   s = &(matgenPtr->state[0]);
    int                 j;

    matgenPtr->next = N;
    for(j = 0, *s++ = x; j < N - 1; *s++ = (x *= 69069U) & 0xFFFFFFFFU) ++j;
    *s++ = 0;
}



static inline matgen_uint32_t reloadMT(matgen_t * matgenPtr)
{
    matgen_uint32_t * p0 = &(matgenPtr->state[0]);
    matgen_uint32_t * p2 = &(matgenPtr->state[2]);
    matgen_uint32_t * pM = &(matgenPtr->state[M]);
    matgen_uint32_t   s0, s1;
    int               j;

    matgenPtr->next = 1;

    for (s0 = matgenPtr->state[0], s1 = matgenPtr->state[1], j = N - M + 1; --j; s0 = s1, s1 = *p2++)
        *p0++ = *pM++ ^ (mixBits(s0, s1) >> 1) ^ (loBit(s1) ? K : 0U);

    for (pM = &(matgenPtr->state[0]), j = M; --j; s0 = s1, s1 = *p2++)
        *p0++ = *pM++ ^ (mixBits(s0, s1) >> 1) ^ (loBit(s1) ? K : 0U);

    s1 = matgenPtr->state[0], *p0 = *pM ^ (mixBits(s0, s1) >> 1) ^ (loBit(s1) ? K : 0U);
    s1 ^= (s1 >> 11);
    s1 ^= (s1 <<  7) & 0x9D2C5680U;
    s1 ^= (s1 << 15) & 0xEFC60000U;
    return(s1 ^ (s1 >> 18));
}



static inline matgen_uint32_t randomMT(matgen_t * matgenPtr)
{
    matgen_uint32_t y;


    if(matgenPtr->next >= N)
        return (reloadMT(matgenPtr));

    y  = matgenPtr->state[matgenPtr->next++];
    y ^= (y >> 11);
    y ^= (y <<  7) & 0x9D2C5680U;
    y ^= (y << 15) & 0xEFC60000U;
    return(y ^ (y >> 18));
}



matgen_t * matgenNew(int numRows, int numColumns, int seed)
{
    matgen_t * matgenPtr;
    
    if (numRows <= 0 || numColumns <= 0 || seed <= 0)
    {
	return NULL;
    }
    matgenPtr = (matgen_t *)malloc(sizeof(struct matgen_s));
    if (matgenPtr != NULL)
    {
	matgenPtr->numLeft = (unsigned)numRows * (unsigned)numColumns;
	seedMT(matgenPtr, seed);
    }
    return matgenPtr;
}



int matgenGenerate(matgen_t * matgenPtr)
{
    matgen_uint32_t res;
    if (matgenPtr->numLeft == 0)
    {
	fprintf(stderr, "No more matrix elements to generate!\n");
	exit(1);
    }
    --matgenPtr->numLeft;
    res = randomMT(matgenPtr) % NUMBER_RANGE;
    return (int)res - (NUMBER_RANGE >> 1);
}



void matgenDestroy(matgen_t * matgenPtr)
{
    free(matgenPtr);
}



matgen_serialized_t * matgenNewSerialized(matgen_t const * matgenPtr)
{
    matgen_serialized_t * serPtr;
    unsigned char *       ptr;
    unsigned              i;
    
    serPtr = (matgen_serialized_t *)malloc(sizeof(matgen_serialized_t));
    if (serPtr == NULL)
    {
	return NULL;
    }
    serPtr->size = 4 * (N + 3);
    serPtr->bytes = (unsigned char *)malloc(serPtr->size);
    if (serPtr->bytes == NULL)
    {
	free(serPtr);
	return NULL;
    }
    ptr = serPtr->bytes;
    *ptr = (unsigned char)(matgenPtr->numLeft >> 24);
    ++ptr;
    *ptr = (unsigned char)(matgenPtr->numLeft >> 16);
    ++ptr;
    *ptr = (unsigned char)(matgenPtr->numLeft >> 8);
    ++ptr;
    *ptr = (unsigned char)(matgenPtr->numLeft);
    ++ptr;
    *ptr = (unsigned char)(matgenPtr->next >> 24);
    ++ptr;
    *ptr = (unsigned char)(matgenPtr->next >> 16);
    ++ptr;
    *ptr = (unsigned char)(matgenPtr->next >> 8);
    ++ptr;
    *ptr = (unsigned char)(matgenPtr->next);
    ++ptr;
    for (i = 0; i < N + 1; ++i)
    {
        *ptr = (unsigned char)(matgenPtr->state[i] >> 24);
        ++ptr;
        *ptr = (unsigned char)(matgenPtr->state[i] >> 16);
        ++ptr;
        *ptr = (unsigned char)(matgenPtr->state[i] >> 8);
        ++ptr;
        *ptr = (unsigned char)(matgenPtr->state[i]);
        ++ptr;
    }
    return serPtr;
}



matgen_t * matgenFromSerialized(matgen_serialized_t const * serPtr)
{
    matgen_t *      matgenPtr;
    unsigned char * ptr;
    unsigned        i;
    
    if (serPtr->size != 4 * (N + 3))
    {
	return NULL;
    }
    matgenPtr = (matgen_t *)malloc(sizeof(matgen_t));
    if (matgenPtr == NULL)
    {
	return NULL;
    }
    ptr = serPtr->bytes;
    matgenPtr->numLeft = *ptr;
    ++ptr;
    matgenPtr->numLeft = (matgenPtr->numLeft << 8) | (*ptr);
    ++ptr;
    matgenPtr->numLeft = (matgenPtr->numLeft << 8) | (*ptr);
    ++ptr;
    matgenPtr->numLeft = (matgenPtr->numLeft << 8) | (*ptr);
    ++ptr;
    matgenPtr->next = *ptr;
    ++ptr;
    matgenPtr->next = (matgenPtr->next << 8) | (*ptr);
    ++ptr;
    matgenPtr->next = (matgenPtr->next << 8) | (*ptr);
    ++ptr;
    matgenPtr->next = (matgenPtr->next << 8) | (*ptr);
    ++ptr;
    for (i = 0; i < N + 1; ++i)
    {
	matgenPtr->state[i] = *ptr;
	++ptr;
        matgenPtr->state[i] = (matgenPtr->state[i] << 8) | (*ptr);
	++ptr;
        matgenPtr->state[i] = (matgenPtr->state[i] << 8) | (*ptr);
	++ptr;
        matgenPtr->state[i] = (matgenPtr->state[i] << 8) | (*ptr);
	++ptr;
    }
    return matgenPtr;
}



void matgenDestroySerialized(matgen_serialized_t * serPtr)
{
    free(serPtr->bytes);
    free(serPtr);
}
