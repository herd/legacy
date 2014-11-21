/*********************************************************************/
/*                          Litmus                                   */
/*                                                                   */
/*        Luc Maranget, INRIA Paris-Rocquencourt, France.            */
/*        Susmit Sarkar, University of Cambridge, UK.                */
/*                                                                   */
/*  Copyright 2010 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

#include <stdint.h>
#include "litmus_rand.h"

/*
  Simple generator
  http://en.wikipedia.org/wiki/Linear_congruential_generator
*/


/*

  From ocaml sources: (globroot.c)
  Linear congruence with modulus = 2^32, multiplier = 69069
  (Knuth vol 2 p. 106, line 15 of table 1), additive = 25173.


  Knuth (vol 2 p. 13) shows that the least significant bits are
  "less random" than the most significant bits with a modulus of 2^m.
  We just swap half words, enough? */

static const uint32_t a = 69069;
static const uint32_t c = 25173 ;

inline static uint32_t unlocked_rand(st_t *st)  {
  uint32_t r = a * *st + c ;
  *st = r ;
  /* Swap high & low bits */
  uint32_t low = r & 0xffff ;
  uint32_t high = r >> 16 ;
  r = high | (low << 16) ;
  return r ;
}

int rand_bit(st_t *st)  {
  uint32_t r = unlocked_rand(st) ;
  r &= 1 ;
  return r ; 
}

static const uint32_t r_max = UINT32_MAX ;

uint32_t rand_k (uint32_t *st,uint32_t k) {
  uint32_t r, v ;
  do {
    r = unlocked_rand(st) ;
    v = r % k ;
  } while (r-v > r_max-k+1) ;
  return v ;
}
