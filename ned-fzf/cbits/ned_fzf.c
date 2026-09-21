#include "ned_fzf.h"

#include <stdlib.h>

/* One scored candidate. The length is kept so the sort can break ties on it
 * without walking the arena again. */
typedef struct {
  int32_t score;
  uint32_t index;
  uint32_t len;
} ned_fzf_hit_t;

/* Best first: higher score, then shorter candidate, then earlier one. The
 * index tiebreak makes the order total, so the sort is deterministic whatever
 * qsort does with equal elements. */
static int ned_fzf_cmp(const void *a, const void *b) {
  const ned_fzf_hit_t *x = (const ned_fzf_hit_t *)a;
  const ned_fzf_hit_t *y = (const ned_fzf_hit_t *)b;
  if (x->score != y->score) {
    return x->score > y->score ? -1 : 1;
  }
  if (x->len != y->len) {
    return x->len < y->len ? -1 : 1;
  }
  if (x->index != y->index) {
    return x->index < y->index ? -1 : 1;
  }
  return 0;
}

size_t ned_fzf_match_many(fzf_pattern_t *pattern, fzf_slab_t *slab,
                           const char *arena, const uint32_t *offsets,
                           size_t count, size_t limit, uint32_t *out_index,
                           int32_t *out_score) {
  if (count == 0) {
    return 0;
  }

  /* An empty query matches everything at score 1. Scoring and sorting it
   * would only shuffle equal candidates, so take the arena's order. */
  if (pattern->ptr == NULL || pattern->size == 0) {
    size_t written = count < limit ? count : limit;
    for (size_t i = 0; i < written; i++) {
      out_index[i] = (uint32_t)i;
      out_score[i] = 1;
    }
    return count;
  }

  ned_fzf_hit_t *hits =
      (ned_fzf_hit_t *)malloc(count * sizeof(ned_fzf_hit_t));
  if (hits == NULL) {
    return 0;
  }

  size_t matched = 0;
  for (size_t i = 0; i < count; i++) {
    int32_t score = fzf_get_score(arena + offsets[i], pattern, slab);
    if (score > 0) {
      hits[matched].score = score;
      hits[matched].index = (uint32_t)i;
      /* Offsets are candidate starts and every candidate carries its NUL, so
       * the gap to the next start is the length plus that NUL. */
      hits[matched].len = offsets[i + 1] - offsets[i] - 1;
      matched++;
    }
  }

  qsort(hits, matched, sizeof(ned_fzf_hit_t), ned_fzf_cmp);

  size_t written = matched < limit ? matched : limit;
  for (size_t i = 0; i < written; i++) {
    out_index[i] = hits[i].index;
    out_score[i] = hits[i].score;
  }

  free(hits);
  return matched;
}

const uint32_t *ned_fzf_positions_data(fzf_position_t *pos) {
  return pos == NULL ? NULL : pos->data;
}

size_t ned_fzf_positions_size(fzf_position_t *pos) {
  return pos == NULL ? 0 : pos->size;
}
