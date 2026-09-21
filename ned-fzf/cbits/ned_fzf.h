/* Bulk matching over a candidate arena, on top of the vendored fzf matcher.
 *
 * A candidate arena is one buffer of NUL-terminated UTF-8 strings plus an
 * offset table of count + 1 entries: candidate i runs from arena + offsets[i]
 * to its NUL, and offsets[count] is the end of the buffer. Scoring a whole
 * arena is one call, so a query over a large file list does not pay an FFI
 * crossing per candidate.
 */
#ifndef NED_FZF_H_
#define NED_FZF_H_

#include "fzf.h"

#include <stddef.h>
#include <stdint.h>

/* Score every candidate against `pattern` and write the best ones, best
 * first, into `out_index` and `out_score` (both at least `limit` long).
 *
 * Ties go to the shorter candidate, then to the earlier one, so equally good
 * matches keep the arena's order. A pattern with no terms matches everything
 * with score 1 and is not sorted at all: the arena's order is the result.
 *
 * Returns the number of candidates that matched, which may exceed `limit`;
 * the number written is the smaller of the two.
 */
size_t ned_fzf_match_many(fzf_pattern_t *pattern, fzf_slab_t *slab,
                           const char *arena, const uint32_t *offsets,
                           size_t count, size_t limit, uint32_t *out_index,
                           int32_t *out_score);

/* The matched character offsets `fzf_get_positions` returned, and how many
 * there are. Reading the struct's fields through these keeps its layout on the
 * C side. `pos` may be NULL, which reads as an empty run of offsets. */
const uint32_t *ned_fzf_positions_data(fzf_position_t *pos);
size_t ned_fzf_positions_size(fzf_position_t *pos);

#endif // NED_FZF_H_
