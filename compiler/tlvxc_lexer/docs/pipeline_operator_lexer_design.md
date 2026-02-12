# Medi Lexer: Pipeline Operator (`|>`) Feature Flag Design

## Overview
- Add optional support for a pipeline operator `|>` as a single token, gated behind a Cargo feature flag.
- Default behavior (v0.1) remains unchanged: `|>` tokenizes as `|` (BitOr) + `>` (Greater).
- Support across all lexers:
  - Logos-based paths (streaming and chunked) recognize `|>` when feature is enabled.
  - Chunk boundary handling merges `|` at end-of-chunk with `>` in next chunk when the feature is enabled.

## Goals
- Add `|>` tokenization without changing default behavior or existing tests.
- Keep performance and memory characteristics identical to current paths.
- Preserve accurate source locations (start at `|`).

## Non-goals
- Parser/semantic changes or evaluation semantics of `|>`.
- Pretty-printer or formatting updates.

## Feature flagging strategy
- Introduce Cargo feature: `pipeline_op` in `compiler/medic_lexer/Cargo.toml`.
  - Default: disabled.
- Gate Logos recognition at compile-time using `#[cfg(feature = "pipeline_op")]` for the `|>` pattern.
  - Ensures the default lexer does NOT see a `|>` pattern and continues to emit `|` then `>`.
- TokenType can include the semantic variant unconditionally (safe, forward-compatible), or cfg-gated if desired. Design recommends adding it unconditionally for simplicity.

## Token model changes
- In `src/logos_token.rs`:
  - Add new variant:
    - `#[cfg(feature = "pipeline_op")]`
      `#[token("|>", priority = 60)]` `PipeGreater`
  - Placement: in the Operators section above single-char `BitOr` (`|`) and `Greater` (`>`). Priority > single-char tokens so `|>` wins when present.
- In `src/token.rs` (`TokenType`):
  - Add `PipeGreater` operator variant (unconditional).
    - Rationale: Safe to add; downstream code can ignore when feature off because Logos never produces it.

## Conversion mapping
- In `src/convert.rs`:
  - Add match arm mapping: `LogosToken::PipeGreater => TokenType::PipeGreater` under `#[cfg(feature = "pipeline_op")]`.
  - No changes to `ConversionConfig` required for compile-time gating. If a runtime toggle is desired later, a `pipeline_operator_as_pair: bool` could be introduced, but not needed now.
- In `src/streaming_lexer.rs` (inline conversion):
  - Add mapping: `LogosToken::PipeGreater => TokenType::PipeGreater` under `#[cfg(feature = "pipeline_op")]`.

## Lexer integration details

### Streaming lexer (`src/streaming_lexer.rs`)
- Logos will produce `LogosToken::PipeGreater` when feature is enabled; the inline match maps it to `TokenType::PipeGreater`.
- Location: the existing position tracking will anchor the token at the `|` character; no changes needed.
- Default behavior unchanged: test `test_pipeline_operator_not_tokenized_streaming` continues to pass with feature disabled.

### Chunked lexer (`src/chunked_lexer.rs`)
- Single-chunk case: when feature is enabled, Logos produces `LogosToken::PipeGreater` and `convert_logos_to_token()` maps it to `TokenType::PipeGreater`.
- Cross-chunk case: add boundary handling to merge `|` (tail) + `>` (head) into a single token when feature is enabled.
  - Implementation approach:
    - Deferral strategy (preferred, minimal allocations):
      - At the end of `tokenize_chunk()`, if not final chunk and feature enabled, detect a trailing `|` at the textual tail (or the last token is `BitOr` whose end aligns with chunk end).
      - Defer from the `|` index (similar to existing unmatched-string/comment deferrals) by:
        - Dropping any tokens whose span starts at/after the `|` position.
        - Returning `partial_string = Some(chunk[cut_idx..].to_string())` so the next call preprends it to the next chunk.
      - On the next chunk, Logos sees contiguous `|>` and emits `PipeGreater`.
    - Post-process merge (optional complement):
      - After tokenization of a chunk (when finalizing tokens from both current and previous partials), if two adjacent tokens are `BitOr` and `Greater` with no intervening text, replace them with a single `TokenType::PipeGreater` token (start location = `BitOr` location, lexeme = "|>").
      - This is useful even within a single chunk if one wants to avoid relying solely on Logos. Not strictly necessary if Logos variant is present.
  - Location: when deferring from `|`, the resulting `PipeGreater` start location will be the original `|` location. This is consistent with streaming.
  - Performance: the deferral mirrors existing mechanisms (strings/comments/exponents) and avoids extra scanning passes.

## Priorities and ordering notes
- `|>` must be recognized before `|` and `>` to avoid splitting.
- Suggested `priority = 60` (greater than most single-char operators and above `BitOr`/`Greater`). Exact value only matters relative to `BitOr` and `Greater`. Keep it consistent with existing operator priorities.

## Backward compatibility
- With `pipeline_op` feature disabled (default):
  - `LogosToken::PipeGreater` does not exist, so `|>` lexes as `BitOr` then `Greater` everywhere.
  - Existing tests remain unchanged and continue to pass. In particular: `test_pipeline_operator_not_tokenized_streaming`.

## Test plan
- Default-off tests (no change):
  - `|>` tokenizes as 2 tokens in streaming lexer (`BitOr`, `Greater`). Existing test already enforces this.
  - Add similar tests for chunked lexer (with a small chunk size) to assert two tokens.
- Feature-on tests (`#[cfg(feature = "pipeline_op")]`):
  - Streaming lexer: `|>` tokenizes as a single `TokenType::PipeGreater` with the correct lexeme and location.
  - Chunked lexer:
    - Single chunk: same as streaming.
    - Cross-chunk boundary: with chunk size = 1 or split at `|`/`>`, verify it produces a single `PipeGreater` token. Location must point to the `|` position.
  - Mixed cases: `a |> b`, `a |>b`, `a|> b` with whitespace around but not inside the operator (operator itself has no spaces). Ensure no false merges across whitespace.
- Negative tests:
  - `| =` (ensure only `BitOr` + `Equal`).
  - ` |> ` with leading space (still `PipeGreater` once feature is enabled as long as `|>` is contiguous).

## Documentation updates
- In `LANG_SPEC.md` (or docs site): document the pipeline operator under an “experimental/feature-gated” section.
  - Syntax: `|>`.
  - Feature flag: `pipeline_op` (disabled by default in v0.1).
  - Behavior: tokenization only; semantics to be defined separately.
  - Note: streaming + chunked lexers support boundary-safe recognition when enabled.

## Implementation checklist
- Cargo feature
  - Add to `compiler/medic_lexer/Cargo.toml`:
    - `[features]`
      `pipeline_op = []`
  - Ensure default features exclude `pipeline_op`.
- Token variants
  - `src/logos_token.rs`: add `#[cfg(feature = "pipeline_op")]` `PipeGreater` with `#[token("|>", priority = 60)]`, placed above `BitOr` and `Greater`.
  - `src/token.rs`: add `TokenType::PipeGreater`.
- Conversion
  - `src/convert.rs`: add (cfg-gated) mapping from `LogosToken::PipeGreater` to `TokenType::PipeGreater`.
  - `src/streaming_lexer.rs`: add (cfg-gated) mapping in the inline `match`.
- Chunked boundary handling (feature-on only)
  - In `tokenize_chunk()`:
    - If not final chunk and textual tail ends with `|` (or last token is `BitOr` and ends at chunk end), drop trailing token(s) from that index and set `partial_string` from that `|`.
    - Optionally, add a small post-process to merge adjacent `BitOr` + `Greater` into `PipeGreater` (guarded by `#[cfg(feature = "pipeline_op")]`).
- Tests
  - Keep `test_pipeline_operator_not_tokenized_streaming` as-is (default-off guard not required since it asserts the default behavior).
  - Add streaming tests under `#[cfg(feature = "pipeline_op")]` asserting single `PipeGreater`.
  - Add chunked tests (default-off: two tokens; feature-on: single token, including cross-chunk case).
- Docs
  - Update `LANG_SPEC.md` and site docs to mention feature flag and default-off status.

## Notes on alternatives
- Runtime toggle instead of Cargo feature: could be done via `ConversionConfig` and post-lex merge logic, but would require additional passes in streaming lexer (currently pure Logos). The Cargo feature approach is simpler and matches project conventions for feature-gated tokens.

## Risk assessment and mitigations
- Risk: introducing the Logos variant could accidentally change default behavior.
  - Mitigation: strict `#[cfg(feature = "pipeline_op")]` on the Logos variant and conversion arms; keep default-off tests.
- Risk: chunked lexer boundary merges could affect other `|` usages.
  - Mitigation: only defer/merge when the literal last character is `|` at the end of chunk; no whitespace allowed inside operator. Add targeted tests.
