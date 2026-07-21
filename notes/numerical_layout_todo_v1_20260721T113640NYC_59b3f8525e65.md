# Numerical layout maturation TODO

Author: Carter Schonwald
Date: 2026-07-21
Status: working design and implementation queue
Source branch: `origin/layout-fixups` at `b9281e2c5aa9a3034647cfb4f6a35c8442ae9437`

## Purpose

Mature the existing address-centric layout API now that sparse and tiled ML
training supplies a concrete consumer. Preserve the original generality:
logical geometry, layout-owned addresses and traversal, storage representation,
and residency/world are separate axes.

This note records current Carter decisions, verified implementation gaps, and
open choices. Later tuple-oriented preservation prose is archaeological context,
not authority for pruning the class surface.

## Current decisions

### Coordinate and layout order

- User-visible coordinates remain `x :* y :* z :* Nil`.
- Row layout: leftmost `x` is innermost and changes fastest; comparison is
  right-to-left lexicographic.
- Column layout: rightmost coordinate is innermost and changes fastest;
  comparison is left-to-right lexicographic.
- `compareIndex` defines the layout's manifest enumeration order and must agree
  with address traversal. It is not componentwise dominance or a universal
  tuple order.

### Universal `Layout` surface

Retain the existing structural operations and add the missing primitives:

```haskell
logicalShape
blockExtent
logicalForm
transposedLayout
compareIndex
addressRange
toAddress
toIndex

nextAddr
prevAddr
seekGEFrom
seekLEFrom
affineAddressShift

addressPopCount
addressAsInt
fromSomeAddress
```

`nextAddr`, `prevAddr`, both directional seeks, and `affineAddressShift` are
representation-supplied primops.

`seekGEFrom` and `seekLEFrom` seek from the supplied logical index. Each also
accepts a vital address hint for representation-specific acceleration. `From`
names the index boundary, not the hint. Exact signatures and the validity law
for directional hints still need to be fixed.

Inclusive GE/LE semantics are the leading candidate because exact lookup plus
`nextAddr`/`prevAddr` derives strict GT/LT. This remains an explicit design
choice until encoded in laws and approved by Carter.

`affineAddressShift` means displacement in manifest enumeration/popcount space.
Positive one agrees with `nextAddr`; negative one agrees with `prevAddr`. It may
cross CSR row boundaries and sparse gaps. Local affine storage regions are a
separate concept.

`logicalForm` means the format obtained by freshly allocating the same logical
object. It canonicalizes away view-specific or borrowed storage accidents while
preserving the object's intended format. It is not merely an address-arithmetic
helper or an arbitrary packed sibling. Operationally, that fresh canonical form
is also what enables packed address translation, population displacement,
copy/reformat planning, and related address-manipulation tricks. The fresh-allocation
meaning supplies the semantics; the address tricks are a load-bearing use.

### Rectilinearity

- CSR of any payload is rectilinear.
- Rectilinearity concerns named ordered axes and axis-aligned structural
  slicing; it does not imply density, affinity, contiguity, or a rectilinear
  payload.
- CSR is `RectilinearLayout Rowed`, but not generally `DenseLayout`.
- A CSR slice may preserve opaque payloads without recursively slicing them.

The rank-indexed slice GADT is the canonical slice operation object:

```haskell
data RSlice from to where
  RSNil   :: RSlice Z Z
  RSPick  :: Int -> RSlice from to -> RSlice (S from) to
  RSRange :: Int -> Int -> RSlice from to -> RSlice (S from) (S to)
  RSAll   :: RSlice from to -> RSlice (S from) (S to)
```

`applySlice` should be the load-bearing `RectilinearLayout` method. Major-axis
slice/project and corner-box syntax are derived constructors or convenience
functions. The GADT is preferable to untyped axis lists because source and
result ranks are checked by construction.

Rectilinear operations act on the current blocking shape when blocking exists.
For a leaf, `blockExtent == logicalShape`; for a nested layout, `logicalShape`
describes the full element-coordinate object while `blockExtent` describes the
outer block grid seen by structure-preserving rectilinear operations.
Consequently `rectlinearShape` should be derived from `blockExtent` rather than
be an unrelated shape primitive.

Slices are blocking-aware because `RSlice` is interpreted over
`rectlinearShape == blockExtent`. Applying an `RSlice` to `Nest` therefore
selects whole tiles in the outer block grid. For leaves, where
`blockExtent == logicalShape`, the same syntax selects elements. Cutting through
a tile is a distinct copy/reformat operation, not a structural slice of the
tiled layout. `SlicedForm` carries the structural result at the type level.

### CSR empty rows

- Ordinary CSR permits empty rows; repeated row pointers are canonical.
- Builders build valid CSR and do not reject or crash because rows are empty.
- A separate lint reports only the canonical run-compressed locations of empty
  rows. Counts, fractions, longest runs, and DCSR recommendations are derived
  policy, not redundant lint constructors.
- Do not add alternate skip metadata to canonical CSR without measurements.
- DCSR remains a separate possible representation for workloads with abundant
  empty outer fibers, not a semantic repair for invalid CSR.

### Blocking and nesting

- `blockExtent` is a base `Layout` primitive because Morton/Hilbert and other
  non-rectilinear layouts can still have block structure.
- Leaf Dense and CSR: `blockExtent == logicalShape`.
- `Nest outer tile inner` invariant: `tile == blockExtent inner`.
- `logicalShape (Nest o _ i) = logicalShape o * logicalShape i`, pointwise.
- `blockExtent (Nest o _ _) = logicalShape o`.
- Coordinate decomposition uses `logicalShape inner`, never the stored `tile`.
- The standalone `Tile` combinator is removed; the right side of `Nest` is a
  tile by construction.
- Nest addresses are the Cartesian product of outer and inner addresses.

## Verified implementation gaps and bugs

### Contract conflicts

- The current class comment and CSR implement inclusive seek, while Dense and
  DirectSparse implement strict successor behavior inherited from
  `basicNextIndex`.
- `prevAddr` is absent even though Pure/Mutable comments anticipate it and CSR
  contains a private predecessor implementation.
- CSR's current hint path can skip the correct result because it does not verify
  the hinted coordinate is directionally usable.
- `RectilinearLayout` has no concrete instances. `applySlice` exists only in
  prose/TODO form; `SlicedForm` and companion type-family instances are absent.
- `Nest` and `blockExtent` are not implemented in Numerical Haskell.
- Normal CSR construction does not use the experimental negative skip-row
  encoding.

### Concrete correctness bugs

- Dense Row and Column `compareIndex` implementations appear reversed relative
  to their address enumeration and the XYZ convention.
- Dense `seekDenseGeneric` uses a pointwise bound that rejects indices with a
  valid lexicographic successor.
- DirectSparse `toAddress` excludes logical index zero.
- DirectSparse `nextAddr` can construct `Address length` after the last entry.
- DirectSparse `seek` indexes the last LUT element without handling an empty LUT.
- CSR `toAddress` checks upper bounds but not negative coordinates before vector
  indexing.
- `basicHybridSearchUp` uses an absolute probe constant rather than `lo + probe`,
  so nonzero-lower-bound searches can leave their intended interval.

### Verification gaps

- `tests/NumericalUnit/Layout.hs` is empty.
- `tests/PropLayout.hs` leaves layout properties undefined.
- The test runner currently exercises Shape but no Layout laws.
- Current Cabal bounds reject the installed GHC/base combination because
  `ghc-prim < 0.13` conflicts with installed `ghc-prim-0.13.1`.

## Property-first implementation order

1. Add executable reference enumeration for small layouts.
2. State and test XYZ/layout-order coherence:
   `compareIndex`, `toIndex`, `nextAddr`, and `prevAddr` must induce the same
   manifest order.
3. Test lookup/decode retraction on every manifest index/address.
4. Fix Dense Row/Column ordering and boundary behavior.
5. Add `prevAddr` to `Layout`, PureArray, Array, and DenseArray surfaces.
6. Choose and encode inclusive GE/LE seek laws plus directional hint validity.
7. Implement Dense, DirectSparse, CSR, and CSC seeks against one reference law.
8. Test hint and no-hint results against the same reference result.
9. Test `affineAddressShift` against repeated next/prev for small signed steps.
10. Remove or isolate experimental CSR skip-row encoding; add empty-row-run lint.
11. Make rank-indexed `RSlice` the canonical slice object and add its laws.
12. Implement `SlicedForm`/`applySlice` as the `RectilinearLayout` primitive;
    derive major-axis and corner-box helpers from the GADT.
13. Implement concrete blocking-aware `RectilinearLayout` instances for Dense,
    CSR, CSC, and Nest. Interpret the GADT over `blockExtent`, so Nest slicing
    selects whole outer-grid tiles without recursively cutting payloads.
14. Add `blockExtent`, then implement and law-test `Nest`.
15. Restore compilation on current GHC and run the complete property suite.
16. Benchmark only after semantic equivalence is externally checked.

## ML forcing cases

Use these as applications and regression fixtures:

```text
BLIS inner loop = rowMaj * panelTile rowMaj * microTile rowMaj
Flash attention = rowMaj * tiled rowMaj
Sparse attention = CSR * tiled rowMaj
KV cache = EntryCache(rowMaj, tiled rowMaj)
```

Prime logical extents must work through complete padded physical tiles without
changing logical XYZ coordinates or introducing scalar semantic trailers.

## Open decisions

- Exact directional-hint precondition and behavior for stale/unhelpful hints.
- Out-of-bounds GE/LE behavior at the low and high sides of the logical shape.
- Result-form types for slices that preserve an outer CSR while an opaque inner
  payload cannot itself be recursively sliced.
- Whether DCSR is needed by observed ML layouts versus ordinary dense-row-pointer
  CSR plus linting.
- Required asymptotic contract for `affineAddressShift` beyond extensional
  equivalence to repeated next/prev.
- Runtime-dependent layout ordering, since the current proxy-only
  `compareIndex` cannot inspect value-level permutations.
