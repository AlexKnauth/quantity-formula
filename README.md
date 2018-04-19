# quantity-formula
Finding chains of multiple formulas to find quantities

Somewhat inspired by [Aristarchus](https://github.com/walkeri/Aristarchus).

### Data Definitions

A `FundamentalDepIndex` is any value that can be
equal-compared and hashed. It is something that
quantities can "depend on", in a way that if it is
different, the quantities are treated as if they were
"different quantities". For example, the masses of
two separate objects A and B should be treated as 
different quantities, so there should be a
`FundamenalDepIndex` for object A and a different one
for objcet B.

A `Quantity` is the result of a function defined by
`declare-quantity`.

A `Formula` is the result of a function defined by
`define-formula`, and can be used as a `State -> State`
function.

A `FormulaPath` is a `[Listof Formula]`
where the formulas are applied in order, from left to
right, to get from one state to another.

A `State` is a `[Hashof Quantity Any]`.

### Forms for defining Quantities and Formulas

```
declare-quantity name (fundamental-dep-name ...)
```
Defines `name` as a function that takes the
`fundamental-dep-name`s as arguments, and returns a
`Quantity`.

```
define-formula name (fundamental-dep-name ...)
  #:input ([in-name in-quantity] ...)
  #:output ([out-name out-quantity] ...)
  out-name = expr
  ...
```
Defines `name` as a function that takes the
`fundamental-dep-name`s as arguments, and returns
a `Formula`.

### Finding paths through multiple formulas to find quantities

A `FormulaGraph` is a graph where:
 - A `Vertex` is a `[Setof Quantity]` representing which
   quantities are "known" at that point.
 - An `Edge` can correspond to a `Formula`.

```
(formulas->graph formulas) -> FormulaGraph
  formulas : [Listof Formula]
```
Creates a formula graph from the given formulas.

```
(formula-graph-find-path G start end) -> FormulaPath
  G     : FormulaGraph
  start : [Setof Quantity]
  end   : [Setof Quantity]
```
Finds a path from `start` -> `end`, and produces the formulas
needed to get to knowing `end` from knowing `start`.

```
(formula-path-state state path) -> State
  state : State
  path  : FormulaPath
```
Applies the formulas in the given path to the given knowledge
state, producing a new state that contains the new quantities
introduced by the formulas.
