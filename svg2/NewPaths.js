"use strict";

/*

PADDINGH
├──┤
┌─────────────┐ ┬
│    border   │ │ PADDINGV
│  ┌───────┐  │ ┴
│  │       │  │
│  │content│  │
│  │       │  │
│  └───────┘  │
│             │
└─────────────┘
content may have an effective size of 0.
border graphics should be contained within the border region.


relevant constants from Layout.js:┌
const PADDINGH = 8
const PADDINGV = 8
font size is 16


Assumptions:
The border graphics will be an SVG path that encloses the content

Options for conveying meaning:
Color (currently used to indicate what belongs to what, but imperfectly)
animation (polymorphism?)
stylistic differences (eg curves vs straight edges and sharp corners)
Loops in the border indicating polymorphic types that may be arrow types (and so may have holes)
  these should be in the bottom right corner
Base types should have rotational symmetry (order 4 when the box is square)

Descisions to make:
what should be a 'default' border?
- minimal rectangle around content (non-holes have jigsaw protrusions)
    - problematic if the content has size 0
    - the right kind of protrusions feel like 'part of the shape'
- maximal border inside the region (holes have jigsaw protrusions)
    - Things appear bigger
    - could make types feel like 'missing bits' which is misleading
- in the middle
    - allows both protrusions and intrusions
    - protrusions and intrusions would be smaller, making it harder to see details
      - interesting interlocking patterns that do both would allow the same level of detail
Corner embellishments or edge embellishments?
- Edge embellishments are easier to combine sensibly
  - put them adjacent for pairs or perhaps Either
  - for type constructors * -> * (lists, etc), vertical edges can be used for
    the constructor and horizontal edges for the argument
- corners could be considered part of vertical edges
- how to treat corners when
How to scale?
- It's important that boxes of different sizes can be recognised as having the same (or different) types
- Stretching along the whole length
  - the thickness of the border is constrained by a constant, so it can't be scaled
  - distortions arise - something may go from looking like a needle, to a semicircle, to a gentle hump
- Adding straight segments
- Adding straight segments, but doing a bit of scaling for very small cases
  - How should corners work if horizontal and vertical are scaled by different amounts?
    - scale them to match the smaller
- Change stroke thickness?


Options for graphics:
Finite list of hand-constructed shapes
  - This might not be bad for builtin types when the shapes have some meaning
    - eg shape that looks like square brackets for lists
  - Has the advantage of producing things
Procedural generation
  - can lead to similar looking things
    such similarity would have no semantic relevance
  - can lead to janky looking things e.g. self-intersections
  - assuming a finite combinatorial design (e.g. 5 parts each of which can be 10 different things)
    there's a tradeoff between the following:
    - the size of the state space (too small and shapes get reused)
    - the similarity between the closest points in the state space
      (too similar and you can't tell different things apart easily)
    - Nothing looks broken; it all scales reasonably
*/