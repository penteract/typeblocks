#Architecture
##Why I'm using the DOM tree to track information
If program source code is text, then best practice for program analysis
(including typechecking, interpreting, compilation) is to turn the source into
an AST as soon as possible, then do all operations on that AST. This separation
of concerns is a good thing and avoids things like changes to comments affecting
program behavior. An IDE which used the same data structure to track the source
code for display and compilation would and should be frowned upon (although it
might be acceptable for syntax highlighting, particularly if that improved
efficiency)

However, the tree of HTML elements used in TypeBlocks exactly matches the AST of
the program being written, so that can be used directly for typechecking
operations. Also, part of the goal is to make invalid programs unconstructable,
and display as much information as possible about things like the syntax tree
and types. Various builtin features of the DOM (such as `a.contains(b)`) are
useful and probably efficient.

Doing it this way does make it hard to separate the details of display from the
type system, but the alternative would be to have a pair of similar data
structures which would introduce numerous opportunities for them to become out
of sync.

##Divs or SVGs
###Advantages of Divs
Browsers have done most of the fancy display stuff I care about, like text
wrapping and automatically resizing  parent containers when the content of an
inner one changes. The system for wrapping nested containers is better than what
I could implement in a reasonable amount of time. The methods used by browsers
are also faster than what I can do in Javascript, both because they get to use
lower level languages and they can afford to put more effort in.

Divs also support the Drag and Drop API and CSS z-index.

###Advantages of SVGs
Making the background an SVG with bits to modify gives pretty complete control
over what everything looks like, and is more consistent across browsers/devices.

###History and Current Status
Initially I started with SVGs, but then switched to divs because I didn't want
to implement a layout engine. I'm now trying to return to SVGs, mostly for
animation, but also for a bit more control when drawing borders (I had been
using border-image-repeat, which is inconsistent across browsers and is not
perfectly implemented anywhere).

###SVGs and Divs?
It would be possible to use divs with attached SVGs which redraw themselves on
reflow after the associated divs have worked out what size they should be.
It's not clear how easy it is to listen for that event and only change the
necessary parts.

## The data associated with a node
The key data structure is the syntax tree which coincides with the DOM tree. We
will call such elements boxes (these are `div` elements in some versions, `g`
elements when the whole thing is an SVG)

properties of non definition boxes:

 - `scope`/`owner` : a box cannot leave its scope - it must always be a
                     descendant of its scope.
 - `isHole` : indicates that the box is a hole. The child boxes of holes are
              not holes and the child boxes of non holes are all holes.
 - `children`/`floating` : The DOM children track the syntax tree. For
                           non-holes that are functions, these are the holes
                           where the arguments go. For holes, these are the
                           terms drawn in the hole
                           In SVGs, text nodes count as children, so the
                           attribute `floating` tracks the list of children that
                           are boxes.
 - `parent` : The inverse relation to children - each box has a single parent
 - `filled` : This property only exists for holes. If defined, it is one of the
              box's children and indicates that that child should be considered
              the argument to the hole's parent (in the position corresponding
              to the hole). If it is defined, all other children are hidden
              until it becomes undefined.



