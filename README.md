#TypeBlocks
https://penteract.github.io/typeblocks/typeblocks.html

Types are shapes.
A function has the shape of its return type and holes for its arguments.
The shapes of those holes are the types of the arguments.

If the argument's type is itself a function type, then the hole needs an
antihole to match the hole of the expected argument. This antihole can be
thought of as an argument to a lambda.

[TODO: Add pictures]


#UI

Ctrl-drag to copy terms ; right click to delete

Dragging something into an appropriately shaped hole will make it fill the hole,
dragging it a little will make it un-snap and appear as a floating term within
the hole.
