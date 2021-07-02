
function deleteNode(g){
	if(!(g.isHole || g.isLHS)){
		g.delete()
		return true
	}
}

function fillOrUnfill(g){
  if(g.isLHS){
    return
  }
  if(g.isHole){
    if (g.children.length==0) g=g.parentElement
    else return;
  }
  if(g===g.parentElement.filled){
    makeFloating(g,g.parentElement)
    return;
  }
  if(g.isHole){
    // Consider using this to expand holes
    return;
  }
  if (!g.parentElement.filled
    && checkMatch(g.parentElement.type,g.type)){
    fillHole(g,g.parentElement)
  }
}

function dragInto(argument,hole,pointerPos,pointerDelta,dropEffect){
	// Don't do anything if the target is not something we expect
	if (!root.contains(hole)){
		console.log("Target is not part of a term")
		return;
	}
	if (dropEffect=="move" && argument.contains(hole)){
		console.log("Cannot move into self")
		return;
	}
	if (isDefn(argument)){
	  if (hole===root && dropEffect=="move"){
		  argument.changexy(...pointerDelta)
		}
		else console.log("can't copy definitions");
		return;
	}

	// Find out how to move the argument into the hole (if appropriate)
	let cmd = getMoveCommand(argument,hole,pointerPos)
	if (cmd===false) return; // the attempted move is invalid

	// save some data that may be helpful for repositioning
	let savedpos = argument.getPageXY()

	if (dropEffect=="copy"){
		argument=argument.duplicate()
	}
	let fn = cmd[0]
	cmd[0] = argument
	let moved = fn(...cmd)// moved within the syntax tree
	console.log(moved)
	if (argument.parentElement===root){ // this implies moved in the current code
		argument.setxy(savedpos[0]+pointerDelta[0], savedpos[1]+pointerDelta[1])
	}
	if (dropEffect=="copy"){
		argument.classList.remove("hidden")
	}
	else if(!moved){
		argument.changexy(...pointerDelta)
	}

}

// What should be done when argument is dragged into hole
// returns false if the attempted move is invalid
function getMoveCommand(argument,hole,pointerPosition){
  if (!hole.isHole){
    if(isArg(argument)){
      if (argument.parentElement.parentElement===hole){
        // If you drag a filled argument just outside its hole, make it float
				// (and make it the first in the list of floating terms)
        return [makeFloating,argument.parentElement,"start"]
      }
    }
    if (hole===argument.scope.parentElement){
      //Let something be made floating by dragging it just outside its context
      return [makeFloating,argument.scope]
    }
		console.log("Not a hole") // Consider trying the parent hole
    return false
  }
  else if(!argument.scope.contains(hole)){
    console.log("Out of scope") // consider snapping argument back to where it came from
    // alternatively, consider putting the new context into this hole (if possible)
    return false
  }
	else if (hole===root){//Put stuff into the root
		return [makeFloating,hole,"end"]
	}
  else if(hole.filled===argument){// Take something out of a hole by moving it a little
    return [makeFloating,hole,"start"]
  }
	else if(hole.filled){//don't put things into holes that are already filled
		console.log("Already filled")
		return false
	}
	else {
    //If the types match and there's no other reason not to, go ahead and fill the hole
    // (currently does not do so)
    return [tryToFillHole,hole,pointerPosition]
	}
}

// See if a term can fit a hole perfectly, taking into account
// stuff that is no longer in scope, but dealing with complicated cases correctly
function tryToFillHole(arg, hole, pointerPos){
  //detatch(arg)//hopefully
  if(arg.baseType!=hole.baseType)
    return makeFloating(arg,hole,pointerPos);
  return makeFloating(arg,hole,pointerPos);
  /*hole.appendChild(arg)
  for(let ch of Array(...arg.children).reverse()){
    if
  }*/
}

/**
Tricky case:
given
> fn :: (((o->o->b)->s)->b)->r
> inner :: ((o->o->b)->s)->b
and the term
> fn (\ fn0 -> _ (fn0 (\ fn1v0 fn1v1 -> inner (\ inner0 -> _ (inner0 fn1v1 fn1v0)))))
if 'inner' is dragged into the outer hole, what should be the result?
> fn (\ fn0 -> inner (\ inner0 -> fn0 (\ fn1v0 fn1v1 -> inner0 fn1v1 fn1v0)))
feels right, but current implementations would detatch 'fn1v1' and 'fn1v0' from
their holes before trying t
and result in
> fn (\ fn0 -> inner (\ inner0 -> fn0 (\ fn1v0 fn1v1 -> inner0 fn1v1 fn1v0)))
(η-equivalent to 'fn inner'
*/
