
function deleteNode(g){
	if(!g.isHole){
		g.delete()
		return true
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
    if (hole===argument.owner.parentElement){
      //Let something be made floating by dragging it just outside its context
      return [makeFloating,argument.owner]
    }
		console.log("Not a hole") // Consider trying the parent hole
    return false
  }
  else if(!argument.owner.contains(hole)){
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
	else if(checkMatch(argument.type,hole.type)){
    //If the types match and there's no other reason not to, go ahead and fill the hole
    return [fillHole,hole]
	}
  else{ //If it can't be made an argument, make it floating
		return [makeFloating,hole,pointerPosition]
  }
}
