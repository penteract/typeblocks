"use strict";
function deleteNode(g) {
  if (!(g.isHole || g.isLHS)) {
    g.delete()
    return true
  }
}

function fillOrUnfill(g) {
  if (g.isLHS) {
    return
  }
  if (g.isHole) {
    if (g.hasNoChildren()) g = g.parentElement
    else return;
  }
  if (g === g.parentElement.filled) {
    makeFloating(g, g.parentElement)
    return;
  }
  if (g.isHole) {
    // Consider using this to expand holes
    return;
  }
  if (!g.parentElement.filled
    && checkMatch(g.parentElement.type, g.type)) {
    fillHole(g, g.parentElement)
  }
}

// Returns true if the action fails
function dragInto(argument, hole, pointerPos, dropEffect) {
  //console.log(argument, hole, pointerPos, dropEffect)
  // TODO: Can the pointerPos argument be avoided?
  //TODO: consider not duplicating terms that get snapped back

  // Don't do anything if the target is not something we expect
  if (!root.contains(hole)) {
    console.log("Target is not part of a term")
    return true;
  }
  if (dropEffect == "move" && argument.contains(hole)) {
    console.log("Cannot move into self")
    return false;
  }
  if (isDefn(argument)) {
    if (hole === root && dropEffect == "move") {
      argument.changexy(...pointerDelta)
      root.appendChild(argument)
    }
    else console.log("can't copy definitions");
    return false;
  }
  if (hole.isLHS){
    console.log("can't (yet) put anything inside LHS")
    //TODO: Allow constructors in LHS
    return false;
  }

  // Find out how to move the argument into the hole (if appropriate)
  let cmd = getMoveCommand(argument, hole, pointerPos)
  if (cmd === false) return false; // the attempted move is invalid

  // save some data that may be helpful for repositioning
  //let savedpos = argument.getPageXY()

  /*if (dropEffect == "copy") {
    argument = argument.duplicate()
  }*/
  let fn = cmd[0]
  cmd[0] = argument
  let moved = fn(...cmd)// moved within the syntax tree
  return moved;
  /*if (argument.parentElement === root) { // this implies moved in the current code
    argument.setxy(savedpos[0] + pointerDelta[0], savedpos[1] + pointerDelta[1])
  }
  if (dropEffect == "copy") {
    argument.classList.remove("hidden")
  }
  else if (!moved) {
    argument.changexy(...pointerDelta)
  }*/

}

// What should be done when argument is dragged into hole
// returns false if the attempted move is invalid
function getMoveCommand(argument, hole, pointerPosition) {
  if (!hole.isHole) {
    if (isArg(argument)) {
      if (argument.parentElement.parentElement === hole) {
        // If you drag a filled argument just outside its hole, make it float
        // (and make it the first in the list of floating terms)
        return [makeFloating, argument.parentElement, "start"]
      }
    }
    if (hole === argument.scope.parentElement) {
      //Let something be made floating by dragging it just outside its context
      return [makeFloating, argument.scope]
    }
    console.log("Not a hole") // Consider trying the parent hole
    return false
  }
  else if (!argument.scope.contains(hole)) {
    console.log("Out of scope") // consider snapping argument back to where it came from
    // alternatively, consider putting the new context into this hole (if possible)
    return false
  }
  else if (hole === root) {//Put stuff into the root
    return [makeFloating, hole, "end"]
  }
  else if (hole.filled === argument) {// Take something out of a hole by moving it a little
    return [makeFloating, hole, "start"]
  }
  else if (hole.filled) {//don't put things into holes that are already filled
    console.log("Already filled")
    return false
  }
  else {
    //If the types match and there's no other reason not to, go ahead and fill the hole
    // (currently does not do so)
    return [tryToFillHole, hole, pointerPosition]
  }
}

// See if a term can fit a hole perfectly, taking into account
// stuff that is no longer in scope, but dealing with complicated cases correctly
function tryToFillHole(arg, hole, pointerPos) {
  //detatch(arg)//hopefully
  if (arg.baseType != hole.baseType)
    return makeFloating(arg, hole, pointerPos);
  //return makeFloating(arg,hole,pointerPos);
  return fillHole(arg, hole)
  /*hole.appendChild(arg)
  for(let ch of Array(...arg.children).reverse()){
    if
  }*/
}

// Do the work of taking something out of where it came from and putting it (floating) somewhere else
// returns true unless the
function makeFloating(g, target, location) {
  if (!target.isHole) throw "Things should only float in holes"
  let before = null

  function findfirst(test) {
    for (let ch of target.boxes()) if (ch !== g && test(ch)) {
      before = ch
      break;
    }
  }
  if (location === "start") {
    findfirst((c) => true)
  }
  else if (location === "end") {
    before = null
  }
  else if (location !== undefined) {
    let [x, y] = target.toLocalCoords(...location)
    before = getInsertLocation(target, x, y)
    if (before === g) before = before.nextSibling
    if (g.parentElement === target && g.nextSibling === before) {
      return false // Don't rearrange if it's staying roughly where it was
    }
  }
  else if (target === g.scope) {
    findfirst((ch) => ch.scope === target && ch.scopeIndex > g.scopeIndex)
  }
  detach(g)
  target.insertBefore(g, before)
  for (let ch of g.boxes()) {
    checkScope(ch)
  }
  //g.setxy(0, 0)
  return true // It moved
}


// future Strategy: If there is an unambiguous ordered bijection between (((a subset of the argument's holes) and (the hole's arguments)) ^or^ ((the argument's holes) and (a subset of the hole's arguments)) then go for it; otherwise, if the final types line up, stick with that.
//   a bijection is ambiguous if there is another one that does not involve a strict subset of chosen argument set

// Do the work of moving filling a hole with an argument
function fillHole(argument, hole) {
  //If something moves, need to check hereditarily that everything is still in scope
  let oldPar = argument.parentElement
  detach(argument)
  hole.appendChild(argument)
  if (!oldPar.contains(hole)) {//If it does, we don't need to check recursively
    checkScope(argument)
  }//doing this before matching types hopefully saves some headaches
  //We need to detach it again because "isNearPerfectMatch" uses the DOM tree
  hole.removeChild(argument)
  let match = isNearPerfectMatch(argument, hole)
  for (let i = 0; i < match.length; i++) {
    let s = match[i][0]
    let h = match[i][1]
    if (i > 0) detach(s) // match[0]  is [argument,hole]
    h.classList.add("filled")
    h.filled = s
    s.classList.add("filling")
    h.appendChild(s) // There might be an argument for prepending here
  }
  return true
}



function checkScope(g) {
  if (!g.isHole) {
    if (!g.scope.contains(g.parentElement)) {//out of scope
      //TODO: think more carefully about where it should go - should it just go
      // up to the nearest legal hole (nearest before the move that caused checkscope),
      // rather than all the way up to its scope?
      //
      snapback(g)
      return
    }
  }
  else {
    if (g.filled) checkScope(g.filled)
  }
  for (let ch of [...g.boxes()]) if (ch) {// don't let g.children mutate in the middle of iteration
    checkScope(ch)
  }
}

// When something would become out of context, snap it back to its scope
function snapback(g) {
  makeFloating(g, g.scope)
}


function detach(g) {
  //TODO: we don't always need to add both (a duplicated node may not directly affect its old parent)
  // Note: the redrawing code crashes when a deleted node gets added to the list before being redrawn
  // At the moment, that shouldn't happen, but beware of that for the future
  if (!g.toBeDeleted) dirty.push(g)
  if (!g.parentElement.toBeDeleted) dirty.push(g.parentElement)
  if (isArg(g)) {
    unfill(g.parentElement)
  } else {
    g.classList.remove("filling")//for recently duplicated terms
    unfloat(g)
  }
}

function unfloat(g) {
  let holder = g.parentElement
  holder.removeChild(g)
}
function unfill(g) {
  g.classList.remove("filled")
  let arg = g.filled
  arg.classList.remove("filling")
  g.removeChild(arg)
  g.filled = false
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
