"use strict";
// Event handlers
var dragging = false
var draggingData = undefined
var position = undefined
function startDrag(target) {
  return function(e) {
    let g = target
    if (e.touches) {
      e.x = e.touches[0].clientX
      e.y = e.touches[0].clientY
    }
    if (e.button === 0 || (e.touches && e.touches.length == 1)) {
      e.preventDefault()
      e.stopPropagation()
      position = [e.x, e.y]
      //console.log(e)
      if (dragging === false) {
        // TODO: sort out duplication properly (always duplicate and conditionally hide)
        if (e.ctrlKey) {
          g = g.duplicate()
        }
        dragging = g
        // svg2 z-index not yet implemented in firefox
        // If it was, this would be much easier
        draggingData = [g.parentElement, g.nextSibling, dragging.getClientXY()]

        g.classList.add("dragging")
        root.appendChild(g)
        g.setClientXY(...draggingData[2])
      }
      else console.log("spurious", g, e)
    }
    if (e.button === 2) {
      e.preventDefault()
      e.stopPropagation()
      g.delete()
      //Why do none of these prevent the contextmenu event?
      return false
    }
  }
}

function hideMenu(e) {
  e.preventDefault()
  return false
}

function drag(e) {
  //try{
  if (e.touches) {
    e.x = e.touches[0].clientX
    e.y = e.touches[0].clientY
  }
  if (dragging) {
    dragging.changeClientXY(e.x - position[0], e.y - position[1])
    position = [e.x, e.y]
  }
  //}catch(e){alert(e);throw e}
  //else console.log("spurious move",g)
}
function endDrag(e) {
  //try{
  if (e.touches) {
    e.x = e.changedTouches[0].clientX
    e.y = e.changedTouches[0].clientY
  }
  //console.log("main",e)
  if (dragging) {
    dragging.classList.remove("dragging")
    dragging.classList.add("hidden")
    var over = document.elementFromPoint(e.x, e.y)
    dragging.classList.remove("hidden")
    var moved = false
    draggingData[0].insertBefore(dragging, draggingData[1])
    if (over) {
      //TODO: check if the node was occupying a hole and if so,
      //check if it should snap back and if not, redraw the hole it came from
      var slot = over.parentElement
      moved = false // dragInto(dragging, slot)
    }
    if (!moved && draggingData[0] !== root) {
      dragging.setClientXY(...draggingData[2])
    }
    dragging = false
  }
  //TODO: hsTerm.innerText = printTerm(root.children[root.children.length - 1])
  //}catch(e){alert(e);throw e}
  //else console.log("spurious end",g)
}
//O pointerEvents, why are you so buggy?
root.addEventListener("mousemove", drag)
root.addEventListener("touchmove", drag)
root.addEventListener("mouseup", endDrag)
root.addEventListener("touchend", endDrag)
root.addEventListener("mouseleave", endDrag)
// I'm sorry for doing this - I only want to supress context menu when the right
// click is deleting something, but the mousedown handler won't let me do that
// and the context menu handler doesn't run for the element that's deleted.
root.addEventListener("contextmenu", hideMenu)

