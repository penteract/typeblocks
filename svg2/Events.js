"use strict";
// Event handlers
var dragging = false
var original = undefined // the box which was duplicated to make 'dragging'
var position = undefined
var copying = undefined
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
        dragging = g.duplicate()
        original = g
        copying = e.ctrlKey
        if (!copying) {
          g.classList.add("invisible")
        }
        root.parentElement.style.cursor = copying ? "copy" : "grabbing"
        dragging.classList.add("dragging")
        root.appendChild(dragging)
        // TODO: find a cleaner way to allow top-level terms to move freely
        dragging.setClientXY(...g.getClientXY())
      }
      else console.log("spurious", g, e)
    }
    if (e.button === 2) {
      if (!dragging) {
        e.preventDefault()
        e.stopPropagation()
        g.delete()
      }
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
function checkCtrl(e) {
  if (dragging) {
    if (copying != e.ctrlKey) {
      copying = e.ctrlKey
      if (copying) {
        original.classList.remove("invisible")
      } else {
        original.classList.add("invisible")
      }
      root.parentElement.style.cursor = copying ? "copy" : "grabbing"
    }
  }
}
function endDrag(e) {
  console.log(e.button, e.buttons, e)

  //try{
  if (e.touches) {
    e.x = e.changedTouches[0].clientX
    e.y = e.changedTouches[0].clientY
  }
  //if it's not a touch event, only end the drag if the left mouse button was the one released
  else if (e.button !== 0) return;
  //console.log("main",e)
  if (dragging) {
    original.classList.remove("invisible")
    root.parentElement.style.cursor = ""
    let savedPos = undefined
    if (e.ctrlKey) {
      // put it in the right place (ensures scopes are valid)
      original.parentElement.insertBefore(dragging, original)
      dragging.classList.remove("dragging")
    }
    else {
      // we don't need a copy, move the original
      savedPos = [dragging.xPos, dragging.yPos]
      original.setPos()
      dragging.remove()
      dragging = original
    }
    dragging.classList.add("hidden")
    let over = document.elementFromPoint(e.x, e.y)
    dragging.classList.remove("hidden")
    if (over) {
      let slot = over.parentElement
      let success = dragInto(dragging, slot, [e.x, e.y], e.ctrlKey ? "copy" : "move")
      if (!success && e.ctrlKey) {
        dragging.remove()
      }
      else if (savedPos && dragging.parentElement === root && slot === root) {
        dragging.setPos(...savedPos)
      }
      redraw(dirty)
      dirty = []
    } else {
      console.log("Not over anything")
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
//root.addEventListener("mouseleave", endDrag)
// I'm sorry for doing this - I only want to supress context menu when the right
// click is deleting something, but the mousedown handler won't let me do that
// and the context menu handler doesn't run for the element that's deleted.
root.addEventListener("contextmenu", hideMenu)

document.addEventListener("keydown", checkCtrl)
document.addEventListener("keyup", checkCtrl)

