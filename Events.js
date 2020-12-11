"use strict";
// Event handlers
var dragging=false
var selected=false
var position=undefined
function startDrag(target){
  return function(e){
    let g=target
    if(e.touches){
      e.x=e.touches[0].clientX
      e.y=e.touches[0].clientY
    }
    if(e.button===0 || (e.touches && e.touches.length==1)){
      e.preventDefault()
      e.stopPropagation()
      //console.log(e)
      if (dragging===false){
        if(e.ctrlKey){
          g=g.duplicate()
        }
        dragging=g
        selected=g
        g.classList.add("dragging")
        // svg2 z-index not yet implemented in firefox
        // If it was, I'd be able to use DOM structure rather than keeping a list of floating terms
        var climb=g
        while(climb!==svg){
          climb.parentElement.appendChild(climb)
          climb = climb.parentElement
        }

        position=[e.x,e.y]
      }
      else console.log("spurious",g,e)
    }
  }
}
function drag(e){
  //try{
  if(e.touches){
    e.x=e.touches[0].clientX
    e.y=e.touches[0].clientY
  }
  if (dragging){
    let m=dragging.parentElement.getCTM()
    dragging.changexy((e.x-position[0])/m.a, (e.y-position[1])/m.d)
    position=[e.x,e.y]
  }
  //}catch(e){alert(e);throw e}
  //else console.log("spurious move",g)
}
function endDrag(e){
  //try{
  if(e.touches){
    e.x=e.changedTouches[0].clientX
    e.y=e.changedTouches[0].clientY
  }
  //console.log("main",e)
  if (dragging) {
    dragging.classList.remove("dragging")
    dragging.classList.add("hidden")
    var over = document.elementFromPoint(e.x,e.y)
    dragging.classList.remove("hidden")
    var moved=false
    if(over){
      //TODO: check if the node was occupying a hole and if so,
        //check if it should snap back and if not, redraw the hole it came from
      var slot = over.parentElement
      moved = dragInto(dragging,slot)
    }
    if(!moved && isArg(dragging)){
      dragging.setxy(0,0)
    }
    dragging=false
  }
  hsTerm.innerText = printTerm(svg.children[svg.children.length-1])
//}catch(e){alert(e);throw e}
  //else console.log("spurious end",g)
}

//O pointerEvents, why are you so buggy?
svg.addEventListener("mousemove",drag)
svg.addEventListener("touchmove",drag)
svg.addEventListener("mouseup",endDrag)
svg.addEventListener("touchend",endDrag)
svg.addEventListener("mouseleave",endDrag)
