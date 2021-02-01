"use strict";
// Event handlers
var dragging=false
var position=undefined

/* events fired on the draggable target */
document.addEventListener("drag", function( event ) {

}, false);

document.addEventListener("dragstart", function( event ) {
  dragging = event.target;
  // hide it after the bitmap copy has been made
  setTimeout(function(){event.target.classList.add("invisible");},10)
  position=[event.x,event.y]
}, false);

document.addEventListener("dragend", function( event ) {
    // reset the transparency
    event.target.style.opacity = "";
    event.target.classList.remove("invisible")
    //dragging=undefined
}, false);

/* events fired on the drop targets */
document.addEventListener("dragover", function( event ) {
    // prevent default to allow drop
    event.preventDefault();
}, false);

document.addEventListener("dragenter", function( event ) {
    if ( event.target.isHole ) {
    }
    event.preventDefault();
}, false);

document.addEventListener("dragleave", function( event ) {
    if ( event.target.isHole ) {
    }

}, false);

document.addEventListener("drop", function( event ) {
    // prevent default action (open as link for some elements)
    event.preventDefault();
    // Don't do anything if the target is not something we expect
    // perhaps this check should be in dragInto?
    if ( root.contains(event.target) && !dragging.contains(event.target) ) {
        let pos
        if (event.target===root) pos = dragging.getClientXY()
        if (event.target===dragging.parentElement){
          dragging.changexy(event.x-position[0],event.y-position[1])
        }
        // Move the thing being dragged into the target (if appropriate)
        let moved = dragInto(dragging,event.target)
        console.log(moved)
        if (event.target===root && event.target===dragging.parentElement){
          dragging.setClientXY(...pos)
          dragging.changexy(event.x-position[0],event.y-position[1])
        }
        else if(moved){
          dragging.setxy(0,0)
        }
    }
    hsTerm.innerText = printTerm(root.children[root.children.length-1])

}, false);
