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
  position=[event.pageX,event.pageY]
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
    let target = event.target
    if (target===document.body.parentElement){
      console.log("root")
      target=root  // This seems easier than figuring out if there exists CSS
      //  make the root both scrollable when needed and fill the screen
    }
    dragInto(dragging,target,
      [event.x,event.y],
      [event.pageX-position[0],event.pageY-position[1]],
      event.dataTransfer.dropEffect)

    hsTerm.innerText = printTerm(root.children[root.children.length-1])

}, false);
