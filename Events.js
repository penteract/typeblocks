"use strict";
// Event handlers
var dragging=false
var position=undefined
var effect=undefined

/* events fired on the draggable target */
document.addEventListener("drag", function( event ) {

}, false);

document.addEventListener("dragstart", function( event ) {
  dragging = event.target;
  if(dragging.parentElement===root)
    event.dataTransfer.setData("text/plain", printReduced(dragging))
  effect=undefined
  // hide it after the bitmap copy has been made
  //setTimeout(function(){event.target.classList.add("invisible");},10)
  position=[event.pageX,event.pageY]
}, false);

document.addEventListener("dragend", function( event ) {
    // reset the transparency
    //event.target.style.opacity = "";
    event.target.classList.remove("invisible")
    //dragging=undefined
}, false);

/* events fired on the drop targets */
document.addEventListener("dragover", function( event ) {
    if(event.dataTransfer.dropEffect!=effect){
      if(effect=="move"){
      dragging.classList.remove("invisible")
      }
      effect=event.dataTransfer.dropEffect
      if(effect=="move"){
      dragging.classList.add("invisible")
      }
    }
    // prevent default to allow drop
    event.preventDefault();
}, false);
/*document.addEventListener("drag",function(event){
    //console.log("drag",event.dataTransfer.dropEffect)

}, false)*/

document.addEventListener("dragenter", function( event ) {
    //console.log("enter",event.dataTransfer.dropEffect)
    /*if(event.dataTransfer.dropEffect!=effect){
      if(effect=="move"){
      dragging.classList.remove("invisible")
      }
      effect=event.dataTransfer.dropEffect
      if(effect=="move"){
      dragging.classList.add("invisible")
      }
    }*/

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

    hsTerm.innerText = printReduced(root.children[root.children.length-1])

}, false);

/*document.addEventListener("mousedown",function(event){
  console.log(event)

  if(event.button===2){
    deleteNode(event.target)
    event.preventDefault()
    event.stopPropagation()
    //Why do none of these prevent the contextmenu event?
    return false
  }
})*/


// I'm sorry for doing this - I only want to supress context menu when the right
// click is deleting something, but the mousedown handler won't let me do that
// and the context menu handler doesn't run for the element that's deleted.
document.addEventListener("contextmenu",function (e){
  console.log(e)
  if(event.target.classList.contains("box")){
    if(deleteNode(event.target)){
      e.preventDefault()
      return false
    }
  }
})
