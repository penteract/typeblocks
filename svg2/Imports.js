"use strict";


for (let k of ["+"]){ // in builtins){
    let [type,defn] = builtins[k]
    let parsedType=parseType(partition(type)[0])
    //let g = makeBox(k,parsedType)
    let c = Math.random()*360
    let fill = [c,100,80] //hsluv.hsluvToHex([c,100,80]) //`hsl(${c},100%,50%)`
    let stroke =  hsluv.hsluvToHex([c,50,50])//`hsl(${c},50%,70%)`
    let g = makeBox(k,parsedType,[fill,stroke],["#DDD","#BBB"],false,root)
    //g.colors = [fill,stroke]
    g.defn = defn
    root.appendChild(g)
    redrawDirty()
    let svg = document.createElementNS(SVGNS,"svg")
    imports.appendChild(svg)
    svg.setAttribute("viewBox","0 0 "+g.width+" "+g.height)
    svg.appendChild(g)
    svg.classList.add("import")
    console.log(g,g.type)

    /*
    g.visit(n => {
      n.isLHS=true
      n.draggable = (n===g)
    })*/
  
  }