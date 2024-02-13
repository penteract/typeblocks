"use strict";


for (let k in builtins){
  let [type,defn] = builtins[k]
  let parsedType=parseType(partition(type)[0])
  let g = makeBox(k,parsedType)
  g.defn = defn
  imports.appendChild(g)
  g.visit(n => {
    n.isImport=true
    n.draggable = (n===g)
  })
  g.redraw(100)
  //console.log(g,g.type)
}
for(let i=1,h=0; i<imports.children.length; i++){
  imports.children[i].setPos(0,h)
  h+=imports.children[i].height+SPACINGV/2
}