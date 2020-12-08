function diff([x1,y1],[x2,y2]){
  return [x1-x2,y1-y2]
}
function add([x1,y1],[x2,y2]){
  return [x1+x2,y1+y2]
}
function mul([x1,y1],k){
  return [x1*k,y1*k]
}


//given an initial height and width of a spike,
//make a path which describes infinitely many spikes
//scaling down as they get further from the center
function mkzigzag(h,w,scale){
  if(scale>=1)throw "will not terminate"
  let l=50
  let r=50
  let p=["L", [l=l-w, 0], "L", [50, h], "L", [r=r+w, 0]]
  while(w>1){
    h*=scale
    w*=scale
    p=["L", [l=l-w, h]].concat(p).concat( ["L", [r=r+w, h]])
    p=["L", [l=l-w, 0]].concat(p).concat( ["L", [r=r+w, 0]])
  }
  return p.concat(["L", [100, 0]])
}

// top edges
caret =["L",[50,-40],"L",[100,0]]
point =["L",[40,0], "L",[50,-40], "L",[60,0], "L",[100,0]]
zigzag = mkzigzag(-40,8,0.7)
square =["L",[30,0], "L",[30,-40], "L",[70,-40], "L",[70,0], "L",[100,0]]
curve =["C",[25,25],[75,-90],[100,0]]
curve2 =["C",[25,25],[25,-70],[50,0],"C",[75,25],[75,-70],[100,0]]
circ = ["L", [40,0], "A", ["scale",10,10], "0 1 0", [40, 0.001], "L", [100, 0]]

shapes=[caret,point,square,curve,curve2,zigzag] //,circ]


// stretches a path along x and rotates it to flow from start to end.
// Does not include the point 'start' -  assumes that it was already there
function makePath(name,start,end){
  if (name=="line"){
    return `L ${end[0]} ${end[1]}`
  }
  var [dx,dy] = diff(end,start)
  scale = Math.hypot(dx,dy)
  dx/=100
  dy/=100
  pts=[]
  for (p of shapes[hash(name)%shapes.length]){
    if (Array.isArray(p)){
      if (p[0]==="scale"){
        var[x,y]=p.slice(1)
        y=y*13/scale
        pts.push([x*dx+y*dy, x*dy-y*dx].join(" "))
      }else{
        var [x,y]=p
        y=y*5/scale
        pts.push(add(start,[x*dx+y*dy, x*dy-y*dx]).join(" "))
      }
    }
    else pts.push(p)
  }
  return pts.join(" ")+" "
}
MARX=3// Rise up!
MARY=3
BASEHEIGHT=7//with text (vertical center is BASEHEIGHT/2)

// Given starting coordinates and a base type, make a path
function simplePath(x,y,w,h,type){
  y = y-(BASEHEIGHT-h)/2
  bl = [x-MARX,y+MARY]
  br = [x+w+MARX,y+MARY]
  tr = [x+w+MARX,y-h-MARY]
  tl = [x-MARX,y-h-MARY]
  var pth = "M "+bl[0]+" "+bl[1]+" "+
    makePath(type,bl,br) +
    makePath(type,br,tr) +
    makePath(type,tr,tl) +
    makePath(type,tl,bl) +"Z "
  return pth
}
//Make a path to work with a filled hole
//Note: x doesn't translate the output of this, just gives the width of the thing in the hole
function filledHolePath(x,y,w,h,type){
  y = y-(BASEHEIGHT-h)/2
  bl = [-MARX,y+MARY]
  bm = [x+MARX,y+MARY]
  br = [x+w+MARX,y+MARY]
  tr = [x+w+MARX,y-h-MARY]
  tm = [x+MARX,y-h-MARY]
  tl = [-MARX,y-h-MARY]
  var pth = "M "+bl[0]+" "+bl[1]+" "+
    makePath(type,bl,bm) +
    makePath("line",bm,br) +
    makePath("line",br,tr) +
    makePath("line",tr,tm) +
    makePath(type,tm,tl) +
    makePath(type,tl,bl) +"Z "
  return pth
}
