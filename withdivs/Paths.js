"use strict";
function diff([x1,y1],[x2,y2]){
  return [x1-x2,y1-y2]
}
function add([x1,y1],[x2,y2]){
  return [x1+x2,y1+y2]
}
function mul([x1,y1],k){
  return [x1*k,y1*k]
}


// Mini component = dx, mid, pathpart
// Describes a path from 0,0 to dx,-2 roughly passing through mid,-1
// A full piece is composed of 4 mini components and some spacing
// -- a             d --
//    b --spacing-- c

// c and d get rotated 180 degrees

const mdiag1 = {
    "dx" : 2
  , "mid" : 1
  , "pathpart" : ["l",[2,-2]]
  //, "space" : 0
  }
const mdiag2 = {
    "dx" : -2
  , "mid" : -1
  , "pathpart" : ["l",[-2,-2]]
  //, "space" : 2
  }
const msemi1 = {
    "dx" : 0
  , "mid" : 1
  , "pathpart" : ["a",[1,1],"0 0 1", [0,-2]]
  //, "space" : 0
  }
const msemi2 = {
    "dx" : 0
  , "mid" : -1
  , "pathpart" : ["a",[1,1],"0 0 0", [0,-2]]
  //, "space" : 1
  }
function marca(n){
  return {
      "dx" : 2
    , "mid" : 0.3+n*1.4
    , "pathpart" : ["a",[2,2],`0 0 ${n}`, [2,-2]]
    //, "space" : 0
    }
}
const marc1 = marca(0)
const marc2 = marca(1)
function marcb(n){
  return {
      "dx" : -2
    , "mid" : -1.7+n*1.4
    , "pathpart" : ["a",[2,2],`0 0 ${n}`, [-2,-2]]
    //, "space" : 1+n
    }
}
const marc3 = marcb(0)
const marc4 = marcb(1)
const mvert = {
  "dx" : 0
, "mid" : 0
, "pathpart" : ["l", [0,-2]]
//, "space" : 0
}
marc2,mdiag1,marc1,mdiag2
const minis = [mdiag1,mdiag2,msemi1,msemi2,marc1,marc2,marc3,marc4,mvert]


// Determine the minimum spacing to avoid overlap
// -- a             d --
//    b --spacing-- c

function _spacing(b,c){
  // b.mid - b.dx < spacing - c.mid // spacing > b.mid+c.mid - b.dx
  // -b.dx < spacing - c.dx //
  return Math.max(b.mid+c.mid-b.dx, +c.dx - b.dx+0.5 )+1
}
function spacing(a,b,c,d){
  return  Math.max(_spacing(b,c), _spacing(a,d)-b.dx+c.dx+0.5, 0)
}

// find how far left the path extends
function left(a,b){
  return -Math.min(0,a.mid,a.dx,a.dx+b.mid,a.dx+b.dx)
}

function addpath(pth, parts, scale){
  for(let part of parts){
    if (Array.isArray(part)){
      let [x,y]=part
      pth.push([x*scale,y*scale])
    }
    else pth.push(part)
  }
}

function makePath(a,b,c,d, scale){
  let sp = spacing(a,b,c,d)+0.01
  let l = left(a,b)
  let r = left(c,d)+a.dx+b.dx+sp
  let pth = []
  pth.push("l",[20-scale*(r-l)/2, 0])
  addpath(pth, a.pathpart, scale)
  addpath(pth,["l", [0,0.01]], scale)
  addpath(pth, b.pathpart, scale)
  addpath(pth,["l", [sp,0]], scale)
  addpath(pth,c.pathpart,-scale)
  addpath(pth,["l", [0,-0.01]], scale)
  addpath(pth,d.pathpart,-scale)
  return pth

  //return `M 0 0 L ${10-(r-l)/2} 0 ${stretchPath(a.pathpart,[0,0],[20,0])} l 0 0.01 ${stretchPath(b.pathpart,[0,0],[20,0])} l ${sp} 0 ${stretchPath(c.pathpart,[0,0],[-20,0])} l 0 -0.01 ${stretchPath(d.pathpart,[0,0],[-20,0])} L 20 0 L 20 20 L 0 20 Z`
}

// stretches a path along x and rotates it to flow from start to end.
// for some reason, it reflects points in the Y axis
function stretchPath(shape,start,end){
  let [dx,dy] = diff(end,start)
  let scale = Math.hypot(dx,dy)
  dx/=40
  dy/=40
  let pts=[]
  for (let p of shape){
    if (Array.isArray(p)){
      let [x,y] = p
      pts.push([x*dx+y*dy, x*dy-y*dx].join(" "))
    }
    else pts.push(p)
  }
  pts.push("L "+end.join(" "))
  return pts.join(" ")+" "
}

function getPath(type, scale){
  let path=""
  let n = hash(type)
  let abcd=[]
  for(let i=0;i<4;i++){
    abcd.push(minis[n%minis.length])
    n=Math.floor(n/minis.length)
  }
  if(n==0)console.log(type,n)
  return makePath(...abcd, scale)
}

//Path for border-image, ignores margins, starts at 0,0
function borderPath(w,h,type){
  let bl = [0,0]
  let br = [w,0]
  let tr = [w,h]
  let tl = [0,h]
  let pth = getPath(type,2)
  var path = "M "+bl[0]+" "+bl[1]+" "+
    stretchPath(pth,bl,br) +
    stretchPath(pth,br,tr) +
    stretchPath(pth,tr,tl) +
    stretchPath(pth,tl,bl) +"Z "
  return path
}
function testp(n){
  return borderPath(40,40,n)
}

function borderSVG(type,fillcol,strokecol){
  // Quite a bit of this could be cached
  return`<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 40 40" width="100" height="100"> <path stroke="${strokecol}" fill="${fillcol}" d="${borderPath(40,40,type)}" /> </svg>`
}
