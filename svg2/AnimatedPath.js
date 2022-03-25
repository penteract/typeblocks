"use strict"

// Theoretical function that would describe the ideal path over time
// -4<x<4
//const Λ=3
function wave(x,t,Λ){
  return 4*Math.sin(x/Λ*(2*Math.PI)-t)*Math.sin((x+4)/8*Math.PI)**2 // t may be multiplied by wave speed, and a phase adjusting constant may be added
}

function dwave(x,t,Λ){// d wave(x,t,Λ) / dx
  let a = 4*Math.cos(x/Λ*(2*Math.PI)-t)*((1-Math.cos((x+4)/4*Math.PI))/2)/Λ*(2*Math.PI)
  +       4*Math.sin(x/Λ*(2*Math.PI)-t)*(Math.sin((x+4)/4*Math.PI)/2)/4*Math.PI
  return a
}
function zeros(t,Λ){// The values of x for which wave(x,t,Λ) is zero
  // x*Λ*(2*Math.PI)-t is an integer multiple of PI
  // x*2/Λ-t/Math.PI is an integer (n)
  // x = (n+t/Math.PI)*Λ/2
  t/=Math.PI
  let result = []
  let minn = (-4*2/Λ-t)|0
  let n = minn
  while (n<=4*2/Λ-t){
    let x = (n+t)*Λ/2
    if(x>3.99) break;
    else if (x<-3.99) minn+=1
    else result.push(x)
    n += 1
  }
  return [result,minn,n]
}

// make an svg path consisting of cubic bezier curves
// which is close to the graph of wave(x,t,Λ) for fixed t and Λ
function wavePath(t,Λ){
  // 0<t<2*Math.PI
  // maximum number of points at the start/end: 3
  let pz = zeros(0,Λ)
  let path = "L -4 0"
  let zs = zeros(t,Λ)

  for(let i=0; i < zs[1]-pz[1]+2; i++){
    path += " C -4 0 -4 0 -4 0"
  }
  function mkGrad(x0,x1){
    // return the first control point between x0 and x1
    // (both should be zeros of wave(x,t,Λ))
    let Δx = x1-x0
    let ymid = wave(x0+Δx/3,t,Λ)
    let dy = dwave(x0,t,Λ)
    let dx
    if (Math.abs(dy)<0.001) dx = Δx/2
    else dx = (ymid*1.5)/dy
    if (Math.abs(dx)>Math.abs(Δx)/2) dx=Δx/2
    return [x0+dx, dy*dx]
  }
  let z0 = zs[0][0]
  path += ` C ${(z0+4)/2-4} 0`
  let prevx =-4
  let i=0
  let zi = zs[0][i]
  zs[0].push(4)
  while (i<zs[0].length-1){
    let [cx,cy] = mkGrad(zi,prevx)
    path+=` ${cx} ${cy} ${zi} 0`
    prevx =zi
    zi = zs[0][++i]
    ;[cx,cy] = mkGrad(prevx,zi)
    path += ` C ${cx} ${cy}`
  }
  path+=` ${(prevx-4)/2+4} 0 4 0`
  for(let i=0; i < pz[2]-zs[2]; i++){
    path += " C 4 0 4 0 4 0"
  }
  return path
}

function animatedPaths(Λ){
  // return a list of paths suitable for animation of wave(x,t,Λ)
  let pths=[]
  for (let i=0;i<=2*Math.PI+0.001;i+=2*Math.PI/8){
    pths.push(wavePath(i,Λ))
  }
  return pths
}