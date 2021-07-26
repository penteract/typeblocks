import math
import sys

w=0x80
h=0x80

header=f"""
42 4d 36 {hex((w*h*3)//256)[-2:]} 00 00 00 00 00 00 36 00 00 00 28 00
00 00 40      00                   00 00 40 00 00 00 01 00 18 00 00 00
00 00 00 {hex((w*h*3)//256)[-2:]} 00 00 13 0b 00 00 13 0b 00 00 00 00
00 00 00 00 00 00"""

hdr=bytes([0x42,0x4d])
hsz=0x36
psz=3 # assume w*psz is a multiple of 4
hdr+=(hsz+psz*w*h).to_bytes(4,"little")
hdr+=bytes(int(k,16) for k in "00 00  00 00  36 00 00 00   28 00 00 00".split())
hdr+=w.to_bytes(4,"little")
hdr+=h.to_bytes(4,"little")
hdr+=bytes(int(k,16) for k in "01 00  18 00  00 00 00 00".split())
hdr+=(psz*w*h).to_bytes(4,"little")
hdr+=bytes(int(k,16) for k in "13 0b 00 00 13 0b 00 00  00 00 00 00 00 00 00 00".split())



assert len(hdr)==0x36

CENTER=0.58/1.5
NUMINCENTER=3

def f(x,y):
  #if 0.25<x/w<0.75 and 0.25<y/h<0.75:
  #  return 0
  k=math.sin(math.pi*2*(NUMINCENTER*x/(w*CENTER)))
  #if abs(x-y)<4:
  #  return k*((abs(x-y)+4)/8)
  #if abs(x-y)>w*0.8:
  #  return k/(1+ (abs(x-y) - w*0.8)/4)
  return k

sys.stdout.buffer.write(hdr+bytes (int((f(x,h-1-y) if c==1 else f(h-1-y,x) if c==2 else 0)*128+128) for y in range(h) for x in range(w) for c in range(psz)))

