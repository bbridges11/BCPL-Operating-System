/*
NEWVEC:
This implementation of newvec returns a pointer to the supplied number of words
but also allocates 3 additional words: Whether or not the chunk is in use,
the size, and the size again at the end of the chunk. The reason for the size
at the end is because in the future freevec will work its way up and down the
heap and merge adjacent chunks that are not in use. The pointer starts at the bottom
of the heap and searches for the first place it can put the chunk. It has to have
enough space to store the given number of words and turn the rest of the space into
an out of use chunk. 

FREEVEC:
This implementation of freevec simply sets the word corresponding to use to 0.
This allows newvec to allocate a subset of that space.

*/

export {
  good_newvec, good_freevec
}


import "io"

manifest {
  F_USED = 0,
  F_SIZE = 1,
  F_DATA = 2,
  F_SIZEEND = -1,
  F_INUSE = -100,
  F_NOTINUSE = -99,
  NUM_ENTS = 3
}


static { vecsize = 0, vecused = 0, vecspace, minaddr }

let init1(v, s) be
{ vecsize := s;
  vecspace := v + 1;
  minaddr := v;
  vecused := 0 }

let good_newvec(n) be {
  let r = vecspace;
  let rsize = 0;

  n +:= (n + NUM_ENTS) rem 4;
    
  while rsize < vecsize do {   
    out("size: %d, vecused: %d ", rsize,vecused);
    out("fsize: %d, fused: %d\n\n", r ! F_SIZE, r ! F_USED);
    out("size of chunk: %d, n: %d\n", r ! F_SIZE, n+NUM_ENTS);
    
    if r ! F_USED <> F_INUSE /\ (r ! F_SIZE) > (n+NUM_ENTS) then {
      let oldsize,newsize;             //Replace an out of use chunk.
      oldsize := r ! F_SIZE;
      r ! F_USED := F_INUSE;           //Put it back in use
      r ! F_SIZE := n;                 //Put new size
      (r + n + NUM_ENTS) ! F_SIZEEND := n; 
      (r + n + NUM_ENTS) ! F_USED := F_NOTINUSE;    // Put the rest in out of use chunk.
      (r + n + NUM_ENTS) ! F_SIZE := oldsize - n - NUM_ENTS;
      (r + oldsize + NUM_ENTS) ! F_SIZEEND := oldsize - n - NUM_ENTS;
      out("Replaced newvec at %x\n", r + F_DATA);
      resultis r + F_DATA;
    }
    
    if vecused = 0 \/  r ! F_SIZE = 0 then {
      r ! F_USED := F_INUSE;                 //Create newvec at top of heap.
      r ! F_SIZE := n;
      (r + n + NUM_ENTS) ! F_SIZEEND := n;
      vecused +:= n + NUM_ENTS;
      out("Placed newvec at %d\n", vecused - n - 2);
      resultis r + F_DATA; 
    }
    rsize +:= (r ! F_SIZE) + NUM_ENTS;  //Advance the pointer.
    r +:= (r ! F_SIZE) + NUM_ENTS;
  }
  out("ERROR: Out of space in heap.\n");
}

/*let better_freevec(v) be {
  v -:= F_DATA;
  v ! F_USED := F_NOTINUSE;
  out("Freed vec at %x\n", v);
}*/

let good_freevec(v) be {
  let up, down, space;
  let bottommost, topmost;
  v -:= F_DATA;
  bottommost := v;
  topmost := v;
  up := v + (v ! F_SIZE) + NUM_ENTS;
  down := v - (v ! F_SIZEEND) - NUM_ENTS;
  //if down < minaddr then down := minaddr;
  out("up: %x, down: %x, minaddr: %x, v: %x\n", up, down, minaddr, v);
  space := v ! F_SIZE; 
  
  v ! F_SIZE := 0;
  v ! F_USED := F_NOTINUSE;
  //v ! F_SIZEEND := 66;
 
  while up ! F_USED = F_NOTINUSE do {
    let size = up ! F_SIZE;
    out("ENTERED UP\n");
    topmost := up;
    if up ! F_SIZE = 0 then {
      break;
    } 
    out("Entered up: %x\n", up);
    up ! F_USED := F_NOTINUSE;
    up ! F_SIZE := 0;
    up ! F_SIZEEND := 0;
    up +:=  size;
    space +:= size +NUM_ENTS;
    out("space: %d\n", space);
    //out("space2: %d size: %d\n", space, size);
  }
  while down ! F_USED = F_NOTINUSE /\ down > minaddr do {
    let size = down ! F_SIZE;
    out("ENTERED DOWN\n");
    bottommost := down;
    out("Entered down, replacing: %x\n", down);
    //down ! F_USED := F_NOTINUSE;
    //(down + size) ! F_SIZE := -42;
    down := down - (down ! F_SIZEEND);
    space +:= size + NUM_ENTS;
    out("Increased space %d\n", space);
    out("new down: %x\n", down);
    if down ! F_SIZE = 0 \/ down ! F_SIZEEND = 0 then {
      break;
    }
    
  }
  bottommost ! F_USED := F_NOTINUSE;
  bottommost ! F_SIZE := space;
  (bottommost + space + NUM_ENTS) ! F_SIZEEND := space;
  //v ! F_SIZE := space;
  //out("v: %x\n", v);
  //out("space: %d\n", space);
  //(v + space + NUM_ENTS) ! F_SIZEEND := space;
  out("Freed vec at %x\n", v);
}

let testheap(x, heap) be {
  for i=0 to x do {
    out("%3d: %4d    %10x    %x\n", i, heap ! i, heap ! i, heap + i);
  }
}

/*let start() be {
  let heap = vec 10000;
  let x,y,z;
  init1(heap, 10000);
  newvec := better_newvec;
  freevec := better_freevec;
  z := newvec(5);
  testheap(30, heap);
  y := newvec(10);
  testheap(30, heap);
  x := newvec(10);
  testheap(30, heap);
  better_freevec1(y);
  testheap(30, heap);
  better_freevec1(z);
  testheap(30, heap);
  out("Terminated successfully\n");
}*/
