import "io"

static { lastPage, nextFreePage, numberOfPages}

let tape_load(number, fName, mode) be {
    let r = devctl(DC_TAPE_LOAD, number, fName, mode);
    if r <= 0 then {
        out("Could not load tape number %d\n", number);
    }
    resultis r;
}

let setup_memory() be {
  let firstword = (! 0x101);
  let lastword  = (! 0x100) - 1;
  nextFreePage := (firstword + 2047) >> 11;
  lastPage      := lastword >> 11;
}

let getpage() be {
  let p = nextFreePage << 11;
  if nextFreePage > lastPage then {
    out("No more free pages before OS starts\n");
    finish
  }
  nextFreePage +:= 1;
  assembly {
    clrpp [<p>]
  }
  resultis p 
}

let loados(name) be {  
  let pgdir = getpage();
  let ptcode = getpage();
  let ptheap = getpage();
  let ptstack = getpage();
  let ptspec = getpage();
  let psyscalls = getpage();
  let intvec = getpage();
  let freelist = getpage();
  let pinfopage = getpage();
  let userheap = getpage();
  let usercode = getpage();
  let userstack = getpage();
  let sysheap = getpage();
  let syscode = getpage();
  let sysstack = getpage();
  let n, avail, pos, r;
  
  r := tape_load(1, name, 'r');
  if r < 0 then { 
    out("error %d for loading os file '%s'\n", r, name);
    finish 
  }
  
  assembly {	
    load  r1,[<intvec>]
    setsr r1, $intvec
    load  r1, [<psyscalls>]
    setsr r1, $cgbr
  }
  
  ptcode  ! 0    := syscode bitor 1;
  ptheap ! 1     := sysheap bitor 1;
  ptstack ! 2047 := sysstack bitor 1;
   
  n := 0;
  avail := 16;
  pos := 1;
  r := 512;
  while r = 512 do { 
    r := devctl(DC_TAPE_READ, 1, syscode);
    if r < 0 then {
      out("error %d while reading os file '%s'\n", r, name);
      finish
    }
    n +:= r;
    avail -:= 1;
    syscode +:= 128;
    if avail = 0 then {
      syscode := getpage();
      ptcode ! pos := syscode bitor 1;
      pos +:= 1;
      avail := 16
    } 
  }
  
  
  ptheap  ! 0    := ptheap bitor 1;
  freelist ! 0   := freelist bitor 1;
  userheap ! 0   := userheap bitor 1;
  usercode ! 0   := usercode bitor 1;
  userstack ! 0  := userstack bitor 1;
  pinfopage ! 0  := pinfopage bitor 1;
  
  pgdir ! 0x010 := freelist bitor 1;
  pgdir ! 0x020 := pinfopage bitor 1;
  pgdir ! 0x030 := ptheap bitor 1;
  pgdir ! 0x040 := userheap bitor 1;
  pgdir ! 0x100 := usercode bitor 1;
  pgdir ! 0x1FF := userstack bitor 1;
  pgdir ! 0x200 := ptcode bitor 1;
  pgdir ! 0x2FF := ptstack bitor 1;
  pgdir ! 0x300 := ptspec bitor 1;
  
  ptspec ! 0 := ptspec bitor 1;
  ptspec ! 1 := pgdir bitor 1;  // pointer to page dir
  ptspec ! 2 := 1;              //sysheap starting index
  ptspec ! 3 := psyscalls bitor 1; //system call vector
  ptspec ! 4 := intvec bitor 1; //int vec
  ptspec ! 5 := 2047;              //start of system stack index  

  numberOfPages := lastpage - nextfreepage + 1;
    for i = 1 to numberOfPages do
    {
        freelist ! i := getpage() bitor 1;
    }
 
  out("%d bytes, %d pages of '%s' loaded\n", n, pos, name);
  resultis pgdir 
}


let start() be
{ let pdir;
  setup_memory();
  pdir := loados("OS.exe");
  out("Loaded OS, jumping there now\n");
  assembly {
    load   r1, [<pdir>]
    setsr  r1, $pdbr
    load   sp, 0x0000
    loadh  sp, 0xC000
    load   fp, sp
    getsr  r1, $flags
    sbit   r1, $vm
    load   r2, 0x0000
    loadh  r2, 0x8000
    flagsj r1, r2 
  }
}
