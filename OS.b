import "io"
import "fsi"
import "string"
import "enhancednewvec"

let sysvar;

manifest 
{ 
  pcbHeap = 1,
  pcbCode = 2,
  pcbStack = 3,
  freelistVA =  0x04000000,
  pInfoPageVA = 0x08000000, 
  sysHeapVA =   0x0C000000,
  userHeapVA =  0x10000000,
  userCodeVA =  0x40000000,
  userStackVA = 0x7FC00000,
  sysStackVA =  0xBFFFFFFF,
  sysAttic =    0xC0000000,
  syspgdirVA =  0xC0000800,
  intvecVA =    0xC0002000,
  syscallsVA =  0xC0001800,
  userpgdirVA = 0xC0005800
}

static { numpages = 0, nextPage = 0}

let setpages() be
{
    let i = 1;
    while freelistVA ! i <> 0 do
    {
        i +:= 1
    }
    numpages := i-1
}
let getpage() be {
    for i = 1 to numpages do
    {
        nextPage +:= 1;
        if nextPage > numpages then
        {
            nextPage := 1;
        }
        if ((freelistVA ! nextPage) bitand 4) = 0 then
        {
            freelistVA ! nextPage := (freelistVA ! nextPage) bitor 4;
            resultis freelistVA ! nextPage;
        }
    }
    out("No more free pages\n");
    resultis nil;
}

let freepage(paddr) be
{
    let pageNum = paddr >> 11, ppage;
    for i = 1 to numpages do
    {
        if pageNum = ((freelistVA ! i) >> 11) then
        {
            if ((freelistVA ! i) bitand 4) = 0 then
            {
                break;
            }
            freelistVA ! i -:= 4;
            ppage := paddr bitand 0xFFFFF800;
            assembly
            { clrpp [<ppage>] }
            return
        }
    }
    out("Page 0x%x not found\n", paddr);
}

let set_timer(t) be {
 assembly { 
   load  r1, [<t>]
   setsr r1, $timer 
 } 
}

let halt_handler() be {
    let saved_FP    = sysAttic ! 6;
    let saved_SP    = sysAttic ! 7;
    let saved_PC    = sysAttic ! 8;

//    out("Halted\n");   
//  out("fp: %x, sp: %x, pc: %x\n", saved_FP, saved_sp, saved_pc);
    assembly
    {
        getsr     r1, $flags
        sbit      r1, $sys
	cbit      r1, $ip
        setsr     r1, $flags
        load      r1, [<saved_FP>]
        load      r2, [<saved_SP>]
        load      r3, [<saved_PC>]
        load      fp, r1
        load      sp, r2
        load      pc, r3
  }  
}

//Keyboard input functions
let kbbuff = vec(25),
    kbbs = 0, kbbe = 0, kbbn = 0, kbbmax = 99, kbblines = 0;

let kbbadd(c) be
{ if kbbn >= kbbmax then resultis 0;
  byte kbbe of kbbuff := c;
  if c = '\n' then kbblines +:= 1;
  kbbn +:= 1;
  kbbe +:= 1;
  if kbbe > kbbmax then kbbe := 0;
  resultis 1 }

let kbbunadd() be
{ let newkbbe = kbbe - 1, c;
  if newkbbe < 0 then newkbbe := kbbmax;
  c := byte newkbbe of kbbuff;
  if c = '\n' \/ kbbn = 0 then resultis 0;
  kbbe := newkbbe;
  kbbn -:= 1;
  resultis 1 }

let kbbremove() be
{ let c;
  if kbblines = 0 then resultis 0;
  c := byte kbbs of kbbuff;
  kbbn -:= 1;
  kbbs +:= 1;
  if kbbs > kbbmax then kbbs := 0;
  if c = '\n' then kbblines -:= 1;
  resultis c }

let kbbackch() be
{ let c;
  if kbbn >= kbbmax + 1 then return;
  kbbs -:= 1;
  if kbbs < 0 then kbbs := kbbmax;
  kbbn +:= 1;
  c := byte kbbs of kbbuff;
  if c = '\n' then kbblines +:= 1 }

let minch() be
{ let c = 0;
  while true do
  { c := kbbremove();
    if c <> 0 then
      resultis c;
    assembly { pause } } }

let minno() be
{ let n = 0, c, s = 0;
  c := minch() repeatuntil c>='0' /\ c<='9' \/ c='-' \/ c='+';
  test c='-' then 
  { s := 1;
    c := minch() }
  or if c='+' then
    c := minch();
  while c>='0' /\ c<='9' do
  { n := n * 10 + c - '0';
    c := minch() }
  if s then
    resultis -n;
  resultis n }

let keyboard_handler() be
{ let c, v = vec 3;
  assembly
  { load  r1, [<v>]
    load  r2, $terminc
    store r2, [r1+0]
    load  r2, 1
    store r2, [r1+1]
    load  r2, <c>
    store r2, [r1+2]
    peri  r2, r1 }
  test c < 32 /\ c <> '\n' then
  { test c = 'H'-64 then
    { if kbbunadd() then
        assembly
        { type 8
          type ' '
          type 8 } }
    else test c = 'X'-64 then { 
      halt_handler() }     
    else
    { kbbadd(c);
      out("[%d]", c) } }
  else
  { if kbbadd(c) then
      assembly
      { type [<c>] } }
  ireturn }

manifest
{ PCB_FLAGS = 0, PCB_INTCODE = 1, PCB_INTADDR = 2, PCB_INTX = 3, PCB_PC = 4,
  PCB_FP = 5, PCB_SP = 6, PCB_R12 = 7, PCB_R11 = 8, PCB_R10 = 9, PCB_R9 = 10,
  PCB_R8 = 11, PCB_R7 = 12, PCB_R6 = 13, PCB_R5 = 14, PCB_R4 = 15,
  PCB_R3 = 16, PCB_R2 = 17, PCB_R1 = 18, PCB_R0 = 19, PCB_STATE = 20,
  SIZEOF_PCB = 21 }

manifest
{ FLAG_R = 1, FLAG_Z = 2, FLAG_N = 4, FLAG_ERR = 8, FLAG_SYS = 16, 
FLAG_IP = 32, FLAG_VM = 64 }

//Pre-VM pcb creation for timesharing
let make_pcb(pcb, pc, sp, code) be
{ pcb ! PCB_R0 := 0;
  pcb ! PCB_R1 := 1*code;
  pcb ! PCB_R2 := 2*code;
  pcb ! PCB_R3 := 3*code;
  pcb ! PCB_R4 := 4*code;
  pcb ! PCB_R5 := 5*code;
  pcb ! PCB_R6 := 6*code;
  pcb ! PCB_R7 := 7*code;
  pcb ! PCB_R8 := 8*code;
  pcb ! PCB_R9 := 9*code;
  pcb ! PCB_R10 := 10*code;
  pcb ! PCB_R11 := 11*code;
  pcb ! PCB_R12 := 12*code;
  pcb ! PCB_SP := sp;
  pcb ! PCB_FP := sp;
  pcb ! PCB_PC := pc;
  pcb ! PCB_INTX := 0;
  pcb ! PCB_INTADDR := 0;
  pcb ! PCB_INTCODE := 0;
  pcb ! PCB_FLAGS := FLAG_R;
  pcb ! PCB_STATE := 'R' }

manifest
{ iv_none = 0,        iv_memory = 1,      iv_pagefault = 2,   iv_unimpop = 3,
  iv_halt = 4,        iv_divzero = 5,     iv_unwrop = 6,      iv_timer = 7,
  iv_privop = 8,      iv_keybd = 9,       iv_badcall = 10,    iv_pagepriv = 11,
  iv_debug = 12,      iv_intrfault = 13 }

manifest { USER_STACK_SIZE = 1000 }

static
{ 
  curr_proc = 0;
  num_procs = 0;
  loading = 1;
  proc_table
}
 
 

let createprocess(name, pn) be {
    let userHindex  = 1;
    let userCindex  = 1;
    let userSindex = 2047;

    let userHeapPage   = getPage();
    let userCodePage   = getPage();
    let userStackPage  = getPage();
 
 let system_pgdir, user_pgdir = getPage();
    let process_id = 0;

    //used for reading in the .exe file for the process
    let r = 512;
    let read = 0;
    let blocks = 16;
    let codeAddr, temp;
    let offset = 0x00000800;
    test pn = 0 then {
      codeAddr := userCodeVA + offset;
    } else {
      codeAddr := userCodeVA + (offset * (pn/10));
    }    
	
    temp := codeAddr;
    curr_proc := pn;
	
    // initialize the first page for the user's heap, code, and stack
    userHeapVA  ! (userHindex+pn)  := userHeapPage;
    userCodeVA  ! (userCindex+pn)  := userCodePage;
    userStackVA ! (userSindex-pn) := userStackPage;

    //sets up the tape to be read
    devctl(DC_TAPE_LOAD, 1, name, 'R');

	//if the tape doesn't exist display error message
    if devctl(DC_TAPE_CHECK, 1) = 0 then
    {
        out("Tape does not exist\n", name);
        return
    }

    //reads in the .exe file for the process
    while r = 512 do
    {
        r := devctl(DC_TAPE_READ, 1, codeAddr);
        if r < 0 then
        {
            out("error %d while reading tape '%s'\n", r, name);
            finish
        }
        read +:= r;
        blocks -:= 1;
        codeAddr +:= 128;
        if blocks = 0 then
        {
            userCodePage := getPage();
            userCindex +:= 1;
            userCodeVA ! (userCindex+pn) := userCodePage;
            blocks := 16
        }
    }
	
    out("\nLoaded %d bytes) of '%s' starting at %x\n", read, name, temp);

    // sets up the PCB
    pinfopageVA ! (1+pn) := userHindex;
    pinfopageVA ! (2+pn) := userCindex;
    pinfopageVA ! (3+pn) := userSindex;
    pinfopageVA ! (4+pn) := 1;  // 1 marks it as runnable, switches to 0 when its finished
    sysAttic ! 11 := user_pgdir;

    system_pgdir := (sysAttic ! 1) bitand 0xFFFFF800;
    user_pgdir   := user_pgdir bitand 0xFFFFF800;
    sysAttic ! (10+pn)  := system_pgdir;
    sysAttic ! (11+pn)  := user_pgdir;
	//separates out the stack for two different processes
    if pn = 0 then {
      proc_table ! (1+pn) := 0x80000000;
      proc_table ! (2+pn) := 0x80000000;
    }
    if pn = 20 then {
      proc_table ! (1+pn) := 0x7FFFFF00;
      proc_table ! (2+pn) := 0x7FFFFF00;
    }     
    //stores the starting address for the process code
    proc_table ! (3+pn) := temp;
   
}

let startProcess(pn) be {
    let saved_FP, saved_SP, saved_PC;
    curr_proc := pn;

	//saves the fp, sp, and pc for the OS shell before starting user programs
    saved_FP := @(sysAttic ! 6);
    saved_SP := @(sysAttic ! 7);
    saved_PC := @(sysAttic ! 8);

	//starts a user program
    assembly
    {
        load      r1, [<saved_FP>]
        load      r2, fp
        store     r2, [r1]
        load      r1, [<saved_SP>]
        load      r2, sp
        store     r2, [r1]
        load      r1, [<saved_PC>]
        load      r2, pc
        add       r2, 14
        store     r2, [r1]

        getsr     r1, $flags
        cbit      r1, $sys
        setsr     r1, $flags
        load      sp, 0x0000
        loadh     sp, 0x8000
        load      fp, sp
        load      r2, 0x0800
        loadh     r2, 0x4000
        jump      r2
    }
	//marks it as finished once it ends
    pinfopageva ! (4+pn) := 0;
}

static { count = 0 }

//timer handler for time sharing 
let timer_handler(intcode, intaddr, intx, pc, fp, sp, r12, r11, r10, r9, r8, r7, r6, r5, r4, r3, r2, r1, r0) be { 
  let pn = curr_proc;


//doesn't try to swap processes if nothing is running yet
  if loading = 1 then {
    ireturn;
  }
  count +:= 1;
  if count = 100 then { 
   outs("\n  next line \n");
   count := 0; 
  }
  while true do {
    pn +:= 20;
    if pinfoPageVA ! (1+pn) = 0 then { 
      pn := 0;
    }
    if pn = curr_proc then{
       break;
    }
    if pinfopageVA ! (4+pn) = 1 then {
      break;
    }
  }
  set_timer(500000);
  //saves the current process' information before loading in the next process
  unless pn = curr_proc do {
    let oldproc = proc_table + curr_proc;
    let newproc = proc_table + pn;
    let newfp, newsp, newpc;
    newfp := newproc ! 1;
    newsp := newproc ! 2;
    newpc := newproc ! 3;
    oldproc ! 1 := fp;
    oldproc ! 2 := sp;
    oldproc ! 3 := pc;
    curr_proc := pn;

    assembly {
      getsr r1, $flags
      cbit  r1, $ip
      setsr r1, $flags
      load  r1, [<newfp>]
      load  r2, [<newsp>]
      load  r3, [<newpc>]
      load  fp, r1
      load  sp, r2
      load  pc, r3
    }
  } 
 ireturn; 
}

let pagefault_handler(intcode, address, info, pc) be {
    let userHindex;
    let userSindex;
    let sysHindex;
    let sysSindex;
    let userCindex;
//allocates more heap space for the user 
    test address > 0x10000000  /\ address < 0x10010000 then {
        userHindex := pinfopageVA ! (1+curr_proc);
        userHindex +:= 1;
        userHeapVA ! userHindex := getpage();
        pinfopageVA ! (1+curr_proc) := userHindex;
//allocates more stack space for the user
    } else test address > 0x7FFF0000 /\ address < 0x7FFFFFFF then {
        userSindex := pinfopageVA ! (3+curr_proc);
        userSindex -:= 1;
        userStackVA ! userSindex := getpage();
        pinfopageVA ! (3+curr_proc) := userSindex;
//allocates more space for user code
    } else test address > 0x40000000 /\ address < 0x40100000 then {
        userCindex := pinfopageVA ! (2+curr_proc);
        userCindex +:= 1;
        userCodeVA ! userCindex := getpage();
        pinfopageVA ! (2+curr_proc) := userCindex;
//allocates more heap space for the system if needed
     } else test address > 0x0C000000 /\ address < 0x0C010000 then {
        sysHindex := sysAttic ! 2;
        sysHindex +:= 1;
        sysHeapVA ! sysHindex := getpage();
        sysAttic ! 2 := sysHindex;
//allocates more stack space for the system if needed
     } else test address > 0x90000000 /\ address < 0xBFFFFFFF then {
        sysSindex := sysAttic ! 4;
        sysSindex +:= 1;
        sysStackVA ! sysSindex := getpage();
        sysAttic ! 5 := sysSindex;
    }  else {
//displays faulty address if it is not inside one of the alowable areas
        out("address was 0x%x\n", address);
        out("PC was 0x%x\n", pc);
        finish
    }
    ireturn
}
//pointers to the disc and file objects for the file system
static { disc = 0; file = 0}

//System call functions
//exit system call for user programs to halt early
let exit_call() be {
  halt_handler();
}

//mounting a disc to open a file
let mount_call(num,name) be {
   disc := mount(num, name);
}

//dismounting a disc to save changes
let dismount_call() be {
  if disc <> 0 then {
    dismount(disc);
    disc := 0;
  }
}

//opening a file
let open_call(name, mode) be {
   if disc <> 0 then {
     file := open(disc, name, mode);
   }
}

//closing a file
let close_call() be {
  if ( file <> 0) then {
  close(file);
  file := 0;
  }
}

//reading from a file
let read_call() be {
  if file <> 0 then {
    readbyte(file);
  }
}

//writing to a file
let write_call(x) be {
  if file <> 0 then {
    writebyte(file, x);
  }
}

let shell() be {
  let input = vec 80, disc, file, inputerr = 0;
  let heap = sysHeapVA;
  let n;
  let user_stack_1, user_stack_2, user_stack_3;
  let pcb_1, pcb_2, pcb_3;
  let prog1, prog2, i;  
  let sysvar;

  out("Basic Operating System\n");
 
  newvec := good_newvec;
  freevec := good_freevec;
  
  init(heap, 20000); 
  user_stack_1 := newvec(USER_STACK_SIZE);
  user_stack_2 := newvec(USER_STACK_SIZE);
  user_stack_3 := newvec(USER_STACK_SIZE);
  pcb_1 := getpage();
  pcb_2 := getpage();
  pcb_3 := getpage();

  sysvar := newvec(3); 
  proc_table := newvec(100);

  while true do {
    let input = vec 32;
    if inputerr = 0 then out("> ");
    inputerr := 0;
    ins(input, 80);
    test strcmp(input, "format") = 0 then
      {
        let num, name = vec 32;
        out("Enter disc unit number: ");
        num := inno();
        out("Enter name: ");
        ins(name, 32);
        format(num, name)
      }
    else test strcmp(input, "mount") = 0 then
      {
        let num, name = vec 32;
        out("Enter disc unit number: ");
        num := inno();
        out("Enter name: ");
        ins(name, 32);
        disc := mount(num, name);
      }
    else test strcmp(input, "dismount") = 0 then
      {
	    test disc <> 0 then {
          dismount(disc);
		  disc := 0;
		} else {
		  out("no disc mounted\n");
		}
      }
    else test strcmp(input, "ls") = 0 then
      {
	    test disc <> 0 then {
          ls(disc);
		} else {
		  out("no disc mounted\n");
		}
      }
    else test strcmp(input, "run") = 0 then
      {
        let file_name = vec(32);
        let prog3;
        out("Enter the file name: ");
        ins(file_name, 32);
        num_procs +:= 1;
        createProcess(file_name, 0);
        startProcess(0);
        num_procs -:= 1;
      }
    else test strcmp(input, "timeshare") = 0 then
      {
        num_procs +:= 2;
        intvecVA ! iv_keybd := keyboard_handler;
        set_timer(500000);
        createProcess("prog1.exe", 0);
        createProcess("prog2.exe", 20);
        loading := 0;
        startProcess(0);
        loading := 1;
        num_procs -:= 2;
        intvecVA ! iv_keybd := nil;
      }
    else test strcmp(input, "open") = 0 then
      {
	   test disc <> 0 then {
        let file_name = vec 32, direction = 'w';
        out("Enter the file name: ");
        ins(file_name, 32);
        out("Enter direction (R)ead or (W)rite: ");
        direction := inch();
        file := open(disc, file_name, direction);
	   } else {
	     out("no disc mounted\n");
	   }
      }
    else test strcmp(input, "create") = 0 then
      {
	   test disc <> 0 then {
        let file_name = vec 32;
        out("Enter the file name: ");
        ins(file_name, 32);
        create(disc, file_name);
	   } else {
	     out("no disc mounted\n");
	   }
      }
    else test strcmp(input, "mkdir") = 0 then 
      {
	    test disc <> 0 then {
          let name = vec 32;
          out("Enter directory name: ");
          ins(name, 32);
          mkdir(disc,name);
		} else {
	     out("no disc mounted\n");
	    }
      }
    else test strcmp(input, "rmdir") = 0 then 
      {
        let name = vec 32;
        out("Enter directory name: ");
        ins(name, 32);
        rmdir(disc, name);
      }
    else test strcmp(input, "cd") = 0 then 
      {
	    test disc <> 0 then {
          let name = vec 32;  
          out("Enter directory name: ");
          ins(name, 32);
          changedir(disc,name);
		} else {
		  out("no disc mounted\n");
		}
     }
    else test strcmp(input, "close") = 0 then
      {
	   test file <> 0 then {
        close(file);
		file := 0;
	   } else {
	     out("no file open\n");
	   }
      }
    else test strcmp(input, "delete") = 0 then
      {
	   test disc <> 0 then {
        let file_name = vec 32;
        out("Enter the file name: ");
        ins(file_name, 32);
        delete(disc, file_name);
	   } else {
	     out("no disc mounted\n");
	   }
      }
    else test strcmp(input, "readbyte") = 0 then
      {
	   test file <> 0 then {
        readbyte(file);
	   } else {
	     out("no file open\n");
	   }
      }
    else test strcmp(input, "writebyte") = 0 then
      {
	   test file <> 0 then {
        let x;
        out("Enter the byte to write: ");
        x := inno();
        writebyte(file, x);
	   } else {
	     out("no file open\n");
	   }
      }
    else test strcmp(input, "eof") = 0 then
      {
	   test file <> 0 then {
        let b = eof(file);
        if b = 1 then out ("Yes\n");
        if b = 0 then out ("No\n");
	   } else {
	     out("no file open\n");
	   }
      }
    else test strcmp(input, "save") = 0 then
      {
	  test disc <> 0 then {
        save(disc);
	  } else {
	    out("no disc mounted\n");
	  }
      }
    else test strcmp(input, "") = 0 then
      {
        // Sometimes empty input would get passed
        // through for some reason so this eliminates
        // that from causing an error message.
        inputerr := 1;
      }
    else test strcmp(input, "exit") = 0 then
      {
	    if disc <> 0 then {
		  save(disc);
		}
        break;
      } 
    else
      {
        out("Invalid input\n");
      }
  }
  out("Exiting\n");
}

let start() be {
//check how many free pages there are
  setPages();
//setup the interrupt handlers
  intvecVA ! iv_timer := timer_handler;
  intvecVA ! iv_pagefault := pagefault_handler;
  intvecVA ! iv_halt := halt_handler;

//turns on interrupt processing
  assembly {
    getsr  r1, $flags
    cbit   r1, $ip
    setsr  r1, $flags
  }
  
  //sets up the system call vector
  syscallsVA ! 0 := 0;
  syscallsVA ! 1 := exit_call;
  syscallsVA ! 2 := mount_call;
  syscallsVA ! 3 := dismount_call;
  syscallsVA ! 4 := open_call;
  syscallsVA ! 5 := close_call;
  syscallsVA ! 6 := read_call;
  syscallsVA ! 7 := write_call;
  
  assembly
    {
        getsr     r2, $cglen
        load      r2, 8
        setsr     r2, $cglen
    }
//jumps to the OS shell for user commands
  shell();
}


