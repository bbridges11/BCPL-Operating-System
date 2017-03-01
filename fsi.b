import "io"
import "string"
import "vec"

export {
  format, mount, disc_write, disc_read, save, dismount,
  ls, changedir, mkdir, rmdir, create, open, close, readbyte, writebyte, executefile,
  delete, eof, tape_read
}

/* File System using multiple pointer levels for non-contigous memory allocations
   format: Erases everything on the disc, sets up the superblock and empty root directory, and creates a freelist with every remaining disc block in it.
   mount: Loads the specified disc, reads the root dirctory, superblock, and current block of the freelist into memory and returns a pointer to the disc struct.
   dismount: Writes the superblock, current directory, and root directory back to disc and frees the memory for the disc struct. Sets current disc number to -1 to indicate no disc is loaded.
   save: Performs the operations for dismount without unloading the current disc. 
   ls: Prints a human-readable listing of all files and directories within the current directory. Files and directories are marked 'File' and 'Dir' respectively in the listing.
   changedir: changes the current directory to the one specified. Specified directory must exist within the current directory. Input "." leaves current directory unchanged. Input ".." changes to the 
              current directories parent, if the current directory is not the root directory.    
   mkdir: Creates a new empty directory within the current directory.
   rmdir: Deletes the specified directory, along with all files and directories within it and returns the used blocks to the free list.
   create: creates a new file within the current directory. If the specified name already exists, returns warning message and does not create file. If the disc is completely full, returns error message and does not create file. Files start at a size of 0 and increase as data is written to them.
   open: Opens a file for either reading 'r' or writing 'w'. Returns a pointer to the file struct which contains information about the file like the size, pointer to the header block, current position  
         within the file, etc.
   close: Frees the memory allocated to the file struct. If the file was open for writing, writes the file buffer back to disc to make sure no changes are lost.
   readbyte: Returns the next byte in the file, or a message if it has reached the end of the file.
   writebyte: Writes a byte to the files buffer if the buffer is not full. If the buffer is full, writes the buffer back to disc, empties it,  and then adds the new byte to the buffer.
   executefile: Loads an executable file from tape and runs it.
   delete: If the file exists in the current directory, marks it as deleted in the directory and returns all blocks allocated to the file to the free list.
   eof: returns 0 if the current position in the file is not the end of the file, returns 1 if the end of the file has been reached.
*/
 
manifest {
  SB = 0,
  DIR = 1,
  FREESTART = 2,
  
  SUPER_NAME = 0,
  SUPER_TIME = 8,
  SUPER_FIRSTFREE = 9,
  SUPER_ROOTSTART = 10,

  ENT_NAME = 0,
  ENT_HBLOCK = 8,
  ENT_TYPE = 9,
  ENT_STATUS = 10,
  ENT_SIZE = 11,
  ENT_FILEACTIVE = 0,
  ENT_FILEDELETED = 1,
  ENT_DIR = 0,
  ENT_FILE = 1,
  
  DIR_NUMENTS = 10,
  DIR_PARENT = (DIR_NUMENTS * ENT_SIZE),
  DIR_PREV = (DIR_NUMENTS * ENT_SIZE) + 1,
  DIR_NEXT = (DIR_NUMENTS * ENT_SIZE) + 2,
  DIR_BN = (DIR_NUMENTS * ENT_SIZE) + 3,
  DIR_USED = (DIR_NUMENTS * ENT_SIZE) + 4,
  DIR_NAME = (DIR_NUMENTS * ENT_SIZE) + 5,

  FILE_SIZE = 0,
  FILE_BUFF = 1,
  FILE_CURRBYTE = 2,
  FILE_HEADER = 3,
  FILE_FIRST = 4,
  FILE_SECOND = 5,
  FILE_DISC = 6,
  FILE_DIR = 7,
  FILE_DISCNUM = 8,
  FILE_MODE = 9,
  FILE_ABPOS = 10,
  FILE_LENGTH = 11,
  FILE_CLOSED = 0,
  FILE_READ = 1,
  FILE_WRITE = 2,
  
  
  DISC_NAME = 0,
  DISC_SB = 8,
  DISC_CURRDIR = 9,
  DISC_ROOT = 10,
  DISC_FREELIST = 11,
  DISC_LENGTH = 12,
  

  LAYER_POS = 128,
  LAYER_BN = 129,

  HEADER_NAME = 0,
  HEADER_SIZE = 8,
  HEADER_CURRPOS = 9,
  HEADER_TIME = 10,
  HEADER_LENGTH = 11,
  HEADER_USEDBYTES = HEADER_LENGTH*4,

  HMAX = (128-HEADER_LENGTH) * 4,
  ONEMAX = (128 - HEADER_LENGTH) * 512,
  TWOMAX = ONEMAX * 128,
  FL_MAXNUM = 5950
}
  
static {
  DISCNUM = 1,
  superblock, 
  rootdirectory
}

let disc_write(startblock, numblocks, data) be {
  let r;
  if DISCNUM < 0 then {
    out("ERROR: No disc mounted\n");
    resultis -1;
  }
  r := devctl(DC_DISC_WRITE, DISCNUM, startblock, numblocks, data);
  if r < 0 then out("Error writing disc: %d\n", r);
  resultis r;
}

let disc_read(startblock, numblocks, data) be {
  let r;
  if DISCNUM < 0 then {
    out("ERROR: No disc mounted\n");
    resultis -1;
  }
  r := devctl(DC_DISC_READ, DISCNUM, startblock, numblocks, data);
  if r < 0 then out("Error reading disc: %d\n", r);
  resultis r;
}

let tape_read(filename, buff) be {
  let r, pos = 0;

  r := devctl(DC_TAPE_LOAD, DISCNUM, filename, 'R');
  if r < 0 then
  { out("error %d for load\n", r);
    finish }

  { r := devctl(DC_TAPE_READ, DISCNUM, buff + pos);
    pos +:= r / 4;
    } repeatuntil r < 512;

  //out("done, %d words read\n", pos);
  devctl(DC_TAPE_UNLOAD, 1);
}

let save(disc) be {   
  let rootdir, currdir, super;   
  rootdir := disc ! DISC_ROOT;
  super := disc ! DISC_SB;
  currdir := disc ! DISC_CURRDIR;
  disc_write(SB,1,super);
  disc_write(DIR, 1, rootdir);
  if(currdir <> 0) then{
    disc_write(currdir ! DIR_BN, 1, currdir);
  }
  out("Saved disc %d\n", DISCNUM);
}
  
let returnblock(disc, blocknumber) be {
  let sb = disc ! DISC_SB;
  let freelist = disc ! DISC_FREELIST;
  let offset = (sb ! SUPER_FIRSTFREE) rem 128;
  let freebn = ((sb ! SUPER_FIRSTFREE) / 128) + FREESTART;
  
  if DISCNUM < 0 then {
    out("no disc mounted\n");
  }
 
  offset -:= 1;
  if offset < 0 then {
    disc_write(freebn, 1, freelist);
    freebn -:= 1;
    if freebn < FREESTART then {
    //  out("error deleting file\n");
      resultis nil;
    }
    disc_read(freebn, 1, freelist);
    offset := 127;
  }
  freelist ! offset := blocknumber;
  sb ! SUPER_FIRSTFREE -:= 1;
}

let convtodate(t) = 2000.0 #+ float(t) #/ float(365 * 24 * 60 * 60)

let format(disc_unit_number, name) be {
  let superblock = vec(128), rootdir = vec(128), result;
  let totalblocks, emptyvec = newvec(128);
  let rootname = "root";
  let listblock = vec(128);
  let index = 0;
  let block = 2;

  totalblocks := devctl(DC_DISC_CHECK, disc_unit_number);
  if totalblocks < 0 then {
    out("Error formatting disc");
    resultis nil;
  }
  DISCNUM := disc_unit_number;
  for i = 0 to 127 do {
    emptyvec ! i := 0;
  }
  for i = 0 to totalblocks - 1 do {
    disc_write(i, 1 , emptyvec);
  }
  for i = 0 to 127 do {
    superblock ! i := 0;
  }
  // WRITE SUPERBLOCK
  fixed_to_str(name, superblock + SUPER_NAME, 32);
  superblock ! SUPER_TIME := seconds();
  superblock ! SUPER_FIRSTFREE := 0;
  superblock ! SUPER_ROOTSTART := DIR;
  
  result := disc_write(SB, 1, superblock);
  if result < 0 then {
    out("Error formatting superblock\n");
    resultis nil;
  }
  // WRITE ROOTDIR
  for i = 0 to 127 do {
    rootdir ! i := 0;
  }
  strcpy(rootdir + DIR_NAME, rootname);
  rootdir ! DIR_BN := DIR;
  rootdir ! DIR_PREV := 0;
  rootdir ! DIR_NEXT := 0;
  result := disc_write(DIR, 1, rootdir);
  if result < 0 then {
    out("Error formatting root directory\n");
    resultis nil;
  }
  // CREATE FREELIST
  for count = 49 to 5999 do {
    if index >= 128 do {
      disc_write(block, 1, listblock);
      index := 0;
      block +:= 1;
    }
    listblock ! index := count;
    index +:= 1;
    if count = 5999 do {
      disc_write(block,1,listblock);
    }
  }
  
  out("Successfully formatted disk '%s'\n", name, result);
  resultis result;
}

let mount(disc_unit_number, name) be {
  let result, sbname;
  let disc = newvec(DISC_LENGTH);
  let freelist = newvec(128);
  let totalblocks = devctl(DC_DISC_CHECK, disc_unit_number);
  let currentdir = newvec(128);
  if totalblocks < 0 then {
    out("Could not find disc %d: %d\n", disc_unit_number, totalblocks);
  }
  DISCNUM := disc_unit_number;
  superblock := newvec(128);
  for i = 0 to 127 do
    superblock ! i := 0;
  sbname := newvec(32);
  result := disc_read(SB, 1, superblock);
  fixed_to_str(superblock + SUPER_NAME, sbname, 32);
  if strcmp(name, sbname) <> 0 then {
    out("Given name '%s' did not match disc name '%s'\n", name, sbname);
    resultis nil;    
  }
  // GET ROOTDIR
  rootdirectory := newvec(128);
  result := disc_read(superblock ! SUPER_ROOTSTART, 1, rootdirectory);
  if result < 0 then out("Failed to read rootdir");
 
  // END ROOTDIR 
  disc_read(((superblock ! SUPER_FIRSTFREE) / 128) + FREESTART, 1, freelist);
  disc ! DISC_SB := superblock;
  disc ! DISC_ROOT := rootdirectory;
  disc ! DISC_CURRDIR := 0;
  disc ! DISC_NAME := sbname;
  disc ! DISC_FREELIST := FREESTART;
  disc ! DISC_FREELIST := freelist;
  out("Mounted disc '%s'\n", sbname);
  freevec(sbname);
  resultis disc;
}
    
let dismount(disc) be {
  let oldnum;
  if DISCNUM < 0 then {
    out("No disc mounted\n");
  }
  oldnum := DISCNUM;
  save(disc);
  freevec(disc);
  DISCNUM := -1;
  out("Dismounted disc %d\n", oldnum);    
}


let ls(disc) be {
  let currentdir, p, counter = 0, index = 1;
  let file_name = newvec(32);
  let header = newvec(128);
  let type;
  if DISCNUM < 0 then {       
    out("No disc mounted.\n");     
    resultis nil;    
  }

  currentdir := disc ! DISC_CURRDIR;

  while currentdir <> 0 /\ currentdir ! DIR_PREV <> 0 do {
    if counter = 0 then {
      disc_write(currentdir ! DIR_BN, 1, currentdir);
    }
    counter +:= 1;
    disc_read(currentdir ! DIR_PREV, 1, disc ! DISC_CURRDIR);
    currentdir := disc ! DISC_CURRDIR;
  }
  counter := 0;

  if currentdir = 0 then {
    currentdir := disc ! DISC_ROOT;
  }
 
  p := currentdir;
  out("File #    Type    Name       Size       Modified\n");

  while p ! ENT_NAME <> 0 do {
    if p ! ENT_STATUS = ENT_FILEDELETED then {
      p +:= ENT_SIZE;
      counter +:= 1; 
      loop;      
    }    
  
    counter +:= 1;
    fixed_to_str(p + ENT_NAME, file_name, 32); 
    test p ! ENT_TYPE = ENT_FILE then {     
      disc_read(p ! ENT_HBLOCK, 1, header);
      type := "File";
      out("%2d:       %7s %10s %6d    %f\n",index,type, file_name, header ! HEADER_SIZE, convtodate(header ! HEADER_TIME));
    } else {
      type := "Dir";
      out("%2d:       %7s %10s\n", index, type, file_name)
    } 

    
    p +:= ENT_SIZE;   
    index +:= 1;
    if counter = DIR_NUMENTS then {
      test currentdir ! DIR_NEXT <> 0 then {
        disc_read(currentdir ! DIR_NEXT, 1, disc ! DISC_CURRDIR);
	currentdir := disc ! DISC_CURRDIR;
        p := currentdir;
        counter := 0;
      } else { 
        break;
      }
    } 
  }
  freevec(header);
  freevec(file_name);
}

let changedir(disc, name) be {
  let current = disc ! DISC_CURRDIR;
  let counter = 0;
  let fname = newvec(32);
  let p;
  
  while current <> 0 /\ current ! DIR_PREV <> 0 do {
    if counter = 0 then {
      disc_write(current ! DIR_BN, 1, current);
    }
    counter +:= 1;
    disc_read(current ! DIR_PREV, 1, disc ! DISC_CURRDIR);
    current := disc ! DISC_CURRDIR;
  }
   
  counter := 0;
  
  if current = 0 then {
    current := disc ! DISC_ROOT;
  }
  
  test strcmp(name, ".") = 0 then {
    freevec(fname);
    resultis nil;
  } else test strcmp(name, "..") = 0 then {
      let changed = newvec(128);
      if current ! DIR_PARENT = 0 then {
	  out("No parent directory\n");
	  freevec(fname);
	  resultis nil;
      }  
      disc_write(current ! DIR_BN, 1, current);
      disc_read(current ! DIR_PARENT, 1, changed);
      disc ! DISC_CURRDIR := changed;
      freevec(fname);
      resultis nil;
  } else {
     p := current;
   
     while p ! ENT_NAME <> 0 do {
      if p ! ENT_TYPE = ENT_FILE \/ p ! ENT_STATUS = ENT_FILEDELETED then {
        p +:= ENT_SIZE;
        counter +:= 1; 
        loop;      
      }    
	
      fixed_to_str(p + ENT_NAME, fname, 32);
      if strcmp(fname, name) = 0 then {
	let changed = newvec(128);
        disc_write(current ! DIR_BN, 1, current);
	disc_read(p ! ENT_HBLOCK, 1, changed); 
        disc ! DISC_CURRDIR := changed;
	freevec(fname);
        resultis nil;
      }
      counter +:= 1;
      p +:= ENT_SIZE;   
	
      if counter >= DIR_NUMENTS then {
        test current ! DIR_NEXT <> 0 then {
          disc_write(current ! DIR_BN, 1, current);
          disc_read(current ! DIR_NEXT, 1, disc ! DISC_CURRDIR);
          current := disc ! DISC_CURRDIR;
          p := current;
          counter := 0;
        } else { 
          break;
        }
      } 
    } 
	out("Directory not found\n");
  }
  freevec(fname);
}


let mkdir(disc, name) be {
  let sb = disc ! DISC_SB;
  let current = disc ! DISC_CURRDIR;
  let fname = newvec(32);
  let ent, count = 0;
  let offset, freeBN;
  let header = newvec(128);
  let list = disc ! DISC_FREELIST;

  if DISCNUM < 0 then {
    out("No disc mounted\n");
    resultis nil
  }
  while current <> 0 /\ current ! DIR_PREV <> 0 do {
    disc_read(current ! DIR_PREV, 1, current);
  }
  
  if current = 0 then {
    current := disc ! DISC_ROOT;
  }
  
  ent := current;
  fixed_to_str(ent + ENT_NAME, fname, 32);

  while ent ! ENT_NAME <> 0  do{
    count +:= 1;
    test count >= DIR_NUMENTS then {
      count := 0;
      test current ! DIR_NEXT <> 0 then {
        disc_write(current ! DIR_BN, 1, current);
        disc_read(current ! DIR_NEXT, 1, disc ! DISC_CURRDIR);
        current := disc ! DISC_CURRDIR;
        ent := current;
      } else {
        break;
      }
    } else {
        ent +:= ENT_SIZE;;
    }
    if(strcmp(name,fname) = 0) /\ ent ! ENT_STATUS = ENT_FILEACTIVE then{
      out("Directory name already exists\n");
      resultis nil;
    }
    fixed_to_str(ent + ENT_NAME, fname, 32);
  }
  
  count := 0;
  current := disc ! DISC_CURRDIR;
  
  test current = 0 then {
    current := disc ! DISC_ROOT;
  } else {
    while current ! DIR_PREV <> 0 do {
     if count = 0 then {
       disc_write(current ! DIR_BN, 1 , current);
     }
     count +:= 1; 
     disc_read(current ! DIR_PREV,1,current);
    }
  }

  count := 0;
  ent := current;

  while ent ! ENT_NAME <> 0 do {
      if ent ! ENT_STATUS = ENT_FILEDELETED then {
        break;
      }
      count +:= 1;
      test count >= DIR_NUMENTS then {
        count := 0;
        test current ! DIR_NEXT <> 0 then {
          disc_write(current ! DIR_BN, 1, current);
          disc_read(current ! DIR_NEXT, 1, disc ! DISC_CURRDIR);  
          current := disc ! DISC_CURRDIR;
          ent := current;
        } else {
          let newdir = newvec(128);
          for i = 0 to 127 do {
            newdir ! i := 0;
          }
		  
          offset := (sb ! SUPER_FIRSTFREE) rem 128;
          newdir ! DIR_PREV := current ! DIR_BN;
          newdir ! DIR_PARENT := current ! DIR_PARENT;
          newdir ! DIR_NEXT := 0;
          strcpy(newdir + DIR_NAME, current + DIR_NAME);
          newdir ! DIR_BN := list ! offset;
          list ! offset := 0;
          sb ! SUPER_FIRSTFREE +:= 1;
          newdir ! DIR_USED := 0;
          current ! DIR_NEXT := newdir ! DIR_BN;
          disc_write(current ! DIR_BN, 1, current);
          disc ! DISC_CURRDIR := newdir;
          current := newdir;
          ent := newdir;
          disc_write(current ! DIR_BN, 1, current);          
          if offset = 127 then {
            freeBN := ((sb ! SUPER_FIRSTFREE) / 128) + FREESTART;
            disc_read(freeBN,1,disc ! DISC_FREELIST);
            list := disc ! DISC_FREELIST;
          }                   
          break;
        }
      } else {
        ent +:= ENT_SIZE; 
      }
  }

  for i = 0 to 127 do {
    header ! i := 0;
  }

  for i = 0 to 7 do {
    ent ! i := 0;
  }
  offset := (sb ! SUPER_FIRSTFREE) rem 128;
  str_to_fixed(name, header + DIR_NAME, 32);
  header ! DIR_PARENT := current ! DIR_BN;
  header ! DIR_PREV := 0;
  header ! DIR_NEXT := 0;
  header ! DIR_BN := list ! offset;
  list ! offset := 0;
  sb ! SUPER_FIRSTFREE +:= 1;
  str_to_fixed(name, ent + ENT_NAME, 32);
  ent ! ENT_HBLOCK := header ! DIR_BN;
  ent ! ENT_TYPE := ENT_DIR;
  ent ! ENT_STATUS := ENT_FILEACTIVE;
  disc_write(current ! DIR_BN, 1, current);
  disc_write(header ! DIR_BN, 1, header);

  freevec(fname);
}


//DEBC
let create(disc, name) be {
  let sb = disc ! DISC_SB;
  let dir = disc ! DISC_CURRDIR;
  let fname = newvec(32);
  let ent, count = 0;
  let list, offset, free_BN;
  let header = newvec(128);
  let newent;
  let newdir = newvec(128);
  list:= disc ! DISC_FREELIST;
  
  if dir = 0 then {
    dir := disc ! DISC_ROOT;
  }

  if DISCNUM < 0 then {
    out("No disc mounted\n");
    resultis nil
  }
  
  if (sb ! SUPER_FIRSTFREE) > FL_MAXNUM then {
    out("Disc is full\n");
	resultis nil;
  }
  
  while dir ! DIR_PREV <> 0 do {
    disc_read(dir ! DIR_PREV, 1, dir);
  }

  ent := dir;
  fixed_to_str(ent + ENT_NAME, fname, 32);
  while ent ! ENT_NAME <> 0  do{
    count +:= 1;
    test count > DIR_NUMENTS then {
      count := 1;
      test dir ! DIR_NEXT <> 0 then {
        disc_read(dir ! DIR_NEXT, 1, disc ! DISC_CURRDIR);
        dir := disc ! DISC_CURRDIR;
        ent := dir;
      } else {
        break;
      }
    } else {
        ent +:= ENT_SIZE;;
    }

    if(strcmp(name,fname) = 0) /\ ent ! ENT_STATUS <> ENT_FILEDELETED then{
      out("file name already exists in this directory\n");
      resultis nil;
    }
    fixed_to_str(ent + ENT_NAME, fname, 32);
  }
  count := 0;
  dir := disc ! DISC_CURRDIR;
  
  test dir = 0 then {
    dir := disc ! DISC_ROOT;
  } else {
    while dir ! DIR_PREV <> 0 do {
      disc_read(dir ! DIR_PREV,1,dir);
    }
  }
  newent := dir;

  while newent ! ENT_NAME <> 0 do {
      if newent ! ENT_STATUS = ENT_FILEDELETED then {
        break;
      }
      count +:= 1;
      test count = DIR_NUMENTS then {
        count := 0;
        test dir ! DIR_NEXT <> 0 then {
          disc_write(dir ! DIR_BN, 1, dir);
          disc_read(dir ! DIR_NEXT, 1, disc ! DISC_CURRDIR);  
          dir := disc ! DISC_CURRDIR;
          newent := dir;
        } else {
          let newdir = newvec(128);
	      for i = 0 to 127 do {
	        newdir ! i := 0;
	      }
          offset := (sb ! SUPER_FIRSTFREE) rem 128;
          newdir ! DIR_PREV := dir ! DIR_BN;
          newdir ! DIR_PARENT := dir ! DIR_PARENT;
          newdir ! DIR_NEXT := 0;
          strcpy(newdir + DIR_NAME, dir + DIR_NAME);
          newdir ! DIR_BN := list ! offset;
          list ! offset := 0;
          sb ! SUPER_FIRSTFREE +:= 1;
          newdir ! DIR_USED := 0;
          dir ! DIR_NEXT := newdir ! DIR_BN;
	      disc_write(dir ! DIR_BN, 1, dir);
          disc ! DISC_CURRDIR := newdir;
          dir := newdir;
	      newent := newdir;
          if offset = 127 then {
            free_BN := ((sb ! SUPER_FIRSTFREE) / 128) + FREESTART;
            disc_read(free_BN,1,disc ! DISC_FREELIST);
            list := disc ! DISC_FREELIST;
          }                   
          break;
        }
      } else {
        newent +:= ENT_SIZE; 
      }
  }
  
  offset := (sb ! SUPER_FIRSTFREE) rem 128;
  dir ! DIR_USED +:= 1;
  for i = 0 to 7 do {
    newent ! i := 0;
  }
  str_to_fixed(name, newent + ENT_NAME, 32);
  newent ! ENT_TYPE := ENT_FILE;
  newent ! ENT_STATUS := ENT_FILEACTIVE;
  newent ! ENT_HBLOCK := list ! offset;

  sb ! SUPER_FIRSTFREE +:= 1;
  list ! offset := 0;
  offset +:= 1;
  if offset > 127 then {
    free_BN := (sb ! SUPER_FIRSTFREE) / 128;
    disc_read(free_BN,1,disc ! DISC_FREELIST);
    list := disc ! DISC_FREELIST;
    offset := 0;
  } 

  str_to_fixed(name, header + HEADER_NAME, 32);
  header ! HEADER_SIZE := 0;
  header ! HEADER_TIME := seconds();
  header ! HEADER_CURRPOS := 0;
  disc_write(newent ! ENT_HBLOCK, 1, header);
 
  out("Created file '%s'\n", name);
  
  if(disc ! DISC_CURRDIR) <> 0 then {
    disc_write((disc ! DISC_CURRDIR) ! DIR_BN, 1, disc ! DISC_CURRDIR);
  }
  freevec(fname);
  freevec(header);
}


let open(disc, file_name, direction) be {
  let newfile = newvec(FILE_LENGTH);
  let currentdir, p, sb, header = newvec(130);
  let count = 0;
  currentdir := disc ! DISC_CURRDIR;
  sb := disc ! DISC_SB;
  
  if DISCNUM < 0 then {
    out("No disc mounted\n");      
    resultis nil;
  } 

  if currentdir = 0 then {
    currentdir := disc ! DISC_ROOT;
  }

  while currentdir ! DIR_PREV <> 0 do {
    disc_read(currentdir ! DIR_PREV, 1, currentdir);
  }
  
  for i = 0 to (FILE_LENGTH - 1) do {
    newfile ! i := 0;
  }
  
  test direction = 'w' \/ direction = 'W' then {
    newfile ! FILE_MODE := FILE_WRITE;
  } else test direction = 'r' \/ direction = 'R' then {
    newfile ! FILE_MODE := FILE_READ;
  } else {  
    out("Invalid direction\n");
    resultis nil;
  }
    
  p := currentdir;
  while strcmp(p + ENT_NAME, file_name) <> 0 do {
    count +:= 1;
    p +:= ENT_SIZE;
    if count = DIR_NUMENTS then {
      test currentdir ! DIR_NEXT <> 0 then {
	disc_read(currentdir ! DIR_NEXT, 1, currentdir);
        p := currentdir;
	count := 0;
      } else {
	out("File not found in this directory\n");
	resultis nil;
      }
   } 
    if p ! ENT_NAME = 0 then {
      out("Could not find file\n");
      resultis nil;
    }
  }

  disc_read(p! ENT_HBLOCK, 1, header);
  header ! LAYER_BN := p ! ENT_HBLOCK;

  test newfile ! FILE_MODE = FILE_WRITE then {
    test header ! HEADER_SIZE < HMAX then {
	  newfile ! FILE_CURRBYTE := (header ! HEADER_SIZE) + (HEADER_USEDBYTES);
	  newfile ! FILE_DIR := disc ! DISC_CURRDIR;
	  newfile ! FILE_DISC := disc;
	  newfile ! FILE_DISCNUM := DISCNUM;
	  newfile ! FILE_HEADER := header;
	  newfile ! FILE_SIZE := header ! HEADER_SIZE;
    } else test header ! HEADER_SIZE < ONEMAX then {
      let blocknum = (header ! HEADER_SIZE) / 512;
	  let hindex = blocknum + HEADER_LENGTH;
      let firstlayer = newvec(130);
	  header ! LAYER_POS := hindex;
      disc_read(header ! hindex, 1, firstlayer);
      firstlayer ! LAYER_BN := header ! hindex;
	  firstlayer ! LAYER_POS := (header ! HEADER_SIZE) rem 512;
      newfile ! FILE_HEADER := header;
      newfile ! FILE_FIRST := firstlayer;
	  newfile ! FILE_CURRBYTE := firstlayer ! LAYER_POS;
	  newfile ! FILE_SIZE := header ! HEADER_SIZE;
    } else {
      let blocknum = (header ! HEADER_CURRPOS) / 512;
      let hindex = (blocknum / 16384) + HEADER_LENGTH;
      let firstindex = (blocknum - 16384*hindex) / 128;
      let firstlayer = newvec(130);
      let secondlayer = newvec(130);
      disc_read(header ! hindex, 1, firstlayer);
      disc_read(firstlayer ! firstindex, 1, secondlayer);
      firstlayer ! LAYER_BN := header ! hindex;
      secondlayer ! LAYER_BN := firstlayer ! firstindex;
      newfile ! FILE_HEADER := header;
      newfile ! FILE_FIRST := firstlayer;
      newfile ! FILE_SECOND:= secondlayer;
      header ! LAYER_POS := hindex;
      firstlayer ! LAYER_POS := firstindex;
      secondlayer ! LAYER_POS := (header ! HEADER_SIZE) rem 512;
      newfile ! FILE_CURRBYTE := secondlayer ! LAYER_POS;
      newfile ! FILE_SIZE := header ! HEADER_SIZE;
    }
    out("Opened file '%s'\n", file_name);
    resultis newfile;
  } else {
      test header ! HEADER_SIZE < HMAX then {
	    newfile ! FILE_CURRBYTE := HEADER_USEDBYTES;
	    newfile ! FILE_DIR := disc ! DISC_CURRDIR;
	    newfile ! FILE_DISC := disc;
	    newfile ! FILE_DISCNUM := DISCNUM;
	    newfile ! FILE_HEADER := header;
	    newfile ! FILE_ABPOS := 0;
        newfile ! FILE_SIZE := header ! HEADER_SIZE;
      } else test header ! HEADER_SIZE < ONEMAX then {
          let firstlayer = newvec(130);
          let hpos;
	      header ! LAYER_POS := HEADER_LENGTH;
          hpos := header ! LAYER_POS;
          disc_read(header ! hpos, 1, firstlayer);
          firstlayer ! LAYER_BN := header ! hpos;
	      firstlayer ! LAYER_POS := 0;
          newfile ! FILE_HEADER := header;
          newfile ! FILE_FIRST := firstlayer;
	      newfile ! FILE_CURRBYTE := 0;
	      newfile ! FILE_ABPOS := 0;
          newfile ! FILE_SIZE := header ! HEADER_SIZE;
    } else {
	  let firstlayer = newvec(130);
      let secondlayer = newvec(130);
      let hpos;
	  header ! LAYER_POS := HEADER_LENGTH;
      hpos := header ! LAYER_POS;
      disc_read(header ! hpos, 1, firstlayer);
      disc_read(firstlayer ! 0, 1, secondlayer);
      firstlayer ! LAYER_BN := header ! hpos;
      secondlayer ! LAYER_BN := firstlayer ! 0;
      newfile ! FILE_HEADER := header;
      newfile ! FILE_FIRST := firstlayer;
      newfile ! FILE_SECOND:= secondlayer;
      firstlayer ! LAYER_POS := 0;
	  secondlayer ! LAYER_POS := 0;
	  newfile ! FILE_CURRBYTE := 0;
	  newfile ! FILE_SIZE := header ! HEADER_SIZE;
      newfile ! FILE_ABPOS := 0;
    }
    out("Opened file '%s'\n", file_name);
    resultis newfile;
  }
}


let close(file) be {
 let header, first,second;
    test file ! FILE_MODE = FILE_WRITE then {
      header := file ! FILE_HEADER;
      first := file ! FILE_FIRST;
      second := file ! FILE_SECOND;
      header ! HEADER_TIME := seconds();
      disc_write(header ! LAYER_BN , 1, header);
      if second <> 0 then {
        disc_write(second ! LAYER_BN, 1, second);
		freevec(file ! FILE_SECOND);
      } 
	  if first <> 0 then {
        disc_write(first ! LAYER_BN, 1, first);
		freevec(file ! FILE_FIRST);
      } 
    } else test file ! FILE_MODE = FILE_READ then {
      
    } else {
      out("No file open\n");
      resultis nil;
    }
  file ! FILE_MODE := FILE_CLOSED; 
  freevec(file ! FILE_HEADER);
  freevec(file);
  out("Closed file\n");
}


let delete(disc, file_name) be {
  let currentdir, p, superblock;
  let header = newvec(128), first = newvec(128);
  let count = 0;
  let name = newvec(32);

  if DISCNUM < 0 then {
    out("No disc mounted\n");
    resultis nil;
  }
  
  for i = 0 to 127 do {
    header ! i :=0;
    first ! i := 0;
  }
  
  currentdir := disc ! DISC_CURRDIR;
  
  while currentdir <> 0 /\ currentdir ! DIR_PREV <> 0 do{
    disc_read(currentdir ! DIR_PREV, 1, currentdir);
  }
  
  if currentdir = 0 then {
    currentdir := disc ! DISC_ROOT;
  }

  p := currentdir;
  fixed_to_str(p+ENT_NAME, name, 32);
  while p ! ENT_STATUS = ENT_FILEDELETED \/ strcmp(name, file_name) <> 0 do {
    if p ! ENT_NAME = 0 then {
      out("Could not find file\n");
      resultis nil;
    }
    test count = DIR_NUMENTS then {
      test currentdir ! DIR_NEXT <> 0 then {
        disc_read(currentdir ! DIR_NEXT, 1, currentdir);
        p := currentdir;
        count := 0;
      } else {
        out("File not found in this directory\n");
        resultis nil;
      }
    } else {
      p +:= ENT_SIZE;
      fixed_to_str(p+ENT_NAME, name, 32);
      count +:= 1;
    }
  }

  p ! ENT_STATUS := ENT_FILEDELETED;
  
  disc_read(p ! ENT_HBLOCK, 1, header);
  superblock := disc ! DISC_SB;
  test header ! HEADER_SIZE < 400 then {
    returnblock(disc, p ! ENT_HBLOCK);
  } else test header ! HEADER_SIZE < ONEMAX then {
    let i = HEADER_LENGTH;
	while i < 128 /\ header ! i <> 0 do {
	  returnblock(disc, header ! i);
	  i +:= 1;
	}
	returnblock(disc, p ! ENT_HBLOCK);
   } else {
     let first = newvec(128);
	 let i = HEADER_LENGTH;
	 let x = 0;
         while i < 128 /\ header ! i <> 0 do {	
           disc_read(header ! i, 1, first);
           while x < 128 /\ first ! x <> 0 do {
             returnblock(disc, first ! x);
             x +:= 1;
           }
           returnblock(disc, header ! i); 
	   i +:= 1;
           x := 0;
     }
   } 
  out("Deleted file '%s'\n", file_name);
freevec(header);
freevec(first);
freevec(name);
}

let rmdir(disc, name) be{
  let currentdir, p, superblock;
  let header = newvec(128);
  let count = 0;
  let fname = newvec(32);
  let hbn;

  if DISCNUM < 0 then {
    out("No disc mounted\n");
    resultis nil;
  }
  
  for i = 0 to 127 do {
    header ! i :=0;
  }
  
  currentdir := disc ! DISC_CURRDIR;

  while currentdir  <> 0 /\ currentdir ! DIR_PREV <> 0 do{
    disc_read(currentdir ! DIR_PREV, 1, currentdir);
  }
  
  if currentdir = 0 then {
    currentdir := disc ! DISC_ROOT;
  }

  p := currentdir;
  fixed_to_str(p+ENT_NAME, fname, 32);
  while p ! ENT_STATUS = ENT_FILEDELETED \/ strcmp(name, fname) <> 0 do {
    if p ! ENT_NAME = 0 then {
      out("Directory not found\n");
      resultis nil;
    }
    test count = DIR_NUMENTS then {
      test currentdir ! DIR_NEXT <> 0 then {
        disc_read(currentdir ! DIR_NEXT, 1, currentdir);
        p := currentdir;
        count := 0;
      } else {
        out("Directory not found\n");
        resultis nil;
      }
    } else {
      p +:= ENT_SIZE;
      fixed_to_str(p+ENT_NAME, fname, 32);
      count +:= 1;
    }
  }

  p ! ENT_STATUS := ENT_FILEDELETED;
  disc_read(p ! ENT_HBLOCK, 1, header);
  p := header;
  disc ! DISC_CURRDIR := header;
  count := 0;
  hbn := header ! DIR_BN;
  
  while p ! ENT_NAME <> 0 do {
    if p ! ENT_STATUS = ENT_FILEDELETED then {
      count +:= 1;
      p +:= ENT_SIZE; 
      loop;
    }
	
	fixed_to_str(p+ENT_NAME, fname, 32);
	if p ! ENT_STATUS <> ENT_FILEDELETED /\ p ! ENT_TYPE = ENT_FILE then {
	  delete(disc, fname);
	}
	if p ! ENT_STATUS <> ENT_FILEDELETED /\ p ! ENT_TYPE = ENT_DIR then {
	  rmdir(disc, fname);
	}
	
    test count >= DIR_NUMENTS then {
      test header ! DIR_NEXT <> 0 then {
        let bn = header ! DIR_NEXT;
        returnblock(disc, header ! DIR_BN);
        disc_read(bn, 1, header);
        p := header;
        count := 0;
      } else {
        break;
      }
    } else {
      p +:= ENT_SIZE;
      count +:= 1;
    }
  }
returnblock(disc,hbn);
disc ! DISC_CURRDIR := currentdir;
freevec(header);
freevec(name);
}

let eof(file) be {
  test file ! FILE_ABPOS >= file ! FILE_HEADER ! HEADER_SIZE then {
    resultis 1;
  } else {
    resultis 0;
  }
}


let readbyte(file) be {
  let b, header,first,second;
  header := file ! FILE_HEADER;
  first := file ! FILE_FIRST;
  second := file ! FILE_SECOND;
  test file ! FILE_SIZE < HMAX then {
    if eof(file) = 1 then {
	  out("reached end of file\n");
	  resultis nil;
	}
    b := byte file ! FILE_CURRBYTE of header;
    file ! FILE_CURRBYTE +:= 1;
    file ! FILE_ABPOS +:= 1;
    out("%d\n", b);
  } else test file ! FILE_SIZE < ONEMAX then {
      if (file ! FILE_CURRBYTE > 511) then {
        let pos;
		if eof(file) = 1 then {
		  out("reached end of file\n");
          resultis nil;
		}
        header ! LAYER_POS +:= 1;
    	pos := header ! LAYER_POS;
        if pos > 127 \/ header ! pos = 0 then {
	      out("reached end of file\n");
          resultis nil;
	    }
        disc_read(header ! pos, 1, first);
	    first ! LAYER_BN := header ! pos;
	    first ! LAYER_POS := 0;
        file ! FILE_CURRBYTE := 0;
     }
     b := byte (file ! FILE_CURRBYTE) of first; 
     if eof(file) = 1 then{
        out("reached end of file\n");
        resultis nil;
     }
      file ! FILE_ABPOS +:= 1;
      file ! FILE_CURRBYTE +:= 1; 
      out("%d\n",b);
   } else {
     if(file ! FILE_CURRBYTE > 511) then {
       let fpos;
       first ! LAYER_POS +:= 1;
       fpos := first ! LAYER_POS;
       if fpos > 127 \/ first ! fpos = 0 then {
	let hpos;
        header ! LAYER_POS +:= 1;
	hpos := header ! LAYER_POS;
	if hpos > 127 \/ header ! hpos = 0 then {
	  out("reached end of file\n");
	  resultis nil;
        }
        disc_read(header ! hpos, 1, first);
	first ! LAYER_BN := header ! hpos;
	first ! LAYER_POS := 0;
       }
     disc_read(first ! fpos, 1, second);
     second ! LAYER_BN := first ! fpos;
     second ! LAYER_POS := 0;
     file ! FILE_CURRBYTE := 0;
    }
    b := byte file ! FILE_CURRBYTE of second;
    if eof(file) = 1 then {
     out("reached end of file\n");
     resultis nil;
    }
    file ! FILE_ABPOS +:= 1;
    file ! FILE_CURRBYTE +:= 1;
    out("%d\n",b);
  }
}

let writebyte(file, x) be {
  let b, header,first,second;
  let FL_offset, FL_block, freelist, sb, disc;
  header := file ! FILE_HEADER;
  first := file ! FILE_FIRST;
  second := file ! FILE_SECOND;
  disc := file ! FILE_DISC;
  sb := disc ! DISC_SB;
  freelist := disc ! DISC_FREELIST;
  FL_offset := (sb ! SUPER_FIRSTFREE) rem 128;
  FL_block := (sb ! SUPER_FIRSTFREE) / 128;
  
  if (sb ! SUPER_FIRSTFREE) = FL_MAXNUM then {
    out("Disc is full, cannot write to file\n");
	resultis nil;
  }
  
  test file ! FILE_SIZE < HMAX+1 then {
    test ( file ! FILE_CURRBYTE > 511) then {
	  let index = 0;
	  first := newvec(128);
	  for i = HEADER_USEDBYTES to 511 do {
	   byte index of first := byte i of header;
	   byte i of header := 0;
	   index +:= 1;
	  }
	  if FL_offset > 127 then {
        FL_block +:= 1;
        disc_write(FL_block - 1, 1, freelist);
        disc_read(FL_block, 1, freelist);
        FL_offset := 0;
      }
	  header ! HEADER_LENGTH := freelist ! FL_offset;
	  first ! LAYER_POS := 128 - HEADER_LENGTH;
	  first ! LAYER_BN := header ! HEADER_LENGTH;
	  freelist ! FL_offset := 0;
	  sb ! SUPER_FIRSTFREE +:= 1;
	  disc_write(header ! LAYER_BN, 1, header);
	  disc_write(first ! LAYER_BN, 1, first);
	  file ! FILE_FIRST := first;
	  file ! FILE_CURRBYTE := (first ! LAYER_POS) * 4;
	  header ! LAYER_POS := HEADER_LENGTH;
	  byte file ! FILE_CURRBYTE of first := x;
      file ! FILE_CURRBYTE +:= 1;
      header ! HEADER_CURRPOS +:= 1;
	  header ! HEADER_SIZE +:= 1;
	  file ! FILE_SIZE +:= 1;
	} else {
      byte file ! FILE_CURRBYTE of header := x;
      file ! FILE_CURRBYTE +:= 1;
      header ! HEADER_CURRPOS +:= 1;
	  header ! HEADER_SIZE +:= 1;
	  file ! FILE_SIZE +:= 1;
	}
  } else test file ! FILE_SIZE < ONEMAX + 1 then {
    if (file ! FILE_CURRBYTE > 511) then { 
      let pos;
      disc_write(first ! LAYER_BN, 1, first);
      header ! LAYER_POS +:= 1;
      pos := header ! LAYER_POS;
      test pos > 127 then {
        let index = 0;
		second := newvec(128);
		file ! FILE_SECOND := second;
	    for i = 0 to 127 do {
		  first ! i := 0;
		}
	    for i = HEADER_LENGTH to 127 do {
	      first ! index := header ! i;
		  header ! i := 0;
		  index +:= 1;
	    }
		file ! FILE_FIRST := first;
		
		if FL_offset > 127 then {
          FL_block +:= 1;
          disc_write(FL_block - 1, 1, freelist);
          disc_read(FL_block, 1, freelist);
          FL_offset := 0;
        }
		
		header ! HEADER_LENGTH := freelist ! FL_offset;
		freelist ! FL_offset := 0;
		sb ! SUPER_FIRSTFREE +:= 1;
		header ! LAYER_POS := HEADER_LENGTH;
		first ! LAYER_BN := header ! HEADER_LENGTH;
		first ! LAYER_POS := 128 - HEADER_LENGTH;
		pos := first ! LAYER_POS;
		first ! (pos + 1) := freelist ! FL_offset;
		freelist ! FL_offset := 0;
		sb ! SUPER_FIRSTFREE +:= 1;
		second ! LAYER_BN := first ! (pos + 1);
		first ! LAYER_POS +:= 1;
		file ! FILE_CURRBYTE := 0;
		byte (file ! FILE_CURRBYTE) of second := x;
        file ! FILE_CURRBYTE +:= 1;
        header ! HEADER_CURRPOS +:= 1;
	    header ! HEADER_SIZE +:= 1;
	    file ! FILE_SIZE +:= 1;
		disc_write(header ! LAYER_BN, 1, header);
		disc_write(first ! LAYER_BN, 1, first);
      } else {
	    if FL_offset > 127 then {
          FL_block +:= 1;
          disc_write(FL_block - 1, 1, freelist);
          disc_read(FL_block, 1, freelist);
          FL_offset := 0;
        }
	    header ! pos := freelist ! FL_offset;
		freelist ! FL_offset := 0;
		sb ! SUPER_FIRSTFREE +:= 1;
        for i = 0 to 127 do {
		  first ! i := 0;
		}
        first ! LAYER_BN := header ! pos;
        first ! LAYER_POS := 0;
		file ! FILE_FIRST := first;
        file ! FILE_CURRBYTE := 0;
		byte (file ! FILE_CURRBYTE) of first := x;
        file ! FILE_CURRBYTE +:= 1;
        header ! HEADER_CURRPOS +:= 1;
	    header ! HEADER_SIZE +:= 1;
	    file ! FILE_SIZE +:= 1;
     }
    }
	byte (file ! FILE_CURRBYTE) of first := x;
    file ! FILE_CURRBYTE +:= 1;
    header ! HEADER_CURRPOS +:= 1;
	header ! HEADER_SIZE +:= 1;
	file ! FILE_SIZE +:= 1;
  } else {
    if(file ! FILE_CURRBYTE > 511) then {
      let fpos;
      disc_write(second ! LAYER_BN,1, second);
      first ! LAYER_POS +:= 1;
      fpos := first ! LAYER_POS;
      if fpos > 127 \/ first ! fpos = 0 then {
        let hpos;
        header ! LAYER_POS +:= 1;
        hpos := header ! LAYER_POS;
        if hpos > 127 \/ header ! hpos = 0 then {
          out("Can not write to file, disc is full\n");
          resultis nil;
        }
		if FL_offset > 127 then {
          FL_block +:= 1;
          disc_write(FL_block - 1, 1, freelist);
          disc_read(FL_block, 1, freelist);
          FL_offset := 0;
        }
	    header ! hpos := freelist ! FL_offset;
		sb ! SUPER_FIRSTFREE +:= 1;
		freelist ! FL_offset := 0;
		FL_offset +:= 1;
        for i = 0 to 127 do {
		  first ! i := 0;
		}
        first ! LAYER_BN := header ! hpos;
        first ! LAYER_POS := 0;
      }
	  if FL_offset > 127 then {
          FL_block +:= 1;
          disc_write(FL_block - 1, 1, freelist);
          disc_read(FL_block, 1, freelist);
          FL_offset := 0;
      }
      first ! LAYER_POS := freelist ! FL_offset;
	  sb ! SUPER_FIRSTFREE +:= 1;
	  freelist ! FL_offset := 0;
	  for i = 0 to 127 do {
	    second ! i := 0;
	  }
      second ! LAYER_BN := first ! fpos;
      second ! LAYER_POS := 0;
      file ! FILE_CURRBYTE := 0;
     }
     byte file ! FILE_CURRBYTE of second := x;
     file ! FILE_CURRBYTE +:= 1;
     header ! HEADER_CURRPOS +:= 1;
	 header ! HEADER_SIZE +:= 1;
	 file ! FILE_SIZE +:= 1;
   }
}

let printbuff(file) be {
  let i;
  for i = 0 to 10 do {
    out("%2d: %d\n",i,byte i of (file ! FILE_BUFF));
  }
}

let testwriting() be {
  let f1, f2, f3;
  f1 := newvec(128);
  f2 := newvec(128);
  f3 := newvec(128);
  disc_read(50, 1, f1);
  disc_read(51, 1, f2);
  disc_read(52, 1, f3);
  for i = 0 to 511 do {
     out("%d ", byte i of f1);
  }
  out("\n");
    for i = 0 to 511 do {
     out("%d ", byte i of f2);
  }
  out("\n");
    for i = 0 to 511 do {
     out("%d ", byte i of f3);
  }

}

let executefile(file_name) be {
    let buffr = newvec(2000);
    let r, pos = 0;
    r := devctl(DC_TAPE_LOAD, DISCNUM, file_name, 'R');
    if r < 0 then
    { out("error %d while loading tape\n", r);
      resultis nil; }

    { r := devctl(DC_TAPE_READ, 1, buffr + pos);
      pos +:= r / 4;
      } repeatuntil r < 512;
    devctl(DC_TAPE_UNLOAD, DISCNUM);
    //out("First 11 words of exec: \n");
    //for i = 0 to 10 do
    //  out("   %8x\n", buff ! i);
    buffr();
}

  


