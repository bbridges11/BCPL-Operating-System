
import "io"
 
export {
 strcpy, strcat, strdup, strcmp,
 ins, str_to_fixed, fixed_to_str }

let strcpy(d, s) be {
  let i;
  let len = strlen(s);
  for i = 0 to len-1 do {
    byte i of d := byte i of s }
  byte len of d := 0;
  resultis d
}

let strcat(d, s) be {
  let len = strlen(d);
  let newlen = strlen(s);
  for i = 0 to newlen-1 do {
    byte len + i of d := byte i of s }
  byte len + newlen of d := 0;
  resultis d
}

let strdup(s) be {
  let d = newvec(10);
  let len = strlen(s);
  for i = 0 to len-1 do {
    byte i of d := byte i of s }
  resultis d;
}

let strcmp(a,b) be {
  let i;
  let len1 = strlen(a);
  let len2 = strlen(b);
  let len;

  test len1 >= len2 then {
    len := len1 }
  else {
    len := len2 }
  
  for i = 0 to len1 do {
    test byte i of a > byte i of b then {
      resultis 1 }
    else test byte i of a < byte i of b then {
      resultis -1 }
    else { }
  }
  resultis 0;
}

let ins(ptr, sz) be {
  let max = sz*4-1, len = 0;
  while true do {
    let c = inch();
    if c = '\n' then break;
    if len < max then
    { byte len of ptr := c;
      len +:= 1 }
  }
  byte len of ptr := 0;
  resultis ptr
}

let fixed_to_str(src, dst, size) be {
  let len = 0;
  while len<size do {
    let c = byte len of src;
    if c = 0 then break;
    byte len of dst := c;
    len +:= 1 }
  byte len of dst := 0
}

let str_to_fixed(s, a, size) be {
  let i,l;
  let len = strlen(s);
  test len < size*4 then {
    l := len;
  } else {
    l := size*4;
  }
  for i=0 to l-1 do {
    byte i of a := byte i of s
  }
  resultis a
}

/*let start() be {
  let a;
  let d = vec(10);
  let catd = vec(10);
  let t = "abcde";
  let heap = vec(1000);
  let b;
  let fa = vec 10;
  let fs, fsi = 10;
  let sa = vec 4;
  let ss , ssi = 4;

out("Testing...\n\n");

  strcpy(d, "abcde");
  out("StrCpy: %s\n", d);

  strcat(d,"fghi");
  out("StrCat: %s\n", d);

  init(heap, 18);
  b := strdup(d);
  out("StrDup: %s\n", b);

  strcpy(b,"abcd");
  out("StrCmp: %s %s %d\n",b,d,strcmp(b,d));
  out("StrCmp: %s %s %d\n",d,b,strcmp(d,b));

  a := newvec(4);
  //ins(a, 4);
  out("Ins: %s\n", a);

  fa ! 0 := 'ab';
  fa ! 1 := 'cd';
  fa ! 2 := 'efgh';
  fs := a;
  fixed_to_str(fa,fsi,fs);
  out("FixedToS: %s\n",fs);

  ss := newvec(4);
  strcpy(ss, "teststring");
  str_to_fixed(ss,sa,ssi);
  out("StrToFixed: %c%c%c%c\n", byte 6 of sa, byte 7 of sa, byte 8 of
sa, byte 9 of sa)
}*/