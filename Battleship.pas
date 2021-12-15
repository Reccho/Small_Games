{$mode Delphi}
{$r+}
program BattleShipPvP;
uses crt, math, character, strutils, sysutils;
type
  board = array['A'..'J', 0..9] of integer;
  stats = array[1..5] of integer;
  node = record
    x: char;
    y: integer;
    next: ^node; //stack
    end;
  coord = ^node;
  stack = ^node; //stack
CONST
  SYMBOL: array[-2..5] of string = ('O', 'X', '~', 'P', 'D', 'S', 'B', 'C');
  COLORS: array[-2..5] of integer = (DarkGray, LightRed, LightBlue, LightGray, LightGray, LightGray, LightGray, LightGray);
  NAMES: array[1..5] of string = ('Patrol', 'Destroyer', 'Submarine', 'Battleship', 'Carrier');
  HEALTH: array[1..5] of integer = (2, 3, 3, 4, 5);
  GRIDlet: array[0..9] of char = ('A','B','C','D','E','F','G','H','I','J');
var
  player: array[1..2] of string;
  field, map: array[1..2] of board;
  life: array[1..2] of integer;
  fleet: array[1..2] of stats;
  mode, target: integer;
  person: integer = 1;
  comp: integer = 1;
  attStk: stack;
//STACK
procedure init(var s: stack);
 begin
  s := nil;
 end;
procedure push(var s: stack; j: char; k: integer);
 var n: ^node;
 begin
  new(n);
  n^.x := j;
  n^.y := k;
  n^.next := s;
  s := n;
 end;
procedure pop(var s: stack; out j: char; out k: integer);
 var n: ^node;
 begin
  j := s^.x;
  k := s^.y;
  n := s;
  s := s^.next;
  dispose(n);
 end;
function isEmpty(s: stack): boolean;
 begin
  isEmpty := (s = nil);
 end;
//Utility
function Yes(): boolean;
 var ano: string;
 begin
  repeat
    write('Enter ''yes'' or ''no'' : ');
    readln(ano);
  until (ano = 'yes') or (ano = 'no');
  exit(ano = 'yes');
 end;
function Read(): node;
 var spot: string;
 begin
  repeat
    readln(spot);
    if spot[1] in ['a'..'j'] then
      spot[1] := upCase(spot[1]);
    if not (spot[1] in ['A'..'J']) or not (spot[2] in ['0'..'9']) then
      writeln('Enter a location in the form of a letter A-J & a number 0-9. (Ex. ''A8'', ''G6'')');
  until (length(spot) = 2) and(spot[1] in ['A'..'J']) and (spot[2] in ['0'..'9']);

  Read.x := spot[1];
  Read.y := strToInt(spot[2]);
 end;
procedure Display(const t: board);
 var
  u: char; v: byte;

  procedure DiSpace(x: integer);
   begin
   TextColor(COLORS[x]);
   write(SYMBOL[x], ' ');
   end;
 
 begin// lil' main
  TextColor(Cyan);
    writeln('  ______________________  ');
    write(' /  ');
  TextColor(Magenta);  write('0 1 2 3 4 5 6 7 8 9');
  TextColor(Cyan);  writeln(' \');
  //TextColor(3);  writeln('|---|-|-|-|-|-|-|-|-|-|--|');
  for u := 'A' to 'J' do begin
    TextColor(Cyan);  write(' |');
    TextColor(Magenta);  write(u,' ');
    for v := 0 to 9 do
      DiSpace(t[u,v]);
    TextColor(Cyan);  writeln('|');
    end;//for u
  TextColor(Cyan);  writeln(' \______________________/');
  TextColor(LightGray);
 end;
procedure DisplayWhole(const t, f: board);
 begin
  Display(t);
  Display(f);
 end;
procedure SwitchP(x: integer; out y: integer);
 begin
  y := 3-x;
 end;
procedure Fresh();
 var
  u: char;
  v: integer;
 begin
  life[1] := 5; //Player hp
  life[2] := 5;
  
  for u := 'A' to 'J' do //grid spaces
    for v := 0 to 9 do begin
      field[1][u,v] := 0;
      field[2][u,v] := 0;
      map[1][u,v] := 0;
      map[2][u,v] := 0;
      end;//for v
  
  for v := 1 to 5 do begin //Ships' hp
    fleet[1][v] := HEALTH[v];
    fleet[2][v] := HEALTH[v];
    end;//for v
 end;
procedure Tutorial();
 begin
  writeln;
  writeln('The game is simple...');
  writeln('The goal is to sink the enemy''s ships before they sink yours.');
  writeln('Each player has 5 ships, and may place them in any configuration.');
  writeln('Ships may be place horizontally or vertically.');
  writeln('(Obviously, ships may not be out-of-bounds, overlapped, or separated)');
  writeln;
  writeln('On your turn, enter a coordinate to attack.');
  writeln('The proper format is a capital letter from ''A'' to ''J'',');
  writeln('  and a number from 0 to 9.  Ex. A7, F0, G3, etc.');
  writeln;
  writeln('Each turn only one location may be attacked,');
  writeln('  and the result will be marked for future reference.');
  writeln('That is about it, so press ''Enter'' when you are ready to begin...');
  repeat
    ReadKey;
  until ReadKey = #13; //'Enter'
 end;
//Game
procedure WelcomeSolo(out p1: string);
 begin
  writeln('Welcome, what is your name?');
  readln(p1);
  writeln('Do you know how to play?');
  
  if Yes then exit;
  Tutorial;
 end;
procedure Welcome(out p1: string; out p2: string);
 begin
  writeln('Welcome, what are your names?');
  readln(p1);
  readln(p2);
  writeln('Do you know how to play? (yes/no)');
  
  if Yes then exit;
  Tutorial;
 end;
procedure Place(var t: board);
 var
  crd1, crd2: node;
  i: integer;
  
  function PlaceCheck(): boolean;
   var j: integer;
   begin
    if (crd1.x = crd2.x) then begin //Horizontal
      if ((abs(crd1.y - crd2.y) + 1) <> HEALTH[i]) then exit(FALSE);
      for j := crd1.y to crd2.y do
        if t[crd1.x, j] <> 0 then exit(FALSE);
      end;//if horizontal
      
    if (crd1.y = crd2.y) then begin //Vertical
      if ((abs( ord(crd1.x) - ord(crd2.x) ) + 1) <> ord(HEALTH[i]) ) then exit(FALSE);
      for j := ord(crd1.x) to ord(crd2.x) do
        if t[char(j), crd1.y] <> 0 then exit(FALSE);
      end;//if vertical
    exit(TRUE);
   end;//PlaceCheck
  procedure PlaceMark();
   var j: integer;
   begin
    if (crd1.x = crd2.x) then begin //Horizontal
      if crd1.y < crd2.y then    // 0 --> 9
        for j := crd1.y to crd2.y do
          t[crd1.x, j] := i
      else // crd1^.y > crd2^.y    // 9 --> 0
        for j := crd2.y to crd1.y do
          t[crd1.x, j] := i;
      end;//horizontal
   
    if (crd1.y = crd2.y) then begin //Vertical
      if ord(crd1.x) < ord(crd2.x) then // A --> Z
        for j := ord(crd1.x) to ord(crd2.x) do
          t[char(j), crd1.y] := i
      else // ord(crd1^.x) > ord(crd2^.x) // Z --> A
        for j := ord(crd2.x) to ord(crd1.x) do
          t[char(j), crd1.y] := i;
      end;//vertical
   end;//PlaceMark
 
 begin// lil' main
  i := 5;
  while i > 0 do begin
    ClrScr;
    Display(t);
    TextColor(LightGray);
    repeat //Coordinate Entry/Checking
      writeln(player[person], ', enter 2 coordinates for the ends of your ', NAMES[i], '.');
      writeln('(The ', NAMES[i], ' covers ', HEALTH[i], ' tiles.)');
      repeat
        crd1 := Read();
        crd2 := Read();
      until (crd1.x = crd2.x) or (crd1.y = crd2.y);
      if not PlaceCheck then begin
        writeln('Invalid Placement.');
        writeln('(Ship placements may not overlap, and must be correct length.)');
        writeln;
        end;//if not PlaceCheck
    until PlaceCheck;
    PlaceMark; //Board Marking
    ClrScr;
    i := i - 1;
  end;//while
  TextColor(LightGray);
 end;
procedure PlaceComp(var t: board);
 var
  u, v, i, j: integer;
  vert: boolean;
  
  function ScanComp(x, y, c: integer): boolean;
   var k: integer;
   begin
    for k := 1 to HEALTH[c] do begin
      if t[GRIDlet[x], y] <> 0 then exit(FALSE);
      if vert = FALSE then y := y + 1
      else x := x + 1;
      end;//for k
    exit(TRUE);
   end;//ScanComp
 
 begin// lil' main
  randomize;
  i := 5;
  for i := 5 downto 1 do begin
    vert := boolean(random(2));
    repeat
      u := random(10-i);
      v := random(10-i);
    until ScanComp(u, v, i); //and (t[GRIDlet[u], v] = 0)
  
    for j := 1 to HEALTH[i] do begin
      t[GRIDlet[u], v] := i;
      if vert = FALSE then v := v + 1
      else u := u + 1;
      end;//for j
  end;//for i
 end;//SetComp
function Turn(p: integer): boolean;
 var
  crd: node;
  x: integer;
  sink: boolean = FALSE;
  opp: integer;
 begin
  opp := 3-p;
  DisplayWhole(map[p], field[p]);
   TextColor(LightGray);
   writeln('It is now ', player[p], '''s turn.');
   writeln('On which enemy space will you fire?');
  repeat
    crd := Read();
    x := field[opp][crd.x, crd.y];
    if x < 0 then writeln('This location has already been fired upon');
  until x >= 0;
  ClrScr;

  if x = 0 then begin //Miss
    writeln('Your torpedo found no target...');
    field[opp][crd.x, crd.y] := -2; //mark enemy board
    map[p][crd.x, crd.y] := -2; //mark player map
    end //if x = 0
  else begin //HIT
    writeln('You''ve scored a hit!');
    field[opp][crd.x, crd.y] := -1; //mark enemy board
    map[p][crd.x, crd.y] := -1; //mark player map
    fleet[opp][x] := fleet[opp][x] - 1; //Lower hit ship's hp
    if fleet[opp][x] = 0 then sink := TRUE;
    end;//else
  
  if sink then writeln('You sank the enemy ', NAMES[x], '!');
  DisplayWhole(map[p], field[p]);
  Delay(1500);
  ClrScr;
  exit(sink);
  //if sink then exit(TRUE);
  //exit(FALSE);
  //&nbs p; exit(sink); //suggestion
 end;//Turn
function TurnComp(d: integer): boolean;
 var
  u: char;
  v, x: integer;
  
  procedure RandomAtt();
   begin
    randomize;
     repeat
       u := GRIDlet[random(10)];
       v := random(10);
     until field[1][u, v] >= 0;
     x := field[1][u, v];
   end;//RandomAtt
  procedure PlanAtt();
   var row: integer;
   begin
    if ((v+1) in [0..9]) and (field[1][u, v+1] >= 0)then
      push(attStk, u, v+1);
    if ((v-1) in [0..9]) and (field[1][u, v-1] >= 0) then
      push(attStk, u, v-1);
    row := ord(u);
    if (char(row+1) in ['A'..'J']) and (field[1][char(row+1), v] >= 0) then
      push(attStk, char(row+1), v);
    if (char(row-1) in ['A'..'J']) and (field[1][char(row-1), v] >= 0) then
      push(attStk, char(row-1), v);
   end;//PlanAtt
 
 begin// lil' main
  Delay(1500);
  ClrScr;
  
  if d > 0 then begin //Focused Attack
    if not isEmpty(attStk) then begin
      pop(attStk, u, v);
      x := field[1][u, v];
      end
    else RandomAtt;
    end//if
  else RandomAtt;
  
  if x = 0 then field[1][u, v] := -2
  else begin
    if target = 0 then target := x;
    field[1][u,v] := -1;
    fleet[1][x] := fleet[1][x] - 1;
    PlanAtt;
    
    if fleet[1][x] = 0 then begin
      DisplayWhole(map[1], field[1]);
      writeln('Your ', NAMES[x], ' has been sunk!');
      Delay(1000);
      if target = x then target := 0;
      end;
    exit(fleet[1][x] = 0); //Sunk
    end;//else

  exit(FALSE);
 end;
procedure OnePlayer();
 begin
 //Setup
  WelcomeSolo(player[1]);
  Fresh;
  Place(field[1]);
  PlaceComp(field[2]);
 //Game
  while (life[1] > 0) and (life[2] > 0) do begin
    if comp = 2 then
      if Turn(1) then life[2] := life[2] - 1
      else if TurnComp(target) then life[1] := life[1] - 1;
    Delay(500);
    ClrScr;
    comp := 3 - person;
    end;//while
 //Game End
  DisplayWhole(map[1], field[1]);
  TextColor(LightGray);
  if life[1] = 0 then writeln('The computer has bested you!')
  else writeln('You have bested the computer!');
 end;
procedure TwoPlayer();
 begin
 //Setup
  Welcome(player[1], player[2]);
  writeln('Players will now set their boards...');
  Delay(2000);
  Fresh; //(Re)Sets the board
  Place(field[person]); //field[1]
  SwitchP(person, person); //now Player 2
  Place(field[person]); //field[2]
  SwitchP(person, person); //now Player 1
 //Game
  while (life[1] > 0) and (life[2] > 0) do begin
    if Turn(person) then life[3-person] := life[3-person] - 1;
      
    TextColor(LightGray);
    writeln('Pass the game...');
    Delay(3000);
    ClrScr;
    SwitchP(person, person);
    end;
 //Game Result
  TextColor(LightGray);
  if life[person] = 0 then writeln(player[3-person], ' is victorious!')
  else writeln(player[person], ' is victorious!');
 end;

// MAIN \\
begin
  repeat
    writeln('Enter the number of players... (1 or 2).');
    repeat
      readln(mode);
    until (mode = 1) or (mode = 2);
  
    case (mode) of
      1: OnePlayer();
      2: TwoPlayer();
     end;//CASE (mode) OF
  
    writeln('Play again? (yes/no)');
  until not Yes;
  writeln('Goodbye.');

end.

{...
 DISPLAY
   ______________________
  /  0 1 2 3 4 5 6 7 8 9 \
  |A ~ S ~ ~ ~ ~ ~ ~ ~ ~ |
  |B ~ S ~ ~ ~ ~ ~ ~ ~ ~ |
  |C ~ S ~ ~ ~ ~ ~ D ~ ~ |
  |D ~ ~ ~ ~ ~ ~ ~ D C ~ |
  |E ~ P P ~ ~ ~ ~ D C ~ |
  |F ~ ~ ~ ~ ~ ~ ~ ~ C ~ |
  |G ~ B B B B ~ ~ ~ C ~ |
  |H ~ ~ ~ ~ ~ ~ ~ ~ C ~ |
  |I ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ |
  |J ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ |
  \______________________/
 OR
    _____________________
   / 1 2 3 4 5 6 7 8 9 0 \
   |+++++++++++++++++++++|
 A | ~ ~ ~ ~ ~ ~ 0 X X 0 |
 B | ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ |
 C | ~ ~ ~ 0 ~ ~ ~ ~ ~ ~ |
 D | ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ |
 E | ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ |
 F | ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ |
 G | ~ ~ ~ ~ ~ 0 ~ ~ ~ ~ |
 H | ~ C C C C X ~ ~ ~ ~ |
 I | ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ |
 J | ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ |
   \_____________________/

 ...
 Colors
  Black = 0;
  Blue = 1;
  Green = 2;
  Cyan = 3;
  Red = 4;
  Magenta = 5;
  Brown = 6;
  LightGray = 7;
  DarkGray = 8;
  LightBlue = 9;
  LightGreen = 10;
  LightCyan = 11;
  LightRed = 12;
  LightMagenta = 13;
  Yellow = 14;
  White = 15;
  Blink = 128;
}
