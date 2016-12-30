(* #use "sat_solver.ml";; *)
open Graphics;;
open Array;;



type seed = {c : color option; x : int; y : int} ;;

type voronoi = {dim : int * int; seeds : seed array} ;;

module Variables = struct
  type t = {c :color; x : int; y : int}
  let compare t1 t2 =  compare t1 t2 
end;;

let gy = float_of_int 5;;

let taxicab x y =
  let x1 = match x with (x1,y1) -> x1 in
  let y1 = match x with (x1,y1) -> y1 in
  let x2 = match y with (x2,y2) -> x2 in
  let y2 = match y with (x2,y2) -> y2 in
  (abs(x1-x2))+(abs(y1-y2)) ;;
  
 let euclide x y =
  let x1 = match x with (x1,y1) -> x1 in
  let y1 = match x with (x1,y1) -> y1 in
  let x2 = match y with (x2,y2) -> x2 in
  let y2 = match y with (x2,y2) -> y2 in
  sqrt (float ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)));; 


let v1 = {
  dim = 200,200;
  seeds = [|
    {c=Some red; x=50; y=100};
    {c=Some green; x=100; y=50};
    {c=None; x=100; y=150};
    {c=None; x=150; y=100};
    {c=Some blue; x=100; y=100}   (*square's seed*)
  |]
}

let v2 = {
  dim = 600,600;
  seeds = [|
    {c = None; x=100; y=100};
    {c = Some red; x=125; y=550};
    {c = None; x=250; y=50};
    {c = Some blue; x=150; y=250};
    {c = None; x=250; y=300};
    {c = None; x=300; y=500};
    {c = Some red; x=400; y=100};
    {c = None; x=450; y=450};
    {c = None; x=500; y=250};
    {c = Some yellow; x=575; y=350};
    {c = Some green; x=300; y=300};
    {c = None; x=75; y=470};
  |]}

let v3 = {
  dim = 600,600;
  seeds = [|
    {c = None; x=100; y=100};
    {c = Some red; x=125; y=550};
    {c = None; x=250; y=50};
    {c = Some blue; x=150; y=250};
    {c = None; x=250; y=300};
    {c = None; x=300; y=500};
    {c = Some red; x=400; y=100};
    {c = None; x=450; y=450};
    {c = None; x=500; y=250};
    {c = None; x=575; y=350};
    {c = Some green; x=300; y=300};
    {c = None; x=75; y=470};
    {c = None; x=10; y=14};
    {c = Some red; x=122; y=55};
    {c = None; x=25; y=345};
    {c = Some blue; x=23; y=550};
    {c = None; x=25; y=30};
    {c = None; x=367; y=530};
    {c = None; x=434; y=10};
    {c = None; x=45; y=50};
    {c = None; x=50; y=25};
    {c = Some yellow; x=578; y=550};
    {c = Some green; x=30; y=350};
    {c = None; x=375; y=47};
  |]}


let v4 =  {
    dim = 800,800;
    seeds = [|
              {c = None; x=100; y=75};
              {c = None; x=125; y=225};
              {c = Some red; x=25; y=255};
              {c = None; x=60; y=305};
              {c = Some blue; x=50; y=400};
              {c = Some green; x=100; y=550};
              {c = Some green; x=150; y=25};
              {c = Some red; x=200; y=55};
              {c = None; x=200; y=200};
              {c = None; x=250; y=300};
              {c = None; x=300; y=450};
              {c = None; x=350; y=10};
              {c = None; x=357; y=75};
              {c = Some yellow; x=450; y=80};
              {c = Some blue; x=400; y=150};
              {c = None; x=550; y=350};
              {c = None; x=400; y=450};
              {c = None; x=400; y=500};
              {c = Some red; x=500; y=75};
              {c = Some green; x=600; y=100};
              {c = Some red; x=700; y=75};
              {c = None; x=578; y=175};
              {c = None; x=750; y=205};
              {c = None; x=520; y=345};
              {c = None; x=678; y=420};
              {c = None; x=600; y=480};
              {c = Some blue; x=650; y=480};
              {c = None; x=750; y=500};
              {c = None; x=600; y=550};
              {c = Some red; x=700; y=550};
            |]
    }


(*
let regions_voronoi dist v =
  let dim_x = match v.dim with (x,y)->x in
  let dim_y = match v.dim with (x,y)->y in
  let m = (make_matrix dim_x dim_y 0)  in 
  for i=0 to (dim_x-1) do
    for j=0 to (dim_y-1) do
      for a=0 to (length v.seeds -1) do
        if( (dist v.seeds.(a).x v.seeds.(a).y i j)  < (dist v.seeds.(m.(i).(j)).x v.seeds.(m.(i).(j)).y i j) )
        then 
         ( print_string "valeur d'avant : ";
          print_int (dist v.seeds.(m.(i).(j)).x v.seeds.(m.(i).(j)).y i j) ;
          print_string "valeur  : ";
          print_int (dist v.seeds.(a).x v.seeds.(a).y i j);
          print_newline();
          m.(i).(j) <- a )
         else ()
      done
    done
  done;
  m
  ;;
  *)

let regions_voronoi dist v =
  let dim_x = match v.dim with (x,y)->x in
  let dim_y = match v.dim with (x,y)->y in
  let m = (make_matrix dim_x dim_y 0)  in
  for i=0 to (dim_x-1) do
    for j=0 to (dim_y-1) do
      for a=0 to (length v.seeds -1) do
        if(dist (v.seeds.(a).x,v.seeds.(a).y) (i,j) < dist (v.seeds.(m.(i).(j)).x,v.seeds.(m.(i).(j)).y) (i,j))
        then            
          m.(i).(j) <- a          
         else ()         
      done
    done
  done;
  m
  ;;

let adjacences_voronoi v mr =
  let dim_x = match v.dim with (x,y)->x in
  let dim_y = match v.dim with (x,y)->y in
  let ma = (make_matrix (length v.seeds) (length v.seeds) false)  in
  for h=0 to (length v.seeds -1) do
    for k=0 to (length v.seeds -1) do
      for i=0 to (dim_x-1) do
        for j=0 to (dim_y-1) do
          if begin (mr.(i).(j)==h) &&
            ((not (i==0) && (mr.(i-1).(j)==k))
            || (not (j==0) && (mr.(i).(j-1)==k))
            || (not (i==(length mr -1)) && (mr.(i+1).(j)==k))
            || (not (j==(length mr -1)) && (mr.(i).(j+1)==k)))
            &&( not (k==h))
            end
            then ma.(h).(k) <-true
        done
      done
    done 
  done;
  ma
  ;;

let color_seeds v =
  let m = make (length v.seeds) None in
  for i=0 to length m -1 do
    m.(i) <- v.seeds.(i).c
  done;
  m
  ;;

let constraint_exist c = ();;

let constraints_unique c = () ;;

let constraints_adj c = ();;

let produce_constraints =
  let c = ref [] in
  constraint_exist c;
  constraints_unique c;
  constraints_adj c;
  !c
;;

let display m =
  for i=0 to (length m -1) do
    for j=0 to (length m.(0) -1) do
      print_string (string_of_bool (m.(i).(j)))
    done
  done
  ;;



let draw_voronoi v m =
  auto_synchronize false; 
  open_graph (" "^string_of_int (length m)^"x"^string_of_int (length m.(0)));
  for i=0 to (length m -1) do
    for j=0 to (length m.(0) -1) do
      if (not (i==0) && not (m.(i).(j)==m.(i-1).(j))
          || not (j==0) && not (m.(i).(j)==m.(i).(j-1))
          || not (i==(length m -1)) && not (m.(i).(j)==m.(i+1).(j))
          || not (j==(length m -1)) && not (m.(i).(j)==m.(i).(j+1))          )
      then begin
        set_color black;
        plot i j
      end
      else begin
        let color = match (v.seeds.(m.(i).(j)).c) with
          None -> white
          | Some c -> c
         in
        set_color color;        
        plot i j
      end
    done
  done;
  synchronize()
  ;;

let create_seed s color =
  let newSeed = {c = Some color; x=s.x; y=s.y} in newSeed ;;

let rec next_move v mr col = 
  let e = wait_next_event [Key_pressed; Button_down] in
  let x = e.mouse_x and y = e.mouse_y in
    if (e.button) then
      begin
        match v.seeds.(mr.(x).(y)).c with
          |None -> if ( !col <> blue || !col <> green || !col <> yellow || !col <> red ) then fill_circle 50 50 50 else 

          let newSeed= create_seed v.seeds.(mr.(x).(y)) !col in
          v.seeds.(mr.(x).(y)) <- newSeed ;
          draw_voronoi v mr
          |Some c -> col := c
      end
      else ();
    next_move v mr col
  ;;

let colorized v =
  ()
  ;;
  
draw_voronoi v2 (regions_voronoi taxicab v2);;
next_move v2 (regions_voronoi taxicab v2) (ref white) ;;


 (*draw_voronoi v2 (regions_voronoi euclide v2);; *)

