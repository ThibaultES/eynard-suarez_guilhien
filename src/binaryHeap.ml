open Graph

type cost = int

type 'cost heap = {
  mutable size : int;
  mutable data : (id * 'cost) array;
  pos : (id, int) Hashtbl.t;   (* id -> index *)
}

let create_heap capacity =
  {
    size = 0;
    data = Array.make capacity (0, max_int);
    pos = Hashtbl.create capacity;
  }

let parent i = (i - 1) / 2
let left i = 2 * i + 1
let right i = 2 * i + 2

let swap h i j =
  let (vi, _) = h.data.(i) in
  let (vj, _) = h.data.(j) in
  let tmp = h.data.(i) in
  h.data.(i) <- h.data.(j);
  h.data.(j) <- tmp;
  Hashtbl.replace h.pos vi j;
  Hashtbl.replace h.pos vj i

let rec sift_up h i =
  if i > 0 then
    let p = parent i in
    let (_, cost_i) = h.data.(i) in
    let (_, cost_p) = h.data.(p) in
    if cost_i < cost_p then begin
      swap h i p;
      sift_up h p
    end

let rec sift_down h i =
  let smallest = ref i in
  let l = left i in
  let r = right i in

  if l < h.size then
    let (_, cost_l) = h.data.(l) in
    let (_, cost_s) = h.data.(!smallest) in
    if cost_l < cost_s then smallest := l;

  if r < h.size then
    let (_, cost_r) = h.data.(r) in
    let (_, cost_s) = h.data.(!smallest) in
    if cost_r < cost_s then smallest := r;

  if !smallest <> i then begin
    swap h i !smallest;
    sift_down h !smallest
  end

let add_heap h (v, cost) =
  if Hashtbl.mem h.pos v then begin
    let i = Hashtbl.find h.pos v in
    let (_, old_cost) = h.data.(i) in
    h.data.(i) <- (v, cost);
    if cost < old_cost then sift_up h i
    else sift_down h i
  end else begin
    if h.size = Array.length h.data then
      failwith "Heap full";
    h.data.(h.size) <- (v, cost);
    Hashtbl.add h.pos v h.size;
    sift_up h h.size;
    h.size <- h.size + 1
  end

let take_min_heap h =
  if h.size = 0 then None
  else begin
    let min = h.data.(0) in
    Hashtbl.remove h.pos (fst min);
    h.size <- h.size - 1;
    if h.size > 0 then begin
      h.data.(0) <- h.data.(h.size);
      Hashtbl.replace h.pos (fst h.data.(0)) 0;
      sift_down h 0
    end;
    Some min
  end


let is_empty_heap h = h.size = 0
