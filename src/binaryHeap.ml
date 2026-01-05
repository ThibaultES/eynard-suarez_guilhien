open Graph

type cost = int

type 'cost heap = {
  mutable size : int;
  mutable data : (id * 'cost) array;
}

let create_heap capacity = {size = 0; data = Array.make capacity (0,max_int);}

let parent i = (i - 1) / 2
let left i = 2 * i + 1
let right i = 2 * i + 2

let swap a i j =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let rec sift_up h i =
  if i > 0 then
    let p = parent i in
    let (_, cost_i) = h.data.(i) in
    let (_, cost_p) = h.data.(p) in
    if cost_i < cost_p then begin
      swap h.data i p;
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
    swap h.data i !smallest;
    sift_down h !smallest
  end

let add_heap h (v, cost) =
  if h.size = Array.length h.data then
    failwith "Heap full";

  h.data.(h.size) <- (v, cost);
  sift_up h h.size;
  h.size <- h.size + 1

let take_min_heap h =
  if h.size = 0 then None
  else begin
    let min = h.data.(0) in
    h.size <- h.size - 1;
    h.data.(0) <- h.data.(h.size);
    sift_down h 0;
    Some min
  end


let is_empty_heap h = h.size = 0
