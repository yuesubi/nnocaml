
(* Neural with a list of wights and a bias *)
type neural = Neural of float list * float;;
(* Layer with a list of neurals and the number of neurals*)
type layer = Layer of neural list * int;;
(* Network with a list of layers*)
type network = Network of layer list;;


(* Misc *)
let rand_float low hight =
  (Random.float (hight -. low)) -. low
;;

let rec rand_float_list size =
  match size with
  | 0 -> []
  | _ -> (rand_float (-1.0) (1.0)) :: rand_float_list (size - 1)
;;

let rec rev_list lst =
  match lst with
  | x :: sml_lst -> rev_list sml_lst @ [x]
  | _ -> lst
;;


(* Neurals *)
let new_neural input_size =
  Neural(
    rand_float_list input_size,  (* weights *)
    rand_float (-1.0) (1.0)   (* bias *)
  )
;;

let rec neural_list input_size size =
  match size with
  | 0 -> []
  | _ -> new_neural input_size :: neural_list input_size (size - 1)
;;


(* Layers *)
let new_layer input_size layer_size =
  Layer(
    neural_list input_size layer_size,
    input_size
  )
;;

let rec layer_list config =
  match config with
  | prev_layout :: (layout :: lst) ->
    (new_layer prev_layout layout) :: (layer_list (layout :: lst))
  | _ -> []
;;


let rec create_network net_cfg =
  Network(layer_list net_cfg)
;;


(* TODO: make something to print the network to see if any function above is messed up *)
(* TODO: seperate in multiple files *)
let print_nn net =



let () = print_endline "Hello, World!";;
