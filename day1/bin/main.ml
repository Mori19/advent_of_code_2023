let file = "input.txt"
let () = 
    let ic = open_in file in
    let rec get_char s = 
        match s with
    | [] -> raise (Not_found)
    | h :: _ when int_of_char h >= 48 && int_of_char h <= 57 -> h
    | _ :: t -> get_char t in

    let get_num s = 
        Printf.sprintf "%c%c" 
    (get_char (s |> String.to_seq |> List.of_seq))
    (get_char (s |> String.to_seq |> List.of_seq |> List.rev))
    |> int_of_string
    in
    let rec read_all t = 
        match input_line ic with 
        | line -> read_all (t + get_num line)
        | exception End_of_file -> close_in ic; t

    in
    print_endline (read_all 0 |> string_of_int)
