(*print_char(Task.char_succ 'a');;
print_newline ();;
print_string(Task.string_cons 'a' "bcd");;
print_newline ();;
print_string(Task.char_range 'a' 'd');;
print_newline ();;
print_string(Task.char_range_rev 'a' 'd');;
print_newline ();;
let res = Task.is_palindrome "ata";;
Printf.printf "%b" res;;*)


print_int (Task2.Task.last [1;2;3;4;5]);;
print_newline ();;
Task2.Task.print_list (Task2.Task.swap [1;2;3;4;5]);;
print_newline ();;
List.iter (fun x -> print_string (x)) (Task2.Task.repeat "Bonjour" 3);;
print_newline ();;
Task2.Task.print_list (Task2.Task.range_i 1 5);;
print_newline ();;
Task2.Task.print_list (Task2.Task.decr_list [1;2;3;4;5]);;
print_newline ();;
Task2.Task.print_list (Task2.Task.rev [1;2;3;4;5]);;
print_newline ();;
let res = Task2.Task.mem 6 [1;2;3;4;5];;
Printf.printf "%b" res;;
print_newline ();;
let res = Task2.Task.mem 7 [1;2;3;4;5];;
Printf.printf "%b" res;;
print_newline ();;
Task2.Task.print_list (Task2.Task.append [1;2;3;4;5] [7;8;9]);;
print_newline ();;
Task2.Task.print_list (Task2.Task.interpose 0 [7;8;9]);;
print_newline ();;
Task2.Task.print_list (Task2.Task.slutter [7;8;9]);;
print_newline ();;
Task2.Task.print_list (Task2.Task.add_list [1;2;3] [7;8;9]);;
print_newline ();;
Task2.Task.print_list (Task2.Task.remove_dup [1;2;2;3;3]);;
print_newline ();;
Task2.Task.print_list (Task2.Task.append_it [1;2;3;4;5] [7;8;9]);;
print_newline ();;
Task2.Task.print_list (Task2.Task.rev_it [1;2;3]);;
print_newline ();;
print_int (Task2.Task.max [1;2;3;4;5]);;
print_newline ();;

