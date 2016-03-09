let gensym_counter = ref 0

let make_name base =
  gensym_counter := 1 + !gensym_counter;
  base ^ (string_of_int !gensym_counter)
