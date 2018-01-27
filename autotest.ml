open Automachine
open QCheck

let initf a = Gen.(int >>= fun i -> a >>= fun a -> return (fun () -> i,a) )
let node s a =
  let open Gen in
  int                                  >>= fun index ->
  frequencyl [1,Inital;2,Node;3,Final] >>= fun nodetype ->
  s                                    >>= fun (s:'s) -> 
  a                                    >>= fun (a:'a) -> 
  int                                  >>= fun nindex ->
  return (fun (_:'a) -> s)             >>= fun statf ->
  return (fun (_:'s) -> nindex, a)     >>= fun argf ->
  return {
    index = index;
    nodetype = nodetype;
    action = statf;
    trans_fun = argf;
    comment = ""
  }
let auto s a =
  let open Gen in
  initf a >>= fun inf -> list (node s a) >>= fun ns -> return (create inf ns)
let stat = Gen.unit
let arg = Gen.unit

let () =
  let tests = [Test.make ~name:"autotest" ~count:1000 (make (auto stat arg)) (fun a -> try run a; true with Unknown_node -> true)] in
  exit (QCheck_runner.run_tests ~colors:true ~verbose:true tests)

