open Cohttp
open Cohttp_lwt_unix

let headers =
  let headers = Header.init () in
  Header.add_list headers
    [ ("Content-type", "application/json"); ("User-Agent", "Ecmabot/1.0") ]

let default_slack_mrkdwn title body =
  `Assoc
    [ ( "blocks"
      , `List
          [ `Assoc
              [ ("type", `String "header")
              ; ( "text"
                , `Assoc
                    [ ("type", `String "plain_text")
                    ; ("text", `String title)
                    ; ("emoji", `Bool true)
                    ] )
              ]
          ; `Assoc
              [ ("type", `String "section")
              ; ( "text"
                , `Assoc [ ("type", `String "mrkdwn"); ("text", `String body) ]
                )
              ]
          ] )
    ]

let url_of_string str = Uri.of_string str

let post url body =
  let body = Cohttp_lwt.Body.of_string (Yojson.to_string body) in
  Client.post ~body ~headers url

(** Sends POST and forgets about it *)
let post_and_forget url body =
  let open Lwt.Syntax in
  let+ _ = post url body in
  ()
