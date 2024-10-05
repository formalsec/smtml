let notify_done results url =
  let url = Webhook.url_of_string url in
  let head = Git.get_head () in
  let title = Fmt.str "Test results (commit hash=%s) :octopus:" head in
  let body = Fmt.str "```%s```" results in
  let body = Webhook.default_slack_mrkdwn title body in
  Lwt_main.run @@ Webhook.post_and_forget url body
