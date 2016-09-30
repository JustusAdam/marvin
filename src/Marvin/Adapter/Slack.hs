module Marvin.Adapter.Slack where



buildAdapter = BuildAdapter "slack.rtm" $ \cfg ->

    return $ Adapter 