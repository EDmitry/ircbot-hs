# ircbot-hs
Gitlab commit tracking IRC bot

Sample usage:

    {-# LANGUAGE OverloadedStrings #-}
    import IrcBot
    import GitLab

    main = runIrcBot BotSettings { server = "irc.freenode.org"
                                 , port = 6697
                                 , nick = "SomeBot1234"
                                 , chan = "#bottest"
                                 , chanPassword = ""
                                 , nickPassword = "nickpassword"
                                 , modules = [ModuleBox GitLabModule]
                                 }
