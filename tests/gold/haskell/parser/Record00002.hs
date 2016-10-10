module Record00002 where

instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    sendVerifyEmail email _ verurl =
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders = [("Subject", "Verify your email address")]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partHeaders = []
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
                Please confirm your email address by clicking on the link below.

                #{verurl}

                Thank you.
              |]
            }
        htmlPart = textPart
            { partType = "text/html; charset=utf-8"
            , partContent = renderHtml [shamlet|
                <p>Please confirm your email address by clicking the link below.
                <p>
                    <a href=#{verurl}>#{verurl}>
                <p>Thank you
              |]
            }
