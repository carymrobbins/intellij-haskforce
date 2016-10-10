module Layout00005 where

getOAuthTokens = do
  let r = u $ initReq
                { method="POST"
                , requestHeaders=[]
                }
  return r
