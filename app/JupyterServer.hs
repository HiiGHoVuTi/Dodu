{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module JupyterServer (
  requestHandler  
) where


import Interpreter (eval, showVal, builtinNames, Scope)
import Parser (parseCell)

-- Imports from 'base'
import Control.Concurrent (MVar, modifyMVar)
import Data.List (nub)
import Data.Map (fromList)

-- Imports from 'text'
import Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'jupyter'
import Jupyter.Kernel (defaultClientRequestHandler, KernelProfile, KernelCallbacks(..))
import Jupyter.Messages (ClientRequest(..), KernelReply(..), KernelInfo(..),
                         LanguageInfo(..), HelpLink(..), CodeBlock(..), CodeOffset(..),
                         ExecutionCount, KernelOutput(..), ErrorInfo(..), displayPlain,
                         CompletionMatch(..), CursorRange(..),
                         pattern CompleteOk, pattern ExecuteOk, pattern InspectOk,
                         pattern ExecuteError)

requestHandler :: KernelProfile -> MVar ExecutionCount -> MVar Scope -> KernelCallbacks -> ClientRequest -> IO KernelReply
requestHandler profile execCountVar scopeVar callbacks req =
  case req of
    ExecuteRequest code _ ->
      -- For this simple kernel, ignore the execution options, as they do not apply
      -- to our simple kernel. Also, automatically increment the execution counter.
      modifyMVar execCountVar $ \execCount -> do
        -- Kernels are responsible for broadcasting any execution request code blocks they receive
        -- to all connected frontends via kernel outputs.
        sendKernelOutput callbacks $ ExecuteInputOutput execCount code

        reply <- handleExecuteRequest execCount scopeVar code callbacks
        return (execCount + 1, reply)
    InspectRequest code offset _ ->
      -- Ignore the detail level, because for this simple kernel we don't have
      -- documentation of identifiers at multiple detail levels.
      handleInspectRequest code offset
    CompleteRequest code offset -> handleCompleteRequest code offset
    other ->
      -- Any unhandled messages can be handled in the default manner.
      defaultClientRequestHandler profile kernelInfo callbacks other
  where
    -- This KernelInfo is returned by the default client request handler when it receives a
    -- KernelInfoRequest message, which is usually the first message that the client sends to the
    -- kernel.
    kernelInfo = KernelInfo
      { kernelProtocolVersion = "5.0"
      , kernelBanner = "Welcome to the Dodu Playground!"
      , kernelImplementation = "Dodu-Kernel"
      , kernelImplementationVersion = "1.0"
      , kernelHelpLinks = [ HelpLink "Dodu docs"
                              "https://github.com/HiiGHoVuTi/Dodu"
                          ]
      , kernelLanguageInfo = LanguageInfo
        { languageName = "Dodu"
        , languageVersion = "0.1.0.0"
        , languageMimetype = "text/plain"
        , languageFileExtension = ".du"
        , languagePygmentsLexer = Nothing
        , languageCodeMirrorMode = Nothing
        , languageNbconvertExporter = Nothing
        }
      }

handleInspectRequest :: CodeBlock -> CodeOffset -> IO KernelReply
handleInspectRequest (CodeBlock code) (CodeOffset offset) =
  let _token = findPreceedingToken code offset
  in pure . InspectReply . InspectOk $ Nothing

handleExecuteRequest :: ExecutionCount -> MVar Scope -> CodeBlock -> KernelCallbacks -> IO KernelReply
handleExecuteRequest execCount scopeVar (CodeBlock code) KernelCallbacks { .. } =
    case parseCell (show execCount) $ T.unpack code of
      Left err -> reply $ ExecuteError ErrorInfo 
        { errorName = "Parse Error"
        , errorValue = T.pack $ show err
        , errorTraceback = []
        }
      Right (program, expression) -> do
        modifyMVar scopeVar $ \scope -> do
          case mapM (eval scope) (fromList program) of
            Left err -> (scope, ) <$> reply (ExecuteError ErrorInfo
              { errorName = "Runtime Error"
              , errorValue = err
              , errorTraceback = []
              })
            Right newScope -> 
              case eval newScope expression of
                Left err -> (newScope, ) <$> reply (ExecuteError ErrorInfo
                  { errorName = "Runtime Error"
                  , errorValue = err
                  , errorTraceback = []
                  })
                Right val -> (newScope, ) <$> do 
                  sendKernelOutput
                    $ DisplayDataOutput $ displayPlain $ T.pack $ showVal 16 val
                  reply ExecuteOk
  where
    reply = pure . ExecuteReply execCount

findPreceedingToken :: Text -> Int -> Text
findPreceedingToken code offset =
  let beforeCursor = T.take offset code
      allowedSymbolChars = nub $ T.unpack $ T.concat builtinNames
      token = T.takeWhileEnd (`elem` allowedSymbolChars) beforeCursor
  in token

handleCompleteRequest :: CodeBlock -> CodeOffset -> IO KernelReply
handleCompleteRequest (CodeBlock code) (CodeOffset offset) =
  let token = findPreceedingToken code offset
      start = offset - T.length token
      completions = filter (T.isPrefixOf token) builtinNames
  in return $ CompleteReply $
    CompleteOk (map CompletionMatch completions) (CursorRange start offset) mempty
