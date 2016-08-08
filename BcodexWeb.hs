{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Data.Char
import Control.Applicative ((<$>))
import Control.Monad (when)
import Text.Bcodex (codex)
import Text.Bcodex.Html (codexHtml)

import Haste
import Haste.DOM
import Haste.Events

main :: IO ()
main = withElems ["args","input","output","areaoutput","go","auto","areacheck","try"] bcodexMain

truthy :: String -> Bool
truthy "" = False
truthy (map toLower -> "false") = False
truthy _ = True

bcodexMain :: [Elem] -> IO ()
bcodexMain [argsTag, inputTag, outputTag, areaOutputTag, goTag, autoTag,
            areaCheckTag, tryTag] = do
        redisplay
        recalc
        onEvent goTag Click $ const recalc
        onEvent argsTag KeyUp $ const autorecalc
        onEvent inputTag KeyUp $ const autorecalc
        onEvent tryTag Change $ const (putTry >> autorecalc)
        onEvent areaCheckTag Change $ const (redisplay >> recalc)
        return ()
    where
        recalc = do
            argsMaybe <- getValue argsTag
            inputMaybe <- getValue inputTag
            areaCheckProp <- getProp areaCheckTag "checked"
            case (argsMaybe, inputMaybe) of
                (Just (words -> args), Just input) ->
                    if truthy areaCheckProp
                        then case codex args of
                            Left errMsg -> do
                                setClass documentBody "err" True
                                setProp areaOutputTag "value" errMsg
                            Right func -> do
                                setClass documentBody "err" False
                                setProp areaOutputTag "value" $ func input
                        else case codexHtml args of
                            Left errMsg -> do
                                setClass documentBody "err" True
                                setProp outputTag "textContent" errMsg
                            Right func -> do
                                setClass documentBody "err" False
                                setProp outputTag "innerHTML" $ func input
                _ -> return ()
        autorecalc = do
            autoProp <- getProp autoTag "checked"
            when (truthy autoProp) recalc
        putTry = do
            tryMaybe <- getValue tryTag
            case tryMaybe of
                Just val -> setProp argsTag "value" val
                _ -> return ()
        redisplay = do
            areaCheckProp <- getProp areaCheckTag "checked"
            setClass documentBody "areachecked" $ truthy areaCheckProp
