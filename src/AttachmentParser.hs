{-# LANGUAGE FlexibleContexts #-}


module AttachmentParser where

import           Text.Regex.PCRE


import           Data.Conduit
import qualified Data.Conduit.Binary    as CB
import qualified Data.Conduit.List      as CL

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BC
import           Data.ByteString.Lazy   (toStrict)

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Char              (ord)
import           Data.String.Utils      (replace)
import qualified System.IO              as IO

isAttachement :: B.ByteString -> Bool
isAttachement line = line =~ "Content-Disposition:\\s*attachment;|filename(\\*[0-9]+\\*?)?="

isFromSender :: B.ByteString -> Bool
isFromSender line = line =~ "Return-Path\\s*:"

isBase64 :: B.ByteString -> Bool
isBase64 line = line =~ "^[/=+A-Za-z0-9]+$"


-- Controle l'execution du parser
-- elimine toutes les lignes jusqu'à trouver une qui match "condition" puis concataine les lignes
-- suivantes qui match $ Retourne B.empty si EOF
readUntil ::  MonadIO m =>
              (B.ByteString -> Bool) -> Sink B.ByteString m B.ByteString
readUntil condition = skipUntilMatch condition >> takeWhileMatch condition

    where
      takeWhileMatch doesMatch = do
        line <- toStrict <$> (CB.takeWhile (/= fromIntegral (ord '\r')) =$= CB.sinkLbs)

        if not (doesMatch line)
            then return B.empty
            else do
                _ <- CB.drop 2
                line' <- takeWhileMatch doesMatch
                return $ B.append line line'

      skipUntilMatch doesMatch = do
         c <- CL.peek
         case c of
           Nothing -> return ()
           _ -> do
               line <- toStrict <$> (CB.takeWhile (/= fromIntegral (ord '\r')) =$= CB.sinkLbs)
               if doesMatch line
                 then leftover line
                 else CB.drop 2 >> skipUntilMatch doesMatch

-- TODO : betting handling of filename encoding
-- Extract the filename of the attachment
extractFilename :: B.ByteString -> B.ByteString
extractFilename str = escapeChars $ if isEncoded str
                                       then if isEncodedPrintable str
                                            then BC.pack . replaceEncodedChars . BC.unpack $ getFilename rEncodedBytes --Use some mixed of ascii and hexa
                                            else B64.decodeLenient $ getFilename rEncodedBytes --Base64 yeahh !!!
                                       else getFilename rASCIIBytes
    where
        isEncoded line = line =~ "filename(\\*[0-9]+)?=[\r\n\t ]*\"=\\?[^\\?]+\\?[BQ]\\?"
        isEncodedPrintable line = line =~ "filename(\\*[0-9]+\\*?)?=[\r\n\t ]*\"=\\?[^\\?]+\\?Q\\?"
        rEncodedBytes = "=\\?[^\\?]+\\?[BQ]\\?([^\\?]+)\\?="
        rASCIIBytes = "filename\\*?[0-9]*=[\r\n\t ]*\"?([^\";]+)\"?;?"
        getFilename regex =  foldl (\x y -> x `B.append` (y !! 1)) B.empty
                                   ((\m -> getAllTextSubmatches $ m =~ regex :: [B.ByteString])
                                   <$> (getAllTextMatches $ str =~ regex :: [B.ByteString]))


        -- replaceEncodedChars :: String -> String
        replaceEncodedChars line = do
                                  let m = line =~ "=[0-9A-Za-z]{2}" :: String
                                  if null m
                                      then line
                                      else replaceEncodedChars $ replace m "_" line

        escapeChars = BC.map (\x -> if x `elem` " ',*$&\\~#|²£%µ:;!§?<>=`^+" then '_' else x )


-- Parse an attachment file -- If filename empty it's the end
parseAttachement ::  ResumableSource IO B.ByteString -> IO (ResumableSource IO B.ByteString, BC.ByteString, B.ByteString)
parseAttachement r = do

    (res1, attachmentStr) <- r $$++ readUntil isAttachement
    (res2, attachmentBody) <- res1 $$++ readUntil isBase64

    -- Extracting absolute path where to store the attachment
    let filename = BC.unpack $ extractFilename attachmentStr
    let file = either (const BC.empty) id (B64.decode attachmentBody)

    return (res2, file, BC.pack filename)




--
run :: IO ()
run = do
    let res = newResumableSource (CB.sourceHandle IO.stdin)
    go res

    where
      go res = do
        (res2, file, str) <- parseAttachement res
        unless (BC.null str) $ do
            print str
            B.writeFile (BC.unpack str) file
            go res2


