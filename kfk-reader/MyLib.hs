{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MyLib (someFunc) where

import Control.Exception (bracket, throwIO)
import Control.Exception.Base (Exception)
import Control.Monad (forever)
import Data.Function ((&))
import Data.Kind (Type)
import Kafka.Consumer (ConsumerGroupId, ConsumerProperties, ConsumerRecord (ConsumerRecord), KafkaConsumer, KafkaError (KafkaError, KafkaResponseError), OffsetReset (Earliest), RdKafkaRespErrT (RdKafkaRespErrTimedOut), Subscription, Timeout (Timeout), TopicName (TopicName), brokersList, closeConsumer, groupId, newConsumer, offsetReset, pollMessage, topics)
import Kafka.Consumer.Types (ConsumerGroupId (ConsumerGroupId))
import Kafka.Types (BrokerAddress (BrokerAddress))

class (Exception e) => Throws (m :: Type) e r | m -> r, m -> e where
  throws :: IO m -> IO r

instance Exception e => Throws (Maybe e) e () where
  throws = (=<<) (maybe (return ()) throwIO)

instance (Exception e) => Throws (Either e r) e r where
  throws = (=<<) (either throwIO return)

class With c properties subscription | c -> properties, c -> subscription where
  with :: properties -> subscription -> (c -> IO r) -> IO r
  with properties subscriptions = bracket (open properties subscriptions) close
  open :: properties -> subscription -> IO c
  close :: c -> IO ()

instance With KafkaConsumer ConsumerProperties Subscription where
  open = ((.) . (.)) (throws @(Either KafkaError KafkaConsumer)) newConsumer
  close = throws . closeConsumer

someFunc :: IO ()
someFunc = do
  with @KafkaConsumer
    ( brokersList [BrokerAddress "localhost:9092"]
        <> groupId (ConsumerGroupId "script")
    )
    ( topics [TopicName "foo"]
        <> offsetReset Earliest
    )
    \consumer -> forever do
      pollMessage consumer (Timeout 1000)
        >>= \case
          Left (KafkaResponseError RdKafkaRespErrTimedOut) -> return ()
          Left e -> throwIO e
          Right cr@ConsumerRecord {..} -> print cr
  putStrLn "someFunc"
