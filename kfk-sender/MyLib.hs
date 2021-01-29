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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module MyLib (someFunc) where

import Control.Exception (bracket, throwIO)
import Control.Exception.Base (Exception)
import Data.Function ((&))
import Data.Kind (Type)
import Kafka.Producer
  ( BrokerAddress,
    KafkaError,
    KafkaProducer,
    ProducePartition (UnassignedPartition),
    ProducerProperties,
    ProducerRecord (prKey, prPartition, prTopic, prValue),
    brokersList,
    closeProducer,
    newProducer,
    produceMessage,
  )
import qualified Kafka.Producer as Kfk
import Kafka.Types (BrokerAddress (BrokerAddress))

class (Exception e) => Throws (m :: Type) e r | m -> r, m -> e where
  throws :: IO m -> IO r

instance Exception e => Throws (Maybe e) e () where
  throws = (=<<) (maybe (return ()) throwIO)

instance (Exception e) => Throws (Either e r) e r where
  throws = (=<<) (either throwIO return)

class With p properties | p -> properties where
  with :: properties -> (p -> IO r) -> IO r
  with properties = bracket (open properties) close
  open :: properties -> IO p
  close :: p -> IO ()

instance With KafkaProducer ProducerProperties where
  open properties = throws @(Either KafkaError KafkaProducer) $ newProducer properties
  close = closeProducer

someFunc :: IO ()
someFunc = do
  with (brokersList [BrokerAddress "localhost:9092"]) \producer -> do
    throws $
      produceMessage
        producer
        Kfk.ProducerRecord
          { prTopic = "foo",
            prPartition = UnassignedPartition,
            prKey = Nothing,
            prValue = Just "Hello, People!"
          }
  putStrLn "someFunc"
