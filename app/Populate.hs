{-# LANGUAGE OverloadedStrings #-}

module Populate where

import Agda.Interaction.FindFile (SourceFile (..), rootNameModule)
import qualified Agda.Interaction.FindFile as Agda
import Agda.Interaction.Imports
import Agda.Interaction.Options
import Agda.Syntax.Abstract
import Agda.Syntax.Abstract.Views
import Agda.Syntax.Common
import qualified Agda.Syntax.Concrete as Con
import Agda.Syntax.Info
import Agda.Syntax.Internal (Dom, Type, domName, unEl)
import Agda.Syntax.Position
import Agda.Syntax.Scope.Base
import Agda.Syntax.Translation.AbstractToConcrete (abstractToConcrete_)
import Agda.Syntax.Translation.InternalToAbstract (Reify (reify))
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Monad.Base
import Agda.TypeChecking.Monad.Options
import Agda.TypeChecking.Pretty (PrettyTCM (prettyTCM))
import Agda.TypeChecking.Serialise (decodeFile)
import Agda.Utils.CallStack (SrcLoc (srcLocEndCol))
import Agda.Utils.FileName
import qualified Agda.Utils.Maybe.Strict as S
import Agda.Utils.Pretty
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (toLower)
import Data.Foldable
import Data.Generics
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.AhoCorasick.Automaton (AcMachine)
import qualified Data.Text.AhoCorasick.Automaton as Aho
import Data.Traversable
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import Database.SQLite.Simple
import Schema

setupTCM :: FilePath -> TCMT IO String
setupTCM basep = do
  absp <- liftIO $ absolute basep
  setCommandLineOptions' absp defaultOptions {optLocalInterfaces = True}
  pure (filePath absp)

findInScopeSet :: FilePath -> TCMT IO InScopeSet
findInScopeSet path = do
  source <- parseSource . SourceFile =<< liftIO (absolute path)
  cr <- typeCheckMain TypeCheck source

  let iface = crInterface cr

  setScope (iInsideScope iface)
  _scopeInScope <$> getScope

killDomainNames :: Type -> Type
killDomainNames = everywhere (mkT unDomName)
  where
    unDomName :: Dom Type -> Dom Type
    unDomName m = m {domName = Nothing}

killQual :: Con.Expr -> Con.Expr
killQual = everywhere (mkT unQual)
  where
    unQual :: Con.QName -> Con.QName
    unQual (Con.Qual _ x) = unQual x
    unQual x = x

removeImpls :: Expr -> Expr
removeImpls (Pi _ (x :| xs) e) =
  makePi (map (mapExpr removeImpls) $ filter ((/= Hidden) . getHiding) (x : xs)) (removeImpls e)
removeImpls (Fun span arg ret) =
  Fun span (removeImpls <$> arg) (removeImpls ret)
removeImpls e = e

makePi :: [TypedBinding] -> Expr -> Expr
makePi [] = id
makePi (b : bs) = Pi exprNoRange (b :| bs)

insertIdentifier :: Connection -> QName -> TCMT IO ()
insertIdentifier conn name = do
  t <- getConstInfo' name
  case t of
    Left _ -> pure ()
    Right definfo -> do
      fname <- friendlyQName name
      expr <- reify . killDomainNames $ defType definfo
      tystr <-
        fmap (render . pretty . killQual)
          . abstractToConcrete_
          . removeImpls
          $ expr
      let modname = render . pretty . qnameModule
      case rangeToIntervalWithFile (nameBindingSite (qnameName name)) of
        Just (Interval pn _) ->
          liftIO $
            execute
              conn
              "INSERT INTO identifiers (name, typestr, modname, byteoffset, fileref) VALUES (?, ?, ?, ?, ?)"
              (Text.unpack fname, tystr, modname name, posPos pn, S.toLazy (fmap (\x -> filePath x ++ ":" ++ show (posLine pn)) (srcFile pn)))
        Nothing -> pure ()

friendlyQName :: MonadIO m => QName -> TCMT m Text
friendlyQName = pure . Text.pack . render . pretty

runAgda :: FilePath -> TCMT IO a -> IO a
runAgda basep k = do
  e <- runTCMTop $ do
    setupTCM basep
    k
  case e of
    Left s -> error (show s)
    Right x -> pure x
