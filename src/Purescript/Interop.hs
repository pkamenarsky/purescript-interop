{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -ddump-splices #-}

module Purescript.Interop where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Exception
import Control.Monad

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import qualified Data.ByteString.Lazy as B
import Data.Hashable
import Data.List
import Data.Maybe

import Debug.Trace

instance Lift Type where
  lift (ConT n) = [| ConT (mkName nstr) |] where nstr = show n
  lift (AppT a b) = [| AppT a b |]
  lift (TupleT x) = [| TupleT x |]
  lift (ListT) = [| ListT |]

--------------------------------------------------------------------------------

-- name, args, hash
type API = [(String, [Type], String)]

type SerAPI = [(String, String)]

class Export a where
  export :: a -> String

emptyApi :: SerAPI
emptyApi = []

parseCall :: Value -> Parser (String, String, Value)
parseCall (Object o) = do
  call    <- o .: "call"
  version <- o .: "version"
  args    <- o .: "args"
  return (call, version, args)
parseCall _ = fail "Could not parse RPC call"

mkExports :: Maybe (String, String, FilePath) -> [(Name, Bool)] -> Q [Dec]
mkExports out ts = do
  exports <- forM ts $ \(t, json) -> do
    TyConI dec <- reify t
    return $ mkExport dec
          ++ if json
               then "\n\n" ++ mkToJson dec ++ "\n\n" ++ mkFromJson dec
               else ""

  let exports' = commonPurescriptImports ++ intercalate "\n\n" exports

  exportsDec <- valD (varP $ mkName "rpcExports")
                     (normalB $ litE $ stringL exports')
                     []

  case out of
    Just (header, footer, path) -> runIO $ writeFile path (header ++ exports' ++ footer)
    Nothing -> return ()

  return [exportsDec]

    where
      mkExport (NewtypeD _ n tyvars con _)
        = "newtype " ++ nameBase n ++ " " ++ intercalate " " (map mkTyVar tyvars) ++ " = "
        ++ mkCon con
      mkExport (DataD _ n tyvars cons _)
        = "data " ++ nameBase n ++ " " ++ intercalate " " (map mkTyVar tyvars) ++ " = "
        ++ intercalate "|\n" (map mkCon cons)
      mkExport (TySynD n tyvars t)
        = "type " ++ nameBase n ++ " " ++ intercalate " " (map mkTyVar tyvars) ++ " = "
        ++ mkType t

      mkCon (RecC n vars) = nameBase n ++ " {\n" ++ intercalate ",\n" (map mkVar vars) ++ "\n}"
      mkCon (NormalC n vars) = nameBase n ++ " " ++ intercalate " " (map mkVar' vars) ++ "\n"

      mkVar (n, _, t) = "  " ++ nameBase n ++ " :: " ++ mkType t
      mkVar' (_, t) = mkType t

      mkType (ConT n) | nameBase n == "Set" = "Array"
                      | nameBase n == "Bool" = "Boolean"
      mkType (ConT n) = nameBase n
      mkType (VarT a) = takeWhile (/= '_') $ nameBase a
      mkType (AppT f x) = "(" ++ mkType f ++ " " ++ mkType x ++ ")"
      mkType (TupleT 0) = "Unit "
      mkType (TupleT 2) = "Tuple "
      mkType (TupleT n) = "Tuple" ++ show n ++ " "
      mkType ListT = "Array "

      mkTyVar (PlainTV n) = nameBase n
      mkTyVar (KindedTV n _) = nameBase n

      -- ToJSON deriving

      firstToLower :: String -> String
      firstToLower [] = []
      firstToLower (x:xs) = toLower x:xs

      -- TODO: add ToJSON constraints
      mkToJson (NewtypeD _ n tyvars con _) = concat
        [ "instance " ++ firstToLower (nameBase n) ++ "ToJson :: "
           ++ mkConstraints "ToJSON" tyvars
           ++ " ToJSON (" ++ nameBase n ++ " "
           ++ intercalate " " (map mkTyVar tyvars) ++ ") where\n"
        , case con of
            NormalC _ _ -> "  toJSON (" ++ conToName con ++ " x) = toJSON x"
            RecC _ _ -> mkConToJson con
        ]
      mkToJson (DataD _ n tyvars cons _) = concat
        [ "instance " ++ firstToLower (nameBase n) ++ "ToJson :: "
           ++ mkConstraints "ToJSON" tyvars
           ++ " ToJSON (" ++ nameBase n ++ " "
           ++ intercalate " " (map mkTyVar tyvars) ++ ") where\n"
        , concatMap mkConToJson cons
        ]
      mkToJson (TySynD n tyvars t) = ""

      conToName (RecC n _) = nameBase n
      conToName (NormalC n _) = nameBase n

      mkConToJson (RecC n vars) = concat
        [ "  toJSON (" ++ nameBase n ++ " v) = object $\n"
        , "    [ \"tag\" .= \"" ++ nameBase n ++ "\"\n"
        , concatMap mkVarToJson vars
        , "    ]\n"
        ]
      mkConToJson (NormalC n vars) = concat
        [ "  toJSON (" ++ nameBase n ++ " " ++ intercalate " " vars' ++ ") = object $\n"
        , "    [ \"tag\" .= \"" ++ nameBase n ++ "\"\n"
        , if null vars
            then "    , \"contents\" .= ([] :: Array String)\n"
            else "    , \"contents\" .= " ++ wrapContent vars (intercalate ", " (map ("toJSON " ++) vars')) ++ "\n"
        , "    ]\n"
        ]
        where vars' = map (("x" ++) . show) [0..length vars - 1]

      wrapContent :: [a] -> String -> String
      wrapContent vars str | length vars == 1 = str
                           | otherwise        = "[" ++ str ++ "]"

      mkVarToJson (n, _, _) = "    , \"" ++ nameBase n ++ "\" .= v." ++ nameBase n ++ "\n"

      -- FromJSON deriving

      mkConstraints _ [] = ""
      mkConstraints cns tyvars = "(" ++ intercalate ", " (map (\tv -> cns ++ " " ++ mkTyVar tv) tyvars) ++ ") => "

      mkFromJson (NewtypeD _ n tyvars con _) = concat
        [ "instance " ++ firstToLower (nameBase n) ++ "FromJson :: "
           ++ mkConstraints "FromJSON" tyvars
           ++ " FromJSON (" ++ nameBase n ++ " "
           ++ intercalate " " (map mkTyVar tyvars)
           ++ ") where\n"
        , case con of
            NormalC _ _ -> "  parseJSON x = " ++ conToName con ++ " <$> parseJSON x"
            RecC _ _ -> "  parseJSON (JObject o) = do\n" ++ mkConFromJson False con
        ]
      mkFromJson (DataD _ n tyvars cons _) = concat
        [ "instance " ++ firstToLower (nameBase n) ++ "FromJson :: "
           ++ mkConstraints "FromJSON" tyvars
           ++ " FromJSON (" ++ nameBase n ++ " "
           ++ intercalate " " (map mkTyVar tyvars)
           ++ ") where\n"
        , "  parseJSON (JObject o) = do\n"
        , if length cons > 1
            then concat
              [ "    tag <- o .: \"tag\"\n"
              , "    case tag of\n"
              ]
            else ""
        , concatMap (mkConFromJson (length cons > 1)) cons
        ]
      mkFromJson (TySynD n tyvars t) = ""

      mkConFromJson useCase (RecC n vars) = concat
        [ if useCase then "      \"" ++ nameBase n ++ "\" -> do\n" else ""
        , concatMap mkVarFromJson vars
        , "        return $ " ++ nameBase n ++ " { "
        , intercalate ", " $ map (\(n, _, _) -> nameBase n ++ " : " ++ nameBase n) vars
        , " }\n"
        ]
      mkConFromJson useCase (NormalC n vars) = concat
        [ if useCase then "      \"" ++ nameBase n ++ "\" -> do\n" else ""
        , if null vars'
            then "         return " ++ nameBase n ++ "\n"
            else concat
              [ "         " ++ wrapContent vars (intercalate ", " vars') ++  " <- o .: \"contents\"\n"
              , "         " ++ nameBase n ++ " <$> " ++ intercalate " <*> " (map ("parseJSON " ++) vars') ++ "\n"
              ]
        , "\n"
        ]
        where vars' = map (("x" ++) . show) [0..length vars - 1]

      mkVarFromJson (n, _, _) = "        " ++ nameBase n ++ " <- o .: \"" ++ nameBase n ++ "\"\n"

mkRPC :: Bool -> [Name] -> Q [Dec]
mkRPC checkver fs = do
  rpc <- funD (mkName "handleRPC")
    [clause
      []
      ( normalB $ lamE [varP json]
        $ caseE [| parse parseCall $(varE json) |]
                $ (map mkMatch fs)
               ++ [ match [p| _ |]
                          (normalB [| Error "No such endpoint" |])
                          []
                  ]
      )
      []
    ]

  api <- forM fs $ \f -> do
    (cons, args) <- gatherArgTypes f
    return (nameBase f, cons, args)

  defs <- valD (varP $ mkName "rpcDefs")
               (normalB [e| api |])
               []

  -- check API version
  when checkver $ do
    file <- runIO $ try $ B.readFile "api.json" :: Q (Either SomeException B.ByteString)

    let oldApi = either (const emptyApi) (fromMaybe emptyApi . decode) file
        serApi = map (\(a, _, c) -> (a, c)) api
        diff   = oldApi \\ serApi

    when (not $ null diff) $
      error $ "Versions lost in new API: " ++ show diff
           ++ " -- either delete line(s) from api.json or add version(s) back."

    runIO $ B.writeFile "api.json" $ encode serApi

  return [rpc, defs]

  where
    json = mkName "json"
    v = mkName "v"

    gatherCons (AppT (AppT ArrowT app) x) = app:gatherCons x
    gatherCons x@(AppT _ _) = [x]
    gatherCons x@(ConT _) = [x]
    gatherCons (ForallT _ _ x) = gatherCons x
    gatherCons _ = []

    -- TODO: source out
    gatherArgTypes f = do
      VarI _ apps _ _ <- reify f
      let cons = gatherCons apps

      -- TODO: make fingerprinting stable
      let rfcon [ConT n] = [reify n]
          rfcon [VarT n] = [return $ TyConI $ DataD [] n [] [] []]
          rfcon [AppT f x] = rfcon [f] ++ rfcon [x]
          rfcon [TupleT x] = [return $ TyConI $ DataD [] (mkName $ "tuple" ++ show x) [] [] []]
          rfcon [ListT] = [return $ TyConI $ DataD [] (mkName $ "list") [] [] []]
          rfcon (x:xs) = rfcon [x] ++ rfcon xs
          rfcon [] = []

          -- remove kind variables, beacause those are not stable (exact names
          -- are random) and thus not suitable for hashing
          unkind (TyConI (DataD a b _ d e)) = TyConI (DataD a b [] d e)
          unkind (TyConI (NewtypeD a b _ d e)) = TyConI (NewtypeD a b [] d e)
          unkind (TyConI (TySynD a _ c)) = TyConI (TySynD a [] c)
          unkind x = x

      args <- sequence $ rfcon cons
      return (cons, show $ hash $ show $ map unkind args)

    mkMatch f = do
      (cons, args) <- gatherArgTypes f
      match [p| Success ( $(litP $ stringL $ nameBase f)
                        , $(litP $ stringL args)
                        , $(varP v)
                        )
            |]
            (normalB [| fmap toJSON <$> ($(mkPat f cons) <$> fromJSON $(varE v)) |])
            []

    mkPat f cons = do
      args <- mapM (newName . ("x" ++ ) . show) [0..length cons - 2]
      lamE [vargs args] (foldl appE (varE f) (map varE args))
      where vargs [] = error "Empty argument list"
            vargs [x] = varP x
            vargs (x:xs) = tupP [varP x, vargs xs]


traceLog msg x = trace (msg ++ show x) x

genPurescriptRpcs :: API -> String
genPurescriptRpcs = concatMap gen
  where
    gen :: (String, [Type], String) -> String
    gen (name, sig, hash) = concat
      [ name ++ " :: forall eff. Socket.Socket -> " ++ genSig sig ++ "\n"
      , name ++ " socket " ++ intercalate " " args ++ "\n"
      , if isAsync (last sig)
        then " = send socket $ encode \n"
        else " = sendSync socket $ encode \n"
      , "   $ object [ \"call\" .= \"" ++ name ++ "\"\n"
      , "            , \"version\" .= \"" ++ hash ++ "\"\n"
      , "            , \"args\" .= " ++ vargs args ++ "\n"
      , "            ]\n"
      , "\n"
      ]
      where args = map (("x" ++) .show) [0..length sig - 2]
            vargs :: [String] -> String
            vargs [] = ""
            vargs [x] = x
            vargs (x:xs) = "(Tuple " ++ x ++ " " ++ vargs xs ++ ")"

    isAsync (AppT _ (TupleT 0)) = True
    isAsync (TupleT 0) = True
    isAsync _ = False

    genSig [ConT n] | nameBase n == "Set" = "Array"
                    | nameBase n == "Bool" = "Boolean"
    genSig [ConT n] = nameBase n
    genSig [AppT f x] = "(" ++ genSig [f] ++ " " ++ genSig [x] ++ ")"
    genSig [ListT] = "Array"
    genSig [TupleT 0] = "Unit"
    genSig [TupleT 2] = "Tuple"
    genSig [TupleT n] = "Tuple" ++ show n ++ " "
    genSig [x] = error $ "Not supported type in function signature: " ++ show x
    genSig [x, (AppT (ConT n) r)] | nameBase n == "IO" = genSig [x] ++ " -> Aff (websocket :: Socket.WebSocket | eff) " ++ genSig [r]
    genSig [x, r] = genSig [x] ++ " -> Aff (websocket :: Socket.WebSocket | eff) " ++ genSig [r]
    genSig (x:xs) = genSig [x] ++ " -> " ++ genSig xs
    genSig [] = ""

commonPurescriptImports :: String
commonPurescriptImports = intercalate "\n"
  [ ""
  , ""
  , "import Data.JSON"
  , "import Data.Either"
  , "import Data.Maybe"
  , "import Data.List (List ())"
  , "import Data.Tuple"
  , "import Data.Set (Set ())"
  , "import Control.Monad.Aff"
  , "import Prelude"
  , ""
  , "type Text = String"
  , ""
  , ""
  ]

{- This all is needed for Type to be a JSON instance

deriveJSON defaultOptions ''ModName
deriveJSON defaultOptions ''PkgName
deriveJSON defaultOptions ''NameSpace
deriveJSON defaultOptions ''NameFlavour
deriveJSON defaultOptions ''TyLit
deriveJSON defaultOptions ''OccName
deriveJSON defaultOptions ''TyVarBndr
deriveJSON defaultOptions ''Name
deriveJSON defaultOptions ''Type

-}

