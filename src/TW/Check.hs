{-# LANGUAGE FlexibleContexts #-}
module TW.Check where

import TW.Ast
import TW.BuiltIn

import Control.Monad.Except
import qualified Data.Map as M

data DefinedType
   = DefinedType
   { dt_name :: QualTypeName
   , dt_args :: [TypeVar]
   } deriving (Show, Eq)

builtInToDefTy :: BuiltIn -> DefinedType
builtInToDefTy bi =
    DefinedType
    { dt_name = bi_name bi
    , dt_args = bi_args bi
    }

typeDefToDefTy :: Maybe ModuleName -> TypeDef -> DefinedType
typeDefToDefTy qualOwner td =
    let mkTy =
            case qualOwner of
              Nothing -> QualTypeName (ModuleName [])
              Just qt -> QualTypeName qt
    in case td of
         TypeDefEnum ed ->
             DefinedType (mkTy (ed_name ed)) (ed_args ed)
         TypeDefStruct sd ->
             DefinedType (mkTy (sd_name sd)) (sd_args sd)

checkModules :: [Module] -> Either String [Module]
checkModules modules =
    runExcept $
    forM modules $ \m ->
    do let currentMStr = printModuleNameS $ m_name m
       defTypes <-
           M.fromList . map (\dt -> (dt_name dt, dt_args dt)) <$>
           getDefinedTypes m
       let isValidType args t =
               case t of
                 TyVar tv ->
                     if tv `elem` args
                     then return ()
                     else throwError $ "Undefined type variable " ++ show tv ++ " in " ++ currentMStr
                 TyCon qt qtArgs ->
                     case M.lookup qt defTypes of
                       Nothing ->
                           throwError $ "Undefined type variable " ++ show qt ++ " in " ++ currentMStr
                       Just tvars ->
                           do forM_ qtArgs (isValidType args)
                              when (length tvars /= length qtArgs) $
                                   throwError $
                                   "Type " ++ show qt ++ " got applied wrong number of arguments in " ++ currentMStr
       forM_ (m_typeDefs m) $ \td ->
           case td of
             TypeDefEnum ed ->
                 forM_ (ed_choices ed) $ \ch ->
                 case ec_arg ch of
                   Nothing -> return ()
                   Just ty -> isValidType (ed_args ed) ty
             TypeDefStruct sd ->
                 forM_ (sd_fields sd) $ \fld ->
                 do isValidType (sd_args sd) (sf_type fld)
       return m
    where
      getDefinedTypes m =
          do importedTypes <-
                 forM (m_imports m) $ \im ->
                 case M.lookup im moduleMap of
                   Nothing ->
                       throwError $
                       "Unknown module " ++ printModuleNameS im
                       ++ " referenced from " ++ (printModuleNameS $ m_name m)
                   Just imModel ->
                       return $ map (typeDefToDefTy (Just im)) $ m_typeDefs imModel
             return $ concat importedTypes
                        ++ (map (typeDefToDefTy Nothing) $ m_typeDefs m)
                        ++ map builtInToDefTy allBuiltIns
      moduleMap =
          M.fromList $
          map (\m -> (m_name m, m)) modules
