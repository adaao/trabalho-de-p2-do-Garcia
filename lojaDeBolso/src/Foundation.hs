{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Usuario
    nome Text
    email Text
    senha Text
    UniqueEmail email
    deriving Show

Cliente
    nomeCliente Text
    telefoneCliente Text sqltype=varchar(11)
--  Informações sobre o endereço -> será melhorado futuramente
    rua Text
    numero Int
    complemento Text
--    cep Text -> para um proximo release
    deriving Show
    
Venda
    usuatioid UsuarioId
    clienteid ClienteId
    vendaPaga Bool
    vlVenda Double
    deriving Show

Produto
    dsProduto Text
    vlProduto Double
    deriving Show

ProdutosVendidos
    vendaid VendaId
    produtoid ProdutoId
    qtProduto Int
    vlProduto Double
    deriving Show


|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "config/routes")

mkMessage "Sitio" "messages" "pt-BR"

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    --isAuthorized UsuarioR _ = isAdmin
    --isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser
{-

isAdmin = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized
        Just _ -> Unauthorized "Soh o admin acessa aqui!"
-}

isUser = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")
