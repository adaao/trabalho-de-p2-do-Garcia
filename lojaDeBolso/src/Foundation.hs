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
    UniqueEmail email
    telefone Text sqltype=varchar(11)
    senha Text
    deriving Show

Cliente
    nomeCliente Text
    telefoneCliente Text sqltype=varchar(11)
    UniqueEmail email
    cep Text --resolver
    nroEndeteco Int
    complemento Text
    deriving Show
    
Venda
    usuatioid UsuarioId
    clienteid ClienteId
    dataVenda Date
    vendaPaga Bool
    deriving Show

Produto
    dsProduto Text
    vlProduto Double
    deriving Show

ProdutosVendidos
    vendaid VendaId
    produtoid ProdutoId
    qtProduto Int
    vlVenda Double
    deriving Show
    
Cep
    UniqueCep cep
    logradouro Text
    estadoid EstadoId
    cidadeid CidadeId
    deriving Show


-- Tabelas do professor
Departamento
   nome Text
   sigla Text sqltype=varchar(3)
   deriving Show

Pessoa
   nome Text
   idade Int
   salario Double
   deptoid DepartamentoId
   deriving Show
  
Usuario
   nome Text
   email Text
   senha Text
   UniqueEmail email
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
    isAuthorized UsuarioR _ = isAdmin
    isAuthorized AdminR _ = isAdmin
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
