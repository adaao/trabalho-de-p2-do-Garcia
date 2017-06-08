{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Produto where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

formProduto :: Form Produto
formProduto = renderDivs $ Produto <$>
             areq textField "Produto" Nothing <*>
             areq doubleField "Valor" Nothing <*>
 
getProdutoR :: Handler Html
getProdutoR = do
            (widget, enctype) <- generateFormPost formPro
            defaultLayout $ widgetForm ProdutoR enctype widget "Cadastro de produtos"