
module Parser where

import UU.Parsing
import Scanner
import GramaticaAbstracta
import AG.ContextoAG

pRoot  =  sem_Root_Root <$> pModule

pModule = sem_Module_ConsModule <$ pPalClave "module" 
								<*> pModuleDeclaration 
								<* pPalClave "where" 
								<*> pModuleBody

pModuleDeclaration = sem_ModuleDeclaration_ModDec <$> pIdent

pModuleBody  = sem_ModuleBody_ConsModBody <$> pImports <*> pPortal
		   <|> sem_ModuleBody_ConsModBody2 <$> pPortal

-- pImports = pFoldr1Sep (sem_Imports_Cons,sem_Imports_Nil) (pOpClave ";") pImport

pImports = pFoldr (sem_Imports_Cons,sem_Imports_Nil) pImport

pImport = sem_Import_IdentificadorImport <$ pPalClave "import" 
										 <*> pIdent 
										 <* pOpClave ";"

pPortal = sem_Portal_ConsPortal <$ pPalClave "new" 
								<*> pIdent 
								<* pOpClave "<%" 
								<*> pPortalBody 
								<* pOpClave "%>"
		<|> sem_Portal_ConsPortal2 <$ pPalClave "new" 
								   <*> pIdent 
								   <*> pExtend 
								   <* pOpClave "<%" 
								   <*> pPortalBody 
								   <* pOpClave "%>"

pExtend = sem_Extend_ConsExtend <$ pOpClave "(" <*> pType <* pOpClave ")"

pPortalBody = sem_PortalBody_ConsPortalBody <$> pTopDecls <*> pMethods -- pList pMethod

-- pMethods = pFoldr1Sep (sem_Methods_Cons,sem_Methods_Nil) (pOpClave ";") pMethod
pMethods = pFoldr (sem_Methods_Cons,sem_Methods_Nil) pMethod

-- pTopDecls = pFoldr1Sep (sem_TopDecls_Cons,sem_TopDecls_Nil) (pOpClave ";") pTopDecl
pTopDecls = pFoldr (sem_TopDecls_Cons,sem_TopDecls_Nil) pTopDecl

pTopDecl = sem_TopDecl_ConsTopDecl <$ pPalClave "type" 
								   <*> pIdent 
								   <* pOpClave "=" 
								   <*> pType 
								   <* pOpClave ";"
		
--		<|> sem_TopDecl_ConsTopDeclMethod <$> pMethod

pType = sem_Type_ConsType <$> pIdent

-- pMethods = pFoldr1Sep (sem_Methods_Cons,sem_Methods_Nil) (pOpClave "") pMethod

pMethod = sem_Method_ConsMethod <$> pIdent 
								<* pOpClave "<%" 
								<*> pDecls 
								<* pOpClave "%>"

-- pDecls = pFoldr1Sep (sem_Decls_Cons,sem_Decls_Nil) (pOpClave ";") pDecl
pDecls = pFoldr (sem_Decls_Cons,sem_Decls_Nil) pDecl

pDecl  = sem_Decl_ConsDeclSimple <$> pDeclSimple <* pOpClave ";"
	  <|> sem_Decl_ConsDefInterna <$> pDefInterna

pDeclSimple = sem_DeclSimple_ConsAddIcon <$ pPalClave "addIcon" <* pOpClave "->" <*> pCadena
		  <|> sem_DeclSimple_ConsAddTitle <$ pPalClave "addTitle" <* pOpClave "->" <*> pCadena
		  <|> sem_DeclSimple_ConsAddPortlet <$ pPalClave "addPortlet" <* pOpClave "->" <*> pIdent

pDefInterna = sem_DefInterna_ConsInterno <$> pIdent <* pOpClave "<$" <*> pCuerpoInterno <* pOpClave "$>"

-- pCuerpoInterno = pFoldr1Sep (sem_CuerpoInterno_Cons,sem_CuerpoInterno_Nil) (pOpClave ";") pDeclBodyPortlet
pCuerpoInterno = pFoldr (sem_CuerpoInterno_Cons,sem_CuerpoInterno_Nil) pDeclBodyPortlet

pDeclBodyPortlet = sem_DeclBodyPortlet_ConsDefTipo <$ pPalClave "TypePortlet" <* pOpClave "->" <*> pIdent <* pOpClave ";"
				<|> sem_DeclBodyPortlet_ConsDefTitle <$ pPalClave "Title" <* pOpClave "->" <*> pCadena <* pOpClave ";"
				<|> sem_DeclBodyPortlet_ConsDefBackground <$ pPalClave "Background" <* pOpClave "->" <*> pIdent <* pOpClave ";"
				<|> sem_DeclBodyPortlet_ConsDefCampos <$ pPalClave "Field" <* pOpClave "->" <*> pDefCampos <* pOpClave ";"

pLiteral = sem_Literal_LiteralCadena <$> pCadena
{--
pCadena :: Parser Token String
pCadena = (\(Token TokCadena str _) -> str) <$> pSym (Token TokCadena "" 0)
-}
pDefCampos = sem_DefCampos_ConsCampos <$ pOpClave "[" <*> pDeclFields <* pOpClave "]"

pDeclFields = pFoldr1Sep (sem_DeclFields_Cons,sem_DeclFields_Nil) (pOpClave ",") pDeclField

pDeclField = sem_DeclField_ConsDeclField <$> pIdent