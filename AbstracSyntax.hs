
module AbstracSyntax where

{--
PortalBody  -> TopDecls
TopDecls    -> TopDecl+
TopDecl     -> "type" Identificador "=" Type ;
			 | Method
Method      -> Decls
Decls       -> DeclSimple
             | DefInterna
             | Decls
DeclSimple  -> DeclAddIcon
			 | DeclAddTitle
			 | DeclAddPortlet
DeclAddIcon -> "addIcon" "->" path_icon ;
DeclAddTitle-> "addTitle" "->" titulo ;
DeclAddPortlet -> "addPortlet" "->" Identificador ;
DefInterna  -> Identificador "$" "<" CuerpoIdent ">"
CuerpoIdent -> DeclsBodyPortlet+
DeclsBodyPortlet -> DefTipo
				  | DefTitle
				  | DefCampos
				  | DefFilas
				  | DefBackground
DefTipo     -> "TypePortlet" "->" Identificador ;
DefTitle    -> "Title" "->" Identificador ;
DefBackground -> "Background" "->" Identificador ;
DefCambos   -> "[" Identificador "|" (,Identificador*)? "]"
ContentPortlet -> KeyWords
Type        -> KeyWords
KeyWords -> "String" | "Portal" | "Portlet" | "Lista" | "Tabla" | "Imagen"


data Import 	 = Identificador String
			 	 deriving Show
data DeclPackage = Identificador String
				 deriving Show
data Portal 	 = Identificador Extend
				 deriving Show
data Extend      = ExtendType Type
				 deriving Show
--}