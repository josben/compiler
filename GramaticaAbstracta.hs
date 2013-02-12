
module GramaticaAbstracta where

data Module			 = ConsModule ModuleDeclaration ModuleBody
					 deriving Show

data ModuleDeclaration = ModDec String
					   deriving Show

data ModuleBody		 = ConsModBody Imports Portal
					 | ConsModBody2 Portal
					 deriving Show

data Import          = IdentificadorImport String 
                     deriving Show

type Imports		 = [Import]

data Portal 	     = ConsPortal String PortalBody
					 | ConsPortal2 String Extend PortalBody
	    	     	 deriving Show

data Extend 	     = ConsExtend Type 
	    	     	 deriving Show

data PortalBody      = ConsPortalBody TopDecls Methods
		     		 deriving Show

data Type 	     	 = ConsType String
	  	     	 	 deriving Show

type TopDecls 	     = [TopDecl]

data TopDecl 	     = ConsTopDecl String Type
--	     	     	 | ConsTopDeclMethod Method
	     	     	 deriving Show

type Methods		 = [Method]

data Method 	     = ConsMethod String Decls
	    	     	 deriving Show

data Decl 	     	 = ConsDeclSimple DeclSimple
	   	     		 | ConsDefInterna DefInterna
	             	 deriving Show

type Decls 			 = [Decl]

{-
data DeclSimple      = ConsAddIcon DeclAddIcon
		     		 | ConsAddTitle DeclAddTitle
		     		 | ConsAddPortlet DeclAddPortlet
		     		 deriving Show
-}
data DeclSimple      = ConsAddIcon String
					 | ConsAddTitle String
					 | ConsAddPortlet String
					 deriving Show
					 
data Literal         = LiteralCadena String
					 deriving Show
			 
-- data DeclAddIcon     = ConsDeclAddIcon String
--		     		 deriving Show

-- data DeclAddTitle    = ConsDeclAddTitle String
--		     		 deriving Show

-- data DeclAddPortlet  = ConsDeclAddPortlet String
--		     		 deriving Show

data DefInterna      = ConsInterno String CuerpoInterno
		     		 deriving Show

type CuerpoInterno   = [DeclBodyPortlet]

{-
data DeclBodyPortlet = ConsDefTipo DefTipo
		     		 | ConsDefTitle DefTitle
		     		 | ConsDefCampos DefCampos
		     		 | ConsDefBackground DefBackground
		     		 deriving Show
-}

data DeclBodyPortlet = ConsDefTipo String
					 | ConsDefTitle String
					 | ConsDefBackground String
					 | ConsDefCampos DefCampos
					 deriving Show

{-
data DefTipo 	     = ConsPortlet ContentPortlet
	     	     	 deriving Show

data DefTitle 	     = ConsTitle String
	      	     	 deriving Show

data DefBackground   = ConsBackground String
		     		 deriving Show
-}
data DefCampos       = ConsCampos DeclFields
	       	     	 deriving Show
	       	     	 
type DeclFields		 = [DeclField]

data DeclField		 = ConsDeclField String
					 deriving Show
{--					 
data Literal		 = Lit String
					 deriving Show
-}
{-
data ContentPortlet  = ConsContentPortlet String
		     		 deriving Show
-}

-- Funciones Semanticas
{--
sem_Root_Root				  		  = id
sem_Module_ConsModule				  = ConsModule
sem_ModuleDeclaration_ModDec		  = ModDec
sem_ModuleBody_ConsModBody			  = ConsModBody
sem_ModuleBody_ConsModBody2			  = ConsModBody2
sem_Import_IdentificadorImport		  = IdentificadorImport
sem_Imports_Cons					  = (:)
sem_Imports_Nil						  = []
sem_Portal_ConsPortal		  		  = ConsPortal
sem_Portal_ConsPortal2		  		  = ConsPortal2
sem_Extend_ConsExtend		  		  = ConsExtend
sem_PortalBody_ConsPortalBody 		  = ConsPortalBody
sem_Type_ConsType			  		  = ConsType
sem_TopDecls_Cons			  		  = (:)
sem_TopDecls_Nil			  		  = []
sem_TopDecl_ConsTopDecl		  		  = ConsTopDecl
-- sem_TopDecl_ConsTopDeclMethod 		  = ConsTopDeclMethod
sem_Methods_Cons					  = (:)
sem_Methods_Nil						  = []
sem_Method_ConsMethod		  		  = ConsMethod
sem_Decl_ConsDeclSimple	  	  		  = ConsDeclSimple
sem_Decl_ConsDefInterna	  	  		  = ConsDefInterna
sem_Decls_Cons				  		  = (:)
sem_Decls_Nil			      		  = []
sem_DeclSimple_ConsAddIcon	  		  = ConsAddIcon
sem_DeclSimple_ConsAddTitle   		  = ConsAddTitle
sem_DeclSimple_ConsAddPortlet 		  = ConsAddPortlet
sem_Literal_LiteralCadena             = LiteralCadena
-- sem_DeclAddIcon_ConsDeclAddIcon		  = ConsDeclAddIcon
-- sem_DeclAddTitle_ConsDeclAddTitle	  = ConsDeclAddTitle
-- sem_DeclAddPortlet_ConsDeclAddPortlet = ConsDeclAddPortlet
sem_DefInterna_ConsInterno 			  = ConsInterno
sem_CuerpoInterno_Cons 				  = (:)
sem_CuerpoInterno_Nil  				  = []
sem_DeclBodyPortlet_ConsDefTipo 	  = ConsDefTipo
sem_DeclBodyPortlet_ConsDefTitle 	  = ConsDefTitle
sem_DeclBodyPortlet_ConsDefCampos 	  = ConsDefCampos
sem_DeclBodyPortlet_ConsDefBackground = ConsDefBackground
-- sem_DefTipo_ConsPortlet 			  = ConsPortlet
-- sem_DefTitle_ConsTitle 				  = ConsTitle
-- sem_DefBackground_ConsBackground 	  = ConsBackground
sem_DefCampos_ConsCampos 			  = ConsCampos
sem_DeclFields_Cons					  = (:)
sem_DeclFields_Nil					  = []
sem_DeclField_ConsDeclField			  = ConsDeclField
-- sem_ContentPortlet_ConsContentPortlet = ConsContentPortlet
--}