
module GramaticaAbstracta where

data Import         = ConsImportSimple Literal 
                    | ConsImportVarios Literal [Literal] 
                    deriving Show

data Literal         = Lit String
		     deriving Show

data Package 	     = ConsPackageSimple Literal 
	     	     | ConsPackageVarios Literal [Literal] 
	     	     deriving Show

data Portal 	     = ConsPortal Literal Extend PortalBody
	    	     deriving Show

data Extend 	     = ConsExtend Type 
	    	     deriving Show

data PortalBody      = ConsPortalBody TopDecls
		     deriving Show

data Type 	     = Type Literal
	  	     deriving Show

data TopDecls 	     = ConsTopDecls [TopDecl]
	       	     deriving Show

data TopDecl 	     = ConsTopDecl Literal Type
	     	     | ConstTopDeclMethod Method
	     	     deriving Show

data Method 	     = ConsMethod Decls
	    	     deriving Show

data Decls 	     = ConsDeclSimple DeclSimple
	   	     | ConsDefInterna DefInterna
	   	     | Decls
	             deriving Show

data DeclSimple      = ConsAddIcon DeclAddIcon
		     | ConsAddTitle DeclAddTitle
		     | ConsAddPortlet DeclAddPortlet
		     deriving Show

data DeclAddIcon     = ConsDeclAddIcon Literal
		     deriving Show

data DeclAddTitle    = ConsDeclAddTitle Literal
		     deriving Show

data DeclAddPortlet  = ConsDeclAddPortlet Literal
		     deriving Show

data DefInterna      = ConsInterno Literal CuerpoInterno
		     deriving Show

data CuerpoInterno   = ConsCuerpoInterno [DeclBodyPortlet]
		     deriving Show

data DeclBodyPortlet = ConsDefTipo DefTipo
		     | ConsDefTitle DefTitle
		     | ConsDefCampos DefCampos
		     | ConsDefBackground DefBackground
		     deriving Show

data DefTipo 	     = ConsPortlet ContentPortlet
	     	     deriving Show

data DefTitle 	     = ConsTitle Literal
	      	     deriving Show

data DefBackground   = ConsBackground Literal
		     deriving Show

data DefCampos       = ConsCampos Literal [Literal]
	       	     deriving Show

data ContentPortlet  = ConsContentPortlet Literal
		     deriving Show

