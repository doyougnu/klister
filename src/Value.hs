{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Value where

import Control.Lens
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Text as T
import System.IO (Handle)
import qualified Data.Foldable as F

import Core
import Datatype
import Env
import ModuleName
import Syntax
import Syntax.SrcLoc
import Type

type VEnv = Env Var Value
type TEnv = Env MacroVar Value

data MacroAction
  = MacroActionPure Value
  | MacroActionBind MacroAction Closure
  | MacroActionSyntaxError (SyntaxError Syntax)
  | MacroActionIdentEq HowEq Value Value
  | MacroActionLog Syntax
  | MacroActionIntroducer
  | MacroActionWhichProblem
  | MacroActionTypeCase VEnv SrcLoc Ty (Seq (TypePattern, Core))

instance Show MacroAction where
  show _ = "MacroAction..."


data Value
  = ValueClosure Closure
  | ValueSyntax Syntax
  | ValueMacroAction MacroAction
  | ValueIOAction (IO Value)
  | ValueOutputPort Handle
  | ValueInteger Integer
  | ValueCtor Constructor (Seq Value)
  | ValueType Ty
  | ValueString Text

instance Show Value where
  show _ = "Value..."

primitiveCtor :: Text -> Seq Value -> Value
primitiveCtor name args =
  let ctor = Constructor (KernelName kernelName) (ConstructorName name)
  in ValueCtor ctor args

valueText :: Value -> Text
valueText (ValueClosure _) = "#<closure>"
valueText (ValueSyntax stx) = "'" <> syntaxText stx
valueText (ValueMacroAction _) = "#<macro>"
valueText (ValueIOAction _) = "#<IO>"
valueText (ValueOutputPort _) = "#<ouptut port>"
valueText (ValueInteger s) = "#!" <> T.pack (show s)
valueText (ValueCtor c args) =
  "(" <> view (constructorName . constructorNameText) c <> " " <>
  T.intercalate " " (map valueText (F.toList args)) <> ")"
valueText (ValueType ptr) = "#t<" <> T.pack (show ptr) <> ">"
valueText (ValueString str) = T.pack (show str)

-- | Find a simple description that is suitable for inclusion in error messages.
describeVal :: Value -> Text
describeVal (ValueClosure _) = "function"
describeVal (ValueSyntax _) = "syntax"
describeVal (ValueMacroAction _) = "macro action"
describeVal (ValueIOAction _) = "IO action"
describeVal (ValueOutputPort _) = "output port"
describeVal (ValueInteger _) = "integer"
describeVal (ValueCtor c _args) =
  view (constructorName . constructorNameText) c
describeVal (ValueType _) = "type"
describeVal (ValueString _) = "string"

data FOClosure = FOClosure
  { _closureEnv   :: VEnv
  , _closureIdent :: Ident
  , _closureVar   :: Var
  , _closureBody  :: Core
  }

data Closure = FO FOClosure | HO (Value -> Value)

instance Show Closure where
  show _ = "Closure {...}"

makePrisms ''MacroAction
makePrisms ''Value
makeLenses ''Closure
makeLenses ''FOClosure
