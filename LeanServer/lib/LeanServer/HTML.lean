import ProofWidgets
open Lean

def cleanAttr (s : String) : String := match s with 
 | "class_" => "class"
 | "section_" => "section"
 | s => String.replace s "_" "-"
  

def attrToString (sj : String × Json) : String := s!"{cleanAttr sj.fst}={sj.snd}"
def attrsToString (l : List (String × Json)) : String := String.join <| List.map attrToString l
def listArrayToString (l : Array String) : String := String.join l.data

partial def htmlToString (x: ProofWidgets.Html) : String := match x with
  | ProofWidgets.Html.element tag attrs children => s!"<{tag} {attrsToString attrs.data}> {listArrayToString (children.map htmlToString)} </{tag}>"
  | ProofWidgets.Html.text s => s
  | ProofWidgets.Html.component s a a2 a3 => "ERROR components not supported ERROR"

