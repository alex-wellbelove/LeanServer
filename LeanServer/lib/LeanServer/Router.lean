import Lean.Data.HashMap
import ProofWidgets
import Lean.Data.Parsec
-- import LeanServer.HTML

open Lean.Parsec
open Lean

  
inductive HTTPMethod where
  | GET : HTTPMethod
  | POST : HTTPMethod
 deriving Repr, BEq

instance : ToString HTTPMethod where
  toString method := match method with 
   | HTTPMethod.GET => "GET"
   | HTTPMethod.POST => "POST"

@[reducible]
def HeaderMap := List (String × String)

structure Request where
  (method : HTTPMethod)
  (path : String)
  (headers : HeaderMap)
  (body : Option String)
deriving Repr

instance : ToString Request where
  toString r := s!"{r.method} at {r.path} with headers {r.headers} and body {r.body}"

structure Response where
  (code : Int)
  (statusMessage : String)
  (body : String)
  (headers : HeaderMap)
deriving Repr

instance : ToString Response where
  toString r := s!"{r.code} {r.statusMessage}. Returning body: {r.body} with headers: {r.headers}"

def makeError (_: String) : Response := Response.mk 500 "Error" "" []

abbrev Ok text := Response.mk 200 "OK" text []

def err404 : Response := Response.mk 404 "Not Found" "Resource Not Found" []
def err404Handler (_ : Request) : Response := err404
def constHandler (response : Response) : Request →  IO Response := Function.const Request <| return response

def Handler : Type := (Request →  IO Response)

@[reducible]
def Route : Type := ((String →  HTTPMethod →  Bool) × Handler)

@[reducible]
def RoutingTable : Type := List Route

def getRoute (s : String) : (String →  HTTPMethod →  Bool) :=  fun url => fun method => (url == s) && (method == HTTPMethod.GET)
def postRoute (s : String) : (String →  HTTPMethod →  Bool) :=  fun url => fun method => (url == s) && (method == HTTPMethod.POST)

 def post (s : String) (handler : Handler) : Route := (postRoute s, handler) 


def matchUrl (matchers : List Route) (url : String) (method : HTTPMethod) : (Request →  IO Response) := match matchers with
  | [] => (fun _ => return err404)
  | ((f,r)::xs) => if (f url method) then r else matchUrl xs url method

def handleRequest (routes : RoutingTable) (req : Request) := (matchUrl routes req.path req.method) req
  
def toUTF8 (resp : Response) : ByteArray := (s!"HTTP/1.1 {resp.code} {resp.statusMessage}\r\n" ++
            s!"Content-Length: {resp.body.length}\r\n" ++
            "\r\n" ++
            resp.body).toUTF8

-- open scoped ProofWidgets.JSX
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


def get (s : String) (body : ProofWidgets.Html) : Route := (getRoute s, constHandler (Ok (htmlToString body))) 
